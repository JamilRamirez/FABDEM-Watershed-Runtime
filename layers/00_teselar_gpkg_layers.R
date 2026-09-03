# ============================================================
# 00_teselar_gpkg_layers.R
#
# FABDEM Watershed Explorer
# PREPROCESAMIENTO DE CAPAS GPKG PESADAS
# v3: teselado directo con GDAL/OGR sin leer geometrías fuente con st_read
#
# Colocar este script en la RAIZ de:
#   FABDEM_Watershed_Runtime/layers/
#
# Funcion:
#   - recorre recursivamente todas las subcarpetas;
#   - detecta archivos .gpkg;
#   - deja intactos los GPKG <= 50 MiB;
#   - subdivide espacialmente los GPKG > 50 MiB;
#   - intenta que CADA tesela final quede <= 50 MiB;
#   - crea un index.gpkg por capa teselada;
#   - conserva el GPKG original por seguridad.
#
# IMPORTANTE:
#   La subdivision NO corta por numero arbitrario de filas.
#   Se hace espacialmente. Si una tesela sigue pesando mas de
#   50 MiB, se subdivide recursivamente en cuatro hasta cumplir
#   el limite.
#
# Dependencias:
#   install.packages(c("sf", "DBI", "RSQLite"))
#
# v3 usa sf::gdal_utils("vectortranslate") para recortar directamente
# con GDAL/OGR. No usa st_read() sobre los GPKG fuente pesados.
# ============================================================


# ============================================================
# 1. CONFIGURACION
# ============================================================

MAX_SOURCE_MB <- 50

# Se usa un objetivo algo menor que 50 MiB para dejar margen.
TARGET_TILE_MB <- 45

# Limite duro final por tesela.
MAX_TILE_MB <- 50

# Proteccion contra subdivisiones patologicas.
MAX_RECURSION_DEPTH <- 12L

# FALSE = si ya existe una salida terminada, no la rehace.
OVERWRITE_OUTPUT <- FALSE

# Si una ejecucion anterior dejo una carpeta __tiles sin
# TILING_READY.txt, se elimina y se reconstruye automaticamente.
RESTART_INCOMPLETE_OUTPUT <- TRUE

# Por seguridad el original se conserva siempre por defecto.
# Si se cambia a TRUE, solo se borra DESPUES de terminar y
# validar todas las teselas de ese GPKG.
DELETE_ORIGINAL_AFTER_SUCCESS <- FALSE

# Nombre usado para carpetas generadas.
TILED_SUFFIX <- "__tiles"


# ============================================================
# 2. PAQUETES
# ============================================================

required_pkgs <- c(
  "sf",
  "DBI",
  "RSQLite"
)

missing_pkgs <- required_pkgs[
  !vapply(
    required_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_pkgs) > 0L) {
  stop(
    paste0(
      "Faltan paquetes: ",
      paste(missing_pkgs, collapse = ", "),
      "\n\nInstala con:\ninstall.packages(c(\"",
      paste(missing_pkgs, collapse = "\", \""),
      "\"))"
    )
  )
}


# ============================================================
# 3. RAIZ = CARPETA DONDE ESTA ESTE SCRIPT
# ============================================================

get_script_dir <- function() {

  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)

  if (length(hit) > 0L) {
    script_file <- sub("^--file=", "", hit[1])

    return(
      dirname(
        normalizePath(
          script_file,
          winslash = "/",
          mustWork = TRUE
        )
      )
    )
  }

  # Si se ejecuta con source() desde RStudio, se usa el wd.
  normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
}


LAYERS_ROOT <- get_script_dir()

cat(
  "\nRaiz de layers:\n",
  LAYERS_ROOT,
  "\n\n",
  sep = ""
)

cat(
  "sf: ",
  as.character(utils::packageVersion("sf")),
  " | GDAL: ",
  as.character(sf::sf_extSoftVersion()[["GDAL"]]),
  "\n",
  sep = ""
)


# ============================================================
# 4. HELPERS GENERALES
# ============================================================

bytes_to_mb <- function(x) {
  as.numeric(x) / 1024^2
}


file_mb <- function(path) {

  info <- file.info(path)

  if (
    nrow(info) != 1L ||
    is.na(info$size)
  ) {
    return(NA_real_)
  }

  bytes_to_mb(info$size)
}


safe_name <- function(x) {

  x <- gsub(
    "[^A-Za-z0-9_-]+",
    "_",
    as.character(x)
  )

  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  if (!nzchar(x)) {
    x <- "layer"
  }

  x
}


path_relative_to <- function(path, root) {

  p <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )

  r <- normalizePath(
    root,
    winslash = "/",
    mustWork = TRUE
  )

  prefix <- paste0(r, "/")

  if (identical(p, r)) {
    return(".")
  }

  if (!startsWith(tolower(p), tolower(prefix))) {
    return(p)
  }

  substring(
    p,
    nchar(prefix) + 1L
  )
}


is_generated_path <- function(path) {

  normalized <- gsub(
    "\\\\",
    "/",
    normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    )
  )

  grepl(
    paste0(
      "(^|/)[^/]*",
      TILED_SUFFIX,
      "(/|$)"
    ),
    normalized,
    ignore.case = TRUE
  ) ||
    identical(
      tolower(basename(path)),
      "index.gpkg"
    )
}


remove_if_exists <- function(path) {

  if (file.exists(path)) {
    unlink(
      path,
      recursive = TRUE,
      force = TRUE
    )
  }

  invisible(NULL)
}


quote_sql_ident <- function(x) {
  paste0(
    '"',
    gsub(
      '"',
      '""',
      as.character(x),
      fixed = TRUE
    ),
    '"'
  )
}


sf_supports_wkt_filter <- function() {

  method <- tryCatch(
    getS3method(
      "st_read",
      "character",
      envir = asNamespace("sf")
    ),
    error = function(e) NULL
  )

  if (is.null(method)) {
    return(FALSE)
  }

  "wkt_filter" %in% names(
    formals(method)
  )
}


gpkg_layer_sql_info <- function(
    gpkg,
    layer_name
) {

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    gpkg
  )

  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )

  geom_row <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT column_name FROM gpkg_geometry_columns ",
      "WHERE table_name = ? LIMIT 1"
    ),
    params = list(
      layer_name
    )
  )

  if (nrow(geom_row) != 1L) {
    stop(
      paste0(
        "No se pudo identificar la columna geometrica de la capa '",
        layer_name,
        "'."
      )
    )
  }

  geom_col <- as.character(
    geom_row$column_name[1]
  )

  table_info <- DBI::dbGetQuery(
    con,
    paste0(
      "PRAGMA table_info(",
      quote_sql_ident(layer_name),
      ")"
    )
  )

  pk_rows <- table_info[
    suppressWarnings(
      as.integer(table_info$pk)
    ) > 0L,
    ,
    drop = FALSE
  ]

  if (nrow(pk_rows) > 0L) {
    pk_rows <- pk_rows[
      order(
        as.integer(pk_rows$pk)
      ),
      ,
      drop = FALSE
    ]

    fid_col <- as.character(
      pk_rows$name[1]
    )
  } else if (
    "fid" %in% tolower(
      as.character(table_info$name)
    )
  ) {
    hit <- match(
      "fid",
      tolower(
        as.character(table_info$name)
      )
    )

    fid_col <- as.character(
      table_info$name[hit]
    )
  } else {
    fid_col <- NA_character_
  }

  rtree_name <- paste0(
    "rtree_",
    layer_name,
    "_",
    geom_col
  )

  rtree_exists <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT name FROM sqlite_master ",
      "WHERE type IN ('table','view') AND name = ? LIMIT 1"
    ),
    params = list(
      rtree_name
    )
  )

  list(
    geometry_column = geom_col,
    fid_column = fid_col,
    rtree_name = rtree_name,
    has_rtree = nrow(rtree_exists) == 1L
  )
}


# ============================================================
# 5. METADATA GPKG SIN LEER TODAS LAS GEOMETRIAS
# ============================================================

read_gpkg_feature_layers <- function(gpkg) {

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    gpkg
  )

  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )

  sql <- paste(
    "SELECT",
    "table_name, data_type, identifier,",
    "min_x, min_y, max_x, max_y, srs_id",
    "FROM gpkg_contents",
    "WHERE data_type = 'features'"
  )

  out <- DBI::dbGetQuery(
    con,
    sql
  )

  if (nrow(out) == 0L) {
    return(out)
  }

  out
}


layer_feature_counts <- function(gpkg) {

  layers <- read_gpkg_feature_layers(
    gpkg
  )

  if (nrow(layers) == 0L) {
    return(NULL)
  }

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    gpkg
  )

  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )

  counts <- vapply(
    as.character(layers$table_name),
    function(layer_name) {

      sql <- paste0(
        "SELECT COUNT(*) AS n FROM ",
        quote_sql_ident(layer_name)
      )

      out <- DBI::dbGetQuery(
        con,
        sql
      )

      suppressWarnings(
        as.numeric(out$n[1])
      )
    },
    numeric(1)
  )

  data.frame(
    layer = as.character(layers$table_name),
    features = counts,
    stringsAsFactors = FALSE
  )
}


gpkg_crs_from_srs_id <- function(
    gpkg,
    srs_id
) {

  srs_id <- suppressWarnings(
    as.integer(srs_id)
  )

  if (!is.finite(srs_id)) {
    stop(
      "srs_id invalido en gpkg_contents."
    )
  }

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    gpkg
  )

  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )

  row <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT srs_id, organization, organization_coordsys_id, definition ",
      "FROM gpkg_spatial_ref_sys WHERE srs_id = ? LIMIT 1"
    ),
    params = list(
      srs_id
    )
  )

  if (nrow(row) != 1L) {
    stop(
      paste0(
        "No existe srs_id=",
        srs_id,
        " en gpkg_spatial_ref_sys."
      )
    )
  }

  organization <- toupper(
    trimws(
      as.character(row$organization[1])
    )
  )

  organization_id <- suppressWarnings(
    as.integer(
      row$organization_coordsys_id[1]
    )
  )

  if (
    identical(
      organization,
      "EPSG"
    ) &&
    is.finite(
      organization_id
    ) &&
    organization_id > 0L
  ) {

    crs <- sf::st_crs(
      organization_id
    )

    if (!is.na(crs)) {
      return(crs)
    }
  }

  definition <- as.character(
    row$definition[1]
  )

  if (
    !is.na(definition) &&
    nzchar(definition) &&
    !identical(
      toupper(definition),
      "UNDEFINED"
    )
  ) {

    crs <- sf::st_crs(
      definition
    )

    if (!is.na(crs)) {
      return(crs)
    }
  }

  stop(
    paste0(
      "No se pudo interpretar el CRS del srs_id=",
      srs_id,
      "."
    )
  )
}


layer_bbox <- function(
    gpkg,
    layer_row,
    layer_name
) {

  coords <- suppressWarnings(
    as.numeric(
      unlist(
        layer_row[
          1,
          c(
            "min_x",
            "min_y",
            "max_x",
            "max_y"
          ),
          drop = FALSE
        ],
        use.names = FALSE
      )
    )
  )

  if (
    length(coords) != 4L ||
    any(!is.finite(coords)) ||
    coords[1] >= coords[3] ||
    coords[2] >= coords[4]
  ) {
    stop(
      paste0(
        "gpkg_contents no contiene una extension espacial valida para la capa '",
        layer_name,
        "'.\n",
        "Este script v3 evita leer el GPKG pesado completo en R. ",
        "Regenera el GPKG con extent valido antes de teselar."
      )
    )
  }

  crs <- gpkg_crs_from_srs_id(
    gpkg = gpkg,
    srs_id = layer_row$srs_id[1]
  )

  sf::st_bbox(
    c(
      xmin = coords[1],
      ymin = coords[2],
      xmax = coords[3],
      ymax = coords[4]
    ),
    crs = crs
  )
}


# ============================================================
# 6. GRILLA INICIAL
# ============================================================

bbox_crs <- function(bbox) {

  crs <- attr(
    bbox,
    "crs",
    exact = TRUE
  )

  if (is.null(crs) || is.na(crs)) {
    stop(
      "El bounding box no conserva un CRS valido."
    )
  }

  crs
}


make_initial_grid <- function(
    bbox,
    estimated_parts
) {

  estimated_parts <- max(
    2L,
    as.integer(
      ceiling(estimated_parts)
    )
  )

  width <- as.numeric(
    bbox["xmax"] - bbox["xmin"]
  )

  height <- as.numeric(
    bbox["ymax"] - bbox["ymin"]
  )

  if (
    !is.finite(width) ||
    !is.finite(height) ||
    width <= 0 ||
    height <= 0
  ) {
    stop("Bounding box invalido para teselar.")
  }

  aspect <- width / height

  ncols <- max(
    1L,
    as.integer(
      ceiling(
        sqrt(
          estimated_parts * aspect
        )
      )
    )
  )

  nrows <- max(
    1L,
    as.integer(
      ceiling(
        estimated_parts / ncols
      )
    )
  )

  xs <- seq(
    as.numeric(bbox["xmin"]),
    as.numeric(bbox["xmax"]),
    length.out = ncols + 1L
  )

  ys <- seq(
    as.numeric(bbox["ymin"]),
    as.numeric(bbox["ymax"]),
    length.out = nrows + 1L
  )

  out <- vector(
    "list",
    ncols * nrows
  )

  k <- 0L

  for (row in seq_len(nrows)) {
    for (col in seq_len(ncols)) {

      k <- k + 1L

      out[[k]] <- sf::st_bbox(
        c(
          xmin = xs[col],
          ymin = ys[row],
          xmax = xs[col + 1L],
          ymax = ys[row + 1L]
        ),
        crs = bbox_crs(bbox)
      )
    }
  }

  out
}


split_bbox_four <- function(bbox) {

  xmin <- as.numeric(bbox["xmin"])
  ymin <- as.numeric(bbox["ymin"])
  xmax <- as.numeric(bbox["xmax"])
  ymax <- as.numeric(bbox["ymax"])

  xmid <- (xmin + xmax) / 2
  ymid <- (ymin + ymax) / 2

  crs <- bbox_crs(bbox)

  list(
    sf::st_bbox(
      c(xmin = xmin, ymin = ymid, xmax = xmid, ymax = ymax),
      crs = crs
    ),
    sf::st_bbox(
      c(xmin = xmid, ymin = ymid, xmax = xmax, ymax = ymax),
      crs = crs
    ),
    sf::st_bbox(
      c(xmin = xmin, ymin = ymin, xmax = xmid, ymax = ymid),
      crs = crs
    ),
    sf::st_bbox(
      c(xmin = xmid, ymin = ymin, xmax = xmax, ymax = ymid),
      crs = crs
    )
  )
}


# ============================================================
# 7. LEER Y RECORTAR SOLO UNA VENTANA
# ============================================================

gpkg_feature_count <- function(
    gpkg,
    layer_name = NULL
) {

  layers <- read_gpkg_feature_layers(
    gpkg
  )

  if (nrow(layers) == 0L) {
    return(0)
  }

  if (is.null(layer_name)) {
    layer_name <- as.character(
      layers$table_name[1]
    )
  }

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    gpkg
  )

  on.exit(
    DBI::dbDisconnect(con),
    add = TRUE
  )

  out <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      quote_sql_ident(layer_name)
    )
  )

  suppressWarnings(
    as.numeric(out$n[1])
  )
}


gdal_clip_bbox_to_gpkg <- function(
    source_gpkg,
    source_layer,
    bbox,
    destination_gpkg
) {

  remove_if_exists(
    destination_gpkg
  )

  xmin_b <- format(
    as.numeric(bbox["xmin"]),
    digits = 17,
    scientific = FALSE,
    trim = TRUE
  )

  ymin_b <- format(
    as.numeric(bbox["ymin"]),
    digits = 17,
    scientific = FALSE,
    trim = TRUE
  )

  xmax_b <- format(
    as.numeric(bbox["xmax"]),
    digits = 17,
    scientific = FALSE,
    trim = TRUE
  )

  ymax_b <- format(
    as.numeric(bbox["ymax"]),
    digits = 17,
    scientific = FALSE,
    trim = TRUE
  )

  options <- c(
    "-f",
    "GPKG",
    "-overwrite",
    "-spat",
    xmin_b,
    ymin_b,
    xmax_b,
    ymax_b,
    "-clipsrc",
    xmin_b,
    ymin_b,
    xmax_b,
    ymax_b,
    "-nln",
    "data",
    "-lco",
    "SPATIAL_INDEX=YES",
    source_layer
  )

  sf::gdal_utils(
    util = "vectortranslate",
    source = source_gpkg,
    destination = destination_gpkg,
    options = options,
    quiet = TRUE,
    config_options = c(
      OGR2OGR_USE_ARROW_API = "NO"
    )
  )

  if (!file.exists(destination_gpkg)) {
    return(
      list(
        n_features = 0,
        size_mb = NA_real_
      )
    )
  }

  layers_out <- read_gpkg_feature_layers(
    destination_gpkg
  )

  if (nrow(layers_out) == 0L) {
    remove_if_exists(
      destination_gpkg
    )

    return(
      list(
        n_features = 0,
        size_mb = NA_real_
      )
    )
  }

  out_layer <- as.character(
    layers_out$table_name[1]
  )

  n_features <- gpkg_feature_count(
    destination_gpkg,
    out_layer
  )

  if (
    !is.finite(n_features) ||
    n_features < 1
  ) {
    remove_if_exists(
      destination_gpkg
    )

    return(
      list(
        n_features = 0,
        size_mb = NA_real_
      )
    )
  }

  list(
    n_features = as.integer(n_features),
    size_mb = file_mb(destination_gpkg)
  )
}


# ============================================================
# 8. ESCRIBIR UNA CAPA TESelADA
# ============================================================

split_one_layer <- function(
    gpkg,
    layer_name,
    layer_bbox_value,
    output_dir,
    source_stem,
    estimated_layer_mb
) {

  tile_dir <- file.path(
    output_dir,
    "tiles"
  )

  dir.create(
    tile_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  tile_counter <- 0L
  index_rows <- list()
  manifest_rows <- list()


  accept_tile <- function(
      n_features,
      candidate_file,
      depth,
      source_bbox
  ) {

    tile_counter <<- tile_counter + 1L

    tile_id <- sprintf(
      "%s_%04d",
      safe_name(source_stem),
      tile_counter
    )

    final_file <- file.path(
      tile_dir,
      paste0(
        tile_id,
        ".gpkg"
      )
    )

    remove_if_exists(final_file)

    moved <- file.rename(
      candidate_file,
      final_file
    )

    if (!isTRUE(moved)) {

      copied <- file.copy(
        candidate_file,
        final_file,
        overwrite = TRUE
      )

      if (!isTRUE(copied)) {
        stop(
          paste0(
            "No se pudo mover/copiar la tesela final:\n",
            final_file
          )
        )
      }

      unlink(
        candidate_file,
        force = TRUE
      )
    }

    final_mb <- file_mb(final_file)

    if (
      !is.finite(final_mb) ||
      final_mb > MAX_TILE_MB
    ) {
      stop(
        paste0(
          "Una tesela aceptada supera el limite final de ",
          MAX_TILE_MB,
          " MiB:\n",
          final_file,
          "\nTamano: ",
          sprintf("%.2f", final_mb),
          " MiB"
        )
      )
    }

    footprint <- sf::st_as_sfc(
      source_bbox
    )

    index_rows[[length(index_rows) + 1L]] <<-
      sf::st_sf(
        TILE_ID = tile_id,
        FILE_NAME = basename(final_file),
        RELATIVE_PATH = path_relative_to(
          final_file,
          LAYERS_ROOT
        ),
        SIZE_MB = round(final_mb, 3),
        N_FEATURES = as.integer(n_features),
        DEPTH = as.integer(depth),
        SOURCE_GPKG = basename(gpkg),
        SOURCE_LAYER = layer_name,
        geometry = footprint
      )

    manifest_rows[[length(manifest_rows) + 1L]] <<-
      data.frame(
        TILE_ID = tile_id,
        FILE_NAME = basename(final_file),
        RELATIVE_PATH = path_relative_to(
          final_file,
          LAYERS_ROOT
        ),
        SIZE_MB = round(final_mb, 3),
        N_FEATURES = as.integer(n_features),
        DEPTH = as.integer(depth),
        SOURCE_GPKG = basename(gpkg),
        SOURCE_LAYER = layer_name,
        stringsAsFactors = FALSE
      )

    cat(
      sprintf(
        "      OK %-28s %7.2f MiB | %d features\n",
        basename(final_file),
        final_mb,
        as.integer(n_features)
      )
    )

    invisible(NULL)
  }


  process_bbox <- function(
      bbox,
      depth = 0L
  ) {

    candidate_file <- tempfile(
      pattern = ".candidate_",
      tmpdir = tile_dir,
      fileext = ".gpkg"
    )

    remove_if_exists(candidate_file)

    candidate <- gdal_clip_bbox_to_gpkg(
      source_gpkg = gpkg,
      source_layer = layer_name,
      bbox = bbox,
      destination_gpkg = candidate_file
    )

    if (
      !is.finite(candidate$n_features) ||
      candidate$n_features < 1L
    ) {
      remove_if_exists(candidate_file)
      return(invisible(NULL))
    }

    candidate_mb <- candidate$size_mb

    if (!is.finite(candidate_mb)) {
      remove_if_exists(candidate_file)
      stop("No se pudo medir una tesela temporal generada por GDAL.")
    }

    if (candidate_mb <= MAX_TILE_MB) {

      accept_tile(
        n_features = candidate$n_features,
        candidate_file = candidate_file,
        depth = depth,
        source_bbox = bbox
      )

      gc(verbose = FALSE)

      return(invisible(NULL))
    }

    remove_if_exists(candidate_file)

    if (depth >= MAX_RECURSION_DEPTH) {
      stop(
        paste0(
          "Se alcanzo MAX_RECURSION_DEPTH y una ventana sigue pesando ",
          sprintf("%.2f", candidate_mb),
          " MiB.\n",
          "Aumenta MAX_RECURSION_DEPTH o revisa la geometria."
        )
      )
    }

    width <- as.numeric(
      bbox["xmax"] - bbox["xmin"]
    )

    height <- as.numeric(
      bbox["ymax"] - bbox["ymin"]
    )

    if (
      !is.finite(width) ||
      !is.finite(height) ||
      width <= .Machine$double.eps ||
      height <= .Machine$double.eps
    ) {
      stop(
        "No se puede subdividir mas una ventana sobredimensionada."
      )
    }

    cat(
      sprintf(
        "      subdividiendo %.2f MiB | profundidad %d\n",
        candidate_mb,
        depth + 1L
      )
    )

    gc(verbose = FALSE)

    children <- split_bbox_four(
      bbox
    )

    for (child in children) {
      process_bbox(
        child,
        depth = depth + 1L
      )
    }

    invisible(NULL)
  }


  estimated_parts <- max(
    2L,
    ceiling(
      estimated_layer_mb /
        TARGET_TILE_MB
    )
  )

  initial_grid <- make_initial_grid(
    bbox = layer_bbox_value,
    estimated_parts = estimated_parts
  )

  cat(
    "    Grilla inicial: ",
    length(initial_grid),
    " ventanas\n",
    sep = ""
  )

  for (i in seq_along(initial_grid)) {

    cat(
      sprintf(
        "    [%d/%d] ",
        i,
        length(initial_grid)
      )
    )

    process_bbox(
      initial_grid[[i]],
      depth = 0L
    )
  }

  if (length(index_rows) == 0L) {
    stop(
      paste0(
        "No se genero ninguna tesela para la capa: ",
        layer_name
      )
    )
  }

  index_sf <- do.call(
    rbind,
    index_rows
  )

  index_file <- file.path(
    output_dir,
    "index.gpkg"
  )

  remove_if_exists(index_file)

  sf::st_write(
    index_sf,
    index_file,
    layer = "tiles",
    driver = "GPKG",
    quiet = TRUE,
    delete_dsn = TRUE,
    layer_options = c(
      "SPATIAL_INDEX=YES"
    )
  )

  manifest <- do.call(
    rbind,
    manifest_rows
  )

  utils::write.csv(
    manifest,
    file.path(
      output_dir,
      "tile_manifest.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  max_written_mb <- max(
    manifest$SIZE_MB,
    na.rm = TRUE
  )

  list(
    n_tiles = nrow(manifest),
    max_tile_mb = max_written_mb,
    index_file = index_file,
    manifest = manifest
  )
}


# ============================================================
# 9. PROCESAR UN GPKG
# ============================================================

process_gpkg <- function(gpkg) {

  source_mb <- file_mb(gpkg)

  rel_source <- path_relative_to(
    gpkg,
    LAYERS_ROOT
  )

  cat(
    "\n------------------------------------------------------------\n",
    rel_source,
    "\nTamano: ",
    sprintf("%.2f", source_mb),
    " MiB\n",
    sep = ""
  )

  if (
    !is.finite(source_mb) ||
    source_mb <= 0
  ) {
    warning(
      paste0(
        "No se pudo determinar el tamano de:\n",
        gpkg
      )
    )

    return(
      data.frame(
        SOURCE = rel_source,
        SOURCE_SIZE_MB = source_mb,
        STATUS = "invalid_size",
        OUTPUT_DIR = NA_character_,
        N_TILES = NA_integer_,
        MAX_TILE_MB = NA_real_,
        stringsAsFactors = FALSE
      )
    )
  }

  if (source_mb <= MAX_SOURCE_MB) {

    cat(
      "  <= ",
      MAX_SOURCE_MB,
      " MiB: se conserva sin teselar.\n",
      sep = ""
    )

    return(
      data.frame(
        SOURCE = rel_source,
        SOURCE_SIZE_MB = round(source_mb, 3),
        STATUS = "small_keep_as_is",
        OUTPUT_DIR = NA_character_,
        N_TILES = 1L,
        MAX_TILE_MB = round(source_mb, 3),
        stringsAsFactors = FALSE
      )
    )
  }

  source_stem <- tools::file_path_sans_ext(
    basename(gpkg)
  )

  base_output_dir <- file.path(
    dirname(gpkg),
    paste0(
      source_stem,
      TILED_SUFFIX
    )
  )

  ready_file <- file.path(
    base_output_dir,
    "TILING_READY.txt"
  )

  if (
    dir.exists(base_output_dir) &&
    file.exists(ready_file) &&
    !isTRUE(OVERWRITE_OUTPUT)
  ) {

    manifest_file <- file.path(
      base_output_dir,
      "tile_manifest.csv"
    )

    old_manifest <- if (file.exists(manifest_file)) {
      tryCatch(
        utils::read.csv(
          manifest_file,
          stringsAsFactors = FALSE
        ),
        error = function(e) NULL
      )
    } else {
      NULL
    }

    n_tiles <- if (!is.null(old_manifest)) {
      nrow(old_manifest)
    } else {
      NA_integer_
    }

    max_tile <- if (
      !is.null(old_manifest) &&
      "SIZE_MB" %in% names(old_manifest)
    ) {
      max(
        old_manifest$SIZE_MB,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }

    cat(
      "  Ya existe una salida terminada. Se omite.\n"
    )

    return(
      data.frame(
        SOURCE = rel_source,
        SOURCE_SIZE_MB = round(source_mb, 3),
        STATUS = "already_tiled",
        OUTPUT_DIR = path_relative_to(
          base_output_dir,
          LAYERS_ROOT
        ),
        N_TILES = n_tiles,
        MAX_TILE_MB = max_tile,
        stringsAsFactors = FALSE
      )
    )
  }

  if (dir.exists(base_output_dir)) {

    if (
      !isTRUE(OVERWRITE_OUTPUT) &&
      !isTRUE(RESTART_INCOMPLETE_OUTPUT)
    ) {
      stop(
        paste0(
          "Existe una salida incompleta para:\n",
          gpkg,
          "\n\nRevisa o elimina:\n",
          base_output_dir,
          "\nO establece RESTART_INCOMPLETE_OUTPUT <- TRUE."
        )
      )
    }

    cat(
      "  Eliminando salida incompleta previa y reiniciando...\n"
    )

    unlink(
      base_output_dir,
      recursive = TRUE,
      force = TRUE
    )
  }

  dir.create(
    base_output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  layers <- read_gpkg_feature_layers(
    gpkg
  )

  if (nrow(layers) == 0L) {
    stop(
      paste0(
        "El GPKG no contiene capas vectoriales 'features':\n",
        gpkg
      )
    )
  }

  counts <- layer_feature_counts(
    gpkg
  )

  # Los normalizados del proyecto normalmente tienen una capa.
  # Si hay varias, se procesan de forma independiente.
  layer_results <- list()
  combined_manifest <- list()

  total_features <- if (
    !is.null(counts) &&
    any(is.finite(counts$features))
  ) {
    sum(
      counts$features[
        is.finite(counts$features)
      ],
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  for (i in seq_len(nrow(layers))) {

    layer_name <- as.character(
      layers$table_name[i]
    )

    cat(
      "\n  Capa: ",
      layer_name,
      "\n",
      sep = ""
    )

    bbox_i <- layer_bbox(
      gpkg = gpkg,
      layer_row = layers[i, , drop = FALSE],
      layer_name = layer_name
    )

    layer_output_dir <- if (nrow(layers) == 1L) {
      base_output_dir
    } else {
      file.path(
        base_output_dir,
        safe_name(layer_name)
      )
    }

    dir.create(
      layer_output_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    estimated_layer_mb <- source_mb / nrow(layers)

    if (
      !is.null(counts) &&
      is.finite(total_features) &&
      total_features > 0
    ) {

      hit <- match(
        layer_name,
        counts$layer
      )

      if (
        !is.na(hit) &&
        is.finite(counts$features[hit])
      ) {
        estimated_layer_mb <- source_mb *
          counts$features[hit] /
          total_features
      }
    }

    result_i <- split_one_layer(
      gpkg = gpkg,
      layer_name = layer_name,
      layer_bbox_value = bbox_i,
      output_dir = layer_output_dir,
      source_stem = if (nrow(layers) == 1L) {
        source_stem
      } else {
        paste0(
          source_stem,
          "_",
          safe_name(layer_name)
        )
      },
      estimated_layer_mb = estimated_layer_mb
    )

    layer_results[[layer_name]] <- result_i

    manifest_i <- result_i$manifest
    manifest_i$LAYER_OUTPUT_DIR <- path_relative_to(
      layer_output_dir,
      LAYERS_ROOT
    )

    combined_manifest[[length(combined_manifest) + 1L]] <- manifest_i
  }

  all_manifest <- do.call(
    rbind,
    combined_manifest
  )

  utils::write.csv(
    all_manifest,
    file.path(
      base_output_dir,
      "tile_manifest.csv"
    ),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  all_tile_files <- file.path(
    LAYERS_ROOT,
    all_manifest$RELATIVE_PATH
  )

  actual_sizes <- vapply(
    all_tile_files,
    file_mb,
    numeric(1)
  )

  if (
    any(!is.finite(actual_sizes)) ||
    any(actual_sizes > MAX_TILE_MB)
  ) {
    stop(
      paste0(
        "Validacion final fallida para:\n",
        gpkg,
        "\nHay teselas ausentes o mayores de ",
        MAX_TILE_MB,
        " MiB."
      )
    )
  }

  writeLines(
    c(
      paste("Completed:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste("Source:", rel_source),
      paste("Source size MiB:", sprintf("%.3f", source_mb)),
      paste("Target tile MiB:", TARGET_TILE_MB),
      paste("Maximum tile MiB:", MAX_TILE_MB),
      paste("Number of tiles:", nrow(all_manifest)),
      paste("Largest tile MiB:", sprintf("%.3f", max(actual_sizes))),
      paste("Original preserved:", !DELETE_ORIGINAL_AFTER_SUCCESS),
      "Status: OK"
    ),
    ready_file,
    useBytes = TRUE
  )

  if (isTRUE(DELETE_ORIGINAL_AFTER_SUCCESS)) {

    deleted <- file.remove(gpkg)

    if (!isTRUE(deleted)) {
      warning(
        paste0(
          "Teselado correcto, pero no se pudo borrar el original:\n",
          gpkg
        )
      )
    }
  }

  cat(
    "\n  COMPLETADO: ",
    nrow(all_manifest),
    " teselas | max ",
    sprintf("%.2f", max(actual_sizes)),
    " MiB\n",
    sep = ""
  )

  data.frame(
    SOURCE = rel_source,
    SOURCE_SIZE_MB = round(source_mb, 3),
    STATUS = "tiled_ok",
    OUTPUT_DIR = path_relative_to(
      base_output_dir,
      LAYERS_ROOT
    ),
    N_TILES = nrow(all_manifest),
    MAX_TILE_MB = round(max(actual_sizes), 3),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 10. DESCUBRIR GPKG
# ============================================================

all_gpkg <- list.files(
  LAYERS_ROOT,
  pattern = "\\.gpkg$",
  full.names = TRUE,
  recursive = TRUE,
  ignore.case = TRUE
)

all_gpkg <- sort(
  normalizePath(
    all_gpkg,
    winslash = "/",
    mustWork = TRUE
  )
)

all_gpkg <- all_gpkg[
  !vapply(
    all_gpkg,
    is_generated_path,
    logical(1)
  )
]

if (length(all_gpkg) == 0L) {
  stop(
    paste0(
      "No se encontraron GPKG bajo:\n",
      LAYERS_ROOT
    )
  )
}

cat(
  "GPKG encontrados: ",
  length(all_gpkg),
  "\n",
  sep = ""
)


# ============================================================
# 11. EJECUCION
# ============================================================

root_results <- list()

for (i in seq_along(all_gpkg)) {

  cat(
    "\n============================================================\n",
    "ARCHIVO ",
    i,
    "/",
    length(all_gpkg),
    "\n",
    sep = ""
  )

  result_i <- tryCatch(
    process_gpkg(
      all_gpkg[i]
    ),
    error = function(e) {

      warning(
        paste0(
          "ERROR en:\n",
          all_gpkg[i],
          "\n",
          conditionMessage(e)
        )
      )

      data.frame(
        SOURCE = path_relative_to(
          all_gpkg[i],
          LAYERS_ROOT
        ),
        SOURCE_SIZE_MB = file_mb(all_gpkg[i]),
        STATUS = paste0(
          "ERROR: ",
          conditionMessage(e)
        ),
        OUTPUT_DIR = NA_character_,
        N_TILES = NA_integer_,
        MAX_TILE_MB = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  )

  root_results[[length(root_results) + 1L]] <- result_i
}

summary_table <- do.call(
  rbind,
  root_results
)

summary_file <- file.path(
  LAYERS_ROOT,
  "layers_tiling_summary.csv"
)

utils::write.csv(
  summary_table,
  summary_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat(
  "\n============================================================\n",
  "TERMINADO\n",
  "Resumen:\n",
  summary_file,
  "\n============================================================\n",
  sep = ""
)

print(
  summary_table,
  row.names = FALSE
)
