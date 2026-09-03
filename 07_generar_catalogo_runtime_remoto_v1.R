# ============================================================
# 07_generar_catalogo_runtime_remoto_v2_fix_ids.R
#
# FABDEM Watershed Explorer
# Catalogo liviano para la app remota
# ============================================================
#
# EJECUTAR DESDE:
#   FABDEM_Watershed_Runtime/
#
# ESTRUCTURA ESPERADA:
#
#   FABDEM_Watershed_Runtime/
#   └── core/
#       ├── BLOCK_001/
#       ├── ...
#       └── BLOCK_021/
#
# SALIDA:
#
#   FABDEM_Watershed_Runtime/
#   └── posit_data/
#       ├── block_lookup.gpkg
#       ├── block_metadata.csv
#       ├── remote_manifest.csv
#       ├── remote_sources.csv
#       └── CATALOG_READY.txt
#
# Estos archivos son pequeños y son los que luego se copian
# dentro de data/ de la app desplegada en Posit.
#
# IMPORTANTE:
# - NO sube nada a Internet.
# - NO recalcula hidrologia.
# - NO necesita DEM.
# - NO usa STREAM_MASK.tif.
# - Valida que reverse_stripes y stream_stripes correspondan.
# ============================================================


# ============================================================
# 1. CONFIGURACION
# ============================================================

SOURCE_REGISTRY_GPKG <- paste0(
  "F:/DEM - copia/BLOQUES_HIDROLOGICOS_FINAL/",
  "WATERSHED_INDEX/watershed_explorer_registry.gpkg"
)

REGISTRY_LAYER <- "basins"

CORE_DIR_NAME <- "core"

POSIT_DATA_DIR_NAME <- "posit_data"

EXPECTED_N_BLOCKS <- 21L

DEFAULT_SOURCE_ID <- "SOURCE_1"

DEFAULT_BASE_URL <- paste0(
  "https://media.githubusercontent.com/media/",
  "JamilRamirez/FABDEM-Watershed-Runtime/main/"
)


# ============================================================
# 2. PAQUETES
# ============================================================

if (!requireNamespace("sf", quietly = TRUE)) {
  stop(
    "Falta el paquete sf."
  )
}

if (!requireNamespace("terra", quietly = TRUE)) {
  stop(
    "Falta el paquete terra."
  )
}


# ============================================================
# 3. RUTAS
# ============================================================

RUNTIME_ROOT <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

CORE_DIR <- file.path(
  RUNTIME_ROOT,
  CORE_DIR_NAME
)

POSIT_DATA_DIR <- file.path(
  RUNTIME_ROOT,
  POSIT_DATA_DIR_NAME
)


if (!dir.exists(CORE_DIR)) {
  stop(
    paste0(
      "No existe:\n",
      CORE_DIR,
      "\n\nEjecuta este script desde FABDEM_Watershed_Runtime/."
    )
  )
}


dir.create(
  POSIT_DATA_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. HELPERS
# ============================================================

file_nonempty <- function(path) {

  if (
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(path) ||
    !file.exists(path)
  ) {
    return(
      FALSE
    )
  }


  info <- file.info(
    path
  )


  isTRUE(
    is.finite(
      info$size
    ) &&
      info$size > 0
  )
}


relative_path <- function(path) {

  root <- normalizePath(
    RUNTIME_ROOT,
    winslash = "/",
    mustWork = TRUE
  )

  x <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )


  prefix <- paste0(
    root,
    "/"
  )


  if (!startsWith(x, prefix)) {
    stop(
      paste0(
        "El archivo no esta dentro del runtime:\n",
        x
      )
    )
  }


  substring(
    x,
    nchar(prefix) + 1L
  )
}


read_csv_auto <- function(path) {

  first_line <- readLines(
    path,
    n = 1L,
    warn = FALSE
  )


  if (
    length(first_line) == 1L &&
    grepl(
      ";",
      first_line,
      fixed = TRUE
    ) &&
    !grepl(
      ",",
      first_line,
      fixed = TRUE
    )
  ) {
    return(
      read.csv2(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )
  }


  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}


get_single_value <- function(
    x,
    field,
    default = NA
) {

  if (
    is.null(x) ||
    nrow(x) < 1L ||
    !field %in% names(x)
  ) {
    return(
      default
    )
  }


  x[[field]][1]
}


parse_stripe_id <- function(path, prefix) {

  name <- basename(
    path
  )


  pattern <- paste0(
    "^",
    prefix,
    "_([0-9]{5})\\.tif$"
  )


  m <- regexec(
    pattern,
    name
  )

  hit <- regmatches(
    name,
    m
  )[[1]]


  if (length(hit) != 2L) {
    stop(
      paste0(
        "Nombre de franja inesperado: ",
        name
      )
    )
  }


  as.integer(
    hit[2]
  )
}


make_asset_row <- function(
    block_id,
    asset_type,
    path,
    stripe_id = NA_integer_,
    row_start = NA_integer_,
    row_end = NA_integer_,
    nrows = NA_integer_,
    ncols = NA_integer_,
    source_id = DEFAULT_SOURCE_ID
) {

  info <- file.info(
    path
  )


  data.frame(
    BLOCK_ID = block_id,
    ASSET_TYPE = asset_type,
    STRIPE_ID = stripe_id,
    ROW_START = row_start,
    ROW_END = row_end,
    NROWS = nrows,
    NCOLS = ncols,
    SOURCE_ID = source_id,
    RELATIVE_PATH = gsub(
      "\\\\",
      "/",
      relative_path(
        path
      )
    ),
    SIZE_BYTES = as.numeric(
      info$size
    ),
    REMOTE_URL = "",
    stringsAsFactors = FALSE
  )
}


# ============================================================
# 5. INVENTARIO DE BLOQUES
# ============================================================

block_dirs <- list.dirs(
  CORE_DIR,
  full.names = TRUE,
  recursive = FALSE
)

block_dirs <- block_dirs[
  grepl(
    "^BLOCK_[0-9]+$",
    basename(
      block_dirs
    )
  )
]

block_dirs <- sort(
  block_dirs
)


if (length(block_dirs) == 0L) {
  stop(
    "No se encontraron BLOCK_xxx dentro de core/."
  )
}


if (
  !is.null(EXPECTED_N_BLOCKS) &&
  length(block_dirs) != EXPECTED_N_BLOCKS
) {
  stop(
    paste0(
      "Se esperaban ",
      EXPECTED_N_BLOCKS,
      " bloques y se encontraron ",
      length(block_dirs),
      "."
    )
  )
}


block_ids <- basename(
  block_dirs
)


cat(
  "\nBloques runtime: ",
  length(block_ids),
  "\n",
  paste(
    block_ids,
    collapse = ", "
  ),
  "\n\n",
  sep = ""
)


# ============================================================
# 6. VALIDAR Y CONSTRUIR MANIFEST + METADATA
# ============================================================

manifest_rows <- list()

metadata_rows <- list()


for (block_dir in block_dirs) {

  block_id <- basename(
    block_dir
  )

  index_dir <- file.path(
    block_dir,
    "index"
  )

  reverse_dir <- file.path(
    index_dir,
    "reverse_stripes"
  )

  stream_dir <- file.path(
    index_dir,
    "stream_stripes"
  )

  stream_manifest_file <- file.path(
    index_dir,
    "stream_stripes_manifest.csv"
  )

  stream_ready_file <- file.path(
    index_dir,
    "STREAM_STRIPES_READY.txt"
  )

  index_metadata_file <- file.path(
    index_dir,
    "index_metadata.rds"
  )

  run_info_file <- file.path(
    block_dir,
    "run_info.csv"
  )

  network_file <- file.path(
    block_dir,
    "hydrography_block.gpkg"
  )


  cat(
    "Validando ",
    block_id,
    "...\n",
    sep = ""
  )


  required <- c(
    stream_manifest_file,
    stream_ready_file,
    index_metadata_file,
    run_info_file
  )


  missing_required <- required[
    !vapply(
      required,
      file_nonempty,
      logical(1)
    )
  ]


  if (length(missing_required) > 0L) {
    stop(
      paste0(
        block_id,
        ": faltan archivos:\n",
        paste(
          missing_required,
          collapse = "\n"
        )
      )
    )
  }


  reverse_files <- sort(
    list.files(
      reverse_dir,
      pattern = "^reverse_[0-9]{5}\\.tif$",
      full.names = TRUE
    )
  )

  stream_files <- sort(
    list.files(
      stream_dir,
      pattern = "^stream_[0-9]{5}\\.tif$",
      full.names = TRUE
    )
  )


  if (
    length(reverse_files) == 0L ||
    length(stream_files) == 0L
  ) {
    stop(
      paste0(
        block_id,
        ": faltan reverse_stripes o stream_stripes."
      )
    )
  }


  reverse_ids <- unname(
    vapply(
      reverse_files,
      parse_stripe_id,
      integer(1),
      prefix = "reverse"
    )
  )

  stream_ids <- unname(
    vapply(
      stream_files,
      parse_stripe_id,
      integer(1),
      prefix = "stream"
    )
  )


  if (!identical(
    reverse_ids,
    stream_ids
  )) {
    stop(
      paste0(
        block_id,
        ": IDs de reverse y stream no coinciden realmente. ",
        "reverse=[",
        paste(reverse_ids, collapse = ","),
        "] stream=[",
        paste(stream_ids, collapse = ","),
        "]."
      )
    )
  }


  stripe_manifest <- read_csv_auto(
    stream_manifest_file
  )


  required_manifest_fields <- c(
    "BLOCK_ID",
    "STRIPE_ID",
    "ROW_START",
    "ROW_END",
    "NROWS",
    "NCOLS"
  )


  missing_fields <- required_manifest_fields[
    !required_manifest_fields %in%
      names(
        stripe_manifest
      )
  ]


  if (length(missing_fields) > 0L) {
    stop(
      paste0(
        block_id,
        ": stream_stripes_manifest.csv no contiene: ",
        paste(
          missing_fields,
          collapse = ", "
        )
      )
    )
  }


  stripe_manifest <- stripe_manifest[
    order(
      stripe_manifest[["STRIPE_ID"]]
    ),
    ,
    drop = FALSE
  ]


  manifest_ids <- unname(
    as.integer(
      stripe_manifest[["STRIPE_ID"]]
    )
  )


  if (!identical(
    manifest_ids,
    reverse_ids
  )) {
    stop(
      paste0(
        block_id,
        ": STRIPE_ID del manifest no coincide con archivos."
      )
    )
  }


  metadata <- readRDS(
    index_metadata_file
  )

  run_info <- read_csv_auto(
    run_info_file
  )


  nrows <- suppressWarnings(
    as.integer(
      metadata[["nrows"]]
    )
  )

  ncols <- suppressWarnings(
    as.integer(
      metadata[["ncols"]]
    )
  )

  n_stripes <- suppressWarnings(
    as.integer(
      metadata[["n_stripes"]]
    )
  )

  stripe_rows <- suppressWarnings(
    as.integer(
      metadata[["stripe_rows"]]
    )
  )


  if (
    !is.finite(nrows) ||
    !is.finite(ncols) ||
    !is.finite(n_stripes) ||
    nrows < 1L ||
    ncols < 1L ||
    n_stripes < 1L
  ) {
    stop(
      paste0(
        block_id,
        ": index_metadata.rds tiene dimensiones invalidas."
      )
    )
  }


  if (
    n_stripes != length(reverse_files)
  ) {
    stop(
      paste0(
        block_id,
        ": n_stripes de metadata=",
        n_stripes,
        " pero hay ",
        length(reverse_files),
        " archivos."
      )
    )
  }


  if (
    sum(
      as.integer(
        stripe_manifest[["NROWS"]]
      )
    ) != nrows
  ) {
    stop(
      paste0(
        block_id,
        ": la suma de NROWS de las franjas no coincide con nrows."
      )
    )
  }


  extent_values <- metadata[["extent"]]

  resolution_values <- metadata[["resolution"]]


  if (
    is.null(extent_values) ||
    length(extent_values) != 4L ||
    is.null(resolution_values) ||
    length(resolution_values) != 2L
  ) {
    stop(
      paste0(
        block_id,
        ": extent/resolution faltantes en index_metadata.rds."
      )
    )
  }


  threshold_cells <- suppressWarnings(
    as.numeric(
      get_single_value(
        run_info,
        "STREAM_THRESHOLD_CELLS",
        NA_real_
      )
    )
  )

  threshold_km2 <- suppressWarnings(
    as.numeric(
      get_single_value(
        run_info,
        "STREAM_THRESHOLD_KM2",
        NA_real_
      )
    )
  )


  metadata_rows[[block_id]] <- data.frame(
    BLOCK_ID = block_id,
    ENGINE = as.character(
      metadata[["engine"]]
    ),
    REGION = as.character(
      metadata[["region"]]
    ),
    MODE = as.character(
      metadata[["mode"]]
    ),
    NROWS = nrows,
    NCOLS = ncols,
    RES_X = as.numeric(
      resolution_values[1]
    ),
    RES_Y = as.numeric(
      resolution_values[2]
    ),
    XMIN = as.numeric(
      extent_values[["xmin"]]
    ),
    XMAX = as.numeric(
      extent_values[["xmax"]]
    ),
    YMIN = as.numeric(
      extent_values[["ymin"]]
    ),
    YMAX = as.numeric(
      extent_values[["ymax"]]
    ),
    CRS = as.character(
      metadata[["crs"]]
    ),
    STRIPE_ROWS = stripe_rows,
    N_STRIPES = n_stripes,
    STREAM_THRESHOLD_CELLS = threshold_cells,
    STREAM_THRESHOLD_KM2 = threshold_km2,
    INDEX_ALGORITHM = as.character(
      metadata[["algorithm_version"]]
    ),
    stringsAsFactors = FALSE
  )


  for (i in seq_along(reverse_files)) {

    row_info <- stripe_manifest[
      i,
      ,
      drop = FALSE
    ]


    manifest_rows[[
      length(
        manifest_rows
      ) + 1L
    ]] <- make_asset_row(
      block_id = block_id,
      asset_type = "reverse",
      path = reverse_files[[i]],
      stripe_id = reverse_ids[[i]],
      row_start = as.integer(
        row_info[["ROW_START"]][1]
      ),
      row_end = as.integer(
        row_info[["ROW_END"]][1]
      ),
      nrows = as.integer(
        row_info[["NROWS"]][1]
      ),
      ncols = as.integer(
        row_info[["NCOLS"]][1]
      )
    )


    manifest_rows[[
      length(
        manifest_rows
      ) + 1L
    ]] <- make_asset_row(
      block_id = block_id,
      asset_type = "stream",
      path = stream_files[[i]],
      stripe_id = stream_ids[[i]],
      row_start = as.integer(
        row_info[["ROW_START"]][1]
      ),
      row_end = as.integer(
        row_info[["ROW_END"]][1]
      ),
      nrows = as.integer(
        row_info[["NROWS"]][1]
      ),
      ncols = as.integer(
        row_info[["NCOLS"]][1]
      )
    )
  }


  manifest_rows[[
    length(
      manifest_rows
    ) + 1L
  ]] <- make_asset_row(
    block_id = block_id,
    asset_type = "index_metadata",
    path = index_metadata_file
  )


  manifest_rows[[
    length(
      manifest_rows
    ) + 1L
  ]] <- make_asset_row(
    block_id = block_id,
    asset_type = "run_info",
    path = run_info_file
  )


  if (file_nonempty(network_file)) {

    manifest_rows[[
      length(
        manifest_rows
      ) + 1L
    ]] <- make_asset_row(
      block_id = block_id,
      asset_type = "hydrography",
      path = network_file
    )
  }


  cat(
    "  OK | ",
    n_stripes,
    " reverse + ",
    n_stripes,
    " stream\n",
    sep = ""
  )
}


remote_manifest <- do.call(
  rbind,
  manifest_rows
)

block_metadata <- do.call(
  rbind,
  metadata_rows
)


remote_manifest <- remote_manifest[
  order(
    remote_manifest[["BLOCK_ID"]],
    remote_manifest[["ASSET_TYPE"]],
    remote_manifest[["STRIPE_ID"]],
    na.last = TRUE
  ),
  ,
  drop = FALSE
]


block_metadata <- block_metadata[
  order(
    block_metadata[["BLOCK_ID"]]
  ),
  ,
  drop = FALSE
]


# ============================================================
# 7. BLOCK LOOKUP GPKG
# ============================================================

if (!file_nonempty(SOURCE_REGISTRY_GPKG)) {
  stop(
    paste0(
      "No existe SOURCE_REGISTRY_GPKG:\n",
      SOURCE_REGISTRY_GPKG
    )
  )
}


available_layers <- sf::st_layers(
  SOURCE_REGISTRY_GPKG
)$name


if (!REGISTRY_LAYER %in% available_layers) {
  stop(
    paste0(
      "El GPKG no contiene la capa ",
      REGISTRY_LAYER,
      "."
    )
  )
}


basins <- sf::st_read(
  SOURCE_REGISTRY_GPKG,
  layer = REGISTRY_LAYER,
  quiet = TRUE
)


if (!"BLOCK_ID" %in% names(basins)) {
  stop(
    "La capa basins no contiene BLOCK_ID."
  )
}


basins <- basins[
  as.character(
    basins[["BLOCK_ID"]]
  ) %in% block_ids,
  ,
  drop = FALSE
]


basins <- sf::st_make_valid(
  basins
)


basins <- basins[
  !sf::st_is_empty(
    basins
  ),
  ,
  drop = FALSE
]


lookup_rows <- lapply(
  block_ids,
  function(block_id) {

    x <- basins[
      as.character(
        basins[["BLOCK_ID"]]
      ) == block_id,
      ,
      drop = FALSE
    ]


    if (nrow(x) == 0L) {
      stop(
        paste0(
          "No hay geometria de lookup para ",
          block_id,
          "."
        )
      )
    }


    geom <- sf::st_union(
      sf::st_geometry(
        x
      )
    )


    sf::st_sf(
      BLOCK_ID = block_id,
      geometry = geom
    )
  }
)


block_lookup <- do.call(
  rbind,
  lookup_rows
)


block_lookup <- sf::st_make_valid(
  block_lookup
)


if (nrow(block_lookup) != length(block_ids)) {
  stop(
    "block_lookup no tiene una geometria por bloque."
  )
}


if (!setequal(
  as.character(
    block_lookup[["BLOCK_ID"]]
  ),
  block_ids
)) {
  stop(
    "BLOCK_ID de block_lookup no coincide con runtime."
  )
}


# ============================================================
# 8. ESCRIBIR CATALOGO
# ============================================================

manifest_out <- file.path(
  POSIT_DATA_DIR,
  "remote_manifest.csv"
)

metadata_out <- file.path(
  POSIT_DATA_DIR,
  "block_metadata.csv"
)

sources_out <- file.path(
  POSIT_DATA_DIR,
  "remote_sources.csv"
)

lookup_out <- file.path(
  POSIT_DATA_DIR,
  "block_lookup.gpkg"
)


write.csv(
  remote_manifest,
  manifest_out,
  row.names = FALSE
)


write.csv(
  block_metadata,
  metadata_out,
  row.names = FALSE
)


remote_sources <- data.frame(
  SOURCE_ID = DEFAULT_SOURCE_ID,
  BASE_URL = DEFAULT_BASE_URL,
  NOTES = "Archivos publicados en GitHub mediante Git LFS.",
  stringsAsFactors = FALSE
)


write.csv(
  remote_sources,
  sources_out,
  row.names = FALSE
)


if (file.exists(lookup_out)) {
  unlink(
    lookup_out,
    force = TRUE
  )
}


sf::st_write(
  block_lookup,
  lookup_out,
  layer = "blocks",
  quiet = TRUE
)


# ============================================================
# 9. VALIDACION FINAL
# ============================================================

expected_stripes <- sum(
  block_metadata[["N_STRIPES"]]
)


n_reverse <- sum(
  remote_manifest[["ASSET_TYPE"]] == "reverse"
)

n_stream <- sum(
  remote_manifest[["ASSET_TYPE"]] == "stream"
)


if (
  n_reverse != expected_stripes ||
  n_stream != expected_stripes
) {
  stop(
    paste0(
      "Conteo final inconsistente. Esperadas ",
      expected_stripes,
      " franjas por tipo; reverse=",
      n_reverse,
      ", stream=",
      n_stream,
      "."
    )
  )
}


if (any(
  duplicated(
    remote_manifest[
      ,
      c(
        "BLOCK_ID",
        "ASSET_TYPE",
        "STRIPE_ID",
        "RELATIVE_PATH"
      ),
      drop = FALSE
    ]
  )
)) {
  stop(
    "remote_manifest contiene filas duplicadas."
  )
}


total_bytes <- sum(
  remote_manifest[["SIZE_BYTES"]],
  na.rm = TRUE
)


ready_file <- file.path(
  POSIT_DATA_DIR,
  "CATALOG_READY.txt"
)


writeLines(
  c(
    paste(
      "Completed:",
      format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      )
    ),
    paste(
      "Blocks:",
      nrow(
        block_metadata
      )
    ),
    paste(
      "Reverse stripes:",
      n_reverse
    ),
    paste(
      "Stream stripes:",
      n_stream
    ),
    paste(
      "Manifest rows:",
      nrow(
        remote_manifest
      )
    ),
    paste(
      "Logical runtime assets GB:",
      sprintf(
        "%.3f",
        total_bytes /
          1024^3
      )
    ),
    "Remote base URL completed: YES",
    "Hydrology recalculated: NO",
    "Status: READY FOR GITHUB PUBLICATION"
  ),
  ready_file
)


cat(
  "\n\n=============================================\n",
  "CATALOGO RUNTIME GENERADO\n",
  "=============================================\n",
  "Bloques: ",
  nrow(
    block_metadata
  ),
  "\n",
  "Reverse stripes: ",
  n_reverse,
  "\n",
  "Stream stripes: ",
  n_stream,
  "\n",
  "Filas manifest: ",
  nrow(
    remote_manifest
  ),
  "\n",
  "Tamano logico catalogado: ",
  sprintf(
    "%.3f",
    total_bytes /
      1024^3
  ),
  " GB\n\n",
  "Copia despues TODO el contenido de:\n",
  POSIT_DATA_DIR,
  "\n\na data/ de la app Posit.\n\n",
    "La URL base de GitHub queda incluida en remote_sources.csv.\n",
  sep = ""
)
