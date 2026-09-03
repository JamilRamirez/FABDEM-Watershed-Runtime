# ============================================================
# 06_generar_stream_stripes_runtime_v1.R
#
# FABDEM Watershed Explorer
# Preparar STREAM_MASK para acceso remoto por franjas
# ============================================================
#
# OBJETIVO
# ------------------------------------------------------------
# Para cada BLOCK_xxx:
#
#   index/STREAM_MASK.tif
#          +
#   index/reverse_stripes/reverse_XXXXX.tif
#
# genera:
#
#   index/stream_stripes/stream_XXXXX.tif
#
# Cada stream_XXXXX.tif:
# - corresponde EXACTAMENTE a la misma franja de filas que
#   reverse_XXXXX.tif;
# - tiene la misma geometria, dimensiones y CRS;
# - contiene solo 0/1;
# - NO resamplea;
# - NO interpola;
# - NO recalcula hidrologia.
#
# USO
# ------------------------------------------------------------
# Coloca este R:
#
# A) en la carpeta que contiene directamente BLOCK_001...BLOCK_021
#
# o
#
# B) en FABDEM_Watershed_Runtime/
#    si los bloques estan dentro de ./core/
#
# El script detecta ambas estructuras automaticamente.
#
# IMPORTANTE
# ------------------------------------------------------------
# DELETE_STREAM_MASK_AFTER_VALIDATION = FALSE por defecto.
# Primero verifica las franjas. Si luego quieres retirar el
# STREAM_MASK completo de la COPIA RUNTIME, cambia a TRUE y
# vuelve a ejecutar. Nunca afecta tu copia maestra si estas
# trabajando sobre una copia separada.
# ============================================================


# ============================================================
# 1. CONFIGURACION
# ============================================================

BLOCKS_ONLY <- NULL

DELETE_STREAM_MASK_AFTER_VALIDATION <- TRUE

OVERWRITE_INVALID <- TRUE

STREAM_DIR_NAME <- "stream_stripes"

GEOM_TOL_CELLS <- 1e-7


# ============================================================
# 2. PAQUETES
# ============================================================

if (!requireNamespace("terra", quietly = TRUE)) {
  stop(
    "Falta el paquete terra."
  )
}


# ============================================================
# 3. DETECTAR RAIZ DE BLOQUES
# ============================================================

SCRIPT_ROOT <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)


find_block_dirs <- function(root) {

  immediate <- list.dirs(
    root,
    full.names = TRUE,
    recursive = FALSE
  )

  immediate <- immediate[
    grepl(
      "^BLOCK_[0-9]+$",
      basename(
        immediate
      )
    )
  ]


  if (length(immediate) > 0L) {
    return(
      sort(
        immediate
      )
    )
  }


  core_dir <- file.path(
    root,
    "core"
  )


  if (dir.exists(core_dir)) {

    core_blocks <- list.dirs(
      core_dir,
      full.names = TRUE,
      recursive = FALSE
    )

    core_blocks <- core_blocks[
      grepl(
        "^BLOCK_[0-9]+$",
        basename(
          core_blocks
        )
      )
    ]


    if (length(core_blocks) > 0L) {
      return(
        sort(
          core_blocks
        )
      )
    }
  }


  candidates <- list.dirs(
    root,
    full.names = TRUE,
    recursive = TRUE
  )

  candidates <- candidates[
    grepl(
      "^BLOCK_[0-9]+$",
      basename(
        candidates
      )
    )
  ]

  candidates <- candidates[
    vapply(
      candidates,
      function(folder) {
        file.exists(
          file.path(
            folder,
            "index",
            "STREAM_MASK.tif"
          )
        )
      },
      logical(1)
    )
  ]


  sort(
    unique(
      candidates
    )
  )
}


block_dirs <- find_block_dirs(
  SCRIPT_ROOT
)


if (length(block_dirs) == 0L) {
  stop(
    paste0(
      "No se encontraron carpetas BLOCK_xxx con index/STREAM_MASK.tif.\n",
      "Carpeta actual: ",
      SCRIPT_ROOT
    )
  )
}


if (!is.null(BLOCKS_ONLY)) {

  wanted <- toupper(
    trimws(
      as.character(
        BLOCKS_ONLY
      )
    )
  )


  block_dirs <- block_dirs[
    basename(
      block_dirs
    ) %in% wanted
  ]


  if (length(block_dirs) == 0L) {
    stop(
      "BLOCKS_ONLY no coincide con ningun bloque encontrado."
    )
  }
}


cat(
  "\nRaiz de trabajo: ",
  SCRIPT_ROOT,
  "\n",
  "Bloques encontrados: ",
  length(block_dirs),
  "\n\n",
  sep = ""
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


extent_vector <- function(r) {

  c(
    xmin = terra::xmin(r),
    xmax = terra::xmax(r),
    ymin = terra::ymin(r),
    ymax = terra::ymax(r)
  )
}


same_crs <- function(a, b) {

  isTRUE(
    terra::same.crs(
      a,
      b
    )
  )
}


geom_diagnostic <- function(
    reference,
    candidate
) {

  ref_ext <- extent_vector(
    reference
  )

  can_ext <- extent_vector(
    candidate
  )

  rr <- terra::res(
    reference
  )


  delta_units <- can_ext -
    ref_ext


  delta_cells <- c(
    xmin = delta_units[["xmin"]] / abs(rr[1]),
    xmax = delta_units[["xmax"]] / abs(rr[1]),
    ymin = delta_units[["ymin"]] / abs(rr[2]),
    ymax = delta_units[["ymax"]] / abs(rr[2])
  )


  list(
    same_rows = terra::nrow(reference) == terra::nrow(candidate),
    same_cols = terra::ncol(reference) == terra::ncol(candidate),
    same_crs = same_crs(reference, candidate),
    delta_units = delta_units,
    delta_cells = delta_cells,
    max_abs_cells = max(
      abs(
        delta_cells
      ),
      na.rm = TRUE
    )
  )
}


valid_stream_stripe <- function(
    path,
    reverse_r,
    tol_cells = GEOM_TOL_CELLS
) {

  if (!file_nonempty(path)) {
    return(
      FALSE
    )
  }


  out_r <- tryCatch(
    terra::rast(
      path
    ),
    error = function(e) NULL
  )


  if (is.null(out_r)) {
    return(
      FALSE
    )
  }


  d <- geom_diagnostic(
    reference = reverse_r,
    candidate = out_r
  )


  if (
    !d$same_rows ||
    !d$same_cols ||
    !d$same_crs ||
    !is.finite(d$max_abs_cells) ||
    d$max_abs_cells > tol_cells
  ) {
    return(
      FALSE
    )
  }


  sample_values <- tryCatch(
    terra::spatSample(
      out_r,
      size = min(
        10000L,
        terra::ncell(
          out_r
        )
      ),
      method = "regular",
      na.rm = FALSE,
      values = TRUE,
      as.df = FALSE
    ),
    error = function(e) NULL
  )


  if (is.null(sample_values)) {
    return(
      FALSE
    )
  }


  sample_values <- as.numeric(
    sample_values
  )


  sample_values <- sample_values[
    !is.na(
      sample_values
    )
  ]


  all(
    sample_values %in%
      c(
        0,
        1
      )
  )
}


write_stream_stripe <- function(
    mask,
    reverse_r,
    row_start,
    nrows,
    output_file
) {

  vals <- terra::values(
    mask,
    row = row_start,
    nrows = nrows,
    mat = FALSE
  )


  if (length(vals) != terra::ncell(reverse_r)) {
    stop(
      paste0(
        "Lectura de STREAM_MASK inconsistente. Esperadas ",
        terra::ncell(reverse_r),
        " celdas; recibidas ",
        length(vals),
        "."
      )
    )
  }


  vals <- ifelse(
    is.na(vals),
    0L,
    ifelse(
      vals > 0,
      1L,
      0L
    )
  )


  if (!all(vals %in% c(0L, 1L))) {
    stop(
      "STREAM_MASK produjo valores distintos de 0/1."
    )
  }


  out_r <- terra::rast(
    nrows = terra::nrow(reverse_r),
    ncols = terra::ncol(reverse_r),
    xmin = terra::xmin(reverse_r),
    xmax = terra::xmax(reverse_r),
    ymin = terra::ymin(reverse_r),
    ymax = terra::ymax(reverse_r),
    crs = terra::crs(
      reverse_r
    )
  )


  terra::values(
    out_r
  ) <- vals


  terra::writeRaster(
    out_r,
    output_file,
    overwrite = TRUE,
    datatype = "INT1U",
    gdal = c(
      "COMPRESS=DEFLATE",
      "PREDICTOR=1",
      "TILED=YES",
      "BIGTIFF=IF_SAFER"
    )
  )


  if (!file_nonempty(output_file)) {
    stop(
      paste0(
        "No se creo correctamente:\n",
        output_file
      )
    )
  }


  sum(
    vals > 0,
    na.rm = TRUE
  )
}


# ============================================================
# 5. PROCESAR UN BLOQUE
# ============================================================

process_block <- function(block_dir) {

  block_id <- basename(
    block_dir
  )

  index_dir <- file.path(
    block_dir,
    "index"
  )

  mask_file <- file.path(
    index_dir,
    "STREAM_MASK.tif"
  )

  reverse_dir <- file.path(
    index_dir,
    "reverse_stripes"
  )

  stream_dir <- file.path(
    index_dir,
    STREAM_DIR_NAME
  )


  cat(
    "\n=============================================\n",
    block_id,
    "\n",
    "=============================================\n",
    sep = ""
  )


  if (!file_nonempty(mask_file)) {
    stop(
      paste0(
        block_id,
        ": falta index/STREAM_MASK.tif."
      )
    )
  }


  if (!dir.exists(reverse_dir)) {
    stop(
      paste0(
        block_id,
        ": falta index/reverse_stripes/."
      )
    )
  }


  reverse_files <- list.files(
    reverse_dir,
    pattern = "^reverse_[0-9]{5}\\.tif$",
    full.names = TRUE,
    ignore.case = FALSE
  )

  reverse_files <- sort(
    reverse_files
  )


  if (length(reverse_files) == 0L) {
    stop(
      paste0(
        block_id,
        ": no existen reverse_XXXXX.tif."
      )
    )
  }


  expected_names <- sprintf(
    "reverse_%05d.tif",
    seq_along(
      reverse_files
    )
  )


  if (!identical(
    basename(
      reverse_files
    ),
    expected_names
  )) {
    stop(
      paste0(
        block_id,
        ": la secuencia reverse_XXXXX.tif tiene huecos o nombres inesperados."
      )
    )
  }


  dir.create(
    stream_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )


  mask <- terra::rast(
    mask_file
  )


  if (terra::nlyr(mask) != 1L) {
    stop(
      paste0(
        block_id,
        ": STREAM_MASK debe tener una sola banda."
      )
    )
  }


  nr_mask <- terra::nrow(
    mask
  )

  nc_mask <- terra::ncol(
    mask
  )


  cat(
    "STREAM_MASK: ",
    nr_mask,
    " filas x ",
    nc_mask,
    " columnas\n",
    "Franjas reverse: ",
    length(reverse_files),
    "\n",
    sep = ""
  )


  rows_done <- 0L

  block_manifest <- vector(
    "list",
    length(
      reverse_files
    )
  )


  for (i in seq_along(reverse_files)) {

    reverse_file <- reverse_files[[i]]

    reverse_r <- terra::rast(
      reverse_file
    )


    stripe_rows <- terra::nrow(
      reverse_r
    )

    stripe_cols <- terra::ncol(
      reverse_r
    )


    row_start <- rows_done +
      1L

    row_end <- rows_done +
      stripe_rows


    if (stripe_cols != nc_mask) {
      stop(
        paste0(
          block_id,
          " / ",
          basename(reverse_file),
          ": ncol no coincide con STREAM_MASK."
        )
      )
    }


    if (row_end > nr_mask) {
      stop(
        paste0(
          block_id,
          " / ",
          basename(reverse_file),
          ": la suma de filas reverse excede STREAM_MASK."
        )
      )
    }


    expected_ymax <- terra::ymax(
      mask
    ) -
      (
        row_start -
          1L
      ) *
      terra::res(mask)[2]


    expected_ymin <- terra::ymax(
      mask
    ) -
      row_end *
      terra::res(mask)[2]


    expected_reverse <- terra::rast(
      nrows = stripe_rows,
      ncols = nc_mask,
      xmin = terra::xmin(
        mask
      ),
      xmax = terra::xmax(
        mask
      ),
      ymin = expected_ymin,
      ymax = expected_ymax,
      crs = terra::crs(
        mask
      )
    )


    reverse_diag <- geom_diagnostic(
      reference = expected_reverse,
      candidate = reverse_r
    )


    if (
      !reverse_diag$same_rows ||
      !reverse_diag$same_cols ||
      !reverse_diag$same_crs ||
      !is.finite(reverse_diag$max_abs_cells) ||
      reverse_diag$max_abs_cells > GEOM_TOL_CELLS
    ) {
      stop(
        paste0(
          block_id,
          " / ",
          basename(reverse_file),
          ": la geometria reverse no corresponde a las filas ",
          row_start,
          "-",
          row_end,
          " de STREAM_MASK. Max delta=",
          format(
            reverse_diag$max_abs_cells,
            scientific = FALSE,
            digits = 10
          ),
          " celdas."
        )
      )
    }


    stream_file <- file.path(
      stream_dir,
      sprintf(
        "stream_%05d.tif",
        i
      )
    )


    status <- "EXISTS_VALID"

    n_stream_cells <- NA_real_


    if (!valid_stream_stripe(
      path = stream_file,
      reverse_r = reverse_r
    )) {

      if (
        file.exists(stream_file) &&
        !OVERWRITE_INVALID
      ) {
        stop(
          paste0(
            "Existe una franja stream invalida y OVERWRITE_INVALID=FALSE:\n",
            stream_file
          )
        )
      }


      if (file.exists(stream_file)) {
        unlink(
          stream_file,
          force = TRUE
        )
      }


      n_stream_cells <- write_stream_stripe(
        mask = mask,
        reverse_r = reverse_r,
        row_start = row_start,
        nrows = stripe_rows,
        output_file = stream_file
      )


      if (!valid_stream_stripe(
        path = stream_file,
        reverse_r = reverse_r
      )) {
        stop(
          paste0(
            block_id,
            " / ",
            basename(stream_file),
            ": fallo la validacion posterior a escritura."
          )
        )
      }


      status <- "CREATED"

    } else {

      stream_r_existing <- terra::rast(
        stream_file
      )

      freq_existing <- tryCatch(
        terra::global(
          stream_r_existing,
          fun = "sum",
          na.rm = TRUE
        ),
        error = function(e) NULL
      )


      if (
        !is.null(freq_existing) &&
        nrow(freq_existing) >= 1L
      ) {
        n_stream_cells <- as.numeric(
          freq_existing[1, 1]
        )
      }
    }


    stream_r <- terra::rast(
      stream_file
    )


    final_diag <- geom_diagnostic(
      reference = reverse_r,
      candidate = stream_r
    )


    block_manifest[[i]] <- data.frame(
      BLOCK_ID = block_id,
      STRIPE_ID = i,
      ROW_START = row_start,
      ROW_END = row_end,
      NROWS = stripe_rows,
      NCOLS = stripe_cols,
      REVERSE_FILE = gsub(
        "\\\\",
        "/",
        file.path(
          "index",
          "reverse_stripes",
          basename(
            reverse_file
          )
        )
      ),
      STREAM_FILE = gsub(
        "\\\\",
        "/",
        file.path(
          "index",
          STREAM_DIR_NAME,
          basename(
            stream_file
          )
        )
      ),
      STREAM_CELLS = n_stream_cells,
      MAX_GEOM_DELTA_CELLS = final_diag$max_abs_cells,
      STATUS = status,
      stringsAsFactors = FALSE
    )


    rows_done <- row_end


    cat(
      "\r",
      sprintf(
        "%s: %5d / %5d | filas %d-%d | %s",
        block_id,
        i,
        length(reverse_files),
        row_start,
        row_end,
        status
      ),
      sep = ""
    )


    rm(
      reverse_r,
      expected_reverse,
      stream_r
    )


    if (i %% 20L == 0L) {
      gc()
    }
  }


  cat(
    "\n"
  )


  if (rows_done != nr_mask) {
    stop(
      paste0(
        block_id,
        ": las franjas reverse cubren ",
        rows_done,
        " filas, pero STREAM_MASK tiene ",
        nr_mask,
        "."
      )
    )
  }


  manifest <- do.call(
    rbind,
    block_manifest
  )


  if (
    any(
      !is.finite(
        manifest$MAX_GEOM_DELTA_CELLS
      )
    ) ||
    any(
      manifest$MAX_GEOM_DELTA_CELLS >
        GEOM_TOL_CELLS
    )
  ) {
    stop(
      paste0(
        block_id,
        ": alguna stream stripe no coincide exactamente con su reverse stripe."
      )
    )
  }


  stream_files_final <- file.path(
    block_dir,
    manifest$STREAM_FILE
  )


  if (!all(
    vapply(
      stream_files_final,
      file_nonempty,
      logical(1)
    )
  )) {
    stop(
      paste0(
        block_id,
        ": faltan stream stripes despues de generar."
      )
    )
  }


  manifest_file <- file.path(
    index_dir,
    "stream_stripes_manifest.csv"
  )


  write.csv(
    manifest,
    manifest_file,
    row.names = FALSE
  )


  ready_file <- file.path(
    index_dir,
    "STREAM_STRIPES_READY.txt"
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
        "Block:",
        block_id
      ),
      paste(
        "Stream stripes:",
        nrow(
          manifest
        )
      ),
      paste(
        "Rows:",
        nr_mask
      ),
      paste(
        "Cols:",
        nc_mask
      ),
      paste(
        "Max geometry delta cells:",
        format(
          max(
            manifest$MAX_GEOM_DELTA_CELLS,
            na.rm = TRUE
          ),
          scientific = FALSE,
          digits = 12
        )
      ),
      "Resampling: NO",
      "Interpolation: NO",
      "Hydrology recalculated: NO",
      "Status: READY"
    ),
    ready_file
  )


  if (DELETE_STREAM_MASK_AFTER_VALIDATION) {

    unlink(
      mask_file,
      force = TRUE
    )


    if (file.exists(mask_file)) {
      stop(
        paste0(
          block_id,
          ": no se pudo eliminar STREAM_MASK.tif."
        )
      )
    }


    cat(
      "STREAM_MASK.tif eliminado de la copia runtime tras validacion.\n"
    )
  }


  cat(
    "OK | ",
    nrow(manifest),
    " stream stripes | max delta=",
    format(
      max(
        manifest$MAX_GEOM_DELTA_CELLS,
        na.rm = TRUE
      ),
      scientific = FALSE,
      digits = 12
    ),
    " celdas\n",
    sep = ""
  )


  manifest
}


# ============================================================
# 6. EJECUCION GLOBAL
# ============================================================

all_results <- list()

failures <- list()


for (block_dir in block_dirs) {

  block_id <- basename(
    block_dir
  )


  result <- tryCatch(
    process_block(
      block_dir
    ),
    error = function(e) e
  )


  if (inherits(result, "error")) {

    msg <- conditionMessage(
      result
    )


    cat(
      "\nERROR EN ",
      block_id,
      ":\n",
      msg,
      "\n",
      sep = ""
    )


    failures[[block_id]] <- data.frame(
      BLOCK_ID = block_id,
      ERROR = msg,
      stringsAsFactors = FALSE
    )

  } else {

    all_results[[block_id]] <- result
  }
}


# ============================================================
# 7. REPORTES GLOBALES
# ============================================================

if (length(all_results) > 0L) {

  global_manifest <- do.call(
    rbind,
    all_results
  )


  global_manifest_file <- file.path(
    SCRIPT_ROOT,
    "STREAM_STRIPES_MANIFEST.csv"
  )


  write.csv(
    global_manifest,
    global_manifest_file,
    row.names = FALSE
  )

} else {

  global_manifest <- data.frame()
}


if (length(failures) > 0L) {

  failure_table <- do.call(
    rbind,
    failures
  )


  failure_file <- file.path(
    SCRIPT_ROOT,
    "FAILED_STREAM_STRIPES_BLOCKS.csv"
  )


  write.csv(
    failure_table,
    failure_file,
    row.names = FALSE
  )


  cat(
    "\n\n=============================================\n",
    "TERMINADO CON ERRORES\n",
    "=============================================\n",
    "Bloques correctos: ",
    length(all_results),
    "\n",
    "Bloques fallidos: ",
    length(failures),
    "\n",
    "Revisa: ",
    failure_file,
    "\n",
    sep = ""
  )


  stop(
    "Uno o mas bloques fallaron. No se genero READY global."
  )
}


old_failure_file <- file.path(
  SCRIPT_ROOT,
  "FAILED_STREAM_STRIPES_BLOCKS.csv"
)


if (file.exists(old_failure_file)) {
  unlink(
    old_failure_file,
    force = TRUE
  )
}


global_ready <- file.path(
  SCRIPT_ROOT,
  "ALL_STREAM_STRIPES_READY.txt"
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
      length(
        all_results
      )
    ),
    paste(
      "Total stream stripes:",
      nrow(
        global_manifest
      )
    ),
    "Resampling: NO",
    "Interpolation: NO",
    "Hydrology recalculated: NO",
    paste(
      "STREAM_MASK deleted:",
      DELETE_STREAM_MASK_AFTER_VALIDATION
    ),
    "Status: ALL READY"
  ),
  global_ready
)


cat(
  "\n\n=============================================\n",
  "TODAS LAS STREAM STRIPES ESTAN LISTAS\n",
  "=============================================\n",
  "Bloques: ",
  length(all_results),
  "\n",
  "Franjas totales: ",
  nrow(global_manifest),
  "\n",
  "Manifest: ",
  file.path(
    SCRIPT_ROOT,
    "STREAM_STRIPES_MANIFEST.csv"
  ),
  "\n",
  "READY: ",
  global_ready,
  "\n",
  sep = ""
)


if (!DELETE_STREAM_MASK_AFTER_VALIDATION) {

  cat(
    "\nSTREAM_MASK.tif se conservo.\n",
    "Cuando confirmes que todo esta OK, puedes cambiar:\n\n",
    "DELETE_STREAM_MASK_AFTER_VALIDATION <- TRUE\n\n",
    "y volver a ejecutar. Las franjas validas se reutilizan y solo se retirara ",
    "el STREAM_MASK completo de la copia runtime.\n",
    sep = ""
  )
}
