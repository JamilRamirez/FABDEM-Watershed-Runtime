# ============================================================
# 08_construir_fast_cache_block004_v1.R
#
# FAST CACHE PURO R PARA BLOCK_004
# ============================================================
#
# Ejecutar desde FABDEM_Watershed_Runtime/
#
# Convierte:
#   core/BLOCK_004/index/reverse_stripes/reverse_XXXXX.tif
#
# a:
#   core/BLOCK_004/fast_cache/reverse_raw/reverse_XXXXX.rds
#
# Cada RDS guarda directamente el vector raw 0..255 y sus
# dimensiones. No recalcula hidrologia y no modifica los TIFF.
# ============================================================


BLOCK_ID <- "BLOCK_004"
OVERWRITE <- FALSE
VERIFY_WRITTEN_RDS <- TRUE


if (!requireNamespace("terra", quietly = TRUE)) {
  stop("Falta el paquete terra.")
}


RUNTIME_ROOT <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

BLOCK_DIR <- file.path(
  RUNTIME_ROOT,
  "core",
  BLOCK_ID
)

INDEX_DIR <- file.path(
  BLOCK_DIR,
  "index"
)

REVERSE_DIR <- file.path(
  INDEX_DIR,
  "reverse_stripes"
)

INDEX_METADATA_FILE <- file.path(
  INDEX_DIR,
  "index_metadata.rds"
)

FAST_DIR <- file.path(
  BLOCK_DIR,
  "fast_cache"
)

FAST_RAW_DIR <- file.path(
  FAST_DIR,
  "reverse_raw"
)

FAST_MANIFEST_FILE <- file.path(
  FAST_DIR,
  "fast_cache_manifest.csv"
)

FAST_METADATA_FILE <- file.path(
  FAST_DIR,
  "fast_cache_metadata.rds"
)

FAST_READY_FILE <- file.path(
  FAST_DIR,
  "FAST_CACHE_READY.txt"
)


if (!dir.exists(BLOCK_DIR)) {
  stop(
    paste0(
      "No existe:\n",
      BLOCK_DIR,
      "\nEjecuta este script desde FABDEM_Watershed_Runtime/."
    )
  )
}

if (!dir.exists(REVERSE_DIR)) {
  stop(
    paste0(
      "No existe:\n",
      REVERSE_DIR
    )
  )
}

if (!file.exists(INDEX_METADATA_FILE)) {
  stop(
    paste0(
      "No existe:\n",
      INDEX_METADATA_FILE
    )
  )
}

dir.create(
  FAST_RAW_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


file_nonempty <- function(path) {

  if (
    length(path) != 1L ||
    is.na(path) ||
    !nzchar(path) ||
    !file.exists(path)
  ) {
    return(FALSE)
  }

  info <- file.info(path)

  isTRUE(
    is.finite(info$size) &&
      info$size > 0
  )
}


parse_id <- function(path) {

  name <- basename(path)

  hit <- regmatches(
    name,
    regexec(
      "^reverse_([0-9]{5})\\.tif$",
      name
    )
  )[[1]]

  if (length(hit) != 2L) {
    stop(
      paste0(
        "Nombre inesperado: ",
        name
      )
    )
  }

  as.integer(hit[2])
}


validate_raw_object <- function(
    obj,
    expected_nrows,
    expected_ncols
) {

  if (!is.list(obj)) {
    return(FALSE)
  }

  if (!all(
    c(
      "values",
      "nrows",
      "ncols"
    ) %in% names(obj)
  )) {
    return(FALSE)
  }

  if (!is.raw(obj[["values"]])) {
    return(FALSE)
  }

  if (
    as.integer(obj[["nrows"]]) !=
      as.integer(expected_nrows) ||
    as.integer(obj[["ncols"]]) !=
      as.integer(expected_ncols)
  ) {
    return(FALSE)
  }

  expected_length <- as.double(
    expected_nrows
  ) *
    as.double(
      expected_ncols
    )

  isTRUE(
    length(obj[["values"]]) ==
      expected_length
  )
}


validate_edge_bits <- function(
    values_raw,
    ncols,
    stripe_id,
    n_stripes
) {

  first_col_index <- seq.int(
    from = 1L,
    to = length(values_raw),
    by = ncols
  )

  last_col_index <- seq.int(
    from = ncols,
    to = length(values_raw),
    by = ncols
  )

  first_col <- as.integer(
    values_raw[first_col_index]
  )

  last_col <- as.integer(
    values_raw[last_col_index]
  )

  # Borde oeste: no NW/W/SW.
  if (any(
    bitwAnd(
      first_col,
      41L
    ) != 0L
  )) {
    stop(
      paste0(
        "Bits imposibles en borde oeste, stripe ",
        stripe_id,
        "."
      )
    )
  }

  # Borde este: no NE/E/SE.
  if (any(
    bitwAnd(
      last_col,
      148L
    ) != 0L
  )) {
    stop(
      paste0(
        "Bits imposibles en borde este, stripe ",
        stripe_id,
        "."
      )
    )
  }

  if (stripe_id == 1L) {

    top_row <- as.integer(
      values_raw[
        seq_len(ncols)
      ]
    )

    # Borde norte: no NW/N/NE.
    if (any(
      bitwAnd(
        top_row,
        7L
      ) != 0L
    )) {
      stop(
        "Bits imposibles en borde norte global."
      )
    }
  }

  if (stripe_id == n_stripes) {

    start_bottom <- length(values_raw) -
      ncols +
      1L

    bottom_row <- as.integer(
      values_raw[
        start_bottom:
          length(values_raw)
      ]
    )

    # Borde sur: no SW/S/SE.
    if (any(
      bitwAnd(
        bottom_row,
        224L
      ) != 0L
    )) {
      stop(
        "Bits imposibles en borde sur global."
      )
    }
  }

  TRUE
}


metadata <- readRDS(
  INDEX_METADATA_FILE
)

required_metadata <- c(
  "nrows",
  "ncols",
  "n_stripes"
)

if (!all(
  required_metadata %in%
    names(metadata)
)) {
  stop(
    "index_metadata.rds no contiene nrows/ncols/n_stripes."
  )
}

expected_nrows <- as.integer(
  metadata[["nrows"]]
)

expected_ncols <- as.integer(
  metadata[["ncols"]]
)

expected_n_stripes <- as.integer(
  metadata[["n_stripes"]]
)

if (
  !is.finite(expected_nrows) ||
  !is.finite(expected_ncols) ||
  !is.finite(expected_n_stripes) ||
  expected_nrows < 1L ||
  expected_ncols < 1L ||
  expected_n_stripes < 1L
) {
  stop(
    "Dimensiones invalidas en index_metadata.rds."
  )
}


reverse_files <- sort(
  list.files(
    REVERSE_DIR,
    pattern = "^reverse_[0-9]{5}\\.tif$",
    full.names = TRUE
  )
)

if (length(reverse_files) != expected_n_stripes) {
  stop(
    paste0(
      "Se esperaban ",
      expected_n_stripes,
      " reverse stripes y existen ",
      length(reverse_files),
      "."
    )
  )
}

stripe_ids <- unname(
  vapply(
    reverse_files,
    parse_id,
    integer(1)
  )
)

if (!identical(
  stripe_ids,
  seq_len(expected_n_stripes)
)) {
  stop(
    "La secuencia reverse_XXXXX.tif no es continua."
  )
}


cat(
  "\nFAST CACHE ",
  BLOCK_ID,
  "\nGrilla: ",
  format(
    expected_nrows,
    big.mark = ","
  ),
  " x ",
  format(
    expected_ncols,
    big.mark = ","
  ),
  "\nStripes: ",
  expected_n_stripes,
  "\n\n",
  sep = ""
)


manifest_rows <- vector(
  "list",
  expected_n_stripes
)

rows_total <- 0L
raw_bytes_total <- 0


for (i in seq_along(reverse_files)) {

  source_file <- reverse_files[[i]]
  source_info <- file.info(source_file)

  r <- terra::rast(source_file)

  nr <- terra::nrow(r)
  nc <- terra::ncol(r)

  if (nc != expected_ncols) {
    stop(
      paste0(
        "Ncols inconsistente en ",
        basename(source_file),
        "."
      )
    )
  }

  rows_total <- rows_total +
    nr

  fast_file <- file.path(
    FAST_RAW_DIR,
    sprintf(
      "reverse_%05d.rds",
      i
    )
  )

  reuse <- FALSE
  obj <- NULL

  if (
    !OVERWRITE &&
    file_nonempty(fast_file)
  ) {

    existing <- tryCatch(
      readRDS(fast_file),
      error = function(e) NULL
    )

    if (
      !is.null(existing) &&
      validate_raw_object(
        existing,
        expected_nrows = nr,
        expected_ncols = nc
      )
    ) {
      reuse <- TRUE
      obj <- existing
    }
  }

  if (!reuse) {

    vals <- terra::values(
      r,
      mat = FALSE
    )

    expected_values <- as.double(nr) *
      as.double(nc)

    if (length(vals) != expected_values) {
      stop(
        paste0(
          "Lectura incompleta en ",
          basename(source_file),
          "."
        )
      )
    }

    vals[
      is.na(vals)
    ] <- 0

    vals_int <- as.integer(vals)

    if (any(
      vals_int < 0L |
        vals_int > 255L
    )) {
      stop(
        paste0(
          "Valores reverse fuera de 0..255 en ",
          basename(source_file),
          "."
        )
      )
    }

    values_raw <- as.raw(
      vals_int
    )

    validate_edge_bits(
      values_raw = values_raw,
      ncols = nc,
      stripe_id = i,
      n_stripes = expected_n_stripes
    )

    obj <- list(
      values = values_raw,
      nrows = nr,
      ncols = nc
    )

    saveRDS(
      obj,
      fast_file,
      compress = FALSE,
      version = 3
    )

    if (!file_nonempty(fast_file)) {
      stop(
        paste0(
          "No se creo:\n",
          fast_file
        )
      )
    }

    if (VERIFY_WRITTEN_RDS) {

      check_obj <- tryCatch(
        readRDS(fast_file),
        error = function(e) NULL
      )

      if (
        is.null(check_obj) ||
        !validate_raw_object(
          check_obj,
          expected_nrows = nr,
          expected_ncols = nc
        )
      ) {
        stop(
          paste0(
            "Fallo la verificacion de ",
            basename(fast_file),
            "."
          )
        )
      }

      validate_edge_bits(
        values_raw = check_obj[["values"]],
        ncols = nc,
        stripe_id = i,
        n_stripes = expected_n_stripes
      )

      rm(check_obj)
    }

    rm(
      vals,
      vals_int,
      values_raw
    )
  }

  fast_info <- file.info(fast_file)

  raw_bytes <- length(
    obj[["values"]]
  )

  raw_bytes_total <- raw_bytes_total +
    raw_bytes

  manifest_rows[[i]] <- data.frame(
    BLOCK_ID = BLOCK_ID,
    STRIPE_ID = i,
    SOURCE_FILE = basename(source_file),
    SOURCE_SIZE = as.numeric(
      source_info$size
    ),
    SOURCE_MTIME = format(
      source_info$mtime,
      "%Y-%m-%d %H:%M:%S"
    ),
    FAST_FILE = basename(fast_file),
    FAST_SIZE = as.numeric(
      fast_info$size
    ),
    NROWS = nr,
    NCOLS = nc,
    NVALUES = raw_bytes,
    STATUS = if (reuse) "REUSED" else "CREATED",
    stringsAsFactors = FALSE
  )

  cat(
    "\r",
    sprintf(
      "%s | %3d/%3d | %s",
      BLOCK_ID,
      i,
      expected_n_stripes,
      if (reuse) "REUSED " else "CREATED"
    ),
    sep = ""
  )

  rm(
    r,
    obj
  )

  if (i %% 10L == 0L) {
    gc()
  }
}

cat("\n")


if (rows_total != expected_nrows) {
  stop(
    paste0(
      "La suma de filas es ",
      rows_total,
      " pero metadata indica ",
      expected_nrows,
      "."
    )
  )
}


manifest <- do.call(
  rbind,
  manifest_rows
)

write.csv(
  manifest,
  FAST_MANIFEST_FILE,
  row.names = FALSE
)


fast_metadata <- list(
  version = "1.0",
  block_id = BLOCK_ID,
  created = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  nrows = expected_nrows,
  ncols = expected_ncols,
  n_stripes = expected_n_stripes,
  total_cells = as.double(
    expected_nrows
  ) *
    as.double(
      expected_ncols
    ),
  raw_bytes = raw_bytes_total,
  raw_gib = raw_bytes_total /
    1024^3,
  edge_safe = TRUE,
  compression = FALSE,
  hydrology_recalculated = FALSE
)

saveRDS(
  fast_metadata,
  FAST_METADATA_FILE
)


fast_files <- file.path(
  FAST_RAW_DIR,
  manifest[["FAST_FILE"]]
)

if (!all(
  vapply(
    fast_files,
    file_nonempty,
    logical(1)
  )
)) {
  stop(
    "Faltan archivos fast cache despues de construir."
  )
}


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
      BLOCK_ID
    ),
    paste(
      "Stripes:",
      expected_n_stripes
    ),
    paste(
      "Raw GiB:",
      sprintf(
        "%.3f",
        fast_metadata$raw_gib
      )
    ),
    "Edge validation: PASSED",
    "Hydrology recalculated: NO",
    "Status: READY"
  ),
  FAST_READY_FILE
)


cat(
  "\n=============================================\n",
  "FAST CACHE LISTO\n",
  "=============================================\n",
  "Bloque: ",
  BLOCK_ID,
  "\n",
  "Stripes: ",
  expected_n_stripes,
  "\n",
  "Raw en RAM aprox.: ",
  sprintf(
    "%.3f",
    fast_metadata$raw_gib
  ),
  " GiB\n",
  "Edge validation: PASSED\n",
  "READY: ",
  FAST_READY_FILE,
  "\n",
  sep = ""
)
