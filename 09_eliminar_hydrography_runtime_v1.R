# ============================================================
# 09_eliminar_hydrography_runtime_v1.R
#
# FABDEM Watershed Explorer
# Eliminar hydrography_block.gpkg del runtime
# ============================================================
#
# EJECUTAR DESDE:
#   FABDEM_Watershed_Runtime/
#
# Elimina exclusivamente:
#   core/BLOCK_xxx/hydrography_block.gpkg
#
# Adicionalmente, si existe posit_data/remote_manifest.csv,
# elimina las filas con ASSET_TYPE == "hydrography".
# ============================================================

BLOCKS_ONLY <- NULL
DELETE_FILES <- TRUE
CLEAN_REMOTE_MANIFEST <- TRUE

RUNTIME_ROOT <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

CORE_DIR <- file.path(
  RUNTIME_ROOT,
  "core"
)

POSIT_DATA_DIR <- file.path(
  RUNTIME_ROOT,
  "posit_data"
)

REMOTE_MANIFEST_FILE <- file.path(
  POSIT_DATA_DIR,
  "remote_manifest.csv"
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

block_dirs <- list.dirs(
  CORE_DIR,
  full.names = TRUE,
  recursive = FALSE
)

block_dirs <- block_dirs[
  grepl(
    "^BLOCK_[0-9]+$",
    basename(block_dirs)
  )
]

block_dirs <- sort(block_dirs)

if (length(block_dirs) == 0L) {
  stop(
    "No se encontraron carpetas BLOCK_xxx dentro de core/."
  )
}

if (!is.null(BLOCKS_ONLY)) {

  wanted <- toupper(
    trimws(
      as.character(BLOCKS_ONLY)
    )
  )

  block_dirs <- block_dirs[
    basename(block_dirs) %in% wanted
  ]

  if (length(block_dirs) == 0L) {
    stop(
      "BLOCKS_ONLY no coincide con ningún bloque encontrado."
    )
  }
}

cat(
  "\n=============================================\n",
  "ELIMINAR HYDROGRAPHY DEL RUNTIME\n",
  "=============================================\n",
  "Raíz: ",
  RUNTIME_ROOT,
  "\n",
  "Bloques inspeccionados: ",
  length(block_dirs),
  "\n",
  "DELETE_FILES: ",
  DELETE_FILES,
  "\n\n",
  sep = ""
)

targets <- file.path(
  block_dirs,
  "hydrography_block.gpkg"
)

exists_before <- file.exists(targets)

bytes_before <- 0

if (any(exists_before)) {
  info <- file.info(targets[exists_before])
  bytes_before <- sum(
    info$size,
    na.rm = TRUE
  )
}

cat(
  "Archivos encontrados: ",
  sum(exists_before),
  "\n",
  "Tamaño total: ",
  sprintf(
    "%.3f",
    bytes_before / 1024^3
  ),
  " GiB\n\n",
  sep = ""
)

if (any(exists_before)) {
  for (path in targets[exists_before]) {
    cat(
      "  ",
      gsub("\\\\", "/", path),
      "\n",
      sep = ""
    )
  }
  cat("\n")
}

deleted <- character(0)

if (
  DELETE_FILES &&
  any(exists_before)
) {

  for (path in targets[exists_before]) {

    unlink(
      path,
      force = TRUE
    )

    if (file.exists(path)) {
      stop(
        paste0(
          "No se pudo eliminar:\n",
          path
        )
      )
    }

    deleted <- c(
      deleted,
      path
    )
  }
}

manifest_rows_removed <- 0L

if (
  CLEAN_REMOTE_MANIFEST &&
  file.exists(REMOTE_MANIFEST_FILE)
) {

  manifest <- read.csv(
    REMOTE_MANIFEST_FILE,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if ("ASSET_TYPE" %in% names(manifest)) {

    remove_rows <- !is.na(
      manifest[["ASSET_TYPE"]]
    ) &
      tolower(
        trimws(
          as.character(
            manifest[["ASSET_TYPE"]]
          )
        )
      ) == "hydrography"

    manifest_rows_removed <- sum(remove_rows)

    if (manifest_rows_removed > 0L) {

      manifest <- manifest[
        !remove_rows,
        ,
        drop = FALSE
      ]

      write.csv(
        manifest,
        REMOTE_MANIFEST_FILE,
        row.names = FALSE
      )
    }

  } else {

    warning(
      "remote_manifest.csv existe pero no contiene ASSET_TYPE. No se modificó."
    )
  }
}

still_exists <- targets[
  file.exists(targets)
]

if (
  DELETE_FILES &&
  length(still_exists) > 0L
) {
  stop(
    paste0(
      "Quedaron archivos hydrography_block.gpkg:\n",
      paste(
        still_exists,
        collapse = "\n"
      )
    )
  )
}

cat(
  "\n=============================================\n",
  "LIMPIEZA COMPLETADA\n",
  "=============================================\n",
  "Archivos encontrados: ",
  sum(exists_before),
  "\n",
  "Archivos eliminados: ",
  length(deleted),
  "\n",
  "Espacio retirado: ",
  sprintf(
    "%.3f",
    bytes_before / 1024^3
  ),
  " GiB\n",
  "Filas hydrography retiradas del manifest: ",
  manifest_rows_removed,
  "\n",
  "Estado: OK\n",
  sep = ""
)

if (!DELETE_FILES) {
  cat(
    "\nDELETE_FILES = FALSE: fue solo una inspección.\n",
    "Cambia a TRUE para eliminar los archivos.\n",
    sep = ""
  )
}
