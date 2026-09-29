# ============================================================
# 11_retirar_lfs_del_runtime.R
#
# FABDEM Watershed Runtime
# FASE 2: RETIRAR GIT LFS DEL BRANCH MAIN
#
# Ejecutar SOLO despues de:
#   1) completar 10_migrar_lfs_a_releases.R;
#   2) verificar que FABDEM Watershed Explorer usa Releases;
#   3) confirmar que los mapas/delimitacion funcionan.
#
# El script:
#   - comprueba que el manifiesto cubra todos los assets de produccion;
#   - verifica que los assets existan en GitHub Releases con el tamano esperado;
#   - quita los archivos LFS del INDICE Git con git rm --cached;
#   - NO borra los binarios de la copia local;
#   - elimina .gitattributes;
#   - agrega *.tif, *.gpkg y *.rds a .gitignore;
#   - hace commit y push a main.
#
# Los GPKG fuente que ya tienen teselas terminadas se consideran
# redundantes y se retiran sin exigir una copia en Releases.
# ============================================================


REPO <- "JamilRamirez/FABDEM-Watershed-Runtime"
MANIFEST_FILE <- "runtime_release_manifest.csv"


run_cmd <- function(
    command,
    args = character(0),
    stdout = TRUE,
    stderr = TRUE,
    fail = TRUE
) {

  quoted_args <- if (length(args) > 0L) {
    vapply(
      as.character(args),
      shQuote,
      character(1)
    )
  } else {
    character(0)
  }

  out <- tryCatch(
    system2(
      command,
      args = quoted_args,
      stdout = stdout,
      stderr = stderr
    ),
    error = function(e) {
      z <- character(0)
      attr(z, "status") <- 999L
      attr(z, "error_message") <- conditionMessage(e)
      z
    }
  )

  status <- attr(out, "status")

  if (is.null(status)) {
    status <- 0L
  }

  if (isTRUE(fail) && !identical(as.integer(status), 0L)) {
    stop(
      paste0(
        "Fallo el comando:\n",
        command,
        " ",
        paste(args, collapse = " "),
        if (length(out) > 0L) {
          paste0("\n\nSalida:\n", paste(out, collapse = "\n"))
        } else {
          ""
        }
      )
    )
  }

  list(
    status = as.integer(status),
    output = out
  )
}


normalize_rel <- function(x) {
  gsub(
    "\\\\",
    "/",
    as.character(x)
  )
}


release_assets <- function(tag) {

  result <- run_cmd(
    "gh",
    c(
      "release",
      "view",
      tag,
      "--repo",
      REPO,
      "--json",
      "assets",
      "--jq",
      ".assets[] | [.name, .size] | @tsv"
    ),
    stdout = TRUE,
    stderr = TRUE,
    fail = TRUE
  )

  lines <- result$output
  lines <- lines[nzchar(lines)]

  if (length(lines) == 0L) {
    return(
      data.frame(
        ASSET_NAME = character(0),
        SIZE_BYTES = numeric(0),
        stringsAsFactors = FALSE
      )
    )
  }

  parts <- strsplit(
    lines,
    "\t",
    fixed = FALSE
  )

  data.frame(
    ASSET_NAME = vapply(
      parts,
      function(x) x[1],
      character(1)
    ),
    SIZE_BYTES = suppressWarnings(
      as.numeric(
        vapply(
          parts,
          function(x) if (length(x) >= 2L) x[2] else NA_character_,
          character(1)
        )
      )
    ),
    stringsAsFactors = FALSE
  )
}


# ============================================================
# PRECONDICIONES
# ============================================================

if (!dir.exists(".git")) {
  stop(
    "Ejecuta este script desde la raiz de FABDEM-Watershed-Runtime."
  )
}


branch <- run_cmd(
  "git",
  c(
    "rev-parse",
    "--abbrev-ref",
    "HEAD"
  )
)$output[1]

if (!identical(branch, "main")) {
  stop(
    paste0(
      "La rama activa debe ser main. Rama actual: ",
      branch
    )
  )
}


if (!file.exists(MANIFEST_FILE)) {
  stop(
    paste0(
      "No existe ",
      MANIFEST_FILE,
      ". Ejecuta primero 10_migrar_lfs_a_releases.R."
    )
  )
}


manifest <- utils::read.csv(
  MANIFEST_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


required <- c(
  "RELATIVE_PATH",
  "RELEASE_TAG",
  "ASSET_NAME",
  "REMOTE_URL",
  "SIZE_BYTES"
)

if (!all(required %in% names(manifest))) {
  stop(
    "El manifiesto de Releases no tiene las columnas esperadas."
  )
}


manifest$RELATIVE_PATH <- normalize_rel(
  manifest$RELATIVE_PATH
)

manifest$SIZE_BYTES <- suppressWarnings(
  as.numeric(manifest$SIZE_BYTES)
)


lfs_files <- run_cmd(
  "git",
  c(
    "lfs",
    "ls-files",
    "-n"
  )
)$output

lfs_files <- unique(
  normalize_rel(
    trimws(
      lfs_files[
        nzchar(trimws(lfs_files))
      ]
    )
  )
)


if (length(lfs_files) == 0L) {
  cat(
    "No quedan archivos LFS en el branch actual. No hay nada que retirar.\n"
  )
  quit(
    save = "no",
    status = 0
  )
}


# ============================================================
# FUENTES TESELADAS REDUNDANTES
# ============================================================

redundant_lfs_files <- character(0)

tiling_summary_file <- file.path(
  "layers",
  "layers_tiling_summary.csv"
)

if (file.exists(tiling_summary_file)) {

  tiling_summary <- utils::read.csv(
    tiling_summary_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (
    all(
      c(
        "SOURCE",
        "STATUS",
        "OUTPUT_DIR"
      ) %in% names(tiling_summary)
    )
  ) {

    tiled_rows <- (
      tiling_summary$STATUS %in%
        c(
          "tiled_ok",
          "already_tiled"
        )
    ) &
      !is.na(tiling_summary$OUTPUT_DIR) &
      nzchar(
        trimws(
          as.character(
            tiling_summary$OUTPUT_DIR
          )
        )
      )

    redundant_lfs_files <- normalize_rel(
      file.path(
        "layers",
        as.character(
          tiling_summary$SOURCE[
            tiled_rows
          ]
        )
      )
    )

    redundant_lfs_files <- intersect(
      lfs_files,
      redundant_lfs_files
    )
  }
}


production_lfs <- setdiff(
  lfs_files,
  redundant_lfs_files
)


missing_manifest <- setdiff(
  production_lfs,
  manifest$RELATIVE_PATH
)

if (length(missing_manifest) > 0L) {
  stop(
    paste0(
      "El manifiesto NO cubre todos los archivos LFS de produccion. ",
      "No se retirara LFS. Faltan:\n",
      paste(
        head(missing_manifest, 50L),
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# VERIFICACION DE RELEASES
# ============================================================

cat(
  "Verificando assets de GitHub Releases...\n"
)

problems <- character(0)

tags <- unique(
  manifest$RELEASE_TAG[
    manifest$RELATIVE_PATH %in% production_lfs
  ]
)


for (tag in tags) {

  remote <- release_assets(tag)

  expected <- manifest[
    manifest$RELEASE_TAG == tag &
      manifest$RELATIVE_PATH %in% production_lfs,
    ,
    drop = FALSE
  ]

  for (i in seq_len(nrow(expected))) {

    hit <- match(
      expected$ASSET_NAME[i],
      remote$ASSET_NAME
    )

    if (is.na(hit)) {
      problems <- c(
        problems,
        paste0(
          tag,
          " | ausente | ",
          expected$ASSET_NAME[i]
        )
      )
      next
    }

    if (
      !is.finite(remote$SIZE_BYTES[hit]) ||
      !identical(
        as.numeric(remote$SIZE_BYTES[hit]),
        as.numeric(expected$SIZE_BYTES[i])
      )
    ) {
      problems <- c(
        problems,
        paste0(
          tag,
          " | tamano distinto | ",
          expected$ASSET_NAME[i]
        )
      )
    }
  }
}


if (length(problems) > 0L) {
  stop(
    paste0(
      "La verificacion de Releases fallo. LFS NO sera retirado:\n",
      paste(
        head(problems, 50L),
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# IGNORAR BINARIOS LOCALES
# ============================================================

gitignore <- if (file.exists(".gitignore")) {
  readLines(
    ".gitignore",
    warn = FALSE,
    encoding = "UTF-8"
  )
} else {
  character(0)
}


needed_ignores <- c(
  "*.tif",
  "*.gpkg",
  "*.rds"
)

missing_ignores <- setdiff(
  needed_ignores,
  trimws(gitignore)
)

if (length(missing_ignores) > 0L) {

  writeLines(
    c(
      gitignore,
      if (
        length(gitignore) > 0L &&
        nzchar(tail(gitignore, 1L))
      ) {
        ""
      } else {
        character(0)
      },
      "# Runtime binario servido desde GitHub Releases",
      missing_ignores
    ),
    ".gitignore",
    useBytes = TRUE
  )
}


# ============================================================
# RETIRAR PUNTEROS LFS DEL INDICE, NO DEL DISCO
# ============================================================

cat(
  "\nRetirando ",
  length(lfs_files),
  " archivos LFS del indice Git. ",
  "Los archivos locales se conservan.\n",
  sep = ""
)


chunk_size <- 50L
chunks <- split(
  lfs_files,
  ceiling(
    seq_along(lfs_files) /
      chunk_size
  )
)


for (i in seq_along(chunks)) {

  cat(
    sprintf(
      "  [%d/%d] git rm --cached (%d archivos)\n",
      i,
      length(chunks),
      length(chunks[[i]])
    )
  )

  run_cmd(
    "git",
    c(
      "rm",
      "--cached",
      "--",
      chunks[[i]]
    ),
    stdout = TRUE,
    stderr = TRUE,
    fail = TRUE
  )
}


if (file.exists(".gitattributes")) {
  run_cmd(
    "git",
    c(
      "rm",
      "--",
      ".gitattributes"
    ),
    stdout = TRUE,
    stderr = TRUE,
    fail = TRUE
  )
}


run_cmd(
  "git",
  c(
    "add",
    ".gitignore",
    MANIFEST_FILE
  )
)


changes <- run_cmd(
  "git",
  c(
    "status",
    "--porcelain"
  )
)$output


if (length(changes) == 0L) {
  cat(
    "No hay cambios para confirmar.\n"
  )
  quit(
    save = "no",
    status = 0
  )
}


run_cmd(
  "git",
  c(
    "commit",
    "-m",
    "Retire Git LFS runtime assets"
  )
)


run_cmd(
  "git",
  c(
    "push",
    "origin",
    "main"
  )
)


cat(
  "\n============================================================\n",
  "GIT LFS RETIRADO DEL BRANCH MAIN\n",
  "============================================================\n",
  "Assets de produccion en Releases: ",
  length(production_lfs),
  "\nFuentes teseladas redundantes retiradas: ",
  length(redundant_lfs_files),
  "\nBinarios locales conservados e ignorados.\n",
  "\nNota: los objetos LFS historicos pueden seguir contabilizando ",
  "almacenamiento hasta que GitHub purgue el historial/objetos. ",
  "La aplicacion ya no depende de ellos.\n",
  sep = ""
)
