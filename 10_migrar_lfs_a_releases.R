# ============================================================
# 10_migrar_lfs_a_releases.R
#
# FABDEM Watershed Runtime
# MIGRACION DE BINARIOS GIT LFS -> GITHUB RELEASE ASSETS
#
# Objetivo:
#   - publicar una sola vez los binarios reales que hoy estan en LFS;
#   - distribuirlos en varias Releases para no superar 1000 assets;
#   - generar runtime_release_manifest.csv;
#   - dejar que FABDEM Watershed Explorer descargue desde Releases
#     en lugar de consumir ancho de banda Git LFS.
#
# IMPORTANTE:
#   Ejecutar desde una copia LOCAL del repositorio Runtime que
#   ya contenga los archivos reales. Si hay punteros LFS en vez
#   de binarios, el script se detiene antes de publicar nada.
#
# Requisitos:
#   - git
#   - git-lfs
#   - GitHub CLI (gh) autenticado
#   - permisos de escritura sobre JamilRamirez/FABDEM-Watershed-Runtime
#
# Este script NO elimina todavia los archivos LFS ni .gitattributes.
# Primero publica y verifica las Releases. La limpieza se hace
# solamente despues de comprobar la web.
# ============================================================


REPO <- "JamilRamirez/FABDEM-Watershed-Runtime"
MANIFEST_FILE <- "runtime_release_manifest.csv"
MAX_ASSETS_PER_RELEASE <- 950L

# FALSE = omite assets ya presentes con el mismo nombre/tamano.
# TRUE  = vuelve a subirlos con --clobber.
FORCE_REUPLOAD <- FALSE


# ============================================================
# HELPERS
# ============================================================

run_cmd <- function(
    command,
    args = character(0),
    stdout = TRUE,
    stderr = TRUE,
    fail = TRUE
) {

  out <- tryCatch(
    system2(
      command,
      args = args,
      stdout = stdout,
      stderr = stderr
    ),
    error = function(e) {
      attr(character(0), "status") <- 999L
      attr(character(0), "error_message") <- conditionMessage(e)
      character(0)
    }
  )

  status <- attr(out, "status")

  if (is.null(status)) {
    status <- 0L
  }

  if (isTRUE(fail) && !identical(as.integer(status), 0L)) {
    extra <- attr(out, "error_message")

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
        },
        if (!is.null(extra)) {
          paste0("\n\n", extra)
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


check_tool <- function(tool) {

  result <- run_cmd(
    tool,
    "--version",
    stdout = TRUE,
    stderr = TRUE,
    fail = FALSE
  )

  if (!identical(result$status, 0L)) {
    stop(
      paste0(
        "No se encontro '",
        tool,
        "' o no esta disponible en PATH."
      )
    )
  }

  invisible(TRUE)
}


is_lfs_pointer <- function(path) {

  if (!file.exists(path)) {
    return(FALSE)
  }

  size <- suppressWarnings(
    as.numeric(file.info(path)$size)
  )

  if (!is.finite(size) || size <= 0 || size > 2048) {
    return(FALSE)
  }

  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)

  prefix_raw <- readBin(
    con,
    what = "raw",
    n = min(512L, as.integer(size))
  )

  prefix <- tryCatch(
    rawToChar(prefix_raw),
    error = function(e) ""
  )

  startsWith(
    prefix,
    "version https://git-lfs.github.com/spec/v1"
  )
}


normalize_rel <- function(x) {
  gsub(
    "\\\\",
    "/",
    as.character(x)
  )
}


release_tag_for <- function(path) {

  path <- normalize_rel(path)

  if (grepl("^core/BLOCK_[0-9]+/", path)) {

    block <- sub(
      "^core/(BLOCK_[0-9]+)/.*$",
      "\\1",
      path
    )

    return(
      paste0(
        "runtime-core-",
        tolower(block),
        "-v1"
      )
    )
  }

  if (startsWith(path, "dem/")) {
    return("runtime-dem-v1")
  }

  if (startsWith(path, "layers/")) {
    return("runtime-layers-v1")
  }

  if (startsWith(path, "auxiliary/")) {
    return("runtime-auxiliary-v1")
  }

  if (startsWith(path, "posit_data/")) {
    return("runtime-posit-data-v1")
  }

  "runtime-misc-v1"
}


asset_name_for <- function(path) {

  path <- normalize_rel(path)

  out <- gsub(
    "/",
    "__",
    path,
    fixed = TRUE
  )

  out <- gsub(
    "[^A-Za-z0-9._-]+",
    "_",
    out
  )

  out <- gsub(
    "_+",
    "_",
    out
  )

  if (!nzchar(out)) {
    stop(
      paste0(
        "No se pudo construir nombre de asset para: ",
        path
      )
    )
  }

  out
}


release_url_for <- function(
    tag,
    asset_name
) {

  paste0(
    "https://github.com/",
    REPO,
    "/releases/download/",
    tag,
    "/",
    utils::URLencode(
      asset_name,
      reserved = TRUE
    )
  )
}


release_exists <- function(tag) {

  result <- run_cmd(
    "gh",
    c(
      "release",
      "view",
      tag,
      "--repo",
      REPO
    ),
    stdout = TRUE,
    stderr = TRUE,
    fail = FALSE
  )

  identical(result$status, 0L)
}


ensure_release <- function(tag) {

  if (release_exists(tag)) {
    return(invisible(TRUE))
  }

  message("Creando Release: ", tag)

  run_cmd(
    "gh",
    c(
      "release",
      "create",
      tag,
      "--repo",
      REPO,
      "--title",
      tag,
      "--notes",
      paste0(
        "FABDEM Watershed Runtime binary assets. ",
        "Generated automatically to serve runtime data without Git LFS."
      )
    )
  )

  invisible(TRUE)
}


release_assets <- function(tag) {

  if (!release_exists(tag)) {
    return(
      data.frame(
        ASSET_NAME = character(0),
        SIZE_BYTES = numeric(0),
        stringsAsFactors = FALSE
      )
    )
  }

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

check_tool("git")
check_tool("gh")

lfs_check <- run_cmd(
  "git",
  c("lfs", "version"),
  stdout = TRUE,
  stderr = TRUE,
  fail = FALSE
)

if (!identical(lfs_check$status, 0L)) {
  stop("git-lfs no esta instalado o no esta disponible.")
}


root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

if (!dir.exists(file.path(root, ".git"))) {
  stop(
    paste0(
      "Ejecuta este script desde la raiz del repositorio ",
      "FABDEM-Watershed-Runtime."
    )
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


auth <- run_cmd(
  "gh",
  c("auth", "status"),
  stdout = TRUE,
  stderr = TRUE,
  fail = FALSE
)

if (!identical(auth$status, 0L)) {
  stop(
    paste0(
      "GitHub CLI no esta autenticado. Ejecuta primero:\n",
      "gh auth login"
    )
  )
}


# ============================================================
# INVENTARIO LFS
# ============================================================

lfs_files <- run_cmd(
  "git",
  c(
    "lfs",
    "ls-files",
    "-n"
  )
)$output

lfs_files <- normalize_rel(
  trimws(lfs_files)
)

lfs_files <- unique(
  lfs_files[
    nzchar(lfs_files)
  ]
)

if (length(lfs_files) == 0L) {
  stop(
    "git lfs ls-files no devolvio ningun archivo."
  )
}


missing_files <- lfs_files[
  !file.exists(lfs_files)
]

if (length(missing_files) > 0L) {
  stop(
    paste0(
      "Faltan ",
      length(missing_files),
      " archivos LFS en la copia local. Ejemplos:\n",
      paste(
        head(missing_files, 20L),
        collapse = "\n"
      )
    )
  )
}


pointer_files <- lfs_files[
  vapply(
    lfs_files,
    is_lfs_pointer,
    logical(1)
  )
]

if (length(pointer_files) > 0L) {
  stop(
    paste0(
      "La copia local contiene punteros LFS en lugar de los binarios reales. ",
      "No se publicara nada. Ejemplos:\n",
      paste(
        head(pointer_files, 20L),
        collapse = "\n"
      )
    )
  )
}


sizes <- vapply(
  lfs_files,
  function(x) {
    suppressWarnings(
      as.numeric(
        file.info(x)$size
      )
    )
  },
  numeric(1)
)


if (
  any(!is.finite(sizes)) ||
  any(sizes <= 0)
) {
  stop(
    "Hay archivos LFS con tamano invalido."
  )
}


manifest <- data.frame(
  RELATIVE_PATH = lfs_files,
  RELEASE_TAG = vapply(
    lfs_files,
    release_tag_for,
    character(1)
  ),
  ASSET_NAME = vapply(
    lfs_files,
    asset_name_for,
    character(1)
  ),
  SIZE_BYTES = sizes,
  stringsAsFactors = FALSE
)


manifest$REMOTE_URL <- mapply(
  release_url_for,
  tag = manifest$RELEASE_TAG,
  asset_name = manifest$ASSET_NAME,
  USE.NAMES = FALSE
)


manifest$MD5 <- unname(
  tools::md5sum(
    manifest$RELATIVE_PATH
  )
)


# Un nombre de asset debe ser unico dentro de cada Release.
dup_key <- paste(
  manifest$RELEASE_TAG,
  manifest$ASSET_NAME,
  sep = "::"
)

if (anyDuplicated(dup_key)) {
  duplicated_rows <- manifest[
    duplicated(dup_key) |
      duplicated(dup_key, fromLast = TRUE),
    ,
    drop = FALSE
  ]

  stop(
    paste0(
      "Hay colisiones de nombres de assets. No se publicara nada.\n",
      paste(
        head(
          paste(
            duplicated_rows$RELEASE_TAG,
            duplicated_rows$RELATIVE_PATH,
            sep = " | "
          ),
          30L
        ),
        collapse = "\n"
      )
    )
  )
}


counts <- table(
  manifest$RELEASE_TAG
)

too_many <- counts[
  counts > MAX_ASSETS_PER_RELEASE
]

if (length(too_many) > 0L) {
  stop(
    paste0(
      "Una o mas Releases superan el limite operativo de ",
      MAX_ASSETS_PER_RELEASE,
      " assets:\n",
      paste(
        names(too_many),
        as.integer(too_many),
        sep = " = ",
        collapse = "\n"
      )
    )
  )
}


cat(
  "\nArchivos LFS reales detectados: ",
  nrow(manifest),
  "\nTamano total: ",
  sprintf(
    "%.2f GiB",
    sum(manifest$SIZE_BYTES) / 1024^3
  ),
  "\nReleases necesarias: ",
  length(unique(manifest$RELEASE_TAG)),
  "\n\n",
  sep = ""
)

print(
  data.frame(
    RELEASE_TAG = names(counts),
    N_ASSETS = as.integer(counts),
    SIZE_GIB = vapply(
      names(counts),
      function(tag) {
        sum(
          manifest$SIZE_BYTES[
            manifest$RELEASE_TAG == tag
          ]
        ) / 1024^3
      },
      numeric(1)
    ),
    row.names = NULL
  )
)


# ============================================================
# PUBLICACION
# ============================================================

tags <- unique(
  manifest$RELEASE_TAG
)


for (tag in tags) {

  ensure_release(tag)

  existing <- release_assets(tag)

  rows <- which(
    manifest$RELEASE_TAG == tag
  )

  cat(
    "\n============================================================\n",
    tag,
    "\nAssets: ",
    length(rows),
    "\n",
    sep = ""
  )

  for (j in seq_along(rows)) {

    i <- rows[j]

    source_file <- manifest$RELATIVE_PATH[i]
    asset_name <- manifest$ASSET_NAME[i]
    expected_size <- manifest$SIZE_BYTES[i]

    hit <- match(
      asset_name,
      existing$ASSET_NAME
    )

    already_ok <- (
      !is.na(hit) &&
      is.finite(existing$SIZE_BYTES[hit]) &&
      identical(
        as.numeric(existing$SIZE_BYTES[hit]),
        as.numeric(expected_size)
      )
    )

    if (
      isTRUE(already_ok) &&
      !isTRUE(FORCE_REUPLOAD)
    ) {
      cat(
        sprintf(
          "  [%d/%d] OK existente: %s\n",
          j,
          length(rows),
          asset_name
        )
      )
      next
    }

    upload_dir <- tempfile(
      pattern = "fabdem_release_"
    )

    dir.create(
      upload_dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    upload_file <- file.path(
      upload_dir,
      asset_name
    )

    copied <- file.copy(
      source_file,
      upload_file,
      overwrite = TRUE,
      copy.date = TRUE
    )

    if (!isTRUE(copied)) {
      unlink(
        upload_dir,
        recursive = TRUE,
        force = TRUE
      )

      stop(
        paste0(
          "No se pudo preparar asset temporal: ",
          source_file
        )
      )
    }

    cat(
      sprintf(
        "  [%d/%d] Subiendo %.2f MiB: %s\n",
        j,
        length(rows),
        expected_size / 1024^2,
        asset_name
      )
    )

    args <- c(
      "release",
      "upload",
      tag,
      normalizePath(
        upload_file,
        winslash = "/",
        mustWork = TRUE
      ),
      "--repo",
      REPO
    )

    if (
      isTRUE(FORCE_REUPLOAD) ||
      !is.na(hit)
    ) {
      args <- c(
        args,
        "--clobber"
      )
    }

    run_cmd(
      "gh",
      args,
      stdout = TRUE,
      stderr = TRUE,
      fail = TRUE
    )

    unlink(
      upload_dir,
      recursive = TRUE,
      force = TRUE
    )
  }
}


# ============================================================
# VERIFICACION REMOTA
# ============================================================

cat(
  "\nVerificando nombres y tamanos remotos...\n"
)

problems <- character(0)

for (tag in tags) {

  remote <- release_assets(tag)

  rows <- which(
    manifest$RELEASE_TAG == tag
  )

  for (i in rows) {

    hit <- match(
      manifest$ASSET_NAME[i],
      remote$ASSET_NAME
    )

    if (is.na(hit)) {
      problems <- c(
        problems,
        paste0(
          tag,
          " | ausente | ",
          manifest$ASSET_NAME[i]
        )
      )
      next
    }

    if (
      !is.finite(remote$SIZE_BYTES[hit]) ||
      !identical(
        as.numeric(remote$SIZE_BYTES[hit]),
        as.numeric(manifest$SIZE_BYTES[i])
      )
    ) {
      problems <- c(
        problems,
        paste0(
          tag,
          " | tamano distinto | ",
          manifest$ASSET_NAME[i]
        )
      )
    }
  }
}


if (length(problems) > 0L) {
  stop(
    paste0(
      "La verificacion remota fallo. No se publicara el manifiesto:\n",
      paste(
        head(problems, 50L),
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# MANIFIESTO Y PUSH
# ============================================================

manifest <- manifest[
  order(
    manifest$RELEASE_TAG,
    manifest$RELATIVE_PATH
  ),
  ,
  drop = FALSE
]

utils::write.csv(
  manifest,
  MANIFEST_FILE,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


run_cmd(
  "git",
  c(
    "add",
    MANIFEST_FILE
  )
)


status_manifest <- run_cmd(
  "git",
  c(
    "status",
    "--porcelain",
    "--",
    MANIFEST_FILE
  )
)$output


if (length(status_manifest) > 0L) {

  run_cmd(
    "git",
    c(
      "commit",
      "-m",
      "Publish runtime Release manifest"
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

} else {
  message(
    "El manifiesto no cambio; no fue necesario crear commit."
  )
}


cat(
  "\n============================================================\n",
  "MIGRACION COMPLETADA\n",
  "============================================================\n",
  "Manifest: ",
  MANIFEST_FILE,
  "\nAssets verificados: ",
  nrow(manifest),
  "\nTamano total: ",
  sprintf("%.2f GiB", sum(manifest$SIZE_BYTES) / 1024^3),
  "\n\n",
  "La aplicacion FABDEM Watershed Explorer ya puede resolver estos ",
  "archivos desde GitHub Releases.\n",
  "Todavia NO se eliminaron los objetos/punteros LFS del repositorio. ",
  "Primero verifica la web y luego ejecuta la fase de limpieza.\n",
  sep = ""
)
