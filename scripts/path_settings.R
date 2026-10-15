# Accept paths as data, never as R expressions.
mts_clean_path <- function(path) {
  if (length(path) != 1L || is.na(path)) stop("Enter one path.")
  path <- trimws(sub("^\ufeff", "", path))
  if (nchar(path) >= 2L && substr(path, 1L, 1L) %in% c('"', "'") &&
      substr(path, nchar(path), nchar(path)) == substr(path, 1L, 1L))
    path <- trimws(substr(path, 2L, nchar(path)-1L))
  if (!nzchar(path) || grepl("[\r\n]", path)) stop("Enter one non-empty file or folder path.")
  chartr("\\", "/", path)
}
mts_saved_path <- function(file) {
  if (!file.exists(file)) return("")
  lines <- readLines(file, warn=FALSE, encoding="UTF-8")
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return("")
  if (length(lines) != 1L) stop("Use one path on one line in ", file)
  mts_clean_path(lines)
}
mts_database_path <- function(root, fallback, mode="local") {
  if (mode == "demo") return(file.path(root, "Make_Thin_Section_demo.sqlite"))
  env <- Sys.getenv("PETRO_DB_PATH")
  saved <- if (nzchar(env)) "" else mts_saved_path(file.path(root, "database_path.txt"))
  mts_clean_path(if (nzchar(env)) env else if (nzchar(saved)) saved else fallback)
}
mts_validate_database <- function(path) {
  path <- mts_clean_path(path)
  if (!file.exists(path) || dir.exists(path)) stop("Database file not found: ", path)
  path <- normalizePath(path, winslash="/", mustWork=TRUE)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname=path, flags=RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(con), add=TRUE)
  if (!"samples" %in% DBI::dbListTables(con)) stop("This is not a Make Thin Section database: the samples table is missing.")
  path
}
mts_save_path <- function(path, file) {
  path <- mts_clean_path(path)
  # Prepare the complete setting before replacing the previous one.
  tmp <- tempfile("path-setting-", tmpdir=dirname(file))
  on.exit(unlink(tmp), add=TRUE)
  writeLines(enc2utf8(path), tmp, useBytes=TRUE)
  if (!file.copy(tmp, file, overwrite=TRUE)) stop("Cannot save settings in this folder: ", dirname(file))
  invisible(path)
}
mts_browse_database <- function() {
  if (.Platform$OS.type != "windows") stop("Paste the database path on this operating system.")
  result <- utils::choose.files(caption="Select an existing Make Thin Section SQLite database", multi=FALSE)
  if (!length(result) || !nzchar(result)) return(NULL)
  mts_clean_path(result)
}
mts_browse_fragmentator <- function() {
  if (.Platform$OS.type != "windows") stop("Paste the installation folder on this operating system.")
  result <- utils::choose.dir(caption="Select the Image Fragmentator installation folder")
  if (is.na(result) || !nzchar(result)) return(NULL)
  mts_clean_path(result)
}
