# Launch Make_Thin_Section with the demo SQLite database located next to this file.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file) || !nzchar(script_file)) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- args[grepl("^--file=", args)]
  script_file <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else NULL
}

app_dir <- if (!is.null(script_file) && nzchar(script_file)) {
  normalizePath(dirname(script_file), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

demo_db <- file.path(app_dir, "Make_Thin_Section_demo.sqlite")
if (!file.exists(demo_db)) {
  stop("Demo database not found: ", demo_db,
       "\nRun create_demo_sqlite.R first.")
}

Sys.setenv(PETRO_DB_PATH = demo_db)
setwd(app_dir)
shiny::runApp("app_Make_Thin_Section_version_62_mineral_reference_web_search.R")
