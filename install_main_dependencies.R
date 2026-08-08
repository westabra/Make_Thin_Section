# Install R packages required by Make Thin Section.
#
# Run this file once before the first launch:
#   source("install_main_dependencies.R")
#
# Packages that are already installed are not downloaded again.

cran_repo <- getOption("repos")[["CRAN"]]
if (is.null(cran_repo) ||
    length(cran_repo) == 0 ||
    is.na(cran_repo) ||
    !nzchar(cran_repo) ||
    identical(cran_repo, "@CRAN@")) {
  cran_repo <- "https://cloud.r-project.org"
}

options(repos = c(CRAN = cran_repo))

core_packages <- c(
  "shiny",
  "DT",
  "officer",
  "flextable",
  "magick",
  "base64enc",
  "sf",
  "leaflet",
  "DBI",
  "RSQLite",
  "dplyr",
  "stringr",
  "readr",
  "openxlsx",
  "tibble"
)

advanced_export_packages <- c(
  "htmlwidgets",
  "webshot2",
  "zip"
)

required_packages <- unique(c(core_packages, advanced_export_packages))

is_installed <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

missing_packages <- required_packages[
  !vapply(required_packages, is_installed, logical(1))
]

cat("\nMake Thin Section dependency installer\n")
cat("CRAN repository:", getOption("repos")[["CRAN"]], "\n\n")

if (length(missing_packages) > 0) {
  cat(
    "Installing missing packages:\n  ",
    paste(missing_packages, collapse = ", "),
    "\n\n",
    sep = ""
  )

  install.packages(missing_packages)
} else {
  cat("All required packages are already installed.\n")
}

still_missing <- required_packages[
  !vapply(required_packages, is_installed, logical(1))
]

if (length(still_missing) > 0) {
  stop(
    "\nSome packages could not be installed:\n  ",
    paste(still_missing, collapse = ", "),
    "\n\nReview the installation messages above. On Windows, use a recent ",
    "version of R so CRAN binary packages are available."
  )
}

installed_versions <- vapply(
  required_packages,
  function(package) as.character(utils::packageVersion(package)),
  character(1)
)

cat("\nInstallation check completed successfully.\n")
cat("Installed package versions:\n")
for (package in names(installed_versions)) {
  cat(sprintf("  %-14s %s\n", package, installed_versions[[package]]))
}

cat("\nYou can now start the demonstration with:\n")
cat('  source("run_demo.R")\n\n')
