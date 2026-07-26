# Install R packages required by Image Fragmentator.
#
# Run this file once before the first Fragmentator launch:
#   source("install_fragmentator_dependencies.R")
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

cran_packages <- c(
  "shiny",
  "magick",
  "png",
  "BiocManager"
)

is_installed <- function(package) {
  requireNamespace(package, quietly = TRUE)
}

missing_cran_packages <- cran_packages[
  !vapply(cran_packages, is_installed, logical(1))
]

cat("\nImage Fragmentator dependency installer\n")
cat("CRAN repository:", getOption("repos")[["CRAN"]], "\n\n")

if (length(missing_cran_packages) > 0) {
  cat(
    "Installing missing CRAN packages:\n  ",
    paste(missing_cran_packages, collapse = ", "),
    "\n\n",
    sep = ""
  )

  install.packages(missing_cran_packages)
} else {
  cat("All required CRAN packages are already installed.\n")
}

if (!is_installed("BiocManager")) {
  stop(
    "\nBiocManager could not be installed. ",
    "Review the CRAN installation messages above."
  )
}

if (!is_installed("EBImage")) {
  cat("\nInstalling EBImage from Bioconductor...\n\n")
  BiocManager::install("EBImage", ask = FALSE, update = FALSE)
} else {
  cat("EBImage is already installed.\n")
}

required_packages <- c("shiny", "magick", "png", "BiocManager", "EBImage")
still_missing <- required_packages[
  !vapply(required_packages, is_installed, logical(1))
]

if (length(still_missing) > 0) {
  stop(
    "\nSome Fragmentator packages could not be installed:\n  ",
    paste(still_missing, collapse = ", "),
    "\n\nReview the installation messages above."
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

cat("\nImage Fragmentator dependencies are ready.\n\n")
