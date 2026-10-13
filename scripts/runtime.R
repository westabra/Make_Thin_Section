mts_library_path <- function() {
  version <- paste(R.version$major,strsplit(R.version$minor,".",fixed=TRUE)[[1]][1],sep=".")
  Sys.getenv("MTS_R_LIBRARY",unset=file.path(tools::R_user_dir("MakeThinSection","data"),"R-library",version))
}
mts_use_library <- function(create=FALSE) {
  path <- mts_library_path()
  if(create && !dir.exists(path) && !dir.create(path,recursive=TRUE)) stop("Cannot create R library: ",path)
  .libPaths(c(path,.libPaths()))
  invisible(path)
}
mts_required_packages <- function(root) {
  utils::read.csv(file.path(root,"R_REQUIREMENTS.csv"),stringsAsFactors=FALSE)$Package
}
mts_missing_packages <- function(root) {
  packages <- mts_required_packages(root)
  packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
}
mts_check_packages <- function(root) {
  mts_use_library()
  missing <- mts_missing_packages(root)
  if(length(missing)) stop("Missing or unloadable R packages: ",paste(missing,collapse=", "),
    ". Run 1_Check_and_install_dependencies.cmd (internet required).",call.=FALSE)
  invisible(TRUE)
}
