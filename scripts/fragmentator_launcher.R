
source(file.path(getwd(), "scripts", "path_settings.R"), local=TRUE, encoding="UTF-8")
mts_fragmentator_root <- function() {
  env <- Sys.getenv("MTS_FRAGMENTATOR_ROOT")
  if(nzchar(env)) return(mts_clean_path(env))
  if(file.exists("fragmentator_path.txt")) return(mts_saved_path("fragmentator_path.txt"))
  ""
}
mts_validate_fragmentator <- function(root) {
  if(length(root)!=1 || is.na(root) || !nzchar(trimws(root))) stop("Image Fragmentator is not included. Use Configure Fragmentator to select an existing 7.7.0 Portable installation.")
  root <- normalizePath(mts_clean_path(root),winslash="/",mustWork=TRUE)
  required <- c("portable_tools/launch.R", "runtime/python/python.exe", "weights/FastSAM-x.pt", "app/image_fragmentator_V7_7_0_grainsight.R", "app/grainsight_support.R")
  missing <- required[!file.exists(file.path(root,required))]
  if(length(missing)) stop("Incomplete Fragmentator installation: ",paste(missing,collapse=", "))
  root
}

mts_launch_fragmentator <- function(root, browser=TRUE, port=httpuv::randomPort()) {
  root <- mts_validate_fragmentator(root)
  required <- c("portable_tools/launch.R", "runtime/python/python.exe", "weights/FastSAM-x.pt",
                "app/image_fragmentator_V7_7_0_grainsight.R", "app/grainsight_support.R")
  missing <- required[!file.exists(file.path(root,required))]
  if (length(missing)) stop("Incomplete portable package: ",paste(missing,collapse=", "))
  rscript <- file.path(R.home("bin"),"Rscript.exe")
  if (!file.exists(rscript)) stop("Rscript.exe not found in current R installation.")
  log_path <- file.path(root,"logs","launched-from-mts.log")
  dir.create(dirname(log_path),recursive=TRUE,showWarnings=FALSE)
  env <- c(FRAGMENTATOR_PORTABLE_ROOT=root, PYTHONNOUSERSITE="1", PYTHONDONTWRITEBYTECODE="1",
           MTS_FRAGMENTATOR_BROWSER=if (browser) "TRUE" else "FALSE", MTS_FRAGMENTATOR_PORT=as.character(port))
  version <- paste(R.version$major,strsplit(R.version$minor,".",fixed=TRUE)[[1]][1],sep=".")
  env <- c(env,R_LIBS_USER=paste(unique(c(file.path(root,"R-library",version),.libPaths())),collapse=.Platform$path.sep),R_LIBS_SITE="")
  process <- processx::process$new(rscript,c("--vanilla",file.path(root,"portable_tools","launch.R")),
    wd=root, env=c("current",env), stdout=log_path, stderr="2>&1", cleanup=TRUE, cleanup_tree=TRUE,
    windows_hide_window=TRUE)
  list(process=process, url=paste0("http://127.0.0.1:",port), log_path=log_path)
}

