local({
  paths <- vapply(sys.frames(),function(x) if(is.null(x$ofile)) "" else as.character(x$ofile[[1]]),character(1))
  paths <- paths[nzchar(paths)]
  args <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  script <- if(length(paths)) tail(paths,1) else if(length(args)) sub("^--file=","",args[1]) else "install_main_dependencies.R"
  root <- dirname(normalizePath(script,winslash="/",mustWork=TRUE))
  source(file.path(root,"scripts/runtime.R"),local=TRUE)
  if(getRversion()<"4.5.0" || .Machine$sizeof.pointer!=8) stop("Install 64-bit R 4.5 or later first. RStudio is optional.")
  lib <- mts_use_library(create=TRUE)
  options(timeout=max(600,getOption("timeout",60)))
  dir.create(file.path(root,"logs"),showWarnings=FALSE)
  report <- file.path(root,"logs","dependency-install.log")
  connection <- file(report,open="wt"); sink(connection,split=TRUE)
  on.exit({sink(); close(connection)},add=TRUE)
  cat("Make Thin Section v70 Lite dependency setup\nR:",R.version.string,"\nLibrary:",lib,"\n")
  missing <- mts_missing_packages(root)
  if(length(missing)) {
    cat("Installing missing/unloadable packages:",paste(missing,collapse=", "),"\n")
    # Only required dependencies, not the much larger set of suggested packages.
    utils::install.packages(missing,lib=lib,repos="https://cloud.r-project.org",
      dependencies=NA,type=if(.Platform$OS.type=="windows") "binary" else "source")
  } else cat("All required packages are already available; nothing downloaded.\n")
  mts_check_packages(root)
  req <- utils::read.csv(file.path(root,"R_REQUIREMENTS.csv"),stringsAsFactors=FALSE)
  req$InstalledVersion <- vapply(req$Package,function(p) as.character(utils::packageVersion(p)),character(1))
  utils::write.csv(req,file.path(root,"logs","installed_R_packages.csv"),row.names=FALSE)
  print(req[,c("Package","TestedVersion","InstalledVersion")],row.names=FALSE)
  if(any(req$TestedVersion!=req$InstalledVersion)) cat("Some versions differ from the release test environment. Run the demo before using a working database.\n")
  cat("\nSUCCESS: all required namespaces load. Start 3_Start_Demo.cmd.\nNo Python or Image Fragmentator dependencies were installed.\n")
})
