local({
  frames <- sys.frames()
  paths <- vapply(frames,function(x) if(is.null(x$ofile)) "" else as.character(x$ofile[[1]]),character(1))
  paths <- paths[nzchar(paths)]
  args <- grep("^--file=",commandArgs(FALSE),value=TRUE)
  path <- if(length(paths)) tail(paths,1) else if(length(args)) sub("^--file=","",args[1]) else ""
  root <- if(nzchar(path)) dirname(normalizePath(path,winslash="/",mustWork=TRUE)) else getwd()
  source(file.path(root,"scripts","run_suite.R"),local=TRUE,encoding="UTF-8")
  mts_run_suite(root,"lan")
})

