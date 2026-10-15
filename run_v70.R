local({
  args <- commandArgs(FALSE)
  path <- sub("^--file=","",grep("^--file=",args,value=TRUE)[1])
  root <- dirname(normalizePath(path,winslash="/",mustWork=TRUE))
  setwd(root)
  mode <- commandArgs(TRUE)[1]
  if(identical(mode,"paths")) source("scripts/configure_paths.R",encoding="UTF-8") else
  if(identical(mode,"setup")) source("install_main_dependencies.R",encoding="UTF-8") else {
    source("scripts/run_suite.R",local=TRUE,encoding="UTF-8")
    mts_run_suite(root,mode)
  }
})
