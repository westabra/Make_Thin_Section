mts_run_suite <- function(root,mode="local") {
  if(!mode %in% c("local","lan","demo")) stop("Unknown launch mode: ",mode)
  root <- normalizePath(root,winslash="/",mustWork=TRUE)
  old <- setwd(root); on.exit(setwd(old),add=TRUE)
  source(file.path(root,"scripts/runtime.R"),local=TRUE)
  mts_check_packages(root)
  source(file.path(root,"config.R"),local=TRUE,encoding="UTF-8")
  db <- if(mode=="demo") file.path(root,"Make_Thin_Section_demo.sqlite") else Sys.getenv("PETRO_DB_PATH",unset=MTS_DATABASE)
  if(!file.exists(db)) stop("Database not found: ",db,". Set MTS_DATABASE in config.R, or start 3_Start_Demo.cmd.")
  db <- normalizePath(db,winslash="/",mustWork=TRUE)
  host <- if(mode=="lan") MTS_LAN_HOST else "127.0.0.1"
  port <- suppressWarnings(as.integer(Sys.getenv("MTS_PORT",unset=as.character(if(mode=="lan") MTS_LAN_PORT else MTS_LOCAL_PORT))))
  if(length(port)!=1 || is.na(port) || port<1 || port>65535) stop("Invalid TCP port in config.R or MTS_PORT.")
  Sys.setenv(PETRO_DB_PATH=db,SHINY_READ_ONLY=if(mode=="lan") "TRUE" else "FALSE")
  message("Make Thin Section v70 Lite\nDatabase: ",db,"\nMode: ",mode,"\nAddress: http://",host,":",port)
  shiny::runApp(file.path(root,"app_Make_Thin_Section_v70.R"),host=host,port=port,
    launch.browser=mode!="lan" && !identical(Sys.getenv("MTS_BROWSER"),"FALSE"))
}
