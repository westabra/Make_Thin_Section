mts_run_suite <- function(root,mode="local") {
  if(!mode %in% c("local","lan","demo")) stop("Unknown launch mode: ",mode)
  root <- normalizePath(root,winslash="/",mustWork=TRUE)
  old <- setwd(root); on.exit(setwd(old),add=TRUE)
  source(file.path(root,"scripts/runtime.R"),local=TRUE)
  mts_check_packages(root)
  source(file.path(root,"config.R"),local=TRUE,encoding="UTF-8")
  source(file.path(root,"scripts/path_settings.R"),local=TRUE,encoding="UTF-8")
  db <- mts_database_path(root,MTS_DATABASE,mode)
  db <- tryCatch(mts_validate_database(db), error=function(e)
    stop(conditionMessage(e), " Run 5_Configure_paths.cmd to select your database, or start 3_Start_Demo.cmd.",call.=FALSE))
  host <- if(mode=="lan") MTS_LAN_HOST else "127.0.0.1"
  port <- suppressWarnings(as.integer(Sys.getenv("MTS_PORT",unset=as.character(if(mode=="lan") MTS_LAN_PORT else MTS_LOCAL_PORT))))
  if(length(port)!=1 || is.na(port) || port<1 || port>65535) stop("Invalid TCP port in config.R or MTS_PORT.")
  Sys.setenv(PETRO_DB_PATH=db,SHINY_READ_ONLY=if(mode=="lan") "TRUE" else "FALSE")
  message("Make Thin Section v70 Lite\nDatabase: ",db,"\nMode: ",mode,"\nAddress: http://",host,":",port)
  shiny::runApp(file.path(root,"app_Make_Thin_Section_v70.R"),host=host,port=port,
    launch.browser=mode!="lan" && !identical(Sys.getenv("MTS_BROWSER"),"FALSE"))
}
