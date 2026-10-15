source("scripts/runtime.R", local=TRUE)
mts_check_packages(getwd())
source("scripts/fragmentator_launcher.R", local=TRUE, encoding="UTF-8")
ui <- shiny::fluidPage(
  shiny::titlePanel("Make Thin Section - connection settings"),
  shiny::helpText("Choose files on this computer or paste ordinary Windows paths. Quotes from Copy as path are accepted. No R code is needed."),
  shiny::h3("SQLite database"),
  shiny::textInput("database", "Existing database file", value=mts_saved_path("database_path.txt"), width="100%"),
  shiny::actionButton("browse_db", "Choose database..."),
  shiny::actionButton("save_db", "Check and save database"),
  shiny::verbatimTextOutput("db_status"),
  shiny::h3("Optional Image Fragmentator"),
  shiny::textInput("fragmentator", "Installation folder", value=mts_fragmentator_root(), width="100%"),
  shiny::actionButton("browse_frag", "Choose folder..."),
  shiny::actionButton("save_frag", "Check and save Fragmentator"),
  shiny::verbatimTextOutput("frag_status"),
  shiny::helpText("Settings apply on the next launch. Existing application sessions keep their current database. Environment variables PETRO_DB_PATH and MTS_FRAGMENTATOR_ROOT override these saved choices."),
  shiny::actionButton("done", "Finish setup"))
server <- function(input, output, session) {
  db_status <- shiny::reactiveVal(""); frag_status <- shiny::reactiveVal("")
  output$db_status <- shiny::renderText(db_status())
  output$frag_status <- shiny::renderText(frag_status())
  shiny::observeEvent(input$browse_db, {
    tryCatch({ p <- mts_browse_database(); if(!is.null(p)) shiny::updateTextInput(session,"database",value=p) }, error=function(e) db_status(conditionMessage(e)))
  })
  shiny::observeEvent(input$browse_frag, {
    tryCatch({ p <- mts_browse_fragmentator(); if(!is.null(p)) shiny::updateTextInput(session,"fragmentator",value=p) }, error=function(e) frag_status(conditionMessage(e)))
  })
  shiny::observeEvent(input$save_db, {
    tryCatch({ p <- mts_validate_database(input$database); mts_save_path(p,"database_path.txt");
      db_status(paste("Database verified and saved. Restart Make Thin Section to use:", p)) }, error=function(e) db_status(conditionMessage(e)))
  })
  shiny::observeEvent(input$save_frag, {
    tryCatch({ p <- mts_validate_fragmentator(input$fragmentator); mts_save_path(p,"fragmentator_path.txt");
      frag_status(paste("Fragmentator installation found. Path saved:", p)) }, error=function(e) frag_status(conditionMessage(e)))
  })
  shiny::observeEvent(input$done, shiny::stopApp())
}
port <- as.integer(Sys.getenv("MTS_PORT", unset=as.character(httpuv::randomPort())))
shiny::runApp(shiny::shinyApp(ui,server),host="127.0.0.1",port=port,
  launch.browser=!identical(Sys.getenv("MTS_BROWSER"),"FALSE"))
