# Read-only keyword search. Project/sample identity is always a composite key.
mts_search_empty <- function() data.frame(project_id=integer(), project_name=character(),
  sample_id=character(), rock_name=character(), field_rock_name=character(),
  suggested_unit=character(), mineral_composition=character(), field_desc=character(),
  rock_desc=character(), stringsAsFactors=FALSE)

mts_search_projects <- function(con) {
  DBI::dbGetQuery(con,"SELECT project_id, project_name FROM projects ORDER BY project_name")
}

mts_search_samples <- function(con, scope="one", projects=integer(), terms=list(), combine="all") {
  fields <- c("rock_name","field_rock_name","suggested_unit","mineral_composition")
  if (!(scope %in% c("one","several","all"))) stop("Choose a valid search scope.")
  if (!(combine %in% c("all","any"))) stop("Choose how to combine the filters.")
  terms <- setNames(lapply(fields,function(k) {
    x <- terms[[k]]
    if (is.null(x)) return("")
    if (length(x)!=1L || is.na(x)) stop("Each keyword must be one text value.")
    trimws(as.character(x))
  }),fields)
  active <- fields[vapply(terms,nzchar,logical(1))]
  if (!length(active)) stop("Enter a keyword in at least one field.")
  projects <- unique(as.character(projects))
  if (scope!="all" && (!length(projects) || any(!grepl("^[0-9]+$",projects)))) stop("Select the project(s) to search.")
  if (scope=="one" && length(projects)!=1L) stop("Select exactly one project.")
  where <- if (scope=="all") "" else paste0(" WHERE s.project_id IN (",paste(rep("?",length(projects)),collapse=","),")")
  params <- if(scope=="all") NULL else as.list(as.numeric(projects))
  cols <- DBI::dbListFields(con,"samples")
  expr <- function(name) if(name %in% cols) paste0("COALESCE(s.",name,",'') AS ",name) else paste0("'' AS ",name)
  mapping <- "suggested_mapping_unit_id" %in% cols && DBI::dbExistsTable(con,"mapping_units")
  unit <- if(mapping) "COALESCE(u.unit_name,'') AS suggested_unit" else "'' AS suggested_unit"
  join <- if(mapping) " LEFT JOIN mapping_units u ON u.unit_id=s.suggested_mapping_unit_id" else ""
  sql <- paste0("SELECT s.project_id, p.project_name, s.sample_id, ",
    paste(vapply(c("rock_name","field_rock_name","field_desc","rock_desc"),expr,character(1)),collapse=","),",",unit,
    " FROM samples s JOIN projects p ON p.project_id=s.project_id",join,where," ORDER BY p.project_name,s.sample_id")
  rows <- DBI::dbGetQuery(con,sql,params=params)
  rows$mineral_composition <- rep("",nrow(rows))
  if(nrow(rows) && DBI::dbExistsTable(con,"mineral_composition")) {
    mcols <- DBI::dbListFields(con,"mineral_composition")
    mexpr <- function(n) if(n %in% mcols) paste0("COALESCE(m.",n,",'') AS ",n) else paste0("'' AS ",n)
    mq <- paste0("SELECT m.project_id,m.sample_id,",paste(vapply(c("mineral","vol_text","morphology","genesis"),mexpr,character(1)),collapse=","),
      " FROM mineral_composition m JOIN samples s ON s.project_id=m.project_id AND s.sample_id=m.sample_id",where,
      " ORDER BY m.project_id,m.sample_id",if("row_order" %in% mcols) ",m.row_order" else "",if("id" %in% mcols) ",m.id" else "")
    minerals <- DBI::dbGetQuery(con,mq,params=params)
    if(nrow(minerals)) {
      line <- apply(minerals[c("mineral","vol_text","morphology","genesis")],1,function(x) paste(x[nzchar(trimws(x))],collapse=" | "))
      key <- function(x) paste(x$project_id,x$sample_id,sep="\034")
      grouped <- vapply(split(line,key(minerals)),paste,collapse="; ",FUN.VALUE=character(1))
      text <- unname(grouped[key(rows)])
      rows$mineral_composition <- ifelse(is.na(text),"",text)
    }
  }
  # Unicode case folding and literal matching: %, _, quotes and regex characters are ordinary text.
  matches <- lapply(active,function(k) stringi::stri_detect_fixed(rows[[k]],terms[[k]],case_insensitive=TRUE))
  keep <- Reduce(if(combine=="all") `&` else `|`,matches)
  rows <- rows[!is.na(keep) & keep,,drop=FALSE]
  rows[,names(mts_search_empty()),drop=FALSE]
}

mts_database_search_ui <- function(id="rock_search") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Search rock descriptions"),
    shiny::tags$p(class="small-note","Search saved SQLite records. Keywords are literal, case-insensitive substrings; each box accepts a word or a phrase. Empty boxes are ignored."),
    shiny::radioButtons(ns("scope"),"Search in",c("One project"="one","Selected projects"="several","Entire database"="all"),inline=TRUE),
    shiny::uiOutput(ns("project_controls")),
    shiny::actionButton(ns("refresh"),"Refresh project list"),
    shiny::fluidRow(
      shiny::column(6,shiny::textInput(ns("rock_name"),"4) Rock name (microscope identification)"),
        shiny::textInput(ns("field_rock_name"),"2a) Field Rock Name (field identification)")),
      shiny::column(6,shiny::textInput(ns("suggested_unit"),"Suggested lithological unit"),
        shiny::textInput(ns("mineral_composition"),"Mineral composition (names and row descriptions)"))),
    shiny::radioButtons(ns("combine"),"Combine non-empty fields",c("Match all (AND)"="all","Match any (OR)"="any"),inline=TRUE),
    shiny::tags$div(style="display:flex;gap:8px;flex-wrap:wrap;",
      shiny::actionButton(ns("search"),"Search",class="btn-primary"),
      shiny::actionButton(ns("clear"),"Clear search"),
      shiny::downloadButton(ns("csv"),"Export results (CSV)")),
    shiny::textOutput(ns("status")), DT::DTOutput(ns("results")),
    shiny::actionButton(ns("open"),"Open selected sample"),
    shiny::uiOutput(ns("preview")),shiny::tags$hr())
}

mts_database_search_server <- function(id, connect, load_sample) {
  shiny::moduleServer(id,function(input,output,session) {
    projects <- shiny::reactiveVal(data.frame(project_id=integer(),project_name=character()))
    results <- shiny::reactiveVal(mts_search_empty())
    status <- shiny::reactiveVal("Enter a keyword and click Search.")
    pending <- shiny::reactiveVal(NULL)
    run_safe <- function(expr) tryCatch(force(expr),error=function(e) {
      status(conditionMessage(e)); shiny::showNotification(conditionMessage(e),type="error"); NULL
    })
    refresh <- function() run_safe({ con <- connect(); on.exit(DBI::dbDisconnect(con)); projects(mts_search_projects(con)) })
    shiny::observeEvent(input$refresh,refresh(),ignoreInit=TRUE)
    session$onFlushed(refresh,once=TRUE)
    output$project_controls <- shiny::renderUI({
      p <- projects(); choices <- stats::setNames(as.character(p$project_id),p$project_name)
      if(identical(input$scope,"all")) return(shiny::tags$p("All saved projects will be searched."))
      shiny::selectizeInput(session$ns("projects"),"Project(s)",choices=choices,
        multiple=identical(input$scope,"several"), options=list(placeholder="Select project(s)..."))
    })
    shiny::observeEvent(input$search,{
      results(mts_search_empty()); pending(NULL)
      run_safe({
        con <- connect(); on.exit(DBI::dbDisconnect(con))
        df <- mts_search_samples(con,input$scope,input$projects,list(rock_name=input$rock_name,
          field_rock_name=input$field_rock_name,suggested_unit=input$suggested_unit,mineral_composition=input$mineral_composition),input$combine)
        results(df)
        status(paste(nrow(df),"matching sample(s). Results reflect the last Search; rerun after changing filters or saved records."))
      })
    },ignoreInit=TRUE)
    shiny::observeEvent(input$clear,{
      for(k in c("rock_name","field_rock_name","suggested_unit","mineral_composition")) shiny::updateTextInput(session,k,value="")
      results(mts_search_empty()); pending(NULL); status("Search cleared.")
    },ignoreInit=TRUE)
    output$status <- shiny::renderText(status())
    output$results <- DT::renderDT({
      df <- results()[,c("project_name","sample_id","rock_name","field_rock_name","suggested_unit","mineral_composition"),drop=FALSE]
      names(df) <- c("Project","Sample ID","Microscope rock name","Field rock name","Suggested lithological unit","Mineral composition")
      DT::datatable(df,rownames=FALSE,escape=TRUE,selection="single",filter="none",options=list(pageLength=10,scrollX=TRUE))
    },server=TRUE)
    selected <- shiny::reactive({
      i <- input$results_rows_selected; df <- results()
      if(length(i)!=1L || is.na(i) || i<1 || i>nrow(df)) return(NULL)
      df[i,,drop=FALSE]
    })
    output$preview <- shiny::renderUI({
      r <- selected(); if(is.null(r)) return(NULL)
      shiny::tags$details(open=NA,shiny::tags$summary(paste(r$project_name,r$sample_id,sep=" / ")),
        shiny::h5("Field description"),shiny::tags$pre(r$field_desc),
        shiny::h5("Microscope description"),shiny::tags$pre(r$rock_desc),
        shiny::h5("Mineral composition"),shiny::tags$pre(r$mineral_composition))
    })
    shiny::observeEvent(input$open,{
      r <- selected(); shiny::req(!is.null(r)); pending(r)
      shiny::showModal(shiny::modalDialog(title="Open saved sample",
        paste("Load",r$project_name,"/",r$sample_id,"? This replaces the current form. Save any work you want to keep first."),
        footer=shiny::tagList(shiny::modalButton("Cancel"),shiny::actionButton(session$ns("confirm"),"Load sample",class="btn-primary"))))
    },ignoreInit=TRUE)
    shiny::observeEvent(input$confirm,{
      r <- pending(); shiny::req(!is.null(r)); pending(NULL); shiny::removeModal()
      run_safe(load_sample(r$project_name[[1]],r$sample_id[[1]]))
    },ignoreInit=TRUE)
    output$csv <- shiny::downloadHandler(filename=function() paste0("rock_search_",Sys.Date(),".csv"),content=function(file) {
      # Excel-safe text export: avoid interpreting stored descriptions as spreadsheet formulas.
      df <- results(); for(k in names(df)) if(is.character(df[[k]])) {
        dangerous <- grepl("^[[:space:]]*[=+@-]",df[[k]])
        df[[k]][dangerous] <- paste0("'",df[[k]][dangerous])
      }
      con <- file(file,open="wb"); on.exit(close(con)); writeBin(as.raw(c(239,187,191)),con)
      utils::write.table(df,con,sep=",",row.names=FALSE,na="",qmethod="double",fileEncoding="UTF-8")
    })
    list(results=results,selected=selected)
  })
}
