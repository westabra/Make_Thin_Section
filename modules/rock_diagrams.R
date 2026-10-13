# Ternary modal diagrams. Percent coordinates: top, left, right (sum = 100).
# Geometry and classification share the same polygons, including boundary detection.
mts_ternary_xy <- function(x) {
  x <- matrix(x,ncol=3)
  cbind(x[,3]/100+x[,1]/200,sqrt(3)/2*x[,1]/100)
}
mts_clip <- function(poly, f) {
  if(!nrow(poly)) return(poly)
  out <- matrix(numeric(),ncol=3)
  for(i in seq_len(nrow(poly))) {
    a <- poly[i,]; b <- poly[if(i==nrow(poly)) 1 else i+1,]
    fa <- f(a); fb <- f(b); ina <- fa>=-1e-9; inb <- fb>=-1e-9
    if(ina) out <- rbind(out,a)
    if(xor(ina,inb)) out <- rbind(out,a+(b-a)*fa/(fa-fb))
  }
  unique(out)
}
mts_modal_definitions <- function() {
  triangle <- rbind(c(100,0,0),c(0,100,0),c(0,0,100))
  field <- function(name, ...) {
    rules <- list(...); p <- triangle
    for(rule in rules) p <- mts_clip(p,rule)
    list(name=name,polygon=p,rules=rules)
  }
  ge <- function(i,v) { force(i);force(v); function(x) x[i]-v }
  le <- function(i,v) { force(i);force(v); function(x) v-x[i] }
  # The 1:1 basal split is used only between the two mixed Px/Hbl fields.
  rge <- function(v) { force(v); function(x) x[3]-v*(x[2]+x[3]) }
  rle <- function(v) { force(v); function(x) v*(x[2]+x[3])-x[3] }
  make <- function(title, components, fields, note, guides=numeric(),source) list(title=title,
    components=components,fields=fields,note=note,guides=guides,source=source)
  bgs <- "Gillespie & Styles (1999), BGS RR 99-06, Figs. 13 and 16 (after Streckeisen)."
  amph <- make("Amphibolite modal field (SCMR)",c("Remaining constituents","Plagioclase","Amphibole"),
    list(field("Amphibolite modal field",le(1,25),ge(3,30),ge(2,5))),
    "Enter the entire rock mode: remaining constituents includes every mineral other than plagioclase and amphibole. Modal membership alone does not establish metamorphic texture or amphibole type.",
    source="SCMR, Amphibolite and Granulite (2007), Fig. 8.2; IGC2004 SCMR report.")
  oc <- list(field("Dunite",ge(1,90)),
    field("Harzburgite",ge(1,40),le(1,90),le(3,5)),
    field("Lherzolite",ge(1,40),le(1,90),ge(3,5),ge(2,5)),
    field("Wehrlite",ge(1,40),le(1,90),le(2,5)))
  # BGS Fig. 16: continuous 5% minor-component lines, terminated by
  # 90% basal-component end fields. These are NOT 10:90 basal ratios.
  oc <- c(oc,list(
    field("Olivine orthopyroxenite",le(1,40),le(3,5),le(2,90)),
    field("Olivine websterite",ge(1,5),le(1,40),ge(2,5),ge(3,5)),
    field("Olivine clinopyroxenite",le(1,40),le(2,5),le(3,90)),
    field("Orthopyroxenite",ge(2,90)),
    field("Websterite",le(1,5),le(2,90),le(3,90)),
    field("Clinopyroxenite",ge(3,90))))
  oh <- list(field("Dunite",ge(1,90)),
    field("Pyroxene peridotite",ge(1,40),le(1,90),le(3,5)),
    field("Pyroxene hornblende peridotite",ge(1,40),le(1,90),ge(3,5),ge(2,5)),
    field("Hornblende peridotite",ge(1,40),le(1,90),le(2,5)))
  oh <- c(oh,list(
    field("Olivine pyroxenite",le(1,40),le(3,5),le(2,90)),
    field("Olivine hornblende pyroxenite",ge(1,5),le(1,40),ge(2,5),ge(3,5),rle(.5)),
    field("Olivine pyroxene hornblendite",ge(1,5),le(1,40),ge(2,5),ge(3,5),rge(.5)),
    field("Olivine hornblendite",le(1,40),le(2,5),le(3,90)),
    field("Pyroxenite (s.s.)",ge(2,90)),
    field("Hornblende pyroxenite",le(1,5),le(2,90),le(3,90),rle(.5)),
    field("Pyroxene hornblendite",le(1,5),le(2,90),le(3,90),rge(.5)),
    field("Hornblendite (s.s.)",ge(3,90))))
  gab <- function(hornblende=FALSE) {
    upper <- list(field("Anorthosite",ge(1,90)),
      field("Gabbro / gabbronorite / norite",ge(1,10),le(1,90),le(3,5)),
      field(if(hornblende) "Pyroxene hornblende gabbro / gabbronorite / norite" else "Olivine gabbro / gabbronorite / norite",ge(1,10),le(1,90),ge(3,5),ge(2,5)),
      field(if(hornblende) "Hornblende gabbro" else "Troctolite",ge(1,10),le(1,90),le(2,5)))
    lower <- if(hornblende) list(
      field("Plagioclase-bearing pyroxenite",ge(2,90)),
      field("Plagioclase-bearing hornblende pyroxenite",le(1,10),le(2,90),le(3,90),rle(.5)),
      field("Plagioclase-bearing pyroxene hornblendite",le(1,10),le(2,90),le(3,90),rge(.5)),
      field("Plagioclase-bearing hornblendite",ge(3,90))) else
      list(field("Plagioclase-bearing ultramafic rocks",le(1,10)))
    c(upper,lower)
  }
  ig_note <- "Use modal volume proportions of the three named components. Values are normalized to 100%. Confirm the whole-rock classification context before applying a rock name."
  list(amphibolite=amph,
    ol_opx_cpx=make("Olivine - Orthopyroxene - Clinopyroxene",c("Olivine","Orthopyroxene","Clinopyroxene"),oc,
      paste(ig_note,"Intended for ultramafic rocks; the three values alone do not verify >90% whole-rock mafic minerals."),source=bgs),
    ol_px_hbl=make("Olivine - Pyroxene - Hornblende",c("Olivine","Pyroxene","Hornblende"),oh,
      paste(ig_note,"Pyroxene = orthopyroxene + clinopyroxene. Intended for ultramafic rocks."),source=bgs),
    pl_px_ol=make("Plagioclase - Pyroxene - Olivine",c("Plagioclase","Pyroxene","Olivine"),gab(FALSE),
      paste(ig_note,"Pyroxene = Opx + Cpx. These axes cannot distinguish gabbro, gabbronorite and norite; that requires separate Opx/Cpx information and appropriate plagioclase composition."),c(35,65),bgs),
    pl_px_hbl=make("Plagioclase - Pyroxene - Hornblende",c("Plagioclase","Pyroxene","Hornblende"),gab(TRUE),
      paste(ig_note,"These axes cannot distinguish gabbro, gabbronorite and norite. Hornblende must not be replaced by total amphibole without checking mineral identification."),c(35,65),bgs))
}

mts_modal_prepare <- function(type, values, label="") {
  defs <- mts_modal_definitions(); d <- defs[[type]]
  if(is.null(d)) stop("Choose a supported diagram.")
  if(length(values)!=3L || !is.numeric(values) || any(!is.finite(values)) || any(values<0)) stop("Enter three finite, non-negative values.")
  total <- sum(values); if(!is.finite(total) || total<=0) stop("The sum of the three components must be greater than zero.")
  x <- as.numeric(values)/total*100
  fields <- which(vapply(d$fields,function(f) all(vapply(f$rules,function(rule) rule(x)>=-1e-7,logical(1))),logical(1)))
  boundary <- length(fields)>1L
  if(type=="amphibolite" && length(fields)) boundary <- any(abs(vapply(d$fields[[1]]$rules,function(rule) rule(x),numeric(1)))<1e-7)
  field <- if(length(fields)) paste(vapply(d$fields[fields],`[[`,character(1),"name"),collapse=" / ") else "Outside the amphibolite modal field"
  list(type=type,definition=d,raw=setNames(as.numeric(values),d$components),percent=setNames(x,d$components),
    total=total,field=field,fields=fields,boundary=boundary,label=substr(as.character(label),1,160))
}

mts_plot_modal <- function(type, result=NULL) {
  d <- mts_modal_definitions()[[type]]; stopifnot(!is.null(d))
  old <- graphics::par(mar=c(2.5,1,3,1),xpd=NA); on.exit(graphics::par(old))
  annotated <- type!="amphibolite"
  legend_x <- if(annotated) 1.82 else 1.10
  graphics::plot.new(); graphics::plot.window(xlim=c(-.16,if(annotated) 2.62 else 1.78),ylim=c(-.15,1.03),asp=1)
  graphics::title(main=d$title,cex.main=1.12,col.main="#23465b")
  palette <- grDevices::hcl.colors(length(d$fields),"Pastel 1")
  for(i in seq_along(d$fields)) {
    p <- mts_ternary_xy(d$fields[[i]]$polygon)
    graphics::polygon(p,col=palette[i],border="#496578",lwd=1)
    middle <- colMeans(p)
    # Keep basal field numbers inside their polygons, using a smaller font.
    if(annotated && max(d$fields[[i]]$polygon[,1])<=10+1e-7) {
      graphics::text(middle[1],middle[2],labels=i,cex=.70,font=2,col="#203747")
    } else if(is.null(result) || sum((middle-mts_ternary_xy(result$percent))^2)>.003) {
      graphics::text(middle[1],middle[2],labels=i,cex=.85,font=2,col="#203747")
    }
  }
  triangle <- mts_ternary_xy(rbind(c(100,0,0),c(0,100,0),c(0,0,100),c(100,0,0)))
  graphics::lines(triangle,lwd=1.6,col="#203747")
  axis_names <- if(annotated) switch(type,ol_opx_cpx=c("Ol","Opx","Cpx"),
    ol_px_hbl=c("Ol","Px","Hbl"),pl_px_ol=c("Plag","Px","Ol"),pl_px_hbl=c("Plag","Px","Hbl")) else d$components
  graphics::text(.5,.93,axis_names[1],font=2,cex=.94)
  graphics::text(-.015,-.017,axis_names[2],font=2,cex=.82,adj=1)
  graphics::text(1.015,-.017,axis_names[3],font=2,cex=.82,adj=0)
  for(v in unique(c(seq(10,90,10),d$guides))) {
    p <- mts_ternary_xy(rbind(c(v,100-v,0),c(v,0,100-v)))
    graphics::text(p[1,1]-.03,p[1,2],v,cex=.57,col="#536976",adj=1)
    if(v %in% d$guides) graphics::lines(p,lty=2,col="#778a92")
  }
  if(annotated) {
    pl <- startsWith(type,"pl_")
    levels <- if(pl) c(90,65,35,10) else if(type=="ol_opx_cpx") c(90,40,10,0) else c(100,40,0)
    for(v in levels) {
      right <- mts_ternary_xy(c(v,0,100-v))
      graphics::segments(right[1]+.055,right[2],1.70,right[2],lty=2,col="#778a92")
    }
    ys <- if(pl) c(96,77.5,50,22.5,3) else if(type=="ol_opx_cpx") c(65,25,5) else c(70,20)
    labels <- if(pl) c("ANORTHOSITES","leuco-","meso-","mela-","ULTRAMAFIC ROCKS") else
      if(type=="ol_opx_cpx") c("pyroxene-\nperidotite","olivine-\npyroxenite","pyroxenite (s.s.)") else c("peridotite","pyroxenite and\nhornblendite")
    graphics::text(rep(1.37,length(ys)),sqrt(3)/2*ys/100,labels,cex=.77,col="#23465b")
  }
  graphics::text(legend_x,.94,"Diagram fields",adj=c(0,1),font=2,cex=.9,col="#23465b")
  y <- .855
  for(i in seq_along(d$fields)) {
    label <- paste(strwrap(paste0(i,". ",d$fields[[i]]$name),width=32),collapse="\n")
    graphics::text(legend_x,y,label,adj=c(0,1),cex=.73,col="#203747")
    y <- y-.031*(length(strsplit(label,"\n",fixed=TRUE)[[1]])+.4)
  }
  if(!is.null(result)) {
    xy <- mts_ternary_xy(result$percent)
    graphics::points(xy,pch=21,bg="#c4352b",col="white",cex=1.9,lwd=1.7)
    if(nzchar(result$label)) graphics::text(xy[1],xy[2]+.03,result$label,cex=.75,col="#912d26")
    graphics::mtext(paste(sprintf("%s = %.2f%%",names(result$percent),result$percent),collapse="; "),side=1,line=.25,cex=.7)
  }
  graphics::mtext(d$source,side=1,line=1.5,cex=.55,col="#536976")
}

mts_rock_diagrams_ui <- function(id="rock_diagrams") {
  ns <- shiny::NS(id); defs <- mts_modal_definitions()
  shiny::tabPanel("Rock classification",
    shiny::h4("Mafic, ultramafic and amphibolite diagrams"),
    shiny::selectInput(ns("type"),"Diagram",choices=stats::setNames(names(defs),vapply(defs,`[[`,character(1),"title"))),
    shiny::textOutput(ns("note")),shiny::uiOutput(ns("inputs")),
    shiny::textInput(ns("label"),"Sample label (optional)",value=""),
    shiny::tags$div(style="display:flex;gap:8px;flex-wrap:wrap;",
      shiny::actionButton(ns("plot"),"Plot sample",class="btn-primary"),
      shiny::actionButton(ns("clear"),"Clear point"),
      shiny::downloadButton(ns("png"),"Download PNG"),
      shiny::downloadButton(ns("pdf"),"Download PDF"),
      shiny::actionButton(ns("container"),"Add diagram to Image container",class="btn-info")),
    shiny::verbatimTextOutput(ns("summary")),shiny::plotOutput(ns("diagram"),height="650px"),
    shiny::tags$p(class="small-note","Values are entered manually, as in QAPF. Field boundaries are shown explicitly; a point on a shared boundary is reported with all adjacent fields. Nothing is written to SQLite by plotting or downloading."))
}

mts_rock_diagrams_server <- function(id, add_image, read_only=FALSE) {
  shiny::moduleServer(id,function(input,output,session) {
    state <- shiny::reactiveVal(NULL)
    defs <- mts_modal_definitions()
    output$inputs <- shiny::renderUI({
      d <- defs[[input$type]]; shiny::req(!is.null(d))
      shiny::fluidRow(lapply(1:3,function(i) shiny::column(4,shiny::numericInput(session$ns(paste0("v",i)),paste0(d$components[i]," (modal amount)"),value=0,min=0))))
    })
    output$note <- shiny::renderText({ shiny::req(input$type); defs[[input$type]]$note })
    shiny::observeEvent(input$type,state(NULL),ignoreInit=TRUE)
    shiny::observeEvent(input$clear,state(NULL),ignoreInit=TRUE)
    shiny::observeEvent(input$plot,{
      tryCatch(state(mts_modal_prepare(input$type,c(input$v1,input$v2,input$v3),input$label)),error=function(e) {
        state(NULL); shiny::showNotification(conditionMessage(e),type="error")
      })
    },ignoreInit=TRUE)
    output$summary <- shiny::renderPrint({
      r <- state()
      if(is.null(r)) { cat("Enter modal values and click Plot sample."); return() }
      cat(if(r$boundary) "Boundary: " else "Field: ",r$field,"\n",sep="")
      cat("Input total:",r$total,"; normalized total: 100%\n")
      cat(paste(sprintf("%s: %.3f%%",names(r$percent),r$percent),collapse=" | "),"\n")
      cat("The point and downloads reflect the last Plot sample action.")
    })
    output$diagram <- shiny::renderPlot({ shiny::req(input$type); mts_plot_modal(input$type,state()) },res=120)
    export <- function(file,pdf=FALSE) {
      r <- state(); shiny::req(!is.null(r))
      if(pdf) grDevices::pdf(file,width=12,height=8) else grDevices::png(file,width=2100,height=1400,res=180)
      on.exit(grDevices::dev.off()); mts_plot_modal(r$type,r)
    }
    output$png <- shiny::downloadHandler(filename=function() paste0("rock_diagram_",input$type,".png"),content=function(file) export(file))
    output$pdf <- shiny::downloadHandler(filename=function() paste0("rock_diagram_",input$type,".pdf"),content=function(file) export(file,TRUE))
    shiny::observeEvent(input$container,{
      shiny::req(!is.null(state()))
      if(read_only) { shiny::showNotification("Image container changes are disabled in LAN read-only mode. Use Download PNG or PDF.",type="warning"); return() }
      tryCatch(add_image(state(),export),error=function(e) shiny::showNotification(conditionMessage(e),type="error"))
    },ignoreInit=TRUE)
    state
  })
}
