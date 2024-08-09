library(shiny)
library(ggplot2)
library(igraph)
library(shinyWidgets)
library(dplyr)
library(summaryBox)
library(sf)
library(rnaturalearth)
library(shinyjs)
library(DT)
library(visNetwork)

rm(list = ls())

source("MainData.R")
source("TabPanel2.R")

theme <- bslib::bs_theme(version = 4)

# set default graph
alliance <- getAllianceData("SA", routes, airports)
fullgraph <- fullDataGraph(alliance)
graph <- topGraph(fullgraph)
topV <- V(graph$g)$name

# for refresh button
if(interactive()) {

# set UI
ui <- fluidPage(
  useShinyjs(),

  # define spaces between options in checkboxgroup
  tags$style(type = 'text/css',
  ".checkbox, .radio {margin-top: 10px;}"
  ),

  # main background settings
  theme = theme,
  setBackgroundColor(color = "ghostwhite"),

  # set navbarPage - switching between two pages of application
  navbarPage("", id = "navbar",

             # AIRPORTS PAGE
             tabPanel("Airports",
                      fluidRow(
                        column(width = 10, titlePanel(h1("Basic network segmentation by Airline´s alliances", align = "center"))),
                        column(width = 2, actionBttn("refresh", label = "Refresh", style = "float", color = "danger", size = "md"))
                        ),
                      fluidRow(
                        column(width = 10, div(h4("Allows you to work with 100 airports with highest Degree Centrality.")),
                               align = "center")
                        ),
                      # alliance options + summaryboxes
                      fluidRow(
                        column(width = 2, style = "width:100vh; margin:3vh;",
                               selectInput("alliance", label = "Select alliance",
                                           c("Star Alliance" = "SA", "SkyTeam" = "ST", "Oneworld" = "OW"))),
                        column(width = 9, style = "width:105h; margin:3vh;", uiOutput("summarybox"))
                        ),
                      # mainLayout
                      sidebarLayout(
                        sidebarPanel(style = "height: 80vh; overflow-y: auto;", class = "sidebar", width = 2,
                                     div(style ="font-size:80%;", div(style = "margin-bottom:15px; margin-top:15px;",
                                                                      actionBttn(inputId = "Uncheck", label = "Uncheck all",
                                                                                 style = "float", color = "danger", size = "xs")),
                                         div(checkboxGroupInput("arpt", "Choose airports to delete:", topV)))
                                     ),
                        mainPanel(width = 10,
                                  fluidRow(
                                    #Table with TopAirports
                                    column(width = 2,
                                           div(DTOutput("table"),
                                               style ="font-size:80%; height: 80vh; position: relative; overflow-x: auto;")),
                                    #Main graph
                                    column(width = 10,
                                           h4("Brush and double-click to zoom"),
                                           p("In the map you can always see 100 Top Airports with highest Degree Centrality
                                             (without airports deleted by option in left sidebar).
                                             So if you delete all vertices you will see airports with position 101-200."),
                                           div(plotOutput("map", dblclick = "map_dblclick",
                                                          brush = brushOpts("map_brush", resetOnNew = TRUE)),
                                               style ="position: relative;"),
                                           #Airport name check option
                                           h4("Check Airport name by writting IATA code"),
                                           fluidRow(column(width = 5, textInput("airportCode", label = " ", value = "",
                                                                                width = NULL, placeholder = NULL)
                                                           ),
                                                    column(width = 5, div(h4(textOutput("airportName"))))
                                                    )
                                           )
                                    )
                                  )
                        )
                      ),
             # AIRLINES PAGE
             tabPanel("Airlines",
                      fluidPage(
                        div(actionBttn("refresh2", label = "Refresh", style = "float", color = "danger", size = "md"),
                            align = "right"),
                        fluidRow(
                          column(width = 2, style = "width:100vh; margin:3vh;",
                                 selectInput("alliance2", label = "Select alliance",
                                             c("Star Alliance" = "SA", "SkyTeam" = "ST", "Oneworld" = "OW"))),
                          column(width = 9, style = "width:105h; margin:3vh;", uiOutput("airlineSummary"))
                          ),
                        # MainPlot
                        fluidRow(
                          column(width = 2, uiOutput("airlineBox")),
                          column(width = 10, uiOutput("degreeCentralitySlider"),
                                 visNetworkOutput("plot"),
                                 # Airport/Airline name check option
                                 fluidRow(
                                   column(width = 4,
                                          textInput("tab2AirportCodeIn", label = "Check Airport name by writting IATA code", value = ""),
                                          div(textOutput("tab2AirportNameOut"))
                                          ),
                                   column(width = 4,
                                          textInput("tab2AirlineCodeIn", label = "Check Airline name by writting IATA code", value = ""),
                                          div(textOutput("tab2AirlineNameOut"))
                                          ),
                                   column(width = 4,
                                          textInput("tab2AirlineNameIn", label = "Check Airline IATA code by writting name", value = ""),
                                          div(textOutput("tab2AirlineCodeOut"))
                                 )
                          )
                                 )
                          )
                        )
                      )
             )
  )


# set server side
server <- function(input, output, session) {
  # whole page refresh button
  observeEvent(input$refresh, refresh())
  observeEvent(input$refresh2, reset())

  # AIRPORTS PAGE
  #define current latitude and longitude for graph - default setting for full world map
  ranges <- reactiveValues(x = c(-180,180), y = c(-90,90))
  
  observe({
    # choose airlines according to alliance input
    alliance <- getAllianceData(input$alliance, routes, airports)
    airlines <- allianceData[[input$alliance]]

    # graph for alliance
    fullgraph <- fullDataGraph(alliance)
    graph <- topGraph(fullgraph)
    topV <- V(graph$g)$name

    # values for alliance summary
    vertices <- gorder(fullgraph)
    edges <- gsize(fullgraph)
    numberOfAirlines <- length(airlines)
    r <- reactiveValues(vertices = vertices, edges = edges)

    # load airports data to checkbox
    updateCheckboxGroupInput(session, "arpt", "Choose airports to delete:", topV)

    # airports checkbox update
    observeEvent(input$Uncheck, {
      updateCheckboxGroupInput(session, "arpt", "Choose airports to delete:", topV)
    })

    # summaryboxes
    output$summarybox <- renderUI({
      fluidRow(width = 12,
               summaryBox2("Vertices", r$vertices, width = 3, style = "info"),
               summaryBox2("Edges", r$edges, width = 3, style = "info"),
               summaryBox2("Airlines", numberOfAirlines, width = 3, style = "info")
               )
    })

    # create a reactive expression to generate a reduced graph by removing the selected airports from the fullgraph
    reducedGraph <- reactive({
      if(!is.null(input$arpt)){ #if there is some option checked
        res <- delete_vertices(fullgraph, input$arpt)
        r$vertices <- gorder(res)
        r$edges <- gsize(res)
        res <- topGraph(res)
      } else{
        r$vertices <- gorder(fullgraph)
        r$edges <- gsize(fullgraph)
        res <- graph
      }
      return(res)
    })

    # table with top 100 degree centrality vertices
    output$table <- renderDataTable({
      datatable(reducedGraph()$df, class = "compact, hover", rownames = FALSE, options = list(pageLength = nrow(df), paging = FALSE))
    })

    # plot the map
    output$map <- renderPlot({
      graph <- reducedGraph()$g
      return(graphForPlot(graph, airports, ranges))
    })

    # set the region of the map
    observeEvent(input$map_dblclick, {
      brush <- input$map_brush
      if(!is.null(brush) && abs(brush$xmax-brush$xmin) > 0.1 && abs(brush$ymax-brush$ymin) > 0.1){
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else{
        ranges$x <- c(-180,180)
        ranges$y <- c(-90,90)
      }
    })

    # print the name of the airport
    output$airportName <- renderText({
      req(input$airportCode)
      airport <- airports %>% filter(IATA == toupper(trimws(input$airportCode)))
      if(nrow(airport) == 0) "Airport not found" else airport$Name
    })
  })

  # AIRLINES PAGE
  
  # define reactive values
  numberOfAirlines <- reactiveVal(NULL)
  fullgraph <- reactiveValues(g = NULL, gorder = NULL, gsize = NULL, deg = NULL, mindeg = NULL, maxdeg = NULL)
  currgraph <- reactiveValues(g = NULL, gorder = NULL, gsize = NULL, deg = NULL, mindeg = NULL, maxdeg = NULL)

  # observe the selection of an airline alliance and update the corresponding data and UI elements
  observeEvent(input$alliance2, {
    alliance <- getAllianceData(input$alliance2, routes, airports)
    airlines <- allianceData[[input$alliance2]]
    numberOfAirlines(length(airlines))
    
    g <- fullDataGraph(alliance)
    deg <- degree(g, v = V(g),
                  mode = "all",
                  loops = FALSE,
                  normalized = FALSE)
    currgraph$g <- fullgraph$g <- g
    currgraph$gorder <- fullgraph$gorder <- gorder(g)
    currgraph$size <- fullgraph$gsize <- gsize(g)
    currgraph$deg <- fullgraph$deg <- deg
    currgraph$mindeg <- fullgraph$mindeg <- min(deg)
    currgraph$maxdeg <- fullgraph$maxdeg <- max(deg)

    # render UI for selecting airlines in the left menu
    output$airlineBox <- renderUI({
      fluidPage(
        div(style = "font-size:80%;",
            div(style = "margin-bottom:15px; margin-top:15px;",
                actionBttn(inputId = "Uncheck2", label = "Uncheck all", style = "float", color = "danger", size = "xs")),
            checkboxGroupInput("airlinesInput", "Choose airline: ", choices = airlines, selected = airlines))
        )
    })
  })

  # airlines checkbox update
  observeEvent(input$Uncheck2, {
    updateCheckboxGroupInput(session, "airlinesInput", "Choose airline:", choices = allianceData[[input$alliance2]], selected = NULL)
  })

  # observe the selection of an airline alliance and update currgraph
  observeEvent(input$airlinesInput, {
    isolate({
      if(length(input$airlinesInput) == numberOfAirlines()){
        currgraph$g <- fullgraph$g
        currgraph$gorder <- fullgraph$gorder
        currgraph$size <- fullgraph$gsize
        currgraph$deg <- fullgraph$deg
        currgraph$mindeg <- fullgraph$mindeg
        currgraph$maxdeg <- fullgraph$maxdeg
      } else{
        g <- CurrentAirlinesGraph(fullgraph$g, input$airlinesInput)
        deg <- degree(g, v = V(g),
                      mode = "all",
                      loops = FALSE,
                      normalized = FALSE)
        currgraph$g <- g
        currgraph$gorder <- gorder(g)
        currgraph$size <- gsize(g)
        if(currgraph$gorder == 0){
          currgraph$deg <- NULL
          currgraph$mindeg <- NULL
          currgraph$maxdeg <- NULL
        } else{
          currgraph$deg <- deg
          currgraph$mindeg <- min(deg)
          currgraph$maxdeg <- max(deg)
        }
      }
    })

    # render the slider for selecting degree centrality range
    output$degreeCentralitySlider <- renderUI({
      a <- currgraph$mindeg
      b <- currgraph$maxdeg
      if (is.null(a) || is.null(b)){
        a <- fullgraph$mindeg
        b <- fullgraph$maxdeg
      }
      sliderInput("slider", label = "Degree centrality", value = c(a, b), min = a, max = b, step = 1)
    })

    # render the network plot with dynamic adjustments based on selected airlines and degree centrality
    output$plot <- renderVisNetwork({
      showNotification(paste("In checker", isolate(currgraph$gorder), collapse = ', '))
      isolate({
        visIgraph(currgraph$g, layout = "layout.kamada.kawai") %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% visInteraction(navigationButtons = TRUE)
      })
    })
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # observe the slider
  observeEvent(input$slider, {
    changed <- !(input$slider[1] == isolate(currgraph$mindeg) && input$slider[2] == isolate(currgraph$maxdeg))
    if(changed && isolate(currgraph$gorder > 0)){
      # render the network plot with dynamic adjustments based on selected airlines and degree centrality
      output$plot <- renderVisNetwork({
        showNotification(paste("In slider", isolate(currgraph$gorder), collapse = ', '))
        g <- isolate(currgraph$g)
        d <- isolate(currgraph$deg)
        h <- delete_vertices(g, d < input$slider[1] | d > input$slider[2])
        tryCatch({
          visIgraph(h, layout = "layout.kamada.kawai") %>%
            visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>% visInteraction(navigationButtons = TRUE)
        }, error = function(e) {})
      })
    }
  })

  # summaryboxes
  output$airlineSummary <- renderUI({
    fluidRow(width = 12,
             summaryBox2("Vertices", currgraph$gorder, width = 3, style = "info"),
             summaryBox2("Edges", currgraph$size, width = 3, style = "info"),
             summaryBox2("Airlines", numberOfAirlines(), width = 3, style = "info")
             )
  })

  # print the name of the airport
  output$tab2AirportNameOut <- renderText({
    req(input$tab2AirportCodeIn)
    airport <- airports %>% filter(IATA == toupper(trimws(input$tab2AirportCodeIn)))
    if(nrow(airport) == 0) "Airport not found" else airport$Name
  })

  # print the name of the airline
  output$tab2AirlineNameOut <- renderText({
    req(input$tab2AirlineCodeIn)
    airline <- airlinesData %>% filter(IATA == toupper(trimws(input$tab2AirlineCodeIn)))
    if(nrow(airline) == 0) "Airline not found" else airline$Name
  })

  # print the IATA code of the airline
  output$tab2AirlineCodeOut <- renderText({
    req(input$tab2AirlineNameIn)
    airline <- airlinesData %>% filter(grepl(trimws(input$tab2AirlineNameIn), Name, ignore.case = TRUE))
    if(nrow(airline) == 0) "Airline not found" else paste(paste(airline$IATA, " (", airline$Name, ")", sep = ""), collapse = ", ")
  })
}

shinyApp(ui, server)
}