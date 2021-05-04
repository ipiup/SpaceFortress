library(shiny)
library(shinyFiles)

fun_clean<-function(path,path_clean){
  fil=list.files(path=path,recursive = T)
  print(fil)
  lapply(fil,write_file,path=path,path_clean=path_clean)
}

ui <- fluidPage(
  titlePanel("Space Fortress Data Extraction"),
  
  sidebarLayout(
    sidebarPanel(
    #Raw File directory
    shinyDirButton("dir_raw", "Raw File directory", "Upload"),
    verbatimTextOutput("dir_raw", placeholder = TRUE),
    #Clean File directory
    shinyDirButton("dir_clean", "Clean File directory", "Upload"),
    verbatimTextOutput("dir_clean", placeholder = TRUE),
    # textInput("filename","Choose a file name"),
    # actionButton("namego","go"),
    # textOutput("b"),
    actionButton("filego","write")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")), 
      tabPanel("Summary", verbatimTextOutput("summary")), 
      tabPanel("Table", tableOutput("table"))
    )
  )
  ))

server <- function(input, output) {
  shinyDirChoose(input,'dir_raw',roots = c(files = "E:"),filetypes = c('', 'txt'))
  shinyDirChoose(input,'dir_clean',roots = c(files = 'E:'),filetypes = c('', 'txt'))
  
  global <- reactiveValues(datapath = "")#getwd())
  dir_raw <- reactive(input$dir_raw)
  output$dir_raw <- renderText({
    global$datapath
  })
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$dir_raw},
    handlerExpr = {
      if (!"path" %in% names(dir_raw())) return()
      global$datapath <-
        normalizePath(file.path("E:\\", paste(unlist(dir_raw()$path[-1]), collapse = .Platform$file.sep)))
      
    }
  )
  
  global_cl<-reactiveValues(datapath = "")#getwd())
  dir_clean <- reactive(input$dir_clean)
  output$dir_clean <- renderText({
    global_cl$datapath
  })

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$dir_clean},
    handlerExpr = {
      if (!"path" %in% names(dir_clean())) return()
      global_cl$datapath <-
        normalizePath(file.path("E:\\", paste0(unlist(dir_clean()$path[-1]), collapse = .Platform$file.sep)))
    }
  )

  reac<-eventReactive(input$filego,{fun_clean(global$datapath,global_cl$datapath)})
  #reac<-eventReactive(input$filego,{print(global_cl$datapath)})
  observe(reac())
}

# Run the application
shinyApp(ui = ui, server = server)