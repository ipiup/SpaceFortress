library(shiny)
library(shinyFiles)
source("E:\\SpaceFortress\\Clean\\Clean_SF.R")

fun_clean<-function(path,path_clean){
  fil=list.files(path=path,recursive = T)
  print(fil)
  lapply(fil,write_file,path=path,path_clean=path_clean)
}

fun_read_clean<-function(path_clean){
  fil_clean=list.files(path=path_clean,recursive = T) #load the clean files
  data=read_final_Score(fil_clean,path_clean,detailed = FALSE) #Create the data 
  df_GROUPS=read.table("E:\\ISAE-2021\\Alldata\\GROUPS.txt",header=TRUE)
  for(str_pseudo in unique(data$Pseudo)){
    data$Treatment[data$Pseudo==str_pseudo]=as.numeric(df_GROUPS$Treatment[df_GROUPS$Pseudo==str_pseudo])
  }
  data$Pseudo[data$Pseudo=="SL2804"]="SL0804"
  return(data) 
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
    actionButton("writeFile","Write Clean Files"),
    actionButton("readClean","Read Clean Files")
  ),
  mainPanel(
    DT::dataTableOutput('tabledata')
    # tabsetPanel(
    #   tabPanel("Plot", plotOutput("plot")), 
    #   tabPanel("Summary", verbatimTextOutput("summary")), 
    #   tabPanel("Table", tableOutput("table"))
    # )
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

  #reac<-eventReactive(input$writeFile,{fun_clean(global$datapath,global_cl$datapath)})
  reac<-eventReactive(input$readClean,{fun_read_clean(global_cl$datapath)})

  #reac<-eventReactive(input$readClean,{print(global$datapath)})
  observe(reac())
  dt<-reactive({
    fun_read_clean(global_cl$datapath)
  })
  output$tabledata<-DT::renderDataTable({
    dt()
    })

}

# Run the application
shinyApp(ui = ui, server = server)