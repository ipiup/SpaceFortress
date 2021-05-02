library(shiny)
library(shinyFiles)


fun_test<-function(x){
  return(paste(x,"is your input"))
  
}

ui <- fluidPage( # Application title
  mainPanel(
    shinyDirButton("dir", "Input directory", "Upload"),
    verbatimTextOutput("dir", placeholder = TRUE),
    textOutput("txt_out")
  ))

server <- function(input, output) {
  shinyDirChoose(
    input,
    'dir',
    roots = c(wd = 'C:'),
    filetypes = c('', 'txt')
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir)
  
  output$dir <- renderText({
    global$datapath
  })
  
  observeEvent(
    ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <-("~")
                 global$datapath <-
                   normalizePath(file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep)))
               }
    )
  
  output$txt_out<-renderText({global$datapath
    #write.table("blablabla",paste(global$datapath,"testshinyfile.txt"))
    })
  observe(print(global$datapath))
  
}

# Run the application
shinyApp(ui = ui, server = server)