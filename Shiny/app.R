library(shiny)
library(shinyFiles)


d=read.table("C:\\Users\\Chamery\\Documents\\Shiny_SF testshinyfile.txt")
n="cc.txt"

fun_test(d,"C:\\Users\\Chamery\\Documents\\test.txt")
fun_test<-function(f,pth){
  write.table(f,pth)
}

ui <- fluidPage( # Application title
  mainPanel(
    shinyDirButton("dir", "Input directory", "Upload"),
    verbatimTextOutput("dir", placeholder = TRUE),
    textInput("filename","Choose a file name"),
    actionButton("namego","go"),
    textOutput("b"),
    actionButton("filego","write")
  ))

server <- function(input, output) {
  shinyDirChoose(input,'dir',roots = c(wd = 'C:'),filetypes = c('', 'txt'))
  
  global <- reactiveValues(datapath = "")#getwd())
  
  dir <- reactive(input$dir)
  
  filename<-reactive(input$filename)
  
  output$dir <- renderText({
    global$datapath
  })
  output$filename<-renderText(input$filename)
  
  output$b<-renderText(inpu$dir)
  observeEvent(
    ignoreNULL = TRUE,
               eventExpr = {input$dir},
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   normalizePath(file.path(home))#,paste(unlist(dir()$path[-1])), collapse = .Platform$file.sep))
               }
    )

  
  # output$txt_out<-renderText({global$datapath
  #   #write.table("blablabla",paste(global$datapath,"testshinyfile.txt"))
  #   })
  #reac<-eventReactive(input$namego,{input$filename})
  #output$b<-renderText({reac()})
  #observe(print(paste0(global$datapath,"\\",input$filename,".txt")))
  #observe(fun_test(d,global$datapath))
  #reac<-eventReactive(input$namego,{print(paste0(global$datapath,"\\",input$filename,".txt"))})
  reac_n<-eventReactive(input$namego,{paste0(global$datapath,"\\",input$filename,".txt")})
  output$b<-renderText({reac_n()})
  reac<-eventReactive(input$filego,{fun_test(d,reac_n())})
  observe(reac())
}

# Run the application
shinyApp(ui = ui, server = server)