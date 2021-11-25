library(shiny)
library(shinyFiles)
library(shinyWidgets)
source("E:\\SpaceFortress\\Clean\\Clean_SF.R")

fun_clean<-function(path,path_clean){
  fil=list.files(path=path,recursive = T)
  print(fil)
  lapply(fil,write_file,path=path,path_clean=path_clean)
}

fun_read_clean<-function(path_clean,wide=TRUE){
  fil_clean=list.files(path=path_clean,recursive = T) #load the clean files
  data=read_final_Score(fil_clean,path_clean,detailed = FALSE) #Create the data 
  df_GROUPS=read.table("E:\\ISAE-2021\\Alldata\\GROUPS.txt",header=TRUE)
  for(str_pseudo in unique(data$Pseudo)){
    data$Treatment[data$Pseudo==str_pseudo]=as.numeric(df_GROUPS$Treatment[df_GROUPS$Pseudo==str_pseudo])
  }
  data$Pseudo[data$Pseudo=="SL2804"]="SL0804"
  #DEMOGRAPHICS
  df_demographique=read.csv("E:\\SpaceFortress\\Data\\demographicsbrut.csv",head=TRUE,dec = ",",sep=";")
  
  names(df_demographique)[names(df_demographique)=="Votre.âge"]="Age"
  data_long=demographie_long(data,df_demographique)#LONG FORMAT of the data with dem info
  data_wide=demographie(data,df_demographique,ZMean=FALSE,Delta=TRUE) #WIDE FORMAT of the data with dem info

  #OUTLIERS : LM2411 & EC1603 & TB0301
  data_long=subset(data_long,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301")#outliers on long format
  data_wide=subset(data_wide,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301")#outliers on wide format
  data_long=data_long[colnames(data_long)[colnames(data_long)!="Treatment"]]
  
  if(wide){
    return(data_wide)
  }else{
    return(data_long)
  }
}

ui <- fluidPage(
  titlePanel("Space Fortress Data Extraction"),
  
    sidebarPanel(
      
      tabsetPanel(
        tabPanel("Raw Files", shinyDirButton("dir_raw", "Raw File directory", "Upload"),
                 verbatimTextOutput("dir_raw", placeholder = TRUE),
                 actionButton("writeFile","Write Clean Files")),
        tabPanel("Clean Files",shinyDirButton("dir_clean", "Clean File directory", "Upload"),
                 verbatimTextOutput("dir_clean", placeholder = TRUE),
                 radioButtons("wideOrLong","Data Format",choices=c("Wide","Long")),
                 checkboxGroupInput("parameters","Choose features"),
                 actionButton("displayTab","Show Data")),
        tabPanel("Data Parameters",
                 
                 checkboxGroupInput("columns", "Choose columns"))),
      
  ),
  mainPanel(
    DT::dataTableOutput('tabledata')
  )
  )

server <- function(input, output,session) {

  shinyDirChoose(input,'dir_raw',roots = c(files = "E:"),filetypes = c('', 'txt'))
  shinyDirChoose(input,'dir_clean',roots = c(files = 'E:'),filetypes = c('', 'txt'))
  #Raw File path
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
  #Cleaned Data Path
  global_cl<-reactiveValues(datapath = "E:\\SpaceFortress\\TESTSHINY\\Clean")#getwd())
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

  #Clean files into dataframe
  data<-reactive({
    if(input$wideOrLong=="Wide"){
      fun_read_clean(global_cl$datapath,TRUE)
    }else{
      fun_read_clean(global_cl$datapath,FALSE)
    }
  })
  
  #Columns selection
  react_columns<-eventReactive(input$wideOrLong,{
    dat<-data()
    col = names(dat)
    updateCheckboxGroupInput(session,"columns",choices=col,selected=col)
    if(input$wideOrLong=="Wide"){
      updateCheckboxGroupInput(session,"parameters",choices=c("P1","P2","Delta TotalScore","Delta Sub-Scores","Blinded"),selected=c("P1","P2"))
    }else{
      updateCheckboxGroupInput(session,"parameters",choices=c("Sub-Scores","Blinded"))
    }

  })
  observe(react_columns())

  react_parameters<-reactive({
    dat<-data()
    col=names(dat)
    updateCheckboxGroupInput(session,"columns",choices=col,selected=col)
      if(!"Sub-Scores"%in%input$parameters){
        col=col[col!="Flight"&col!="Bonus"&col!="Mine"&col!="Fortress"]
        updateCheckboxGroupInput(session,"columns",choices=col,selected=col)
      }
      if("Blinded"%in%input$parameters){
        col=col[col!="Group"]
        updateCheckboxGroupInput(session,"columns",choices=col,selected=col)
      }
      if(!"Delta Sub-Scores"%in%input$parameters){
        col=col[col!="DeltaD1D14Flight"&col!="DeltaD14D5Flight"&col!="DeltaD1D14Bonus"&col!="DeltaD14D5Bonus"&col!="DeltaD1D14Mine"&col!="DeltaD14D5Mine"&col!="DeltaD1D14Fortress"&col!="DeltaD14D5Fortress"&col!="DeltaD5D1Fortress"]
        updateCheckboxGroupInput(session,"columns",choices=col,selected=col)
      }
      if(!"Delta TotalScore"%in%input$parameters){
        col=col[col!="DeltaD1D14"&col!="DeltaD14D5"&col!="DeltaD1D5"]
        updateCheckboxGroupInput(session,"columns",choices=col,selected=col)
      }
      if(!"P1"%in%input$parameters){
        col=col[col!="D01P1"&col!="D02P1"&col!="D03P1"&col!="D04P1"&col!="D05P1"&col!="D14P1"]
        updateCheckboxGroupInput(session,"columns",choices=col,selected=col)
      }
      if(!"P2"%in%input$parameters){
        col=col[col!="D02P2"&col!="D03P2"&col!="D04P2"&col!="D05P2"&col!="D14P2"]
        updateCheckboxGroupInput(session,"columns",choices=col,selected=col)
      }
  })
  observe(react_parameters())
  #Parameters for columns display 
  data_subset<-reactive({
    data()%>%
      select(input$columns)
  })
  
  reac_tab<-eventReactive(input$displayTab,{ 
    output$tabledata<-DT::renderDataTable(data_subset(),rownames=FALSE,filter = 'top',
                                          options = list(pageLength = 25, autoWidth = TRUE,dom = 'ltipr'))
  })
  observe(reac_tab())

  
}

# Run the application
shinyApp(ui = ui, server = server)
