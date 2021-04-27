library(shiny)
library(tercen)
library(dplyr)
library(flowCore)

############################################
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {

  rawData <- reactive({
    getRawData(session)
  })
  
  dataInput <- reactive({
    getData(session, rawData())
  })
  
  output$reacOut <- renderUI({
    tagList(
      HTML("<h3><center>Export FCS</center></h3>"),
      fluidRow(
        column(1),
        column(5, verbatimTextOutput("summary")),
        column(2, shiny::downloadButton("downloadData", "Export FCS file")))
    )
  })
  
  output$summary <- renderText({
    ctx       <- getCtx(session)
    raw_data  <- rawData()
    col_names <- ctx$cnames %>% unlist()
    paste(paste("Number of rows:", nrow(raw_data)),
          paste("Number of cols:", ncol(raw_data)),
          paste("Column names:",  paste(col_names, collapse = ",")), sep="\n")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "export.FCS"
    },
    content = function(con) {
      write.FCS(dataInput(), con)
    }
  )
})

getRawData <- function(session) {
  getCtx(session)$as.matrix()  
}

getData <- function(session, raw_data){
  ctx           <- getCtx(session)
  channels      <- ctx$rselect() %>% pull()
  col_names     <- ctx$cnames %>% unlist()
  # some columns might be of character type, but the input for flowFrame should be a numeric matrix
  columns       <- ctx$cselect() %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, as.numeric) %>% 
    replace(is.na(.), 0)
  res           <- as.matrix(cbind(t(raw_data), columns)) 
  colnames(res) <- c(channels, col_names)
  flow_frame    <- flowCore::flowFrame(res)
  
  return(flow_frame)
}
