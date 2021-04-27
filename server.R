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

  dataInput <- reactive({
    getData(session)
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
  
  output$summary <- renderPrint(
    str(dataInput())
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "export.FCS"
    },
    content = function(con) {
      write.FCS(dataInput(), con)
    }
  )
})
  
getData <- function(session){
  ctx           <- getCtx(session)
  channels      <- ctx$rselect() %>% pull()
  cnames        <- ctx$cnames %>% unlist()
  # some columns might be of character type, but the input for flowFrame should be a numeric matrix
  columns       <- ctx$cselect() %>% 
    mutate_if(is.character, as.factor) %>% 
    mutate_if(is.factor, as.numeric) %>% 
    replace(is.na(.), 0)
  res           <- as.matrix(cbind(t(ctx$as.matrix()), columns)) 
  colnames(res) <- c(channels, cnames)
  flow_frame    <- flowCore::flowFrame(res)
  
  return(flow_frame)
}
