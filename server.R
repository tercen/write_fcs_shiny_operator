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
  res           <- t(ctx$as.matrix())
  colnames(res) <- channels
  flow_frame    <- flowCore::flowFrame(res)
  
  return(flow_frame)
}
