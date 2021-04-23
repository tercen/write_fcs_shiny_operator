library(shiny)
library(tercen)
library(dplyr)
library(flowCore)


############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  options("tercen.workflowId"= "4133245f38c1411c543ef25ea3020c41")
  options("tercen.stepId"= "2b6d9fbf-25e4-4302-94eb-b9562a066aa5")
  options("tercen.username"= "admin")
  options("tercen.password"= "admin")
  ctx <- tercenCtx()
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  uiOutput("reacOut"),
  title = "Export FCS"
))

server <- shinyServer(function(input, output, session) {
  
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

runApp(shinyApp(ui, server))  
