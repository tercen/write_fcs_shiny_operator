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
  
  output$summary <- renderText(
    paste(paste("Number of rows:", nrow(raw_data)),
          paste("Number of cols:", ncol(raw_data)),
          paste("Column names:",  paste(col_names, collapse = ",")), sep="\n")
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

ctx       <- getCtx(session)
raw_data  <- ctx$as.matrix()
col_names <- ctx$cnames %>% unlist()

getData <- function(session){
  channels      <- ctx$rselect() %>% pull()
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

runApp(shinyApp(ui, server))  
