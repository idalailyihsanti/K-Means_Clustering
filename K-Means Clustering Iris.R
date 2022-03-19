library(shiny)
library(cluster)
name_UI <- function(id) {
  vars <- setdiff(names(iris),"Species")
  
  pageWithSidebar (
    headerPanel('Iris k-means clustering'),
    sidebarPanel(
      selectInput('xcol','X variabel', vars),
      selectInput('ycol','Y variabel', vars, selected = vars[[2]]),
      numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
}

name <- function(input, output, session) {
 selectedData <- reactive({
   iris[, c(input$xcol, input$xcol)]
 }) 
 clusters <- reactive({
   kmeans (selectedData(), input$clusters)
 })
 output$plot1 <- renderPlot({
   palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
             "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
   
   par(mar = c(5.1, 4.1, 0, 1))
   plot(selectedData(),
        col = clusters()$cluster,
        pch = 20, cex = 3)
   points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
 })
}
shinyApp(name_UI, name)










