server <- function(input, output){
  source("RankDIMs.R")
  load("clean_Data.RData")
  output$plot.1 <- renderPlot({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[7]
    })
  })
  output$plot.2 <- renderPlot({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[8]
    })
  })
  output$plot.3 <- renderPlot({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[9]
    })
  })
  output$ratio_importance <- renderTable({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[10]
    })
  })
  output$df.hh <- renderTable({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[1]
    })
  })
  output$df.neighbour <- renderTable({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[2]
    })
  })
  output$df.region <- renderTable({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[3]
    })
  })
  output$tbl_hh <- renderTable({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[4]
    })
  })
  output$tbl_neighbour <- renderTable({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[5]
    })
  })
  output$tbl_region <- renderTable({
    input$button
    isolate({
      RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[6]
    })
  })
  output$result.A <- renderText({
    input$button
    isolate({
      unlist(RankDIMs(input$A, input$t, input$which_group, c(input$wts1,input$wts2),input$num)[11])
    })
  })
  
}