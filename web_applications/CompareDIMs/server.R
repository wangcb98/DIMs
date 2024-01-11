server <- function(input, output){
  source("CompareDIMs.R")
  load("clean_Data.RData")
  output$plot.DI <- renderPlot({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[1]
    })
  })
  output$df.plot.DI <- renderTable({
    input$button
    isolate(
      {if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
        if(input$space_A=="Neighbourhood"){
          DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
        }
        if(input$space_A=="Region"){
          DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
        }
        if(input$space_B=="Household"){
          DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
        }
        if(input$space_B=="Neighbourhood"){
          DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
        }
        if(input$space_B=="Region"){
          DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
        }
        idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
        labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
        DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
        CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[2]
      })
  })
  output$anova.result.DI <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[3]
    })
    
  })
  output$welch.anova.result.DI <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[4]
    })
  })
  output$tb.t.test.DI <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[5]
    })
  })
  output$pwr.DI <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[6]
    })
  })
  output$pwr.inv.DI <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[7]
    })
  })
  output$plot1 <- renderPlot({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[8]
    })
  })
  output$plot2 <- renderPlot({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[9]
    })
  })
  output$plot3 <- renderPlot({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[10]
    })
  })
  output$plot4 <- renderPlot({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[11]
    })
  })
  output$df.plot <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[12]
    })
  })
  output$tb.t.test <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[13]
    })
  })
  output$tb.pwr <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[14]
    })
  })
  output$tb.t.test.temporal <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[15]
    })
  })
  output$tb.pwr.temporal <- renderTable({
    input$button
    isolate({
      if(input$space_A=="Household"){
        DIM_A<-id_DIMs_hh[which(labels_DIMs_hh==input$A)]
      }
      if(input$space_A=="Neighbourhood"){
        DIM_A<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$A)]
      }
      if(input$space_A=="Region"){
        DIM_A<-id_DIMs_region[which(labels_DIMs_region==input$A)]
      }
      if(input$space_B=="Household"){
        DIM_B<-id_DIMs_hh[which(labels_DIMs_hh==input$B)]
      }
      if(input$space_B=="Neighbourhood"){
        DIM_B<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$B)]
      }
      if(input$space_B=="Region"){
        DIM_B<-id_DIMs_region[which(labels_DIMs_region==input$B)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      DIM_DI<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI)]
      CompareDIMs(DIM_DI,input$which_group,input$t,DIM_A,DIM_B,input$how_jitter)[16]
    })
  })
}