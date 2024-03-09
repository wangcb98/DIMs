server <- function(input, output){
  source("CompareDIMs.R")
  load("clean_Data.RData")
  output$Result.s.welch.anova <- renderTable({
    #DI_s, t_s, which_group_s, DIM_t, space_t, which_group_t, DI_g, space_g, t_g
    input$button
    isolate({
      if(input$space_t=="Household"){
        DIM_t<-id_DIMs_hh[which(labels_DIMs_hh==input$DI_t)]
      }
      if(input$space_t=="Neighbourhood"){
        DIM_t<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$DI_t)]
      }
      if(input$space_t=="Region"){
        DIM_t<-id_DIMs_region[which(labels_DIMs_region==input$DI_t)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      idx_DI_s<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI_s)]
      idx_DI_g<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI_g)]
      CompareDIMs(idx_DI_s,input$t_s,input$which_group_s,DIM_t,input$space_t,input$which_group_t,idx_DI_g,input$space_g,input$t_g)[1]
    })
  })
  output$Result.s <- renderTable({
    #DI_s, t_s, which_group_s, DIM_t, space_t, which_group_t, DI_g, space_g, t_g
    input$button
    isolate({
      if(input$space_t=="Household"){
        DIM_t<-id_DIMs_hh[which(labels_DIMs_hh==input$DI_t)]
      }
      if(input$space_t=="Neighbourhood"){
        DIM_t<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$DI_t)]
      }
      if(input$space_t=="Region"){
        DIM_t<-id_DIMs_region[which(labels_DIMs_region==input$DI_t)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      idx_DI_s<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI_s)]
      idx_DI_g<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI_g)]
      CompareDIMs(idx_DI_s,input$t_s,input$which_group_s,DIM_t,input$space_t,input$which_group_t,idx_DI_g,input$space_g,input$t_g)[2]
    })
  })
  output$Result.t <- renderTable({
    #DI_s, t_s, which_group_s, DIM_t, space_t, which_group_t, DI_g, space_g, t_g
    input$button
    isolate({
      if(input$space_t=="Household"){
        DIM_t<-id_DIMs_hh[which(labels_DIMs_hh==input$DI_t)]
      }
      if(input$space_t=="Neighbourhood"){
        DIM_t<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$DI_t)]
      }
      if(input$space_t=="Region"){
        DIM_t<-id_DIMs_region[which(labels_DIMs_region==input$DI_t)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      idx_DI_s<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI_s)]
      idx_DI_g<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI_g)]
      CompareDIMs(idx_DI_s,input$t_s,input$which_group_s,DIM_t,input$space_t,input$which_group_t,idx_DI_g,input$space_g,input$t_g)[3]
    })
  })
  output$Result.g <- renderTable({
    #DI_s, t_s, which_group_s, DIM_t, space_t, which_group_t, DI_g, space_g, t_g
    input$button
    isolate({
      if(input$space_t=="Household"){
        DIM_t<-id_DIMs_hh[which(labels_DIMs_hh==input$DI_t)]
      }
      if(input$space_t=="Neighbourhood"){
        DIM_t<-id_DIMs_neighbour[which(labels_DIMs_neighbour==input$DI_t)]
      }
      if(input$space_t=="Region"){
        DIM_t<-id_DIMs_region[which(labels_DIMs_region==input$DI_t)]
      }
      idx_DIMs_combined <- c(id_DIMs_spatial,idx_DIMs_all)
      labels_DIMs_combined <- c(labels_DIMs_spatial,labels_DIMs_all)
      idx_DI_s<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI_s)]
      idx_DI_g<-idx_DIMs_combined[which(labels_DIMs_combined==input$DI_g)]
      CompareDIMs(idx_DI_s,input$t_s,input$which_group_s,DIM_t,input$space_t,input$which_group_t,idx_DI_g,input$space_g,input$t_g)[4]
    })
  })
}