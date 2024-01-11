# Created by Chenbo Wang on 12 Sep 2023
# Args:
# t: "2 weeks", "6 months", "over 6 months"
# which_group: "Resident", "Beneficiary", or "Both"
# wts: a vector containing two fractions that sum up to 1: c(weight_resident, weight_beneficiary)

RankDIMs <- function(A, t, which_group, wts,num){
  load("clean_Data.RData")
  # extract the mean importance rating for immediate household, neighbourhood, and region 
  # for the specified stakeholder group(s)
  # Note that B69 does not have a spatial resolution as it relates to indirect economic loss caused
  # by damage to the natural environment
  id_DIMs_hh <- c("B6.1","B7.1","B8.1","B10.1",
                  "B12.1","B13.1","B14.1","B15.1",
                  "B17.1","B18.1","B19.1","B20.1",
                  "B22.1","B23.1","B24.1","B25.1",
                  "B26.1","B28.1","B29.1","B30.1",
                  "B34.1","B36.1","B37.1","B41.1",
                  "B44.1","B45.1","B46.1","B48.1",
                  "B49.1","B50.1","B51.1","B53.1",
                  "B54.1","B56.1","B57.1","B58.1",
                  "B60.1","B67.1","B69","B71.1",
                  "B72.1")
  id_DIMs_neighbour <- c("B1.1","B2.1","B3.1","B4.1",
                         "B5.1","B6.2","B7.2","B8.2",
                         "B9.1","B10.2","B12.2","B13.2",
                         "B14.2","B15.2","B17.2","B18.2",
                         "B19.2","B20.2","B22.2","B23.2",
                         "B24.2","B25.2","B26.2","B28.2",
                         "B29.2","B30.2","B34.2","B36.2",
                         "B37.2","B41.2","B44.2","B45.2",
                         "B46.2","B48.2","B49.2","B50.2",
                         "B51.2","B53.2","B54.2","B56.2",
                         "B57.2","B58.2","B60.2","B67.2",
                         "B68.1","B69","B71.2","B72.2")
  id_DIMs_region <- c("B1.2","B2.2","B3.2","B4.2",
                      "B5.2","B6.3","B7.3","B8.3",
                      "B9.2","B10.3","B12.3","B13.3",
                      "B14.3","B15.3","B17.3","B18.3",
                      "B19.3","B20.3","B22.3","B23.3",
                      "B24.3","B25.3","B26.3","B28.3",
                      "B29.3","B30.3","B34.3","B36.3",
                      "B37.3","B41.3","B44.3","B45.3",
                      "B46.3","B48.3","B49.3","B50.3",
                      "B51.3","B53.3","B54.3","B56.3",
                      "B57.3","B58.3","B60.3","B67.3",
                      "B68.2","B69","B71.3","B72.3")
  id_DIMs_spatial <- c("B31","B32","B33","B38","B39",
                       "B40","B62","B63","B64","B65",
                       "B66") # spatial questions
  tbl_hh <- as_tibble(
    data.frame(
      matrix(nrow=(nrow(Dt_Ben.short)+nrow(Dt_Res.short)),ncol=(1+length(id_DIMs_hh)+length(id_DIMs_spatial)))
    )
  )
  colnames(tbl_hh) <- c("stakeholder",c(id_DIMs_hh,id_DIMs_spatial))
  tbl_neighbour <- as_tibble(
    data.frame(
      matrix(nrow=(nrow(Dt_Ben.short)+nrow(Dt_Res.short)),ncol=(1+length(id_DIMs_neighbour)+length(id_DIMs_spatial)))
    )
  )
  colnames(tbl_neighbour) <- c("stakeholder",c(id_DIMs_neighbour,id_DIMs_spatial))
  tbl_region <- as_tibble(
    data.frame(
      matrix(nrow=(nrow(Dt_Ben.short)+nrow(Dt_Res.short)),ncol=(1+length(id_DIMs_region)+length(id_DIMs_spatial)))
    )
  )
  colnames(tbl_region) <- c("stakeholder",c(id_DIMs_region,id_DIMs_spatial))
  
  tbl_hh["stakeholder"]<-c(rep("Resident",nrow(Dt_Res.short)),rep("Beneficiary",nrow(Dt_Ben.short)))
  tbl_neighbour["stakeholder"]<-c(rep("Resident",nrow(Dt_Res.short)),rep("Beneficiary",nrow(Dt_Ben.short)))
  tbl_region["stakeholder"]<-c(rep("Resident",nrow(Dt_Res.short)),rep("Beneficiary",nrow(Dt_Ben.short)))
  
  switch(t, `2 weeks`={ handle <- 1}, `6 months`={handle <- 2}, `over 6 months`={handle <- 4})
  for (DIMs in id_DIMs_hh){
    temp.Res <- unlist(Dt_Res.short[,DIMs])
    if (DIMs %in% c("B1.1","B1.2","B2.1","B2.2","B3.1","B3.2","B4.1","B4.2","B5.1","B5.2")){
      temp.Res <- as.integer(temp.Res)
    }else{
      temp.Res <- as.integer(temp.Res[seq(handle,250,by=5)])
    }
    temp.Ben <- unlist(Dt_Ben.short[,DIMs])
    temp.Ben <- as.integer(temp.Ben[seq(handle,228,by=6)])
    tbl_hh[DIMs] <- c(temp.Res,temp.Ben)
  }
  for (DIMs in id_DIMs_neighbour){
    temp.Res <- unlist(Dt_Res.short[,DIMs])
    if (DIMs %in% c("B1.1","B1.2","B2.1","B2.2","B3.1","B3.2","B4.1","B4.2","B5.1","B5.2")){
      temp.Res <- as.integer(temp.Res)
    }else{
      temp.Res <- as.integer(temp.Res[seq(handle,250,by=5)])
    }
    temp.Ben <- unlist(Dt_Ben.short[,DIMs])
    temp.Ben <- as.integer(temp.Ben[seq(handle,228,by=6)])
    tbl_neighbour[DIMs] <- c(temp.Res,temp.Ben)
  }
  for (DIMs in id_DIMs_region){
    temp.Res <- unlist(Dt_Res.short[,DIMs])
    if (DIMs %in% c("B1.1","B1.2","B2.1","B2.2","B3.1","B3.2","B4.1","B4.2","B5.1","B5.2")){
      temp.Res <- as.integer(temp.Res)
    }else{
      temp.Res <- as.integer(temp.Res[seq(handle,250,by=5)])
    }
    temp.Ben <- unlist(Dt_Ben.short[,DIMs])
    temp.Ben <- as.integer(temp.Ben[seq(handle,228,by=6)])
    tbl_region[DIMs] <- c(temp.Res,temp.Ben)
  }
  for (DIMs in id_DIMs_spatial){
    temp.Res <- unlist(Dt_Res.short[,DIMs])
    temp.Ben <- unlist(Dt_Ben.short[,DIMs])
    temp.Res_hh <- as.integer(temp.Res[seq(1,250,by=5)])
    temp.Res_neighbour <- as.integer(temp.Res[seq(2,250,by=5)])
    temp.Res_region <- as.integer(temp.Res[seq(3,250,by=5)])
    temp.Ben_hh <- as.integer(temp.Ben[seq(1,228,by=6)])
    temp.Ben_neighbour <- as.integer(temp.Ben[seq(2,228,by=6)])
    temp.Ben_region <- as.integer(temp.Ben[seq(3,228,by=6)])
    tbl_hh[DIMs] <- c(temp.Res_hh,temp.Ben_hh)
    tbl_neighbour[DIMs] <- c(temp.Res_neighbour,temp.Ben_neighbour)
    tbl_region[DIMs] <- c(temp.Res_region,temp.Ben_region)
  }
  id_DIMs_hh <- c(id_DIMs_hh,id_DIMs_spatial)
  id_DIMs_neighbour <- c(id_DIMs_neighbour,id_DIMs_spatial)
  id_DIMs_region <- c(id_DIMs_region,id_DIMs_spatial)
  
  tbl_hh[,c(2:ncol(tbl_hh))]<-tbl_hh[,c(2:ncol(tbl_hh))]-1
  tbl_neighbour[,c(2:ncol(tbl_neighbour))]<-tbl_neighbour[,c(2:ncol(tbl_neighbour))]-1
  tbl_region[,c(2:ncol(tbl_region))]<-tbl_region[,c(2:ncol(tbl_region))]-1
  
  switch(which_group, 
         `Resident`={
           weighted_mean_hh<-colMeans(tbl_hh[tbl_hh$stakeholder=="Resident",id_DIMs_hh],na.rm=T)
           weighted_mean_neighbour<-colMeans(tbl_neighbour[tbl_neighbour$stakeholder=="Resident",id_DIMs_neighbour],na.rm=T)
           weighted_mean_region<-colMeans(tbl_region[tbl_region$stakeholder=="Resident",id_DIMs_region],na.rm=T)
         }, 
         `Beneficiary`={
           weighted_mean_hh<-colMeans(tbl_hh[tbl_hh$stakeholder=="Beneficiary",id_DIMs_hh],na.rm=T)
           weighted_mean_neighbour<-colMeans(tbl_neighbour[tbl_neighbour$stakeholder=="Beneficiary",id_DIMs_neighbour],na.rm=T)
           weighted_mean_region<-colMeans(tbl_region[tbl_region$stakeholder=="Beneficiary",id_DIMs_region],na.rm=T)
         }, 
         `Both`={
           weighted_mean_hh<-wts[1]*colMeans(tbl_hh[tbl_hh$stakeholder=="Resident",id_DIMs_hh],na.rm=T)+wts[2]*colMeans(tbl_hh[tbl_hh$stakeholder=="Beneficiary",id_DIMs_hh],na.rm=T)
           weighted_mean_neighbour<-wts[1]*colMeans(tbl_neighbour[tbl_neighbour$stakeholder=="Resident",id_DIMs_neighbour],na.rm=T)+wts[2]*colMeans(tbl_neighbour[tbl_neighbour$stakeholder=="Beneficiary",id_DIMs_neighbour],na.rm=T)
           weighted_mean_region<-wts[1]*colMeans(tbl_region[tbl_region$stakeholder=="Resident",id_DIMs_region],na.rm=T)+wts[2]*colMeans(tbl_region[tbl_region$stakeholder=="Beneficiary",id_DIMs_region],na.rm=T)}
  )
  weighted_mean_hh[is.na(weighted_mean_hh)]<-0
  weighted_mean_neighbour[is.na(weighted_mean_neighbour)]<-0
  weighted_mean_region[is.na(weighted_mean_region)]<-0
  df.hh<-tibble(
    DIM=id_DIMs_hh[order(weighted_mean_hh, decreasing = T)],
    Score=sort(weighted_mean_hh, decreasing = T))
  df.neighbour<-tibble(
    DIM=id_DIMs_neighbour[order(weighted_mean_neighbour, decreasing = T)],
    Score=sort(weighted_mean_neighbour, decreasing = T))
  df.region<-tibble(
    DIM=id_DIMs_region[order(weighted_mean_region, decreasing = T)],
    Score=sort(weighted_mean_region, decreasing = T))
  # exclude spatial ones that do not belong to the associated time stamp
  switch(t, 
         `2 weeks`={
           df.hh<-df.hh[!df.hh$DIM %in% c("B33","B38"),]
           df.neighbour<-df.neighbour[!df.neighbour$DIM %in% c("B33","B38"),]
           df.region<-df.region[!df.region$DIM %in% c("B33","B38"),]
         }, 
         `6 months`={
           df.hh<-df.hh[!df.hh$DIM %in% c("B31","B32","B62","B63","B64","B65","B66"),]
           df.neighbour<-df.neighbour[!df.neighbour$DIM %in% c("B31","B32","B62","B63","B64","B65","B66"),]
           df.region<-df.region[!df.region$DIM %in% c("B31","B32","B62","B63","B64","B65","B66"),]
         }, 
         `over 6 months`={
           df.hh<-df.hh[!df.hh$DIM %in% c("B31","B32","B62","B63","B64","B65","B66"),]
           df.neighbour<-df.neighbour[!df.neighbour$DIM %in% c("B31","B32","B62","B63","B64","B65","B66"),]
           df.region<-df.region[!df.region$DIM %in% c("B31","B32","B62","B63","B64","B65","B66"),]
         }
  )
  labels_DIMs_spatial <- c("Acute severe injuries occur", # 2 weeks
                           "Fatalities occur", # 2 weeks 
                           "Chronic diseases develop", # 6 months and >6 months
                           "Permanent displacement occurs", # 6 months and >6 months
                           "Voluntary relocation occurs", # 2 weeks, 6 months and >6 months
                           "Connection with family and friends is permanently lost", # 2 weeks, 6 months and >6 months
                           "Damage to buildings leads to direct economic loss for building owners", # 2 weeks
                           "Damage to transportation networks leads to direct economic loss for operators", # 2 weeks
                           "Damage to utility networks leads to direct economic loss for operators", # 2 weeks
                           "Damage to telecommunication networks leads to direct economic loss for operators", # 2 weeks
                           "Damage to the natural environment leads to direct economic loss for relevant stakeholders") # 2 weeks
  labels_DIMs_hh <- c("Access to good-quality soil is lost",
                      "Access to clean air is lost",
                      "Access to green infrastructure is lost",
                      "Access to clean untreated freshwater sources is lost",
                      "Access to train services is lost",
                      "Access to bus services is lost",
                      "Access to private mobility services is lost",
                      "Daily commute time to work significantly increases",
                      "Access to water is lost",
                      "Access to sewage treatment services is lost",
                      "Access to electricity is lost",
                      "Access to natural gas is lost",
                      "Access to telephone networks is lost",
                      "Access to WiFi services is lost",
                      "Access to cellular services is lost",
                      "Access to corporate and academic wide area networks (WANs) is lost",
                      "Access to broadcast networks is lost",
                      "Access to required healthcare is lost",
                      "Access to remote medical consultation is lost",
                      "The mental well-being of people is affected",
                      "People are infected with communicable diseases",
                      "Access to community assets is lost",
                      "Temporary displacement occurs",
                      "Connection with family and friends is temporarily lost",
                      "Homelessness",
                      "Forced temporary re-housing occurs",
                      "Households continue to live in uninhabitable conditions",
                      "Access to in-person primary and secondary education services is lost",
                      "Access to online primary and secondary education services is lost",
                      "Access to in-person higher education services is lost",
                      "Access to online higher education services is lost",
                      "Access to places of worship is lost",
                      "Access to remote worship services is lost",
                      "Access to food is lost",
                      "Access to drinking water is lost",
                      "Food contamination exposure occurs",
                      "Unemployment",
                      "Closure of companies/organisations leads to indirect economic loss",
                      "Damage to the natural environment leads to indirect economic loss",
                      "Access to recreational facilities is lost",
                      "Access to natural or semi-natural areas is lost")
  labels_DIMs_neighbour <- c("Water bodies are contaminated",
                             "Air quality is reduced",
                             "Soil quality is reduced",
                             "Natural habitats are destroyed",
                             "Green infrastructure is damaged",
                             "Access to good-quality soil is lost",
                             "Access to clean air is lost",
                             "Access to green infrastructure is lost",
                             "Natural water purification is lost",
                             "Access to clean untreated freshwater sources is lost",
                             "Access to train services is lost",
                             "Access to bus services is lost",
                             "Access to private mobility services is lost",
                             "Daily commute time to work significantly increases",
                             "Access to water is lost",
                             "Access to sewage treatment services is lost",
                             "Access to electricity is lost",
                             "Access to natural gas is lost",
                             "Access to telephone networks is lost",
                             "Access to WiFi services is lost",
                             "Access to cellular services is lost",
                             "Access to corporate and academic wide area networks (WANs) is lost",
                             "Access to broadcast networks is lost",
                             "Access to required healthcare is lost",
                             "Access to remote medical consultation is lost",
                             "The mental well-being of people is affected",
                             "People are infected with communicable diseases",
                             "Access to community assets is lost",
                             "Temporary displacement occurs",
                             "Connection with family and friends is temporarily lost",
                             "Homelessness",
                             "Forced temporary re-housing occurs",
                             "Households continue to live in uninhabitable conditions",
                             "Access to in-person primary and secondary education services is lost",
                             "Access to online primary and secondary education services is lost",
                             "Access to in-person higher education services is lost",
                             "Access to online higher education services is lost",
                             "Access to places of worship is lost",
                             "Access to remote worship services is lost",
                             "Access to food is lost",
                             "Access to drinking water is lost",
                             "Food contamination exposure occurs",
                             "Unemployment",
                             "Closure of companies/organisations leads to indirect economic loss",
                             "Impact on tourism activities leads to indirect economic loss",
                             "Damage to the natural environment leads to indirect economic loss",
                             "Access to recreational facilities is lost",
                             "Access to natural or semi-natural areas is lost")
  labels_DIMs_region <- c("Water bodies are contaminated",
                          "Air quality is reduced",
                          "Soil quality is reduced",
                          "Natural habitats are destroyed",
                          "Green infrastructure is damaged",
                          "Access to good-quality soil is lost",
                          "Access to clean air is lost",
                          "Access to green infrastructure is lost",
                          "Natural water purification is lost",
                          "Access to clean untreated freshwater sources is lost",
                          "Access to train services is lost",
                          "Access to bus services is lost",
                          "Access to private mobility services is lost",
                          "Daily commute time to work significantly increases",
                          "Access to water is lost",
                          "Access to sewage treatment services is lost",
                          "Access to electricity is lost",
                          "Access to natural gas is lost",
                          "Access to telephone networks is lost",
                          "Access to WiFi services is lost",
                          "Access to cellular services is lost",
                          "Access to corporate and academic wide area networks (WANs) is lost",
                          "Access to broadcast networks is lost",
                          "Access to required healthcare is lost",
                          "Access to remote medical consultation is lost",
                          "The mental well-being of people is affected",
                          "People are infected with communicable diseases",
                          "Access to community assets is lost",
                          "Temporary displacement occurs",
                          "Connection with family and friends is temporarily lost",
                          "Homelessness",
                          "Forced temporary re-housing occurs",
                          "Households continue to live in uninhabitable conditions",
                          "Access to in-person primary and secondary education services is lost",
                          "Access to online primary and secondary education services is lost",
                          "Access to in-person higher education services is lost",
                          "Access to online higher education services is lost",
                          "Access to places of worship is lost",
                          "Access to remote worship services is lost",
                          "Access to food is lost",
                          "Access to drinking water is lost",
                          "Food contamination exposure occurs",
                          "Unemployment",
                          "Closure of companies/organisations leads to indirect economic loss",
                          "Impact on tourism activities leads to indirect economic loss",
                          "Damage to the natural environment leads to indirect economic loss",
                          "Access to recreational facilities is lost",
                          "Access to natural or semi-natural areas is lost")
  labels_DIMs_hh <- c(labels_DIMs_hh, labels_DIMs_spatial)
  labels_DIMs_neighbour <- c(labels_DIMs_neighbour, labels_DIMs_spatial)
  labels_DIMs_region <- c(labels_DIMs_region, labels_DIMs_spatial)
  df.hh$DIM <- labels_DIMs_hh[match(x = df.hh$DIM, table = id_DIMs_hh)]
  df.neighbour$DIM <- labels_DIMs_neighbour[match(x = df.neighbour$DIM, table = id_DIMs_neighbour)]
  df.region$DIM <- labels_DIMs_region[match(x = df.region$DIM, table = id_DIMs_region)]
  df.hh$DIM<-fct_rev(factor(df.hh$DIM,levels=df.hh$DIM, ordered=TRUE))
  df.neighbour$DIM<-fct_rev(factor(df.neighbour$DIM,levels=df.neighbour$DIM, ordered=TRUE))
  df.region$DIM<-fct_rev(factor(df.region$DIM,levels=df.region$DIM, ordered=TRUE))
  # ad-hoc fixes for out-of-bound responses
  df.hh$Score[df.hh$Score>=4]<-4
  df.neighbour$Score[df.neighbour$Score>=4]<-4
  df.region$Score[df.region$Score>=4]<-4
  
  ratio_importance<-tibble(matrix(nrow=3,ncol=3))
  colnames(ratio_importance)<-c("space","ratio of residual importance","ratio of importance")
  ratio_importance$space<-c("household","neighbourhood","region")
  ratio_importance$`ratio of residual importance`<-c(
    sum(df.hh$Score[1:num]-2)/sum(df.hh$Score[df.hh$Score>2]-2),
    sum(df.neighbour$Score[1:num]-2)/sum(df.neighbour$Score[df.neighbour$Score>2]-2),
    sum(df.region$Score[1:num]-2)/sum(df.region$Score[df.region$Score>2]-2)
  )
  ratio_importance$`ratio of importance`<-c(
    sum(df.hh$Score[1:num])/sum(df.hh$Score), # ratio of importance accounted for by top num DIMs
    sum(df.neighbour$Score[1:num])/sum(df.neighbour$Score),
    sum(df.region$Score[1:num])/sum(df.region$Score)
  )
  #retrieve the rankings of the specified DIM
  rank.A <- c(which(A==df.hh$DIM),which(A==df.neighbour$DIM),which(A==df.region$DIM))
  score.A <- c(df.hh$Score[rank.A],df.neighbour$Score[rank.A],df.region$Score[rank.A])
  result.A <- paste0("The selected disaster impact is ranked #",rank.A[1]," at the household level, with an average weighted importance score of ",format(score.A[1], digits = 4),
                     ", #",rank.A[2]," at the neighbourhood level, with an average weighted importance score of ",format(score.A[2], digits = 4),
                     ", and #",rank.A[3]," at the region level, with an average weighted importance score of ",format(score.A[3], digits = 4))
  plot.1 <- ggplot(df.hh[1:num,], aes(x = DIM, y = Score))+
    geom_bar(stat="identity",fill="lightblue",width=0.6) + coord_flip() + 
    ylab(expression(""*S[DIM[i]]*"")) +
    xlab(paste0("Disaster impacts ranked top ",num, " at the household level at ", t, " after the disaster")) + 
    theme_grey(base_size = 20) +
    scale_y_continuous(breaks=c(0,1,2,3,4),
                       limits=c(0,4),
                       labels=c("0\nUnimportant","1\nSomewhat \nunimportant","2\nNeither important \nnor unimportant","3\nSomewhat \nimportant","4\nImportant")) +
    scale_x_discrete(labels = label_wrap(60)) +
    labs(title = paste0("Weights: Residents - ",wts[1],", Other stakeholders - ",wts[2]))
  
  plot.2 <- ggplot(df.neighbour[1:num,], aes(x = DIM, y = Score))+
    geom_bar(stat="identity",fill="darkblue",width=0.6) + coord_flip() + 
    ylab(expression(""*S[DIM[i]]*"")) +
    xlab(paste0("Disaster impacts ranked top ",num, " at the neighbourhood level at ", t, " after the disaster")) + 
    theme_grey(base_size = 20) +
    scale_y_continuous(breaks=c(0,1,2,3,4),
                       limits=c(0,4),
                       labels=c("0\nUnimportant","1\nSomewhat \nunimportant","2\nNeither important \nnor unimportant","3\nSomewhat \nimportant","4\nImportant")) +
    scale_x_discrete(labels = label_wrap(60)) +
    labs(title = paste0("Weights: Residents - ",wts[1],", Other stakeholders - ",wts[2]))
  
  plot.3 <- ggplot(df.region[1:num,], aes(x = DIM, y = Score))+
    geom_bar(stat="identity",fill="darkred",width=0.6) + coord_flip() + 
    ylab(expression(""*S[DIM[i]]*"")) +
    xlab(paste0("Disaster impacts ranked top ",num, " at the region level at ", t, " after the disaster")) + 
    theme_grey(base_size = 20) +
    scale_y_continuous(breaks=c(0,1,2,3,4),
                       limits=c(0,4),
                       labels=c("0\nUnimportant","1\nSomewhat \nunimportant","2\nNeither important \nnor unimportant","3\nSomewhat \nimportant","4\nImportant")) +
    scale_x_discrete(labels = label_wrap(60)) +
    labs(title = paste0("Weights: Residents - ",wts[1],", Other stakeholders - ",wts[2]))
  return(list(df.hh,
              df.neighbour,
              df.region,
              tbl_hh,
              tbl_neighbour,
              tbl_region,
              plot.1,
              plot.2,
              plot.3,
              ratio_importance,
              result.A)
  )
}


