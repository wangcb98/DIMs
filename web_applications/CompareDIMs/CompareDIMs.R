# Created by Chenbo Wang on 18 Dec 2023

# Performs Welch ANOVA, ANOVA, Welch t-test across spatial (Household, Neighbourhood, and Region) questions 
# and for each resolution between residents and otheficiaries
# and shows the bar plots for each spatial resolution

# Applies to questions that have three spatial resolutions
# I.e., B31, B32, B33, B38, B39, B40, B62, B63, B64, B65, and B66

# Args:
# A: question to analyse the responses for
# which_group: "Resident", "`Other stakeholders`", or "Both"
# "res" - residents only, "oth" - otheficiaries only, "both" - both categories
path <- "/Users/patrick/Library/CloudStorage/Dropbox/Chenbo PhD folder/Impact Metric Study/Kathmandu"
setwd(path)
require(tidyverse)
require(dplyr)
require(haven)
require(glmnet)
require(ggplot2)
require(ggpubr)
require(scales)
require(labelled)
require(caret)
require(pROC)
require(ROCR)
require(tree)
require(randomForest)
require(smotefamily)
require(splitTools)
require(boot)
require(readxl)
require(writexl)
require(PRROC)
require(doParallel)
require(tictoc)
require(report)
require(pwr)
require(pwrss)
require(broom)
require(latticeExtra)
# CompareDIMs <- function(DI, which_group, t, A, B, how_jitter){
CompareDIMs <- function(DI_s, t_s, which_group_s, DIM_t, space_t, which_group_t, DI_g, space_g, t_g){
  load("clean_Data.RData")
  # DI_s: code names for disaster impacts to compare spatial scales for
  # DIM_t: code names for disaster impact metrics to compare temporal instances for
  
  # ----------------spatial comparison----------------
  # Compare importance scores across different spatial scales, 
  # considering a specific disaster impact, a prescribed temporal instance, and a set of stakeholder categories
  # --------------------------------------------------
  
  switch(t_s, `two weeks`={ handle_s <- 1}, `six months`={handle_s <- 2}, `over six months`={handle_s <- 4}, `not applicable`={handle_s<-NA})
  ## DIMs that are not spatial questions per se 
  ## (i.e., those that are associated with only one temporal instance, such as fatalities, accute injuries, and chronic disease)
  if(t_s=="not applicable"){
    t_s<-"immediately"
  }
  if (DI_s %in% idx_DIMs_all){
    # extract results associated with the specified disaster impact (DI_s) at different spatial scales
    res_hh<-c()
    oth_hh<-c()
    res_neighbour<-c()
    oth_neighbour<-c()
    res_region<-c()
    oth_region<-c()
    # extract the data for household, neighbourhood, and region (if applies)
    if (DI_s %in% gsub("\\..*","",id_DIMs_hh)){
      id_hh_res<-which(id_DIMs_hh[which(DI_s == gsub("\\..*","",id_DIMs_hh))]==colnames(Dt_Res.short))
      id_hh_oth<-which(id_DIMs_hh[which(DI_s == gsub("\\..*","",id_DIMs_hh))]==colnames(Dt_Oth.short))
      res_hh<-as.integer(unlist(Dt_Res.short[,id_hh_res])[seq(from = handle_s,to=250,by=5)])
      oth_hh<-as.integer(unlist(Dt_Oth.short[,id_hh_oth])[seq(from = handle_s,to=240,by=6)])
    }
    if (DI_s %in% gsub("\\..*","",id_DIMs_neighbour)){
      id_neighbour_res<-which(id_DIMs_neighbour[which(DI_s == gsub("\\..*","",id_DIMs_neighbour))]==colnames(Dt_Res.short))
      id_neighbour_oth<-which(id_DIMs_neighbour[which(DI_s == gsub("\\..*","",id_DIMs_neighbour))]==colnames(Dt_Oth.short))
      res_neighbour<-as.integer(unlist(Dt_Res.short[,id_neighbour_res])[seq(from = handle_s,to=250,by=5)])
      oth_neighbour<-as.integer(unlist(Dt_Oth.short[,id_neighbour_oth])[seq(from = handle_s,to=240,by=6)])
    }
    if (DI_s %in% gsub("\\..*","",id_DIMs_region)){
      id_region_res<-which(id_DIMs_region[which(DI_s == gsub("\\..*","",id_DIMs_region))]==colnames(Dt_Res.short))
      id_region_oth<-which(id_DIMs_region[which(DI_s == gsub("\\..*","",id_DIMs_region))]==colnames(Dt_Oth.short))
      res_region<-as.integer(unlist(Dt_Res.short[,id_region_res])[seq(from = handle_s,to=250,by=5)])
      oth_region<-as.integer(unlist(Dt_Oth.short[,id_region_oth])[seq(from = handle_s,to=240,by=6)])
    }
    # ad-hoc fix for out-of-bound responses
    res_hh[res_hh>5]<-5
    oth_hh[oth_hh>5]<-5
    res_neighbour[res_neighbour>5]<-5
    oth_neighbour[oth_neighbour>5]<-5
    res_region[res_region>5]<-5
    oth_region[oth_region>5]<-5
    
    # revised definition of importance scores
    # S_DIM_i values: -1, 0, 1, 2, 3, 4. 
    res_hh <- res_hh -1
    res_neighbour <- res_neighbour -1
    res_region <- res_region -1
    oth_hh <- oth_hh -1
    oth_neighbour <- oth_neighbour -1
    oth_region <- oth_region -1
    
    df.both<-tibble(
      score=c(res_hh,oth_hh,res_neighbour,oth_neighbour,res_region,oth_region),
      space=c(rep("Household",length(c(res_hh,oth_hh))), # 50 residents and then 40 other stakeholders
              rep("Neighbourhood",length(c(res_neighbour,oth_neighbour))),
              rep("Region",length(c(res_region,oth_region))))
    ) 
    df.res<-tibble(
      score=c(res_hh,res_neighbour,res_region),
      space=c(rep("Household",length(res_hh)),
              rep("Neighbourhood",length(res_neighbour)),
              rep("Region",length(res_region)))
    )
    df.oth<-tibble(
      score=c(oth_hh,oth_neighbour,oth_region),
      space=c(rep("Household",length(oth_hh)),
              rep("Neighbourhood",length(oth_neighbour)),
              rep("Region",length(oth_region)))
    )
    
    switch(which_group_s, 
           Resident={
             df.s<-df.res
           },
           `Other stakeholders`={
             df.s<-df.oth
           },
           Both={
             df.s<-df.both
           })
    DIM_s<-labels_DIMs_all[which(idx_DIMs_all==DI_s)]
    if("Neighbourhood" %in% unique(df.s$space) && "Region" %in% unique(df.s$space)){
      aux.text <- paste0(tolower(unique(df.s$space)[1])," and ",tolower(unique(df.s$space)[2]))
    }
    if("Household" %in% unique(df.s$space) && "Neighbourhood" %in% unique(df.s$space) && "Region" %in% unique(df.s$space)){
      aux.text <- paste0(tolower(unique(df.s$space)[1]),", ",tolower(unique(df.oth$space)[2]), ", and ", tolower(unique(df.s$space)[3]))
    }
    # Welch ANOVA 
    Welch.aov <- oneway.test(score~space,data = na.omit(df.s),var.equal = F)
    if(Welch.aov$p.value>0.05){
      if("Household" %in% unique(df.s$space) && "Neighbourhood" %in% unique(df.s$space) && "Region" %in% unique(df.s$space)){
        Result.s.welch.anova<-paste0("The differences in importance scores for ",tolower(DIM_s),
                                     " at the ",aux.text," level ", t_s,
                                     " after the event are found to be NOT statistically significant with p-value= ",
                                     format(Welch.aov$p.value,digits=4), ", and importance scores being ",
                                     format(mean(df.s$score[df.s$space=="Household"],na.rm=T),digit=4),", ",
                                     format(mean(df.s$score[df.s$space=="Neighbourhood"],na.rm=T),digit=4),", and ",
                                     format(mean(df.s$score[df.s$space=="Region"],na.rm=T),digit=4),", respectively.")
      }else if("Neighbourhood" %in% unique(df.s$space) && "Region" %in% unique(df.s$space)){
        Result.s.welch.anova<-paste0("The differences in importance scores for ",tolower(DIM_s),
                                     " at the ",aux.text," level ", t_s,
                                     " after the event are found to be NOT statistically significant with p-value= ",
                                     format(Welch.aov$p.value,digits=4), ", and importance scores being ",
                                     format(mean(df.s$score[df.s$space=="Neighbourhood"],na.rm=T),digit=4),", and ",
                                     format(mean(df.s$score[df.s$space=="Region"],na.rm=T),digit=4),", respectively.")
      }
    }else{
      if("Household" %in% unique(df.s$space) && "Neighbourhood" %in% unique(df.s$space) && "Region" %in% unique(df.s$space)){
        Result.s.welch.anova<-paste0("The differences in importance scores for ",tolower(DIM_s),
                                     " at the ",aux.text," level ", t_s,
                                     " after the event are found to be statistically significant with p-value= ",
                                     format(Welch.aov$p.value,digits=4), ", and importance scores being ",
                                     format(mean(df.s$score[df.s$space=="Household"],na.rm=T),digit=4),", ",
                                     format(mean(df.s$score[df.s$space=="Neighbourhood"],na.rm=T),digit=4),", and ",
                                     format(mean(df.s$score[df.s$space=="Region"],na.rm=T),digit=4),", respectively.")
      }else{
        Result.s.welch.anova<-paste0("The differences in importance scores for ",tolower(DIM_s),
                                     " at the ",aux.text," level ", t_s,
                                     " after the event are found to be statistically significant with p-value= ",
                                     format(Welch.aov$p.value,digits=4), ", and importance scores being ",
                                     format(mean(df.s$score[df.s$space=="Neighbourhood"],na.rm=T),digit=4),", and ",
                                     format(mean(df.s$score[df.s$space=="Region"],na.rm=T),digit=4),", respectively.")
      }
    }
    
    if("Household" %in% df.s$space){
      # two-sample t test
      tb.t.test.s<-rbind(tidy(t.test(df.s$score[df.s$space == "Household"],df.s$score[df.s$space == "Neighbourhood"])),
                         tidy(t.test(df.s$score[df.s$space == "Household"],df.s$score[df.s$space == "Region"])),
                         tidy(t.test(df.s$score[df.s$space == "Region"],df.s$score[df.s$space == "Neighbourhood"])))
      # power analyses
      # for the specified category(ies): household vs neighbourhood
      pwr.s.1<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Household"],na.rm=T), 
                              mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                              sd1 = sd(df.s$score[df.s$space == "Household"],na.rm=T), 
                              sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                              kappa = length(na.omit(df.s$score[df.s$space == "Household"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                              n2 = length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), alpha = 0.05,
                              alternative = "not equal")# power analysis
      pwr.s.1<-cbind(as.tibble(pwr.s.1$parms),pwr.s.1$test,pwr.s.1$df,pwr.s.1$ncp,pwr.s.1$power,pwr.s.1$n[1],pwr.s.1$n[2])
      colnames(pwr.s.1)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
      pwr.s.1.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Household"],na.rm=T), 
                                  mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                  sd1 = sd(df.s$score[df.s$space == "Household"],na.rm=T), 
                                  sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                  kappa = length(na.omit(df.s$score[df.s$space == "Household"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                                  power = .80, alpha = 0.05,
                                  alternative = "not equal")
      pwr.s.1.inv<-cbind(as.tibble(pwr.s.1.inv$parms),pwr.s.1.inv$test,pwr.s.1.inv$df,pwr.s.1.inv$ncp,pwr.s.1.inv$power,pwr.s.1.inv$n[1],pwr.s.1.inv$n[2])
      colnames(pwr.s.1.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                               "paired", "paired.r", "alpha", "margin", "alternative",
                               "verbose","test","df","ncp","power","n1","n2")
      # "specified category(ies): household vs region"
      pwr.s.2<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Household"],na.rm=T), 
                              mu2 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                              sd1 = sd(df.s$score[df.s$space == "Household"],na.rm=T), 
                              sd2 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                              kappa = length(na.omit(df.s$score[df.s$space == "Household"]))/length(na.omit(df.s$score[df.s$space == "Region"])), 
                              n2 = length(na.omit(df.s$score[df.s$space == "Region"])), alpha = 0.05,
                              alternative = "not equal")# power analysis
      pwr.s.2<-cbind(as.tibble(pwr.s.2$parms),pwr.s.2$test,pwr.s.2$df,pwr.s.2$ncp,pwr.s.2$power,pwr.s.2$n[1],pwr.s.2$n[2])
      colnames(pwr.s.2)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
      pwr.s.2.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Household"],na.rm=T), 
                                  mu2 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                                  sd1 = sd(df.s$score[df.s$space == "Household"],na.rm=T), 
                                  sd2 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                                  kappa = length(na.omit(df.s$score[df.s$space == "Household"]))/length(na.omit(df.s$score[df.s$space == "Region"])), 
                                  power = .80, alpha = 0.05,
                                  alternative = "not equal")
      pwr.s.2.inv<-cbind(as.tibble(pwr.s.2.inv$parms),pwr.s.2.inv$test,pwr.s.2.inv$df,pwr.s.2.inv$ncp,pwr.s.2.inv$power,pwr.s.2.inv$n[1],pwr.s.2.inv$n[2])
      colnames(pwr.s.2.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                               "paired", "paired.r", "alpha", "margin", "alternative",
                               "verbose","test","df","ncp","power","n1","n2")
      
      # "specified category(ies): region vs neighbourhood"
      pwr.s.3<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                              mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                              sd1 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                              sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                              kappa = length(na.omit(df.s$score[df.s$space == "Region"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                              n2 = length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), alpha = 0.05,
                              alternative = "not equal")# power analysis
      pwr.s.3<-cbind(as.tibble(pwr.s.3$parms),pwr.s.3$test,pwr.s.3$df,pwr.s.3$ncp,pwr.s.3$power,pwr.s.3$n[1],pwr.s.3$n[2])
      colnames(pwr.s.3)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
      pwr.s.3.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                                  mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                  sd1 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                                  sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                  kappa = length(na.omit(df.s$score[df.s$space == "Region"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                                  power = .80, alpha = 0.05,
                                  alternative = "not equal")
      pwr.s.3.inv<-cbind(as.tibble(pwr.s.3.inv$parms),pwr.s.3.inv$test,pwr.s.3.inv$df,pwr.s.3.inv$ncp,pwr.s.3.inv$power,pwr.s.3.inv$n[1],pwr.s.3.inv$n[2])
      colnames(pwr.s.3.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                               "paired", "paired.r", "alpha", "margin", "alternative",
                               "verbose","test","df","ncp","power","n1","n2")
      pwr.s <- rbind(pwr.s.1,pwr.s.2,pwr.s.3)
      pwr.s.inv <- rbind(pwr.s.1.inv,pwr.s.2.inv,pwr.s.3.inv)
      Result.s<-c()
      if(tb.t.test.s$p.value[1]>0.05 || pwr.s$power[1]<0.80){
        Result.s[1]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                             " at the household and neighbourhood levels ",
                             t_s, " after the event",
                             " are found to be NOT statistically significant with p-value = ", format(tb.t.test.s$p.value[1],digits=4),
                             ", statistical power = ", format(pwr.s$power[1],digits=4),
                             ", and importance scores being ",format(tb.t.test.s$estimate1[1],digits=4), " and ", format(tb.t.test.s$estimate2[1],digits=4), ", respectively.",
                             " To have a power of 0.80, the required sample sizes for household- and neighbourhood-level responses are ",
                             pwr.s.inv$n1[1], " and ",pwr.s.inv$n2[1], ", respectively.")
      }else{
        Result.s[1]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                             " at the household and neighbourhood levels ",
                             t_s, " after the event",
                             " are found to be statistically significant with p-value= ", format(tb.t.test.s$p.value[1],digits=4),
                             ", statistical power = ", format(pwr.s$power[1],digits=4),
                             ", and importance scores being ",format(tb.t.test.s$estimate1[1],digits=4), " and ", format(tb.t.test.s$estimate2[1],digits=4), ", respectively")
      }
      
      if(tb.t.test.s$p.value[2]>0.05 || pwr.s$power[2]<0.80){
        Result.s[2]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                             " at the household and region levels ",
                             t_s, " after the event",
                             " are found to be NOT statistically significant with p-value = ", format(tb.t.test.s$p.value[2],digits=4),
                             ", statistical power = ", format(pwr.s$power[2],digits=4),
                             ", and importance scores being ",format(tb.t.test.s$estimate1[2],digits=4), " and ", format(tb.t.test.s$estimate2[2],digits=4), ", respectively.",
                             " To have a power of 0.80, the required sample sizes for household- and region-level responses are ",
                             pwr.s.inv$n1[2], " and ",pwr.s.inv$n2[2], ", respectively.")
      }else{
        Result.s[2]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                             " at the household and region levels ",
                             t_s, " after the event",
                             " are found to be statistically significant with p-value= ", format(tb.t.test.s$p.value[2],digits=4),
                             ", statistical power = ", format(pwr.s$power[2],digits=4),
                             ", and importance scores being ",format(tb.t.test.s$estimate1[2],digits=4), " and ", format(tb.t.test.s$estimate2[2],digits=4), ", respectively")
      }
      if(tb.t.test.s$p.value[3]>0.05 || pwr.s$power[3]<0.80){
        Result.s[3]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                             " at the region and neighbourhood levels ",
                             t_s, " after the event",
                             " are found to be NOT statistically significant with p-value = ", format(tb.t.test.s$p.value[3],digits=4),
                             ", statistical power = ", format(pwr.s$power[3],digits=4),
                             ", and importance scores being ",format(tb.t.test.s$estimate1[3],digits=4), " and ", format(tb.t.test.s$estimate2[3],digits=4), ", respectively.",
                             " To have a power of 0.80, the required sample sizes for region- and neighbourhood-level responses are ",
                             pwr.s.inv$n1[3], " and ",pwr.s.inv$n2[3], ", respectively.")
      }else{
        Result.s[3]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                             " at the region and neighbourhood levels ",
                             t_s, " after the event",
                             " are found to be statistically significant with p-value= ", format(tb.t.test.s$p.value[3],digits=4),
                             ", statistical power = ", format(pwr.s$power[3],digits=4),
                             ", and importance scores being ",format(tb.t.test.s$estimate1[3],digits=4), " and ", format(tb.t.test.s$estimate2[3],digits=4), ", respectively")
      }
    }else{
      # Two-sample t test
      tb.t.test.s<-rbind(tidy(t.test(df.s$score[df.s$space == "Region"],df.s$score[df.s$space == "Neighbourhood"])),
                         tidy(t.test(df.res$score[df.res$space == "Neighbourhood"],df.oth$score[df.oth$space == "Neighbourhood"])))
      # Power analysis
      # "specified category(ies): region vs neighbourhood"
      pwr.s.3<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                              mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                              sd1 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                              sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                              kappa = length(na.omit(df.s$score[df.s$space == "Region"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                              n2 = length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), alpha = 0.05,
                              alternative = "not equal")# power analysis
      pwr.s.3<-cbind(as.tibble(pwr.s.3$parms),pwr.s.3$test,pwr.s.3$df,pwr.s.3$ncp,pwr.s.3$power,pwr.s.3$n[1],pwr.s.3$n[2])
      colnames(pwr.s.3)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
      pwr.s.3.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                                  mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                  sd1 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                                  sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                  kappa = length(na.omit(df.s$score[df.s$space == "Region"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                                  power = .80, alpha = 0.05,
                                  alternative = "not equal")
      pwr.s.3.inv<-cbind(as.tibble(pwr.s.3.inv$parms),pwr.s.3.inv$test,pwr.s.3.inv$df,pwr.s.3.inv$ncp,pwr.s.3.inv$power,pwr.s.3.inv$n[1],pwr.s.3.inv$n[2])
      colnames(pwr.s.3.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                               "paired", "paired.r", "alpha", "margin", "alternative",
                               "verbose","test","df","ncp","power","n1","n2")
      pwr.s <- pwr.s.3
      pwr.s.inv <- pwr.s.3.inv
      Result.s<-c()
      if(tb.t.test.s$p.value[1]>0.05 || pwr.s$power[1]<0.80){
        Result.s[1]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                             " at the region and neighbourhood levels ",
                             t_s, " after the event",
                             " are found to be NOT statistically significant with p-value = ", format(tb.t.test.s$p.value[1],digits=4),
                             ", statistical power = ", format(pwr.s$power[1],digits=4),
                             ", and importance scores being ",format(tb.t.test.s$estimate1[1],digits=4), " and ", format(tb.t.test.s$estimate2[1],digits=4), ", respectively.",
                             " To have a power of 0.80, the required sample sizes for region- and neighbourhood-level responses are ",
                             pwr.s.inv$n1[1], " and ",pwr.s.inv$n2[1], ", respectively.")
      }else{
        Result.s[1]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                             " at the region and neighbourhood levels ",
                             t_s, " after the event",
                             " are found to be statistically significant with p-value= ", format(tb.t.test.s$p.value[1],digits=4),
                             ", statistical power = ", format(pwr.s$power[1],digits=4),
                             ", and importance scores being ",format(tb.t.test.s$estimate1[1],digits=4), " and ", format(tb.t.test.s$estimate2[1],digits=4), ", respectively")
      }
    }
  }
  if (DI_s %in% c("B31","B32","B33","B38","B39","B40","B62","B63","B64","B65","B66")){
    id_res_s <- which(DI_s==colnames(Dt_Res.short))
    id_oth_s <- which(DI_s==colnames(Dt_Oth.short))
    res_hh <- as.integer(unlist(Dt_Res.short[,id_res_s])[seq(from = 1,to=250,by=5)])
    res_neighbour <- as.integer(unlist(Dt_Res.short[,id_res_s])[seq(from = 2,to=250,by=5)])
    res_region <- as.integer(unlist(Dt_Res.short[,id_res_s])[seq(from = 3,to=250,by=5)])
    oth_hh <- as.integer(unlist(Dt_Oth.short[,id_oth_s])[seq(from = 1,to=240,by=6)])
    oth_neighbour <- as.integer(unlist(Dt_Oth.short[,id_oth_s])[seq(from = 2,to=240,by=6)])
    oth_region <- as.integer(unlist(Dt_Oth.short[,id_oth_s])[seq(from = 3,to=240,by=6)])
    # ad-hoc fix for out-of-bound responses
    res_hh[res_hh>5]<-5
    oth_hh[oth_hh>5]<-5
    res_neighbour[res_neighbour>5]<-5
    oth_neighbour[oth_neighbour>5]<-5
    res_region[res_region>5]<-5
    oth_region[oth_region>5]<-5
    # revised definition of importance scores
    # S_DIM_i values: -1, 0, 1, 2, 3, 4. 
    res_hh <- res_hh -1
    res_neighbour <- res_neighbour -1
    res_region <- res_region -1
    oth_hh <- oth_hh -1
    oth_neighbour <- oth_neighbour -1
    oth_region <- oth_region -1
    
    df.both<-tibble(
      score=c(res_hh,oth_hh,res_neighbour,oth_neighbour,res_region,oth_region),
      space=c(rep("Household",90),rep("Neighbourhood",90),rep("Region",90)))
    df.res<-df.both[c(1:50,91:140,181:230),]
    df.oth<-df.both[-c(1:50,91:140,181:230),]
    switch(which_group_s, 
           Resident={
             df.s<-tibble(
               score=c(res_hh,res_neighbour,res_region),
               space=c(rep("Household",50),rep("Neighbourhood",50),rep("Region",50)))
           },
           `Other stakeholders`={
             df.s<-tibble(
               score=c(oth_hh,oth_neighbour,oth_region),
               space=c(rep("Household",40),rep("Neighbourhood",40),rep("Region",40)))
           },
           Both={
             df.s<-df.both
           })
    DIM_s<-labels_DIMs_spatial[which(id_DIMs_spatial==DI_s)]
    # Welch ANOVA 
    aux.text <- paste0(tolower(unique(df.s$space)[1]),", ",tolower(unique(df.oth$space)[2]), ", and ", tolower(unique(df.s$space)[3]))
    # Welch ANOVA 
    Welch.aov <- oneway.test(score~space,data = na.omit(df.s),var.equal = F)
    if(Welch.aov$p.value>0.05){
      Result.s.welch.anova<-paste0("The differences in importance scores for ",tolower(DIM_s),
                                   " at the ",aux.text," level ", t_s,
                                   " after the event are found to be statistically significant with p-value= ",
                                   format(Welch.aov$p.value,digits=4),", and importance scores being ",
                                   format(mean(df.s$score[df.s$space=="Household"],na.rm=T),digit=4),", ",
                                   format(mean(df.s$score[df.s$space=="Neighbourhood"],na.rm=T),digit=4),", and ",
                                   format(mean(df.s$score[df.s$space=="Region"],na.rm=T),digit=4),", respectively.")
    }else{
      Result.s.welch.anova<-paste0("The differences in importance scores for ",tolower(DIM_s),
                                   " at the ",aux.text," level ", t_s,
                                   " after the event are found to be statistically significant with p-value= ", 
                                   format(Welch.aov$p.value,digits=4),", and importance scores being ",
                                   format(mean(df.s$score[df.s$space=="Household"],na.rm=T),digit=4),", ",
                                   format(mean(df.s$score[df.s$space=="Neighbourhood"],na.rm=T),digit=4),", and ",
                                   format(mean(df.s$score[df.s$space=="Region"],na.rm=T),digit=4),", respectively.")
    }
    tb.t.test.s<-rbind(tidy(t.test(df.s$score[df.s$space == "Household"],df.s$score[df.s$space == "Neighbourhood"])),
                       tidy(t.test(df.s$score[df.s$space == "Household"],df.s$score[df.s$space == "Region"])),
                       tidy(t.test(df.s$score[df.s$space == "Region"],df.s$score[df.s$space == "Neighbourhood"])))
    # power analyses
    # for the specified category(ies): household vs neighbourhood
    pwr.s.1<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Household"],na.rm=T), 
                            mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                            sd1 = sd(df.s$score[df.s$space == "Household"],na.rm=T), 
                            sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                            kappa = length(na.omit(df.s$score[df.s$space == "Household"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                            n2 = length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
    pwr.s.1<-cbind(as.tibble(pwr.s.1$parms),pwr.s.1$test,pwr.s.1$df,pwr.s.1$ncp,pwr.s.1$power,pwr.s.1$n[1],pwr.s.1$n[2])
    colnames(pwr.s.1)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
    pwr.s.1.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Household"],na.rm=T), 
                                mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                sd1 = sd(df.s$score[df.s$space == "Household"],na.rm=T), 
                                sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                kappa = length(na.omit(df.s$score[df.s$space == "Household"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
    pwr.s.1.inv<-cbind(as.tibble(pwr.s.1.inv$parms),pwr.s.1.inv$test,pwr.s.1.inv$df,pwr.s.1.inv$ncp,pwr.s.1.inv$power,pwr.s.1.inv$n[1],pwr.s.1.inv$n[2])
    colnames(pwr.s.1.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
    # "specified category(ies): household vs region"
    pwr.s.2<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Household"],na.rm=T), 
                            mu2 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                            sd1 = sd(df.s$score[df.s$space == "Household"],na.rm=T), 
                            sd2 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                            kappa = length(na.omit(df.s$score[df.s$space == "Household"]))/length(na.omit(df.s$score[df.s$space == "Region"])), 
                            n2 = length(na.omit(df.s$score[df.s$space == "Region"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
    pwr.s.2<-cbind(as.tibble(pwr.s.2$parms),pwr.s.2$test,pwr.s.2$df,pwr.s.2$ncp,pwr.s.2$power,pwr.s.2$n[1],pwr.s.2$n[2])
    colnames(pwr.s.2)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
    pwr.s.2.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Household"],na.rm=T), 
                                mu2 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                                sd1 = sd(df.s$score[df.s$space == "Household"],na.rm=T), 
                                sd2 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                                kappa = length(na.omit(df.s$score[df.s$space == "Household"]))/length(na.omit(df.s$score[df.s$space == "Region"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
    pwr.s.2.inv<-cbind(as.tibble(pwr.s.2.inv$parms),pwr.s.2.inv$test,pwr.s.2.inv$df,pwr.s.2.inv$ncp,pwr.s.2.inv$power,pwr.s.2.inv$n[1],pwr.s.2.inv$n[2])
    colnames(pwr.s.2.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
    
    # "specified category(ies): region vs neighbourhood"
    pwr.s.3<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                            mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                            sd1 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                            sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                            kappa = length(na.omit(df.s$score[df.s$space == "Region"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                            n2 = length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
    pwr.s.3<-cbind(as.tibble(pwr.s.3$parms),pwr.s.3$test,pwr.s.3$df,pwr.s.3$ncp,pwr.s.3$power,pwr.s.3$n[1],pwr.s.3$n[2])
    colnames(pwr.s.3)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
    pwr.s.3.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.s$score[df.s$space == "Region"],na.rm=T), 
                                mu2 = mean(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                sd1 = sd(df.s$score[df.s$space == "Region"],na.rm=T), 
                                sd2 = sd(df.s$score[df.s$space == "Neighbourhood"],na.rm=T), 
                                kappa = length(na.omit(df.s$score[df.s$space == "Region"]))/length(na.omit(df.s$score[df.s$space == "Neighbourhood"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
    pwr.s.3.inv<-cbind(as.tibble(pwr.s.3.inv$parms),pwr.s.3.inv$test,pwr.s.3.inv$df,pwr.s.3.inv$ncp,pwr.s.3.inv$power,pwr.s.3.inv$n[1],pwr.s.3.inv$n[2])
    colnames(pwr.s.3.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
    pwr.s <- rbind(pwr.s.1,pwr.s.2,pwr.s.3)
    pwr.s.inv <- rbind(pwr.s.1.inv,pwr.s.2.inv,pwr.s.3.inv)
    Result.s<-c()
    if(tb.t.test.s$p.value[1]>0.05 || pwr.s$power[1]<0.80){
      Result.s[1]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                           " at the household and neighbourhood levels ",
                           t_s, " after the event",
                           " are found to be NOT statistically significant with p-value = ", format(tb.t.test.s$p.value[1],digits=4),
                           ", statistical power = ", format(pwr.s$power[1],digits=4),
                           ", and importance scores being ",format(tb.t.test.s$estimate1[1],digits=4), " and ", format(tb.t.test.s$estimate2[1],digits=4), ", respectively.",
                           " To have a power of 0.80, the required sample sizes for household- and neighbourhood-level responses are ",
                           pwr.s.inv$n1[1], " and ",pwr.s.inv$n2[1], ", respectively.")
    }else{
      Result.s[1]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                           " at the household and neighbourhood levels ",
                           t_s, " after the event", " are found to be statistically significant with p-value= ", format(tb.t.test.s$p.value[1],digits=4),
                           ", statistical power = ", format(pwr.s$power[1],digits=4),
                           ", and importance scores being ",format(tb.t.test.s$estimate1[1],digits=4), " and ", format(tb.t.test.s$estimate2[1],digits=4), ", respectively")
    }
    
    if(tb.t.test.s$p.value[2]>0.05 || pwr.s$power[2]<0.80){
      Result.s[2]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                           " at the household and region levels ",
                           t_s, " after the event",
                           " are found to be NOT statistically significant with p-value = ", format(tb.t.test.s$p.value[2],digits=4),
                           ", statistical power = ", format(pwr.s$power[2],digits=4),
                           ", and importance scores being ",format(tb.t.test.s$estimate1[2],digits=4), " and ", format(tb.t.test.s$estimate2[2],digits=4), ", respectively.",
                           " To have a power of 0.80, the required sample sizes for household- and region-level responses are ",
                           pwr.s.inv$n1[2], " and ",pwr.s.inv$n2[2], ", respectively.")
    }else{
      Result.s[2]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                           " at the household and region levels ",
                           t_s, " after the event",
                           " are found to be statistically significant with p-value= ", format(tb.t.test.s$p.value[2],digits=4),
                           ", statistical power = ", format(pwr.s$power[2],digits=4),
                           ", and importance scores being ",format(tb.t.test.s$estimate1[2],digits=4), " and ", format(tb.t.test.s$estimate2[2],digits=4), ", respectively")
    }
    if(tb.t.test.s$p.value[3]>0.05 || pwr.s$power[3]<0.80){
      Result.s[3]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                           " at the region and neighbourhood levels ",
                           t_s, " after the event",
                           " are found to be NOT statistically significant with p-value = ", format(tb.t.test.s$p.value[3],digits=4),
                           ", statistical power = ", format(pwr.s$power[3],digits=4),
                           ", and importance scores being ",format(tb.t.test.s$estimate1[3],digits=4), " and ", format(tb.t.test.s$estimate2[3],digits=4), ", respectively.",
                           " To have a power of 0.80, the required sample sizes for region- and neighbourhood-level responses are ",
                           pwr.s.inv$n1[3], " and ",pwr.s.inv$n2[3], ", respectively.")
    }else{
      Result.s[3]<- paste0("The differences in importance scores for ",tolower(DIM_s),
                           " at the region and neighbourhood levels ",
                           t_s, " after the event",
                           " are found to be statistically significant with p-value= ", format(tb.t.test.s$p.value[3],digits=4),
                           ", statistical power = ", format(pwr.s$power[3],digits=4),
                           ", and importance scores being ",format(tb.t.test.s$estimate1[3],digits=4), " and ", format(tb.t.test.s$estimate2[3],digits=4), ", respectively")
    }
  }
  
  # ----------------temporal comparison----------------
  # Compare importance scores across different temporal instances, 
  # considering a specific disaster impact, a prescribed spatial scale, and a set of stakeholder categories
  # --------------------------------------------------
  id_res_t <- which(DIM_t==colnames(Dt_Res.short))
  id_oth_t <- which(DIM_t==colnames(Dt_Oth.short))
  #otheficiaries
  #------add-hoc fixes (not sure if I should write over original data)
  oth_t <- unlist(Dt_Oth.short[,id_oth_t])[seq(from = 1,to=240,by=6)]
  oth_t[oth_t=="No"] <- "1"
  oth_t[oth_t=="Yes"] <- "5"
  oth_t[oth_t=="Yes somewhat"] <- "4"
  oth_t[oth_t=="Not sure"] <- "3"
  #-------------------
  oth_t<-as.integer(oth_t)
  # residents
  if(id_res_t<26){
    res_t<-as.integer(unlist(Dt_Res.short[,id_res_t]))
  }else{
    res_t <- unlist(Dt_Res.short[,id_res_t])[seq(from = 1,to=250,by=5)]
    res_t<-as.integer(res_t)
  }
  
  # extract the longest tolerable duration associated with DIM_t
  res_t.6month <- as.integer(unlist(Dt_Res.short[,id_res_t])[seq(from = 2,to=250,by=5)])
  oth_t.6month <- unlist(Dt_Oth.short[,id_oth_t])[seq(from = 2,to=240,by=6)]
  oth_t.6month[oth_t.6month=="y"] <- "5"
  oth_t.6month <- as.integer(oth_t.6month)
  ld.res_t <- as.character(unlist(Dt_Res.short[,id_res_t])[seq(from = 3,to=250,by=5)])
  ld.score.res_t <- as.integer(unlist(Dt_Res.short[,id_res_t])[seq(from = 4,to=250,by=5)])
  ld.oth_t <- as.character(unlist(Dt_Oth.short[,id_oth_t])[seq(from = 3,to=240,by=6)])
  ld.score.oth_t <- as.integer(unlist(Dt_Oth.short[,id_oth_t])[seq(from = 4,to=240,by=6)])
  # treat out-of-bound score
  res_t.6month[res_t.6month==6]<-5
  
  #---------New definition of importance scores 0 to 4---------------
  # WARNING: this change is NOT reflected for the longest duration
  res_t <- res_t - 1
  res_t.6month <- res_t.6month - 1
  oth_t <- oth_t - 1
  oth_t.6month <- oth_t.6month - 1
  #------------------------------------------------------------------
  
  df.t.full<-tibble(
    x=c(res_t,oth_t),
    x.6month=c(res_t.6month,oth_t.6month),
    x.ld=c(ld.res_t,ld.oth_t),
    x.ld.score=c(ld.score.res_t,ld.score.oth_t),
    stakeholder=c(rep("Resident",50),rep("Other stakeholders",40)))
  colnames(df.t.full)<-c("Importance score two weeks","Importance score six months",
                         "DIM (temporal) longest tolerable duration",
                         "DIM (temporal) importance score for longest tolerable duration",
                         "Stakeholder")
  
  switch(which_group_t, 
         Resident={
           df.t<-df.t.full[1:50,]
         },
         `Other stakeholders`={
           df.t<-df.t.full[51:90,]
         },
         Both={
           df.t<-df.t.full
         })
  
  tb.t.test.t<-tidy(t.test(df.t[,1],df.t[,2]))
  # power analysis
  dt.temporal.2weeks <- as.vector(unlist((df.t[,1])))
  dt.temporal.6months <- as.vector(unlist((df.t[,2])))
  pwr.t<-pwrss.t.2means(verbose=FALSE,mu1 = mean(dt.temporal.2weeks,na.rm=T), mu2 = mean(dt.temporal.6months,na.rm=T), 
                        sd1 = sd(dt.temporal.2weeks,na.rm=T), sd2 = sd(dt.temporal.6months,na.rm=T), 
                        kappa = length(na.omit(dt.temporal.2weeks))/length(na.omit(dt.temporal.6months)), 
                        n2 = length(na.omit(dt.temporal.6months)), 
                        alpha = 0.05,
                        alternative = "not equal")# power analysis
  pwr.t<-cbind(as.tibble(pwr.t$parms),pwr.t$test,pwr.t$df,pwr.t$ncp,pwr.t$power,pwr.t$n[1],pwr.t$n[2])
  colnames(pwr.t)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                     "paired", "paired.r", "alpha", "margin", "alternative",
                     "verbose","test","df","ncp","power","n1","n2")
  
  pwr.t.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(dt.temporal.2weeks,na.rm=T), mu2 = mean(dt.temporal.6months,na.rm=T), 
                            sd1 = sd(dt.temporal.2weeks,na.rm=T), sd2 = sd(dt.temporal.6months,na.rm=T), 
                            kappa = length(na.omit(dt.temporal.2weeks))/length(na.omit(dt.temporal.6months)), 
                            power = .80, alpha = 0.05,
                            alternative = "not equal")
  pwr.t.inv<-cbind(as.tibble(pwr.t.inv$parms),pwr.t.inv$test,pwr.t.inv$df,pwr.t.inv$ncp,pwr.t.inv$power,pwr.t.inv$n[1],pwr.t.inv$n[2])
  colnames(pwr.t.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
  
  if(space_t=="Household"){
    DI_t<-labels_DIMs_hh[which(id_DIMs_hh==DIM_t)]
  }else if(space_t=="Neighbourhood"){
    DI_t<-labels_DIMs_neighbour[which(id_DIMs_neighbour==DIM_t)]
  }else if(space_t=="Region"){
    DI_t<-labels_DIMs_region[which(id_DIMs_region==DIM_t)]
  }
  
  if(tb.t.test.t$p.value>0.05 || pwr.t$power<0.80){
    Result.t<- paste0("The differences in importance scores for ",tolower(DI_t), " at the ",tolower(space_t), " level",
                      " at two weeks and six months after the event are found to be NOT statistically significant with p-value = ", format(tb.t.test.t$p.value,digits=4),
                      ", statistical power = ", format(pwr.t$power,digits=4),
                      ", and importance scores being ",format(tb.t.test.t$estimate1,digits=4), " and ", format(tb.t.test.t$estimate2,digits=4), ", respectively.",
                      " To have a power of 0.80, the required sample sizes for responses associated with two weeks and six months after the event are ",
                      pwr.t.inv$n1[1], " and ",pwr.t.inv$n2[1], ", respectively.")
  }else{
    Result.t<- paste0("The differences in importance scores for ",tolower(DI_t), " at the ",tolower(space_t), " level",
                      " at two weeks and six months after the event are found to be statistically significant with p-value = ", format(tb.t.test.t$p.value,digits=4),
                      ", statistical power = ", format(pwr.t$power,digits=4),
                      ", and importance scores being ",format(tb.t.test.t$estimate1,digits=4), " and ", format(tb.t.test.t$estimate2,digits=4), ", respectively.")
  }
  # ----------------Stakeholder category comparison----------------
  # Compare importance scores across different stakeholder categories for a specific DIM
  # --------------------------------------------------
  
  switch(t_g, `two weeks`={ handle_g <- 1}, `six months`={handle_g <- 2}, `over six months`={handle_g <- 4}, `not applicable`={handle_g<-NA})
  if (DI_g %in% idx_DIMs_all){
    # extract results associated with the specified disaster impact (DI_g) at specified spatial scale (space_g) and temporal instance (t_g)
    res_g<-c()
    oth_g<-c()
    # extract the data for household, neighbourhood, and region (if applies)
    if (space_g=="Household"){
      id_g_res<-which(id_DIMs_hh[which(DI_g == gsub("\\..*","",id_DIMs_hh))]==colnames(Dt_Res.short))
      id_g_oth<-which(id_DIMs_hh[which(DI_g == gsub("\\..*","",id_DIMs_hh))]==colnames(Dt_Oth.short))
      res_g<-as.integer(unlist(Dt_Res.short[,id_g_res])[seq(from = handle_g,to=250,by=5)])
      oth_g<-as.integer(unlist(Dt_Oth.short[,id_g_oth])[seq(from = handle_g,to=240,by=6)])
    }
    if (space_g=="Neighbourhood"){
      id_g_res<-which(id_DIMs_neighbour[which(DI_g == gsub("\\..*","",id_DIMs_neighbour))]==colnames(Dt_Res.short))
      id_g_oth<-which(id_DIMs_neighbour[which(DI_g == gsub("\\..*","",id_DIMs_neighbour))]==colnames(Dt_Oth.short))
      res_g<-as.integer(unlist(Dt_Res.short[,id_g_res])[seq(from = handle_g,to=250,by=5)])
      oth_g<-as.integer(unlist(Dt_Oth.short[,id_g_oth])[seq(from = handle_g,to=240,by=6)])
    }
    if (space_g=="Region"){
      id_g_res<-which(id_DIMs_region[which(DI_g == gsub("\\..*","",id_DIMs_region))]==colnames(Dt_Res.short))
      id_g_oth<-which(id_DIMs_region[which(DI_g == gsub("\\..*","",id_DIMs_region))]==colnames(Dt_Oth.short))
      res_g<-as.integer(unlist(Dt_Res.short[,id_g_res])[seq(from = handle_g,to=250,by=5)])
      oth_g<-as.integer(unlist(Dt_Oth.short[,id_g_oth])[seq(from = handle_g,to=240,by=6)])
    }
    # ad-hoc fix for out-of-bound responses
    res_g[res_g>5]<-5
    oth_g[oth_g>5]<-5
    # revised definition of importance scores
    # S_DIM_i values: -1, 0, 1, 2, 3, 4. 
    res_g <- res_g -1
    oth_g <- oth_g -1
    DIM_g<-labels_DIMs_all[which(idx_DIMs_all==DI_g)]
  }
  if (DI_g %in% c("B31","B32","B33","B38","B39","B40","B62","B63","B64","B65","B66")){
    # t_g arg will be ignore in this case as these disaster impacts are associated with only one temporal instances
    id_res_g <- which(DI_g==colnames(Dt_Res.short))
    id_oth_g <- which(DI_g==colnames(Dt_Oth.short))
    if (space_g=="Household"){
      res_g <- as.integer(unlist(Dt_Res.short[,id_res_g])[seq(from = 1,to=250,by=5)])
      oth_g <- as.integer(unlist(Dt_Oth.short[,id_oth_g])[seq(from = 1,to=240,by=6)])
    }
    if (space_g=="Neighbourhood"){
      res_g <- as.integer(unlist(Dt_Res.short[,id_res_g])[seq(from = 2,to=250,by=5)])
      oth_g <- as.integer(unlist(Dt_Oth.short[,id_oth_g])[seq(from = 2,to=240,by=6)])
    }
    if (space_g=="Region"){
      res_g <- as.integer(unlist(Dt_Res.short[,id_res_g])[seq(from = 3,to=250,by=5)])
      oth_g <- as.integer(unlist(Dt_Oth.short[,id_oth_g])[seq(from = 3,to=240,by=6)])
    }
    # ad-hoc fix for out-of-bound responses
    res_g[res_g>5]<-5
    oth_g[oth_g>5]<-5
    # revised definition of importance scores
    # S_DIM_i values: -1, 0, 1, 2, 3, 4. 
    res_g <- res_g -1
    oth_g <- oth_g -1
    DIM_g<-labels_DIMs_spatial[which(id_DIMs_spatial==DI_g)]
  }
  tb.t.test.g<-tidy(t.test(res_g,oth_g))
  # power analysis
  pwr.g<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res_g,na.rm=T), mu2 = mean(oth_g,na.rm=T), 
                        sd1 = sd(res_g,na.rm=T), sd2 = sd(oth_g,na.rm=T), 
                        kappa = length(na.omit(res_g))/length(na.omit(oth_g)), 
                        n2 = length(na.omit(oth_g)), 
                        alpha = 0.05,
                        alternative = "not equal")# power analysis
  pwr.g<-cbind(as.tibble(pwr.g$parms),pwr.g$test,pwr.g$df,pwr.g$ncp,pwr.g$power,pwr.g$n[1],pwr.g$n[2])
  colnames(pwr.g)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                     "paired", "paired.r", "alpha", "margin", "alternative",
                     "verbose","test","df","ncp","power","n1","n2")
  
  pwr.g.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res_g,na.rm=T), mu2 = mean(oth_g,na.rm=T), 
                            sd1 = sd(res_g,na.rm=T), sd2 = sd(oth_g,na.rm=T), 
                            kappa = length(na.omit(res_g))/length(na.omit(oth_g)), 
                            power = .80, alpha = 0.05,
                            alternative = "not equal")
  pwr.g.inv<-cbind(as.tibble(pwr.g.inv$parms),pwr.g.inv$test,pwr.g.inv$df,pwr.g.inv$ncp,pwr.g.inv$power,pwr.g.inv$n[1],pwr.g.inv$n[2])
  colnames(pwr.g.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
  
  if(tb.t.test.g$p.value>0.05 || pwr.g$power<0.80){
    Result.g<- paste0("The differences in importance scores associated with residents and other stakeholders, respectively, for ",tolower(DIM_g), " at the ",tolower(space_g), " level ",
                      t_g," after the event are found to be NOT statistically significant with p-value = ", format(tb.t.test.g$p.value,digits=4),
                      ", statistical power = ", format(pwr.g$power,digits=4),
                      ", and importance scores being ",format(tb.t.test.g$estimate1,digits=4), " and ", format(tb.t.test.g$estimate2,digits=4), ", respectively.",
                      " To have a power of 0.80, the required sample sizes for responses of residents and other stakeholders are ",
                      pwr.g.inv$n1[1], " and ",pwr.g.inv$n2[1], ", respectively.")
  }else{
    Result.g<- paste0("The differences in importance scores associated with residents and other stakeholders, respectively, for ",tolower(DIM_g), " at the ",tolower(space_g), " level ",
                      t_g," after the event are found to be statistically significant with p-value = ", format(tb.t.test.g$p.value,digits=4),
                      ", statistical power = ", format(pwr.g$power,digits=4),
                      ", and importance scores being ",format(tb.t.test.g$estimate1,digits=4), " and ", format(tb.t.test.g$estimate2,digits=4), ", respectively.")
  }
  
  return(list(
    #results for spatial comparison
    tibble(`Result_ANOVA`=Result.s.welch.anova),
    tibble(`Result_t_test`=Result.s),
    #results for temporal comparison
    tibble(`Result`=Result.t),
    #results for stakeholder comparison
    tibble(`Result`=Result.g)
  )
  )
}