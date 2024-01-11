# Created by Chenbo Wang on 18 Dec 2023

# Performs Welch ANOVA, ANOVA, Welch t-test across spatial (Household, Neighbourhood, and Region) questions 
# and for each resolution between residents and beneficiaries
# and shows the bar plots for each spatial resolution

# Applies to questions that have three spatial resolutions
# I.e., B31, B32, B33, B38, B39, B40, B62, B63, B64, B65, and B66

# Args:
# A: question to analyse the responses for
# which_group: "Resident", "Beneficiary", or "Both"
# "res" - residents only, "ben" - beneficiaries only, "both" - both groups
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
CompareDIMs <- function(DI, which_group, t, A, B, how_jitter){
  load("clean_Data.RData")
  switch(t, `2 weeks`={ handle <- 1}, `6 months`={handle <- 2}, `over 6 months`={handle <- 4})
  # DIMs that are not spatial questions per se
  if (DI %in% idx_DIMs_all){
    res_hh<-c()
    ben_hh<-c()
    res_neighbour<-c()
    ben_neighbour<-c()
    res_region<-c()
    ben_region<-c()
    # extract the data for household, neighbourhood, and region (if applies)
    if (DI %in% gsub("\\..*","",id_DIMs_hh)){
      id_hh_res<-which(id_DIMs_hh[which(DI == gsub("\\..*","",id_DIMs_hh))]==colnames(Dt_Res.short))
      id_hh_ben<-which(id_DIMs_hh[which(DI == gsub("\\..*","",id_DIMs_hh))]==colnames(Dt_Ben.short))
      res_hh<-as.integer(unlist(Dt_Res.short[,id_hh_res])[seq(from = handle,to=250,by=5)])
      ben_hh<-as.integer(unlist(Dt_Ben.short[,id_hh_ben])[seq(from = handle,to=228,by=6)])
    }
    if (DI %in% gsub("\\..*","",id_DIMs_neighbour)){
      id_neighbour_res<-which(id_DIMs_neighbour[which(DI == gsub("\\..*","",id_DIMs_neighbour))]==colnames(Dt_Res.short))
      id_neighbour_ben<-which(id_DIMs_neighbour[which(DI == gsub("\\..*","",id_DIMs_neighbour))]==colnames(Dt_Ben.short))
      res_neighbour<-as.integer(unlist(Dt_Res.short[,id_neighbour_res])[seq(from = handle,to=250,by=5)])
      ben_neighbour<-as.integer(unlist(Dt_Ben.short[,id_neighbour_ben])[seq(from = handle,to=228,by=6)])
    }
    if (DI %in% gsub("\\..*","",id_DIMs_region)){
      id_region_res<-which(id_DIMs_region[which(DI == gsub("\\..*","",id_DIMs_region))]==colnames(Dt_Res.short))
      id_region_ben<-which(id_DIMs_region[which(DI == gsub("\\..*","",id_DIMs_region))]==colnames(Dt_Ben.short))
      res_region<-as.integer(unlist(Dt_Res.short[,id_region_res])[seq(from = handle,to=250,by=5)])
      ben_region<-as.integer(unlist(Dt_Ben.short[,id_region_ben])[seq(from = handle,to=228,by=6)])
    }
    # ad-hoc fix for out-of-bound responses
    res_hh[res_hh>5]<-5
    ben_hh[ben_hh>5]<-5
    res_neighbour[res_neighbour>5]<-5
    ben_neighbour[ben_neighbour>5]<-5
    res_region[res_region>5]<-5
    ben_region[ben_region>5]<-5
    
    res_hh <- res_hh -1
    res_neighbour <- res_neighbour -1
    res_region <- res_region -1
    ben_hh <- ben_hh -1
    ben_neighbour <- ben_neighbour -1
    ben_region <- ben_region -1
    df.plot.DI.full<-tibble(
      score=c(res_hh,ben_hh,res_neighbour,ben_neighbour,res_region,ben_region),
      space=c(rep("Household",length(c(res_hh,ben_hh))),
              rep("Neighbourhood",length(c(res_neighbour,ben_neighbour))),
              rep("Region",length(c(res_region,ben_region))))
      )
    df.res<-tibble(
      score=c(res_hh,res_neighbour,res_region),
      space=c(rep("Household",length(res_hh)),
              rep("Neighbourhood",length(res_neighbour)),
              rep("Region",length(res_region)))
      )
    df.ben<-tibble(
      score=c(ben_hh,ben_neighbour,ben_region),
      space=c(rep("Household",length(ben_hh)),
              rep("Neighbourhood",length(ben_neighbour)),
              rep("Region",length(ben_region)))
    )
    switch(which_group, 
           Resident={
             df.plot.DI<-df.res
           },
           Beneficiary={
             df.plot.DI<-df.ben
           },
           Both={
             df.plot.DI<-df.plot.DI.full
           })
    DIM_DI<-labels_DIMs_all[which(idx_DIMs_all==DI)]
    plot.DI<-ggplot(na.omit(df.plot.DI), aes(x=score,fill=as.factor(space))) +
      geom_histogram(
        color="black",
        binwidth = 0.5,
        alpha = 0.2,
        linetype="solid",
        position = position_dodge(width = 0.5)) +
      theme(axis.text.x = element_text(face=NULL, color="black", size = 16, angle=0),
            axis.text.y = element_text(face=NULL, color="black", size = 16, angle=0),
            axis.title = element_text(size = 16)) + 
      scale_x_continuous(breaks=0:5,
                         labels=c("0","1","2","3","4","5"))+
      labs(x = paste0('Importance score for ',DI,'\n',DI,": ",DIM_DI), title = which_group) +
      theme_grey(base_size = 16) +
      scale_fill_discrete(name = "Spatial resolution")
    # print(plot.DI)
    # Welch ANOVA 
    Welch.aov <- oneway.test(score~space,data = na.omit(df.plot.DI),var.equal = F)
    # print(Welch.aov) #assume unequal variances
    
    df.Welch.aov.DI<-cbind(as.tibble(Welch.aov$statistic[1]),Welch.aov$parameter[1],Welch.aov$parameter[2],Welch.aov$p.value,Welch.aov$method)
    colnames(df.Welch.aov.DI)<-c("F","num df","denom df","p.value","method")
    df.Welch.aov.DI<-remove_rownames(df.Welch.aov.DI)
    # ANOVA
    res.aov.DI <- aov(score~space,data = na.omit(df.plot.DI)) 
    # print(summary(res.aov.DI))
    # print(report(res.aov.DI)) 
    if("Household" %in% df.plot.DI$space){
      tb.t.test.DI<-rbind(tidy(t.test(df.plot.DI$score[df.plot.DI$space == "Household"],df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])),
                       tidy(t.test(df.plot.DI$score[df.plot.DI$space == "Household"],df.plot.DI$score[df.plot.DI$space == "Region"])),
                       tidy(t.test(df.plot.DI$score[df.plot.DI$space == "Region"],df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])),
                       tidy(t.test(df.res$score[df.res$space == "Household"],df.ben$score[df.ben$space == "Household"])),
                       tidy(t.test(df.res$score[df.res$space == "Neighbourhood"],df.ben$score[df.ben$space == "Neighbourhood"])),
                       tidy(t.test(df.res$score[df.res$space == "Region"],df.ben$score[df.ben$space == "Region"])))
      tb.t.test.DI$which.test<-c("specified group(s): household vs neighbourhood",
                              "specified group(s): household vs region",
                              "specified group(s): region vs neighbourhood",
                              "household: resident vs beneficiary",
                              "neighbourhood: resident vs beneficiary",
                              "region: resident vs beneficiary")
      #power analysis
      # "specified group(s): household vs neighbourhood"
      pwr.DI.1<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                            mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                            sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                            sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                            kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Household"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                            n2 = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.1<-cbind(as.tibble(pwr.DI.1$parms),pwr.DI.1$test,pwr.DI.1$df,pwr.DI.1$ncp,pwr.DI.1$power,pwr.DI.1$n[1],pwr.DI.1$n[2])
      colnames(pwr.DI.1)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.1.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                                mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                                sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                                sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                                kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Household"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.1.inv<-cbind(as.tibble(pwr.DI.1.inv$parms),pwr.DI.1.inv$test,pwr.DI.1.inv$df,pwr.DI.1.inv$ncp,pwr.DI.1.inv$power,pwr.DI.1.inv$n[1],pwr.DI.1.inv$n[2])
      colnames(pwr.DI.1.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      # "specified group(s): household vs region"
      pwr.DI.2<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                            mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                            sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                            sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                            kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Household"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"])), 
                            n2 = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.2<-cbind(as.tibble(pwr.DI.2$parms),pwr.DI.2$test,pwr.DI.2$df,pwr.DI.2$ncp,pwr.DI.2$power,pwr.DI.2$n[1],pwr.DI.2$n[2])
      colnames(pwr.DI.2)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.2.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                                mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                                sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                                sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                                kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Household"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.2.inv<-cbind(as.tibble(pwr.DI.2.inv$parms),pwr.DI.2.inv$test,pwr.DI.2.inv$df,pwr.DI.2.inv$ncp,pwr.DI.2.inv$power,pwr.DI.2.inv$n[1],pwr.DI.2.inv$n[2])
      colnames(pwr.DI.2.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      
      # "specified group(s): region vs neighbourhood"
      pwr.DI.3<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "region"],na.rm=T), 
                            mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                            sd1 = sd(df.plot.DI$score[df.plot.DI$space == "region"],na.rm=T), 
                            sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                            kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "region"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                            n2 = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.3<-cbind(as.tibble(pwr.DI.3$parms),pwr.DI.3$test,pwr.DI.3$df,pwr.DI.3$ncp,pwr.DI.3$power,pwr.DI.3$n[1],pwr.DI.3$n[2])
      colnames(pwr.DI.3)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.3.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                                mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                                sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                                sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                                kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.3.inv<-cbind(as.tibble(pwr.DI.3.inv$parms),pwr.DI.3.inv$test,pwr.DI.3.inv$df,pwr.DI.3.inv$ncp,pwr.DI.3.inv$power,pwr.DI.3.inv$n[1],pwr.DI.3.inv$n[2])
      colnames(pwr.DI.3.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      # "household: resident vs beneficiary"
      pwr.DI.4<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Household"],na.rm=T), 
                            mu2 = mean(df.ben$score[df.ben$space == "Household"],na.rm=T), 
                            sd1 = sd(df.res$score[df.res$space == "Household"],na.rm=T), 
                            sd2 = sd(df.ben$score[df.ben$space == "Household"],na.rm=T), 
                            kappa = length(na.omit(df.res$score[df.res$space == "Household"]))/length(na.omit(df.ben$score[df.ben$space == "Household"])), 
                            n2 = length(na.omit(df.ben$score[df.ben$space == "Household"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.4<-cbind(as.tibble(pwr.DI.4$parms),pwr.DI.4$test,pwr.DI.4$df,pwr.DI.4$ncp,pwr.DI.4$power,pwr.DI.4$n[1],pwr.DI.4$n[2])
      colnames(pwr.DI.4)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.4.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Household"],na.rm=T), 
                                mu2 = mean(df.ben$score[df.ben$space == "Household"],na.rm=T), 
                                sd1 = sd(df.res$score[df.res$space == "Household"],na.rm=T), 
                                sd2 = sd(df.ben$score[df.ben$space == "Household"],na.rm=T), 
                                kappa = length(na.omit(df.res$score[df.res$space == "Household"]))/length(na.omit(df.ben$score[df.ben$space == "Household"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.4.inv<-cbind(as.tibble(pwr.DI.4.inv$parms),pwr.DI.4.inv$test,pwr.DI.4.inv$df,pwr.DI.4.inv$ncp,pwr.DI.4.inv$power,pwr.DI.4.inv$n[1],pwr.DI.4.inv$n[2])
      colnames(pwr.DI.4.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      # "neighbourhood: resident vs beneficiary"
      pwr.DI.5<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                            mu2 = mean(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                            sd1 = sd(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                            sd2 = sd(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                            kappa = length(na.omit(df.res$score[df.res$space == "Neighbourhood"]))/length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), 
                            n2 = length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.5<-cbind(as.tibble(pwr.DI.5$parms),pwr.DI.5$test,pwr.DI.5$df,pwr.DI.5$ncp,pwr.DI.5$power,pwr.DI.5$n[1],pwr.DI.5$n[2])
      colnames(pwr.DI.5)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.5.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                                mu2 = mean(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                                sd1 = sd(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                                sd2 = sd(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                                kappa = length(na.omit(df.res$score[df.res$space == "Neighbourhood"]))/length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.5.inv<-cbind(as.tibble(pwr.DI.5.inv$parms),pwr.DI.5.inv$test,pwr.DI.5.inv$df,pwr.DI.5.inv$ncp,pwr.DI.5.inv$power,pwr.DI.5.inv$n[1],pwr.DI.5.inv$n[2])
      colnames(pwr.DI.5.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      # "region: resident vs beneficiary"
      pwr.DI.6<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Region"],na.rm=T), 
                            mu2 = mean(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                            sd1 = sd(df.res$score[df.res$space == "Region"],na.rm=T), 
                            sd2 = sd(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                            kappa = length(na.omit(df.res$score[df.res$space == "Region"]))/length(na.omit(df.ben$score[df.ben$space == "Region"])), 
                            n2 = length(na.omit(df.ben$score[df.ben$space == "Region"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.6<-cbind(as.tibble(pwr.DI.6$parms),pwr.DI.6$test,pwr.DI.6$df,pwr.DI.6$ncp,pwr.DI.6$power,pwr.DI.6$n[1],pwr.DI.6$n[2])
      colnames(pwr.DI.6)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.6.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Region"],na.rm=T), 
                                mu2 = mean(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                                sd1 = sd(df.res$score[df.res$space == "Region"],na.rm=T), 
                                sd2 = sd(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                                kappa = length(na.omit(df.res$score[df.res$space == "Region"]))/length(na.omit(df.ben$score[df.ben$space == "Region"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.6.inv<-cbind(as.tibble(pwr.DI.6.inv$parms),pwr.DI.6.inv$test,pwr.DI.6.inv$df,pwr.DI.6.inv$ncp,pwr.DI.6.inv$power,pwr.DI.6.inv$n[1],pwr.DI.6.inv$n[2])
      colnames(pwr.DI.6.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      pwr.DI <- rbind(pwr.DI.1,pwr.DI.2,pwr.DI.3,pwr.DI.4,pwr.DI.5,pwr.DI.5)
      pwr.DI.inv <- rbind(pwr.DI.1.inv,pwr.DI.2.inv,pwr.DI.3.inv,pwr.DI.4.inv,pwr.DI.5.inv,pwr.DI.6.inv)
      rownames(pwr.DI)<-1:6
      rownames(pwr.DI.inv)<-1:6
      pwr.DI$which.test<-c("specified group(s): household vs neighbourhood",
                        "specified group(s): household vs region",
                        "specified group(s): region vs neighbourhood",
                        "household: resident vs beneficiary",
                        "neighbourhood: resident vs beneficiary",
                        "region: resident vs beneficiary")
      pwr.DI.inv$which.test<-c("specified group(s): household vs neighbourhood",
                            "specified group(s): household vs region",
                            "specified group(s): region vs neighbourhood",
                            "household: resident vs beneficiary",
                            "neighbourhood: resident vs beneficiary",
                            "region: resident vs beneficiary")
    }else{
      tb.t.test.DI<-rbind(tidy(t.test(df.plot.DI$score[df.plot.DI$space == "Region"],df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])),
                       tidy(t.test(df.res$score[df.res$space == "Neighbourhood"],df.ben$score[df.ben$space == "Neighbourhood"])),
                       tidy(t.test(df.res$score[df.res$space == "Region"],df.ben$score[df.ben$space == "Region"])))
      tb.t.test.DI$which.test<-c("specified group(s): region vs neighbourhood",
                              "neighbourhood: resident vs beneficiary",
                              "region: resident vs beneficiary")
      # Power analysis
      # "specified group(s): region vs neighbourhood"
      pwr.DI.3<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "region"],na.rm=T), 
                            mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                            sd1 = sd(df.plot.DI$score[df.plot.DI$space == "region"],na.rm=T), 
                            sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                            kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "region"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                            n2 = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.3<-cbind(as.tibble(pwr.DI.3$parms),pwr.DI.3$test,pwr.DI.3$df,pwr.DI.3$ncp,pwr.DI.3$power,pwr.DI.3$n[1],pwr.DI.3$n[2])
      colnames(pwr.DI.3)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.3.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                                mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                                sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                                sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                                kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.3.inv<-cbind(as.tibble(pwr.DI.3.inv$parms),pwr.DI.3.inv$test,pwr.DI.3.inv$df,pwr.DI.3.inv$ncp,pwr.DI.3.inv$power,pwr.DI.3.inv$n[1],pwr.DI.3.inv$n[2])
      colnames(pwr.DI.3.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      # "neighbourhood: resident vs beneficiary"
      pwr.DI.5<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                            mu2 = mean(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                            sd1 = sd(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                            sd2 = sd(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                            kappa = length(na.omit(df.res$score[df.res$space == "Neighbourhood"]))/length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), 
                            n2 = length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.5<-cbind(as.tibble(pwr.DI.5$parms),pwr.DI.5$test,pwr.DI.5$df,pwr.DI.5$ncp,pwr.DI.5$power,pwr.DI.5$n[1],pwr.DI.5$n[2])
      colnames(pwr.DI.5)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.5.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                                mu2 = mean(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                                sd1 = sd(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                                sd2 = sd(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                                kappa = length(na.omit(df.res$score[df.res$space == "Neighbourhood"]))/length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.5.inv<-cbind(as.tibble(pwr.DI.5.inv$parms),pwr.DI.5.inv$test,pwr.DI.5.inv$df,pwr.DI.5.inv$ncp,pwr.DI.5.inv$power,pwr.DI.5.inv$n[1],pwr.DI.5.inv$n[2])
      colnames(pwr.DI.5.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      # "region: resident vs beneficiary"
      pwr.DI.6<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Region"],na.rm=T), 
                            mu2 = mean(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                            sd1 = sd(df.res$score[df.res$space == "Region"],na.rm=T), 
                            sd2 = sd(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                            kappa = length(na.omit(df.res$score[df.res$space == "Region"]))/length(na.omit(df.ben$score[df.ben$space == "Region"])), 
                            n2 = length(na.omit(df.ben$score[df.ben$space == "Region"])), alpha = 0.05,
                            alternative = "not equal")# power analysis
      pwr.DI.6<-cbind(as.tibble(pwr.DI.6$parms),pwr.DI.6$test,pwr.DI.6$df,pwr.DI.6$ncp,pwr.DI.6$power,pwr.DI.6$n[1],pwr.DI.6$n[2])
      colnames(pwr.DI.6)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
      pwr.DI.6.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Region"],na.rm=T), 
                                mu2 = mean(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                                sd1 = sd(df.res$score[df.res$space == "Region"],na.rm=T), 
                                sd2 = sd(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                                kappa = length(na.omit(df.res$score[df.res$space == "Region"]))/length(na.omit(df.ben$score[df.ben$space == "Region"])), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
      pwr.DI.6.inv<-cbind(as.tibble(pwr.DI.6.inv$parms),pwr.DI.6.inv$test,pwr.DI.6.inv$df,pwr.DI.6.inv$ncp,pwr.DI.6.inv$power,pwr.DI.6.inv$n[1],pwr.DI.6.inv$n[2])
      colnames(pwr.DI.6.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
      pwr.DI <- rbind(pwr.DI.3,pwr.DI.5,pwr.DI.6)
      pwr.DI.inv <- rbind(pwr.DI.3.inv,pwr.DI.5.inv,pwr.DI.6.inv)
      rownames(pwr.DI)<-1:3
      rownames(pwr.DI.inv)<-1:3
      pwr.DI$which.test<-c("specified group(s): region vs neighbourhood",
                        "neighbourhood: resident vs beneficiary",
                        "region: resident vs beneficiary")
      pwr.DI.inv$which.test<-c("specified group(s): region vs neighbourhood",
                            "neighbourhood: resident vs beneficiary",
                            "region: resident vs beneficiary")
    }
    tb.t.test.DI <- tb.t.test.DI %>%
      select(which.test, everything())
    pwr.DI <- pwr.DI %>%
      select(which.test, everything())
    pwr.DI.inv <- pwr.DI.inv %>%
      select(which.test, everything())
  }
  if (DI %in% c("B31","B32","B33","B38","B39","B40","B62","B63","B64","B65","B66")){
    id_res.DI <- which(DI==colnames(Dt_Res.short))
    id_ben.DI <- which(DI==colnames(Dt_Ben.short))
    res_hh <- as.integer(unlist(Dt_Res.short[,id_res.DI])[seq(from = 1,to=250,by=5)])
    res_neighbour <- as.integer(unlist(Dt_Res.short[,id_res.DI])[seq(from = 2,to=250,by=5)])
    res_region <- as.integer(unlist(Dt_Res.short[,id_res.DI])[seq(from = 3,to=250,by=5)])
    ben_hh <- as.integer(unlist(Dt_Ben.short[,id_ben.DI])[seq(from = 1,to=228,by=6)])
    ben_neighbour <- as.integer(unlist(Dt_Ben.short[,id_ben.DI])[seq(from = 2,to=228,by=6)])
    ben_region <- as.integer(unlist(Dt_Ben.short[,id_ben.DI])[seq(from = 3,to=228,by=6)])
    # ad-hoc fix for out-of-bound responses
    res_hh[res_hh>5]<-5
    ben_hh[ben_hh>5]<-5
    res_neighbour[res_neighbour>5]<-5
    ben_neighbour[ben_neighbour>5]<-5
    res_region[res_region>5]<-5
    ben_region[ben_region>5]<-5
    
    res_hh <- res_hh -1
    res_neighbour <- res_neighbour -1
    res_region <- res_region -1
    ben_hh <- ben_hh -1
    ben_neighbour <- ben_neighbour -1
    ben_region <- ben_region -1
    
    df.plot.DI.full<-tibble(
      score=c(res_hh,ben_hh,res_neighbour,ben_neighbour,res_region,ben_region),
      space=c(rep("Household",88),rep("Neighbourhood",88),rep("Region",88)))
    df.res<-df.plot.DI.full[c(1:50,89:138,177:226),]
    df.ben<-df.plot.DI.full[-c(1:50,89:138,177:226),]
    switch(which_group, 
           Resident={
             df.plot.DI<-tibble(
               score=c(res_hh,res_neighbour,res_region),
               space=c(rep("Household",50),rep("Neighbourhood",50),rep("Region",50)))
           },
           Beneficiary={
             df.plot.DI<-tibble(
               score=c(ben_hh,ben_neighbour,ben_region),
               space=c(rep("Household",38),rep("Neighbourhood",38),rep("Region",38)))
           },
           Both={
             df.plot.DI<-df.plot.DI.full
           })
    DIM_DI<-labels_DIMs_spatial[which(id_DIMs_spatial==DI)]
    plot.DI<-ggplot(na.omit(df.plot.DI), aes(x=score,fill=as.factor(space))) +
      geom_histogram(
        color="black",
        binwidth = 0.5,
        alpha = 0.2,
        linetype="solid",
        position = position_dodge(width = 0.5)) +
      theme(axis.text.x = element_text(face=NULL, color="black", size = 16, angle=0),
            axis.text.y = element_text(face=NULL, color="black", size = 16, angle=0),
            axis.title = element_text(size = 16)) + 
      scale_x_continuous(breaks=0:5,
                         labels=c("0","1","2","3","4","5"))+
      labs(x = paste0('Importance score for ',DI,'\n',DI,": ",DIM_DI), title = which_group) +
      theme_grey(base_size = 16) +
      scale_fill_discrete(name = "Spatial resolution")
    # print(plot.DI)
    # Welch ANOVA 
    Welch.aov <- oneway.test(score~space,data = na.omit(df.plot.DI),var.equal = F)
    # print(Welch.aov) #assume unequal variances
    df.Welch.aov.DI<-cbind(as.tibble(Welch.aov$statistic[1]),Welch.aov$parameter[1],Welch.aov$parameter[2],Welch.aov$p.value,Welch.aov$method)
    colnames(df.Welch.aov.DI)<-c("F","num df","denom df","p.value","method")
    df.Welch.aov.DI<-remove_rownames(df.Welch.aov.DI)
    # ANOVA
    res.aov.DI <- aov(score~space,data = na.omit(df.plot.DI)) 
    # print(summary(res.aov.DI))
    # print(report(res.aov.DI))
    # # Welch t-test across three spatial resolution
    # print(t.test(df.plot.DI$score[df.plot.DI$space == "Household"],df.plot.DI$score[df.plot.DI$space == "Neighbourhood"]))
    # print(t.test(df.plot.DI$score[df.plot.DI$space == "Household"],df.plot.DI$score[df.plot.DI$space == "Region"]))
    # print(t.test(df.plot.DI$score[df.plot.DI$space == "Region"],df.plot.DI$score[df.plot.DI$space == "Neighbourhood"]))
    # #Compare residents with beneficiaries
    # print(t.test(df.res$score[df.res$space == "Household"],df.ben$score[df.ben$space == "Household"]))
    # print(t.test(df.res$score[df.res$space == "Neighbourhood"],df.ben$score[df.ben$space == "Neighbourhood"]))
    # print(t.test(df.res$score[df.res$space == "Region"],df.ben$score[df.ben$space == "Region"]))
    tb.t.test.DI<-rbind(tidy(t.test(df.plot.DI$score[df.plot.DI$space == "Household"],df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), # Welch t-test across three spatial resolution
                     tidy(t.test(df.plot.DI$score[df.plot.DI$space == "Household"],df.plot.DI$score[df.plot.DI$space == "Region"])),
                     tidy(t.test(df.plot.DI$score[df.plot.DI$space == "Region"],df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])),
                     tidy(t.test(df.res$score[df.res$space == "Household"],df.ben$score[df.ben$space == "Household"])), #Compare residents with beneficiaries
                     tidy(t.test(df.res$score[df.res$space == "Neighbourhood"],df.ben$score[df.ben$space == "Neighbourhood"])),
                     tidy(t.test(df.res$score[df.res$space == "Region"],df.ben$score[df.ben$space == "Region"])))
    tb.t.test.DI$which.test<-c("specified group(s): household vs neighbourhood",
                            "specified group(s): household vs region",
                            "specified group(s): region vs neighbourhood",
                            "household: resident vs beneficiary",
                            "neighbourhood: resident vs beneficiary",
                            "region: resident vs beneficiary")
    #power analysis
    # "specified group(s): household vs neighbourhood"
    pwr.DI.1<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                          mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                          sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                          sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                          kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Household"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                          n2 = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), alpha = 0.05,
                          alternative = "not equal")# power analysis
    pwr.DI.1<-cbind(as.tibble(pwr.DI.1$parms),pwr.DI.1$test,pwr.DI.1$df,pwr.DI.1$ncp,pwr.DI.1$power,pwr.DI.1$n[1],pwr.DI.1$n[2])
    colnames(pwr.DI.1)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                       "paired", "paired.r", "alpha", "margin", "alternative",
                       "verbose","test","df","ncp","power","n1","n2")
    pwr.DI.1.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                              mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                              sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                              sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                              kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Household"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                              power = .80, alpha = 0.05,
                              alternative = "not equal")
    pwr.DI.1.inv<-cbind(as.tibble(pwr.DI.1.inv$parms),pwr.DI.1.inv$test,pwr.DI.1.inv$df,pwr.DI.1.inv$ncp,pwr.DI.1.inv$power,pwr.DI.1.inv$n[1],pwr.DI.1.inv$n[2])
    colnames(pwr.DI.1.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
    # "specified group(s): household vs region"
    pwr.DI.2<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                          mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                          sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                          sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                          kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Household"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"])), 
                          n2 = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"])), alpha = 0.05,
                          alternative = "not equal")# power analysis
    pwr.DI.2<-cbind(as.tibble(pwr.DI.2$parms),pwr.DI.2$test,pwr.DI.2$df,pwr.DI.2$ncp,pwr.DI.2$power,pwr.DI.2$n[1],pwr.DI.2$n[2])
    colnames(pwr.DI.2)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                       "paired", "paired.r", "alpha", "margin", "alternative",
                       "verbose","test","df","ncp","power","n1","n2")
    pwr.DI.2.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                              mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                              sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Household"],na.rm=T), 
                              sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                              kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Household"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"])), 
                              power = .80, alpha = 0.05,
                              alternative = "not equal")
    pwr.DI.2.inv<-cbind(as.tibble(pwr.DI.2.inv$parms),pwr.DI.2.inv$test,pwr.DI.2.inv$df,pwr.DI.2.inv$ncp,pwr.DI.2.inv$power,pwr.DI.2.inv$n[1],pwr.DI.2.inv$n[2])
    colnames(pwr.DI.2.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
    
    # "specified group(s): region vs neighbourhood"
    pwr.DI.3<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "region"],na.rm=T), 
                          mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                          sd1 = sd(df.plot.DI$score[df.plot.DI$space == "region"],na.rm=T), 
                          sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                          kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "region"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                          n2 = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), alpha = 0.05,
                          alternative = "not equal")# power analysis
    pwr.DI.3<-cbind(as.tibble(pwr.DI.3$parms),pwr.DI.3$test,pwr.DI.3$df,pwr.DI.3$ncp,pwr.DI.3$power,pwr.DI.3$n[1],pwr.DI.3$n[2])
    colnames(pwr.DI.3)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                       "paired", "paired.r", "alpha", "margin", "alternative",
                       "verbose","test","df","ncp","power","n1","n2")
    pwr.DI.3.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                              mu2 = mean(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                              sd1 = sd(df.plot.DI$score[df.plot.DI$space == "Region"],na.rm=T), 
                              sd2 = sd(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"],na.rm=T), 
                              kappa = length(na.omit(df.plot.DI$score[df.plot.DI$space == "Region"]))/length(na.omit(df.plot.DI$score[df.plot.DI$space == "Neighbourhood"])), 
                              power = .80, alpha = 0.05,
                              alternative = "not equal")
    pwr.DI.3.inv<-cbind(as.tibble(pwr.DI.3.inv$parms),pwr.DI.3.inv$test,pwr.DI.3.inv$df,pwr.DI.3.inv$ncp,pwr.DI.3.inv$power,pwr.DI.3.inv$n[1],pwr.DI.3.inv$n[2])
    colnames(pwr.DI.3.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
    # "household: resident vs beneficiary"
    pwr.DI.4<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Household"],na.rm=T), 
                          mu2 = mean(df.ben$score[df.ben$space == "Household"],na.rm=T), 
                          sd1 = sd(df.res$score[df.res$space == "Household"],na.rm=T), 
                          sd2 = sd(df.ben$score[df.ben$space == "Household"],na.rm=T), 
                          kappa = length(na.omit(df.res$score[df.res$space == "Household"]))/length(na.omit(df.ben$score[df.ben$space == "Household"])), 
                          n2 = length(na.omit(df.ben$score[df.ben$space == "Household"])), alpha = 0.05,
                          alternative = "not equal")# power analysis
    pwr.DI.4<-cbind(as.tibble(pwr.DI.4$parms),pwr.DI.4$test,pwr.DI.4$df,pwr.DI.4$ncp,pwr.DI.4$power,pwr.DI.4$n[1],pwr.DI.4$n[2])
    colnames(pwr.DI.4)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                       "paired", "paired.r", "alpha", "margin", "alternative",
                       "verbose","test","df","ncp","power","n1","n2")
    pwr.DI.4.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Household"],na.rm=T), 
                              mu2 = mean(df.ben$score[df.ben$space == "Household"],na.rm=T), 
                              sd1 = sd(df.res$score[df.res$space == "Household"],na.rm=T), 
                              sd2 = sd(df.ben$score[df.ben$space == "Household"],na.rm=T), 
                              kappa = length(na.omit(df.res$score[df.res$space == "Household"]))/length(na.omit(df.ben$score[df.ben$space == "Household"])), 
                              power = .80, alpha = 0.05,
                              alternative = "not equal")
    pwr.DI.4.inv<-cbind(as.tibble(pwr.DI.4.inv$parms),pwr.DI.4.inv$test,pwr.DI.4.inv$df,pwr.DI.4.inv$ncp,pwr.DI.4.inv$power,pwr.DI.4.inv$n[1],pwr.DI.4.inv$n[2])
    colnames(pwr.DI.4.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
    # "neighbourhood: resident vs beneficiary"
    pwr.DI.5<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                          mu2 = mean(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                          sd1 = sd(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                          sd2 = sd(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                          kappa = length(na.omit(df.res$score[df.res$space == "Neighbourhood"]))/length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), 
                          n2 = length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), alpha = 0.05,
                          alternative = "not equal")# power analysis
    pwr.DI.5<-cbind(as.tibble(pwr.DI.5$parms),pwr.DI.5$test,pwr.DI.5$df,pwr.DI.5$ncp,pwr.DI.5$power,pwr.DI.5$n[1],pwr.DI.5$n[2])
    colnames(pwr.DI.5)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                       "paired", "paired.r", "alpha", "margin", "alternative",
                       "verbose","test","df","ncp","power","n1","n2")
    pwr.DI.5.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                              mu2 = mean(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                              sd1 = sd(df.res$score[df.res$space == "Neighbourhood"],na.rm=T), 
                              sd2 = sd(df.ben$score[df.ben$space == "Neighbourhood"],na.rm=T), 
                              kappa = length(na.omit(df.res$score[df.res$space == "Neighbourhood"]))/length(na.omit(df.ben$score[df.ben$space == "Neighbourhood"])), 
                              power = .80, alpha = 0.05,
                              alternative = "not equal")
    pwr.DI.5.inv<-cbind(as.tibble(pwr.DI.5.inv$parms),pwr.DI.5.inv$test,pwr.DI.5.inv$df,pwr.DI.5.inv$ncp,pwr.DI.5.inv$power,pwr.DI.5.inv$n[1],pwr.DI.5.inv$n[2])
    colnames(pwr.DI.5.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
    # "region: resident vs beneficiary"
    pwr.DI.6<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Region"],na.rm=T), 
                          mu2 = mean(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                          sd1 = sd(df.res$score[df.res$space == "Region"],na.rm=T), 
                          sd2 = sd(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                          kappa = length(na.omit(df.res$score[df.res$space == "Region"]))/length(na.omit(df.ben$score[df.ben$space == "Region"])), 
                          n2 = length(na.omit(df.ben$score[df.ben$space == "Region"])), alpha = 0.05,
                          alternative = "not equal")# power analysis
    pwr.DI.6<-cbind(as.tibble(pwr.DI.6$parms),pwr.DI.6$test,pwr.DI.6$df,pwr.DI.6$ncp,pwr.DI.6$power,pwr.DI.6$n[1],pwr.DI.6$n[2])
    colnames(pwr.DI.6)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                       "paired", "paired.r", "alpha", "margin", "alternative",
                       "verbose","test","df","ncp","power","n1","n2")
    pwr.DI.6.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(df.res$score[df.res$space == "Region"],na.rm=T), 
                              mu2 = mean(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                              sd1 = sd(df.res$score[df.res$space == "Region"],na.rm=T), 
                              sd2 = sd(df.ben$score[df.ben$space == "Region"],na.rm=T), 
                              kappa = length(na.omit(df.res$score[df.res$space == "Region"]))/length(na.omit(df.ben$score[df.ben$space == "Region"])), 
                              power = .80, alpha = 0.05,
                              alternative = "not equal")
    pwr.DI.6.inv<-cbind(as.tibble(pwr.DI.6.inv$parms),pwr.DI.6.inv$test,pwr.DI.6.inv$df,pwr.DI.6.inv$ncp,pwr.DI.6.inv$power,pwr.DI.6.inv$n[1],pwr.DI.6.inv$n[2])
    colnames(pwr.DI.6.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                           "paired", "paired.r", "alpha", "margin", "alternative",
                           "verbose","test","df","ncp","power","n1","n2")
    pwr.DI <- rbind(pwr.DI.1,pwr.DI.2,pwr.DI.3,pwr.DI.4,pwr.DI.5,pwr.DI.5)
    pwr.DI.inv <- rbind(pwr.DI.1.inv,pwr.DI.2.inv,pwr.DI.3.inv,pwr.DI.4.inv,pwr.DI.5.inv,pwr.DI.6.inv)
    rownames(pwr.DI)<-1:6
    rownames(pwr.DI.inv)<-1:6
    pwr.DI$which.test<-c("specified group(s): household vs neighbourhood",
                      "specified group(s): household vs region",
                      "specified group(s): region vs neighbourhood",
                      "household: resident vs beneficiary",
                      "neighbourhood: resident vs beneficiary",
                      "region: resident vs beneficiary")
    pwr.DI.inv$which.test<-c("specified group(s): household vs neighbourhood",
                          "specified group(s): household vs region",
                          "specified group(s): region vs neighbourhood",
                          "household: resident vs beneficiary",
                          "neighbourhood: resident vs beneficiary",
                          "region: resident vs beneficiary")
    
    tb.t.test.DI <- tb.t.test.DI %>%
      select(which.test, everything())
    pwr.DI <- pwr.DI %>%
      select(which.test, everything())
    pwr.DI.inv <- pwr.DI.inv %>%
      select(which.test, everything())
  }
  
  id_res.A <- which(A==colnames(Dt_Res.short))
  id_res.B <- which(B==colnames(Dt_Res.short))
  id_ben.A <- which(A==colnames(Dt_Ben.short))
  id_ben.B <- which(B==colnames(Dt_Ben.short))
  #beneficiaries
  #------add-hoc fixes (not sure if I should write over original data)
  ben.A <- unlist(Dt_Ben.short[,id_ben.A])[seq(from = 1,to=228,by=6)]
  ben.B <- unlist(Dt_Ben.short[,id_ben.B])[seq(from = 1,to=228,by=6)]
  ben.A[ben.A=="No"] <- "1"
  ben.B[ben.B=="No"] <- "1"
  ben.A[ben.A=="Yes"] <- "5"
  ben.B[ben.B=="Yes"] <- "5"
  ben.A[ben.A=="Yes somewhat"] <- "4"
  ben.B[ben.B=="Yes somewhat"] <- "4"
  ben.A[ben.A=="Not sure"] <- "3"
  ben.B[ben.B=="Not sure"] <- "3"
  #-------------------
  ben.A<-as.integer(ben.A)
  ben.B<-as.integer(ben.B)
  # residents
  if(id_res.A<26){
    res.A<-as.integer(unlist(Dt_Res.short[,id_res.A]))
  }else{
    res.A <- unlist(Dt_Res.short[,id_res.A])[seq(from = 1,to=250,by=5)]
    res.A<-as.integer(res.A)
  }
  if(id_res.B<26){
    res.B<-as.integer(unlist(Dt_Res.short[,id_res.B]))
  }else{
    res.B <- unlist(Dt_Res.short[,id_res.B])[seq(from = 1,to=250,by=5)]
    res.B<-as.integer(res.B)
  }
  # extract the longest tolerable duration associated with DIMs A and B
  res.A.6month <- as.integer(unlist(Dt_Res.short[,id_res.A])[seq(from = 2,to=250,by=5)])
  ben.A.6month <- unlist(Dt_Ben.short[,id_ben.A])[seq(from = 2,to=228,by=6)]
  ben.A.6month[ben.A.6month=="y"] <- "5"
  ben.A.6month <- as.integer(ben.A.6month)
  ld.res.A <- as.character(unlist(Dt_Res.short[,id_res.A])[seq(from = 3,to=250,by=5)])
  ld.score.res.A <- as.integer(unlist(Dt_Res.short[,id_res.A])[seq(from = 4,to=250,by=5)])
  ld.ben.A <- as.character(unlist(Dt_Ben.short[,id_ben.A])[seq(from = 3,to=228,by=6)])
  ld.score.ben.A <- as.integer(unlist(Dt_Ben.short[,id_ben.A])[seq(from = 4,to=228,by=6)])
  # treat out-of-bound score
  res.A.6month[res.A.6month==6]<-5
  res.B.6month <- as.integer(unlist(Dt_Res.short[,id_res.B])[seq(from = 2,to=250,by=5)])
  ben.B.6month <- unlist(Dt_Ben.short[,id_ben.B])[seq(from = 2,to=228,by=6)]
  ben.B.6month[ben.B.6month=="y"] <- "5"
  ben.B.6month <- as.integer(ben.B.6month)
  ld.res.B <- as.character(unlist(Dt_Res.short[,id_res.B])[seq(from = 3,to=250,by=5)])
  ld.score.res.B <- as.integer(unlist(Dt_Res.short[,id_res.B])[seq(from = 4,to=250,by=5)])
  ld.ben.B <- as.character(unlist(Dt_Ben.short[,id_ben.B])[seq(from = 3,to=228,by=6)])
  ld.score.ben.B <- as.integer(unlist(Dt_Ben.short[,id_ben.B])[seq(from = 4,to=228,by=6)])
  # treat out-of-bound score
  res.B.6month[res.B.6month==6]<-5
  
  if(!is_empty(labels_DIMs_hh[which(id_DIMs_hh==A)])){
    DIM_A<-labels_DIMs_hh[which(id_DIMs_hh==A)]
  }
  if(!is_empty(labels_DIMs_neighbour[which(id_DIMs_neighbour==A)])){
    DIM_A<-labels_DIMs_neighbour[which(id_DIMs_neighbour==A)]
  }
  if(!is_empty(labels_DIMs_region[which(id_DIMs_region==A)])){
    DIM_A<-labels_DIMs_region[which(id_DIMs_region==A)]
  }
  if(!is_empty(labels_DIMs_hh[which(id_DIMs_hh==B)])){
    DIM_B<-labels_DIMs_hh[which(id_DIMs_hh==B)]
  }
  if(!is_empty(labels_DIMs_neighbour[which(id_DIMs_neighbour==B)])){
    DIM_B<-labels_DIMs_neighbour[which(id_DIMs_neighbour==B)]
  }
  if(!is_empty(labels_DIMs_region[which(id_DIMs_region==B)])){
    DIM_B<-labels_DIMs_region[which(id_DIMs_region==B)]
  }
  #---------New definition of importance scores 0 to 4---------------
  # WARNING: this change isn't reflected for the longest duration
  res.A <- res.A - 1
  res.A.6month <- res.A.6month - 1
  ben.A <- ben.A - 1
  ben.A.6month <- ben.A.6month - 1
  res.B <- res.B - 1
  res.B.6month <- res.B.6month - 1
  ben.B <- ben.B - 1
  ben.B.6month <- ben.B.6month - 1
  #------------------------------------------------------------------
  df.plot<-tibble(
    x=c(res.A,ben.A),
    y=c(res.B,ben.B),
    x.6month=c(res.A.6month,ben.A.6month),
    y.6month=c(res.B.6month,ben.B.6month),
    x.ld=c(ld.res.A,ld.ben.A),
    y.ld=c(ld.res.B,ld.ben.B),
    x.ld.score=c(ld.score.res.A,ld.score.ben.A),
    y.ld.score=c(ld.score.res.B,ld.score.ben.B),
    stakeholder=c(rep("Resident",50),rep("Beneficiary",38)))
  plot1<-ggplot(df.plot, aes(x=x, y=y, shape=stakeholder)) +
    geom_jitter(alpha = 0.2,size=5, na.rm=T,height=0,width = how_jitter)+
    xlab(DIM_A) +
    ylab(DIM_B) + 
    theme_grey(base_size = 16) +
    labs(title = "two weeks following the diaster")
  plot2<-ggplot(df.plot, aes(x=x.6month, y=y.6month, shape=stakeholder)) +
    geom_jitter(alpha = 0.2,size=5, na.rm=T,height=0,width = how_jitter)+
    xlab(DIM_A) +
    ylab(DIM_B) + 
    theme_grey(base_size = 16) +
    labs(title = "six months following the diaster")
  
  df.3D <- as_tibble(matrix(data=NA,nrow = 36,ncol=4))
  colnames(df.3D)<-c("DIM_A","DIM_B","Count_2weeks","Count_6months")
  count <- 0
  for (dim_a in 0:5){
    for(dim_b in 0:5){
      count <- count +1
      df.3D$DIM_A[count]<-dim_a
      df.3D$DIM_B[count]<-dim_b
      df.3D$Count_2weeks[count]<-length(which(df.plot$x==dim_a&df.plot$y==dim_b))
      df.3D$Count_6months[count]<-length(which(df.plot$x.6month==dim_a&df.plot$y.6month==dim_b))
    }
  }
  plot3<-cloud(Count_2weeks~DIM_A+DIM_B, df.3D, panel.3d.cloud=panel.3dbars, col.facet='grey',
               main=paste0("two weeks following the diaster","\n","DIM #1: ",DIM_A,"\n","DIM #2: ",DIM_B),
               ylim=0:6,
               xlim=0:6,
               xlab= "DIM #1",
               ylab= "DIM #2",
               zlab = "count",
               xbase=0.5, 
               ybase=0.5, 
               scales=list(arrows=FALSE, col=1),
               par.settings=list(axis.line = list(col = "transparent")))
  plot4<-cloud(Count_6months~DIM_A+DIM_B, df.3D, panel.3d.cloud=panel.3dbars, col.facet='grey',
               main=paste0("six months following the diaster","\n","DIM #1: ",DIM_A,"\n","DIM #2: ",DIM_B),
               ylim=0:6,
               xlim=0:6,
               xlab= "DIM #1",
               ylab= "DIM #2",
               zlab = "count",
               title = "dto",
               xbase=0.5, 
               ybase=0.5, 
               scales=list(arrows=F, 
                           x=list(at=c(0:5),lab=c(0:5)), 
                           y=list(at=c(0:5),lab=c(0:5)),
                           col = 1),
               par.settings=list(axis.line = list(col = "transparent")))
  
  # Welch Two Sample t-test
  # more reliable when the two samples have unequal variances and possibly unequal sample sizes
  colnames(df.plot)<-c("DIM A score two weeks","DIM B score two weeks","DIM A score six months","DIM B score six months",
                       "DIM A longest tolerable duration","DIM B longest tolerable duration",
                       "DIM A score longest tolerable duration","DIM B score longest tolerable duration",
                       "Stakeholder")
  tb.t.test<-rbind(tidy(t.test(res.A,ben.A)),
                   tidy(t.test(res.B,ben.B)),
                   tidy(t.test(res.A.6month,ben.A.6month)),
                   tidy(t.test(res.B.6month,ben.B.6month)))
  tb.t.test$which.test<-c("DIM A two weeks: resident vs beneficiary",
                          "DIM B two weeks: resident vs beneficiary",
                          "DIM A six months: resident vs beneficiary",
                          "DIM B six months: resident vs beneficiary")
  tb.t.test <- tb.t.test %>%
    select(which.test, everything())
  
  pwr.A.two.weeks<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.A,na.rm=T), mu2 = mean(ben.A,na.rm=T), 
                                  sd1 = sd(res.A,na.rm=T), sd2 = sd(ben.A,na.rm=T), 
                                  kappa = length(na.omit(res.A))/length(na.omit(ben.A)), 
                                  n2 = length(na.omit(ben.A)), alpha = 0.05,
                                  alternative = "not equal")# power analysis
  pwr.A.two.weeks<-cbind(as.tibble(pwr.A.two.weeks$parms),pwr.A.two.weeks$test,pwr.A.two.weeks$df,pwr.A.two.weeks$ncp,pwr.A.two.weeks$power,pwr.A.two.weeks$n[1],pwr.A.two.weeks$n[2])
  colnames(pwr.A.two.weeks)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                               "paired", "paired.r", "alpha", "margin", "alternative",
                               "verbose","test","df","ncp","power","n1","n2")
  pwr.A.two.weeks.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.A,na.rm=T), mu2 = mean(ben.A,na.rm=T), 
                                      sd1 = sd(res.A,na.rm=T), sd2 = sd(ben.A,na.rm=T),
                                      kappa = length(na.omit(res.A))/length(na.omit(ben.A)),
                                      power = .80, alpha = 0.05,
                                      alternative = "not equal")
  pwr.A.two.weeks.inv<-cbind(as.tibble(pwr.A.two.weeks.inv$parms),pwr.A.two.weeks.inv$test,pwr.A.two.weeks.inv$df,pwr.A.two.weeks.inv$ncp,pwr.A.two.weeks.inv$power,pwr.A.two.weeks.inv$n[1],pwr.A.two.weeks.inv$n[2])
  colnames(pwr.A.two.weeks.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                                   "paired", "paired.r", "alpha", "margin", "alternative",
                                   "verbose","test","df","ncp","power","n1","n2")
  
  pwr.B.two.weeks<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.B,na.rm=T), mu2 = mean(ben.B,na.rm=T), 
                                  sd1 = sd(res.B,na.rm=T), sd2 = sd(ben.B,na.rm=T), 
                                  kappa = length(na.omit(res.B))/length(na.omit(ben.B)), 
                                  n2 = length(na.omit(ben.B)), alpha = 0.05,
                                  alternative = "not equal")
  pwr.B.two.weeks<-cbind(as.tibble(pwr.B.two.weeks$parms),pwr.B.two.weeks$test,pwr.B.two.weeks$df,pwr.B.two.weeks$ncp,pwr.B.two.weeks$power,pwr.B.two.weeks$n[1],pwr.B.two.weeks$n[2])
  colnames(pwr.B.two.weeks)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                               "paired", "paired.r", "alpha", "margin", "alternative",
                               "verbose","test","df","ncp","power","n1","n2")
  
  pwr.B.two.weeks.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.B,na.rm=T), mu2 = mean(ben.B,na.rm=T), 
                                      sd1 = sd(res.B,na.rm=T), sd2 = sd(ben.B,na.rm=T),
                                      kappa = length(na.omit(res.B))/length(na.omit(ben.B)),
                                      power = .80, alpha = 0.05,
                                      alternative = "not equal")
  pwr.B.two.weeks.inv<-cbind(as.tibble(pwr.B.two.weeks.inv$parms),pwr.B.two.weeks.inv$test,pwr.B.two.weeks.inv$df,pwr.B.two.weeks.inv$ncp,pwr.B.two.weeks.inv$power,pwr.B.two.weeks.inv$n[1],pwr.B.two.weeks.inv$n[2])
  colnames(pwr.B.two.weeks.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                                   "paired", "paired.r", "alpha", "margin", "alternative",
                                   "verbose","test","df","ncp","power","n1","n2")
  
  pwr.A.six.months<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.A.6month,na.rm=T), mu2 = mean(ben.A.6month,na.rm=T), 
                                   sd1 = sd(res.A.6month,na.rm=T), sd2 = sd(ben.A.6month,na.rm=T), 
                                   kappa = length(na.omit(res.A.6month))/length(na.omit(ben.A.6month)), 
                                   n2 = length(na.omit(ben.A.6month)), alpha = 0.05,
                                   alternative = "not equal")
  pwr.A.six.months<-cbind(as.tibble(pwr.A.six.months$parms),pwr.A.six.months$test,pwr.A.six.months$df,pwr.A.six.months$ncp,pwr.A.six.months$power,pwr.A.six.months$n[1],pwr.A.six.months$n[2])
  colnames(pwr.A.six.months)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                                "paired", "paired.r", "alpha", "margin", "alternative",
                                "verbose","test","df","ncp","power","n1","n2")
  pwr.A.six.months.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.A.6month,na.rm=T), mu2 = mean(ben.A.6month,na.rm=T), 
                                       sd1 = sd(res.A.6month,na.rm=T), sd2 = sd(ben.A.6month,na.rm=T),
                                       kappa = length(na.omit(res.A.6month))/length(na.omit(ben.A.6month)),
                                       power = .80, alpha = 0.05,
                                       alternative = "not equal")
  pwr.A.six.months.inv<-cbind(as.tibble(pwr.A.six.months.inv$parms),pwr.A.six.months.inv$test,pwr.A.six.months.inv$df,pwr.A.six.months.inv$ncp,pwr.A.six.months.inv$power,pwr.A.six.months.inv$n[1],pwr.A.six.months.inv$n[2])
  colnames(pwr.A.six.months.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                                    "paired", "paired.r", "alpha", "margin", "alternative",
                                    "verbose","test","df","ncp","power","n1","n2")
  
  pwr.B.six.months<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.B.6month,na.rm=T), mu2 = mean(ben.B.6month,na.rm=T), 
                                   sd1 = sd(res.B.6month,na.rm=T), sd2 = sd(ben.B.6month,na.rm=T), 
                                   kappa = length(na.omit(res.B.6month))/length(na.omit(ben.B.6month)), 
                                   n2 = length(na.omit(ben.B.6month)), alpha = 0.05,
                                   alternative = "not equal")
  pwr.B.six.months<-cbind(as.tibble(pwr.B.six.months$parms),pwr.B.six.months$test,pwr.B.six.months$df,pwr.B.six.months$ncp,pwr.B.six.months$power,pwr.B.six.months$n[1],pwr.B.six.months$n[2])
  colnames(pwr.B.six.months)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                                "paired", "paired.r", "alpha", "margin", "alternative",
                                "verbose","test","df","ncp","power","n1","n2")
  pwr.B.six.months.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.B.6month,na.rm=T), mu2 = mean(ben.B.6month,na.rm=T), 
                                       sd1 = sd(res.B.6month,na.rm=T), sd2 = sd(ben.B.6month,na.rm=T),
                                       kappa = length(na.omit(res.B.6month))/length(na.omit(ben.B.6month)),
                                       power = .80, alpha = 0.05,
                                       alternative = "not equal")
  pwr.B.six.months.inv<-cbind(as.tibble(pwr.B.six.months.inv$parms),pwr.B.six.months.inv$test,pwr.B.six.months.inv$df,pwr.B.six.months.inv$ncp,pwr.B.six.months.inv$power,pwr.B.six.months.inv$n[1],pwr.B.six.months.inv$n[2])
  colnames(pwr.B.six.months.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                                    "paired", "paired.r", "alpha", "margin", "alternative",
                                    "verbose","test","df","ncp","power","n1","n2")
  tb.pwr<-rbind(pwr.A.two.weeks,pwr.A.two.weeks.inv,pwr.B.two.weeks,pwr.B.two.weeks.inv,
                pwr.A.six.months,pwr.A.six.months.inv,pwr.B.six.months,pwr.B.six.months.inv)
  tb.pwr<-remove_rownames(tb.pwr)
  tb.pwr$which.test <-c("Power analysis of DIM A two weeks: resident vs beneficiary",
                        "Power analysis (inverse) of DIM A two weeks: resident vs beneficiary",
                        "Power analysis of DIM B two weeks: resident vs beneficiary",
                        "Power analysis (inverse) of DIM B two weeks: resident vs beneficiary",
                        "Power analysis of DIM A six months: resident vs beneficiary",
                        "Power analysis (inverse) of DIM A six months: resident vs beneficiary",
                        "Power analysis of DIM B six months: resident vs beneficiary",
                        "Power analysis (inverse) of DIM A six months: resident vs beneficiary")
  tb.pwr <- tb.pwr %>%
    select(which.test, everything())
  
  # For A and B the temporal comparisons (or two stakeholder groups separately and combined)
  tb.t.test.temporal<-rbind(tidy(t.test(res.A,res.A.6month)),
                            tidy(t.test(ben.A,ben.A.6month)),
                            tidy(t.test(c(res.A,ben.A),c(res.A.6month,ben.A.6month))),
                            tidy(t.test(res.B,res.B.6month)),
                            tidy(t.test(ben.B,ben.B.6month)),
                            tidy(t.test(c(res.A,ben.A),c(res.A.6month,ben.A.6month))))
  tb.t.test.temporal$which.test<-c("DIM A resident: two weeks vs six months",
                                   "DIM A beneficiary: two weeks vs six months",
                                   "DIM A both: two weeks vs six months",
                                   "DIM B resident: two weeks vs six months",
                                   "DIM B beneficiary: two weeks vs six months",
                                   "DIM B both: two weeks vs six months"
  )
  tb.t.test.temporal <- tb.t.test.temporal %>%
    select(which.test, everything())
  
  pwr.A.res<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.A,na.rm=T), mu2 = mean(res.A.6month,na.rm=T), 
                            sd1 = sd(res.A,na.rm=T), sd2 = sd(res.A.6month,na.rm=T), 
                            kappa = length(na.omit(res.A))/length(na.omit(res.A.6month)), 
                            n2 = length(na.omit(res.A.6month)), 
                            alpha = 0.05,
                            alternative = "not equal")# power analysis
  pwr.A.res<-cbind(as.tibble(pwr.A.res$parms),pwr.A.res$test,pwr.A.res$df,pwr.A.res$ncp,pwr.A.res$power,pwr.A.res$n[1],pwr.A.res$n[2])
  colnames(pwr.A.res)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
  pwr.A.res.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.A,na.rm=T), mu2 = mean(res.A.6month,na.rm=T), 
                                sd1 = sd(res.A,na.rm=T), sd2 = sd(res.A.6month,na.rm=T), 
                                kappa = length(na.omit(res.A))/length(na.omit(res.A.6month)), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
  pwr.A.res.inv<-cbind(as.tibble(pwr.A.res.inv$parms),pwr.A.res.inv$test,pwr.A.res.inv$df,pwr.A.res.inv$ncp,pwr.A.res.inv$power,pwr.A.res.inv$n[1],pwr.A.res.inv$n[2])
  colnames(pwr.A.res.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
  
  pwr.A.ben<-pwrss.t.2means(verbose=FALSE,mu1 = mean(ben.A,na.rm=T), mu2 = mean(ben.A.6month,na.rm=T), 
                            sd1 = sd(ben.A,na.rm=T), sd2 = sd(ben.A.6month,na.rm=T), 
                            kappa = length(na.omit(ben.A))/length(na.omit(ben.A.6month)), 
                            n2 = length(na.omit(ben.A.6month)), 
                            alpha = 0.05,
                            alternative = "not equal")# power analysis
  pwr.A.ben<-cbind(as.tibble(pwr.A.ben$parms),pwr.A.ben$test,pwr.A.ben$df,pwr.A.ben$ncp,pwr.A.ben$power,pwr.A.ben$n[1],pwr.A.ben$n[2])
  colnames(pwr.A.ben)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
  pwr.A.ben.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(ben.A,na.rm=T), mu2 = mean(ben.A.6month,na.rm=T), 
                                sd1 = sd(ben.A,na.rm=T), sd2 = sd(ben.A.6month,na.rm=T), 
                                kappa = length(na.omit(ben.A))/length(na.omit(ben.A.6month)), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
  pwr.A.ben.inv<-cbind(as.tibble(pwr.A.ben.inv$parms),pwr.A.ben.inv$test,pwr.A.ben.inv$df,pwr.A.ben.inv$ncp,pwr.A.ben.inv$power,pwr.A.ben.inv$n[1],pwr.A.ben.inv$n[2])
  colnames(pwr.A.ben.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
  
  pwr.A.both<-pwrss.t.2means(verbose=FALSE,mu1 = mean(c(res.A,ben.A),na.rm=T), 
                             mu2 = mean(c(res.A.6month,ben.A.6month),na.rm=T), 
                             sd1 = sd(c(res.A,ben.A),na.rm=T), 
                             sd2 = sd(c(res.A.6month,ben.A.6month),na.rm=T), 
                             kappa = length(na.omit(c(res.A,ben.A)))/length(na.omit(c(res.A.6month,ben.A.6month))), 
                             n2 = length(na.omit(c(res.A.6month,ben.A.6month))), 
                             alpha = 0.05,
                             alternative = "not equal")# power analysis
  pwr.A.both<-cbind(as.tibble(pwr.A.both$parms),pwr.A.both$test,pwr.A.both$df,pwr.A.both$ncp,pwr.A.both$power,pwr.A.both$n[1],pwr.A.both$n[2])
  colnames(pwr.A.both)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                          "paired", "paired.r", "alpha", "margin", "alternative",
                          "verbose","test","df","ncp","power","n1","n2")
  pwr.A.both.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(c(res.A,ben.A),na.rm=T), 
                                 mu2 = mean(c(res.A.6month,ben.A.6month),na.rm=T), 
                                 sd1 = sd(c(res.A,ben.A),na.rm=T), 
                                 sd2 = sd(c(res.A.6month,ben.A.6month),na.rm=T), 
                                 kappa = length(na.omit(c(res.A,ben.A)))/length(na.omit(c(res.A.6month,ben.A.6month))), 
                                 power = .80, alpha = 0.05,
                                 alternative = "not equal")
  pwr.A.both.inv<-cbind(as.tibble(pwr.A.both.inv$parms),pwr.A.both.inv$test,pwr.A.both.inv$df,pwr.A.both.inv$ncp,pwr.A.both.inv$power,pwr.A.both.inv$n[1],pwr.A.both.inv$n[2])
  colnames(pwr.A.both.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                              "paired", "paired.r", "alpha", "margin", "alternative",
                              "verbose","test","df","ncp","power","n1","n2")
  
  pwr.B.res<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.B,na.rm=T), mu2 = mean(res.B.6month,na.rm=T), 
                            sd1 = sd(res.B,na.rm=T), sd2 = sd(res.B.6month,na.rm=T), 
                            kappa = length(na.omit(res.B))/length(na.omit(res.B.6month)), 
                            n2 = length(na.omit(res.B.6month)), 
                            alpha = 0.05,
                            alternative = "not equal")# power analysis
  pwr.B.res<-cbind(as.tibble(pwr.B.res$parms),pwr.B.res$test,pwr.B.res$df,pwr.B.res$ncp,pwr.B.res$power,pwr.B.res$n[1],pwr.B.res$n[2])
  colnames(pwr.B.res)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
  pwr.B.res.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(res.B,na.rm=T), mu2 = mean(res.B.6month,na.rm=T), 
                                sd1 = sd(res.B,na.rm=T), sd2 = sd(res.B.6month,na.rm=T), 
                                kappa = length(na.omit(res.B))/length(na.omit(res.B.6month)), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
  pwr.B.res.inv<-cbind(as.tibble(pwr.B.res.inv$parms),pwr.B.res.inv$test,pwr.B.res.inv$df,pwr.B.res.inv$ncp,pwr.B.res.inv$power,pwr.B.res.inv$n[1],pwr.B.res.inv$n[2])
  colnames(pwr.B.res.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
  
  pwr.B.ben<-pwrss.t.2means(verbose=FALSE,mu1 = mean(ben.B,na.rm=T), mu2 = mean(ben.B.6month,na.rm=T), 
                            sd1 = sd(ben.B,na.rm=T), sd2 = sd(ben.B.6month,na.rm=T), 
                            kappa = length(na.omit(ben.B))/length(na.omit(ben.B.6month)), 
                            n2 = length(na.omit(ben.B.6month)), 
                            alpha = 0.05,
                            alternative = "not equal")# power analysis
  pwr.B.ben<-cbind(as.tibble(pwr.B.ben$parms),pwr.B.ben$test,pwr.B.ben$df,pwr.B.ben$ncp,pwr.B.ben$power,pwr.B.ben$n[1],pwr.B.ben$n[2])
  colnames(pwr.B.ben)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                         "paired", "paired.r", "alpha", "margin", "alternative",
                         "verbose","test","df","ncp","power","n1","n2")
  pwr.B.ben.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(ben.B,na.rm=T), mu2 = mean(ben.B.6month,na.rm=T), 
                                sd1 = sd(ben.B,na.rm=T), sd2 = sd(ben.B.6month,na.rm=T), 
                                kappa = length(na.omit(ben.B))/length(na.omit(ben.B.6month)), 
                                power = .80, alpha = 0.05,
                                alternative = "not equal")
  pwr.B.ben.inv<-cbind(as.tibble(pwr.B.ben.inv$parms),pwr.B.ben.inv$test,pwr.B.ben.inv$df,pwr.B.ben.inv$ncp,pwr.B.ben.inv$power,pwr.B.ben.inv$n[1],pwr.B.ben.inv$n[2])
  colnames(pwr.B.ben.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                             "paired", "paired.r", "alpha", "margin", "alternative",
                             "verbose","test","df","ncp","power","n1","n2")
  
  pwr.B.both<-pwrss.t.2means(verbose=FALSE,mu1 = mean(c(res.B,ben.B),na.rm=T), 
                             mu2 = mean(c(res.B.6month,ben.B.6month),na.rm=T), 
                             sd1 = sd(c(res.B,ben.B),na.rm=T), 
                             sd2 = sd(c(res.B.6month,ben.B.6month),na.rm=T), 
                             kappa = length(na.omit(c(res.B,ben.B)))/length(na.omit(c(res.B.6month,ben.B.6month))), 
                             n2 = length(na.omit(c(res.B.6month,ben.B.6month))), 
                             alpha = 0.05,
                             alternative = "not equal")# power analysis
  pwr.B.both<-cbind(as.tibble(pwr.B.both$parms),pwr.B.both$test,pwr.B.both$df,pwr.B.both$ncp,pwr.B.both$power,pwr.B.both$n[1],pwr.B.both$n[2])
  colnames(pwr.B.both)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                          "paired", "paired.r", "alpha", "margin", "alternative",
                          "verbose","test","df","ncp","power","n1","n2")
  pwr.B.both.inv<-pwrss.t.2means(verbose=FALSE,mu1 = mean(c(res.B,ben.B),na.rm=T), 
                                 mu2 = mean(c(res.B.6month,ben.B.6month),na.rm=T), 
                                 sd1 = sd(c(res.B,ben.B),na.rm=T), 
                                 sd2 = sd(c(res.B.6month,ben.B.6month),na.rm=T), 
                                 kappa = length(na.omit(c(res.B,ben.B)))/length(na.omit(c(res.B.6month,ben.B.6month))), 
                                 power = .80, alpha = 0.05,
                                 alternative = "not equal")
  pwr.B.both.inv<-cbind(as.tibble(pwr.B.both.inv$parms),pwr.B.both.inv$test,pwr.B.both.inv$df,pwr.B.both.inv$ncp,pwr.B.both.inv$power,pwr.B.both.inv$n[1],pwr.B.both.inv$n[2])
  colnames(pwr.B.both.inv)<-c("mu1","mu2","sd1","sd2","kappa","welch.df", 
                              "paired", "paired.r", "alpha", "margin", "alternative",
                              "verbose","test","df","ncp","power","n1","n2")
  
  tb.pwr.temporal<-rbind(pwr.A.res,pwr.A.res.inv,pwr.A.ben,pwr.A.ben.inv,pwr.A.both,pwr.A.both.inv,
                         pwr.B.res,pwr.B.res.inv,pwr.B.ben,pwr.B.ben.inv,pwr.B.both,pwr.B.both.inv)
  tb.pwr.temporal<-remove_rownames(tb.pwr.temporal)
  tb.pwr.temporal$which.test <-c("Power analysis of DIM A resident: two weeks vs six months",
                                 "Power analysis (inverse) of DIM A resident: two weeks vs six months",
                                 "Power analysis of DIM A beneficiary: two weeks vs six months",
                                 "Power analysis (inverse) of DIM A beneficiary: two weeks vs six months",
                                 "Power analysis of DIM A both: two weeks vs six months",
                                 "Power analysis (inverse) of DIM A both: two weeks vs six months",
                                 "Power analysis of DIM B resident: two weeks vs six months",
                                 "Power analysis (inverse) of DIM B resident: two weeks vs six months",
                                 "Power analysis of DIM B beneficiary: two weeks vs six months",
                                 "Power analysis (inverse) of DIM B beneficiary: two weeks vs six months",
                                 "Power analysis of DIM B both: two weeks vs six months",
                                 "Power analysis (inverse) of DIM B both: two weeks vs six months"
  )
  tb.pwr.temporal <- tb.pwr.temporal %>%
    select(which.test, everything())
  return(list(
    #results for spatial comparison
    plot.DI,
    df.plot.DI,
    as.tibble(report(res.aov.DI)),
    df.Welch.aov.DI,
    tb.t.test.DI,
    pwr.DI,
    pwr.DI.inv,
    #results for two specific DIMs
    plot1, plot2,plot3, plot4, 
    df.plot, 
    tb.t.test,
    tb.pwr,
    tb.t.test.temporal,
    tb.pwr.temporal
    )
  )
}