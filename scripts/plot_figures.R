# make plots for the article

# require packages
require(data.table)
require(ggplot2);require(viridis)
require(patchwork)
require(DALEX);require(ingredients); require(auditor); require(MASS)
library(nlme); library(MuMIn)

# clean all
rm(list=ls())

# --- prepare datasets ----
  
  # load in the files
  d1 <- fread('products/250429_db_nh3_final.csv')
  d1[,set := 'nh3']
  d2 <- fread('products/250429_db_n2o_final.csv')
  d2[,set := 'n2o']
  d3 <- fread('products/250429_db_no_final.csv')
  d3[,set :='no']
  d4 <- fread('products/250423_db_no3_final.csv')
  d4[,set := 'no3']
  
  # standardize to unit variance for numeric columns
  d5 <- rbind(d1,d2,d3,d4,fill=TRUE)
  
  # variables to standardize
  cols <- c('ph','soc','clay','sand','cec','bd','temp','prec','ndose')
  
  # store median values
  my.summary = function(x) list(mean = mean(x), sd = sd(x))
  db.sum <- d5[,unlist(lapply(.SD,my.summary)),.SDcols = cols]
  
  # standardize numeric columns
  d5[,c(cols) := lapply(.SD,scale),.SDcols = cols]
  
  # set back to original files
  d1 <- d5[set=='nh3']
  d2 <- d5[set=='n2o']
  d3 <- d5[set=='no']
  d4 <- d5[set=='no3']

# source scripts
source('scripts/plot_funs.R')

# plot boxplot with measurements
p1 <- ggplot(data=d1[!is.na(ef_mean) & !is.na(fert_cat)],aes(x = fert_cat,y=ef_mean,fill=fert_cat)) + 
      geom_boxplot(show.legend = F) + theme_bw()+ ylim(0,0.5)+
      scale_fill_viridis(discrete = TRUE, alpha=0.6) +
      geom_jitter(color="black", size=0.4, alpha=0.9,show.legend = F) + 
      ggtitle(expression(paste("N-NH"[3]," emission fraction per fertilizer type"))) +
      xlab('')+ ylab('Emission fraction')+
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size=14),
            plot.title = element_text(size=20))
p2 <- ggplot(data=d2[,.(fert_cat,ef_n2o = ef_mean)],aes(x = fert_cat,y=ef_n2o,fill=fert_cat)) + 
      geom_boxplot(show.legend = F) + theme_bw()+
      scale_fill_viridis(discrete = TRUE, alpha=0.6) + ylim(0,0.05)+
      geom_jitter(color="black", size=0.4, alpha=0.9,show.legend = F) + 
      ggtitle(expression(paste("N-N"[2],"O emission fraction per fertilizer type"))) + 
      xlab('')+ ylab('') +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size=14),
            plot.title = element_text(size=20))
p3 <- ggplot(data=d3[,.(fert_cat,ef_no = ef_mean)],aes(x = fert_cat,y=ef_no,fill=fert_cat)) + 
      geom_boxplot(show.legend = F) + theme_bw()+
      scale_fill_viridis(discrete = TRUE, alpha=0.6) + ylim(0,0.05)+
      geom_jitter(color="black", size=0.4, alpha=0.9,show.legend = F) + 
      ggtitle(expression(paste("N-N","O emission fraction per fertilizer type"))) + 
      xlab('')+ ylab('Emission fraction') +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size=14),
            plot.title = element_text(size=20))
p4 <- ggplot(data=d4[,.(fert_cat,ef_no3 = ef_mean)],aes(x = fert_cat,y=ef_no3,fill=fert_cat)) + 
      geom_boxplot(show.legend = F) + theme_bw()+
      scale_fill_viridis(discrete = TRUE, alpha=0.6) + ylim(0,0.5)+
      geom_jitter(color="black", size=0.4, alpha=0.9,show.legend = F) + 
      ggtitle(expression(paste("N-NO"[3]," emission fraction per fertilizer type"))) + 
      xlab('')+ ylab('') +
      theme(axis.text = element_text(size = 20),
            axis.title = element_text(size=14),
            plot.title = element_text(size=20))

p5 <- (p1 | p2)/(p3 | p4)

ggsave(plot=p5,filename='products/figure_boxplots_efs2.png',width = 28,height=15,dpi=440) # was 24-15

# update model to predict NH3
d1[,log_ef := log(EF_NH3_value)]
setnames(d1,c('tmp_mean','pre_mean'),c('temp','prec'),skip_absent = T)

m1 <-lme(log_ef ~ ph+I(ph^2) + crop_cat + ndose + I(ndose^2) +fert_cat + soc:clay + temp + prec+ 
           measmethod_nh3+appc+ph:ndose-1, 
         random = ~ 1 | GEnZ/exptype/phtype, weights = varFunc(~ log(EF_NH3_sd+0.0001)),  control=lmeControl(sigma = 1), 
         data=d1,na.action=na.omit)

  # parameters NH3 model
  cols.lm <- c('ph','ndose','clay','cec','soc','temp','prec','crop_cat','fert_cat',
               'measmethod_nh3','appc','GEnZ','exptype','phtype')

  # make the model explainer for the training dataset
  explainer.train.lm <- explain(model = m1, data = as.data.frame(d1[, .SD, .SDcols = cols.lm]), y = d1$log_ef, label = "")

  # make a Variance Importance Plot (VIP) on training sets
  imp.nh3.lm <- ingredients::feature_importance(explainer.train.lm, loss_function = loss_root_mean_square, type = "difference")
  
  # plot the figure (figure has not yet nice formatting)
  p1 <- plot(imp.nh3.lm[imp.nh3.lm$variable !='exptype',]) + theme_bw() + theme(legend.position = 'none') + 
        ggtitle(label=expression(paste("Feature importance for N-NH"[3])),
                subtitle=expression(paste("created for meta-regression model predicting EF for N-H"[3])))+
        ylab('') +
        theme(axis.text = element_text(size = 20),
              axis.title = element_text(size=14),
              plot.title = element_text(size=20))

  # model the residual
  res.lm.nh3 <- auditor::model_residual(explainer.train.lm)  
  plot.nh3.res <- ggplot_hist(res.lm.nh3) + ggtitle(expression(paste("Residual plot for log-EF N-NH"[3])))
  plot.nh3.mp <- ggplot_onetoone(res.lm.nh3) + ggtitle(expression(paste("Residual plot for log-EF N-NH"[3])))
  
  # plot ALE for fertilizer category
  explainer.train.lm <- explain(model = m1, data = as.data.frame(d1[, .SD, .SDcols = cols.lm]), y = d1$log_ef, label = "")
  ale.nh3.fertcat <- ingredients::accumulated_dependency(explainer.train.lm, 'fert_cat')
  ale.nh3.ndose <- ingredients::accumulated_dependency(explainer.train.lm, 'ndose')
  ale.nh3.prec <- ingredients::accumulated_dependency(explainer.train.lm, 'prec')
  ale.nh3.temp <- ingredients::accumulated_dependency(explainer.train.lm, 'temp')
  ale.nh3.soc <- ingredients::accumulated_dependency(explainer.train.lm, 'soc')
  
# make a model to predict N2O
# m2 <-lme(log_ef ~ fert_cat+ndose + ph+ clay : soc + ph + crop_cat + I(temp^2)+ I(prec^2)+
#            prec * temp +GEnZ+ appc + nsplitc-1, random = ~ 1 |studyid, weights = varFunc(~ log(ef_n2o_se)),  
#          data=d2,na.action=na.omit)
m2 <-lme(log_ef ~ fert_cat+ndose + ph+ clay * soc + ph + crop_cat + I(temp^2)+ I(prec^2)+I(ndose^2)+
           GEnZ + appc + nsplitc-1, 
         random = ~ 1 |studyid, weights = varFunc(~ log(ef_n2o_se)),  
         data=d2,na.action=na.omit)

  # first, select teh relevant explanatory variables being used in the model
  cols.lm <- c('ph','ndose','clay','soc','temp','prec','crop_cat','fert_cat','appc','GEnZ','nsplitc','studyid')
  
  # make the model explainer for the training dataset
  explainer.train.lm <- explain(model = m2, data = as.data.frame(d2[, .SD, .SDcols = cols.lm]), y = d2$log_ef, label = "")

  # make a Variance Importance Plot (VIP) on training sets
  imp.n2o.lm <- ingredients::feature_importance(explainer.train.lm,loss_function = loss_root_mean_square, type = "difference")
  
  # plot the figure (figure has not yet nice formatting)
  p2 <- plot(imp.n2o.lm[imp.n2o.lm$variable !='studyid',]) + theme_bw() + theme(legend.position = 'none') + 
        ggtitle(label=expression(paste("Feature importance for N-N"[2],"O")),
                subtitle=expression(paste("created for meta-regression model predicting EF for N-N"[2],"O")))+
        ylab('')+
        theme(axis.text = element_text(size = 20),
              axis.title = element_text(size=14),
              plot.title = element_text(size=20))
  
  # model the residual
  res.lm.n2o <- auditor::model_residual(explainer.train.lm)  
  plot.n2o.res <- ggplot_hist(res.lm.n2o) + ggtitle(expression(paste("Residual plot for log-EF N-N"[2],"O")))
  plot.n2o.mp <- ggplot_onetoone(res.lm.n2o)+ ggtitle(expression(paste("Residual plot for log-EF N-N"[2],"O")))
  
  # plot ALE for fertilizer category
  explainer.train.lm <- explain(model = m2, data = as.data.frame(d2[, .SD, .SDcols = cols.lm]), y = d2$log_ef, label = "")
  ale.n2o.fertcat <- ingredients::accumulated_dependency(explainer.train.lm, 'fert_cat')
  ale.n2o.ndose <- ingredients::accumulated_dependency(explainer.train.lm, 'ndose')
  ale.n2o.prec <- ingredients::accumulated_dependency(explainer.train.lm, 'prec')
  ale.n2o.temp <- ingredients::accumulated_dependency(explainer.train.lm, 'temp')
  ale.n2o.soc <- ingredients::accumulated_dependency(explainer.train.lm, 'soc')
  
# make a model to predict N2O
  
  m3 <-lme(log_ef ~ fert_cat+ndose + ph+soc +I(ph^2) + prec+temp+GEnZ+
             appc + nsplitc-1, 
           random = ~ 1 |studyid, weights = varFunc(~ log(ef_no_se)),  
           data=d3,na.action=na.omit)
  
  # first, select teh relevant explanatory variables being used in the model
  cols.lm <- c('ph','ndose','soc','temp','prec','fert_cat','appc','GEnZ','nsplitc','studyid')
  
  # make the model explainer for the training dataset
  explainer.train.lm <- explain(model = m3, data = as.data.frame(d3[, .SD, .SDcols = cols.lm]), y = d3$log_ef, label = "")
  
  # make a Variance Importance Plot (VIP) on training sets
  imp.no.lm <- ingredients::feature_importance(explainer.train.lm,loss_function = loss_root_mean_square, type = "difference")
  
  # plot the figure (figure has not yet nice formatting)
  p3 <- plot(imp.no.lm[imp.no.lm$variable !='studyid',]) + theme_bw() + theme(legend.position = 'none') + 
    ggtitle(label=expression(paste("Feature importance for N-N","O")),
            subtitle=expression(paste("created for meta-regression model predicting EF for N-N","O")))+
    ylab('RMSE loss after permutation')+
    theme(axis.text = element_text(size = 20),
          axis.title = element_text(size=14),
          plot.title = element_text(size=20))
  
  # model the residual
  res.lm.no <- auditor::model_residual(explainer.train.lm)  
  plot.no.res <- ggplot_hist(res.lm.no) + ggtitle(expression(paste("Residual plot for log-EF N-NO")))
  plot.no.mp <- ggplot_onetoone(res.lm.no)+ ggtitle(expression(paste("Residual plot for log-EF N-NO")))
  
  # plot ALE for fertilizer category
  explainer.train.lm <- explain(model = m3, data = as.data.frame(d3[, .SD, .SDcols = cols.lm]), y = d3$log_ef, label = "")
  ale.no.fertcat <- ingredients::accumulated_dependency(explainer.train.lm, 'fert_cat')
  ale.no.ndose <- ingredients::accumulated_dependency(explainer.train.lm, 'ndose')
  ale.no.prec <- ingredients::accumulated_dependency(explainer.train.lm, 'prec')
  ale.no.temp <- ingredients::accumulated_dependency(explainer.train.lm, 'temp')
  ale.no.soc <- ingredients::accumulated_dependency(explainer.train.lm, 'soc')
  
# make a model to predict NO3
  
  m4 <-lme(log_ef ~ fert_cat+ndose+clay+soc  + prec+temp+I(temp^2)+GEnZ, 
           random = ~ 1 |studyid, weights = varFunc(~ n),  
           data=d4,na.action=na.omit)
  
  # first, select teh relevant explanatory variables being used in the model
  cols.lm <- c('ndose','soc','clay','temp','prec','fert_cat','GEnZ','studyid')
  
  # make the model explainer for the training dataset
  explainer.train.lm <- explain(model = m4, data = as.data.frame(d4[, .SD, .SDcols = cols.lm]), y = d4$log_ef, label = "")
  
  # make a Variance Importance Plot (VIP) on training sets
  imp.no3.lm <- ingredients::feature_importance(explainer.train.lm,loss_function = loss_root_mean_square, type = "difference")
  
  # plot the figure (figure has not yet nice formatting)
  p4 <- plot(imp.no3.lm[imp.no3.lm$variable !='studyid',]) + theme_bw() + theme(legend.position = 'none') + 
        ggtitle(label=expression(paste("Feature importance for N-NO"[3])),
                subtitle=expression(paste("created for meta-regression model predicting EF for N-NO"[3])))+
        ylab('RMSE loss after permutation')+
        theme(axis.text = element_text(size = 20),
              axis.title = element_text(size=14),
              plot.title = element_text(size=20))
  
  # model the residual
  res.lm.no3 <- auditor::model_residual(explainer.train.lm)  
  plot.no3.res <- ggplot_hist(res.lm.no3) + ggtitle(expression(paste("Residual plot for log-EF N-NO"[3])))
  plot.no3.mp <- ggplot_onetoone(res.lm.no3)+ ggtitle(expression(paste("Residual plot for log-EF N-NO"[3])))
  
  # plot ALE for fertilizer category
  explainer.train.lm <- explain(model = m4, data = as.data.frame(d4[, .SD, .SDcols = cols.lm]), y = d4$log_ef, label = "")
  ale.no3.fertcat <- ingredients::accumulated_dependency(explainer.train.lm, 'fert_cat')
  ale.no3.ndose <- ingredients::accumulated_dependency(explainer.train.lm, 'ndose')
  ale.no3.prec <- ingredients::accumulated_dependency(explainer.train.lm, 'prec')
  ale.no3.temp <- ingredients::accumulated_dependency(explainer.train.lm, 'temp')
  ale.no3.soc <- ingredients::accumulated_dependency(explainer.train.lm, 'soc')
  
  # combine both figures 1-to-1 and residual
  p5 <- (p1 | p2) / (p3 | p4)
  ggsave(plot=p5,filename='products/figure_vips_efs.png',width = 20,height=17,dpi=440)

  p5 <- (plot.nh3.res | plot.n2o.res | plot.no.res | plot.no3.res)/
        (plot.nh3.mp | plot.n2o.mp | plot.no.mp | plot.no3.mp)
  ggsave(plot=p5,filename='products/figure_residual_efs.png',width = 25,height=17,dpi=440)
  
  # plot the ALE plot for the fert_cat
  p1 <- ggplot_ale_notscaled(ale.nh3.fertcat) + ggtitle('Impact of fertilizer type on EF-NH3') + xlab('')
  p2 <- ggplot_ale_notscaled(ale.n2o.fertcat) + ggtitle('Impact of fertilizer type on EF-N2O')+ xlab('')
  p3 <- ggplot_ale_notscaled(ale.no.fertcat) + ggtitle('Impact of fertilizer type on EF-NO')+ xlab('')
  p4 <- ggplot_ale_notscaled(ale.no3.fertcat) + ggtitle('Impact of fertilizer type on EF-NO3')+ xlab('')
  p5 <- (p1 | p2 | p3 | p4)
  ggsave(plot=p5,filename='products/figure_ale_ferttype.png',width = 30,height=8.5,dpi=440)
  
  # plot ggplot for N dose
  p1 <- ggplot_ale_notscaled(ale.nh3.ndose) + ggtitle('Impact of N dose on EF-NH3') + xlab('')+xlim(0,600)+theme(legend.position = 'none')
  p2 <- ggplot_ale_notscaled(ale.n2o.ndose) + ggtitle('Impact of N dose on EF-N2O')+ xlab('')+xlim(0,600)+theme(legend.position = 'none')
  p3 <- ggplot_ale_notscaled(ale.no.ndose) + ggtitle('Impact of N dose on EF-NO')+ xlab('')+xlim(0,600)+theme(legend.position = 'none')
  p4 <- ggplot_ale_notscaled(ale.no3.ndose) + ggtitle('Impact of N dose on EF-NO3')+ xlab('')+xlim(0,600)+theme(legend.position = 'none')
  p5 <- (p1 | p2 | p3 | p4)
  ggsave(plot=p5,filename='products/figure_ale_ndose.png',width = 30,height=8.5,dpi=440)
  
  
  # plot ggplot for precipitation
  p1 <- ggplot_ale_notscaled(ale.nh3.prec) + ggtitle('Impact of precipitation on EF-NH3') + xlab('')+theme(legend.position = 'none')
  p2 <- ggplot_ale_notscaled(ale.n2o.prec) + ggtitle('Impact of precipitation on EF-N2O')+ xlab('')+theme(legend.position = 'none')
  p3 <- ggplot_ale_notscaled(ale.no.prec) + ggtitle('Impact of precipitation on EF-NO')+ xlab('')+theme(legend.position = 'none')
  p4 <- ggplot_ale_notscaled(ale.no3.prec) + ggtitle('Impact of precipitation on EF-NO3')+ xlab('')+theme(legend.position = 'none')
  p5 <- (p1 | p2 | p3 | p4)
  ggsave(plot=p5,filename='products/figure_ale_prec.png',width = 30,height=8.5,dpi=440)
  
  
  
