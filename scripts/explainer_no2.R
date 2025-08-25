# here some copy-paste part of the code to make a model explainer

# require packages
require(DALEX); require(data.table); require(ggplot2)
require(ingredients); require(auditor); require(MASS)
require(patchwork)

# source the plotting funs
source('scripts/plot_funs.R')

# Load the model

  # load the database to train model
  d2 <- fread('products/250311_db_no2_final.csv')

  # train the model (see analysis_n2o.R)
  m1 <-lme(log_ef ~ fert_cat+ndose + ph+ clay : soc + ph + crop_cat + I(temp^2)+ I(prec^2)+
             prec * temp +GEnZ+ 
             appc + nsplitc-1, 
           random = ~ 1 |studyid, weights = varFunc(~ log(ef_n2o_se)),  
           data=d2,na.action=na.omit)
  
# Interpret the model -----------------------------------------------------

# make a simple custom prediciton function      
custom_predict <- function(model, newdata) {
  x <- predict(model, newdata = newdata)
  return(x)
}

# Create a model explainers for this models
# first, select the relevant explanatory variables being used in the model
cols.lm <- c('ph','ndose','clay','soc','temp','prec','crop_cat','fert_cat','appc','GEnZ','nsplitc','studyid')

# make the model explainer for the training dataset, model m1 is prepared in analysis_n2o
explainer.train.lm <- explain(model = m1, 
                              data = as.data.frame(d2[, .SD, .SDcols = cols.lm]), 
                              y = d2$log_ef, 
                              label = "model_train_glm")

# make a Variance Importance Plot (VIP) on training sets

  # make VIP
  imp.lm <- ingredients::feature_importance(explainer.train.lm, 
                                            loss_function = loss_root_mean_square, 
                                            type = "difference")
  
  # plot the figure (figure has not yet nice formatting)
  p1 <- plot(imp.lm) + theme_bw() + theme(legend.position = 'none') + 
        ggtitle(label='Feature importance',subtitle='created for MA model predicting EF for N2O')+
        ylab('RMSE loss after permutation')
  ggsave(plot = p1, filename='products/model_n2o_vip.png',width = 12,height = 15,units = 'cm')

# make a residual plot on test

  # model the residual
  res.lm.train <- auditor::model_residual(explainer.train.lm)  
  # auditor::score_r2(explainer.test.lm)
  # plot the histogram for multiple models (function is given below, note that function format is taken from old script, needs to be updated)
  plot.res <- ggplot_hist(res.lm.train)
  # save the plot
  #ggsave(plot = plot.res,filename = 'products/plot2_res.png',width = 13.16, height = 8.90, units='cm')
  
  # make a 1-to-1 plot of both regressions using an own plot function (see below in this file)
  plot.mp <- ggplot_onetoone(res.lm.train)
  
  # combine plots and save
  p2 <- plot.mp + plot.res
  ggsave(plot = p2,filename = 'products/model_n2o_1to1_res.png',width = 14, height = 10, units='cm')

# plot ALE-plots

  # make model explainers for ALE for each model parameter
  ale.ph.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ph')
  ale.temp.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'temp')
  ale.clay.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'clay')
  ale.soc.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'soc')
  ale.ndose.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ndose')
  ale.prec.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'prec')
  ale.prec.fertcat <- ingredients::accumulated_dependency(explainer.train.lm, 'fert_cat')
  ale.prec.appc <- ingredients::accumulated_dependency(explainer.train.lm, 'appc')
  ale.prec.nsplitc <- ingredients::accumulated_dependency(explainer.train.lm, 'nsplitc')

#make a combined plot (with the two lines into one figure)

  # note that all variables are scaled to unit variance
  plot.ale <- ggplot_ale(ale.ph.lm,ale.ph2.lm,ale.clay.lm,ale.ndose.lm,ale.ndose2.lm,ale.tmp.lm,ale.prec.lm)
  ggsave(plot = plot.ale,filename = 'products/model_nh3_ale.png',width = 13.16, height = 10.90, units='cm')

  # plot here the figures on original scale
  p1 <- ggplot_ale_notscaled(ale.ph.lm)
  ggsave(plot = p1,filename = 'products/model_n2o_ale_ph.png',width = 13.16, height = 10.90, units='cm')
  p2 <- ggplot_ale_notscaled(ale.ndose.lm)
  ggsave(plot = p2,filename = 'products/model_n2o_ale_ndose.png',width = 13.16, height = 10.90, units='cm')
  p3 <- ggplot_ale_notscaled(ale.prec.fertcat)
  ggsave(plot = p3,filename = 'products/model_n2o_ale_fertcat.png',width = 13.16, height = 10.90, units='cm')
  p4 <- ggplot_ale_notscaled(ale.prec.lm)
  ggsave(plot = p4,filename = 'products/model_n2o_ale_prec.png',width = 13.16, height = 10.90, units='cm')
  p5 <- ggplot_ale_notscaled(ale.prec.appc)
  ggsave(plot = p5,filename = 'products/model_n2o_ale_appc.png',width = 13.16, height = 10.90, units='cm')
  p6 <- ggplot_ale_notscaled(ale.temp.lm)
  ggsave(plot = p6,filename = 'products/model_n2o_ale_temp.png',width = 13.16, height = 10.90, units='cm')
  p7 <- ggplot_ale_notscaled(ale.soc.lm)
  ggsave(plot = p7,filename = 'products/model_n2o_ale_soc.png',width = 13.16, height = 10.90, units='cm')
  p8 <- ggplot_ale_notscaled(ale.clay.lm)
  ggsave(plot = p8,filename = 'products/model_n2o_ale_clay.png',width = 13.16, height = 10.90, units='cm')
  
  pf <- (p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)
  ggsave(plot=pf,filename='products/figure_ale_n2o_combi.png',width = 20,height=10)

