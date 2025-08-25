# here some copy-paste part of the code to make a model explainer

rm(list=ls())

# require packages
require(DALEX);require(data.table);require(ggplot2)
require(ingredients);require(auditor);require(MASS)
require(patchwork)

# load in database and plot functions
d1 <- fread('products/250423_db_nh3_final.csv')
d1[,log_ef := log(EF_NH3_value)]
setnames(d1,c('tmp_mean','pre_mean'),c('temp','prec'),skip_absent = T)

# prepare the model
m1 <-lme(log_ef ~ ph+I(ph^2) + crop_cat + ndose + I(ndose^2) +fert_cat + soc:clay + temp + prec+ 
           measmethod_nh3+appc+ph:ndose-1, 
         random = ~ 1 | GEnZ/exptype/phtype, weights = varFunc(~ log(EF_NH3_sd+0.0001)),  control=lmeControl(sigma = 1), 
         data=d1,na.action=na.omit)


# load in plot functions (to be syncrhonized with functions in plot_funs later, here the titles are made for NH3)
if(TRUE){
  
  # own function to plot multiple histograms
  # all these plot functions have been defined for a single paper and axis titles or plot titles are specific for this purpose.
  ggplot_hist <- function(...) {
    
    obj <- list(...)
    
    # make empty list
    out <- list()
    
    # convert all residual objects into one data.bele
    for(i in 1:length(obj)){
      
      out[[i]] <- data.table(label = levels(obj[[i]]$`_label_`),
                             residual = obj[[i]]$`_residuals_`)
      
    }
    out <- rbindlist(out)
    out <- out[order(label)]
    
    # make a residual plot
    if(length(obj) > 1){ 
      
      p <- ggplot(out,aes(x=residual,y=..density..,fill=label)) + 
        geom_histogram(position='identity')  +
        geom_density(aes(x=residual,y=..density..))+
        facet_wrap(~label,scales = "free_y")
      
    } else {
      
      p <- ggplot(out,aes(x=residual,y=..density..,fill=label)) + 
        geom_histogram(position='identity')  +
        geom_density(aes(x=residual,y=..density..))  
      
    }
    
    p <- p + theme_bw() + theme(legend.position = 'none') 
    
    
    return(p)
  }
  
  # plot 1 to 1 plots
  # plot multiple histograms
  ggplot_onetoone <- function(...) {
    
    obj <- list(...)
    
    # make empty list
    out <- list()
    
    # convert all residual objects into one data.bele
    for(i in 1:length(obj)){
      
      out[[i]] <- data.table(label = levels(obj[[i]]$`_label_`),
                             obs = obj[[i]]$`_y_`,
                             pred = obj[[i]]$`_y_hat_`)
      
    }
    out <- rbindlist(out)
    out <- out[order(label)]
    out[,label := gsub('_lm','_glm',label)]
    
    # make a residual plot
    if(length(obj) > 1){ 
      
      p <- ggplot(out,aes(x=obs,y=pred,col=label)) + 
        geom_point()  + geom_abline(intercept = 0,slope = 1,lty=2)+ 
        facet_wrap(~label)
      
    } else {
      
      p <- ggplot(out,aes(x=obs,y=pred,col=label)) + 
        geom_point()  + geom_abline(intercept = 0,slope = 1,lty=2)
      
    }
    
    p <- p + theme_bw() + theme(legend.position = 'none') + 
      ylab('Predicted EF') + 
      xlab('Observed EF')
    
    return(p)
  }
  
  ggplot_ale <- function(...){
    
    obj <- list(...)
    
    # make empty list
    out <- list()
    
    # convert all residual objects into one data.bele
    for(i in 1:length(obj)){
      
      out[[i]] <- data.table(parm = unique(obj[[i]]$`_vname_`),
                             label = unique(obj[[i]]$`_label_`),
                             y = obj[[i]]$`_yhat_`,
                             x = obj[[i]]$`_x_`)
      
    }
    out <- rbindlist(out)
    out <- out[order(label)]
    
    # remove log from the label
    out[,parm:= gsub('ln_','',parm)]
    
    # set axes
    xaxmax <- min(3,max(out$x))
    xaxmin <- max(-3,min(out$x))
    
    # make a ALE plot
    if(length(obj) > 1){ 
      
      p <- ggplot(out,aes(x=x,y=y,col=parm)) + 
        geom_point()  + geom_smooth()+
        facet_wrap(~label,scales='free')
      
    } else {
      
      p <- ggplot(out,aes(x=x,y=y,col=parm)) + 
        geom_point()  + geom_smooth()
      
    }
    
    p <- p + theme_bw() + theme(legend.position = 'bottom') + 
      ylab('Change in average predicted EF') + 
      xlab('Change in explanatory variable (scaled to unit variance)') +
      xlim(xaxmin,xaxmax) +
      theme(axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            axis.title = element_text(size=10))
    
    return(p)
  }
  
  
  ggplot_ale_notscaled <- function(...){
    
    obj <- list(...)
    
    # make empty list
    out <- list()
    
    # convert all residual objects into one data.bele
    for(i in 1:length(obj)){
      
      out[[i]] <- data.table(parm = unique(obj[[i]]$`_vname_`),
                             label = unique(obj[[i]]$`_label_`),
                             y = obj[[i]]$`_yhat_`,
                             x = obj[[i]]$`_x_`)
      
    }
    out <- rbindlist(out)
    out <- out[order(label)]
    
    # remove log from the label
    out[,parm:= gsub('ln_','',parm)]
    
    # set axes
    if(class(out$x)=='factor'){
      
      p <- ggplot(out,aes(x = x,y = y,color=parm,fill=parm)) + geom_bar(stat='identity',show.legend = FALSE)  +
        facet_wrap(~label,scales='free')
      
      p <- p + theme_bw() + 
        scale_color_viridis_d()+
        scale_fill_viridis_d()+
        ylab('Change in average predicted EF') + 
        xlab('Change in explanatory variable') +
        theme(axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              axis.title = element_text(size=10)) +
        ggtitle(paste0('Impact of ',unique(out$parm), ' on the EF of NH3'))
    } else {
      
      xaxmax <- max(out$x)
      xaxmin <- min(out$x)
      
      p <- ggplot(out,aes(x=x,y=y,col=parm)) + 
        geom_point(show.legend = FALSE)  + geom_smooth()+
        facet_wrap(~label,scales='free')
      
      p <- p + theme_bw() + 
        theme(legend.position ='inside',
              legend.position.inside = c(0.2,0.8)) + 
        scale_color_viridis_d()+
        ylab('Change in average predicted EF') + 
        xlab('Change in explanatory variable') +
        xlim(xaxmin,xaxmax) +
        theme(axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              axis.title = element_text(size=10)) +
        ggtitle(paste0('Impact of ',unique(out$parm), ' on the EF of NH3'))
      
    }
    
    
    
    return(p)
  }
  
  
  ggplot_pdp <- function(obj, x) {
    
    
    out <- as.data.table(obj$agr_profiles)
    out[,label]
    p <- 
      as_tibble() %>%
      mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>%
      ggplot(aes(`_x_`, `_yhat_`)) +
      geom_line(data = as_tibble(obj$cp_profiles),
                aes(x = {{ x }}, group = `_ids_`),
                size = 0.5, alpha = 0.05, color = "gray50")
    
    num_colors <- n_distinct(obj$agr_profiles$`_label_`)
    
    if (num_colors > 1) {
      p <- p + geom_line(aes(color = `_label_`), size = 1.2, alpha = 0.8)
    } else {
      p <- p + geom_line(color = "midnightblue", size = 1.2, alpha = 0.8)
    }
    
    p
  }
  
  
}

# Interpret the model -----------------------------------------------------

# mak a simple custom prediciton function      
custom_predict <- function(model, newdata) {
  x <- predict(model, newdata = newdata)
  return(x)
}

# Create a model explainers for this models
  # first, select the relevant explanatory variables being used in the model
  cols.lm <- c('ph','ndose','clay','cec','soc','temp','prec',
               'crop_cat','fert_cat','measmethod_nh3','appc','fertinhib','GEnZ','exptype','phtype')
  
  # make the model explainer for the training dataset
  explainer.train.lm <- explain(model = m1, 
                                data = as.data.frame(d1[, .SD, .SDcols = cols.lm]), 
                                y = d1$log_ef, 
                                label = "model_train_glm")

# make a Variance Importance Plot (VIP) on training sets

  # make VIP
  imp.lm <- ingredients::feature_importance(explainer.train.lm, 
                                          loss_function = loss_root_mean_square, 
                                          type = "difference")

  # plot the figure (figure has not yet nice formatting)
  p1 <- plot(imp.lm) + theme_bw() + theme(legend.position = 'none') + 
    ggtitle(label='Feature importance',subtitle='created for MA model predicting EF for NH3')+
    ylab('RMSE loss after permutation')
  ggsave(plot = p1, filename='products/model_nh3_vip.png',width = 12,height = 15,units = 'cm')
  
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

  # combine both plots and save
  p2 <- plot.mp + plot.res
  ggsave(plot = p2,filename = 'products/model_nh3_1to1_res.png',width = 14, height = 10, units='cm')

# plot ALE-plots 

  # make explainers for ALE
  ale.ph.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ph')
  ale.temp.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'tmp_mean')
  ale.clay.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'clay')
  ale.cec.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'cec')
  ale.ndose.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'ndose')
  ale.tmp.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'tmp_mean')
  ale.prec.lm <- ingredients::accumulated_dependency(explainer.train.lm, 'pre_mean')
  ale.prec.fertcat <- ingredients::accumulated_dependency(explainer.train.lm, 'fert_cat')
  ale.prec.appc <- ingredients::accumulated_dependency(explainer.train.lm, 'appc')

  #make a combined plot (with the two lines into one figure)
  # note that all variables are scaled to unit variance
  plot.ale <- ggplot_ale(ale.ph.lm,ale.ph2.lm,ale.clay.lm,ale.ndose.lm,ale.ndose2.lm,ale.tmp.lm,ale.prec.lm)
  ggsave(plot = plot.ale,filename = 'products/model_nh3_ale.png',width = 13.16, height = 10.90, units='cm')

  # make plots on original scale
  p1 <- ggplot_ale_notscaled(ale.ph.lm)
  ggsave(plot = p1,filename = 'products/model_nh3_ale_ph.png',width = 13.16, height = 10.90, units='cm')
  p2 <- ggplot_ale_notscaled(ale.ndose.lm)
  ggsave(plot = p2,filename = 'products/model_nh3_ale_ndose.png',width = 13.16, height = 10.90, units='cm')
  p3 <- ggplot_ale_notscaled(ale.prec.fertcat)
  ggsave(plot = p3,filename = 'products/model_nh3_ale_fertcat.png',width = 13.16, height = 10.90, units='cm')
  p4 <- ggplot_ale_notscaled(ale.prec.lm)
  ggsave(plot = p4,filename = 'products/model_nh3_ale_prec.png',width = 13.16, height = 10.90, units='cm')
  p5 <- ggplot_ale_notscaled(ale.prec.appc)
  ggsave(plot = p5,filename = 'products/model_nh3_ale_appc.png',width = 13.16, height = 10.90, units='cm')
  p6<- ggplot_ale_notscaled(ale.tmp.lm)
  ggsave(plot = p6,filename = 'products/model_nh3_ale_temp.png',width = 13.16, height = 10.90, units='cm')
  p7 <- ggplot_ale_notscaled(ale.cec.lm)
  ggsave(plot = p7,filename = 'products/model_nh3_ale_cec.png',width = 13.16, height = 10.90, units='cm')
  p8 <- ggplot_ale_notscaled(ale.clay.lm)
  ggsave(plot = p8,filename = 'products/model_nh3_ale_clay.png',width = 13.16, height = 10.90, units='cm')
  
  pf <- (p1 | p2 | p6 | p4)/(p3 | p5 | p7 | p8)
  ggsave(plot=pf,filename='products/figure_ale_nh3_combi.png',width = 20,height=10)

