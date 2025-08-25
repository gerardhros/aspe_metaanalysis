# building explainers

# require packages
require(data.table)
require(ggplot2)
require(patchwork)
require(DALEX);require(ingredients); require(auditor); require(MASS)
library(nlme); library(MuMIn)

# clean all
rm(list=ls())

# --- prepare datasets ----

  # load in the files (old = 250423, new = 250429)
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
  
  # combine U and UM 
  d5[fert_cat=='UM', fert_cat := 'U']
  
  # variables to standardize
  cols <- c('ph','soc','clay','sand','cec','bd','temp','prec','ndose')
  
  # store median values
  my.summary = function(x) list(mean = mean(x), sd = sd(x))
  db.sum <- d5[,unlist(lapply(.SD,my.summary)),.SDcols = cols]
  saveRDS(db.sum,'products/dbsum.rds')
  
  # standardize numeric columns
  d5[,c(cols) := lapply(.SD,scale),.SDcols = cols]
  
  # standardize crop types, application mode
  d5[grepl('grass',crop_cat),crop_cat := 'grass']
  d5[grepl('arableveg|veget|rice|legum|other|pere',crop_cat),crop_cat := 'non-cereal']
  d5[grepl('solu',appc),appc :='irrigation']
  
  # convert categorial ones to factor
  colsc <- c('fert_cat','crop_cat','appc','nsplitc','phtype','exptype','measmethod','GEnZ')
  d5[,c(colsc) := lapply(.SD,factor),.SDcols = colsc]
  
  # remove extreme high values for precipitation
  d5 <- d5[prec <= 4]
  
  # set back to original files
  d1 <- d5[set=='nh3']
  d2 <- d5[set=='n2o']
  d3 <- d5[set=='no']
  d4 <- d5[set=='no3']

# ---- prepare models for NH3 (m1), N2O (m2), NO (m3) and NO3 (m4) ----

  m1 <-lme(log_ef ~ fert_cat+crop_cat+appc + ph + I(ph^2)+ clay + prec+temp+ph:I(ndose^2)+
             measmethod+nsplitc+GEnZ-1, 
           random = ~ 1 | exptype, weights = varFunc(~ log(ef_sd+0.0001)), 
           data=d1,na.action=na.omit)
  m2 <-lme(log_ef ~ fert_cat+crop_cat+appc + ph+I(ph^2) + temp +I(temp^2)+prec+ 
             ndose +I(ndose^2)+ nsplitc -1, 
           random = ~ 1 |GEnZ/phtype, weights = varFunc(~ log(ef_sd)),  
           data=d2,na.action=na.omit)
  m3 <-lme(log_ef ~ fert_cat+crop_cat+appc + ph+I(ph^2) + temp +I(temp^2)+prec+ soc+clay+
             ndose +I(ndose^2)+ nsplitc -1, 
           random = ~ 1 |GEnZ/phtype, weights = varFunc(~ log(ef_sd)),  
           data=d3,na.action=na.omit)
  m4 <-lme(log_ef ~ fert_cat+ndose+I(ndose^2)+soc  + prec+temp+I(temp^2)+I(prec^2)+crop_cat+nsplitc-1, 
           random = ~ 1 |GEnZ, weights = varFunc(~ 1/n),  
           data=d4,na.action=na.omit)
  
  # get model statistics for the paper
  anova(m1); r.squaredGLMM(m1);AIC(m1)
  anova(m2); r.squaredGLMM(m2);AIC(m2)
  anova(m3); r.squaredGLMM(m3);AIC(m3)
  anova(m4); r.squaredGLMM(m4);AIC(m4)
  summary(lm(as.numeric(predict(m1))~d1$log_ef))
  summary(lm(as.numeric(predict(m2))~d2$log_ef))
  summary(lm(as.numeric(predict(m3))~d3$log_ef))
  summary(lm(as.numeric(predict(m4))~d4$log_ef))
  
  # get coefficients
  m1.coef <- data.table(parm = rownames(t(coefficients(m1)[1,])),
                        value = t(coefficients(m1)[1,]))
  m1.coef[grepl('fert_cat',parm), parameter := 'fertilizer type']
  m1.coef[grepl('crop_cat',parm), parameter := 'crop type']
  m1.coef[grepl('appc',parm), parameter := 'application mode']
  m1.coef[grepl('ph',parm), parameter := 'soil pH']
  m1.coef[grepl('clay',parm), parameter := 'clay content']
  m1.coef[grepl('prec',parm), parameter := 'precipitation']
  m1.coef[grepl('temp',parm), parameter := 'temperature']
  m1.coef[grepl('measmethod',parm), parameter := 'measurement method']
  m1.coef[grepl('nsplitc',parm), parameter := 'number of doses']
  m1.coef[grepl('GEnZ',parm), parameter := 'climate zone']
  m1.coef[grepl('Intercept',parm), parameter := 'intercept']
  m1.coef[grepl('ph:I',parm), parameter := 'interactions']
  m1.coef[,cat := gsub('fert_cat|crop_cat|appc|measmethod|nsplitc|GEnZ','',parm)]
  m1.coef[,model := 'logNH3']
  m1.coef <- m1.coef[,.(model,parameter,cat,coefficient = value.field)]
  
  m2.coef <- data.table(parm = c(rownames(t(coefficients(m2)[1,])),rownames(coefficients(m2)["(Intercept)"])),
                        value = c(t(coefficients(m2)[2,]),as.numeric(unlist(coefficients(m2)["(Intercept)"]))))
  m2.coef[grepl('fert_cat',parm), parameter := 'fertilizer type']
  m2.coef[grepl('crop_cat',parm), parameter := 'crop type']
  m2.coef[grepl('appc',parm), parameter := 'application mode']
  m2.coef[grepl('ph',parm), parameter := 'soil pH']
  m2.coef[grepl('clay',parm), parameter := 'clay content']
  m2.coef[grepl('prec',parm), parameter := 'precipitation']
  m2.coef[grepl('temp',parm), parameter := 'temperature']
  m2.coef[grepl('ndose',parm), parameter := 'N dose']
  m2.coef[grepl('measmethod',parm), parameter := 'measurement method']
  m2.coef[grepl('nsplitc',parm), parameter := 'number of doses']
  m2.coef[grepl('GEnZ',parm), parameter := 'climate zone']
  m2.coef[grepl('Intercept',parm), parameter := 'intercept']
  m2.coef[grepl('ph:I',parm), parameter := 'interactions']
  m2.coef[,cat := gsub('fert_cat|crop_cat|appc|measmethod|nsplitc|GEnZ','',parm)]
  m2.coef[grepl('\\/',parm), parameter := 'random climate and pH type']
  m2.coef[grepl('\\/',parm), cat := gsub('\\/','-',parm)]
  m2.coef[,model := 'logN2O']
  m2.coef <- m2.coef[,.(model,parameter,cat,coefficient = value)]
  
  m3.coef <- data.table(parm = c(rownames(t(coefficients(m3)[1,])),rownames(coefficients(m3)["(Intercept)"])),
                        value = c(t(coefficients(m3)[2,]),as.numeric(unlist(coefficients(m3)["(Intercept)"]))))
  m3.coef[grepl('fert_cat',parm), parameter := 'fertilizer type']
  m3.coef[grepl('crop_cat',parm), parameter := 'crop type']
  m3.coef[grepl('appc',parm), parameter := 'application mode']
  m3.coef[grepl('ph',parm), parameter := 'soil pH']
  m3.coef[grepl('clay',parm), parameter := 'clay content']
  m3.coef[grepl('soc',parm), parameter := 'soil organic carbon content']
  m3.coef[grepl('prec',parm), parameter := 'precipitation']
  m3.coef[grepl('temp',parm), parameter := 'temperature']
  m3.coef[grepl('ndose',parm), parameter := 'N dose']
  m3.coef[grepl('measmethod',parm), parameter := 'measurement method']
  m3.coef[grepl('nsplitc',parm), parameter := 'number of doses']
  m3.coef[grepl('GEnZ',parm), parameter := 'climate zone']
  m3.coef[grepl('Intercept',parm), parameter := 'intercept']
  m3.coef[grepl('ph:I',parm), parameter := 'interactions']
  m3.coef[,cat := gsub('fert_cat|crop_cat|appc|measmethod|nsplitc|GEnZ','',parm)]
  m3.coef[grepl('\\/',parm), parameter := 'random climate and pH type']
  m3.coef[grepl('\\/',parm), cat := gsub('\\/','-',parm)]
  m3.coef[,model := 'logNO']
  m3.coef <- m3.coef[,.(model,parameter,cat,coefficient = value)]
  
  m4.coef <- data.table(parm = c(rownames(t(coefficients(m4)[1,])),rownames(coefficients(m4)["(Intercept)"])),
                        value = c(t(coefficients(m4)[2,]),as.numeric(unlist(coefficients(m4)["(Intercept)"]))))
  m4.coef[grepl('fert_cat',parm), parameter := 'fertilizer type']
  m4.coef[grepl('crop_cat',parm), parameter := 'crop type']
  m4.coef[grepl('appc',parm), parameter := 'application mode']
  m4.coef[grepl('ph',parm), parameter := 'soil pH']
  m4.coef[grepl('clay',parm), parameter := 'clay content']
  m4.coef[grepl('soc',parm), parameter := 'soil organic carbon content']
  m4.coef[grepl('prec',parm), parameter := 'precipitation']
  m4.coef[grepl('temp',parm), parameter := 'temperature']
  m4.coef[grepl('ndose',parm), parameter := 'N dose']
  m4.coef[grepl('measmethod',parm), parameter := 'measurement method']
  m4.coef[grepl('nsplitc',parm), parameter := 'number of doses']
  m4.coef[grepl('GEnZ',parm), parameter := 'climate zone']
  m4.coef[grepl('Intercept',parm), parameter := 'intercept']
  m4.coef[grepl('ph:I',parm), parameter := 'interactions']
  m4.coef[,cat := gsub('fert_cat|crop_cat|appc|measmethod|nsplitc|GEnZ','',parm)]
  m4.coef[grepl('^G|^H|^K|^N|^Q',parm), parameter := 'random climate and pH type']
  m4.coef[grepl('^G|^H|^K|^N|^Q',parm), cat := parm]
  m4.coef[,model := 'logNO3']
  m4.coef <- m4.coef[,.(model,parameter,cat,coefficient = value)]
  
  mall.coeff <- rbind(m1.coef,m2.coef,m3.coef,m4.coef)
  fwrite(mall.coeff,'products/model_coefficients.csv',sep=';',dec=',')
  
# prepare explainers
  
  # parameters lm model
  cols.lm <- c('ph','ndose','clay','cec','soc','temp','prec','crop_cat','fert_cat','nsplitc',
                'appc','GEnZ','phtype','measmethod')
  
  # make the model explainer for the training dataset
  # use explainer only on EU data
  explainer.nh3.lm <- explain(model = m1, data = as.data.frame(d1[region=='EU', .SD, .SDcols = c(cols.lm,'exptype')]), y = d1[region=='EU',log_ef], label = "")
  explainer.n2o.lm <- explain(model = m2, data = as.data.frame(d2[region=='EU', .SD, .SDcols = cols.lm]), y = d2[region=='EU',log_ef], label = "")
  explainer.no.lm  <- explain(model = m3, data = as.data.frame(d3[region=='EU', .SD, .SDcols = cols.lm]), y = d3[region=='EU',log_ef], label = "")
  explainer.no3.lm <- explain(model = m4, data = as.data.frame(d4[region=='EU', .SD, .SDcols = cols.lm]), y = d4[region=='EU',log_ef], label = "")

  # use explainer on all data
  explainer.nh3.lm <- explain(model = m1, data = as.data.frame(d1[, .SD, .SDcols = c(cols.lm,'exptype')]), y = d1[,log_ef], label = "")
  explainer.n2o.lm <- explain(model = m2, data = as.data.frame(d2[, .SD, .SDcols = cols.lm]), y = d2[,log_ef], label = "")
  explainer.no.lm  <- explain(model = m3, data = as.data.frame(d3[, .SD, .SDcols = cols.lm]), y = d3[,log_ef], label = "")
  explainer.no3.lm <- explain(model = m4, data = as.data.frame(d4[, .SD, .SDcols = cols.lm]), y = d4[,log_ef], label = "")
  
  # make a Variance Importance Plot (VIP) on training sets
  imp.nh3.lm <- ingredients::feature_importance(explainer.nh3.lm, loss_function = loss_root_mean_square, type = "difference")
  imp.n2o.lm <- ingredients::feature_importance(explainer.n2o.lm, loss_function = loss_root_mean_square, type = "difference")
  imp.no.lm <- ingredients::feature_importance(explainer.no.lm, loss_function = loss_root_mean_square, type = "difference")
  imp.no3.lm <- ingredients::feature_importance(explainer.no3.lm, loss_function = loss_root_mean_square, type = "difference")
  p1 <- ggplot_vip(imp.nh3.lm,imp.n2o.lm,imp.no.lm,imp.no3.lm,vexclude='exptype',ftitle=c('nh3','n2o','no','no3'))
  p2 <- ggplot_vip(imp.nh3.lm,imp.n2o.lm,imp.no.lm,imp.no3.lm,vexclude='exptype',ftitle=c('nh3','n2o','no','no3'),plotrow=2)
  
  ggsave(plot=p1,filename='products/figure_vip_all3_eu.png',width = 34,height=13,dpi=600)
  ggsave(plot=p2,filename='products/figure_vip_all3_eu_2rows.png',width = 15,height=20,dpi=600)
  
  ggsave(plot=p1,filename='products/figure_vip_all3_global.png',width = 34,height=13,dpi=600)
  
  
  # make ALE for fertilizer category
  set.seed(152)
  ale.nh3.fertcat <- ingredients::accumulated_dependency(explainer.nh3.lm, 'fert_cat')
  ale.n2o.fertcat <- ingredients::accumulated_dependency(explainer.n2o.lm, 'fert_cat')
  ale.no.fertcat <- ingredients::accumulated_dependency(explainer.no.lm, 'fert_cat')
  ale.no3.fertcat <- ingredients::accumulated_dependency(explainer.no3.lm, 'fert_cat')
  p1 <- ggplot_ale(ale.nh3.fertcat,ale.n2o.fertcat,ale.no.fertcat,ale.no3.fertcat,
                   ftitle=c('nh3','n2o','no','no3'),pncol = 2)
  ggsave(plot=p1,filename='products/figure_ale_fertcat3.png',width = 30,height=15,dpi=600)
  
  # make 1-to-1 and histograms
  res.lm.nh3 <- auditor::model_residual(explainer.nh3.lm)  
  plot.nh3.res <- ggplot_hist(res.lm.nh3) + ggtitle(expression(paste("Residual plot for log-EF N-NH"[3])))
  plot.nh3.mp <- ggplot_onetoone(res.lm.nh3) + ggtitle(expression(paste("Residual plot for log-EF N-NH"[3])))
  res.lm.n2o <- auditor::model_residual(explainer.n2o.lm)  
  plot.n2o.res <- ggplot_hist(res.lm.n2o) + ggtitle(expression(paste("Residual plot for log-EF N-N"[2],"O")))
  plot.n2o.mp <- ggplot_onetoone(res.lm.n2o)+ ggtitle(expression(paste("Residual plot for log-EF N-N"[2],"O")))
  res.lm.no <- auditor::model_residual(explainer.no.lm)  
  plot.no.res <- ggplot_hist(res.lm.no) + ggtitle(expression(paste("Residual plot for log-EF N-NO")))
  plot.no.mp <- ggplot_onetoone(res.lm.no)+ ggtitle(expression(paste("Residual plot for log-EF N-NO")))
  res.lm.no3 <- auditor::model_residual(explainer.no3.lm)  
  plot.no3.res <- ggplot_hist(res.lm.no3) + ggtitle(expression(paste("Residual plot for log-EF N-NO"[3])))
  plot.no3.mp <- ggplot_onetoone(res.lm.no3)+ ggtitle(expression(paste("Residual plot for log-EF N-NO"[3])))
  p5 <- list(plot.nh3.res,plot.n2o.res,plot.no.res,plot.no3.res,
             plot.nh3.mp,plot.n2o.mp,plot.no.mp,plot.no3.mp)
  p6 <- patchwork::wrap_plots(p5, ncol=4,axis_titles = 'collect') 
  
  ggsave(plot=p6,filename='products/figure_residual_efs_v3_complete.png',width = 25,height=17,dpi=600)
  
    # make ALE for site properties
  set.seed(152)
  ale.nh3.prec <- ingredients::accumulated_dependency(explainer.nh3.lm, 'prec')
  ale.n2o.prec <- ingredients::accumulated_dependency(explainer.n2o.lm, 'prec')
  ale.no.prec <- ingredients::accumulated_dependency(explainer.no.lm, 'prec')
  ale.no3.prec <- ingredients::accumulated_dependency(explainer.no3.lm, 'prec')
  set.seed(152)
  ale.nh3.temp <- ingredients::accumulated_dependency(explainer.nh3.lm, 'temp')
  ale.n2o.temp <- ingredients::accumulated_dependency(explainer.n2o.lm, 'temp')
  ale.no.temp <- ingredients::accumulated_dependency(explainer.no.lm, 'temp')
  ale.no3.temp <- ingredients::accumulated_dependency(explainer.no3.lm, 'temp')
  set.seed(152)
  ale.nh3.ndose <- ingredients::accumulated_dependency(explainer.nh3.lm, 'ndose')
  ale.n2o.ndose <- ingredients::accumulated_dependency(explainer.n2o.lm, 'ndose')
  ale.no.ndose <- ingredients::accumulated_dependency(explainer.no.lm, 'ndose')
  ale.no3.ndose <- ingredients::accumulated_dependency(explainer.no3.lm, 'ndose')
  set.seed(152)
  ale.nh3.ph <- ingredients::accumulated_dependency(explainer.nh3.lm, 'ph')
  ale.n2o.ph <- ingredients::accumulated_dependency(explainer.n2o.lm, 'ph')
  ale.no.ph <- ingredients::accumulated_dependency(explainer.no.lm, 'ph')
  ale.no3.ph <- ingredients::accumulated_dependency(explainer.no3.lm, 'ph')
  set.seed(152)
  ale.nh3.clay <- ingredients::accumulated_dependency(explainer.nh3.lm, 'clay')
  ale.n2o.clay <- ingredients::accumulated_dependency(explainer.n2o.lm, 'clay')
  ale.no.clay <- ingredients::accumulated_dependency(explainer.no.lm, 'clay')
  ale.no3.clay <- ingredients::accumulated_dependency(explainer.no3.lm, 'clay')
  set.seed(152)
  ale.nh3.soc <- ingredients::accumulated_dependency(explainer.nh3.lm, 'soc')
  ale.n2o.soc <- ingredients::accumulated_dependency(explainer.n2o.lm, 'soc')
  ale.no.soc <- ingredients::accumulated_dependency(explainer.no.lm, 'soc')
  ale.no3.soc <- ingredients::accumulated_dependency(explainer.no3.lm, 'soc')
  p1 <- ggplot_ale(ale.nh3.prec,ale.n2o.prec,ale.no.prec,ale.no3.prec,
                   ale.nh3.temp,ale.n2o.temp,ale.no.temp,ale.no3.temp,
                   ale.nh3.ndose,ale.n2o.ndose,ale.no.ndose,ale.no3.ndose,
                   ale.nh3.ph,ale.n2o.ph,ale.no.ph,ale.no3.ph,
                   ale.nh3.clay,ale.n2o.clay,ale.no.clay,ale.no3.clay,
                   ale.nh3.soc,ale.n2o.soc,ale.no.soc,ale.no3.soc,
                   ftitle=rep(c('nh3','n2o','no','no3'),6),pncol = 2)
  ggsave(plot=p1,filename='products/figure_ale_numeric_v3_complete.png',width = 20,height=20,dpi=440)
  ggsave(plot=p1,filename='products/figure_ale_numeric_v3_eu.png',width = 20,height=20,dpi=440)
  
  # make ALE for categorial variables
  set.seed(152)
  ale.nh3.crop <- ingredients::accumulated_dependency(explainer.nh3.lm, 'crop_cat')
  ale.n2o.crop <- ingredients::accumulated_dependency(explainer.n2o.lm, 'crop_cat')
  ale.no.crop <- ingredients::accumulated_dependency(explainer.no.lm, 'crop_cat')
  ale.no3.crop <- ingredients::accumulated_dependency(explainer.no3.lm, 'crop_cat')
  set.seed(152)
  ale.nh3.appc <- ingredients::accumulated_dependency(explainer.nh3.lm, 'appc')
  ale.n2o.appc <- ingredients::accumulated_dependency(explainer.n2o.lm, 'appc')
  ale.no.appc <- ingredients::accumulated_dependency(explainer.no.lm, 'appc')
  ale.no3.appc <- ingredients::accumulated_dependency(explainer.no3.lm, 'appc')
  set.seed(152)
  ale.nh3.nsplitc <- ingredients::accumulated_dependency(explainer.nh3.lm, 'nsplitc')
  ale.n2o.nsplitc <- ingredients::accumulated_dependency(explainer.n2o.lm, 'nsplitc')
  ale.no.nsplitc <- ingredients::accumulated_dependency(explainer.no.lm, 'nsplitc')
  ale.no3.nsplitc <- ingredients::accumulated_dependency(explainer.no3.lm, 'nsplitc')
  
  p1 <- ggplot_ale(ale.nh3.crop,ale.n2o.crop,ale.no.crop,ale.no3.crop,
                   ale.nh3.appc,ale.n2o.appc,ale.no.appc,ale.no3.appc,
                   ale.nh3.nsplitc,ale.n2o.nsplitc,ale.no.nsplitc,ale.no3.nsplitc,
                   ftitle=rep(c('nh3','n2o','no','no3'),3),pncol = 4)
  ggsave(plot=p1,filename='products/figure_ale_cat3_eu.png',width = 40,height=15,dpi=600)
  
  
# prediction for Europe
  
  
  
  
  # parameters lm model
  cols.lm <- c('ph','ndose','clay','cec','soc','temp','prec','crop_cat','fert_cat','nsplitc',
               'appc','GEnZ','phtype','measmethod')