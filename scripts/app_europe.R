# load in libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
require(data.table)

# clear environment
rm(list=ls())

# set theme
theme_set(theme_bw())

if(FALSE){
  
  # get the raster to plot
  r1 <- terra::rast('D:/Wageningen University & Research/CHANS-EU - INTEGRATOR/data/ncu_integrator_maddy2010_data/raw/gncu2010_ext.asc')
  
  # updated line to set the crs
  terra::crs(r1) <- 'epsg:3035'
  r1 <- terra::project(r1,'epsg:4326',method='near')
  r1.p <- as.data.frame(r1,xy=TRUE)
  r1.p <- as.data.table(r1.p)
  
  saveRDS(r1.p,'products/nculocs.rds')
  
} else {
  
  r1.p <- readRDS('products/nculocs.rds')
  r1.p.cov <- readRDS('products/250425 covariates integrator.rds')
  r1.p.cov <- r1.p.cov[,.(gncu2010_ext,bdod_mean_0_5,cec_mean_0_5,clay_mean_0_5,phw_mean_0_5,sand_mean_0_5,
                          soc_mean_0_5,pre_mean,tmp_mean,GEnZ,GEnS)]
}

# read in the earlier saved database from integrator
n1 <- fread('D:/Wageningen University & Research/CHANS-EU - INTEGRATOR/data/data_keq_ncu.csv')
cols <- c('yield','ph_cacl','clay','ps','temp','manu_n','fert_nh4','fert_no3','pom')
n2 <- n1[,lapply(.SD,function(x) weighted.mean(x, w = crop_area_km,na.rm=T)),.SDcols = cols,by='ncu']
n2[,soc := pom*0.5]
n2[,ndm := manu_n * 14]
n2[,nkm := fert_nh4*2*14]

# merge with covariates
n2 <- merge(n2,r1.p.cov,by.x='ncu',by.y = 'gncu2010_ext',all.x=TRUE)

# replace missing ones
n2[is.na(soc), soc := soc_mean_0_5 * 0.01]
n2[,prec := pre_mean * 12]
n2[is.na(temp),temp := tmp_mean]
n2[is.na(clay),clay := clay_mean_0_5*0.1]
n2[,cec := cec_mean_0_5]
n2[,sand := sand_mean_0_5*0.1]
n2[,bd := bdod_mean_0_5*0.01]
n2[,ph := 2.23 + 0.777 * ((ph_cacl - 0.5262)/0.9288)]
n2[is.na(ph),ph := phw_mean_0_5*0.1]
n2[,ndose := nkm]

# compete set, add missing values
n2 <- n2[,.(ncu,yield,ph,clay,sand,soc,cec,bd,temp,prec,GEnZ,ndose)]
n2[,ph := nafill(ph,type='locf')]
n2[,clay := nafill(clay,type='locf')]
n2[,sand := nafill(sand,type='locf')]
n2[,soc := nafill(soc,type='locf')]
n2[,bd := nafill(bd,type='locf')]
n2[,cec := nafill(cec,type='locf')]
n2[,temp := nafill(temp,type='locf')]
n2[,prec := nafill(prec,type='locf')]
n2[,GEnZ2 := data.table::shift(GEnZ,n=-1)]
n2[is.na(GEnZ),GEnZ := GEnZ2]
n2[,GEnZ2 := data.table::shift(GEnZ,n=1)]
n2[is.na(GEnZ),GEnZ := GEnZ2]
n2[,GEnZ2 := NULL]
n2[is.na(GEnZ),GEnZ := 'J']

db.sum <- readRDS('products/dbsum.rds')
db.sum <- as.data.table(t(db.sum))

n2[,ph := (ph - db.sum$ph.mean)/db.sum$ph.sd]
n2[,soc := (soc - db.sum$soc.mean)/db.sum$soc.sd]
n2[,clay := (clay - db.sum$clay.mean)/db.sum$clay.sd]
n2[,sand := (sand - db.sum$sand.mean)/db.sum$sand.sd]
n2[,cec := (cec - db.sum$cec.mean)/db.sum$cec.sd]
n2[,bd := (bd - db.sum$bd.mean)/db.sum$bd.sd]
n2[,temp := (temp - db.sum$temp.mean)/db.sum$temp.sd]
n2[,prec := (prec - db.sum$prec.mean)/db.sum$prec.sd]
n2[,ndose := (ndose - db.sum$ndose.mean)/db.sum$ndose.sd]

n2[,fert_cat := factor("AS",levels = levels(factor(d1$fert_cat)))]
n2[,crop_cat := factor('cereal',levels=levels(factor(d1$crop_cat)))]
n2[,appc := factor('broadcast',levels=levels(factor(d1$appc)))]
n2[,nsplitc := factor('1',levels=levels(factor(d1$nsplitc)))]
n2[,GEnZ := factor(GEnZ,levels=levels(factor(d1$GEnZ)))]
n2[,phtype := factor('water',levels=levels(factor(d1$phtype)))]
n2[,exptype := factor('field',levels=levels(factor(d1$exptype)))]
n2[,measmethod := factor('D',levels=levels(factor(d1$measmethod)))]
n2[is.na(GEnZ),GEnZ := "J"]

n2.nh3 <- copy(n2)
n2.nh3[, ef_nh3_as := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("AS",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_asc := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("ASC",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_u := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("U",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_an := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("AN",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_uc := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("UC",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_ap := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("DAP-MAP",levels = levels(d1$fert_cat))])))]
n2.nh3[,appc :=  factor('incorporated',levels=levels(factor(d1$appc)))]
n2.nh3[,nsplitc := factor('2',levels=levels(factor(d1$nsplitc)))]
n2.nh3[, ef_nh3_as_bp := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("AS",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_asc_bp := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("ASC",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_u_bp := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("U",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_an_bp := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("AN",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_uc_bp := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("UC",levels = levels(d1$fert_cat))])))]
n2.nh3[, ef_nh3_ap_bp := as.numeric(exp(predict(m1,newdata=n2.nh3[,fert_cat := factor("DAP-MAP",levels = levels(d1$fert_cat))])))]

n2.n2o <- copy(n2)
n2.n2o[, ef_n2o_as := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("AS",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_asc := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("ASC",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_u := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("U",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_an := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("AN",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_uc := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("UC",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_ap := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("DAP-MAP",levels = levels(d2$fert_cat))])))]
n2.n2o[,appc :=  factor('combi',levels=levels(factor(d2$appc)))]
n2.n2o[,nsplitc := factor('3',levels=levels(factor(d2$nsplitc)))]
n2.n2o[, ef_n2o_as_bp := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("AS",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_asc_bp := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("ASC",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_u_bp := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("U",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_an_bp := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("AN",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_uc_bp := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("UC",levels = levels(d2$fert_cat))])))]
n2.n2o[, ef_n2o_ap_bp := as.numeric(exp(predict(m2,newdata=n2.n2o[,fert_cat := factor("DAP-MAP",levels = levels(d2$fert_cat))])))]

n2.no <- copy(n2)

n2.no[,appc := factor('unknown',levels= levels(d3$appc))]
n2.no[, ef_no_as := as.numeric(exp(predict(m3,newdata=n2.no[,fert_cat := factor("AS",levels = levels(d3$fert_cat))])))]
n2.no[, ef_no_asc := as.numeric(exp(predict(m3,newdata=n2.no[,fert_cat := factor("ASC",levels = levels(d3$fert_cat))])))]
n2.no[, ef_no_u := as.numeric(exp(predict(m3,newdata=n2.no[,fert_cat := factor("U",levels = levels(d3$fert_cat))])))]
n2.no[, ef_no_an := as.numeric(exp(predict(m3,newdata=n2.no[,fert_cat := factor("AN",levels = levels(d3$fert_cat))])))]
n2.no[, ef_no_uc := as.numeric(exp(predict(m3,newdata=n2.no[,fert_cat := factor("UC",levels = levels(d3$fert_cat))])))]
n2.no[, ef_no_ap := 0]
n2.no[, ef_no_as_bp := ef_no_as]
n2.no[, ef_no_asc_bp := ef_no_asc]
n2.no[, ef_no_u_bp := ef_no_u]
n2.no[, ef_no_an_bp := ef_no_an]
n2.no[, ef_no_uc_bp := ef_no_uc]
n2.no[, ef_no_ap_bp := ef_no_ap]

n2.no3 <- copy(n2)

n2.no3[, GEnZ := 'K']
n2.no3[, ef_no3_as := as.numeric(exp(pmin(0,predict(m4,newdata=n2.no3[,fert_cat := factor("AS",levels = levels(d4$fert_cat))],level=0))))]
n2.no3[, ef_no3_u := as.numeric(exp(pmin(0,predict(m4,newdata=n2.no3[,fert_cat := factor("U",levels = levels(d4$fert_cat))],level=0))))]
n2.no3[, ef_no3_an := as.numeric(exp(pmin(0,predict(m4,newdata=n2.no3[,fert_cat := factor("AN",levels = levels(d4$fert_cat))],level=0))))]
n2.no3[, ef_no3_uc := as.numeric(exp(pmin(0,predict(m4,newdata=n2.no3[,fert_cat := factor("UC",levels = levels(d4$fert_cat))],level=0))))]
n2.no3[, ef_no3_ap := 0]
n2.no3[,nsplitc := factor('3',levels=levels(factor(d4$nsplitc)))]
n2.no3[, ef_no3_as_bp := as.numeric(exp(pmin(0,predict(m4,newdata=n2.no3[,fert_cat := factor("AS",levels = levels(d4$fert_cat))],level=0))))]
n2.no3[, ef_no3_u_bp := as.numeric(exp(pmin(0,predict(m4,newdata=n2.no3[,fert_cat := factor("U",levels = levels(d4$fert_cat))],level=0))))]
n2.no3[, ef_no3_an_bp := as.numeric(exp(pmin(0,predict(m4,newdata=n2.no3[,fert_cat := factor("AN",levels = levels(d4$fert_cat))],level=0))))]
n2.no3[, ef_no3_uc_bp := as.numeric(exp(pmin(0,predict(m4,newdata=n2.no3[,fert_cat := factor("UC",levels = levels(d4$fert_cat))],level=0))))]
n2.no3[, ef_no3_ap_bp := ef_no3_ap]
n2.no3

n3 <- rbind(n2.n2o,n2.nh3,n2.no,n2.no3,fill=TRUE)

cols.nh3 <- colnames(n2.nh3)[grepl('ncu|^ef',colnames(n2.nh3))]
cols.n2o <- colnames(n2.n2o)[grepl('ncu|^ef',colnames(n2.n2o))]
cols.no <- colnames(n2.no)[grepl('ncu|^ef',colnames(n2.no))]
cols.no3 <- colnames(n2.no3)[grepl('ncu|^ef',colnames(n2.no3))]

n3 <- n2[,.(ncu,ndose,ph)]
n3 <- merge(n3,n2.nh3[,mget(cols.nh3)],by='ncu',all.x=TRUE)
n3 <- merge(n3,n2.n2o[,mget(cols.n2o)],by='ncu',all.x=TRUE)
n3 <- merge(n3,n2.no[,mget(cols.no)],by='ncu',all.x=TRUE)
n3 <- merge(n3,n2.no3[,mget(cols.no3)],by='ncu',all.x=TRUE)

# calculate stats
my.summary = function(x) list(mean = round(mean(x,na.rm=T)*100,2),median = round(median(x,na.rm=T)*100,2), q1 = round(quantile(x,0.1,na.rm=T)*100,2),q9 =round(quantile(x,0.9,na.rm=T)*100,2))
cols <- colnames(n3)[grepl('^ef',colnames(n3))]
eu.sum <- n3[,(lapply(.SD,my.summary)),.SDcols = cols]
eu.sum <- eu.sum[,lapply(.SD,as.numeric)]
eu.sum <- data.table(name = names(eu.sum), 
                     pmean = as.numeric(eu.sum[1,]),
                     pmedian = as.numeric(eu.sum[2,]),
                     q1 = as.numeric(eu.sum[3,]),
                     q9=as.numeric(eu.sum[4,]))
eu.sum[,parm := gsub('ef_|_as|_asc|_u|_uc|_an|_ap','',name)]
eu.sum[,em := tstrsplit(name,'_',keep=3)]
eu.sum[grepl('as$',name)]
eu.sum[grepl('u$',name)]
eu.sum[grepl('an$',name)]
eu.sum[grepl('as_bp$',name)]

# relative change compared to urea
eu.sum[grepl('nh3$',parm),.(name,reductie = round(100*(1-pmean/pmean[em=='u']),1))]
eu.sum[grepl('n2o$',parm),.(name,reductie = round(100*(1-pmean/pmean[em=='u']),1))]
eu.sum[grepl('no$',parm),.(name,reductie = round(100*(1-pmean/pmean[em=='u']),1))]


# impact of 4R principles
eu.sum[grepl('nh3',parm) & !grepl('_bp',parm),pmean] -eu.sum[grepl('nh3',parm) & grepl('_bp',parm),pmean] 
eu.sum[grepl('n2o',parm) & !grepl('_bp',parm),pmean] -eu.sum[grepl('n2o',parm) & grepl('_bp',parm),pmean] 


obs.sum <- d5[,my.summary(ef_mean),by=c('set','fert_cat')]
obs.sum[grepl('AS$',fert_cat)]

# total N losses occuring, compared to urea
cols <- colnames(n3)[grepl('ncu|ph|ef_',colnames(n3))]
n4 <- n3[,mget(cols)]
n4 <- melt(n4,id.vars = c('ncu','ph'))
n4[,fert := tstrsplit(variable,'_',keep=3)]
n4[,em :=tstrsplit(variable,'_',keep=2)]
n4[,mef := fifelse(value==min(value[value>0]),1,0),by= c('ncu','em')]

# reductie from fertilizer type only per emission type
n4.ferttype <- copy(n4[!grepl('_bp',variable)])

  # estimate absolute and relative change in EF (%) for each fertilizer and N loss separate, compared to urea
  n4.ferttype[,ef_red := round(-1*100*(value[fert=='u'] - value),3),by= c('ncu','em')]
  n4.ferttype[,ef_redr := round(100*(value[fert=='u'] - value)/value[fert=='u'],3),by= c('ncu','em')]

  # wat is maximum absolute and relative reduction per N loss, over all fertilizers
  n4.ferttype[,ef_red_max_value := min(ef_red[value>0]),by=c('ncu','em')]
  n4.ferttype[,ef_redr_max_value := max(ef_redr[value>0]),by=c('ncu','em')]

  # what fertilizer gives the highest reduction per N loss
  n4.ferttype[,ef_red_max_ft := fert[ef_red == min(ef_red)][1],by=c('ncu','em')]
  
  # what is the total gas emission reduction per fertilizer type (absolute percentage)
  n4.ferttype[,ef_red_emgas_value := sum(ef_red[em %in% c('nh3','n2o','no') & value>0]),by=c('ncu','fert')]
  
  # total gas emission per fertilizer
  n4.ferttype[,ef_emgas_value := sum(value[em %in% c('nh3','n2o','no') & value > 0]),by= c('ncu','fert')]
  n4.ferttype[,ef_redr_emgas_value := round(100*(ef_emgas_value[fert=='u'] - ef_emgas_value)/ef_emgas_value[fert=='u'],3),by=c('ncu','fert')]

  # total maximum emission reduction (abs and rel)
  n4.ferttype[,ef_red_emgas_max_value := min(ef_red_emgas_value),by='ncu']
  n4.ferttype[,ef_redr_emgas_max_value := max(ef_redr_emgas_value),by='ncu']
  
  # what fertilizer gives the highest reduction for all gaseous N loss
  n4.ferttype[,ef_red_emgas_max_ft := fert[ef_red_emgas_value == min(ef_red_emgas_value)][1],by=c('ncu')]

  # summary stats for the overall total N emission reduction of gasesous losses
  n4.ferttype.s1 <- unique(n4.ferttype[,.(ncu,ef_red_emgas_max_value,ef_redr_emgas_max_value,ef_red_emgas_max_ft)])
  n4.ferttype.s1[,.(abs_q01 = quantile(ef_red_emgas_max_value,0.01,na.rm=T),
                    abs_q50 = quantile(ef_red_emgas_max_value,0.50,na.rm=T),
                    abs_q99 = quantile(ef_red_emgas_max_value,0.99,na.rm=T),
                    rel_q01 = quantile(ef_redr_emgas_max_value,0.01,na.rm=T),
                    rel_q50 = quantile(ef_redr_emgas_max_value,0.50,na.rm=T),
                    rel_q99 = quantile(ef_redr_emgas_max_value,0.99,na.rm=T))]
  
  # summary stats for the max N emission reduction per N loss
  n4.ferttype.s2 <- unique(n4.ferttype[,.(ncu,ef_red_max_value,ef_redr_max_value,ef_red_max_ft,em)])
  n4.ferttype.s2[,.(abs_q01 = quantile(ef_red_max_value,0.01,na.rm=T),
                    abs_q50 = quantile(ef_red_max_value,0.50,na.rm=T),
                    abs_q99 = quantile(ef_red_max_value,0.99,na.rm=T),
                    rel_q01 = quantile(ef_redr_max_value,0.01,na.rm=T),
                    rel_q50 = quantile(ef_redr_max_value,0.50,na.rm=T),
                    rel_q99 = quantile(ef_redr_max_value,0.99,na.rm=T)),by='em']
  
  # summary stats for the overall total N emission reduction of gasesous losses
  n4.ferttype.s3 <- unique(n4.ferttype[,.(ncu,ef_red,ef_redr,em,fert)])
  n4.ferttype.s3[,.(abs_q01 = quantile(ef_red,0.01,na.rm=T),
                    abs_q50 = quantile(ef_red,0.50,na.rm=T),
                    abs_q99 = quantile(ef_red,0.99,na.rm=T),
                    rel_q01 = quantile(ef_redr,0.01,na.rm=T),
                    rel_q50 = quantile(ef_redr,0.50,na.rm=T),
                    rel_q99 = quantile(ef_redr,0.99,na.rm=T)),by=c('em','fert')][em =='nh3',.(fert,rel_q50)]
  
  # summary stats for Table 1
  n4.ferttype.s3 <- n4.ferttype[em !='no3',.(abs_q01 = round(quantile(value*100,0.01,na.rm=T),2),
                                             abs_q50 = round(quantile(value*100,0.50,na.rm=T),2),
                                             mean = round(mean(value*100,na.rm=T),2),
                                             abs_q99 = round(quantile(value*100, 0.99,na.rm=T),2)),by=c('em','fert')]
  
# table S2E 
  d5[,phs := ph *  db.sum$ph.sd + db.sum$ph.mean]
  d5[, phclass := fifelse(phs<6.5,'acidic','calcareous')]
  tab2se <- d5[,list(median = round(median(ef_mean*100),2),
                 mean =  round(mean(ef_mean * 100),2),
                 mean_phlow = round(mean(ef_mean[phclass =='acidic'] * 100,na.rm=T),2),
                 mean_phhigh = round(mean(ef_mean[phclass =='calcareous'] * 100,na.rm=T),2),
                 q1 =  round(quantile(ef_mean * 100, 0.25),2),
                 q3 =  round(quantile(ef_mean * 100, .75),2),
                 n = .N),by=c('set','fert_cat')]
  tab2se <- dcast(tab2se,fert_cat~set,value.var = c('median','mean', 'mean_phlow','mean_phhigh','q1','q3','n')) 
  cols <- paste0(rep(c('median','mean','mean_phlow','mean_phhigh','q1','q3','n'),4),rep(c('_nh3','_n2o','_no','_no3'),each = 7))
  setcolorder(tab2se,c('fert_cat',cols))
  tab2se[fert_cat %in% c('U','AN','AS')]
  
  # modelled for Europe
  db.sum <- as.data.frame(t(db.sum))
  n4[,phs := ph *  db.sum$ph.sd + db.sum$ph.mean]
  n4[, phclass := fifelse(phs<6.5,'acidic','calcareous')]
  tab1b <- n4[!grepl('_bp',variable),
              list(median = round(median(value*100,na.rm=T),2),
                   mean =  round(mean(value * 100,na.rm=T),2),
                   mean_phlow = round(mean(value[phclass =='acidic'] * 100,na.rm=T),2),
                   mean_phhigh = round(mean(value[phclass =='calcareous'] * 100,na.rm=T),2),
                   q1 =  round(quantile(value * 100, 0.25,na.rm=T),2),
                   q3 =  round(quantile(value * 100, .75,na.rm=T),2),
                   n = .N),by=c('em','fert')]
  tab1b <- dcast(tab1b,fert~em,value.var = c('median','mean','mean_phlow','mean_phhigh', 'q1','q3','n')) 
  cols <- paste0(rep(c('median','mean','mean_phlow','mean_phhigh','q1','q3','n'),4),rep(c('_nh3','_n2o','_no','_no3'),each = 7))
  setcolorder(tab1b,c('fert',cols))
  tab1b[fert %in% c('u','an','as')][c(3,1,2),.(fert,mean_nh3,mean_phlow_nh3,mean_phhigh_nh3,mean_n2o,mean_no,mean_no3)]
  
  tab1c <- n4[grepl('_bp',variable),
              list(median = round(median(value*100,na.rm=T),2),
                   mean =  round(mean(value * 100,na.rm=T),2),
                   mean_phlow = round(mean(value[phclass =='acidic'] * 100,na.rm=T),2),
                   mean_phhigh = round(mean(value[phclass =='calcareous'] * 100,na.rm=T),2),
                   q1 =  round(quantile(value * 100, 0.25,na.rm=T),2),
                   q3 =  round(quantile(value * 100, .75,na.rm=T),2),
                   n = .N),by=c('em','fert')]
  tab1c <- dcast(tab1c,fert~em,value.var = c('median','mean','mean_phlow','mean_phhigh', 'q1','q3','n')) 
  cols <- paste0(rep(c('median','mean','mean_phlow','mean_phhigh','q1','q3','n'),4),rep(c('_nh3','_n2o','_no','_no3'),each = 7))
  setcolorder(tab1c,c('fert',cols))
  tab1c[fert %in% c('u','an','as')][c(3,1,2),.(fert,mean_nh3,mean_phlow_nh3,mean_phhigh_nh3,mean_n2o,mean_no,mean_no3)]
  
  
  
n2[,phs := ph *  db.sum$ph.sd + db.sum$ph.mean]
n2[, phclass := fifelse(phs<7,'acidic','calcareous')]
n2[,list(u = mean(ef_nh3_u)*100,
         an = mean(ef_nh3_an)*100,
         as = mean(ef_nh3_as)*100),by=phclass]

d1[,phs := ph *  db.sum$ph.sd + db.sum$ph.mean]
d1[, phclass := fifelse(phs<6.5,'acidic','calcareous')]
d1[,mean(ef_mean)*100,by=.(phclass,fert_cat)][fert_cat %in% c('U','AN','AS')]

# make plots for emission fractions
r.ncu <- merge(r1.p, n2, by.x = 'gncu2010_ext', by.y = 'ncu',all.x=TRUE)
cols <- colnames(r.ncu)[grepl('^ef',colnames(r.ncu))]
r.ncu[, c(cols):= lapply(.SD,function(x) round(x * 100,2)),.SDcols = cols]

setcolorder(r.ncu, c('x', 'y', 'gncu2010_ext'))
r.ncu <- r.ncu[!is.na(ph)]
s_nuts <- st_read("D:/Wageningen University & Research/CHANS-EU - INTEGRATOR/data/nuts/products/eu_nuts.gpkg",layer="eu_nuts")

r.ncu.emr <- merge(r1.p, n4, by.x = 'gncu2010_ext', by.y = 'ncu',all.x=TRUE)
setcolorder(r.ncu.emr, c('x', 'y', 'gncu2010_ext'))
r.ncu.emr <- r.ncu.emr[!is.na(ef_red_tot)]

ggplot_eu_map <- function(sf_nuts,r_ncu,variable,pbreak=NULL,plabel = NULL,ptitle= NULL, pf = 'AS',ft = 1){
  
  r.int <- copy(r_ncu)
  r.int[,var := get(variable)]
  r.int <- r.int[!is.na(var)]
  
  if(is.null(pbreak)){pbreak <- c(-Inf,1,2,4,8,16,Inf)}
  if(is.null(plabel)){plabel <- c('<1','1-2','2-4','4-8','8-16','>16')}
  if(ft==1){
    if(is.null(ptitle)){ptitle <- 'Emission fraction'}
    fname = paste0('EF of ',pf, ' (%)')
    if(ptitle=='nh3') ptitle <- substitute(paste(a," for N-NH"[3]),list(a=fname))
    if(ptitle=='n2o') ptitle <- substitute(paste(a," for N-N"[2],"O"),list(a=fname))
    if(ptitle=='no') ptitle <- substitute(paste(a," for N-NO"),list(a=fname))
    if(ptitle=='no3') ptitle <- substitute(paste(a," for N-NO"[3]),list(a=fname))
  }
  
  if(ft==2){
    
    fname = paste0('Reduction (%)')
    if(ptitle=='nh3') ptitle <- substitute(paste(a," for NH"[3]),list(a=fname))
    if(ptitle=='n2o') ptitle <- substitute(paste(a," for N"[2],"O"),list(a=fname))
    if(ptitle=='no') ptitle <- substitute(paste(a," for NO"),list(a=fname))
    if(ptitle=='no3') ptitle <- substitute(paste(a," for NO"[3]),list(a=fname))
    if(ptitle=='all') ptitle <- substitute(paste(a," for NH"[3]," N"[2],"O, and NO"),list(a=fname))
  }
  p1 <- ggplot() +
        geom_sf(data=sf_nuts,color='black',fill=NA,show.legend = FALSE)+
        geom_tile(data = r.int,aes(x=x,y=y,fill= cut(var,breaks = pbreak,label = plabel)))+ 
        scale_fill_viridis_d(direction = -1, na.value = "white",na.translate = F) +
        xlab("") + ylab("") +
        labs(fill ='EF')+
        ggtitle(ptitle) +
        coord_sf(crs = 4326, lims_method = "box") + theme_classic()+
        theme(text = element_text(size = 24),
              legend.position = 'inside',
              legend.text=element_text(size=22),
              legend.position.inside = c(0.2,0.85),
              legend.background = element_rect(fill = "transparent",color='transparent'),
              legend.box.background = element_rect(fill = "transparent",color='transparent'),
              panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5))
  
  return(p1)
}

p1 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_nh3_as',ptitle='nh3',pf ='AS')
p2 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_n2o_as',ptitle='n2o',pf ='AS',
                    pbreak = c(0,0.25,0.5,0.75,1.25,10),plabel = c('<0.25','0.25-0.50', '0.50-0.75','0.75-1.25','>1.25'))
p3 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_no_as',ptitle='no',pf = 'AS',
                    pbreak = c(0,0.03,0.06,0.3,0.6,10),plabel = c('<0.03','0.03-0.06', '0.06-0.30','0.30-0.60','>0.60'))
p4 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_no3_as',ptitle='no3',pf = 'AS',
                    pbreak = c(-Inf,1,2,8,16,32,Inf),plabel = c('<1','1-2', '2-8','8-16','16-32','>32'))
p5 <- list(p1,p2,p3,p4)
p6 <- patchwork::wrap_plots(p5, ncol=4) 
ggsave(plot = p6,filename = "products/eu_map_as_v2.png",width = 72, height = 23, units = c("cm"), dpi = 600)


p1 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_nh3_u',ptitle='nh3',pf ='urea')
p2 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_n2o_u',ptitle='n2o',pf ='urea',
                    pbreak = c(0,0.25,0.5,0.75,1.25,10),plabel = c('<0.25','0.25-0.50', '0.50-0.75','0.75-1.25','>1.25'))
p3 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_no_u',ptitle='no',pf = 'urea',
                    pbreak = c(0,0.03,0.06,0.3,0.6,10),plabel = c('<0.03','0.03-0.06', '0.06-0.30','0.30-0.60','>0.60'))
p4 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_no3_u',ptitle='no3',pf = 'urea',
                    pbreak = c(-Inf,1,2,8,16,32,Inf),plabel = c('<1','1-2', '2-8','8-16','16-32','>32'))
p5 <- list(p1,p2,p3,p4)
p6 <- patchwork::wrap_plots(p5, ncol=4) 
ggsave(plot = p6,filename = "products/eu_map_u_v2.png",width = 72, height = 23, units = c("cm"), dpi = 600)

p1 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_nh3_an',ptitle='nh3',pf ='(C)AN')
p2 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_n2o_an',ptitle='n2o',pf ='(C)AN',
                    pbreak = c(0,0.25,0.5,0.75,1.25,10),plabel = c('<0.25','0.25-0.50', '0.50-0.75','0.75-1.25','>1.25'))
p3 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_no_an',ptitle='no',pf = '(C)AN')
p4 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_no3_an',ptitle='no3',pf = '(C)AN',
                    pbreak = c(-Inf,1,2,8,16,32,Inf),plabel = c('<1','1-2', '2-8','8-16','16-32','>32'))
p5 <- list(p1,p2,p3,p4)
p6 <- patchwork::wrap_plots(p5, ncol=4) 
ggsave(plot = p6,filename = "products/eu_map_an_v2.png",width = 72, height = 23, units = c("cm"), dpi = 600)

p1 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_nh3_uc',ptitle='nh3',pf ='urea crf')
p2 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_n2o_uc',ptitle='n2o',pf ='urea crf',
                    pbreak = c(0,0.25,0.5,0.75,1.25,10),plabel = c('<0.25','0.25-0.50', '0.50-0.75','0.75-1.25','>1.25'))
p3 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_no_uc',ptitle='no',pf = 'urea crf')
p4 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu,variable='ef_no3_uc',ptitle='no3',pf = 'urea crf',
                    pbreak = c(-Inf,1,2,8,16,32,Inf),plabel = c('<1','1-2', '2-8','8-16','16-32','>32'))
p5 <- list(p1,p2,p3,p4)
p6 <- patchwork::wrap_plots(p5, ncol=4) 
ggsave(plot = p6,filename = "products/eu_map_uc_v2.png",width = 72, height = 23, units = c("cm"), dpi = 600)

p1 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu.emr,variable='nh3',ptitle='nh3',ft=2,
                    pbreak = c(0,5,10,25,50,500),plabel = c('<5','5-10', '10-25','25-50','>50'))
p2 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu.emr,variable='n2o',ptitle='n2o',ft=2,
                    pbreak = c(0,0.2,0.3,0.4,0.5,10),plabel = c('<0.2','0.2-0.3', '0.3-0.4','0.4-0.5','>0.5') )
p3 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu.emr,variable='no',ptitle='no',ft=2,
                    pbreak = c(0,0.05,0.1,0.2,0.4,10),plabel = c('<0.05','0.05-0.1', '0.1-0.2','0.2-0.4','>0.4') )
p4 <- ggplot_eu_map(sf_nuts = s_nuts,r_ncu = r.ncu.emr,variable='ef_red_tot',ptitle='all',ft=2,
                    pbreak = c(0,5,10,25,50,500),plabel = c('<5','5-10', '10-25','25-50','>50'))
p5 <- list(p1,p2,p3,p4)
p6 <- patchwork::wrap_plots(p5, ncol=4) 
ggsave(plot = p6,filename = "products/eu_map_emissionreduction_v2.png",width = 72, height = 23, units = c("cm"), dpi = 600)

