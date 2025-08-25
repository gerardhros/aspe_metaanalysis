# data analyse ASPE

# clear memory
rm(list=ls())

# require packages
require(data.table)
require(ggplot2)
require(viridis)
library(nlme)
library(MuMIn)

# drive locations
ploc <- 'D:/SPRINGG/NMISite - Documenten/Projecten/O 2000 - O 2001/2001.N.23 ASPE review/'
wloc <- 'D:/SPRINGG/Wim Bussink - ASPE project/Database_NH3/'

  # inladen database (versie 1)
  d1 <- as.data.table(readxl::read_xlsx(paste0(wloc,'241121_database_integrated_v2.xlsx'),sheet='database'))
  d2 <- fread('products/241121 covariates metaanalysis.csv')
  d2[,c('lon','lat','study_id') := NULL]
  d3 <- merge(d1,d2,by.x='obs_id',by.y='ID',all.x=TRUE)
  
  # inladen database versie 2
  d1 <- as.data.table(readxl::read_xlsx('data/241121_database_integrated_v2.xlsx',sheet='database'))
  d2 <- fread('products/250429 covariates metaanalysis nh3.csv')
  d2[,c('lon','lat') := NULL]
  d3 <- merge(d1,d2,by.x='obs_id',by.y='sid',all.x=TRUE)
  
  # remove control sites
  d3 <- d3[ndose >0]
  
  # update name both files
  setnames(d3,'Ferttype!','fert_cat_source',skip_absent=TRUE)

# update groupings
  
  # regrouping crops
  d3[,crop := tolower(crop)]
  d3[grepl('grass',crop),crop_cat := 'grassland']
  d3[grepl('bean|clover',crop),crop_cat := 'leguminose']
  d3[grepl('bare|fallow',crop),crop_cat := 'fallow']
  d3[grepl('wheat|barley|maize',crop),crop_cat := 'cereal']
  d3[grepl('rice',crop),crop_cat := 'rice']
  d3[grepl('potato|cabbag|lett|tomato|rape|brassica',crop),crop_cat := 'arableveg']
  d3[grepl('peach|blue|trees|komatsuna|coffee|fruit|dryla|upland',crop),crop_cat := 'other']
  d3[is.na(crop), crop_cat := 'arableveg']
  
# regrouping fertilizer
# d3[grepl('^U_S|Uc',fert_cat), fert_cat := 'U_coated']
# d3[grepl('^Uw',fert_cat), fert_cat := 'U']
# d3[grepl('U',fert_cat),fert_cat2 := 'U']
# d3[grepl('AS',fert_cat),fert_cat2 := 'AS']
# d3[is.na(fert_cat2), fert_cat2 :='others']

  # set fertilizer grouping
  setnames(d3,'ferttype','fert_type')
  d3[grepl('urea|^u|\\+u|entec|superu|_u_|u$| u fertilizer|sulpur u|\\%u|sulfur u|reduced u|coated u|\\(u|u+dcd|uan|u coated|-u-',tolower(fert_type)), fert_cat := 'U']
  d3[fert_cat =='U'& grepl('dmpp|pscu|ibdu|dcd|^uc$|inhib|formalde|entec|coat|thiophosp|pyridine|control|nbpt|slow|crf|stabiliz|\\+cp|dmpsa|polyol|butano|nitrapyr|dicyan|\\+ui|\\+ni',tolower(fert_type)), fert_cat := 'UC']
  d3[fert_cat=='U' & grepl('ridg|furr|ammoni|nitrate|as|nh4|no3|compound|npk|an|as',tolower(fert_type)),fert_cat := 'UM']
  d3[is.na(fert_cat) & grepl('pig|slurry|manure|sludge|biogas|compost|urine|leather|cattle|residue|fish|vetc|crop|msw|straw|org',tolower(fert_type)),fert_cat := 'OM']
  d3[grepl('as|ammonium_sulph|ammonium sulph|nh4so4|^as$',tolower(fert_type)) & is.na(fert_cat), fert_cat := 'AS']
  d3[fert_cat =='AS' & grepl('potassium|npk starter|n-source',tolower(fert_type)),fert_cat := NA_character_]
  d3[fert_cat =='AS'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat|slow|polym|contr|\\+ni|ni$',tolower(fert_type)), fert_cat := 'ASC']
  d3[grepl('asn|ammonium sulphate nitrate',tolower(fert_type)), fert_cat := 'ASN']
  d3[fert_cat =='ASN'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat|\\+ni',tolower(fert_type)), fert_cat := 'ASNC']
  d3[grepl('an|ammonium nitra|nh4no3|ammoini|^cn$|^ac$',tolower(fert_type))& is.na(fert_cat), fert_cat := 'AN']
  d3[fert_cat =='AN'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat',tolower(fert_type)), fert_cat := 'ANC']
  d3[is.na(fert_cat) & grepl('dap|map',tolower(fert_type)),fert_cat := 'DAP-MAP']
  d3[is.na(fert_cat) & grepl('mineral',tolower(fert_type)),fert_cat := "MF"]
  d3[is.na(fert_cat),fert_cat := 'O']

# splitmeanfun
smf <- function(x) {sapply(strsplit(x,'-'),function(y){mean(as.numeric(y))})}
d3[, ph := smf(ph)]
d3[is.na(ph), ph := phw_mean_0_5/10]

# soil type
d3[is.na(clay) & soiltype == 'silty clay', clay := 50]
d3[is.na(clay) & soiltype %in%  c('silty loam','silt loam'), clay := 27.5/2]
d3[is.na(clay) & soiltype == 'silty clay loam', clay := 35]
d3[is.na(clay) & soiltype == 'sandy loam', clay := 10]
d3[is.na(clay) & soiltype == 'sand', clay := 5]
d3[is.na(clay) & soiltype == 'clay', clay := 50]
d3[is.na(clay) & soiltype == 'clay loam', clay := 35]
d3[is.na(clay) & soiltype == 'loam', clay := 20]
d3[is.na(clay) & soiltype == 'loamy sand', clay := 7.5]
d3[is.na(clay), clay := clay_mean_0_5/10]

d3[,sand := sand_mean_0_5*.1]
d3[,bd := bdod_mean_0_5*0.01]
d3[is.na(bd),bd := median(d3$bd,na.rm=T)]

# experimental type
d3[grepl('incu|lab',exptype), exptype := 'lab']

# measure method
d3[grepl('Balance|mass difference|difference|mass balance|balance|indirect meas', measmethod_nh3), measmethod_nh3 := 'B']
d3[grepl('dynamic chamber$|forced draft|vented closed|wind tun', measmethod_nh3), measmethod_nh3 := 'D']
d3[grepl('dynamic chamber and', measmethod_nh3), measmethod_nh3 := 'DM']
d3[grepl('micromet|simplified mass', tolower(measmethod_nh3)), measmethod_nh3 := 'M']
d3[grepl('chamber|jar|passive flux|semi-open|static chamber|chamber', measmethod_nh3), measmethod_nh3 := 'S']
d3[grepl('open and|DM',measmethod_nh3), measmethod_nh3 := 'O']
d3[is.na(measmethod_nh3), measmethod_nh3 := 'unknown']

# crop residues applied
d3[grepl('none|no',cropres), cropres := 'none']
d3[grepl('yes|present|mulch|straw|sugar',cropres), cropres := 'yes']
d3[is.na(cropres),cropres:='unknown']

# coatings
d3[grepl('NBPT|nitrapy|PPD|pyrimi|yes|polymer|thermo|Nutrispher',fertinhib),fertinhib := 'yes']
d3[grepl('nbpt|nitrapy|ppd|pyrimi|yes|polymer|thermo|nutrispher|ibdu|coat|dmpp|eef|controlled',tolower(fert_type)),fertinhib := 'yes']
d3[grepl('gypsu|pyrit|formal|Cu|U-N|lime|sulphu|sulfu|Cop|lime|CaCO3',fertinhib), fertinhib := 'no']
d3[is.na(fertinhib),fertinhib:='unknown']
d3[fert_cat=='AS' & fertinhib=='yes', fert_cat := 'ASC']
d3[fert_cat=='ASN' & fertinhib=='yes', fert_cat := 'ASNC']
d3[fert_cat=='AN' & fertinhib=='yes', fert_cat := 'ANC']
d3[fert_cat=='U' & fertinhib=='yes', fert_cat := 'UC']

# application method
d3[,app := tolower(app)]
d3[grepl('broadc',app) & grepl('banded|incorp',app), appc := 'combi']
d3[grepl('banded',app) & is.na(appc), appc := 'banded']
d3[grepl('broadcast|surface|broadc',app) & is.na(appc), appc := 'broadcast']
d3[grepl('incorporated|incorp',app) & is.na(appc), appc := 'incorporated']
d3[grepl('foliar|solution|liquid',app) & is.na(appc), appc := 'solution']
d3[is.na(appc), appc := 'unknown']

# tillage practices
d3[grepl('no-till|no till',tillage), tillage := 'notill']
d3[is.na(tillage), tillage := 'unknown']

# soil organic carbon
d3[is.na(soc), soc := soc_mean_0_5/100]

# pH type
d3[grepl('cacl2',tolower(phtype)),phtype := 'cacl2']
d3[grepl('water',phtype),phtype := 'water']
d3[is.na(phtype),phtype := 'water']

# CEC
d3[is.na(cec),cec := cec_mean_0_5]

# soil moisture
d3[grepl('wet|high',soilmoisture), soilmoisture := 'high']
d3[is.na(soilmoisture),soilmoisture := 'unknown']

# phosphorus and potassium P dose
d3[is.na(pdose), pdose := median(d3$pdose,na.rm=T)]
d3[is.na(kdose), pdose := median(d3$kdose,na.rm=T)]

# adjust unit from mm/month to mm/year
d3[, pre_mean := pre_mean * 12]

# first main factor analysis
cols <- c('crop_cat','crop_height','ph','clay','GEnZ','soc', 'exptype', 
          'tmp_mean','pre_mean','measmethod_nh3','pdose','kdose')

# estimate SD when missing
d3[,EF_NH3_cv := EF_NH3_sd / EF_NH3_value]
d3[,EF_NH3_cv_mean := mean(EF_NH3_cv,na.rm=T)]
d3[is.na(EF_NH3_sd), EF_NH3_sd := EF_NH3_cv_mean * 1.5 * EF_NH3_value]
d3[is.na(EF_NH3_n), EF_NH3_n := 2]

# scale to unit variance
#cols <- c('ph','ph2','ndose2','ndose','clay','tmp_mean','pre_mean')
#d3[, c(cols) := lapply(.SD,scale),.SDcols = cols]
  
# analysis for NH3 emission fraction
d4 <- d3[!is.na(EF_NH3_value) & EF_NH3_value > 0 & EF_NH3_value <= 0.5]

# remove the organic matter treatments
d4 <- d4[!fert_cat  %in% c('OM') & crop_cat != 'rice']

d4[is.na(ndosenumber), ndosenumber := 1]
d4[,log_ef := log(EF_NH3_value)]

d4[grepl('sam',tolower(region)),region := 'SAm']
d4[grepl('usa',tolower(region)),region := 'NAm']

# select relevant columns
d5 <- d4[,.(studyid = study_id, lat,lon,region,ph,phtype,soc,clay,cec,sand,bd,temp = tmp_mean, prec = pre_mean, GEnZ,GEnS,
            appc,fert_cat,crop_cat,measmethod = measmethod_nh3,exptype,ndose,nsplitc = ndosenumber,
            log_ef,ef_mean = EF_NH3_value,ef_sd = EF_NH3_sd, n =EF_NH3_n)]

fwrite(d5,'products/250423_db_nh3_final.csv')
fwrite(d5,'products/250429_db_nh3_final.csv')

m1 <-lme(EF_NH3_value ~ ph+ph2 + crop_cat + ndose + ndose2 +fert_cat + clay + tmp_mean + pre_mean+ 
                          measmethod_nh3+appc+fertinhib-1, 
         random = ~ 1 | GEnZ/exptype, weights = varFunc(~ EF_NH3_sd+0.0001),  control=lmeControl(sigma = 1), 
         data=d4,na.action=na.omit)

m1 <-lme(log_ef  ~ ph+I(ph^2)+ I(ndose^2) + crop_cat + ndose + fert_cat + clay+cec +soc+ temp + prec + appc + measmethod_nh3-1, 
         random = ~ 1 | GEnZ/exptype/phtype, weights = varFunc(~ log(EF_NH3_sd+0.0001)),  control=lmeControl(sigma = 1), 
         data=d4,na.action=na.omit)
anova(m1)
r.squaredGLMM(m1)


#update model na feedback wim
m1 <-lme(EF_NH3_value ~ ph+I(ph^2) + crop_cat + ndose + I(ndose^2) +fert_cat + soc:clay + tmp_mean + pre_mean+ 
           measmethod_nh3+appc+fertinhib+ph:ndose+appc:ph-1, 
         random = ~ 1 | GEnZ/exptype/phtype, weights = varFunc(~ EF_NH3_sd+0.0001),  control=lmeControl(sigma = 1), 
         data=d4,na.action=na.omit)

d4[,res := abs(m1$residuals[,1])]

# predict model Bouwman
d4[,pbouwman := model_Bouwman(crop = crop_cat,
                              irr = FALSE, 
                              fert_cat = fert_cat, 
                              app = appc, 
                              ph = ph, cec = cec, lat = lat)]
d4[,p1 := predict(m1)]

# plot predictions
d5 <- melt(d4[,.(fert_cat,observed = EF_NH3_value, model1 = p1,model2 = pbouwman)],
           id.vars = 'fert_cat',variable.name = 'model')

p1 <- ggplot(data=d4) + geom_point(aes(x=p1,y=pbouwman))+theme_bw() + xlab('model (this study)') + ylab('model (bouwman)') + 
      geom_abline(slope=1,intercept=0) + ggtitle('Comparison models Bouwman and this study')+
      xlim(0,0.55)+ylim(0,0.55)
ggsave(plot = p1,filename = 'products/model_nh3_bouwman.png',width = 13, height = 13, units='cm')

d5[grepl('model2',model),model := 'model (bouwman)']
d5[grepl('model1',model),model := 'model (this study)']

p1 <- ggplot(data=d5) + geom_boxplot(aes(x=fert_cat,y=value,fill=model))+theme_bw() + 
  xlab('') + ylab('EF NH3') + ylim(0,0.5) + ggtitle('Impact of fertilizer type')+
  theme(legend.position = 'inside',legend.position.inside = c(0.3,0.8))
ggsave(plot = p1,filename = 'products/model_nh3_bouwman_bp.png',width = 13, height = 10, units='cm')



AIC(m1)

p1 <- predict(m1)
plot((p1),(d4$EF_NH3_value))
abline(0,1)
hist(residuals(m1))
summary(lm(p1~d4$EF_NH3_value-1))

















# # make model explainers
# data.table(value = round(m1$coefficients$fixed,3),
#               var = names(m1$coefficients$fixed))
# 
# 
# 
# 
# out <- list()
# 
# for(i in cols){
#   
#   d3[,xvar := get(i)]
#   
#   if(i %in% c('ph','clay','soc', 'tmp_mean','prec_mean','pdose','kdose')){}
#     
#     m1 <- lm(EF_NH3_value~ xvar,data = d3)
#     m1s <- summary(m1)
#     r2 <- m1s$adj.r.squared
#     est <- m1s$coefficients[,1]
#     enames <- rownames(m1s$coefficients)
#     o1 <- data.table(var = i, pars = enames, coeff = est, r2 = r2)
#     out[[i]] <- o1
# }
# 
# out <- rbindlist(out)
# 
# summary(lm(EF_NH3_value~ ph + crop_cat + fert_cat + clay * soc + tmp_mean * pre_mean +pet_mean+ measmethod_nh3-1,data = d3))
# 
# 
# 
# 
# # make categorial
# d4[,phc := cut(ph,c(0,5.5,7.3,8.5,10))]
# 
# 
# 
# 
# 
# d4 <- d3[!is.na(EF_NH3_value) & EF_NH3_value > 0]
# m2 <- glm(EF_NH3_value ~ ph + I(ph^2) + crop_cat + ndose + I(ndose^2) +fert_cat + clay + tmp_mean + pre_mean+ measmethod_nh3-1, data = d4, family = quasibinomial('logit'))
# summary(m2)
# p2 <- predict(m2)
# plot(p2,d4$EF_NH3_value)
# abline(0,1)
# 
# # logistische regressie
# d4 <- d3[,.( EF_NH3_value,ph,crop_cat,fert_cat ,clay , soc , tmp_mean ,pre_mean ,pet_mean, measmethod_nh3)]
# d4[,crop_cat := as.factor(crop_cat)]
# d4[,fert_cat := as.factor(fert_cat)]
# 
# model <- glm(EF_NH3_value ~ph + I(ph^2) + crop_cat + ndose + I(ndose^2) +fert_cat + clay + tmp_mean + pre_mean+ measmethod_nh3-1,family=binomial(link='logit'),data=d4)
# summary(model)
# anova(model, test="Chisq")
# 
# p1 <- predict(model)
# plot(p1,d4$EF_NH3_value)
# 
# 
# library(pscl)
# round(pR2(model)[4],2)
# 
# 
# # plot histogram for NH3 emissions
# p1 <- ggplot(data=d3[fert_cat=='AS'],aes(x=EF_NH3_value)) + geom_histogram(aes(y=..density..),colour='red',fill='white') + 
#       geom_density(alpha=.2, fill="#FF6666") + ylab('')+
#       theme_bw()+ xlim(0,1)+ ylim(0,5)+ xlab('')+ggtitle('Histogram NH3 emissiefracties AS')
# p2 <- ggplot(data=d3[fert_cat %in% c('U','U_coated')],aes(x=EF_NH3_value)) + geom_histogram(aes(y=..density..),colour='red',fill='white') + 
#       geom_density(alpha=.2, fill="#FF6666") + ylab('')+
#       theme_classic()+ xlim(0,1)+ ylim(0,5)+ xlab('')+ggtitle('Histogram NH3 emissiefracties (coated) urea')
# p3 <- ggplot(data=d3[fert_cat %in% c('AN','ASN','CAN','DAP','MAP')],aes(x=EF_NH3_value)) + geom_histogram(aes(y=..density..),colour='red',fill='white') + 
#       geom_density(alpha=.2, fill="#FF6666") + ylab('')+
#       theme_classic()+ xlim(0,1)+ ylim(0,5)+ xlab('')+ggtitle('Histogram NH3 emissiefracties AN, ASN, CAN, DAP and MAP')
# p4 <- ggplot(data=d3[!is.na(EF_NH3_value) & !is.na(fert_cat)],aes(x = fert_cat,y=EF_NH3_value,fill=fert_cat)) + 
#       geom_boxplot() + theme_bw()+
#       scale_fill_viridis(discrete = TRUE, alpha=0.6) +
#       geom_jitter(color="black", size=0.4, alpha=0.9) + ggtitle('NH3 emission fraction per fertilizer type') + xlab('')+ ylab('Emission fraction')
# 
# ggplot(data=d3,aes(x=ph,y=EF_NH3_value)) + geom_point(aes(color=fert_cat2)) +
#   geom_smooth(aes(color = fert_cat2),method = "lm",formula = y ~ sqrt(x), se = FALSE) + theme_bw()
# 
# 
# ggplot(data=d3[!is.na(EF_NH3_value) & !is.na(fert_cat)],aes(x = exptype,y=EF_NH3_value,fill=exptype)) + 
#   geom_boxplot() + theme_bw() + xlab('') + ylab('EF') + ggtitle('Emission fraction per experimental type')
# 
# ggplot(data=d3[!is.na(EF_NH3_value) & !is.na(fert_cat)],aes(x = measmethod_nh3,y=EF_NH3_value,fill=measmethod_nh3)) + 
#   geom_boxplot() + theme_bw() + xlab('') + ylab('EF') + ggtitle('Emission fraction per measurement method')
# 
# ggplot(data=d4[!is.na(EF_NH3_value)],aes(x = ph,y=log(EF_NH3_value))) + 
#   geom_point() + theme_bw() + xlab('') + ylab('EF') + geom_smooth()+ 
#   ggtitle('Emission fraction per measurement method')
# 
# summary(lm(EF_NH3_value~fert_cat + ndose + crop_cat, data = d3))
# 
