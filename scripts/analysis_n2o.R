# analysis N2O preliminary dataset

# clear memory
rm(list=ls())

# require packages
require(data.table); require(ggplot2)
require(viridis); library(nlme); library(MuMIn)

# -- step 1. load data ----
  
  # read in file (analysis 1)
  d1 <- as.data.table(readxl::read_xlsx('data/250310 database aspe n2o.xlsx',sheet='AS'))
  d2 <- fread('products/250311 covariates metaanalysis n2o.csv')

  # read in files (analysis 2)
  d1.as <- as.data.table(readxl::read_xlsx('data/250422 database aspe n2o.xlsx',sheet='AS'))
  d1.other <- as.data.table(readxl::read_xlsx('data/250422 database aspe n2o.xlsx',sheet='Andere meststoffen'))
  d1 <- rbind(d1.as,d1.other,fill=TRUE)
  d2 <- fread('products/250425 covariates metaanalysis n2o.csv')
  d2[,c('lat','lon') := NULL]
  
  # read in files (analysis 3)
  d1.as <- as.data.table(readxl::read_xlsx('data/250422 database aspe n2o.xlsx',sheet='AS'))
  d1.other <- as.data.table(readxl::read_xlsx('data/250422 database aspe n2o.xlsx',sheet='Andere meststoffen'))
  d1 <- rbind(d1.as,d1.other,fill=TRUE)
  d2 <- fread('products/250429 covariates metaanalysis n2o.csv')
  d2[,c('lat','lon') := NULL]
  
  # merge covariates
  d1 <- merge(d1,d2,by.x=c('sid'),by.y='ID',all.x=TRUE)

# -- step 2. clean data -----
  
  # filter only those with EF
  d1 <- d1[!is.na(EF) & ndose > 0]
  d1[abs(EF) > 1, EF := EF * 0.01]

  # set fertilizer grouping
  d1[grepl('urea|^u|\\+u|entec|superu|_u_|u$| u fertilizer| u\\+p|sulpur u|\\%u|sulfur u|reduced u|coated u|\\(u|u+dcd|uan|u coated|-u-',tolower(fert_type)), fert_cat := 'U']
  d1[fert_cat =='U'& grepl('dmpp|dcd|inhib|formalde|entec|coat|thiophosp|pyridine|control|nbpt|slow|crf|stabiliz|\\+cp|dmpsa|polyol|butano|nitrapyr|dicyan|\\+ui|\\+ni',tolower(fert_type)), fert_cat := 'UC']
  d1[fert_cat=='U' & grepl('ammoni|nitrate|as|nh4|no3|compound|npk|an',tolower(fert_type)),fert_cat := 'UM']
  d1[grepl('municipal|straw|pig|cake|manure|msw|ridg|furr|resid|fym|pig|slurry|manure|compost|urine|cattle|residue|fish|vetc|crop|msw|straw|^o$',tolower(fert_type)),fert_cat := 'OM']
  d1[grepl('as|ammonium_sulph|ammonium sulph|nh4so4',tolower(fert_type)) & is.na(fert_cat), fert_cat := 'AS']
  d1[grepl('sulfa',tolower(fert_type)) & grepl('nh4',tolower(fert_type)) & is.na(fert_cat),fert_cat:= 'AS']
  d1[fert_cat =='AS' & grepl('potassium|npk starter|slow release|^controlled',tolower(fert_type)),fert_cat := NA_character_]
  d1[fert_cat =='AS'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat|slow|polym|contr|\\+ni|ni$',tolower(fert_type)), fert_cat := 'ASC']
  d1[grepl('asn|ammonium sulphate nitrate',tolower(fert_type)), fert_cat := 'ASN']
  d1[fert_cat =='ASN'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat|\\+ni',tolower(fert_type)), fert_cat := 'ASNC']
  d1[grepl('an|ammonium nitra|nh4no3|ammoini|calcium nitrate|cn',tolower(fert_type))& is.na(fert_cat), fert_cat := 'AN']
  d1[grepl('no3',tolower(fert_type)) & grepl('nh4',tolower(fert_type)) & is.na(fert_cat),fert_cat:= 'AN']
  d1[fert_cat =='AN'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat|\\+ni',tolower(fert_type)), fert_cat := 'ANC']
  d1[is.na(fert_cat) & grepl('dap|map|ammonium phosphate|nh4h2p',tolower(fert_type)),fert_cat := 'DAP-MAP']
  d1[is.na(fert_cat) & grepl('mineral',tolower(fert_type)),fert_cat := "MF"]
  d1[is.na(fert_cat),fert_cat := 'O']

  # application mode
  d1[,application_mode := tolower(application_mode)]
  d1[grepl('mix',application_mode), appc := 'combi']
  d1[grepl('broadc$|broadcast|surface broad|surface plac|granul|solid|surface',tolower(application_mode)), appc := 'broadcast']
  d1[grepl('band|place$',tolower(application_mode)), appc := 'banded']
  d1[grepl('incorp$|incorporated|inject|deep',tolower(application_mode)), appc := 'incorporated']
  d1[grepl('irrig|ferti|liquid',application_mode),appc := 'irrigation']
  d1[is.na(appc)|application_mode=='na', appc := 'unknown']

  # crop category
  d1[,crop_type := tolower(crop_type)]
  d1[grepl('cereal|maize',tolower(crop_type)),crop_cat := 'cereal']
  d1[grepl('legum|veget|row|horti',tolower(crop_type)),crop_cat := 'vegetable']
  d1[grepl('rice|paddy',tolower(crop_type)),crop_cat := 'rice']
  d1[grepl('grassl',tolower(crop_type)),crop_cat := 'grass']
  d1[grepl('fallow',tolower(crop_type)),crop_cat := 'fallow']
  d1[grepl('peren',tolower(crop_type)),crop_cat := 'perennial']
  d1[is.na(crop_cat),crop_cat := 'other']

  # replace missing numeric ones
  d1[is.na(ph),ph := phw_mean_0_5*.1]
  d1[is.na(ph),ph := median(d1$ph,na.rm=T)]
  d1[is.na(soc),soc := soc_mean_0_5*0.01]
  d1[is.na(soc),soc := median(d1$soc,na.rm=T)]
  d1[is.na(clay),clay := clay_mean_0_5*.1]
  d1[is.na(clay),clay := median(d1$clay,na.rm=T)]
  d1[is.na(cec),cec := cec_mean_0_5]
  d1[is.na(cec),cec := median(d1$cec,na.rm=T)]
  d1[is.na(sand),sand := sand_mean_0_5*.1]
  d1[is.na(sand),sand := median(d1$sand,na.rm=T)]
  d1[is.na(bd),bd := bdod_mean_0_5*0.01]
  d1[is.na(bd),bd := median(d1$bd,na.rm=T)]
  d1[is.na(prec), prec:= pre_mean * 12]
  d1[is.na(prec),prec := median(d1$prec,na.rm=T)]
  d1[is.na(temp), temp:= tmp_mean]
  d1[is.na(temp),temp := median(d1$temp,na.rm=T)]
  d1[is.na(ndose), ndose := median(d1$ndose,na.rm=T)]
  d1[ph <= 4, ph := 4]

  # update categories
  d1[,country := tolower(country)]
  d1[is.na(irr), irr := 'unknown']
  d1[grepl('wfps|drip|irri|furrow|whc|rw',tolower(irr)), irr:='yes']
  d1[is.na(tillage), tillage := 'unknown']
  d1[,nsplit := as.character(nsplit)]
  d1[nsplit=='One'|nsplit=="1", nsplitc := 1]
  d1[nsplit=='Multiple'|nsplit=='2',nsplitc := 2]
  d1[nsplit=='3', nsplitc := 3]
  d1[nsplit %in% c('4','6','7','8','9','13'), nsplitc := 4]
  d1[is.na(nsplit),nsplitc := 1]
  
  d1[is.na(GEnZ) & country %in% c('ireland','germany','usa'), GEnZ := 'J']
  d1[is.na(GEnZ) & country %in% c('scotland','canada'), GEnZ := 'G']
  d1[is.na(GEnZ) & country %in% c('italy','japan','new zealand','spain'), GEnZ := 'K']
  d1[is.na(GEnZ) & country %in% c('puerto rico','brazil','costa rica'), GEnZ := 'R']

  # set ph type
  d1[grepl('cacl2',ph_type), phtype := 'cacl2']
  d1[grepl('water|h2o',tolower(ph_type)), phtype := 'water']
  d1[!grepl('water|h2o|cacl2',tolower(ph_type)),phtype := 'water']
  
  d1[grepl('sam|mam|sa$',tolower(region)),region := 'SAm']
  d1[grepl('usa|nam',tolower(region)),region := 'NAm']
  d1[grepl('asia',tolower(region)),region := 'AS']
  
  # cols to delete
  colsr <- colnames(d1)[grepl('cec_|sand_|clay_|silt_|soc_|ntot_|phw_|bdod_|reference|pre_mean|pre_sd|tmp_mean|tmp_sd',colnames(d1))]
  d1[,c(colsr):=NULL]
  colsr <- colnames(d1)[grepl('crop_type|application_|source|pre_crop|crop$|GEnZname|pet_|nsplit$|info|Startda|Durat',colnames(d1))]
  d1[,c(colsr):=NULL]

  # update uncertainties
  d1[is.na(n), n:= 3]
  d1[,n2o_fert_cv := n2o_fert_se/n2o_fert]
  d1[,n2o_fert_cv_mean := mean(n2o_fert_cv,na.rm=T)*1.5]
  d1[is.na(n2o_fert_se),n2o_fert_se := n2o_fert_cv_mean * n2o_fert]
  d1[,c('n2o_fert_cv','n2o_fert_cv_mean') := NULL]

  d1[is.na(n2o_control), n2o_control := median(d1$n2o_control,na.rm=T)]
  d1[,n2o_contr_cv := n2o_control_se/n2o_control]
  d1[,n2o_contr_cv_mean := mean(n2o_contr_cv,na.rm=T)*1.5]
  d1[is.na(n2o_control_se),n2o_control_se := n2o_contr_cv_mean * n2o_control]
  d1[,c('n2o_contr_cv','n2o_contr_cv_mean') := NULL]
  
  d1[,n2o_diff := n2o_fert - n2o_control]
  d1[,ef_n2o := fifelse(!is.na(n2o_diff),n2o_diff / ndose,EF)]
  d1[,ef_n2o_se := sqrt(n2o_fert_se^2+n2o_control_se^2)/ndose]
  d1[is.na(ef_n2o_se),ef_n2o_se := median(d1$ef_n2o_se,na.rm = T)]

  # set values below zero to almost zero
  d1[ef_n2o <= 0, ef_n2o := pmax(0.0001,ef_n2o)]
  d1[ef_n2o_se <= 0, ef_n2o_se := pmax(0.00001,ef_n2o_se)]
  
  # convert to log scale
  d1[,log_ef := log(ef_n2o)]

  # select variables for a model
  d2 <- d1[,.(studyid,lat,lon,region,ef_mean = ef_n2o,ef_sd = ef_n2o_se,log_ef,
              GEnZ, prec,temp,crop_cat,phtype,measmethod  ='unknown',
              sand,clay,soc,ph,bd,cec,
              irr,tillage,ndose,nsplitc,appc,n,
              fert_cat)]
  
  # remove the organic matter treatments
  d2 <- d2[!fert_cat  %in% c('OM')]
  
  #fwrite(d2,'products/250423_db_n2o_final.csv')
  fwrite(d2,'products/250429_db_n2o_final.csv')
  
# make a model to predict N2O
m1 <-lme(log_ef ~ fert_cat+ndose + ph+ clay * soc + ph + crop_cat + I(temp^2)+ I(prec^2)+I(ndose^2)+
                  GEnZ + appc + nsplitc-1, 
         random = ~ 1 |studyid, weights = varFunc(~ log(ef_n2o_se)),  
         data=d2,na.action=na.omit)

# only the sulphur studies
d2.s <- d2[fert_cat %in% c('AS','ASN','ASC','ASNC'),studyid]
d2.s <- d2[studyid %in% d2.s]

# make a model to predict N2O for AS studies only
m2.s <-lme(log_ef ~ fert_cat+ndose + ph+ clay * soc + ph + crop_cat + I(temp^2)+ I(prec^2)+I(ph^2)+
           prec*temp+GEnZ + appc-1, 
         random = ~ 1 |studyid, weights = varFunc(~ log(ef_n2o_se)),  
         data=d2.s,na.action=na.omit)

# get stats
anova(m1)
r.squaredGLMM(m2.s)

p1 <- predict(m1)
plot(p1,d2$log_ef)
plot(exp(p1),exp(d2$log_ef),xlim=c(0,0.01),ylim=c(0,0.01))
abline(0,1)
hist(residuals(m1))
summary(lm(p1~d1$EF-1))
