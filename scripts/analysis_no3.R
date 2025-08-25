# analysis N2O preliminary dataset

# clear memory
rm(list=ls())

# require packages
require(data.table); require(ggplot2)
require(viridis); library(nlme); library(MuMIn)

# -- step 1. load data ----
  
  # read in files (analysis 2)
  d1 <- as.data.table(readxl::read_xlsx('data/250422 database aspe no3.xlsx',sheet='data'))
  d2 <- fread('products/250425 covariates metaanalysis no3.csv')
  d2[,c('lat','lon') := NULL]
  
  # merge covariates
  d1 <- merge(d1,d2,by=c('sid'),all.x=TRUE)

# -- step 2. clean data -----
  
  # calculate leaching fraction
  d1[,ef_no3 := fifelse(!is.na(NO3_LF_NMI),NO3_LF_NMI,NO3_LF_WANG)]
  d1[,ef_no3 := pmax(-0.1,pmin(1,ef_no3))]
  
  # filter only those with EF
  d1 <- d1[!is.na(ef_no3) & ndose > 0 & !is.na(ndose)]
  
  # set fertilizer grouping
  d1[grepl('urea|^u|\\+u|entec|superu|_u_|u$| u fertilizer|sulpur u|\\%u|sulfur u|reduced u|coated u|\\(u|u+dcd|uan|u coated|-u-',tolower(fert_type)), fert_cat := 'U']
  d1[fert_cat =='U'& grepl('dmpp|dcd|inhib|formalde|entec|coat|thiophosp|pyridine|control|nbpt|slow|crf|stabiliz|\\+cp|dmpsa|polyol|butano|nitrapyr|dicyan|\\+ui|\\+ni',tolower(fert_type)), fert_cat := 'UC']
  d1[fert_cat=='U' & grepl('straw|pig|manure|msw|ridg|furr|resid|ammoni|nitrate|as|nh4|no3|compound|npk|fym|an',tolower(fert_type)),fert_cat := 'UM']
  d1[is.na(fert_cat) & grepl('pig|slurry|manure|compost|urine|cattle|residue|fish|vetc|crop|msw|straw|rapese|excreta',tolower(fert_type)),fert_cat := 'OM']
  d1[grepl('as|ammonium_sulph|ammonium sulph|nh4so4',tolower(fert_type)) & is.na(fert_cat), fert_cat := 'AS']
  d1[fert_cat =='AS' & grepl('potassium|npk starter',tolower(fert_type)),fert_cat := NA_character_]
  d1[fert_cat =='AS'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat|slow|polym|contr|\\+ni|ni$',tolower(fert_type)), fert_cat := 'ASC']
  d1[grepl('asn|ammonium sulphate nitrate',tolower(fert_type)), fert_cat := 'ASN']
  d1[fert_cat =='ASN'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat|\\+ni',tolower(fert_type)), fert_cat := 'ASNC']
  d1[grepl('an|ammonium nitra|nh4no3|ammoini',tolower(fert_type))& is.na(fert_cat), fert_cat := 'AN']
  d1[fert_cat =='AN'& grepl('dmpp|dmpsa|dcd|inhib|formalde|entec|coat',tolower(fert_type)), fert_cat := 'ANC']
  d1[is.na(fert_cat),fert_cat := 'O']

  # application mode
  d1[grepl('broadc$|broadcast|surface broad|surface plac|granul|solid|surface applie',tolower(application_mode)), appc := 'broadcast']
  d1[grepl('band|place$',tolower(application_mode)), appc := 'banded']
  d1[grepl('incorp$|incorporated|inject',tolower(application_mode)), appc := 'incorporated']
  d1[grepl('irrig|ferti|liquid',application_mode),appc := 'irrigation']
  d1[is.na(appc), appc := 'unknown']

  # crop category
  d1[grepl('cereal|maiz|corn|wheat|barley|oat',tolower(crop_type)),crop_cat := 'cereal']
  d1[grepl('legum|veget|row|horti|pot',tolower(crop_type)),crop_cat := 'vegetable']
  d1[grepl('rice|paddy',tolower(crop_type)),crop_cat := 'rice']
  d1[grepl('grassl|pasture|grass',tolower(crop_type)),crop_cat := 'grass']
  d1[grepl('fallow|bare',tolower(crop_type)),crop_cat := 'fallow']
  d1[grepl('peren',tolower(crop_type)),crop_cat := 'perennial']
  d1[is.na(crop_cat),crop_cat := 'other']

  # replace missing numeric ones
  d1[is.na(ph),ph := phw_mean_0_5*.1]
  d1[is.na(ph),ph := median(d1$ph,na.rm=T)]
  d1[is.na(soc),soc := soc_mean_0_5*0.01]
  d1[is.na(soc),soc := median(d1$soc,na.rm=T)]
  d1[is.na(clay),clay := clay_mean_0_5*.1]
  d1[is.na(clay),clay := median(d1$clay,na.rm=T)]
  d1[is.na(sand),sand := sand_mean_0_5*.1]
  d1[is.na(sand),sand := median(d1$sand,na.rm=T)]
  d1[is.na(bd),bd := bdod_mean_0_5*0.01]
  d1[is.na(bd),bd := median(d1$bd,na.rm=T)]
  d1[is.na(prec), prec:= pre_mean * 12]
  d1[is.na(prec),prec := median(d1$prec,na.rm=T)]
  d1[is.na(cec),cec := cec_mean_0_5]
  d1[is.na(cec),cec := median(d1$cec,na.rm=T)]
  d1[is.na(temp), temp:= tmp_mean]
  d1[is.na(temp),temp := median(d1$temp,na.rm=T)]
  d1[is.na(ndose), ndose := median(d1$ndose,na.rm=T)]
  d1[ph <= 4, ph := 4]

  # update categories
  d1[,country := tolower(location)]
  d1[is.na(`Irrigation (mm)`), irr := 'unknown']
  d1[, tillage := 'unknown']
  d1[is.na(nsplit),nsplit := 1]
  d1[,nsplitc := as.character(nsplit)]
  d1[nsplit>3, nsplitc := '>3']
  d1[is.na(GEnZ) & country %in% c('ireland','germany','usa'), GEnZ := 'J']
  d1[is.na(GEnZ) & country %in% c('scotland','canada'), GEnZ := 'G']
  d1[is.na(GEnZ) & country %in% c('italy','japan','new zealand','spain'), GEnZ := 'K']
  d1[is.na(GEnZ) & country %in% c('puerto rico','brazil','costa rica'), GEnZ := 'R']

  # set ph type
  d1[grepl('cacl2',tolower(ph_type)), phtype := 'cacl2']
  d1[grepl('water|h2o',tolower(ph_type)), phtype := 'water']
  d1[grepl('kcl',tolower(ph_type)), phtype := 'kcl']
  d1[!grepl('water|h2o|cacl2|kcl',tolower(ph_type)),phtype := 'water']
  
  # cols to delete
  colsr <- colnames(d1)[grepl('cec_|sand_|clay_|silt_|soc_|ntot_|phw_|bdod_|reference|pre_mean|pre_sd|tmp_mean|tmp_sd',colnames(d1))]
  d1[,c(colsr):=NULL]
  colsr <- colnames(d1)[grepl('crop_type|application_|source|pre_crop|crop$|GEnZname|pet_|nsplit$|info|Startda|Durat',colnames(d1))]
  d1[,c(colsr):=NULL]
  
  # convert to log scale
  d1[,log_ef := log(pmax(ef_no3,0.001))]

  # add missing n
  d1[is.na(n),n := 4]
  
  # select variables for a model
  d2 <- d1[,.(studyid,ef_mean = ef_no3,log_ef, lat,lon,region,
              GEnZ, prec,temp,crop_cat,
              sand,clay,soc,cec,ph,bd,n,phtype,measmethod  ='unknown',
              irr,tillage,ndose,nsplitc,appc,
              fert_cat)]
 
  # remove the organic matter treatments
  d2 <- d2[!fert_cat  %in% c('OM') & crop_cat != 'rice']
  
  fwrite(d2,'products/250423_db_no3_final.csv')
  
# make a model to predict N2O
m1 <-lme(log_ef ~ fert_cat+ndose+clay+soc  + prec+temp+I(temp^2), 
         random = ~ 1 |studyid, weights = varFunc(~ n),  
         data=d2,na.action=na.omit)

# get stats
anova(m1)
r.squaredGLMM(m1)

# only the sulphur studies
d4.s <- d4[fert_cat %in% c('AS','ASN','ASC','ASNC'),studyid]
d4.s <- d4[studyid %in% d4.s]

# make a model to predict N2O for AS studies only
m4.s <-lme(log_ef ~ fert_cat+ndose+clay+soc  + prec+temp+I(temp^2)-1, 
         random = ~ 1 |studyid, weights = varFunc(~ n),  
         data=d4.s,na.action=na.omit)

# get stats
anova(m1)
r.squaredGLMM(m1)

p1 <- predict(m1)
plot(p1,d2.s$log_ef)
plot(exp(p1),exp(d2$log_ef),xlim=c(0,0.01),ylim=c(0,0.01))
abline(0,1)
hist(residuals(m1))
summary(lm(p1~d1$EF-1))
