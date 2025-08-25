# covariate extraction for meta-analysis

# clear environment
rm(list=ls())

# require packages
require(readxl);require(terra);require(data.table);require(sf)
require(stringr); require(foreign)

# load in the data with location from Wim
# wloc <- 'D:/SPRINGG/Wim Bussink - ASPE project/Database/'
# d1 <- as.data.table(readxl::read_xlsx(paste0(wloc,'241121_database_integrated_v2.xlsx'),sheet='database'))
# d1 <- as.data.table(readxl::read_xlsx('D:/ROSG/2001.N.24 APSE metaanalyse/250310 database aspe.xlsx',sheet='AS'))
# d1 <- d1[!is.na(EF)]

# load in the file prepared by Gerard (updated from files Wim)

  if(fnh3 == TRUE){
    
    d1 <- as.data.table(readxl::read_xlsx('data/241121_database_integrated_v2.xlsx',sheet='database'))
    d1[is.na(lat) & country=='China', c('lat','lon') := list(39.95,119.5)]
    d1 <- d1[,.(sid = obs_id,lat,lon)]
  }

  if (fn2o == TRUE){
    
    # load in file for N2O
    d1.as <- as.data.table(readxl::read_xlsx('data/250422 database aspe n2o.xlsx',sheet='AS'))
    d1.other <- as.data.table(readxl::read_xlsx('data/250422 database aspe n2o.xlsx',sheet='Andere meststoffen'))
    
    # add averaged coordinates for country (derived via Google Maps, manually)
    d1.other[is.na(lat) & grepl('Ireland',country),c('lat','lon') := list(53.24,-7.667)]
    d1.other[is.na(lat) & grepl('Japan',country),c('lat','lon') := list(35.867,138.791)]
    d1.other[is.na(lat) & grepl('Australia',country),c('lat','lon') := list(-34.0366,118.5023)]
    d1.other[is.na(lat) & grepl('Italy',country),c('lat','lon') := list(44.579855,10.944755) ]
    d1.other[is.na(lat) & grepl('Germany',country),c('lat','lon') := list(50.626552,10.005787) ]
    d1.other[is.na(lat) & grepl('India',country),c('lat','lon') := list(21.156265,78.576211)]
    d1.other[is.na(lat) & grepl('Kenya',country),c('lat','lon') := list(-0.2012,38.002)]
    d1.other[is.na(lat) & grepl('Scotland',country),c('lat','lon') := list(56.816,-4.5379)]
    d1.other[is.na(lat) & grepl('Brazil',country),c('lat','lon') := list(-21.0403,-44.12640)]
    d1.other[is.na(lat) & grepl('USA',country),c('lat','lon') := list(38.66898,-96.30167)]
    d1.other[is.na(lat) & grepl('England|UK',country),c('lat','lon') := list(51.19,-1.918)]
    d1.other[is.na(lat) & grepl('Russia',country),c('lat','lon') := list(54.616,39.6029)]
    d1.other[is.na(lat) & grepl('Canada',country),c('lat','lon') := list(54.128984,-110.016)]
    d1.other[is.na(lat) & grepl('Wales',country),c('lat','lon') := list(52.054456,-3.582238)]
    d1.other[is.na(lat) & grepl('China',country),c('lat','lon') := list(25.867952,107.1578)]
    d1.other[is.na(lat) & grepl('Denmark',country),c('lat','lon') := list(55.975,8.903) ]
    d1.other[is.na(lat) & grepl('Costa',country),c('lat','lon') := list(9.767,-83.8695)]
    d1.other[is.na(lat) & grepl('France',country),c('lat','lon') := list(47.46337,2.602)]
    d1.other[is.na(lat) & grepl('New Zeal',country),c('lat','lon') := list(-45.4935,169.7237)]
    d1.other[is.na(lat) & grepl('Mali',country),c('lat','lon') := list(15.6428,-3.769)]
    d1.other[is.na(lat) & grepl('Indonesia',country),c('lat','lon') := list(-3.7818,104.418) ]
    d1.other[is.na(lat) & grepl('Hunga',country),c('lat','lon') := list(46.7203,19.0229) ]
    d1.other[is.na(lat) & grepl('Sweden',country),c('lat','lon') := list(59.8017,17.4834)]
    d1.other[is.na(lat) & grepl('Spain',country),c('lat','lon') := list(37.6906,-4.5423) ]
    d1.other[is.na(lat) & grepl('Netherl',country),c('lat','lon') := list(52.076,5.694) ]
    d1.other[is.na(lat) & grepl('Zimbab',country),c('lat','lon') := list(-19.701,30.248) ]
    d1.other[is.na(lat) & grepl('Norway',country),c('lat','lon') := list(59.69448,9.2333)]
    d1.other[is.na(lat) & grepl('Malays',country),c('lat','lon') := list(3.5578,101.4209)]
    d1.other[is.na(lat) & grepl('Belgium',country),c('lat','lon') := list(50.9872,3.9324)]
    d1.other[is.na(lat) & grepl('Philipp',country),c('lat','lon') := list(7.8117,125.37)]
    
    d1 <- rbind(d1.as[,.(sid,lat,lon)],
                d1.other[,.(sid,lat,lon)]) 
    d1 <- d1[!is.na(lat)]
  }
  
  if (fno == TRUE){
    
    # load in file for NO
    d1.as <- as.data.table(readxl::read_xlsx('data/250704 database aspe no.xlsx',sheet='AS'))
    d1.other <- as.data.table(readxl::read_xlsx('data/250704 database aspe no.xlsx',sheet='Andere meststoffen'))
    
    # combine both
    d1 <- rbind(d1.as[,.(sid,lat,lon)],
                d1.other[,.(sid,lat,lon)]) 
    d1 <- d1[!is.na(lat)]
  }
  
if (fno3 == TRUE){
  
  # load in file for NO3
  d1 <- as.data.table(readxl::read_xlsx('data/250422 database aspe no3.xlsx',sheet='data'))
  d1 <- d1[!is.na(lat),.(sid,lat,lon)]
}
  
id(fint == TRUE){
  
  d1 <- readRDS('products/nculocs.rds')
  setnames(d1,c('x','y'),c('lon','lat'))
  d1 <- d1[,lapply(.SD,mean),by=gncu2010_ext]
  
}  
  # prepare spatial object with sites
  #d1.sven <- d1[,.(long_cor2,lat_cor2,id_site)]
  #d1.sven <- unique(d1.sven)
  #s1 <- st_as_sf(d1.sven,coords = c('long_cor2','lat_cor2'),crs = 4326)
  #st_write(s1,'products/220603 exp sites.gpkg')

# convert to spatial object
s1 <- st_as_sf(d1,coords = c('lon','lat'),crs = 4326)
s1 <- vect(s1)

# read in dbf file for metzger climatic regions
s2 <- foreign::read.dbf('D:/DATA/03 metzger/GenS_v3.dbf')

# what rasters are available
# downloaded via QGIS for ISRIC, 0.5 degrees resolution, https://maps.isric.org/
# downloaded via CRU, https://catalogue.ceda.ac.uk/uuid/89e1e34ec3554dc98594a5732622bce9
# downloaded via https://datashare.ed.ac.uk/handle/10283/3089

# read in the rasters via hard drive
r1 <- list.files('D:/DATA/01 soil',pattern = 'tif|nc',full.names = T)
r1 <- r1[!grepl('stack',r1)]
r2 <- list.files('D:/DATA/02 climate',pattern = 'tif|nc',full.names = T)
r3 <- list.files('D:/DATA/03 metzger',pattern = 'tif|nc',full.names = T)

# read in the raster files and convert to spatrasters
isric <- sds(r1)
climate <- sds(r2)
metzger <- rast(r3)
isric <- rast(isric)
climate <- rast(climate)

# --- extract isric data ----

  # update names of isric raster to avoid duplication in names
  names(isric) <- str_split_fixed(names(isric),"_isric_",2)[,2]

  # extract data for the spatial objects
  d1.isric <- terra::extract(x = isric, y = s1)
  d2.isric <- terra::extract(x = isric, y = buffer(s1,width = 10000), fun = mean, na.rm=T)
  d3.isric <- terra::extract(x = isric, y = buffer(s1,width = 20000), fun = mean, na.rm=T)
  d4.isric <- terra::extract(x = isric, y = buffer(s1,width = 30000), fun = mean, na.rm=T)
  d5.isric <- terra::extract(x = isric, y = buffer(s1,width = 50000), fun = mean, na.rm=T)

  # convert to data.table to facilitatie re-arranging
  setDT(d1.isric);setDT(d2.isric);setDT(d3.isric);setDT(d4.isric);setDT(d5.isric)

    # function to adapt colnames
    acn <- function(x,var='or'){c('ID',paste0(var,'_',gsub('isric_|_mean_|','',x[-1])))}

    # adapt colnames
    setnames(d1.isric,acn(colnames(d1.isric)))
    setnames(d2.isric,acn(colnames(d2.isric),'e1'))
    setnames(d3.isric,acn(colnames(d3.isric),'e2'))
    setnames(d4.isric,acn(colnames(d4.isric),'e3'))
    setnames(d5.isric,acn(colnames(d5.isric),'e4'))

  c1.isric <- merge(d1.isric,d2.isric,by = "ID")
  c1.isric <- merge(c1.isric,d3.isric,by = "ID")
  c1.isric <- merge(c1.isric,d4.isric,by = "ID")
  c1.isric <- merge(c1.isric,d5.isric,by = "ID")

  c1.isric <- melt(c1.isric,id = 'ID',
                   measure=patterns("or_", "e1_","e2_","e3_","e4_"),
                   variable.factor = FALSE,
                   value.name = c("or", "e1","e2","e3","e4"))
  c1.isric[,variable := sort(names(isric))[as.integer(variable)]]
  c1.isric[,value := as.numeric(or)]
  c1.isric[or < 1 & e1 > 1,value := e1]
  c1.isric[or < 1 & e2 > 1,value := e2]
  c1.isric[or < 1 & e3 > 1,value := e3]
  c1.isric[or < 1 & e4 > 1,value := e4]

  c2.isric <- dcast(c1.isric,ID~variable, value.var = 'value')


# --- extract climate data ----


  # extract climate data nc files
  d1.climate <- terra::extract(x = climate, y = s1)

  # convert to data.table
  d1.climate <- as.data.table(d1.climate)

  # rearrange data
  d2.climate <- melt(d1.climate,id.vars = 'ID', variable.name = 'variable')
  d2.climate <- as.data.table(d2.climate)
  d2.climate <- d2.climate[!grepl('_stn_',variable)]
  d2.climate[, cvar :=  stringr::str_extract_all(variable,"(?<=[0-9]{4}\\.[0-9]{4}\\.).+(?=\\.dat_)",simplify = T)]
  d2.climate[, years :=  stringr::str_extract_all(variable,"[0-9]{4}\\.[0-9]{4}",simplify = T)]
  d2.climate[, month :=  stringr::str_extract_all(variable,"(?<=[a-z]{3}_)\\d+",simplify = T)]

  # estimate mean global climate properties over 1991-2019
  # temperature in degrees (mean = tmp, max = tmx, min = tmn)
  # potential evaporation in mm/day
  # precipitation in mm/month
  d3.climate <- dcast(d2.climate,ID+years+month~cvar,value.var = 'value')

  # derive the mean and SD per gridcel over period 1991-2019
  c1.climate <- d3.climate[,list(pre_mean = mean(pre),
                                 pre_sd = sd(pre),
                                 tmp_mean = mean(tmp),
                                 tmp_sd = sd(tmp),
                                 pet_mean = mean(pet),
                                 pet_sd = sd(pet)
                                 ),by='ID']
  c2.climate <- copy(c1.climate)

# --- extract metzger data

  # extract for measurement points
  d1.metzger <- terra::extract(x = metzger, y = s1)
  d2.metzger <- terra::extract(x = metzger, y = buffer(s1,width = 10000), fun = mean, na.rm=T)
  d2.metzger <- as.data.table(d2.metzger)
  d3.metzger <- merge(as.data.table(d1.metzger),
                      d2.metzger[,.(ID,gens_v3B= round(gens_v3))],by='ID',all.x=TRUE)
  d3.metzger <- d3.metzger[,.(ID,gens_v3 = fifelse(is.na(gens_v3),gens_v3B,gens_v3))]
  d3.metzger[,gens_v3 := nafill(gens_v3,type='locf')]
  
  # read metzger decription
  s2.dt <- as.data.table(s2)

  # merge description
  d1.metzger <- as.data.table(d3.metzger)
  c2.metzger <- merge(d1.metzger,s2.dt,by.x = 'gens_v3', by.y = 'GEnS_seq')

  # subset
  c2.metzger <- c2.metzger[,.(ID,GEnZname,GEnZ,GEnS)]

# merge the data files

  dt <- d1[,.(ID = obs_id,lon = lon,lat = lat, study_id)]
  dt <- d1[,.(ID = sid,lon = lon,lat = lat)]
  dt <- d1[,.(ID = gncu2010_ext,lon = lon,lat = lat)]
  dt <- merge(dt,c2.isric, by = 'ID',all.x=TRUE)
  dt <- merge(dt,c2.climate, by = 'ID',all.x=TRUE)
  dt <- merge(dt,c2.metzger,by='ID',all.x=TRUE)
 
  # overwrite missing values for INTEGRATOR
  dt[,pre_mean := nafill(pre_mean,type='locf')]
  dt[, tmp_mean := nafill( tmp_mean,type='locf')]
  dt[,pre_sd := nafill(pre_sd,type='locf')]
  dt[,tmp_sd := nafill(tmp_sd,type='locf')]
  
# save the file
  fwrite(dt,'products/241121 covariates metaanalysis.csv')

  setnames(dt,'ID','sid')
  fwrite(dt,'products/250425 covariates metaanalysis n2o.csv')
  fwrite(dt,'products/250425 covariates metaanalysis no.csv')
  fwrite(dt,'products/250425 covariates metaanalysis no3.csv')
  
  fwrite(dt,'products/250429 covariates metaanalysis nh3.csv')
  fwrite(dt,'products/250429 covariates metaanalysis n2o.csv')
  fwrite(dt,'products/250429 covariates metaanalysis no.csv')
  
  setnames(dt,'ID','gncu2010_ext')
  saveRDS(dt,'products/250425 covariates integrator.rds')  
  