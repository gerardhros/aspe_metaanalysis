
# load in the files
d1 <- fread('products/250429_db_nh3_final.csv')
d2 <- fread('products/250429_db_n2o_final.csv')
d3 <- fread('products/250429_db_no_final.csv')
d4 <- fread('products/250423_db_no3_final.csv')

d1 <- unique(d1[!is.na(lon),.(lon,lat)])
s1 <- st_as_sf(d1,coords = c('lon','lat'),crs = 4326)
s1 <- st_coordinates(s1)
s1 <- as.data.table(s1)

d2 <- unique(d2[!is.na(lon),.(lon,lat)])
s2 <- st_as_sf(d2,coords = c('lon','lat'),crs = 4326)
s2 <- st_coordinates(s2)
s2 <- as.data.table(s2)

d3 <- unique(d3[!is.na(lon),.(lon,lat)])
s3 <- st_as_sf(d3,coords = c('lon','lat'),crs = 4326)
s3 <- st_coordinates(s3)
s3 <- as.data.table(s3)

d4 <- unique(d4[!is.na(lon),.(lon,lat)])
s4 <- st_as_sf(d4,coords = c('lon','lat'),crs = 4326)
s4 <- st_coordinates(s4)
s4 <- as.data.table(s4)

# load in the base map
world <- map_data("world")
p1 <- ggplot() +
      geom_map(
        data = world, map = world,
        aes(long, lat, map_id = region),
        color = "#999999", fill = "#CCCCCC", linewidth = 0.1) +
      geom_point(data = s1,aes(X, Y), alpha = 1, size = 2,shape = 21, 
                 color = "black", fill = "black", stroke = 0.5) +
      geom_point(data = s4,aes(X, Y), alpha = 1, size = 2,shape = 21, 
             color = "blue", fill = "blue", stroke = 0.5) +
      geom_point(data = s2,aes(X, Y), alpha = 1, size = 2,shape = 21, 
                 color = "green3", fill = "green3", stroke = 0.5) +
      geom_point(data = s3,aes(X, Y), alpha = 1, size =2,shape = 21, 
                 color = "red", fill = "red", stroke = 0.5) +
          theme_bw()+ xlab('')+ylab('')+
      #ylim(30,70)+
      #xlim(-20,40)+
      coord_sf(crs=4326)
ggsave(plot=p1,filename='products/samplelocs3.png',width = 18,height=9)

# make histogram per climate region
d5[region=='AF', cont := 'Africa']
d5[region=='EU', cont := 'Europe']
d5[region=='AS', cont := 'Asia']
d5[region %in% c('NAm','Nam'), cont := 'North America']
d5[region=='AU', cont := 'Australia']
d5[region=='SAm', cont := 'South America']

p2 <- ggplot(data = d5[set=='n2o'],aes(x=ef_mean,y=after_stat(density),fill=cont)) + 
      geom_histogram(position='identity',show.legend = FALSE)  +
      geom_density(aes(x=ef_mean,y=after_stat(density)),show.legend = FALSE)+
      facet_wrap(~cont,scales = "free",ncol=6) + theme_bw()+
      #xlab('Ammonia emission fraction (-)')+
      xlab('Nitrous oxide emission fraction (-)')+
      #xlab('Nitric oxide emission fraction (-)')+
      coord_cartesian(xlim = c(0,0.1)) 
      #coord_cartesian(xlim = c(0,0.5)) 

ggsave(plot=p2,filename = 'products/model_nh3_percountry.png',width = 30,height=10,units='cm')
ggsave(plot=p2,filename = 'products/model_n2o_percountry.png',width = 30,height=10,units='cm')
ggsave(plot=p2,filename = 'products/model_no_percountry.png',width = 30,height=10,units='cm')
