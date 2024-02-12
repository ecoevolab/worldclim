#worldclim
#Natalia Said
#06/02/2024
#script downloads data form worldclim for recent (1970-2000) and future (2021-2080) projections. 
  #it plots temperature in the wanted section with longs and lats provided. Finally it also delivers dataframe with temperatures.

 
#useful: https://rdrr.io/github/rspatial/geodata/man/cmip6.html
#tratar de necontrar datos específicos de 2010 y 2024 

library(terra)
library(geodata)



#extracts recent data form bioclim average temp. 1970-2000 (12 files each for one month of the year)
bioclim_1970 <- worldclim_global(var = "tavg", res = 2.5, path = "1970_2000/")

#2021-2040", "2041-2060", & "2061-2080"
forecast_21<-cmip6_world(model = "MPI-ESM1-2-HR", ssp= 245, time="2021-2040", var="bioc", res=2.5, path = "2021_2040/")
forecast_41<-cmip6_world(model = "MPI-ESM1-2-HR", ssp= 245, time="2041-2060", var="bioc", res=2.5, path = "2041_2060/")
forecast_61<-cmip6_world(model = "MPI-ESM1-2-HR", ssp= 245, time="2061-2080", var="bioc", res=2.5, path = "2061_2080/")


#lats and longs from farm and research center in NC
{
  latitude = c(35.89177,35.66645)
  longitude = c(-79.0181,-78.493247)
  
  
  #to expand the map 
  max_lat<-latitude[1]+(latitude[1]*0.05)
  min_lat<-latitude[2]-(latitude[2]*0.05)
  max_lon<-longitude[1]-(longitude[1]*0.05)
  min_lon<-longitude[2]+(longitude[2]*0.05)
  
  
  #dataframe que conecta con los 4 puntos 
  geographic_extent <- ext(x = c(min_lon, max_lon, min_lat, max_lat))
  
  
  #Download data with geodata's world function to use for our base map
  world_map <- world(resolution = 3,
                     path = "data/")
  
  
  # Crop the map to our area of interest
  my_map <- crop(x = world_map, y = geographic_extent)
  
}


# Crop bioclim data to desired extent
bioclim_1970<- crop(x = bioclim_1970, y = geographic_extent)
forecast_21<-crop(x = forecast_21, y = geographic_extent)
forecast_41<-crop(x = forecast_41, y = geographic_extent)
forecast_61<-crop(x = forecast_61, y = geographic_extent)

#mean of bioclim_1970 for the 12 months, produces only one graph
bioclim_1970<-mean(bioclim_1970, na.rm=FALSE)

#increases/decresases
{
aumento_40_años<-forecast_41[[1]]-bioclim_1970
aumento_20_años<-forecast_41-forecast_21

plot(aumento_40_años)
}


#PLOT FOR TAVG RECENT 1970-2000
{ plot(bioclim_1970)
  title(main=paste0("Mean Temperature\n(1970 - 2000)"))
  place<-c("Farms","Research")
  text(longitude, latitude, labels=place, pos=2, cex=0.75)
  points(x = longitude, 
         y = latitude, 
         col = "red", 
         pch = 20, 
         cex = 1)
}
#PLOT FOR TAVG PROJECTED 2021-2040
{plot(forecast_21[[1]])
title(main=paste0("Mean Temperature\n(2021 - 2040)"))
place<-c("Farms","Research")
text(longitude, latitude, labels=place, pos=2, cex=0.75)
points(x = longitude, 
       y = latitude, 
       col = "red", 
       pch = 20, 
       cex = 1)
}
#PLOT FOR TAVG PROJECTED 2041-2060
{plot(forecast_41[[1]])
title(main=paste0("Mean Temperature\n(2041 - 2060)"))
place<-c("Farms","Research")
text(longitude, latitude, labels=place, pos=2, cex=0.75)
points(x = longitude, 
       y = latitude, 
       col = "red", 
       pch = 20, 
       cex = 1)
}
#PLOT FOR TAVG PROJECTED 2061-2080
{plot(forecast_61[[1]])
title(main=paste0("Mean Temperature\n(2061 - 2080)"))
place<-c("Farms","Research")
text(longitude, latitude, labels=place, pos=2, cex=0.75)
points(x = longitude, 
       y = latitude, 
       col = "red", 
       pch = 20, 
       cex = 1)
}


#df de longs y lats 
presence<-data.frame(longitude,latitude)

#Extracts climate info with the coordinate sets and converts SPATRASTER to dataframe
bioclim_recent_1970 <- terra::extract(x = bioclim_1970, y = presence[, c("longitude", "latitude")]) 
bioclim_forecast_21 <- terra::extract(x = forecast_21, y = presence[, c("longitude", "latitude")]) 
bioclim_forecast_41 <- terra::extract(x = forecast_41, y = presence[, c("longitude", "latitude")]) 
bioclim_forecast_61 <- terra::extract(x = forecast_61, y = presence[, c("longitude", "latitude")]) 

#une tabla de lats y longs con sus datos de clima
bioclim_recent_1970 <- cbind(presence, bioclim_recent_1970)
bioclim_forecast_21 <- cbind(presence, bioclim_forecast_21[2])
bioclim_forecast_41 <- cbind(presence, bioclim_forecast_41[2])
bioclim_forecast_61 <- cbind(presence, bioclim_forecast_61[2])

