library(dplyr)
library(sqldf)
library(MASS)
library(gridExtra)

setwd('C:/Users/danie/ghcnd_all')

stations <- sqldf( 'SELECT X1, X2, X3, X4  
                    FROM ghcnd_stations
                    WHERE X5 = "IL" OR X5 = "IA"') # extract stations in Illinois and Iowa

names <- stations[ , 1]

station_list <- list()
not_temp <- list()

begin <- Sys.time()

for (i in 1:length(names)) { # load and clean .dly files from the ghcnd dataset (source in documentation)
  
  station_list[[i]] <- read.fwf(paste(names[i],'.dly', sep=""), widths = c(11, 4, 2, 4, rep(c(5, 1, 1, 1),31)))

  station_list[[i]] <- station_list[[i]][station_list[[i]]$V4 == 'TMAX' & # we keep maximum daily temperatures observed between 1958 and 2019
                                           station_list[[i]]$V2 > 1958  &
                                           station_list[[i]]$V2 < 2020 , ]

  not_temp[[i]] <- logical(ncol(station_list[[i]]))
  
  for (j in 4:ncol(station_list[[i]])){
    not_temp[[i]][j] <- length(unique(station_list[[i]][, j] )) < 10
  }
  
  station_list[[i]] <- station_list[[i]][, !not_temp[[i]]]
  
  station_list[[i]] <- station_list[[i]][station_list[[i]]$V3 > 4 & 
                                         station_list[[i]]$V3 < 10,  ]
  
  years.exist <- unique(station_list[[i]]$V2)
  
  if ( length(years.exist) > 0 ) { # loop makes sure we only keep stations with complete time series
    
    for ( k in 1:length(years.exist)) {
      
      if (nrow(station_list[[i]][station_list[[i]]$V2 == years.exist[k], ]) != 5) # need five months in each growing season
        
      { station_list[[i]][station_list[[i]]$V2 == years.exist[k], 4:ncol(station_list[[i]]) ] <- NA }
    }
    station_list[[i]] <- na.omit(station_list[[i]])
  }

  # here we remove observations outside our time range 1980-2019
  if ( nrow(station_list[[i]]) == 0 || 
       station_list[[i]]$V2[1] > 1980 || 
       station_list[[i]]$V2[nrow(station_list[[i]])] < 2019 ) 
    
  { station_list[[i]] <- NULL } 
  
}

station_list <- station_list[lengths(station_list) != 0] 

finish <- Sys.time() - begin
print(finish)

years <- c(1980:2019)

stat.list <- list()

for ( i in 1:length(station_list) ) {
 
   if ( all(years %in% station_list[[i]]$V2) )
     
      { stat.list[[i]] <- station_list[[i]] }
  
       else { stat.list[[i]] <- NULL }
  
  stat.list[[i]] <- stat.list[[i]][ stat.list[[i]]$V2 >= 1980, ]
  
 }

stat.list <- stat.list[lengths(stat.list) != 0]

for (i in 1:length(stat.list)) {
  
  stat.list[[i]] <- stat.list[[i]][ stat.list[[i]]$V2 >= 1980, ]
  
}


for (i in 1:length(stat.list)) {
  
  stat.list[[i]][, 4:34] <- stat.list[[i]][, 4:34]/10
  
}


for (i in 1:length(stat.list)) { # transform observed temperature to CDDs > 29 degrees C
 
   for (j in 4:34) {
  
    stat.list[[i]][, j] <- pmax((stat.list[[i]][, j] - 29), 0)
    
  }

}



sum_cdd <- list()

for (i in 1:length(stat.list)) {
  
  sum_cdd[[i]] <- matrix(nrow=nrow(stat.list[[i]]), ncol=1)
  
  for (j in 1:nrow(stat.list[[i]])) {
    
    sum_cdd[[i]][j, 1] <- sum(stat.list[[i]][j, 4:34])
  
  }
  
  sum_cdd[[i]] <- cbind(stat.list[[i]][ , 1:3], sum_cdd[[i]])
  
}



station_names <- matrix(nrow=length(stat.list), ncol=1)

for (i in 1:nrow(station_names)){
  
  station_names[i, ] <- unique(sum_cdd[[i]][, 1])

  }

colnames(stations) <- c('STATION', 'LAT', 'LON', 'ELEV')
colnames(station_names) <- c('STATION')

stations <- merge(station_names, stations, by='STATION')



sum_cdd_year <- list()

for (n in 1:length(years)) {

cdd_by_year <- matrix(nrow=length(stat.list), ncol=5)

  for (i in 1:length(stat.list)) { 
  
     if (length((t(sum_cdd[[i]][sum_cdd[[i]][,2] == years[n], ]))[4, ]) == 5) {
    
         cdd_by_year[i, ] <- (t(sum_cdd[[i]][sum_cdd[[i]][,2] == years[n], ]))[4, ]
    
     }
  
     else { cdd_by_year[i, ] <- NA }
  
  }

  sum_cdd_year[[n]] <- na.omit(cbind(stations[, 1:4], cdd_by_year))

}

# CDDs collected by month for interpolation
may.cdd <- matrix(nrow = 53, ncol = 43)
jun.cdd <- matrix(nrow = 53, ncol = 43)
jul.cdd <- matrix(nrow = 53, ncol = 43)
aug.cdd <- matrix(nrow = 53, ncol = 43)
sep.cdd <- matrix(nrow = 53, ncol = 43)

for (i in 1:length(sum_cdd_year)) {
  
  may.cdd[ , i+3] <- sum_cdd_year[[i]][ , 5]
  jun.cdd[ , i+3] <- sum_cdd_year[[i]][ , 6]
  jul.cdd[ , i+3] <- sum_cdd_year[[i]][ , 7]
  aug.cdd[ , i+3] <- sum_cdd_year[[i]][ , 8]
  sep.cdd[ , i+3] <- sum_cdd_year[[i]][ , 9]
  
}

for (i in 2:4) {

  may.cdd[ , i-1] <- sum_cdd_year[[1]][ , i]
  jun.cdd[ , i-1] <- sum_cdd_year[[1]][ , i]
  jul.cdd[ , i-1] <- sum_cdd_year[[1]][ , i]
  aug.cdd[ , i-1] <- sum_cdd_year[[1]][ , i]
  sep.cdd[ , i-1] <- sum_cdd_year[[1]][ , i]

}

class(may.cdd) <- "numeric"
class(jun.cdd) <- "numeric"
class(jul.cdd) <- "numeric"
class(aug.cdd) <- "numeric"
class(sep.cdd) <- "numeric"


# sampling of stations

sample <- stations[sample(nrow(stations), size=round(1*nrow(stations), digits=0)) , ]


may.cdd <- may.cdd[as.numeric(row.names(sample)), ]
jun.cdd <- jun.cdd[as.numeric(row.names(sample)), ]
jul.cdd <- jul.cdd[as.numeric(row.names(sample)), ]
aug.cdd <- aug.cdd[as.numeric(row.names(sample)), ]
sep.cdd <- sep.cdd[as.numeric(row.names(sample)), ]


# Test whether we have all the stations included:

stations_exist <- vector(length = length(station_names))

for (i in 1:nrow(station_names)) {
  
  for (j in 1:length(sum_cdd_year)) {
    
    if (isTRUE(sum_cdd_year[[j]]$STATION[ grepl(station_names[i], sum_cdd_year[[j]]$STATION)] == station_names[i]))
      
      { stations_exist[i] <- station_names[i] }
    
    else { stations_exist <- NA }
  
  }
 
  print(isTRUE(stations_exist[i] == station_names[i]))

}


# GLM of elevation trend in temperature data, collect residuals for Universal Kriging

resid.May <- matrix(nrow = nrow(sample), ncol = 42)
coefs.May <- matrix(nrow = 2, ncol = 40)

resid.Jun <- matrix(nrow = nrow(sample), ncol = 42)
coefs.Jun <- matrix(nrow = 2, ncol = 40)

resid.Jul <- matrix(nrow = nrow(sample), ncol = 42)
coefs.Jul <- matrix(nrow = 2, ncol = 40)

resid.Aug <- matrix(nrow = nrow(sample), ncol = 42)
coefs.Aug <- matrix(nrow = 2, ncol = 40)

resid.Sep <- matrix(nrow = nrow(sample), ncol = 42)
coefs.Sep <- matrix(nrow = 2, ncol = 40)

for (i in 1:40) { # Generalized Linear Model: CDD ~ Elevation
  # May
  LM.May <- lm(may.cdd[ , i+3] ~ may.cdd[ , 3])
  resid.May[, i+2] <- LM.May$residuals
  coefs.May[, i] <- LM.May$coefficients
  # June
  LM.Jun <- lm(jun.cdd[ , i+3] ~ jun.cdd[ , 3])
  resid.Jun[, i+2] <- LM.Jun$residuals
  coefs.Jun[, i] <- LM.Jun$coefficients
  # July
  LM.Jul <- lm(jul.cdd[ , i+3] ~ jul.cdd[ , 3])
  resid.Jul[, i+2] <- LM.Jul$residuals
  coefs.Jul[, i] <- LM.Jul$coefficients
  # August
  LM.Aug <- lm(aug.cdd[ , i+3] ~ aug.cdd[ , 3])
  resid.Aug[, i+2] <- LM.Aug$residuals
  coefs.Aug[, i] <- LM.Aug$coefficients
  # September
  LM.Sep <- lm(sep.cdd[ , i+3] ~ sep.cdd[ , 3])
  resid.Sep[, i+2] <- LM.Sep$residuals
  coefs.Sep[, i] <- LM.Sep$coefficients

}

resid.May[ , 1:2] <- may.cdd[ , 2:1]
resid.May <- as.data.frame(resid.May)

resid.Jun[ , 1:2] <- jun.cdd[ , 2:1]
resid.Jun <- as.data.frame(resid.Jun)

resid.Jul[ , 1:2] <- jul.cdd[ , 2:1]
resid.Jul <- as.data.frame(resid.Jul)

resid.Aug[ , 1:2] <- aug.cdd[ , 2:1]
resid.Aug <- as.data.frame(resid.Aug)

resid.Sep[ , 1:2] <- sep.cdd[ , 2:1]
resid.Sep <- as.data.frame(resid.Sep)


may.cdd <- as.data.frame(may.cdd)
jun.cdd <- as.data.frame(jun.cdd)
jul.cdd <- as.data.frame(jul.cdd)
aug.cdd <- as.data.frame(aug.cdd)
sep.cdd <- as.data.frame(sep.cdd)


# Spatial data and kriging

library(rgdal)
library(sp)
library(gstat)
library(rgeos)
library(geosphere)
library(raster)
library(elevatr)

setwd('C:/Users/danie/Dropbox/Article/wcasdata')

US <- readOGR(dsn="C:/Users/danie/Dropbox/Article/wcasdata", layer="tl_2019_us_county") # Load shapefile of USA

IL_IA <- subset(US, STATEFP %in% c('17', '19')) # Subset Illinois and Iowa

# Assign coordinate datum and get county center coordinates

crs <- CRS("+proj=longlat +datum=WGS84")
IL_IA <- spTransform(IL_IA, crs)
centroids <- gCentroid(IL_IA, byid=T)
cocoords <- spTransform(centroids, crs)
stcoords <- sample[,2:3]
coordinates(stcoords) <- ~LON+LAT
proj4string(stcoords) <- crs


# Nearest neighbor index sample

cdd_station <- matrix(nrow = 40, ncol = nrow(sample))

for ( i in 1:nrow(cdd_station)) { 
  
  cdd_station[i , ] <- may.cdd[ , i+3] + jun.cdd[ , i+3] + jul.cdd[ , i+3] +
    aug.cdd[ , i+3] + sep.cdd[ , i+3]
  
}

# distances calculated with distm function from geosphere package

dists <- matrix(nrow=length(stcoords), ncol=length(cocoords))

for (i in 1:nrow(stcoords@coords)){
  for (j in 1:nrow(cocoords@coords)){
    dists[i,j] <- distm(stcoords@coords[i,], cocoords@coords[j,], fun=distHaversine)
  }
}


# Select the closest station to each county

closest <- matrix(nrow = 1, ncol = length(cocoords))

for (i in 1:ncol(dists)){
  
  closest[, i ] <- which(dists[,i]==min(dists[,i]))
  
}


c2_cdd <- matrix(nrow=nrow(cdd_station), ncol=ncol(closest))

for (i in 1:ncol(closest)){
  
  c2_cdd[ , i] <- cdd_station[ , closest[1, i] ]
}

# CDDS

coordinates(may.cdd) <- ~V2+V1
proj4string(may.cdd) <- crs

coordinates(jun.cdd) <- ~V2+V1
proj4string(jun.cdd) <- crs

coordinates(jul.cdd) <- ~V2+V1
proj4string(jul.cdd) <- crs

coordinates(aug.cdd) <- ~V2+V1
proj4string(aug.cdd) <- crs

coordinates(sep.cdd) <- ~V2+V1
proj4string(sep.cdd) <- crs

# residuals 

coordinates(resid.May) <- ~V1+V2
proj4string(resid.May) <- crs

coordinates(resid.Jun) <- ~V1+V2
proj4string(resid.Jun) <- crs

coordinates(resid.Jul) <- ~V1+V2
proj4string(resid.Jul) <- crs

coordinates(resid.Aug) <- ~V1+V2
proj4string(resid.Aug) <- crs

coordinates(resid.Sep) <- ~V1+V2
proj4string(resid.Sep) <- crs

# Kriging interpolation

geo.grid <- makegrid(IL_IA, cellsize = 0.1) # Make a grid of interpolation area
geo.grid <- SpatialPoints(geo.grid, proj4string = CRS(proj4string(IL_IA)))
geo.grid <- geo.grid[IL_IA, ]
gridded(geo.grid) <- TRUE

elevation_data <-get_elev_raster(geo.grid, z=3, clip="locations",  src = "aws") # get elevation raster from 'elevatr'-package

geo.elev <- as(elevation_data, 'SpatialGridDataFrame') # turn into SpatialGrid

geo.elev <- geo.elev[geo.grid, ]

elev <- geo.grid %over% geo.elev

coordinates(elev) <- ~geo.grid@coords
proj4string(elev) <- crs
gridded(elev) <- TRUE

elev@data <- cbind(elev@coords, elev@data)
colnames(elev@data) <- c('LON', 'LAT', 'ELEV' )
elev$ELEV[1] <- 96.0


idw.May <- list()
idw.Jun <- list()
idw.Jul <- list()
idw.Aug <- list()
idw.Sep <- list()
                            
OK.May <- list()
OK.Jun <- list()
OK.Jul <- list()
OK.Aug <- list()
OK.Sep <- list()

RK.May <- list()
RK.Jun <- list()
RK.Jul <- list()
RK.Aug <- list()
RK.Sep <- list()

s.OK.May <- list()
s.OK.Jun <- list()
s.OK.Jul <- list()
s.OK.Aug <- list()
s.OK.Sep <- list()

s.RK.May <- list()
s.RK.Jun <- list()
s.RK.Jul <- list()
s.RK.Aug <- list()
s.RK.Sep <- list()

m.OK.May <- list()
m.OK.Jun <- list()
m.OK.Jul <- list()
m.OK.Aug <- list()
m.OK.Sep <- list()

m.RK.May <- list()
m.RK.Jun <- list()
m.RK.Jul <- list()
m.RK.Aug <- list()
m.RK.Sep <- list()


data.May <- matrix(ncol = 123, nrow = 3179)
data.Jun <- matrix(ncol = 123, nrow = 3179)
data.Jul <- matrix(ncol = 123, nrow = 3179)
data.Aug <- matrix(ncol = 123, nrow = 3179)
data.Sep <- matrix(ncol = 123, nrow = 3179)

var.May <- matrix(ncol = 122, nrow = 3179)
var.Jun <- matrix(ncol = 122, nrow = 3179)
var.Jul <- matrix(ncol = 122, nrow = 3179)
var.Aug <- matrix(ncol = 122, nrow = 3179)
var.Sep <- matrix(ncol = 122, nrow = 3179)


# We test three different interpolation techniques: Inverse-Distance Weighting,
# Ordinary kriging, and Regression (or residual) kriging:

interpolation.start <- Sys.time()

for (i in 1:40 ) {
  
  # May
  
  # Inverse Distance Weighting
  idw.May[[i]] <- idw(may.cdd@data[ , i+1]~1, locations=may.cdd, newdata=geo.grid)
  data.May[ , 3+i] <- idw.May[[i]]$var1.pred
  var.May[ , 2+i]  <- idw.May[[i]]$var1.var
  
  # Ordinary kriging
  s.OK.May[[i]] <- variogram(may.cdd@data[ , i+1]~1, may.cdd)
  m.OK.May[[i]] <- fit.variogram(s.OK.May[[i]][s.OK.May[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  OK.May[[i]] <- krige(may.cdd@data[ , i+1]~1, may.cdd, geo.grid, model = m.OK.May[[i]])
  data.May[ , 43+i] <- OK.May[[i]]$var1.pred
  var.May[ , 42+i] <- OK.May[[i]]$var1.var

  # regression kriging
  s.RK.May[[i]] <- variogram(resid.May@data[, i]~1, resid.May)
  m.RK.May[[i]]  <- fit.variogram(s.RK.May[[i]][s.RK.May[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  RK.May[[i]] <- krige(resid.May@data[, i]~1, resid.May, geo.grid, model = m.RK.May[[i]])
  data.May[, 83+i] <- coefs.May[1, i] + coefs.May[2, i]*elev$ELEV + RK.May[[i]]$var1.pred
  var.May[, 82+i] <- RK.May[[i]]$var1.var
  
  # June
  
  # Inverse Distance Weighting
  idw.Jun[[i]] <- idw(jun.cdd@data[ , i+1]~1, locations=jun.cdd, newdata=geo.grid)
  data.Jun[ , 3+i] <- idw.Jun[[i]]$var1.pred
  var.Jun[ , 2+i]  <- idw.Jun[[i]]$var1.var
  
  # Ordinary kriging
  s.OK.Jun[[i]] <- variogram(jun.cdd@data[ , i+1]~1, jun.cdd)
  m.OK.Jun[[i]] <- fit.variogram(s.OK.Jun[[i]][s.OK.Jun[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  OK.Jun[[i]] <- krige(jun.cdd@data[ , i+1]~1, jun.cdd, geo.grid, model = m.OK.Jun[[i]])
  data.Jun[ , 43+i] <- OK.Jun[[i]]$var1.pred
  var.Jun[ , 42+i] <- OK.Jun[[i]]$var1.var
  
  # Residual kriging
  s.RK.Jun[[i]] <- variogram(resid.Jun@data[, i]~1, resid.Jun)
  m.RK.Jun[[i]]  <- fit.variogram(s.RK.Jun[[i]][s.RK.Jun[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  RK.Jun[[i]] <- krige(resid.Jun@data[, i]~1, resid.Jun, geo.grid, model = m.RK.Jun[[i]])
  data.Jun[, 83+i] <- coefs.Jun[1, i] + coefs.Jun[2, i]*elev$ELEV + RK.Jun[[i]]$var1.pred
  var.Jun[, 82+i] <- RK.Jun[[i]]$var1.var
  
  # July
  
  # Inverse Distance Weighting
  idw.Jul[[i]] <- idw(jul.cdd@data[ , i+1]~1, locations=jul.cdd, newdata=geo.grid)
  data.Jul[ , 3+i] <- idw.Jul[[i]]$var1.pred
  var.Jul[ , 2+i]  <- idw.Jul[[i]]$var1.var
  
  # Ordinary kriging
  s.OK.Jul[[i]] <- variogram(jul.cdd@data[ , i+1]~1, jul.cdd)
  m.OK.Jul[[i]] <- fit.variogram(s.OK.Jul[[i]][s.OK.Jul[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  OK.Jul[[i]] <- krige(jul.cdd@data[ , i+1]~1, jul.cdd, geo.grid, model = m.OK.Jul[[i]])
  data.Jul[ , 43+i] <- OK.Jul[[i]]$var1.pred
  var.Jul[ , 42+i] <- OK.Jul[[i]]$var1.var
  
  # Residual kriging
  s.RK.Jul[[i]] <- variogram(resid.Jul@data[, i]~1, resid.Jul)
  m.RK.Jul[[i]]  <- fit.variogram(s.RK.Jul[[i]][s.RK.Jul[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  RK.Jul[[i]] <- krige(resid.Jul@data[, i]~1, resid.Jul, geo.grid, model = m.RK.Jul[[i]])
  data.Jul[, 83+i] <- coefs.Jul[1, i] + coefs.Jul[2, i]*elev$ELEV + RK.Jul[[i]]$var1.pred
  var.Jul[, 82+i] <- RK.Jul[[i]]$var1.var
  
  # August
  
  # Inverse Distance Weighting
  idw.Aug[[i]] <- idw(aug.cdd@data[ , i+1]~1, locations=aug.cdd, newdata=geo.grid)
  data.Aug[ , 3+i] <- idw.Aug[[i]]$var1.pred
  var.Aug[ , 2+i]  <- idw.Aug[[i]]$var1.var
  
  # Ordinary kriging
  s.OK.Aug[[i]] <- variogram(aug.cdd@data[ , i+1]~1, aug.cdd)
  m.OK.Aug[[i]] <- fit.variogram(s.OK.Aug[[i]][s.OK.Aug[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  OK.Aug[[i]] <- krige(aug.cdd@data[ , i+1]~1, aug.cdd, geo.grid, model = m.OK.Aug[[i]])
  data.Aug[ , 43+i] <- OK.Aug[[i]]$var1.pred
  var.Aug[ , 42+i] <- OK.Aug[[i]]$var1.var
  
  #residual kriging
  s.RK.Aug[[i]] <- variogram(resid.Aug@data[, i]~1, resid.Aug)
  m.RK.Aug[[i]]  <- fit.variogram(s.RK.Aug[[i]][s.RK.Aug[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  RK.Aug[[i]] <- krige(resid.Aug@data[, i]~1, resid.Aug, geo.grid, model = m.RK.Aug[[i]])
  data.Aug[, 83+i] <- coefs.Aug[1, i] + coefs.Aug[2, i]*elev$ELEV + RK.Aug[[i]]$var1.pred
  var.Aug[, 82+i] <- RK.Aug[[i]]$var1.var
  
  # September
  
  # Inverse Distance Weighting
  idw.Sep[[i]] <- idw(sep.cdd@data[ , i+1]~1, locations=sep.cdd, newdata=geo.grid)
  data.Sep[ , 3+i] <- idw.Sep[[i]]$var1.pred
  var.Sep[ , 2+i]  <- idw.Sep[[i]]$var1.var
  
  # Ordinary kriging
  s.OK.Sep[[i]] <- variogram(sep.cdd@data[ , i+1]~1, sep.cdd)
  m.OK.Sep[[i]] <- fit.variogram(s.OK.Sep[[i]][s.OK.Sep[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  OK.Sep[[i]] <- krige(sep.cdd@data[ , i+1]~1, sep.cdd, geo.grid, model = m.OK.Sep[[i]])
  data.Sep[ , 43+i] <- OK.Sep[[i]]$var1.pred
  var.Sep[ , 42+i] <- OK.Sep[[i]]$var1.var
  
  # Residual kriging
  s.RK.Sep[[i]] <- variogram(resid.Sep@data[, i]~1, resid.Sep)
  m.RK.Sep[[i]]  <- fit.variogram(s.RK.Sep[[i]][s.RK.Sep[[i]]$np > round(nrow(sample)/3, 0), ], vgm(c('Sph', 'Exp')))
  RK.Sep[[i]] <- krige(resid.Sep@data[, i]~1, resid.Sep, geo.grid, model = m.RK.Sep[[i]])
  data.Sep[, 83+i] <- coefs.Sep[1, i] + coefs.Sep[2, i]*elev$ELEV + RK.Sep[[i]]$var1.pred
  var.Sep[, 82+i] <- RK.Sep[[i]]$var1.var
  
}

interpolation.end <- Sys.time() - interpolation.start
print(interpolation.end)

# Collect interpolated temperatures in spatial objects for plotting

data.May[,1:2] <- geo.grid@coords
data.May[, 3] <- elev$ELEV
data.May <- as.data.frame(data.May)
coordinates(data.May) <- ~V1+V2
proj4string(data.May) <- crs
gridded(data.May) = T

data.Jun[,1:2] <- geo.grid@coords
data.Jun[, 3] <- elev$ELEV
data.Jun <- as.data.frame(data.Jun)
coordinates(data.Jun) <- ~V1+V2
proj4string(data.Jun) <- crs
gridded(data.Jun) = T

data.Jul[,1:2] <- geo.grid@coords
data.Jul[, 3] <- elev$ELEV
data.Jul <- as.data.frame(data.Jul)
coordinates(data.Jul) <- ~V1+V2
proj4string(data.Jul) <- crs
gridded(data.Jul) = T

data.Aug[,1:2] <- geo.grid@coords
data.Aug[, 3] <- elev$ELEV
data.Aug <- as.data.frame(data.Aug)
coordinates(data.Aug) <- ~V1+V2
proj4string(data.Aug) <- crs
gridded(data.Aug) = T

data.Sep[,1:2] <- geo.grid@coords
data.Sep[, 3] <- elev$ELEV
data.Sep <- as.data.frame(data.Sep)
coordinates(data.Sep) <- ~V1+V2
proj4string(data.Sep) <- crs
gridded(data.Sep) = T

# Collect kriging variances in spatial object
variance <- matrix(nrow = 3179, ncol=4)
variance[, 1:2] <- geo.grid@coords
variance[, 3] <- (rowMeans(var.May[,43:82]) + rowMeans(var.Jun[,43:82]) + rowMeans(var.Jul[,43:82])
                    + rowMeans(var.Aug[,43:82]) + rowMeans(var.Sep[,43:82]))/5
variance[, 4] <- (rowMeans(var.May[,83:122]) + rowMeans(var.Jun[,83:122]) + rowMeans(var.Jul[,83:122])
                  + rowMeans(var.Aug[,83:122]) + rowMeans(var.Sep[,83:122]))/5
variance <- as.data.frame(variance)
coordinates(variance) <- ~V1+V2
proj4string(variance) <- crs
gridded(variance) = T
colnames(variance@data) <- c('OK', 'RK')

# Collect interpolated CDDs in spatial object
cdd.data <- matrix(nrow = 3179, ncol=120)

for (i in 1:120) { 
  
  cdd.data[ , i] <- data.May@data[ , i+1] + data.Jun@data[ , i+1] + data.Jul@data[ , i+1] +
          data.Aug@data[ , i+1] + data.Sep@data[ , i+1]
  
}

cdd.data <- as.data.frame(cbind(geo.grid@coords, cdd.data))
coordinates(cdd.data) <- ~x1+x2
proj4string(cdd.data) <- crs
gridded(cdd.data) = T

cdd_sample <- cocoords %over% cdd.data
colnames(cdd_sample) <- years
cdd_sample <- t(cdd_sample)



# Load and clean USDA corn yield data 

IL_YIELD <- read.csv(file = "IL_YIELD.csv") # Ilinois yield
IA_YIELD <- read.csv(file = "IA_YIELD.csv") # Iowa yield

IL_YIELD <- sqldf( 'SELECT Year, State, County, Value
                    FROM IL_YIELD' )

IA_YIELD <- sqldf( 'SELECT Year, State, County, Value
                    FROM IA_YIELD' )

IL_yield <- list()
IA_yield <- list()
IL_COUNTIES <- matrix(nrow=102, ncol=1)
IL_COUNTIES[,1] <- toupper(IL_IA$NAME[IL_IA$STATEFP=='17'])
IA_COUNTIES <- matrix(nrow=99, ncol=1)
IA_COUNTIES[,1] <- toupper(IL_IA$NAME[IL_IA$STATEFP=='19'])
colnames(IL_COUNTIES) <- 'County'
colnames(IA_COUNTIES) <- 'County'

for ( i in 1 : 40) {
  
  IL_yield[[i]] <- IL_YIELD[ IL_YIELD$Year == years[i], ]
  row.names(IL_yield[[i]]) <- 1:nrow(IL_yield[[i]])
  
  IA_yield[[i]] <- IA_YIELD[ IA_YIELD$Year == years[i], ]
  row.names(IA_yield[[i]]) <- 1:nrow(IA_yield[[i]])
  
}

IL_join <- IL_COUNTIES
IA_join <- IA_COUNTIES

for (i in 1:40) {
  
  IL_join <- merge(IL_join, IL_yield[[i]], by = 'County')
  IA_join <- merge(IA_join, IA_yield[[i]], by = 'County')
  
}

IL_merged <- matrix(nrow = nrow(IL_join), ncol = 41)
IA_merged <- matrix(nrow = nrow(IA_join), ncol = 41)

columns <- seq(4, 121, 3)

IL_merged[ , 1] <- paste(IL_join[ , 1], "_IL")
IA_merged[ , 1] <- paste(IA_join[ , 1], "_IA")

for (i in 1:40) { 
  
  IL_merged[ , i+1] <- IL_join[ , columns[i] ]
  IA_merged[ , i+1] <- IA_join[ , columns[i] ]

}

IL_IA_names <- IL_IA@data[, c(1,5)]

for (i in 1:nrow(IL_IA_names)) {
  
  if (IL_IA_names[i, 1] == 17) { IL_IA_names[i, 2] <- toupper(paste(IL_IA_names[i, 2], "_IL")) }
  
  else { IL_IA_names[i, 2] <- toupper(paste( IL_IA_names[i, 2], "_IA")) }

}

IL_IA_yield <- rbind(IL_merged, IA_merged)
colnames(IL_IA_yield) <- c('NAME', 1980:2019)

IL_IA_yield <- merge(IL_IA_names, IL_IA_yield, by = 'NAME')


data <- cbind(IL_IA_names$NAME, t(cdd_sample), t(c2_cdd))
colnames(data) <- c('NAME', paste('IDW', 1980:2019), paste('OK', 1980:2019), paste('RK', 1980:2019), paste('NN', 1980:2019))

data <- merge(data, IL_IA_yield, by='NAME')

cdd_idw <- t(as.matrix(data[ , 2:41]))
cdd_ok <- t(as.matrix(data[ , 42:81]))
cdd_rk <- t(as.matrix(data[ , 82:121]))
cdd_nn <- t(as.matrix(data[ , 122:161]))
yield <- t(as.matrix(data[, 163:202]))

class(cdd_idw) <- "numeric"
class(cdd_ok) <- "numeric"
class(cdd_rk) <- "numeric"
class(cdd_nn) <- "numeric"
class(yield) <- 'numeric'

write.matrix(cdd_idw, file=paste('cdd_idw', nrow(sample), 'stations.csv', sep='') )
write.matrix(cdd_ok, file=paste('cdd_ok', nrow(sample), 'stations.csv', sep='') )
write.matrix(cdd_rk, file=paste('cdd_rk', nrow(sample), 'stations.csv', sep='') )
write.matrix(cdd_nn, file=paste('cdd_nn', nrow(sample), 'stations.csv', sep='') ) 
write.matrix(yield, file='yield.csv')

# yield matrix and detrending following Finger 

# create yield matrix to use in WII contract code

trend<-vector( ,length=ncol(yield))
for (i in 1:ncol(yield)){
  trend[i]<-summary(rlm(yield[,i]~as.numeric(1:40), method = 'M'))$coefficients[2,1]
}

trend_deviation<-t(trend%*%t(as.numeric(39:0)))

yield_detrended<-yield+trend_deviation

row.names(yield_detrended)<-as.numeric(1980:2019)

write.matrix(yield_detrended, file='yield_detrended.csv')


# distances calculated with distm function from geosphere package

mindist <- matrix(nrow = 201, ncol=2)
colnames(mindist) <- c("NAME", "MINDIST")

for (i in 1:ncol(dists)){
  mindist[i, 2] <- min(as.numeric(dists[, i]))
}

mindist[,1] <- IL_IA_names[ , 2]

mindist <- cbind(cocoords@coords, mindist)

mindist <- merge(mindist, data, by='NAME')[ ,1:4]

class(mindist$x)<-'numeric'
class(mindist$y)<-'numeric'
class(mindist$MINDIST)<-'numeric'

coordinates(mindist) <- ~x+y
gridded(mindist) = T
