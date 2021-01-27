library(ggplot2)
library(ggthemes)
library(ggrepel)
library(gridExtra)
library(grid)
library(sp)
library(cowplot)
library(RColorBrewer)
library(dplyr)
library(maps)
library(mapdata)


# Define plot theme for publication ##################################
theme_Publication <- function(base_size=12, base_family="helvetica") {
  (theme_foundation(base_size=base_size, base_family=base_family)
  + theme(plot.title = element_text(face = "bold",
                                    size = rel(.7), hjust = 0.5),
          plot.subtitle = element_text(face = 'italic', size = rel(.8)),
          text = element_text(),
          panel.background = element_rect(colour = NA),
          plot.background = element_rect(colour = NA),
          panel.border = element_rect(colour = NA),
          axis.title = element_text(face = "bold",size = rel(.7)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(), 
          axis.line = element_line(colour="black"),
          axis.ticks = element_line(),
          panel.grid.major = element_line(colour="#f0f0f0"),
          panel.grid.minor = element_blank(),
          legend.key = element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size= unit(0.2, "cm"),
          legend.margin = unit(0, "cm"),
          legend.title = element_text(face="italic"),
          plot.margin=unit(c(2,3,2,3),"mm"),
          strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
          strip.text = element_text(face="bold")
  ))

}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#e03a61","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...) {
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#e03a61","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

################################################################################

# needs may.cdd, jun.cdd, jul.cdd, aug.cdd, sep.cdd

cdd_corr_data <- matrix(nrow = 5*nrow(sample), ncol = 5)

cdd_corr_data[1:nrow(cdd_corr_data), 1] <- rep(c(may.cdd[ , 1]), 5)
cdd_corr_data[1:nrow(cdd_corr_data), 2] <- rep(c(may.cdd[ , 2]), 5)
cdd_corr_data[1:nrow(cdd_corr_data), 3] <- rep(c(may.cdd[ , 3]), 5)
cdd_corr_data[1:nrow(cdd_corr_data), 5] <- c(rep('May', 53), rep('Jun', 53),
                                             rep('Jul', 53), rep('Aug', 53),
                                             rep('Sep', 53))
cdd_corr_data[1:nrow(cdd_corr_data), 4] <- c(c(rowMeans(may.cdd[, 4:43])),
                                                 c(rowMeans(jun.cdd[, 4:43])),
                                                 c(rowMeans(jul.cdd[, 4:43])),
                                                 c(rowMeans(aug.cdd[, 4:43])),
                                                 c(rowMeans(sep.cdd[, 4:43])))

cdd_corr_data <- as.data.frame(cdd_corr_data)
colnames(cdd_corr_data) <- c('LAT', 'LON', 'ELEV', 'CDD', 'MONTH')

class(cdd_corr_data$LAT) <- 'numeric'
class(cdd_corr_data$LON) <- 'numeric'
class(cdd_corr_data$ELEV) <- 'numeric'
class(cdd_corr_data$CDD) <- 'numeric'
  
LonElev <- ggplot(cdd_corr_data, aes(x=LON, y=ELEV)) + labs(x = 'Longitude', y = 'Elevation (m)') + 
  geom_point(size=2) +scale_colour_Publication() +theme_Publication() +
  theme(plot.title = element_text(size = 10, face = 'bold'), legend.position = "", legend.title = element_blank(),
        axis.title = element_text(size = 10))

LatElev <- ggplot(cdd_corr_data, aes(x=LAT, y=ELEV)) + labs(x = 'Latitude', y = 'Elevation (m)') + geom_point(size=2) +
           scale_colour_Publication() +theme_Publication() + 
           theme(plot.title = element_text(size = 10, face = 'bold'), legend.position = "", legend.title = element_blank(),
           axis.title = element_text(size = 10))

TempLat <- ggplot(cdd_corr_data, aes(x=LAT, y=CDD, colour = as.factor(MONTH))) + labs(x = 'Latitude', y = 'Sum of CDDs (degrees C)') + geom_point(size=2) +
  scale_colour_Publication() +theme_Publication() + 
  theme(plot.title = element_text(size = 10, face = 'bold'), legend.position = "", legend.title = element_blank(),
                                                          axis.title = element_text(size = 10))
TempLon <- ggplot(cdd_corr_data, aes(x=LON, y=CDD, colour = as.factor(MONTH))) + labs(x = 'Longitude', y = 'Sum of CDDs (degrees C)') + geom_point(size=2) +
  scale_colour_Publication() +theme_Publication() + 
  theme(plot.title = element_text(size = 10, face = 'bold'), legend.position = "", legend.title = element_blank(),
        axis.title = element_text(size = 10))

TempElev <- ggplot(cdd_corr_data, aes(x=ELEV, y=CDD, colour = MONTH)) + labs(x = 'Elevation (m)', y = 'Sum of CDDs (degrees C)') + geom_point(size=2) +
  scale_colour_Publication() +theme_Publication() + 
  theme(plot.title = element_text(size = 10, face = 'bold'), legend.position = "below", legend.title = element_blank(),
        axis.title = element_text(size = 10))

LegendMonth <- ggplot(cdd_corr_data, aes(x=ELEV, y=CDD, colour = MONTH)) + geom_point() + scale_colour_Publication() +
               theme_Publication() + theme(legend.title = element_blank())

LegendMonth <- cowplot::get_legend(LegendMonth)

# PLOT FIGURE 1

png(filename="fig1.png", 
    units="in", 
    width=6.5, 
    height=10, 
    pointsize=12, 
    res=600)
grid.arrange(LatElev, LonElev, TempLat, TempLon, TempElev, LegendMonth, ncol = 2)

dev.off()


# PLOT FIGURE 2

cdd.data@data$IDWmean <- rowMeans(cdd.data@data[,1:40])
cdd.data@data$OKmean <- rowMeans(cdd.data@data[,41:80])
cdd.data@data$RKmean <- rowMeans(cdd.data@data[,81:120])

stations_layer <- list('sp.points', stcoords, col = 'cyan', pch='+', cex = 1.5) 
at.cdd <- seq(0, 300, 20)

IDWavg <- spplot(cdd.data, 'IDWmean', sp.layout = stations_layer, col='transparent', at=at.cdd, main = list(label='Inverse-Distance Weighting', cex=0.9)) + theme_Publication()
OKavg <- spplot(cdd.data, 'OKmean', sp.layout = stations_layer, col='transparent', at=at.cdd, main = list(label='Ordinary Kriging', cex=0.9))
RKavg <- spplot(cdd.data, 'RKmean', sp.layout = stations_layer, col='transparent', at=at.cdd, main = list(label='Regression Kriging', cex=0.9))
ELEVplot <- spplot(geo.elev, sp.layout = stations_layer, col.regions = rev(greyscale), cuts = 8, main=list(label='Elevation (m)', cex=0.9))

png(filename="fig2.png", 
    units="in", 
    width=5.5, 
    height=6, 
    pointsize=12, 
    res=600)
grid.arrange(ELEVplot, IDWavg, OKavg, RKavg, nrow = 2)
dev.off()


#################################################################################

for (i in 163:202) {class(data[,i]) <- 'numeric'}
data$yield <- NA
for (i in 1:nrow(data)) { data$yield[i] <- rowMeans(data[i, 163:202]) }
yieldmeans <- data[, c(1, 203)]

states <- map_data('state')
ILIA <- subset(states, region %in% c("illinois", "iowa"))

counties <- map_data("county")
ILIA_county <- subset(counties, region %in% c("illinois", "iowa"))

for (i in 1:nrow(ILIA_county)) {
  if (ILIA_county$region[i] == 'illinois') {
    ILIA_county$subregion[i] <- paste(toupper(ILIA_county$subregion[i]), '_IL')
  }
  else { ILIA_county$subregion[i] <- paste(toupper(ILIA_county$subregion[i]), '_IA') }
}

data.sp <- merge(ILIA_county, yieldmeans, by.x = 'subregion', by.y = 'NAME')
data.sp <- merge(data.sp, RPDIFF, by.x = 'subregion', by.y = 'NAME')
data.sp <- merge(data.sp, RPDIFF2, by.x = 'subregion', by.y = 'NAME')
data.sp <- arrange(data.sp, group, order)

class(data.sp$yield) <- 'numeric'

# PLOT FIGURE 3 

ILIA_base <- ggplot(data = ILIA, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray") + 
  theme_Publication() + scale_colour_Publication() + scale_fill_Publication() 

yieldplot <- ILIA_base + 
  geom_polygon(data = data.sp, aes(fill = yield), color = 'white') +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='red', high='blue') + labs(title = 'Average Corn Yields: 1980 - 2010 (bu/ha)' ) +
  theme(legend.key = element_rect(colour = NA),
                           legend.position = "right",
                           legend.direction = "vertical",
                           legend.key.width = unit(0.1, "cm"),
                           legend.key.height = unit(1.5, 'cm'),
                           legend.margin = unit(0, "cm"),
                           legend.title = element_blank())

png(filename="fig3.png", 
    units="in", 
    width=5.5, 
    height=6, 
    pointsize=12, 
    res=600)
yieldplot
dev.off()


# PLOT FIGURE 4

NOINStoNN <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method == 'NN', ], aes(fill = full), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white', breaks=seq(-70,10, 10)) + labs(title = 'Uninsured to Nearest-Neighbor Index' ) +
  labs(fill = '%') + theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(1.5, 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank())

NOINStoI <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = full), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white', breaks=seq(-70,10, 10)) + labs(title = 'Uninsured to Interpolation Index' ) +
  labs(fill = '%') + theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(1.5, 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank())

NNtoI <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = fulltwo), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='red', breaks=seq(-20,20, 5)) + labs(title = 'Nearest-Neighbor to Interpolation Index' ) +
  labs(fill = '') + theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(1.5, 'cm'),
        legend.margin = unit(0, "cm"))


NOINS.NN.DIST <- ggplot(data = RPDIFF[RPDIFF$method == 'NN', ], aes(x = dist, y = full)) + 
  geom_point(size = 2, color = '#8634eb', alpha = 0.5) + geom_smooth(size = 2, color = '#8634eb', method = 'lm') + labs(title = 'Uninsured to Nearest-Neighbor Index', x = 'Distance to Nearest Station (m)', y ='Relative RP change (%)') +
  theme_Publication() + scale_colour_Publication()

NOINS.I.DIST <- ggplot(data = RPDIFF[RPDIFF$method != 'NN', ], aes(x = dist, y = full, colour = method)) + 
  geom_point(size = 2) + geom_smooth( method = 'lm') + labs(title = 'Uninsured to Interpolation Index', x = 'Distance to Nearest Station (m)', y ='Relative RP change (%)') +
  theme_Publication() + scale_colour_Publication() + scale_fill_Publication() + labs(colour = 'Method')

NN.I.DIST <- ggplot(data = RPDIFF2, aes(x = dist2, y = fulltwo, colour = method2)) + 
  geom_point(size = 2) + geom_smooth( method = 'lm') + labs(title = 'Nearest-Neighbor to Interpolation Index', x = 'Distance to Nearest Station (m)', y ='Relative RP change (%)') +
  theme_Publication() + scale_colour_Publication() + scale_fill_Publication() + 
  labs(colour = 'Method') 

png(filename="fig4.png", 
    units="in", 
    width=8.5, 
    height=13, 
    pointsize=8, 
    res=900)
grid.arrange(NOINStoNN, NOINS.NN.DIST, NOINStoI, NOINS.I.DIST, NNtoI, NN.I.DIST, ncol = 2)
dev.off()


# PLOT FIGURE 5

alpha1 <- ggplot(data = RPDIFF1, aes(x = Distance, y = X1, colour = method2)) + 
  geom_point(size = 2) + geom_smooth( method = 'lm') + labs(title = 'CRRA Coefficient: 1', x = 'Distance to Nearest Station (m)', y ='Relative RP change (%)') +
  theme_Publication() + scale_colour_Publication() + scale_fill_Publication() + 
  labs(colour = 'Method') 

alpha2 <- ggplot(data = RPDIFF2, aes(x = Distance, y = X2, colour = method2)) + 
  geom_point(size = 2) + geom_smooth( method = 'lm') + labs(title = 'CRRA Coefficient: 2', x = 'Distance to Nearest Station (m)', y ='Relative RP change (%)') +
  theme_Publication() + scale_colour_Publication() + scale_fill_Publication() + 
  labs(colour = 'Method') 

alpha3 <- ggplot(data = RPDIFF3, aes(x = Distance, y = X3, colour = method2)) + 
  geom_point(size = 2) + geom_smooth( method = 'lm') + labs(title = 'CRRA Coefficient: 3', x = 'Distance to Nearest Station (m)', y ='Relative RP change (%)') +
  theme_Publication() + scale_colour_Publication() + scale_fill_Publication() + 
  labs(colour = 'Method')

alpha4 <- ggplot(data = RPDIFF4, aes(x = Distance, y = X4, colour = method2)) + 
  geom_point(size = 2) + geom_smooth( method = 'lm') + labs(title = 'CRRA Coefficient: 4', x = 'Distance to Nearest Station (m)', y ='Relative RP change (%)') +
  theme_Publication() + scale_colour_Publication() + scale_fill_Publication() + 
  labs(colour = 'Method')

alpha5 <- ggplot(data = RPDIFF5, aes(x = Distance, y = X5, colour = method2)) + 
  geom_point(size = 2) + geom_smooth( method = 'lm') + labs(title = 'CRRA Coefficient: 5', x = 'Distance to Nearest Station (m)', y ='Relative RP change (%)') +
  theme_Publication() + scale_colour_Publication() + scale_fill_Publication() + 
  labs(colour = 'Method') 

png(filename="fig5.png", 
    units="in", 
    width=6.5, 
    height=10, 
    pointsize=12, 
    res=600)
grid.arrange(alpha1, alpha2, alpha3, alpha4, alpha5, ncol = 2)
dev.off()


#################################################################################

# APPENDIX A FIGURE 1

A1.df <- data.frame('LAT' = cdd.data@coords[,2],
                    'LON' = cdd.data@coords[,1],
                    'IDW' = cdd.data@data$V42,
                    'OK' = cdd.data@data$V82,
                    'RK' = cdd.data@data$V122)



STATIONS <- sample[, 2:3]



A1PLOT <- ggplot() + geom_tile(data = A1.df, aes(x = LON, y = LAT, fill = RK)) + theme_Publication() +
  scale_fill_gradient(low='yellow', high='red') + labs(title = 'Farm Close and Farm Remote', fill='2019 CDDs (RK)') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = 'right',
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1.1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_text(size = 8, face='bold')) + geom_point(data = STATIONS, aes(x = LON, y = LAT)) +
  geom_point(data = FARMS, aes(x = LON, y = LAT, shape = NAME), size = 3, fill='blue') + 
  scale_shape_manual(values = c(22,23)) 


png(filename="figA1.png", 
    units="in", 
    width=5.5, 
    height=5, 
    pointsize=12, 
    res=600)
A1PLOT
dev.off()


# PLOT FIGURE B.1

noinsNNQ1 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method == 'NN', ], aes(fill = Q1), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white', breaks=seq(-90,10, 10)) + labs(title = '% Change RP: Uninsured to Nearest-Neighbor',
                                                                                                     subtitle = '1st Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
                           legend.position = "right",
                           legend.direction = "vertical",
                           legend.key.width = unit(0.1, "cm"),
                           legend.key.height = unit(rel(1), 'cm'),
                           legend.margin = unit(0, "cm"),
                           legend.title = element_blank() )

noinsNNQ2 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method == 'NN', ], aes(fill = Q2), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white', breaks=seq(-100,1000, 200)) + labs(title = '',
                                                                                                     subtitle = '2nd Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )

noinsNNQ3 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method == 'NN', ], aes(fill = Q3), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white', breaks=seq(-90,30, 10)) + labs(title = '',
                                                                                                         subtitle = '3rd Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )

noinsNNQ4 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method == 'NN', ], aes(fill = Q4), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white', breaks=seq(-65,55, 10)) + labs(title = '',
                                                                                                     subtitle = '4th Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )


noinsIQ1 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = Q1), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white') + labs(title = '% Change RP: Uninsured to Interpolation',
                                                                                                     subtitle = '1st Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )

noinsIQ2 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = Q2), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white') + labs(title = '',
                                                                             subtitle = '2nd Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )

noinsIQ3 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = Q3), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white') + labs(title = '',
                                                                            subtitle = '3rd Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )

noinsIQ4 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = Q4), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='white') + labs(title = '',
                                                                             subtitle = '4th Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )


NNIQ1 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = Q1two), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='red') + labs(title = '% Change RP: Nearest-Neighbor to Interpolation',
                                                                             subtitle = '1st Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )

NNIQ2 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = Q2two), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='red') + labs(title = '',
                                                                             subtitle = '2nd Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )

NNIQ3 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = Q3two), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='red') + labs(title = '',
                                                                             subtitle = '3rd Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )

NNIQ4 <- ILIA_base + 
  geom_polygon(data = data.sp[data.sp$method != 'NN', ], aes(fill = Q4two), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_Publication() + scale_fill_gradient(low='blue', high='red') + labs(title = '',
                                                                             subtitle = '4th Quartile Revenue') +
  theme(legend.key = element_rect(colour = NA),
        legend.position = "right",
        legend.direction = "vertical",
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(rel(1), 'cm'),
        legend.margin = unit(0, "cm"),
        legend.title = element_blank() )


png(filename="figB1.png", 
    units="in", 
    width=8.5, 
    height=12, 
    pointsize=12, 
    res=600)
grid.arrange(noinsNNQ1, noinsIQ1, NNIQ1,
             noinsNNQ2, noinsIQ2, NNIQ2,
             noinsNNQ3, noinsIQ3, NNIQ3,
             noinsNNQ4, noinsIQ4, NNIQ4, nrow = 4)
dev.off()



