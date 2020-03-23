######################################################################
#Cluster timeseries analysis of Nightlights in Bulgaria              #
#MB2 Introduction to Programming and Geostatistics (MSc. EAGLE, JMUW)#
#Yan Chak Christopher, Chan (s382722) christopherchank@gmail.com     #
#Date                                                                #
######################################################################

setwd("D:/MB2_DATA")

#Libraries----
library(RStoolbox)
library(sp)
library(shapefiles)
library(viridis)
library(rasterVis)
library(tidyverse)
library(rgdal)
library(lubridate)
library(glcm)
library(ClusterR)
library(snow)
library(colortools)
library(ggthemes)

################
#PRE-PROCESSING#
################

#Ingest data----

#Create paths
path_2014 <- "D:/MB2_DATA/201401_201412/"
path_2015 <- "D:/MB2_DATA/201501_201512/"
path_2016 <- "D:/MB2_DATA/201601_201612/"
path_2017 <- "D:/MB2_DATA/201701_201712/"
path_2018 <- "D:/MB2_DATA/201801_201812(NA06)/"
path_2019 <- "D:/MB2_DATA/201901_201904/"

#GREP vcmslcfg avg.rade9h
avg_rade_2014 <- grep("*avg.rade9h", list.files(path=path_2014, pattern="avg.rade9h.tif$"), value=TRUE)
avg_rade_2015 <- grep("*avg.rade9h", list.files(path=path_2015, pattern="avg.rade9h.tif$"), value=TRUE)
avg_rade_2016 <- grep("*avg.rade9h", list.files(path=path_2016, pattern="avg.rade9h.tif$"), value=TRUE)
avg_rade_2017 <- grep("*avg.rade9h", list.files(path=path_2017, pattern="avg.rade9h.tif$"), value=TRUE)
avg_rade_2018 <- grep("*avg.rade9h", list.files(path=path_2018, pattern="avg.rade9h.tif$"), value=TRUE)
avg_rade_2019 <- grep("*avg.rade9h", list.files(path=path_2019, pattern="avg.rade9h.tif$"), value=TRUE)

#Assign path to character string
tif_2014 <- paste0(path_2014, avg_rade_2014)
tif_2015 <- paste0(path_2015, avg_rade_2015)
tif_2016 <- paste0(path_2016, avg_rade_2016)
tif_2017 <- paste0(path_2017, avg_rade_2017)
tif_2018 <- paste0(path_2018, avg_rade_2018)
tif_2019 <- paste0(path_2019, avg_rade_2019)

############
#PROCESSING#
############

#Create raster stack & mask----
tif_2014 = lapply(tif_2014[], raster)
tif_2015 = lapply(tif_2015[], raster)
tif_2016 = lapply(tif_2016[], raster)
tif_2017 = lapply(tif_2017[], raster)                  
tif_2018 = lapply(tif_2018[], raster)
tif_2019 = lapply(tif_2019[], raster)

stack_2014 <- stack(tif_2014[1:length(tif_2014)])
stack_2015 <- stack(tif_2015[1:length(tif_2015)])
stack_2016 <- stack(tif_2016[1:length(tif_2016)])
stack_2017 <- stack(tif_2017[1:length(tif_2017)])
stack_2018 <- stack(tif_2018[1:length(tif_2018)])
stack_2019 <- stack(tif_2019[1:length(tif_2019)])

#Check by plotting stack_2019
gplot(stack_2019)+
  geom_raster(aes(x=x, y=y, fill=value))+
  scale_fill_viridis_c()+
  facet_wrap(~variable)+
  coord_quickmap()+
  ggtitle("Europe_NTL_2019(Jan-Apr)")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

#Read mask
Bug_MASK <- shapefile("D:/MB2_DATA/Bulgaria4326.shp")
compareCRS(tif_2019[[1]], Bug_MASK)

#MASKING----
beginCluster()
BUG_Masked_2014 <- crop(stack_2014, Bug_MASK)
BUG_Masked_2015 <- crop(stack_2015, Bug_MASK)
BUG_Masked_2016 <- crop(stack_2016, Bug_MASK)
BUG_Masked_2017 <- crop(stack_2017, Bug_MASK)
BUG_Masked_2018 <- crop(stack_2018, Bug_MASK)
BUG_Masked_2019 <- crop(stack_2019, Bug_MASK)
endCluster()

setwd("D:/MB2_DATA")

#Check by plotting
gplot(BUG_Masked_2019)+
  geom_raster(aes(x=x, y=y, fill=value))+
  scale_fill_viridis_c()+
  facet_wrap(~variable)+
  coord_quickmap()+
  ggtitle("Bulgaria_NTL_2019(Jan-Apr)")+
  xlab("Longitude")+
  ylab("Latitude")+
  theme_classic()+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Bulgaria NTL 2019(Jan-Apr).png", scale=1.5, dpi=300, overwrite=TRUE)

#Clustering ROIs----

#Create the clusters ROIs using 201401
#First make a copy of 201401 where 0=NA
sample_201401 <- BUG_Masked_2014$SVDNB_npp_20140101.20140131_75N060W_vcmslcfg_v10_c2015006171539.avg_rade9h
sample_201401[sample_201401 == 0] <- NA

#KMeans to 2 classes (Yes lights & No lights)
ROI_clusters <- unsuperClass(na.omit(sample_201401), nClasses=2, nStarts=5, nIter=500, norm=FALSE, algorithm="Lloyd")
plot(ROI_clusters$map)

#RUN THE CODE TILL HERE! CHECK THE PLOT BEFORE CONTINUE

#HERE X SHALL BE CHOSEN BASED ON THE PLOT ABOVE!!
NTL_ROI_BUG <- rasterToPolygons(ROI_clusters$map, fun=function(x) (x==2), n=16, na.rm=TRUE, dissolve=TRUE)
compareCRS(BUG_Masked_2014, NTL_ROI_BUG)
plot(NTL_ROI_BUG)
writeOGR(NTL_ROI_BUG,"D:/MB2_DATA", "NTL_ROI_BUG", driver="ESRI Shapefile", overwrite=TRUE)

#Cropping stacks of ONLY NTL for analysis
beginCluster()
BUGNTL_masked_2014 <- mask(BUG_Masked_2014, NTL_ROI_BUG)
setwd("D:/MB2_DATA/201401_201412/Masked")
writeRaster(BUGNTL_masked_2014, filename=names(BUGNTL_masked_2014), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUGNTL_masked_2015 <- mask(BUG_Masked_2015, NTL_ROI_BUG)
setwd("D:/MB2_DATA/201501_201512/Masked")
writeRaster(BUGNTL_masked_2015, filename=names(BUGNTL_masked_2015), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUGNTL_masked_2016 <- mask(BUG_Masked_2016, NTL_ROI_BUG)
setwd("D:/MB2_DATA/201601_201612/Masked")
writeRaster(BUGNTL_masked_2016, filename=names(BUGNTL_masked_2016), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUGNTL_masked_2017 <- mask(BUG_Masked_2017, NTL_ROI_BUG)
setwd("D:/MB2_DATA/201701_201712/Masked")
writeRaster(BUGNTL_masked_2017, filename=names(BUGNTL_masked_2017), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUGNTL_masked_2018 <- mask(BUG_Masked_2018, NTL_ROI_BUG)
setwd("D:/MB2_DATA/201801_201812(NA06)/Masked")
writeRaster(BUGNTL_masked_2018, filename=names(BUGNTL_masked_2018), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUGNTL_masked_2019 <- mask(BUG_Masked_2019, NTL_ROI_BUG)
setwd("D:/MB2_DATA/201901_201904/Masked")
writeRaster(BUGNTL_masked_2019, filename=names(BUGNTL_masked_2019), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)
endCluster()

#Plot comparison between BUG_Masked_2019 & BUGNTL_Maksed_2019
par(mfrow=c(1, 2))
plot(BUG_Masked_2019$SVDNB_npp_20190401.20190430_75N060W_vcmslcfg_v10_c201905191000.avg_rade9h, main="BulgariaNTL_201904", col=viridis_pal(option = "D")(10))
plot(BUGNTL_masked_2019$SVDNB_npp_20190401.20190430_75N060W_vcmslcfg_v10_c201905191000.avg_rade9h, main="BUGNTL_Masked_201904", col=viridis_pal(option = "D")(10))

##########
#ANALYSES#
##########

#Calculate descriptive stats, Moran's I, & GLCM for each month----

BUGNTL_masked_list <- c(BUGNTL_masked_2014, BUGNTL_masked_2015, BUGNTL_masked_2016, BUGNTL_masked_2017, BUGNTL_masked_2018, BUGNTL_masked_2019)
BUGNTL_masked_brick <- brick(BUGNTL_masked_list)
rm(BUGNTL_masked_list)

#Create empty data.frame to populate
#THE SPATIAL RESOLUTION IS TOO ROUGH FROM SOME GLCM: 
SpaAut_Rad <- tibble(
  "Month"=(1:63),
  "Mean_rad"=(1:63),
  "St_dev"=(1:63),
  "MoranI"=(1:63)
)

#Populate month
list.month <- format(seq(as.Date("2014-01-01"), as.Date("2019-04-30"), by="months"), format="%y-%m")
list.month <- list.month[-54]
SpaAut_Rad$Month <- list.month

#Populate Mean_rad
beginCluster()

for (i in 1:dim(BUGNTL_masked_brick)[3]){
  SpaAut_Rad$Mean_rad <- cellStats(BUGNTL_masked_brick, stat="mean", na.rm=TRUE)
}

#Populate st_dev
for (i in 1:dim(BUGNTL_masked_brick)[3]){
  SpaAut_Rad$St_dev <- cellStats(BUGNTL_masked_brick, stat="sd", na.rm=TRUE)
}

#Populate moranI (closer to 1 the more orderly)
for (i in 1:dim(BUGNTL_masked_brick)[3]){
  SpaAut_Rad$MoranI[i] <- Moran(BUGNTL_masked_brick[[i]], w=matrix(c(1/9), 3, 3))
}

#SEPARATE RASTERBRICK glcm_homogeneity
GLCM_Homo <- list()

for (i in 1:dim(BUGNTL_masked_brick)[3]){
  GLCM_Homo[i] <- glcm(BUGNTL_masked_brick[[i]], n_grey=32, window=c(3, 3), shift=c(1, 1), statistics=c("homogeneity"), na_opt="ignore")
}

GLCM_Homo <- brick(GLCM_Homo)
names(GLCM_Homo) <- SpaAut_Rad$Month
plot(GLCM_Homo$X19.04)

#SEPARATE RASTERBRICK glcm_correlation
GLCM_Corr <- list()

for (i in 1:dim(BUGNTL_masked_brick)[3]){
  GLCM_Corr[i] <- glcm(BUGNTL_masked_brick[[i]], n_grey=32, window=c(3, 3), shift=c(1, 1), statistics=c("correlation"), na_opt="ignore")
}

GLCM_Corr <- brick(GLCM_Corr)
names(GLCM_Corr) <- SpaAut_Rad$Month
plot(GLCM_Corr$X19.04)

endCluster()

#Multimonth NTLC of BUGNTL_Masked in City_ROI_Bug----

#Create empty data.frame to populte
Multimonth_NTLC <- tibble(
  "Month_range"=(1:62),
  "Delta_Mean"=(1:62),
  "Delta_St.dev"=(1:62),
  "Delta_Moran_I"=(1:62)
)

#Populate Month range
list.month2 <- seq(as.Date("2014-01-01"), as.Date("2019-04-30"), by="months")
list.month2 <- list.month2[-54]

fun_mRange <- function(i){
  ymd(list.month2[i]) %--% ymd(list.month2[(i+2)])
}

mRange <- fun_mRange(1:length(list.month2[]))
mRange <- mRange[-63]
mRange[62] <- is.interval("2019-03-01 UTC--2019-0-30 UTC")
Multimonth_NTLC$Month_range <- mRange

#Populate Delta mean
fun_Delta_mean <- function(i){
  SpaAut_Rad$Mean_rad[i+1]-SpaAut_Rad$Mean_rad[i]
}
Delta_mean <- fun_Delta_mean(1:length(SpaAut_Rad$Mean_rad))
Delta_mean <- Delta_mean[-63]
Multimonth_NTLC$Delta_Mean <- Delta_mean

#Populate St_dev
fun_Delta_st.dev <- function(i){
  SpaAut_Rad$St_dev[i+1]-SpaAut_Rad$St_dev[i]
}
Delta_st.dev <- fun_Delta_st.dev(1:length(SpaAut_Rad$St_dev))
Delta_st.dev <- Delta_st.dev[-63]
Multimonth_NTLC$Delta_St.dev <- Delta_st.dev

#Populate Delta_moran
fun_Delta_moranI <- function(i){
  SpaAut_Rad$MoranI[i+1]-SpaAut_Rad$MoranI[i]
}
Delta_moranI <- fun_Delta_moranI(1:length(SpaAut_Rad$MoranI))
Delta_moranI <- Delta_moranI[-63]
Multimonth_NTLC$Delta_Moran_I <- Delta_moranI

setwd("C:/Users/s1526/Dropbox/MB2_IPG(s382722)")
write.csv(SpaAut_Rad, "BUGNTL_1419.csv")
write.csv(Multimonth_NTLC, "Delta_BUGNTL_1419.csv")
BUGNTL_1419 <- read.csv("BUGNTL_1419.csv", header=TRUE, sep=",", dec=".")
Delta_BUGNTL_1419 <- read.csv("Delta_BUGNTL_1419.csv", header=TRUE, sep=",", dec=".")

##########################################
#DATA VISUALISATION AND ADVANCED ANALYSIS#
##########################################

#Assumptions: Bulgaria experienced sustained economic growth since 2014 but a continuous shrinking population
  #Therefore, randomness (urban sprawl) should decrease, while radiance intensity would increase
  #Globally: Moran's I; st.dev (randomness shall decrease), while mean radiation shall increase
#NOT SURE GLCM IS APPRIORIATE FOR NTL, probably not
  #Texturally: GLCM_homogeneity shall decrease, while GLCM collinearity shall increase
#INDEPENDENT = Moran's I ; st.dev | GLCM Correlation
#DEPENDENT = mean_rad | GLCM Homogeneity

#H0: NTL of Bulgaria does not show significant pattern change between 2014 to 2019
#H1: NTL of Bulgaria follows the assumption of decreasing disorder, but brightening

#TEST FOR Correlation and covariance correlation between global variables
cor(BUGNTL_1419$Mean_rad, BUGNTL_1419$MoranI, method="spearman")
#Corr of -0.02 between mean and Moran I suggests little and weak relationship between mean radation and spatial randomness
cor(BUGNTL_1419$Mean_rad,BUGNTL_1419$St_dev, method="spearman")
#Corr of 0.79 suggests a moderate positive relation between mean radiance and st.dev
cor(BUGNTL_1419$St_dev, BUGNTL_1419$MoranI, method="spearman")
#Corr of -0.22

#Let's plot the timesries individually anyway with their DELTA

#Transform BUGNTL_1419 Date character back into lubridate date

list.month3 <- seq(as.Date("2014-01-01"), as.Date("2019-04-30"), by="months")
list.month3 <- list.month3[-54]
BUGNTL_1419$Month <- list.month3

(ts <- ggplot(BUGNTL_1419)+
    geom_line(aes(x=Month, y=Mean_rad, color="Red"))+
    geom_line(aes(x=Month, y=St_dev, color="Green"))+
    geom_line(aes(x=Month, y=MoranI*30, color="Purple"))+
    scale_y_continuous(name="Mean rad & St.dev",
                       sec.axis=sec_axis(~./30, name="Moran's I"))+
    scale_x_date(date_labels="%y %m", date_breaks="4 months")+
    labs(title="Time-series stats of Bulgaria NTL", x="Date", y=" ")+
    theme_economist_white())

#GGPLOT DOESN'T WORK WITH 
(Delta_ts <- ggplot(Delta_BUGNTL_1419)+
    geom_line(aes(x=Month_range, y=Delta_Mean, color="Red"))+
    geom_line(aes(x=Month_range, y=Delta_St.dev, color="Green"))+
    geom_line(aes(x=Month_range, y=Delta_Moran_I, color="Purple"))+
    scale_y_continuous(name="Mean rad & St.dev",
                       sec.axis=sec_axis(~./30, name="Delta Moran's I"))+
    labs(title="Time-series (DELTA) stats of Bulgaria NTL", x="Date range", y=" ")+
    theme_economist_white())

#Timeseries decomposition for each stats
mean_rad_ts <- ts(BUGNTL_1419$Mean_rad, start=2014, end=2019, freq=12)
mean_rad_stl <- stl(mean_rad_ts, s.window="period")
png("Decomp Timeseries of Mean Bulgaria NTL.png")
plot(mean_rad_stl, main="Decomp Timeseries of Mean Bulgaria NTL")
dev.off()

st.dev_ts <- ts(BUGNTL_1419$St_dev, start=2014, end=2019, freq=12)
st.dev_stl <- stl(st.dev_ts, s.window="period")
png("Decomp Timeseries of St.dev Bulgaria NTL.png")
plot(st.dev_stl, main="Decomp Timeseries of St.dev Bulgaria NTL")
dev.off()

moranI_ts <- ts(BUGNTL_1419$MoranI, start=2014, end=2019, freq=12)
moranI_stl <- stl(moranI_ts, s.window="period")
png("Decomp Timeseries of Moran's I Bulgaria NTL.png")
plot(moranI_stl, main="Decomp Timeseries of Moran's I Bulgaria NTL")
dev.off()

#Delta timewseries decomp
Delta_mean_ts <- ts(Delta_BUGNTL_1419$Delta_Mean, start=2014, end=2019, freq=12)
Delta_mean_stl <- stl(Delta_mean_ts, s.window="period")
png("Decomp Timeseries of Delta Mean_rad Bulgaria NTL.png")
plot(Delta_mean_stl, main="Decomp Timeseries of Delta Mean_rad Bulgaria NTL")
dev.off()

Delta_st.dev_ts <- ts(Delta_BUGNTL_1419$Delta_St.dev, start=2014, end=2019, freq=12)
Delta_st.dev_stl <- stl(Delta_st.dev_ts, s.window="period")
png("Decomp Timeseries of Delta st.dev Bulgaria NTL.png")
plot(Delta_st.dev_stl, main="Decomp Timeseries of Delta st.dev Bulgaria NTL")
dev.off()

Delta_moranI_ts <- ts(Delta_BUGNTL_1419$Delta_Moran_I, start=2014, end=2019, freq=12)
Delta_moranI_stl <- stl(Delta_moranI_ts, s.window="period")
png("Decomp Timeseries of Delta Moran's I Bulgaria NTL.png")
plot(Delta_moranI_stl, main="Decomp Timeseries of Delta Moran's I Bulgaria NTL")
dev.off()

#ALL CORRELATION TESTING SUGGESTS NO SIGNIFICANT RELATIONSHIP BUT PERHAPS MEAN_RAD AND ST_DEV
#WHICH SUGGESTS THAT THERES NO INDICATION IN 1st ORDER DERIVED STATS
#IF HYPOTHESIS BASED ON 1st ORDER STAT, ACCEPT NULL HYPOTHESIS

#Although no correlation, when examine individually, Mean radiation and St.dev grew steadily generally
#Moran's I grew till 2017 then into a steep drop off

#LET'S TRY THE GLCM, as homogeneity increases, so should correlation

Corr_GLCM <- corLocal(GLCM_Homo, GLCM_Corr, ngb=3, method="spearman", test=TRUE)
png("Spearman correlation between GLCM Homogeneity and correlation")
plot(Corr_GLCM)
dev.off()
