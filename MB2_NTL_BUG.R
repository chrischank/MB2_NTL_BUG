######################################################################
#Cluster timeseries analysis of Nightlights in Bulgaria              #
#MB2 Introduction to Programming and Geostatistics (MSc. EAGLE, JMUW)#
#Yan Chak Christopher, Chan (s382722) christopherchank@gmail.com     #
#Date                                                                #
######################################################################

setwd("D:/MB2_DATA")

#Libraries----
library(RStoolbox)
library(raster)
library(sp)
library(shapefiles)
library(viridis)
library(rasterVis)
library(tidyverse)
library(rgdal)
library(spdep)
library(glcm)

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
avg_rade_2014 <- grep("*avg.rade9h", list.files(path=path_2014, pattern="*avg.rade9h.tif$"), value=TRUE)
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
BUG_Masked_2014 <- crop(stack_2014, Bug_MASK)
setwd("D:/MB2_DATA/201401_201412/Masked")
writeRaster(BUG_Masked_2014, filename=names(BUG_Masked_2014), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUG_Masked_2015 <- crop(stack_2015, Bug_MASK)
setwd("D:/MB2_DATA/201501_201512/Masked")
writeRaster(BUG_Masked_2015, filename=names(BUG_Masked_2015), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUG_Masked_2016 <- crop(stack_2016, Bug_MASK)
setwd("D:/MB2_DATA/201601_201612/Masked")
writeRaster(BUG_Masked_2016, filename=names(BUG_Masked_2016), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUG_Masked_2017 <- crop(stack_2017, Bug_MASK)
setwd("D:/MB2_DATA/201701_201712/Masked")
writeRaster(BUG_Masked_2017, filename=names(BUG_Masked_2017), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUG_Masked_2018 <- crop(stack_2018, Bug_MASK)
setwd("D:/MB2_DATA/201801_201812(NA06)/Masked")
writeRaster(BUG_Masked_2018, filename=names(BUG_Masked_2018), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

BUG_Masked_2019 <- crop(stack_2019, Bug_MASK)
setwd("D:/MB2_DATA/201901_201904/Masked")
writeRaster(BUG_Masked_2019, filename=names(BUG_Masked_2019), bylayer=TRUE, format="GTiff", datatype="INT2S", overwrite=TRUE)

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

#KMeans to 2 classes (High lights & Low lights)
ROI_clusters <- unsuperClass(na.omit(sample_201401), nClasses=2, nStarts=5, nIter=500, norm=FALSE, algorithm="Lloyd")
plot(ROI_clusters$map)

NTL_ROI_BUG <- rasterToPolygons(ROI_clusters$map, fun=function(x) (x==2), n=16, na.rm=TRUE, dissolve=TRUE)
compareCRS(BUG_Masked_2014, NTL_ROI_BUG)
plot(NTL_ROI_BUG)
writeOGR(NTL_ROI_BUG,"D:/MB2_DATA", "NTL_ROI_BUG", driver="ESRI Shapefile", overwrite=TRUE)

#Cropping stacks of ONLY NTL for analysis
BUGNTL_masked_2014 <- stack(crop(BUG_Masked_2014, NTL_ROI_BUG))
BUGNTL_masked_2015 <- stack(crop(BUG_Masked_2015, NTL_ROI_BUG))
BUGNTL_masked_2016 <- stack(crop(BUG_Masked_2016, NTL_ROI_BUG))
BUGNTL_masked_2017 <- stack(crop(BUG_Masked_2017, NTL_ROI_BUG))
BUGNTL_masked_2018 <- stack(crop(BUG_Masked_2018, NTL_ROI_BUG))
BUGNTL_masked_2019 <- stack(crop(BUG_Masked_2019, NTL_ROI_BUG))

#Plot comparison between BUG_Masked_2019 & BUGNTL_Maksed_2019
par(mfrow=c(1, 2))
plot(BUG_Masked_2019$SVDNB_npp_20190401.20190430_75N060W_vcmslcfg_v10_c201905191000.avg_rade9h, main="BulgariaNTL_201904", col=viridis_pal(option = "D")(10))
plot(BUGNTL_masked_2019$SVDNB_npp_20190401.20190430_75N060W_vcmslcfg_v10_c201905191000.avg_rade9h, main="BUGNTL_Masked_201904", col=viridis_pal(option = "D")(10))

##########
#ANALYSES#
##########

BUGNTL_masked_list <- list(BUGNTL_masked_2014, BUGNTL_masked_2015, BUGNTL_masked_2016, BUGNTL_masked_2017, BUGNTL_masked_2018, BUGNTL_masked_2019)

#Calculate Moran's I & Homogeneity for month----

#Create empty data.frame to populate

SpaAut_Rad <- tibble(
  "Month"=list(),
  "Mean_rad"=list(),
  "Moran_I"=list(),
  "St.dev"=list(),
)

#Populate Mean_rad
SpaAut_Rad$Mean_rad <- mean(BUGNTL_masked_2014[])

#Populate Moran's I
SpaAut_Tex$Moran_I <- lapply(BUGNTL_masked_2014[], moran(BUGNTL_masked_2014[], ))

#Multidate LCC of BUG_Masked in City_ROI_Bug----

#Create empty data.frame to populte
Multimonth_NTLC <- tibble(
  "Month_range"=list(),
  "Delta_Mean"=list(),
  "Delta_Moran_I"=list(),
  "Delta_Homogeneity"=list()
)

#Calculate Delta_MD (Change in monthly multi-date)

