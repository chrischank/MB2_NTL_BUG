######################################################################
#Cluster timeseries analysis of Nightlights in Bulgaria              #
#MB2 Introduction to Programming and Geostatistics (MSc. EAGLE, JMUW)#
#Yan Chak Christopher, Chan (s382722) christopherchank@gmail.com     #
#Date                                                                #
######################################################################

setwd("C:/Users/s1526/Dropbox/MB2_IPG(s382722)/MB2_NTL_BUG")

#Libraries----
library(RStoolbox)
library(raster)
library(sp)
library(viridis)
library(rasterVis)
library(tidyverse)

################
#PRE-PROCESSING#
################

#Ingest data----

#Create paths
path_2014 <- "D:/MB2_DATA/201401_201412"
path_2015 <- "D:/MB2_DATA/201501_201512"
path_2016 <- "D:/MB2_DATA/201601_201612"
path_2017 <- "D:/MB2_DATA/201701_201712"
path_2018 <- "D:/MB2_DATA/201801_201812(NA06)"
path_2019 <- "D:/MB2_DATA/201901_201904"

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

avg_rade_2014 <- raster(tif_2014[1:12], layer=tif_2014[1:12])
