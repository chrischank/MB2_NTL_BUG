######################################################################
#Cluster timeseries analysis of Nightlights in Bulgaria              #
#MB2 Introduction to Programming and Geostatistics (MSc. EAGLE, JMUW)#
#Yan Chak Christopher, Chan (s382722) christopherchank@gmail.com     #
#Date                                                                #
######################################################################

setwd("C:/Users/s1526/Dropbox/MB2_IPG(s382722)/MB2_NTL_BUG")

#Libraries----
library(RStoolbox)
library(sp)
library(viridis)
library(rasterVis)
library(tidyverse)

#Ingest data----

#Create paths
path_2014 <- "D:/MB2_DATA/201401_201412"
path_2015 <- "D:/MB2_DATA/201501_201512"
path_2016 <- "D:/MB2_DATA/201601_201612"
path_2017 <- "D:/MB2_DATA/201701_201712"
path_2018 <- "D:/MB2_DATA/201801_201812(NA06)"
path_2019 <- "D:/MB2_DATA/201901_201904"

#STACK & GREP vcmslcfg avg.rade9h
avg_rade_2014 <- grep("*avg.rade9h", list.files(path=path_2014, pattern="*avg.rade9h.tif$"), value=TRUE)

