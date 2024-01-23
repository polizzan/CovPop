############
#MAIN FILE#
##########

#clear environment
rm(list=ls())

#install and load packages
##install.packages("ggprism")
##install.packages("ggpubr")
##install.packages("gt")
##install.packages("here")
##install.packages("magick")
##install.packages("plyr")
##install.packages("RColorBrewer")
##install.packages("tidyverse")
##install.packages("webshot2")
##install.packages("xlsx")
##install.packages("zoo")

##for installation of 'DemoTools', see: https://timriffe.github.io/DemoTools/

library(ggprism)
library(ggpubr)
library(gt)
library(here)
library(magick)
library(RColorBrewer)
library(tidyverse)
library(DemoTools)
library(webshot2)
library(xlsx)
library(zoo)

#load functions
source(here("scripts", "99-functions.R"))

#data preparation (execute whenever input data change)
source(here("scripts", "01-preparation.R"))

#projections
source(here("scripts", "02-projections.R"))
