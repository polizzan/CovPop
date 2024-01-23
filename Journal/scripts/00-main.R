#############
# MAIN FILE #
#############

# ---

#####################
# CLEAR ENVIRONMENT #
#####################

rm(list=ls())

########################
# PACKAGE INSTALLATION #
########################

## packages to be installed from cran
from.cran <- c("devtools", "gifski", "geofacet", 
               "ggpubr", "gt", "here", "HMDHFDplus",
               "openxlsx", "plyr", "RColorBrewer", 
               "svglite", "tidyverse", "webshot2", "zoo")

## packages to be installed from github, paths listed separately
from.git <- c("DemoTools")
from.git.path <- c("timriffe/DemoTools")

## check if installed, else install
for(i in c(from.cran, from.git)){
  
  ## cran packages
  if(i %in% from.cran){
    
    if(system.file(package=i)==""){install.packages(i)}
    
  }
  
  ## github packages
  if(i %in% from.git){
    
    if(system.file(package=i)==""){
      
      if(i=="DemoTools"){
        
        install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
        
      }
      
      devtools::install_github(from.git.path[which(from.git==i)])
      
    }
  }
}

## load frequently used or complicated to use packages
library(gt)
library(tidyverse)

############
# SET PATH #
############

here::i_am("scripts/00-main.R")

###############
# USER INPUTS #
###############

## number of simulations
nsim <- 1000

## random number seed
set.seed(110320) ## 11 March 2020 = day World Health Organization declared COVID-19 a pandemic

## year combinations for which outputs should be created
estimates_years <-
  rbind(c(2025, 2040, 2060), ## one combination per row, maximum year should be below 2060
        c(2025, 2035, 2040))

## color scales
pal_f <- RColorBrewer::brewer.pal(10, "PRGn")[c(3, 1)] ## lighter shade first
pal_m <- RColorBrewer::brewer.pal(10, "PRGn")[c(8, 10)]

################
# FIXED INPUTS #
################

## list of scenarios and corresponding identifiers
scenarios <- 
  rbind(  
    c(1, "MO--"),
    c(2, "-FE-"),    
    c(3, "--MI"),
    c(4, "MOFEMI"),    
    
    c(5, "MOFE-"),
    c(6, "MO-MI"),
    c(7, "-FEMI"),
    
    c(98, "Baseline Census"),
    c(99, "Baseline WPP")
  ) 

## set up grid for some plots
grid <-
  data.frame(code = c("All", "Mortality", "Fertility", "Migration"),
             name = c("All", "Mortality", "Fertility", "Migration"),
             row = c(1, 2, 2, 2),
             col = c(2, 1, 2, 3))

###########
# FOLDERS #
###########

## check if necessary paths for output exist, else create

## paths for gifs: run loop five times to make sure all folders are created
for(l in scenarios[!scenarios[, 1] %in% c(98, 99), 2]){
  for(k in c("with year", "without year")){  
    for(j in c("absolute", "percent")){
      for(i in 1:5){ 
        if(!dir.exists(here::here("out", "gif", j, k, l))){
          if(!dir.exists(here::here("out", "gif", j, k))){
            if(!dir.exists(here::here("out", "gif", j))){
              if(!dir.exists(here::here("out", "gif"))){
                if(!dir.exists(here::here("out"))){
                  dir.create(here::here("out"))
                }else{dir.create(here::here("out", "gif"))}  
              }else{dir.create(here::here("out", "gif", j))}
            }else{dir.create(here::here("out", "gif", j, k))}
          }else{dir.create(here::here("out", "gif", j, k, l))}
        }else{}
      }
    }
  }
}

## path for all other output
if(!dir.exists(here::here("out", "other-output"))){dir.create(here::here("out", "other-output"))}

############
# ANALYSIS #
############

## load custom functions
source(here::here("scripts", "99-functions.R"))

## data preparation (execute whenever input data change)
source(here::here("scripts", "01-preparation.R"))

## run projections
source(here::here("scripts", "02-projections.R"))

## create output plots and tables
source(here::here("scripts", "03-out.R"))

#########################################
# COMPARE MORTALITY / FERTILITY SOURCES #
#########################################

## clear environment
rm(list=ls())

source(here::here("scripts", "98-compare-sources-rr1.R"))
source(here::here("scripts", "98-compare-sources-rr2.R"))
