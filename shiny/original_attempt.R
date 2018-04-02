################################################################################
#                                                                              #
# Enhanced Lexis Diagram                                                       #
# Jorge Cimentada, Pompeu Fabra University, Sebastian Kluesener, MPIDR         #
#                                                                              #
################################################################################

# Load libraries
library(lattice)
library(spdep)
library(rgdal)
library(maptools)
library(mapproj)
library(geosphere)
library(classInt)
library(RColorBrewer)
library(classInt)
library(viridis)
library(scales)
library(extrafont)
library(scales)
library(devtools)
# install_github("timriffe/TR1/TR1/HMDHFDplus")
library(HMDHFDplus)
library(MortalitySmooth)
library(ROMIplot)
library(viridis)
library(classInt)
library(colorRamps)
library(tidyverse)

################################################################################
#                                                                              #
# 1) Import and prepare data                                                   #
#                                                                              #
################################################################################

# Load population data

# Access information to HMD account
id <- read_lines("id.txt")

# Read file with information on country names and demonyms
hmd_cou <- read.table("HMD_countries.csv",sep=",",head=T,stringsAsFactor=F)

# Choose variables of interest are: (1) Cohort mortality rate, 
#                                   (2) Gender differences in 
#                                       cohort mortality rates
#                                   (3) First-order differences 
#                                       in cohort mortality rates 
var_of_int <- 2

# Choose country
country <- "Sweden"

name_cou <- hmd_cou$IDs[hmd_cou$Name==country]

# Choose Male (1) of Female (2)
ch <- 1

# Choose standardization for the line width

# 1) no_stand: If no_stand is equal to true, the whole 
#                 Lexis surface is shown
no_stand <- FALSE

# 2) selected_cohort: standardize by cohort
selected_cohort <- 1960

# 3) selected_year: standardize by year
selected_year <- NA

# 4) If no_stand==T and selected_cohort and selected_year==NA,
# then each cohort is standardized by itself

# Warning messages
if (!is.na(selected_cohort)&!is.na(selected_year)) print("Please do not choose both a cohort and a year")
if (!is.na(selected_cohort)&!is.na(selected_year)&no_stand==T) print("Please do not use more than one method")


# Choose colors: can be 'black' or 'grey90'
bgcol <- "black"
if (bgcol=="black") {
  backgr_color <- "black"
} else {
  backgr_color <- "grey90"
}

# Download
download <- FALSE

# Load HMD data
source("load_data.R")

# Prepare data
source("prepare_data.R")

if (length(pop_ch[pop_ch$Cohort==selected_cohort&pop_ch$Age==0,][,1])==0) {
  print(paste("Please choose a cohort that is observed from birth onwards: ",
              min(pop_ch$Cohort[pop_ch$Age==0]),"-",max(pop_ch$Cohort[pop_ch$Age==0]),sep=""))
}

# Define colors
source("define_color_width.R")

# Cohorts and ages
coh <- as.numeric(unique(color_matrix[,"Cohort"]))
ages <- as.numeric(attr(color_matrix, "dimnames")[[2]][-1])

color_matrix <- color_matrix[, -1]
width_matrix <- width_matrix[, -1]

# Loop over cohorts and ages
n_coh <- length(coh)
n_ages <- length(ages)

# Functions
# Polygon
shrink_fun <- function(x, shrink, x_value = TRUE) {
  
  if(x_value) {
    xman <- x
    xman[1] <- mean(x[1:2])-(shrink/2)
    xman[2] <- mean(x[1:2])+(shrink/2)
  } else {
    xman <- x
    
    xman[3] <- mean(x[3:4])-(shrink/2)
    xman[4] <- mean(x[3:4])+(shrink/2)
  }
  xman
}

if (backgr_color == "black") {
  axis_color <- alpha("grey95",0.75)
} else {
  axis_color <- "grey30"
}

source("create_plot.R")
create_plot()

# dev.off()

if (download) {
  dev.copy(png,
           paste(country, "-", export[ch],".png",sep=""),
           width = 5000, height = 3000, res=300)
  dev.off()
}
