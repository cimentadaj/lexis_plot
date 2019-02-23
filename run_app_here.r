# Load libraries
library(svglite)
library(shiny)
library(shinythemes)
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
library(shinyjs)
library(shinycssloaders)
library(tidyverse)

# Access information to HMD account
id <- read_lines("shiny/id.txt")

# Access country names
hmd_cou <- read_csv("shiny/HMD_countries.csv")

# All options
ind_options <- c("Cohort mortality rate",
                 "Gender differences in cohort mortality rates",
                 "First order differences in cohort mortality rates")

std_options <- c("Classic Lexis surface",
                 "Standardize relative to cohort",
                 "Standardize relative to year",
                 "Standardize cohort by itself")

color_options <- c("Black", "Grey", "White")
gender_options <- c("Male", "Female")

## Add save plot button
align <- "justify"

runApp("shiny/")



