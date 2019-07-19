library(readr)
library(shiny)
library(graphics)
library(grDevices)
library(stats)
library(svglite)
library(classInt)
library(RColorBrewer)
## remotes::install_github("timriffe/TR1/TR1/HMDHFDplus")
library(HMDHFDplus)
library(RCurl)
library(shinyjs)
library(shinycssloaders)
library(shinythemes)
library(xml2)
library(scales)
library(tidyr)
library(dplyr)
library(MortalitySmooth)
library(rlang)
library(here)
# Access information to HMD account
id <- read_lines(here("id.txt"))

# Access country names
hmd_path <- here("data", "HMD_Countries.csv")
hmd_cou <- read_csv(hmd_path)


# All options
ind_options <- c("Cohort mortality rates",
                 "Gender differences in cohort mortality rates (ratio)",
                 "Gender differences in cohort mortality rates (difference)",                 
                 "First order differences in cohort mortality rates")

std_options <- c("Classic Lexis surface",
                 "Standardize relative to cohort",
                 "Standardize relative to year",
                 "Standardize cohort by itself")

color_options <- c("Black", "Grey", "White")
gender_options <- c("Male", "Female")

sexes <- c("Male","Female")
export <- c("MALES","FEMALES")
title <- c("Males","Females")

## Add save plot button
align <- "justify"

# -------------------------------
# TR: make this an option set in server.R
smoothmx <- FALSE
# -------------------------------
