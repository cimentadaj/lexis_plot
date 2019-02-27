library(readr)
library(shiny)
library(graphics)
library(grDevices)
library(stats)
library(svglite)
library(classInt)
library(RColorBrewer)
# # install_github("timriffe/TR1/TR1/HMDHFDplus")
library(HMDHFDplus)
library(RCurl)
library(shinyjs)
library(shinycssloaders)
library(shinythemes)
library(scales)
library(tidyr)
library(dplyr)


# Access information to HMD account
id <- read_lines("shiny/id.txt")

# Access country names
hmd_cou <- read_csv("shiny/data/HMD_countries.csv")

# All options
ind_options <- c("Cohort mortality rates",
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