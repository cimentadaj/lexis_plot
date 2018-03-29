################################################################################
#                                                                              #
# Enhanced Lexis Diagram                                                       #
# Jorge Cimentada, Pompea Fabra University, Sebastian Kluesener, MPIDR         #
#                                                                              #
################################################################################

# Erase all objects in workspace
rm(list=ls(all=TRUE))

# Code to install/update libraries on your computer
# install.packages(c("spdep","rgdal","maptools","mapproj",
# "geosphere","classInt","RColorBrewer"))

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
library(tidyverse)



################################################################################
#                                                                              #
# 1) Import and prepare data                                                   #
#                                                                              #
################################################################################

# Load population data

# replace with your human mortaility accounty
id <- read_rds("id")
country <- "JPN"
# For width reference
# If it's set to NA, the
# width is relative to that cohorts
# maximum pop

# skl: Actually my intention was to standardize by cohort.
# But standardizing by year is also
# a good option as this allows to keep the upper left triangle
# Standardize by cohort or by year. If both are set to NA, each
# cohort will be standardized by the biggest size it ever recorded
selected_cohort <- NA
selected_year <- 1960

# Choose Male (1) of Female (2)
ch <- 1


pop <-
  readHMDweb(CNTRY = country,
             item = "Population",
             username = id[1],
             password = id[2]) %>%
  select(Year, Age, Female1, Male1, Total1) %>%
  rename(Female = Female1,
         Male = Male1,
         Total = Total1) %>%
  arrange(Year, Age)

exp <-
  readHMDweb(CNTRY = country,
             item = "cExposures_1x1",
             username = id[1],
             password = id[2]) %>%
  select(-OpenInterval)

cmx <-
  readHMDweb(CNTRY = country,
             item = "cMx_1x1",
             username = id[1],
             password = id[2]) %>%
  select(-OpenInterval)

# Reshape into long format
pop_long <- reshape(pop, varying=names(pop)[c(3:5)],
                    v.names="Pop",times=names(pop)[c(3:5)],
                    timevar="Sex",direction="long")

# Define row names
row.names(pop_long) <- 1:nrow(pop_long)

# Turn age into a numeric variable
pop_long$Age <- as.numeric(as.character(pop_long$Age))
pop_long$Age[is.na(pop_long$Age)] <- 110

# Subset dataset so that it only includes ages up to 100
pop_long <- subset(pop_long, pop_long$Age<=100, select= -id)
head(pop_long)

# Derive cohort data (Population on January 1st)
pop_long$Cohort <- pop_long$Year-pop_long$Age-1

choose <- c("Male","Female")
export <- c("MALES","FEMALES")
title <- c("Males","Females")

# Choose data from time1 to time2
time <- sort(unique(pop_long$Year))
time1 <- if (time[1] > 1920) 1920 else time[1]
time2 <- time[length(time)]

# skl: If standardized by cohort or year, the upper left triangle with non-completed cohorts will be shown
# If the cohorts are standardized by the biggest size ever recorded, they will not be shown
if (!is.na(selected_cohort) | !is.na(selected_year)) {
  pop_ch <-
    pop_long %>%
    filter(Cohort <= time2, Sex == choose[ch])
} else {
  pop_ch <-
    pop_long %>%
    filter(Cohort >= time1, Cohort <= time2, Sex == choose[ch])
}

# Derive the maximum size that was ever recorded for a cohort
# Split by cohort
bycoh <- split(pop_ch$Pop,pop_ch$Cohort)

# Derive maximum
maxcoh <- unlist(lapply(bycoh,max))

# Match maximum size information to cohort
o <- match(pop_ch$Cohort,names(maxcoh))
pop_ch$Maxpop <- maxcoh[o]

# Here I determine the linewidth
# from selected_cohort, which is at the beginning

# skl: Here I changed it to cohort, so that it is standardized by the maximum size recorded for a cohort
if (!is.na(selected_cohort)&!is.na(selected_year)) print("Please do not choose both a cohort and a year")

if (length(pop_ch[pop_ch$Cohort==selected_cohort&pop_ch$Age==0,][,1])==0) {
  print(paste("Please choose a cohort that is observed from birth onwards: ",
              min(pop_ch$Cohort[pop_ch$Age==0]),"-",max(pop_ch$Cohort[pop_ch$Age==0]),sep=""))
}

# Probably one can program this better
# Standardization by reference cohort
if (!is.na(selected_cohort)) {
  selected_max <- max(pop_ch[pop_ch$Cohort == selected_cohort, "Pop"])
} 
# Standardization by reference year
if (!is.na(selected_year)) {
  selected_max <- max(pop_ch[pop_ch$Year == selected_year, "Pop"])
}
# Standardization of each cohort with its maximum size 
if (is.na(selected_cohort)&is.na(selected_year)) {
  selected_max <- pop_ch$Maxpop
}

factor <- 0.9
pop_ch$relative_pop <- pop_ch$Pop/selected_max*factor

# skl: If any value above 0.95, rescale to 0.95
if (max(pop_ch$relative_pop)>0.95) {
  print("Lines have been rescaled to avoid overlapping lines")
  pop_ch$relative_pop <- pop_ch$relative_pop/(max(pop_ch$relative_pop)/0.95)
}

# Match pop data to cmx data
# Turn age in cmx into a numeric variable
cmx$Age <- as.numeric(as.character(cmx$Age))
cmx$Age[is.na(cmx$Age)] <- 110

matchvecmx <- paste(c(cmx$Year+cmx$Age)+1,cmx$Age)
matchvecpop <- paste(pop_ch$Year,pop_ch$Age)

o1 <- match(matchvecpop,matchvecmx)
csex <- which(colnames(cmx)==choose[ch])

pop_ch$mx <- cmx[,csex][o1]

pop_ch <- filter(pop_ch, mx <= 1)

library(viridis)
library(classInt)
colpal <- magma(100, alpha = 1, begin = 0.1, end = 1)
# Choose bins
bins <- c(seq(0,0.05,0.05/50), seq(0.055,0.2,0.145/20),seq(0.2,1,0.8/28)[-1])
# Assigns color according to fixed breaks categorization
catg <- classIntervals(pop_ch$mx, fixedBreaks=bins,
                       style = "fixed")
color <- findColours(catg, colpal)

pop_ch$color <- color

color_matrix <-
  complete(pop_ch, Cohort, Age, fill = list(Pop = NA, Maxpop = NA, mx = NA, color = NA, relative_pop = NA)) %>%
  select(Cohort, Age, color) %>%
  spread(Age, color) %>%
  as.matrix()

width_matrix <-
  complete(pop_ch, Cohort, Age, fill = list(Pop = NA, Maxpop = NA, mx = NA, color = NA, relative_pop = NA)) %>%
  select(Cohort, Age, relative_pop) %>%
  # mutate(relative_pop = rescale(relative_pop, c(0, 2))) %>%
  spread(Age, relative_pop) %>%
  as.matrix()


# Cohorts and ages
coh <- as.numeric(unique(color_matrix[,"Cohort"]))
ages <- as.numeric(attr(color_matrix, "dimnames")[[2]][-1])

# Loop over cohorts and ages
n_coh <- length(coh)
n_ages <- length(ages)

color_matrix <- color_matrix[, -1]
width_matrix <- width_matrix[, -1]

# Functions
# Polygon
shrink_fun <- function(x, shrink, x_value = TRUE) {
  
  if(x_value) {
    xman <- x
    xman[1] <- mean(x[1:2])-(shrink/2)
    xman[2] <- mean(x[1:2])+(shrink/2)
    
    # xman[1] <- mean(x[1:2])-(x[2] - x[1])*(shrink/2)
    # xman[2] <- mean(x[1:2])+(x[2] - x[1])*(shrink/2)
  } else {
    xman <- x
    
    xman[3] <- mean(x[3:4])-(shrink/2)
    xman[4] <- mean(x[3:4])+(shrink/2)
    
    # xman[3] <- mean(x[3:4])-(x[4] - x[3])*(shrink/2)
    # xman[4] <- mean(x[3:4])+(x[4] - x[3])*(shrink/2)
  }
  xman
}

# pdf(file="HMD_SWE_MALES5_by_itself.pdf",width = 10, height = 5)
#,family="Californian FB")
# png(file=paste("170115_HMD_SWE_",export[ch],"check1.png",sep=""),
#     #family="Californian FB",
#     width = 5000, height = 3000, res=300)
#tiff(file=paste("170115_HMD_SWE_",export[ch],"check1.png",sep=""),
#     #family="Californian FB",
#     #width = 5000, height = 3000, res=300,,compression="lzw")
#     width = 2000, height = 1500, res=300,,compression="lzw")
#
#
# tiff(file=paste("170621_HMD_SWE_",export[ch],"with_grey_old.tif",sep=""),
#      family="Californian FB", width = 20000, height = 7200, res=1200,compression="lzw")

par(bg = "black", mar=c(5, 4, 4, 2),fig=c(0,1,0,1))
ages <- c(0, 100)

plot(x = c(time1, time2),
     y = ages,
     pch=20,
     col="transparent",
     col.axis=alpha("grey95",0.75),
     font.lab=2,
     cex.lab=1.2,
     ylab="Age",
     xlab="Year",
     col.lab=alpha("grey95",0.75),
     xaxt = "n")

axis(1, at = seq(time1, time2, 30), xlab = "Year", col.axis = alpha("grey95", 0.75))

title(main=paste(title[ch]," ", country, " - Cohort Mortality Rates",sep=""),
      col.main=alpha("grey95",0.75))

# You sort of fixed the colors but you still need to figure out how to change
# the border color of the polygons and the first line of colors.

# Loop for cohorts
for (i in 1:n_coh) {
  # In order to fixate point 2 which we are
  # are not shrinking
  mid_x <- seq(coh[i],coh[i]+n_ages,1)
  mid_y <- 0:n_ages
  mid_y <- mid_y[-length(mid_y)]
  
  # Loop for ages
  for (j in 1:n_ages) {
    # Lower Lexis triangle
    x <- c(mid_x[j], mid_x[j]+1, mid_x[j]+1, mid_x[j]+1)
    y <- c(mid_y[j], mid_y[j], mid_y[j],mid_y[j]+1)
    
    x_sh <- shrink_fun(x, width_matrix[i, j])
    # subtract <- (x_sh %% 1)[1]
    # x_sh <- x_sh + 1
    # x_sh[1] <- x_sh[1] - subtract
    # x_sh[2] <- x_sh[2] + subtract
    
    # The bottom poligons were not matching 0 
    # in the Y axis. This shrinkage makes sure that it does.
    match_zero <- 0.20
    
    y_sh <- shrink_fun(y, width_matrix[i, j], x_value = F)
    y_sh <- y_sh + match_zero
    
    # x_sh <- c(1751, 1752, 1752, 1752)
    # y_sh <- c(0, 0, 1.975, 1.025)
    
    polygon(x_sh, y_sh, lty=0,col=adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
    polygon(x_sh, y_sh, lty=0,col=color_matrix[i, j], border = color_matrix[i, j])
    
    # Upper Lexis triangle year + 1
    x_inv <- c(x[2], x[2] , x[2] ,x[2] + (x[2] - x[1]))
    y_inv <- c(y[1],y[4],y[4],y[4])
    
    x_inv_sh <- shrink_fun(x_inv, width_matrix[i, j], x_value = F)
    # x_inv_sh[3] <- x_inv_sh[3] - subtract
    # x_inv_sh[4] <- x_inv_sh[4] + subtract
    
    y_inv_sh <- shrink_fun(y_inv, width_matrix[i, j])
    y_inv_sh <- y_inv_sh + match_zero
    
    # x_inv_sh <- c(1752, 1752, 1752, 1753)
    # y_inv_sh <- c(0.975, 1.025, 1, 1)
    
    polygon(x_inv_sh, y_inv_sh, lty=0, col=adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
    polygon(x_inv_sh, y_inv_sh, lty=0, col=color_matrix[i, j], border = color_matrix[i, j])
  }
}
r_age <- range(ages)
abline(h=c(seq(ages[1],ages[2],10)),col=alpha("grey95",0.5),lty=2)
abline(v=c(seq(time1,time2,10)),col=alpha("grey95",0.5),lty=2)
op1 <- par(mar=c(0,0,0,0), fig=c(0.585,0.7,0.035,0.09), new = TRUE)
# mtext("Cohort death rates",side=1,line=2,col=alpha("grey95",0.75))
plot(c(0,1),c(0,1),col="transparent",axes=F, xlab="", ylab="")
#text(0.5,0.5,"Cohort mortality rates (cmx)",col=alpha("grey95",0.75))
op2 <- par(mar=c(0,0,0,0), fig=c(0.7,0.9,0.05,0.075), new = TRUE)
plot(c(0,1),c(0,1),col="transparent",axes=F, xlab="", ylab="")
lbi <- length(bins)-1
for (i in 1:lbi) {
  rect(bins[i],0,bins[i+1],1,lty=0,col=colpal[i])
}
rect(0,0,1,1,lty=1,border=alpha("grey95",0.75))
op3 <- par(mar=c(0,0,0,0), fig=c(0.70,0.71,0.020,0.045), new = TRUE)
plot(c(0,1),c(0,1),col="transparent",axes=F, xlab="", ylab="")
text(0.68,0.5,sprintf("%1.0f",0),col=alpha("grey95",0.75))
op3 <- par(mar=c(0,0,0,0), fig=c(0.885,0.905,0.020,0.045), new = TRUE)
plot(c(0,1),c(0,1),col="transparent",axes=F, xlab="", ylab="")
text(0.35,0.5,sprintf("%1.0f",1),col=alpha("grey95",0.75))

# dev.off()
