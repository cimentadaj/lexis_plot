################################################################################
#                                                                              #
# Enhanced Lexis Diagram                                                       #
# Sebastian Kl?sener, MPIDR                                                    #
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

id <- read_rds("id")
country <- "SWE"

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

# Choose Male (1) of Female (2)
ch <- 1

# Choose data from time1 to time2
time <- sort(unique(pop_long$Year))
time1 <- time[1]
time2 <- time[length(time)]

pop_ch <-
  pop_long %>%
  filter(Cohort >= time1, Cohort <= time2, Sex == choose[ch])

# Derive the maximum size that was ever recorded for a cohort
# Split by cohort
bycoh <- split(pop_ch$Pop,pop_ch$Cohort)

# Derive maximum
maxcoh <- unlist(lapply(bycoh,max))

# Match maximum size information to cohort
o <- match(pop_ch$Cohort,names(maxcoh))
pop_ch$Maxpop <- maxcoh[o]

# Here I determine the linewidth for the upper lines
# This should be improved
factor <- 3
popstand <- pop_ch$Pop/max(pop_ch$Pop)*factor
lwd_up <- popstand

# Here I determine the linewidth for the lower lines
# This should be improved
popmax <- pop_ch$Maxpop/max(pop_ch$Pop)*factor
lcoh <- length(unique(pop_ch$Cohort))
cohmax <- maxcoh/max(pop_ch$Pop)*factor
lwd_low <- cohmax

# Match pop data to cmx data
# Turn age in cmx into a numeric variable
cmx$Age <- as.numeric(as.character(cmx$Age))
cmx$Age[is.na(cmx$Age)] <- 110

matchvecmx <- paste(c(cmx$Year+cmx$Age)+1,cmx$Age)
matchvecpop <- paste(pop_ch$Year,pop_ch$Age)

o1 <- match(matchvecpop,matchvecmx)
csex <- which(colnames(cmx)==choose[ch])

pop_ch$mx <- cmx[,csex][o1]


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
  complete(pop_ch, Cohort, Age, fill = list(Pop = NA, Maxpop = NA, mx = NA, color = NA)) %>%
  select(Cohort, Age, color) %>%
  spread(Age, color) %>%
  as.matrix()

width_matrix <-
  complete(pop_ch, Cohort, Age, fill = list(Pop = NA, Maxpop = NA, mx = NA, color = NA)) %>%
  select(Cohort, Age, Pop) %>%
  mutate(Pop = rescale(Pop, c(0, 2))) %>%
  spread(Age, Pop) %>%
  as.matrix()


# Cohorts and ages
coh <- as.numeric(unique(color_matrix[,"Cohort"]))
ages <- as.numeric(attr(color_matrix, "dimnames")[[2]][-1])

# Loop over cohorts and ages
n_coh <- length(coh)
n_ages <- length(ages)

# Functions
# Polygon
shrink_fun <- function(x, shrink, x_value = TRUE) {
  
  if(x_value) {
    xman <- x
    xman[1] <- mean(x[1:2])-(x[2] - x[1])*(shrink/2)
    xman[2] <- mean(x[1:2])+(x[2] - x[1])*(shrink/2)
  } else {
    xman <- x
    xman[3] <- mean(x[3:4])-(x[4] - x[3])*(shrink/2)
    xman[4] <- mean(x[3:4])+(x[4] - x[3])*(shrink/2)
  }
  xman
}

# pdf(file="HMD_SWE_MALES4.pdf",width = 20, height = 7,family="Californian FB")
# png(file=paste("170115_HMD_SWE_",export[ch],"check.png",sep=""),
#     family="Californian FB", width = 10000, height = 3600, res=600)
#
# tiff(file=paste("170621_HMD_SWE_",export[ch],"with_grey_old.tif",sep=""),
#      family="Californian FB", width = 20000, height = 7200, res=1200,compression="lzw")

par(bg = "black", mar=c(5, 4, 4, 2),fig=c(0,1,0,1))

plot(x = c(coh[1], coh[length(coh)]),
     y = c(ages[1], ages[length(ages)]),
     pch=20,
     col="transparent", col.axis=alpha("grey95",0.75),
     font.lab=2, cex.lab=1.2, xlab="Year", ylab="Age",
     xlim=c(time1, time2), col.lab=alpha("grey95",0.75))

# title(main=paste("Males"," (Sweden) - Cohort Mortality Rates",sep=""),
#       col.main=alpha("grey95",0.75))

# You sort of fixed the colors but you still need to figure out how to change
# the border color of the polygons and the first line of colors.

# Loop for cohorts
for (i in 1:n_coh) {
  # In order to fixate point 2 which we are
  # are not shrinking
  mid_x <- seq(coh[i],coh[i]+n_ages,1)
  mid_y <- c(0:n_ages-1)
  
  # Loop for ages
  for (j in 2:n_ages) {
    # Lower Lexis triangle
    x <- c(mid_x[j], mid_x[j]+1, mid_x[j]+1, mid_x[j]+1)
    y <- c(mid_y[j], mid_y[j], mid_y[j],mid_y[j]+1)
    
    x_sh <- shrink_fun(x, width_matrix[i, j])
    y_sh <- shrink_fun(y, width_matrix[i, j], x_value = F)
    
    polygon(x_sh, y_sh, lty=0,col=adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
    polygon(x_sh, y_sh, lty=0,col=color_matrix[i, j], border = color_matrix[i, j])
    
    # Upper Lexis triangle year + 1
    x_inv <- c(x[2], x[2] , x[2] ,x[2] + (x[2] - x[1]))
    y_inv <- c(y[1],y[4],y[4],y[4])
    
    x_inv_sh <- shrink_fun(x_inv, width_matrix[i, j], x_value = F)
    y_inv_sh <- shrink_fun(y_inv, width_matrix[i, j])
    
    polygon(x_inv_sh, y_inv_sh, lty=0, col=adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
    polygon(x_inv_sh, y_inv_sh, lty=0, col=color_matrix[i, j], border = color_matrix[i, j])
  }
}

abline(h=c(seq(0,100,10)),col=alpha("grey95",0.5),lty=2)
abline(v=c(seq(1750,2010,10)),col=alpha("grey95",0.5),lty=2)
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

## selected data
country <- "SWE"

population <-
  readHMDweb(CNTRY = country,
             item = "Population",
             username = id[1],
             password = id[2]) %>%
  select(Year, Age, Female1, Male1) %>%
  rename(Female = Female1,
         Male = Male1) %>%
  arrange(Year, Age) %>%
  filter(between(Age, 0, 100), between(Year, 1751, 2011))

death <-
  readHMDweb(CNTRY = country,
             item = "Deaths_1x1",
             username = id[1],
             password = id[2]) %>%
  select(Year, Age, Female, Male, Total) %>%
  arrange(Year, Age)

exp_death <-
  readHMDweb(CNTRY = country,
             item = "Exposures_1x1",
             username = id[1],
             password = id[2]) %>%
  select(-OpenInterval)

death_matrix <-
  death %>%
  select(Year, Age, Female) %>%
  filter(between(Age, 0, 100), between(Year, 1751, 2011)) %>%
  spread(Year, Female) %>%
  as.matrix() %>%
  .[, -1]

rownames(death_matrix) <- 0:100

exp_matrix <-
  exp_death %>%
  select(Year, Age, Female) %>%
  filter(between(Age, 0, 100), between(Year, 1751, 2011)) %>%
  spread(Year, Female) %>%
  as.matrix() %>%
  .[, -1]

rownames(exp_matrix) <- 0:100

ages_unique <- death_matrix %>% row.names %>% as.numeric
years_unique <- death_matrix %>% colnames %>% as.numeric

smooth_calculator <- function(row_unique, col_unique, counts, exposure) {
  
  pre_smoothed <- Mort2Dsmooth(x=row_unique, y=col_unique,
                               Z = counts, offset = log(exposure))
  
  mx <- pre_smoothed$fitted.values/exposure
  mx.2 <- mx[, -1]
  mx.1 <- mx[, -(ncol(mx))]
  aai <- 100 * -log(mx.2/mx.1)
  
  aai
}

smoothed_data <- smooth_calculator(ages_unique, years_unique, death_matrix, exp_matrix)

coh <- years_unique
n_coh <- length(years_unique[-1])
n_ages <- length(ages_unique)

width_matrix <-
  population %>%
  select(-Male) %>%
  mutate(Female = rescale(Female, c(0, 2))) %>%
  spread(Age, Female) %>%
  mutate(Cohort = years_unique) %>%
  select(Cohort, everything(), -Year) %>%
  as.matrix()

stacked_df <-
  smoothed_data %>%
  as_tibble %>%
  gather(year, value) %>%
  mutate(age = rep(ages_unique, length(years_unique[-1])))


library(viridis)
library(classInt)
colpal <- magma(100, alpha = 1, begin = 0.1, end = 1)
# Choose bins
bins <- seq(0, 10)
# Assigns color according to fixed breaks categorization
catg <- classIntervals(stacked_df$value, fixedBreaks=bins, style = "fixed")
color <- findColours(catg, colpal)

color_matrix <-
  stacked_df %>%
  mutate(color = color) %>%
  select(year, age, color) %>%
  spread(age, color) %>%
  as.matrix()

shrink_fun <- function(x, shrink, x_value = TRUE) {
  
  if(x_value) {
    xman <- x
    xman[1] <- mean(x[1:2])-(x[2] - x[1])*(shrink/2)
    xman[2] <- mean(x[1:2])+(x[2] - x[1])*(shrink/2)
  } else {
    xman <- x
    xman[3] <- mean(x[3:4])-(x[4] - x[3])*(shrink/2)
    xman[4] <- mean(x[3:4])+(x[4] - x[3])*(shrink/2)
  }
  xman
}

# The next thing to do is incorporate this smoothe data in the previous plot. This
# shouldn't be that hard given that the data is in the same format (matrix, age x year)
# as in the previous plot.

# Other thing to do: link both smoothed and previous analysis
# to common variables so that both things are estimated from the same
# variables.

par(bg = "black", mar=c(5, 4, 4, 2),fig=c(0,1,0,1))

plot(x = c(years_unique[1], years_unique[length(years_unique)]),
     y = c(ages_unique[1], ages_unique[length(ages_unique)]),
     pch=20,
     col="transparent", col.axis=alpha("grey95",0.75),
     font.lab=2, cex.lab=1.2, xlab="Year", ylab="Age",
     xlim=c(years_unique[1], years_unique[length(years_unique)]), col.lab=alpha("grey95",0.75))

# Loop for cohorts
for (i in 1:n_coh) {
  # In order to fixate point 2 which we are
  # are not shrinking
  mid_x <- seq(coh[i],coh[i]+n_ages,1)
  mid_y <- c(0:n_ages-1)
  
  # Loop for ages
  for (j in 2:n_ages) {
    # Lower Lexis triangle
    x <- c(mid_x[j], mid_x[j]+1, mid_x[j]+1, mid_x[j]+1)
    y <- c(mid_y[j], mid_y[j], mid_y[j],mid_y[j]+1)
    
    x_sh <- shrink_fun(x, width_matrix[i, j])
    y_sh <- shrink_fun(y, width_matrix[i, j], x_value = F)
    
    polygon(x_sh, y_sh, lty=0,col=adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
    polygon(x_sh, y_sh, lty=0,col=color_matrix[i, j], border = color_matrix[i, j])
    
    # Upper Lexis triangle year + 1
    x_inv <- c(x[2], x[2] , x[2] ,x[2] + (x[2] - x[1]))
    y_inv <- c(y[1],y[4],y[4],y[4])
    
    x_inv_sh <- shrink_fun(x_inv, width_matrix[i, j], x_value = F)
    y_inv_sh <- shrink_fun(y_inv, width_matrix[i, j])
    
    polygon(x_inv_sh, y_inv_sh, lty=0, col=adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
    polygon(x_inv_sh, y_inv_sh, lty=0, col=color_matrix[i, j], border = color_matrix[i, j])
  }
}

abline(h=c(seq(0,100,10)),col=alpha("grey95",0.5),lty=2)
abline(v=c(seq(1750,2010,10)),col=alpha("grey95",0.5),lty=2)
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

# Still need to fix legend scale
# Make years automatic because right now you're filtering years manually when reading the dataset.
