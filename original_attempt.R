################################################################################
#                                                                              #
# Enhanced Lexis Diagram                                                       #
# Jorge Cimentada, Pompea Fabra University, Sebastian Kluesener, MPIDR         #
#                                                                              #
################################################################################

# Erase all objects in workspace
#rm(list=ls(all=TRUE))

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
library(viridis)
library(classInt)
# skl2: additional library color ramp
library(colorRamps)


################################################################################
#                                                                              #
# 1) Import and prepare data                                                   #
#                                                                              #
################################################################################

# Load population data

# Access information to HMD account
id <- read_lines("id.txt")

# skl2: Read file with information on country names and demonyms
hmd_cou <- read.table("HMD_countries.csv",sep=",",head=T,stringsAsFactor=F)

# skl2: Choose variables of interest are: (1) Cohort mortality rate, 
#                                   (2) Gender differences in 
#                                       cohort mortality rates
#                                   (3) First-order differences 
#                                       in cohort mortality rates 
var_of_int <- 1

# Choose country
country <- "Sweden"

name_cou <- hmd_cou$IDs[hmd_cou$Name==country]

# Choose Male (1) of Female (2)
ch <- 2

# Choose standardization for the line width
# skl2: no_stand: If no_stand is equal to true, the whole 
#                 Lexis surface is shown
# selected_cohort: standardize by cohort
# selected_year: standardize by year
# If no_stand==T and selected_cohort and selected_year==NA,
# then each cohort is standardized by itself
# skl: This should be reprogrammed so that the user has four choices

selected_cohort <- NA
selected_year <- NA
no_stand <- TRUE

# skl2: Color can be 'black' or 'grey90'
bgcol <- "grey90"
if (bgcol=="black") {
  backgr_color <- "black"
} else {
  backgr_color <- "grey90"
}

# Load HMD data
pop <-
  readHMDweb(CNTRY = name_cou,
             item = "Population",
             username = id[1],
             password = id[2]) %>%
  select(Year, Age, Female1, Male1, Total1) %>%
  rename(Female = Female1,
         Male = Male1,
         Total = Total1) %>%
  arrange(Year, Age)

cmx <-
  readHMDweb(CNTRY = name_cou,
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

# If standardized by cohort or year, the upper left triangle with 
# non-completed cohorts will be shown
# If the cohorts are standardized by the biggest size ever recorded, 
# the upper left triangle will not be shown
if (!is.na(selected_cohort) | !is.na(selected_year | no_stand==T)) {
  pop_ch <- filter(pop_long, Cohort <= time2, Sex == choose[ch])
  cmx <- filter(cmx, Year <= time2)
} else {
  pop_ch <- filter(pop_long,
                   Cohort >= time1, Cohort <= time2, Sex == choose[ch])
  cmx <- filter(cmx, Year >= time1, Year <= time2)
}

# Derive the maximum size that was ever recorded for a cohort
# Split by cohort
bycoh <- split(pop_ch$Pop,pop_ch$Cohort)

# Derive maximum
maxcoh <- unlist(lapply(bycoh,max))

# Match maximum size information to cohort
o <- match(pop_ch$Cohort,names(maxcoh))
pop_ch$Maxpop <- maxcoh[o]

# Warning messages
if (!is.na(selected_cohort)&!is.na(selected_year)) print("Please do not choose both a cohort and a year")
if (!is.na(selected_cohort)&!is.na(selected_year)&no_stand==T) print("Please do not use more than one method")

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

# If any value above 0.95, rescale to 0.95
if (max(pop_ch$relative_pop)>0.95) {
  print("Lines have been rescaled to avoid overlapping lines")
  pop_ch$relative_pop <- pop_ch$relative_pop/(max(pop_ch$relative_pop)/0.95)
}

# skl2 <- Set all to one, in case we want the surface
if (no_stand==T) {
  pop_ch$relative_pop <- 1
}

# skl2: Adjusted viridis function: the line where cols are defined allows now to use any bins and not
# only those of an equally spaced categorisation
magmaadjust <- function (n, alpha = 1, bins, option = "magma") {
  option <- switch(option,
                   A = "A",
                   magma = "A",
                   B = "B",
                   inferno = "B", 
                   C = "C",
                   plasma = "C",
                   D = "D",
                   viridis = "D",
                   {
                     warning(paste0("Option '", option, "' does not exist. Defaulting to 'viridis'."))
                     "D"
                   })
  map <- viridisLite::viridis.map[viridisLite::viridis.map$opt == option, ]
  map_cols <- grDevices::rgb(map$R, map$G, map$B)
  fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", 
                                  interpolate = "spline")
  cols <- fn_cols(bins)/255
  grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha)
}

# Match pop data to cmx data
# Turn age in cmx into a numeric variable
cmx$Age <- as.numeric(as.character(cmx$Age))
cmx$Age[is.na(cmx$Age)] <- 110

matchvecmx <- paste(c(cmx$Year+cmx$Age)+1,cmx$Age)
matchvecpop <- paste(pop_ch$Year,pop_ch$Age)

o1 <- match(matchvecpop,matchvecmx)
csex <- which(colnames(cmx)==choose[ch])

# skl2: Cohort mortality rates (magma colors)
if (var_of_int==1) {
  pop_ch$mx <- cmx[,choose[ch]][o1]
  
  pop_ch <- filter(pop_ch, mx <= 1, mx != 0) # because log(0) is infinity
  
  # The colbins to extract colors from the magmafunction are derived
  # using a beta distribution, providing us high flexibility to cut colors
  # from the magmacolor scheme
  colbins <- pbeta(seq(0,0.95,((0.95-0)/100)),4.4,2.6)
  plot(colbins)
  
  colpal <- magmaadjust(100,bins=colbins)
  
  # Assigns color according to fixed breaks categorization 
  # skl1: Hier I am taking the bins by equal interval from the log scale, and exponentiate them
  bins <- exp(c(-100,seq(-9.9,0,0.1)))
  
  catg <- classIntervals(pop_ch$mx, fixedBreaks=bins,
                         style = "fixed")
  color <- findColours(catg, colpal)
  pop_ch$color <- color
}

# skl2: Gender differences in cohort mortality rates
if (var_of_int==2) {
  pop_ch$mx1 <- cmx[,choose[ch]][o1]
  pop_ch$mx2 <- cmx[,choose[which(choose!=choose[ch])]][o1]
  pop_ch$gendif <- pop_ch$mx1/pop_ch$mx2*100
  pop_ch$gendif[pop_ch$gendif==Inf] <- NA
  red <- brewer.pal(9,"Reds")[9]
  green <- brewer.pal(9,"Greens")[9]
  if (backgr_color=="black") {
    colramp <- colorRampPalette(c(green,"white",red),bias=1,space="rgb",interpolate="linear",alpha=F)
  } else{
    colramp <- colorRampPalette(c(green,"grey60",red),bias=1,space="rgb",interpolate="linear",alpha=F)     
  }
  colpal <- colramp(300)
  bins <- c(seq(-50,249,1),max(pop_ch$gendif,na.rm=T))
  catg <- classIntervals(pop_ch$gendif, fixedBreaks=bins,
                         style = "fixed")
  pop_ch$color <- findColours(catg, colpal)
}   

# skl2: First order differences (this part of the code seems,
# to work now, but then the colors are somehow not assigned to the 
# right cohorts, e.g. 1751 should no longer be visible as we do not
# have a cohort 1750 to compare to, but it is still visible)
if (var_of_int==3) {
  mincoh <- min(cmx$Year)
  maxcoh <- max(cmx$Year)
  rangecoh <- mincoh:maxcoh
  # Length, if we take first order differences
  l_fod <- length(rangecoh)-1
  #l_fod_ma3 <- length(rangecoh)-3
  
  #  Direct first order change - Example for females
  reslist <- list()
  for (i in 1:l_fod) {
    cmx_tm1 <- log(cmx[cmx$Year==rangecoh[i],]) 
    cmx_t <- log(cmx[cmx$Year==rangecoh[i+1],]) 
    fod <- cmx_t[[csex]]-cmx_tm1[[csex]]
    fod[fod==Inf] <- NA
    reslist[[i]] <- data.frame(Year = cmx[cmx$Year==rangecoh[i+1], "Year"],
                               Age = cmx[cmx$Year==rangecoh[i+1], "Age"])
    reslist[[i]][colnames(cmx)[csex]] <- fod
  }
  cmx_new <- bind_rows(reslist)
  mat1 <- paste(pop_ch$Cohort,pop_ch$Age)
  mat2 <- paste(cmx_new$Year,cmx_new$Age)
  o <- match(mat1,mat2)
  pop_ch$change <- cmx_new[,3][o]
  pop_ch$change[pop_ch$change==-Inf] <- NA
  red <- brewer.pal(9,"Reds")[9]
  green <- brewer.pal(9,"Greens")[9]
  if (backgr_color=="black") {
    colramp <- colorRampPalette(c(green,"white",red),bias=1,space="rgb",interpolate="linear",alpha=F)
  } else{
    colramp <- colorRampPalette(c(green,"grey60",red),bias=1,space="rgb",interpolate="linear",alpha=F)     
  }
  colpal <- colramp(200)
  bins <- c(min(pop_ch$change,na.rm=T),
            seq(-0.495,0.495,0.005),
            max(pop_ch$change,na.rm=T))
  catg <- classIntervals(pop_ch$change, fixedBreaks=bins,
                         style = "fixed")
  pop_ch$color <- findColours(catg, colpal)
}

color_matrix <-
  complete(pop_ch, Cohort, Age, fill = list(Pop = NA, Maxpop = NA, mx = NA, color = NA, relative_pop = NA)) %>%
  select(Cohort, Age, color) %>%
  spread(Age, color) %>%
  as.matrix()

width_matrix <-
  complete(pop_ch, Cohort, Age, fill = list(Pop = NA, Maxpop = NA, mx = NA, color = NA, relative_pop = NA)) %>%
  select(Cohort, Age, relative_pop) %>%
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
  } else {
    xman <- x
    
    xman[3] <- mean(x[3:4])-(shrink/2)
    xman[4] <- mean(x[3:4])+(shrink/2)
  }
  xman
}

#svg(file="HMD_SWE_MALES5_by_itself.svg",width = 15, height = 7)
#pdf(file="HMD_SWE_MALES5_by_itself.pdf",width = 15, height = 7)
#,family="Californian FB")
# png(file=paste("170115_HMD_SWE_",export[ch],"check1.png",sep=""),
#     #family="Californian FB",
#     width = 5000, height = 3000, res=300)

if (backgr_color == "black") {
  axis_color <- alpha("grey95",0.75)
} else {
  axis_color <- "grey30"
}

par(bg = backgr_color, mar=c(12, 4, 4, 2),fig=c(0,1,0,1))
ages <- c(0, 100)

plot(x = c(time1, time2),
     y = ages,
     pch=20,
     col="transparent",
     col.axis=axis_color,
     font.lab=2,
     cex.lab=1.2,
     ylab="Age",
     xlab="Year",
     col.lab=axis_color,
     xaxt = "n")

axis(1, at = seq(time1, time2, 30), xlab = "Year", col.axis = axis_color)

if (var_of_int==1) {
  title(main=paste(title[ch]," in ", name_cou, " - Cohort Mortality Rates",sep=""),
        col.main=axis_color)
}
if (var_of_int==2) {
  title(main=paste(title[ch]," in ", name_cou, 
                   " - Cohort Mortality Rates in Comparison to Opposite Sex (",title[choose!=choose[ch]],"=100)",sep=""),
        col.main=axis_color)
}
if (var_of_int==3) {
  title(main=paste(title[ch]," in ", name_cou, " - Cohort Mortality Rates (log-scaled absolute difference compared to preceding year)",sep=""),
        col.main=axis_color)
}
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
    # The bottom poligons were not matching 0 
    # in the Y axis. This shrinkage makes sure that it does.
    match_zero <- 0.20
    
    y_sh <- shrink_fun(y, width_matrix[i, j], x_value = F)
    y_sh <- y_sh + match_zero
    
    # skl2: deactivated the grey polygons as they blur the svg outputs    
    #polygon(x_sh, y_sh, lty=0,col=adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
    polygon(x_sh, y_sh, lty=0,col=color_matrix[i, j], border = color_matrix[i, j])
    
    # Upper Lexis triangle year + 1
    x_inv <- c(x[2], x[2] , x[2] ,x[2] + (x[2] - x[1]))
    y_inv <- c(y[1],y[4],y[4],y[4])
    
    x_inv_sh <- shrink_fun(x_inv, width_matrix[i, j], x_value = F)
    
    y_inv_sh <- shrink_fun(y_inv, width_matrix[i, j])
    y_inv_sh <- y_inv_sh + match_zero
    
    # skl2: deactivated the grey polygons as they blur the svg outputs    
    #polygon(x_inv_sh, y_inv_sh, lty=0, col=adjustcolor("grey",alpha.f=0.5), border = adjustcolor("grey",alpha.f=0.5))
    polygon(x_inv_sh, y_inv_sh, lty=0, col=color_matrix[i, j], border = color_matrix[i, j])
  }
}

r_age <- range(ages)
abline(h=c(seq(ages[1],ages[2],10)),col=axis_color,lty=2)
abline(v=c(seq(time1,time2,10)),col=axis_color,lty=2)

# skl2 Here I plot the legend and density curve on the log scale. 
# Had problem with the lower tail as I was not able to use 0 as the lower limit. 
# Had to improvise manually by using 0.0001 and pretending this to be to 0 when
# defining the label. Not optimal, but at least it seems to work now.
if (var_of_int==1) {
  op1 <- par(mar=c(0,0,0,0), fig=c(0.585,0.7,0.035,0.09), new = TRUE)
  # mtext("Cohort death rates",side=1,line=2,col=axis_color)
  plot(c(0,1),c(0,1),col="transparent",axes=F, xlab="", ylab="")
  # text(0.5,0.5,"Cohort mortality rates (cmx)",col=axis_color)
  op2 <- par(mar=c(1,0,0,0), fig=c(0.7,0.9,0.05,0.175), new = TRUE)
  ymax <- max(density(pop_ch$mx,na.rm = TRUE)$y)
  plot(c(0.0001,1),c(0,ymax),col="transparent",axes=F, xlab="", ylab="",log="x")
  lbi <- length(bins)-1
  lines(density(pop_ch$mx,na.rm = TRUE), xlab="", ylab="", lwd=2, main="")
  # skl: Frist 99 polygons are slightly overlapping
  for (j in 1:(length(bins)-2)){ 
    polygon(c(bins[j],bins[j+1]+0.05,bins[j+1]+0.05,bins[j]),
            c(0,0,ymax,ymax), col=colpal[j],border=NA)
  }
  # skl: last one not.
  for (j in (length(bins)-1)){ 
    polygon(c(bins[j],bins[j+1],bins[j+1],bins[j]),
            c(0,0,ymax,ymax), col=colpal[j],border=NA)
  }
  # box around
  polygon(c(0.0000698,bins[101],bins[101],0.0000698),
          c(0,0,ymax,ymax), col=NA,border=axis_color)
  
  ab <- c(0.001,0.005,0.020,0.1,1)
  abline(v=c(ab),col=axis_color)
  axis(1,at=c(0.00007,ab),
       labels=c(0,round(ab*1000,0)),
       col=axis_color,col.ticks = axis_color,col.axis=axis_color)
  lines(density(pop_ch$mx,na.rm = TRUE), col="grey5",lwd=2)
  lines(density(pop_ch$mx,na.rm = TRUE), col="grey95",lwd=1)
}

# skl2: Legend with density curve for the gender differences
if (var_of_int==2) {
  op2 <- par(mar=c(1,0,0,0), fig=c(0.7,0.9,0.05,0.175), new = TRUE)
  ymax <- max(density(pop_ch$gendif,na.rm = TRUE)$y)
  plot(c(0,300),c(0,ymax),col="transparent",axes=F, xlab="", ylab="")
  lbi <- length(bins)-1
  lines(density(pop_ch$gendif,na.rm = TRUE), xlab="", ylab="", lwd=2, main="")
  bins1 <- bins[bins>-1]
  polygon(c(bins1[1],bins1[301],bins[301],bins[1]),
          c(0,0,ymax,ymax), col="white")
  # skl: Frist 99 polygons are slightly overlapping
  for (j in 1:(length(bins1)-2)){ 
    polygon(c(bins1[j],bins1[j+1]+1,bins1[j+1]+1,bins1[j]),
            c(0,0,ymax,ymax), col=colpal[which(bins>-1)][j],border=NA)
  }
  # skl: last one not.
  for (j in (length(bins1)-1)){ 
    polygon(c(bins1[j],bins1[j+1],bins1[j+1],bins1[j]),
            c(0,0,ymax,ymax), col=colpal[which(bins>-1)][j],border=NA)
  }
  # box around
  polygon(c(bins1[1],bins1[301],bins[301],bins1[1]),
          c(0,0,ymax,ymax), col=NA,border=axis_color)
  
  ab <- c(0,50,100,150,200,250,300)
  abline(v=c(ab),col=axis_color)
  axis(1,at=c(ab),
       labels=c(ab),
       col=axis_color,col.ticks = axis_color,col.axis=axis_color)
  lines(density(pop_ch$gendif,na.rm = TRUE), col="grey5",lwd=2)
  lines(density(pop_ch$gendif,na.rm = TRUE), col="grey95",lwd=1)
}  

# Legend with density curve for the first order differences
if (var_of_int==3) {
  op2 <- par(mar=c(1,0,0,0), fig=c(0.7,0.9,0.05,0.175), new = TRUE)
  ymax <- max(density(pop_ch$change,na.rm = TRUE)$y)
  plot(c(-0.5,0.5),c(0,ymax),col="transparent",axes=F, xlab="", ylab="")
  lbi <- length(bins)-1
  lines(density(pop_ch$change,na.rm = TRUE), xlab="", ylab="", lwd=2, main="")
  polygon(c(bins[1],bins[201],bins[201],bins[1]),
          c(0,0,ymax,ymax), col="white")
  # skl: Frist 99 polygons are slightly overlapping
  for (j in 1:(length(bins)-2)){ 
    polygon(c(bins[j],bins[j+1]+0.1,bins[j+1]+0.1,bins[j]),
            c(0,0,ymax,ymax), col=colpal[j],border=NA)
  }
  # skl: last one not.
  for (j in (length(bins)-1)){ 
    polygon(c(bins[j],bins[j+1],bins[j+1],bins[j]),
            c(0,0,ymax,ymax), col=colpal[j],border=NA)
  }
  # box around
  polygon(c(bins[1],bins[201],bins[201],bins[1]),
          c(0,0,ymax,ymax), col=NA,border=axis_color)
  
  ab <- c(-0.5,-0.25,0,0.25,0.5)
  abline(v=c(ab),col=axis_color)
  axis(1,at=c(ab),
       labels=c(ab),
       col=axis_color,col.ticks = axis_color,col.axis=axis_color)
  lines(density(pop_ch$change,na.rm = TRUE), col="grey5",lwd=2)
  lines(density(pop_ch$change,na.rm = TRUE), col="grey95",lwd=1)
}  

# dev.off()

