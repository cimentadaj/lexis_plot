# Reshape into long format
pop_long <- reshape(pop, varying=names(pop)[c(3:5)],
                    v.names="Pop",times=names(pop)[c(3:5)],
                    timevar="Sex",direction="long")
# Define row names
row.names(pop_long) <- 1:nrow(pop_long)

# Subset dataset so that it only includes ages up to 100
pop_long <- subset(pop_long, pop_long$Age<=100, select= -id)

# Derive cohort data (Population on January 1st)
pop_long$Cohort <- pop_long$Year-pop_long$Age-1

choose <- c("Male","Female")
export <- c("MALES","FEMALES")
title <- c("Males","Females")

# If any age/year cell is empty fill out with a very small number
#to prevent empty polygons in the graph.
pop_long$Pop <- if_else(pop_long$Pop == 0, 0.0000000001, pop_long$Pop)

gender <- choose[ch]
cmx[gender] <- if_else(cmx[[gender]] == 0, 0.0000000001, cmx[[gender]])

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

# The fact by which to standardize cohort lines.
factor <- 0.9

pop_ch$relative_pop <- pop_ch$Pop/selected_max*factor

# If any value above 0.95, rescale to 0.95
if (max(pop_ch$relative_pop, na.rm = TRUE)>0.95) {
  print("Lines have been rescaled to avoid overlapping lines")
  pop_ch$relative_pop <- pop_ch$relative_pop/(max(pop_ch$relative_pop)/0.95)
}

# Set all to one, in case we want the surface
if (no_stand == T) {
  pop_ch$relative_pop <- 1
}