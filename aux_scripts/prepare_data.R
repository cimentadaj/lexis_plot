
# TR: replaced w tidy expression
pop_long <- gather(
  pop, 
  key = "Sex", 
  value = "Pop", 
  Female:Total) %>% 
  # subset to ages <= 100
  filter(Age <= 100) %>% 
  # Derive cohort data (Population on January 1st)
  mutate(Cohort = Year - Age - 1,
         # replace 0s with positive values
         Pop = if_else(Pop == 0, 1e-10, Pop)
  )

# ---------------------------------------------
# TR: this is also not an easy read, IMO we should have a single
# data object, where cmx is also long and in the same form as pop_long.
# but I didn't have time to make downstream usage robust in this sitting.
# will look at that tomorrow.
# ---------------------------------------------

# TR !! (rlang) unquotes so we can use variables
# used together with :=
# cmx <-  mutate(cmx,
#         !!gender := replace(!!gender, !!gender == 0, 1e-10 ) )
# # Choose data from time1 to time2
# time1 <- min(c(min(pop_long$Year),1920L))
# time2 <- max(pop_long$Year)

cmx[gender] <- if_else(cmx[[gender]] == 0, 0.0000000001, cmx[[gender]])
# Choose data from time1 to time2
time  <- sort(unique(pop_long$Year))
time1 <- if_else(time[1] > 1920, 1920L, time[1])
time2 <- time[length(time)]

# If standardized by cohort or year, the upper left triangle with 
# non-completed cohorts will be shown
# If the cohorts are standardized by the biggest size ever recorded, 
# the upper left triangle will not be shown
# 
# TR: not clear to me why filter down Sex, 
# since maybe we want a ratio or difference? Still haven't seen where ratios
# calculated, but it would make sense to do them in this script too.
if (!is.na(selected_cohort) | !is.na(selected_year | no_stand)) {
  pop_ch <- filter(pop_long, Cohort <= time2, Sex == choose[ch])
  cmx    <- filter(cmx, Year <= time2)
} else {
  pop_ch <- filter(pop_long, Cohort >= time1, Cohort <= time2, Sex == choose[ch])
  cmx    <- filter(cmx, Year >= time1, Year <= time2)
}

pop_ch <- pop_ch %>% 
          group_by(Cohort) %>% 
          mutate(MaxPopCohorts = max(Pop)) %>% 
          ungroup() %>% 
          mutate(relative_pop = 1)

if (!no_stand){
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
  if (is.na(selected_cohort) & is.na(selected_year)) {
    selected_max <- pop_ch$Maxpop
  }

  # The factor by which to standardize cohort lines.
  # TR: this could be a user-specified parameter
  shrink_factor <- 0.9
  pop_ch$relative_pop <- pop_ch$Pop / selected_max * shrink_factor

  # If any value above 0.95, rescale to 0.95
  if (max(pop_ch$relative_pop, na.rm = TRUE) > 0.95) {
    print("Lines have been rescaled to avoid overlapping lines")

    max_relative_pop    <- max(pop_ch$relative_pop, na.rm = TRUE)
    pop_ch$relative_pop <- pop_ch$relative_pop / (max_relative_pop / 0.95)
  }
}

# TR: now to start the smoothing chunk

if (smoothmx){
  # TODO:
  # 1) create Dx column. Need to dynamically rename cols just
  # like for pop, but would need to check how these objects used
  # downstream. In end would be better to have a single object
  # with columns Year, Age, Sex, cMx, Pop, cDx. When smoothing
  # enabled, cMx just gets written over.
}



