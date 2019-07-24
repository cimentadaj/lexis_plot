# TR: replaced w tidy expression
# 1) prepare Population (now exposures)
pop_long <- 
  pop %>%       # some data processing moved from server to here.
  select(Year, Age, Female, Male, Total) %>%
  mutate(Cohort = Year - Age) %>% 
  arrange(Year, Age) %>% 
  gather(key = "Sex", 
         value = "Pop", 
         Female:Total) %>% 
  # subset to ages <= 100
  filter(Age <= 100) %>% 
  # Derive cohort data (Population on  January 1st)
  mutate(Pop = if_else(Pop == 0, 1e-10, Pop))

# 2) prepare cmx same way as pop
cmx <-
  cmx %>% 
  select(-OpenInterval) %>% 
  rename(Cohort = Year) %>%
  mutate(Year = Cohort + Age) %>% 
  arrange(Year, Age) %>% 
  filter(Age <= 100)
 
cmx_long <- 
  cmx %>%
  gather(key = "Sex", 
         value = "cmx", 
         Female:Total) %>% 
  mutate(cmx = ifelse(cmx == 0, 0.0000000001, cmx))
  
# Merge datasets
pop_long <-
  pop_long %>%
  left_join(cmx_long, 
            by = c("Cohort", "Age", "Year", "Sex")) %>% 
  arrange(Year, Sex, Age) %>% 
  mutate(cDx = cmx * Pop) %>% 
  filter(Year <= (max(Cohort) + 30)) %>% 
  group_by(Year) %>% 
  filter(!all(is.na(cmx))) %>% 
  ungroup()

# ---------------------------------------------
# TR: maybe this can be eliminated now:
gender      <- sexes[ch]
cmx[gender] <- if_else(cmx[[gender]] == 0, 0.0000000001, cmx[[gender]])
# Choose data from time1 to time2
time  <- sort(unique(pop_long$Year))
time1 <- if_else(time[1] > 1920, 1920L, time[1])
time2 <- time[length(time)]

# TR: had to move this here
if (input$smoothmx) {

  print("Smoothing applied")
  # TR: now all work happens in pop_long
  # pop_long will contain smoothed cmx if this is run.
  # in order to keep everything happy downstream cmx
  # is then re-created from this object in the same layout.
  # It would however make sense to just do everything with pop_long
  # in the future.
  # ?Mort1Dsmooth
  # chunk <- filter(pop_long, Sex == "Male" & Year == 1921)
  smoothyr <- function(x,y,p,mx,lambda = 1){
    off  <- log(p)
    w    <- ifelse(is.na(y) | y == 0, 0, 1)
    # never smooth infants, too sharp
    w[1:2] <- 0
    suppressWarnings(
      cmxs <- exp(Mort1Dsmooth(x = x, 
                               y = y,
                               offset = off,
                               w = w,
                               method = 3,
                               lambda = lambda)$logmortality)
    )

    ind      <- !is.na(y) 
    ind[1:2] <- FALSE
    mx[ind] <- cmxs[ind]
    mx
  }
  
  ## for (yr in unique(pop_long$Year)){
  ##   cat(yr,"\n")
  ##   chunk <- filter(pop_long, Year == yr & Sex == "Male")
  ##   smoothyr(x = chunk$Age, 
  ##            y =  chunk$cDx, 
  ##            p =  chunk$Pop, 
  ##            mx =  chunk$cmx, 
  ##            lambda = 1)
  ## }

  pop_long <-
    pop_long %>% 
    group_by(Year, Sex) %>% 
    mutate(cmx = smoothyr(x=Age, y = cDx, p = Pop, mx = cmx, lambda = 1)) %>% 
    ungroup() 

  # we smooth within period, not within cohorts:
  # Reason: because shocks are period phenomena we 
  # don't want to remove. This procedure can oversmooth
  # abrupt changes over age, e.g. in war years 
  # where < 18 not exposed and >= 18 have excess.
  
  # remake cmx
  cmx <-
    pop_long %>% 
    select(Cohort, Age, Year, Sex, cmx) %>% 
    spread(Sex, cmx) %>% 
    # re-attach NAs from cmx...
    ## anti_join(cmx, ., by = c("Cohort", "Age")) %>% 
    ## bind_rows(test) %>% 
    arrange(Year, Age) %>% 
    # arrange columns
    select(Cohort, Age, Year, Female, Male, Total) 
} # end smoothing chunk.


# If standardized by cohort or year, the upper left triangle with 
# non-completed cohorts will be shown
# If the cohorts are standardized by the biggest size ever recorded, 
# the upper left triangle will not be shown
# 
# TR: not clear to me why filter down Sex, 
# since maybe we want a ratio or difference? Still haven't seen where ratios
# calculated, but it would make sense to do them in this script too.
# TR: I see you kept Sex in cmx columns to be able to rake ratios.
# https://stackoverflow.com/questions/28487526/calculating-ratios-by-group-with-dplyr
if (!is.na(selected_cohort) | !is.na(selected_year | no_stand)) {
  pop_ch <- filter(pop_long, Cohort <= time2, Sex == sexes[ch])
  cmx    <- filter(cmx, Year <= time2)
} else {
  pop_ch <- filter(pop_long, Cohort >= time1, Cohort <= time2, Sex == sexes[ch])
  cmx    <- filter(cmx, Year >= time1, Year <= time2)
}
 
pop_ch <- pop_ch %>% 
          group_by(Cohort) %>% 
          mutate(MaxPopCohorts = max(Pop)) %>% 
          ungroup() %>% 
          mutate(relative_pop = 1)

if (!no_stand) {
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
    selected_max <- pop_ch$MaxPopCohorts
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

# Match pop data to cmx data
cmx$Age[is.na(cmx$Age)] <- 110

cmx <- as_tibble(cmx)

pop_ch <-
  pop_ch %>%
  left_join(cmx) %>%
  mutate(mx = !!sym(sexes[ch]))

