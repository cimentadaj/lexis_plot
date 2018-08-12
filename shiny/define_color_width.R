# Adjusted viridis function: the line where cols are defined allows now to
# use any bins and not only those of an equally spaced categorisation
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
cmx$Age[is.na(cmx$Age)] <- 110

matchvecmx <- paste(c(cmx$Year+cmx$Age)+1,cmx$Age)
matchvecpop <- paste(pop_ch$Year,pop_ch$Age)

o1 <- match(matchvecpop,matchvecmx)
csex <- which(colnames(cmx)==choose[ch])

# Cohort mortality rates (magma colors)
if (var_of_int==1) {
  pop_ch$mx <- cmx[,choose[ch]][o1]
  
  pop_ch <- filter(pop_ch, mx <= 1, mx != 0) # because log(0) is infinity
  
  # The colbins to extract colors from the magmafunction are derived
  # using a beta distribution, providing us high flexibility to cut colors
  # from the magmacolor scheme
  colbins <- pbeta(seq(0,0.95,((0.95-0)/100)),4.4,2.6)
  
  colpal <- magmaadjust(100,bins=colbins, option = "magma")
  
  # Assigns color according to fixed breaks categorization 
  # skl1: Hier I am taking the bins by equal interval from the log scale, and exponentiate them
  bins <- exp(c(-100,seq(-9.9,0,0.1)))
  
  catg <- classIntervals(pop_ch$mx, fixedBreaks=bins,
                         style = "fixed")
  color <- findColours(catg, colpal)
  
  if (backgr_color == "white") {

    # Here I tried from white to red and to firebrick1 and it doesn't look good at all.
    # I also tried the color blind pallete from ggthemes::show_col(ggthemes::colorblind_pal()(8))

    # I think perhaps cleanest way is to try another version of viridis (which btw, I just
    # read that it is colorblind friendly, see https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html)
    # I'm pretty sure we can find a citation for this vignette or a paper they wrote.
    colramp <- colorRampPalette(c("white", "#CC79A7"),bias=1,space="rgb",interpolate="linear",alpha=F)
    colpal <- colramp(100)
    color <- findColours(catg, colpal)
  }
  
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

# skl2: First order differences
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
