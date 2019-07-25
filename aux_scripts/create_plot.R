# Functions
# Polygon
# This is the function to define the width of the line.
# We do so by constructing a a polygon with the width
# according to the population and assign the color
# based on the population. 
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


create_plot <- function(outfile) {
  
  # To save the plot according to the width and height
  # of the user viewing the plot
  width  <- session$clientData$output_graph_width
  height <- session$clientData$output_graph_height
  mysvgwidth <- width/96
  mysvgheight <- height/96

  svglite::svglite(outfile, width = mysvgwidth, height = mysvgheight)
  par(bg = backgr_color, mar=c(12, 4, 4, 2),fig=c(0,1,0,1))
  ages <- c(0, 100)
  
  plot(x = c(time1, time2),
       y = ages,
       pch=20,
       bty="n",
       col="transparent",
       col.axis=axis_color,
       fg=backgr_color,
       font.lab=2,
       cex.lab=1.2,
       ylab="Age",
       xlab="Year",
       col.lab=axis_color,
       xaxt = "n")

  
  axis(1, at = seq(time1, time2, 30), xlab = "Year", col.axis = axis_color,fg=backgr_color)
  
  if (var_of_int==1) {
    title(main=paste(title[ch]," in ", long_cnt_name, " - Cohort Mortality Rates",sep=""),
          col.main=axis_color)
  }
  if (var_of_int==2) {
    title(main=paste(title[ch]," in ", long_cnt_name, 
                     " - Cohort Mortality Rates in Comparison to Opposite Sex (",title[sexes!=sexes[ch]],"=100)",sep=""),
          col.main=axis_color)
  }
  if (var_of_int==3) {
    title(main=paste(title[ch]," in ", long_cnt_name, 
                     " - Cohort Mortality Rates in Comparison to Opposite Sex (difference in deaths per 1000 persons)",sep=""),
          col.main=axis_color)
  }
  if (var_of_int==4) {
    title(main=paste(title[ch]," in ", long_cnt_name, " - Cohort Mortality Rates (log-scaled absolute difference compared to preceding year)",sep=""),
          col.main=axis_color)
  }

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
      
      # Deactivated the grey polygons as they blur the svg outputs    
      polygon(x_sh, y_sh, lty=0,col=color_matrix[i, j], border = color_matrix[i, j])
      
      # Upper Lexis triangle year + 1
      x_inv <- c(x[2], x[2] , x[2] ,x[2] + (x[2] - x[1]))
      y_inv <- c(y[1],y[4],y[4],y[4])
      
      x_inv_sh <- shrink_fun(x_inv, width_matrix[i, j], x_value = F)
      
      y_inv_sh <- shrink_fun(y_inv, width_matrix[i, j])
      y_inv_sh <- y_inv_sh + match_zero
      
      # Deactivated the grey polygons as they blur the svg outputs    
      polygon(x_inv_sh, y_inv_sh, lty=0, col=color_matrix[i, j], border = color_matrix[i, j])
    }
  }
  
  r_age <- range(ages)
  abline(h=c(seq(ages[1],ages[2],10)),col=axis_color,lty=2)
  abline(v=c(seq(time1,time2,10)),col=axis_color,lty=2)
  
  # Here we plot the legend and density curve on the log scale. 
  # Had problem with the lower tail as we were not able to use 0 as the lower limit. 
  # Had to improvise manually by using 0.0001 and pretending this to be to 0 when
  # defining the label. Not optimal, but at least it seems to work now.
  if (var_of_int==1) {
    op1 <- par(mar=c(0,0,0,0), fig=c(0.585,0.7,0.035,0.09), new = TRUE)
    ## mtext("Cohort death rates",side=1,line=2,col=axis_color)
    plot(c(0,1),c(0,1),col="transparent",axes=F, xlab="", ylab="")
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

    # ast one not.
    for (j in (length(bins)-1)){ 
      polygon(c(bins[j],bins[j+1],bins[j+1],bins[j]),
              c(0,0,ymax,ymax), col=colpal[j],border=NA)
    }
    # box around
    polygon(c(0.0000698,bins[101],bins[101],0.0000698),
            c(0,0,ymax,ymax), col=NA,border=axis_color)
    
    ab <- c(0.001,0.005,0.020,0.1,1)
    abline(v=c(ab),col=axis_color)

    axis(1,
         at=c(0.00007,ab),
         labels=c(0,round(ab*1000,0)),
         col=axis_color,
         col.ticks = axis_color,
         col.axis=axis_color)

    mtext(side=1, line=2, "Deaths per 1000", col=axis_color, font=2,cex=1)
    lines(density(pop_ch$mx,na.rm = TRUE), col="grey5",lwd=2)
    lines(density(pop_ch$mx,na.rm = TRUE), col="grey95",lwd=1)
  }
  
  # Legend with density curve for the gender differences
  if (var_of_int==2) {
    op2 <- par(mar=c(1,0,0,0), fig=c(0.7,0.9,0.05,0.175), new = TRUE)
    ymax <- max(density(pop_ch$gendif,na.rm = TRUE)$y)

    plot(c(0,300),c(0,ymax),
         col="transparent",
         axes=FALSE,
         xlab="",
         ylab="",
         col.main = axis_color)
    
    lbi <- length(bins)-1
    lines(density(pop_ch$gendif,na.rm = TRUE), xlab="", ylab="", lwd=2, main="")
    bins1 <- bins[bins>-1]
    # Ensure that bins go from 0 to 300
    if (bins1[1]>0) bins1[1] <- 0 
    if (bins1[length(bins1)]<300) bins1[length(bins1)] <- 300 
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
    label_txt <- paste0("Ratio of deaths per 1000 by ",
                        input$gender, "/", setdiff(gender_options, input$gender))
    mtext(side=1, line=2, label_txt, col=axis_color, font=2,cex=1)
  }

  ## if (var_of_int==3) {
  ##   op1 <- par(mar=c(0,0,0,0), fig=c(0.585,0.7,0.035,0.09), new = TRUE)
  ##   ## mtext("Cohort death rates",side=1,line=2,col=axis_color)
  ##   plot(c(0,1),c(0,1),col="transparent",axes=F, xlab="", ylab="")
  ##   op2 <- par(mar=c(1,0,0,0), fig=c(0.7,0.9,0.05,0.175), new = TRUE)
  ##   ymax <- max(density(pop_ch$gendif,na.rm = TRUE)$y)
  ##   plot(c(0.0001,1),c(0,ymax),col="transparent",axes=F, xlab="", ylab="",log="x")
  ##   lbi <- length(bins)-1
  ##   lines(density(pop_ch$gendif,na.rm = TRUE), xlab="", ylab="", lwd=2, main="")

  ##   # skl: Frist 99 polygons are slightly overlapping
  ##   for (j in 1:(length(bins)-2)){ 
  ##     polygon(c(bins[j],bins[j+1]+0.05,bins[j+1]+0.05,bins[j]),
  ##             c(0,0,ymax,ymax), col=colpal[j],border=NA)
  ##   }

  ##   # ast one not.
  ##   for (j in (length(bins)-1)){ 
  ##     polygon(c(bins[j],bins[j+1],bins[j+1],bins[j]),
  ##             c(0,0,ymax,ymax), col=colpal[j],border=NA)
  ##   }
  ##   # box around
  ##   polygon(c(0.0000698,bins[101],bins[101],0.0000698),
  ##           c(0,0,ymax,ymax), col=NA,border=axis_color)
    
  ##   ab <- c(0.001,0.005,0.020,0.1,1)
  ##   abline(v=c(ab),col=axis_color)

  ##   axis(1,
  ##        at=c(0.00007,ab),
  ##        labels=c(0,round(ab,0)),
  ##        col=axis_color,
  ##        col.ticks = axis_color,
  ##        col.axis=axis_color)

  ##   mtext(side=1, line=2,
  ##         paste0(
  ##           input$gender, "-", setdiff(gender_options, input$gender),
  ##           " difference in deaths per 1000 persons"
  ##         ),
  ##         col=axis_color, font=2,cex=1)
    
  ##   lines(density(pop_ch$gendif,na.rm = TRUE), col="grey5",lwd=2)
  ##   lines(density(pop_ch$gendif,na.rm = TRUE), col="grey95",lwd=1)
  ## }


  
  # Legend with density curve for the first order differences
  if (var_of_int==3) {
    op2 <- par(mar=c(1,0,0,0), fig=c(0.7,0.9,0.05,0.175), new = TRUE)
    ymax <- max(density(pop_ch$change,na.rm = TRUE)$y)
    plot(c(-0.5,0.5),c(0,ymax),col="transparent",axes=F, xlab="", ylab="")
    lbi <- length(bins)-1
    lines(density(pop_ch$change,na.rm = TRUE), xlab="", ylab="", lwd=2, main="")
    # Ensure that bins go from -0.5 to 0.5
    if (bins[1]>-0.5) bins[1] <- -0.5 
    if (bins[length(bins)]<0.5) bins[length(bins)] <- 0.5 
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
    mtext(side=1, line=2, "Percentage change in actual year relative to preceding year",
          col=axis_color, font=2,cex=1)    
    
  }
  
  dev.off()
  
  # Return a list containing the filename
  list(src = normalizePath(outfile),
       contentType = 'image/svg+xml',
       width = width,
       height = height)
}
