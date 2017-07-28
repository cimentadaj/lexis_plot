# Functions

# Probably it is possible to combine these two functions in one, as they are identical only that 
# in the first with the "if" we manipulate x and with "else" y, while the opposite is the case for the second
# function.

# First polygon
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

# Cohorts and ages
coh <- c(1750:2020)
ages <- c(0:100)


# Loop over cohorts and ages
n_coh <- length(coh)
n_ages <- length(ages)

# shrink <- 1 no shrinking
# shrink <- 0 just  line
# Defined this as a vector for each cohort
shrink <- seq(0.9,0,by=-0.008)


#tiff(file=paste("Shrink_test.tif"),width = 9600, height = 4800, res=300, 
#     compression="lzw")
plot(x = c(coh[1], coh[length(coh)]), y = c(ages[1], ages[length(ages)]),col="transparent",xlab="Year",ylab="Age")

# Loop for cohorts 
for (i in 1:n_coh) {
    # In order to fixate point 2 which we are 
    # are not shrinking
    mid_x <- seq(coh[i],coh[i]+n_ages,1)
    mid_y <- c(0:c(n_ages-1))

    # Loop for ages
    for (j in 1:n_ages) {
        # Lower Lexis triangle
        x <- c(mid_x[j],mid_x[j]+1, mid_x[j]+1, mid_x[j]+1)
        y <- c(mid_y[j], mid_y[j], mid_y[j],mid_y[j]+1)

        x_sh <- shrink_fun(x, shrink[j])
        y_sh <- shrink_fun(y, shrink[j], x_value = F)

        polygon(x_sh, y_sh, lty=0,col="red")
        
        # Upper Lexis triangle year + 1
        x_inv <- c(x[2], x[2] , x[2] ,x[2] + (x[2] - x[1]))
        y_inv <- c(y[1],y[4],y[4],y[4])

        x_inv_sh <- shrink_fun1(x_inv, shrink[j],y_value = F)
        y_inv_sh <- shrink_fun1(y_inv, shrink[j])

        polygon(x_inv_sh, y_inv_sh,lty=0,col="red")
    }
}
#dev.off()
