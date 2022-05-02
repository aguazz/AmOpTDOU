###
OUpath <- function(n, x, slope, pull, vol, time_line) {
  
  require(mvtnorm)
  
  N <- length(t)
  final_time <- t[N]
    
  slope <- Vectorize(slope)
  pull <- Vectorize(pull)
  vol <- Vectorize(vol)
  
  # Compute auxiliary functions
  I1 <- Vectorize(function(t) exp(integrate(slope, lower = 0, upper = t)$value))
  I2 <- Vectorize(function(t) slope(t)*pull(t)*I1(t))
  I3 <- Vectorize(function(t) (vol(t)*I1(t))^2)
  IntegrateVectorizeI3 <- Vectorize(function(t) integrate(I3, lower = 0, upper = t)$value) 
  
  # Define mean
  mu <- sapply(t, function(s){
    (x + integrate(I2, lower = 0, upper = s)$value) / I1(s) 
  }) 
  # Define covariance
  Sigma <- outer(t, t, 
                 function(s, u) {
                   I1(u)^-1 * I1(s)^-1 *
                     IntegrateVectorizeI3(pmin(u, s)) / I1(pmin(u, s)^2)
                 }
  )
  
  # Generate paths
  rmvnorm(n = n, mean = mu, sigma = Sigma)
  
}

####
## - The function "boundary" computes the optimal stopping boundary of the (exponentially) 
## discounted optimal stopping problem with finite horizon and gain function 
## G(x) = (strike - x)^+. The underlying process with parameters is a 
## time-dependent Ornstein-Uhlenbeck given by the SDE
##            dX_t = slope(t)(pull(t) - X_t) + vol(t)dW_t.
## - The boundary is computed by running a Picard iteration algorithm,
## that stops when the L2 distance between consecutive boundaries is less than
## tol, and solves the the free-boundary equation.
## - The boundary is computed at the time points provided in time_line
## - If errors == TRUE, a vector of the errors of the Picard scheme is provided
## alongside the boundary
####
boundary <- function (tol = 1e-3, strike = 0, time_line, discount = 0, 
                      boundary_kernel, slope, pull, vol, errors = FALSE) {
  
  N <- length(time_line)       # Partition length
  expiration <- time_line[N]        # Expiration date
  delta <- time_line[2:N] - time_line[1:(N-1)]  # Length step vector
  
  # Creating errors vector if required
  if (errors) er <- c()
  
  # Pre-allocating boundary
  bnd <- rep(min((slope(expiration) * pull(expiration) + discount * strike) /
               (slope(expiration) + discount), strike), N)
  
  # Auxiliary pre-computations for marginal_mean and marginal_var
  f1 <- function(s) Vectorize(function(t) {
    exp(-integrate(slope, lower = t, upper = s, 
                   subdivisions = 10000, rel.tol = 1e-10)$value)
  })
  I1 <- f1(expiration)(time_line)
  f2 <- function(t) slope(t) * pull(t) * f1(expiration)(t)
  I2 <- sapply(time_line, function(s) {
    integrate(f2, lower = s, upper = expiration, 
              subdivisions = 10000, rel.tol = 1e-10)$value
  })
  f3 <- function(t) (f1(expiration)(t) * vol(t))^2
  I3 <- sapply(time_line, function(s) {
    integrate(f3, lower = s, upper = expiration, 
              subdivisions = 10000, rel.tol = 1e-10)$value
  })
  
  # Kernel definition
  boundary_kernel <- function(c1, c2, i1, x1, i2, x2) {
    
    # Compute the marginal mean
    marginal_mean <- (x1 * I1[i1] + (I2[i1] -  I2[i2])) / I1[i2]
    # Compute the marginal standard deviation
    marginal_sd <- sqrt((I3[i1] - I3[i2]) / I1[i2]^2)
    # Compute standardized values
    x2 <- (x2 - marginal_mean) / marginal_sd
    # Compute normal distribution and density
    normal_dist <- pnorm(x2, mean = 0, sd = 1, lower.tail = T)
    normal_dens <- dnorm(x2, mean = 0, sd = 1)
    # Evaluate Kernel
    K <- exp(-discount * (time_line[i2] - time_line[i1])) * 
      ((c1 - c2 * marginal_mean) * normal_dist + c2 * marginal_sd * normal_dens) 
    
    return(K)
    
  }
  
  # Boundary computation
  e <- 1  # error in the while loop
  # Fixed point algorithm
  while (e > tol) {
    
    bnd_old <- bnd
    
    # 
    for (i in (N - 1):1) {  
      
      # print(paste0("updating boundary at t_", i, " = ", time_line[i]))
      # Evaluate the kernel
      K1 <- boundary_kernel(c1 = strike, c2 = 1, 
                            i1 = i, x1 = bnd_old[i], 
                            i2 = N, x2 = strike)
      K2 <- boundary_kernel(c1 = discount * strike + slope(time_line[(i + 1):N]) * pull(time_line[(i + 1):N]),
                            c2 = discount + slope(time_line[(i + 1):N]),
                            i1 = i, x1 = bnd_old[i], 
                            i2 = (i + 1):N, x2 = bnd_old[(i + 1):N])
      
      # Update the boundary at t_present
      bnd[i] <- strike - K1 - sum(K2 * delta[i:(N - 1)])
      
      if(any(bnd[i] > strike)){
        print("Imposible: Boundary above the strike price")
      }
      
    }
    
    # absolute L2 error
    e <- sum((bnd - bnd_old)^2)
    print(paste0("error: ", e))
    if (errors) er <- c(er, e)
    
  }
  
  if (errors) return(list(boundary = bnd, errors = er))
  
  return(bnd)
  
}

# expiration <- 1
# partition_length <- 100
# time_line <- seq(0, 1, l = partition_length)
# strike <- 0
# discount <- 0
# slope <- Vectorize(function(t) 30/5*(1 + t))
# pull <- Vectorize(function(t) 2*sin(4*pi*t))
# vol <- Vectorize(function(t) (1+t)^3)
# 
# bnd <- boundary(strike = strike, time_line = time_line, 
#                 discount = discount, boundary_kernel = boundary_kernel, 
#                 slope = slope, pull = pull, vol = vol, errors = TRUE)
# err <- bnd$errors
# bnd <- bnd$boundary
# 
# # plot
# FigGen <- F
# 
# par(mar=c(3, 3, 1, 1), cex.axis = 1, cex.lab = 1,
#     mgp = c(2, 0.85, 0), tcl = -0.4) # Setting for plotting in RStudio
# 
# if(FigGen == TRUE) { # Setting for saving the PDF image
#   pdf(paste0("boundary.pdf"), width = , height = )
#   par(mar=c(3, 3, 1, 1), cex.axis = 2, cex.lab = 2,
#       mgp = c(1.5, 1.8, 0), fin = c(6, 6.5),tcl = -0.8) # Setting for printing in RStudio
# }
# 
# plot(time_line, bnd, type = "l", ylim = c(min(bnd), max(bnd)),
#      xlab = "Time", ylab = "Boundary")
# # lines(t_line, level[j, ], lty = 2)
# # lines(t_line, x_pin[j, ], lty = 1, col = "red")
# # lines(t_line, level_pin, lty = 2, col = "red")
# # lines(c(0, 1), c(0, 0), lty = 3)
# 
# if (FigGen == TRUE) dev.off()
