#### Pre-loads ####
library(pracma)
source(file = "1.1-GM_AmOp_OS.R")
source(file = "1.2-GMB_OSB.R")
############ Figure 1: Changing the pulling level############
#### Settings
expiration <- 1
partition_length <- 200
time_line <- log(seq(exp(0), exp(expiration), l = partition_length))
strike <- 0
discount <- 1

slope <- Vectorize(function(t) 1)
pull <- list(
  Vectorize(function(t) -1),
  Vectorize(function(t) pnorm(25 - 50*t) - 0.5),
  Vectorize(function(t) 1)
)
vol <- Vectorize(function(t) 1)

bnd <- matrix(ncol = partition_length)
err <- list()
for(i in 1:3){

  bnd_err <- boundary(strike = strike, time_line = time_line,
                      discount = discount,
                      boundary_kernel = boundary_kernel,
                      slope = slope, pull = pull[[i]], vol = vol,
                      errors = TRUE)
  bnd <- rbind(bnd, bnd_err$boundary)
  err[[i]] <- log(bnd_err$errors)

}
bnd <- bnd[-1, ]

pull_line1 <- pull[[1]](time_line)
pull_line2 <- pull[[2]](time_line)
pull_line3 <- pull[[3]](time_line)

save(time_line, bnd, err, expiration, strike,
     pull_line1, pull_line2, pull_line3, file = "Fig1.RData")
############ Figure 2: Changing the slope ############
#### Settings
expiration <- 1
partition_length <- 200
time_line <- log(seq(exp(0), exp(expiration), l = partition_length))
strike <- 0
discount <- 1

pull <- Vectorize(function(t) exp(0.5*t) - 1)
slope <- list(
  Vectorize(function(t) 10),
  Vectorize(function(t) 6 * pnorm(25 - 50*t) + 2),
  Vectorize(function(t) 1)
)
vol <- Vectorize(function(t) 1)

bnd <- matrix(ncol = partition_length)
err <- list()
for(i in 1:3){

  bnd_err <- boundary(strike = strike, time_line = time_line,
                      discount = discount,
                      boundary_kernel = boundary_kernel,
                      slope = slope[[i]], pull = pull, vol = vol,
                      errors = TRUE)
  bnd <- rbind(bnd, bnd_err$boundary)
  err[[i]] <- log(bnd_err$errors)

}
bnd <- bnd[-1, ]

pull_line <- pull(time_line)

save(time_line, bnd, err, expiration, strike, pull_line, file = "Fig2.RData")
############ Figure 3: Changing the volatility ############
#### Settings
expiration <- 1
partition_length <- 200
time_line <- log(seq(exp(0), exp(expiration), l = partition_length))
strike <- 0
discount <- 1

pull <- Vectorize(function(t) sin(2*pi*t))
slope <- Vectorize(function(t) 1)
vol <- list(
  Vectorize(function(t) 3),
  Vectorize(function(t) 2 * (dnorm(25 - 100*t) + dnorm(75 - 100*t)) * sqrt(2*pi) + 2),
  Vectorize(function(t) 1)
)

bnd <- matrix(ncol = partition_length)
err <- list()
for(i in 1:3){

  bnd_err <- boundary(strike = strike, time_line = time_line,
                      discount = discount,
                      boundary_kernel = boundary_kernel,
                      slope = slope, pull = pull, vol = vol[[i]],
                      errors = TRUE)
  bnd <- rbind(bnd, bnd_err$boundary)
  err[[i]] <- log(bnd_err$errors)

}
bnd <- bnd[-1, ]

pull_line <- pull(time_line)

save(time_line, bnd, err, expiration, strike, pull_line, file = "Fig3.RData")
############ Figure 4: Approximation of a BB ############
#### Settings
expiration <- 1
partition_length <- 200
time_line <- log(seq(exp(0), exp(expiration), l = partition_length))
strike <- 0
discount <- 0

pull <- Vectorize(function(t) strike)
slope <- list(
  Vectorize(function(t) sum(t^(0:0))),
  Vectorize(function(t) sum(t^(0:10))),
  Vectorize(function(t) sum(t^(0:100)))
)
vol <- Vectorize(function(t) 1)

bnd <- matrix(ncol = partition_length)
err <- list()
for(i in 1:3){

  bnd_err <- boundary(strike = strike, time_line = time_line,
                      discount = discount,
                      boundary_kernel = boundary_kernel,
                      slope = slope[[i]], pull = pull, vol = vol,
                      errors = TRUE)
  bnd <- rbind(bnd, bnd_err$boundary)
  err[[i]] <- log(bnd_err$errors)

}
bnd <- bnd[-1, ]

pull_line <- pull(time_line)

save(time_line, bnd, err, expiration, strike, pull_line, file = "Fig4.RData")
############ Figure 5: Approximation of a OUB ############
#### Settings
expiration <- 1
partition_length <- 200
time_line <- log(seq(exp(0), exp(expiration), l = partition_length))
strike <- 0
discount <- 0

eps <- c(0.25, 0.05, 0.01)
z <- -2 # pinning point
a <- 5

pull <- Vectorize(function(t) z/cosh(a*(1 - t)))
slope <- list(
  Vectorize(function(t) {
    if(t <= expiration - eps[1]){
      return(a * coth(a*(expiration - t)))
    }
    return((1 - exp(- a^2 * csch(a*eps[1])^2 * (t - expiration + eps[1]))) + a * coth(a*eps[1]))
  }),
  Vectorize(function(t) {
    if(t <= expiration - eps[2]){
      return(a * coth(a*(expiration - t)))
    }
    return((1 - exp(- a^2 * csch(a*eps[2])^2 * (t - expiration + eps[2]))) + a * coth(a*eps[2]))
  }),
  Vectorize(function(t) {
    if(t <= expiration - eps[3]){
      return(a * coth(a*(expiration - t)))
    }
    return((1 - exp(- a^2 * csch(a*eps[3])^2 * (t - expiration + eps[3]))) + a * coth(a*eps[3]))
  })
)
vol <- Vectorize(function(t) 1)

bnd <- matrix(ncol = partition_length)
err <- list()
for(i in 1:3){

  bnd_err <- boundary(strike = strike, time_line = time_line,
                      discount = discount,
                      boundary_kernel = boundary_kernel,
                      slope = slope[[i]], pull = pull, vol = vol,
                      errors = TRUE)
  bnd <- rbind(bnd, bnd_err$boundary)
  err[[i]] <- log(bnd_err$errors)

}
bnd <- bnd[-1, ]
OUB_bnd <- OUB_boundary(tol = 1e-3, z = -z, a = a, b = 0, v = 1, t_eval = time_line,
                        H = expiration, bnd = NULL, errors = FALSE)

pull_line <- pull(time_line)

save(time_line, bnd, err, expiration, strike, pull_line, 
     OUB_bnd, z, a, eps, file = "Fig5.RData")
############ Figure 6: Changing the discounting rate for partition_length = 5 ############
#### Settings
expiration <- 1
partition_length <- 5
time_line <- log(seq(exp(0), exp(expiration), l = partition_length))
strike <- 0
discount <- c(0, 1, 5)

pull <- Vectorize(function(t) 0)
slope <- Vectorize(function(t) 1)
vol <- Vectorize(function(t) 1)

bnd <- matrix(ncol = partition_length)
err <- list()
for(i in 1:3){

  bnd_err <- boundary(strike = strike, time_line = time_line,
                      discount = discount[i],
                      boundary_kernel = boundary_kernel,
                      slope = slope, pull = pull, vol = vol,
                      errors = TRUE)
  bnd <- rbind(bnd, bnd_err$boundary)
  err[[i]] <- log(bnd_err$errors)

}
bnd <- bnd[-1, ]

pull_line <- pull(time_line)

save(time_line, bnd, err, expiration, strike, pull_line, file = "Fig6.RData")
############ Figure 7: Changing the discounting rate for partition_length = 20 ############
#### Settings
expiration <- 1
partition_length <- 20
time_line <- log(seq(exp(0), exp(expiration), l = partition_length))
strike <- 0
discount <- c(0, 1, 5)

pull <- Vectorize(function(t) 0)
slope <- Vectorize(function(t) 1)
vol <- Vectorize(function(t) 1)

bnd <- matrix(ncol = partition_length)
err <- list()
for(i in 1:3){

  bnd_err <- boundary(strike = strike, time_line = time_line,
                      discount = discount[i],
                      boundary_kernel = boundary_kernel,
                      slope = slope, pull = pull, vol = vol,
                      errors = TRUE)
  bnd <- rbind(bnd, bnd_err$boundary)
  err[[i]] <- log(bnd_err$errors)

}
bnd <- bnd[-1, ]

pull_line <- pull(time_line)

save(time_line, bnd, err, expiration, strike, pull_line, file = "Fig7.RData")
############ Figure 8: Changing the discounting rate for partition_length = 200 ############
#### Settings
expiration <- 1
partition_length <- 200
time_line <- log(seq(exp(0), exp(expiration), l = partition_length))
strike <- 0
discount <- c(0, 1, 5)

pull <- Vectorize(function(t) 0)
slope <- Vectorize(function(t) 1)
vol <- Vectorize(function(t) 1)

bnd <- matrix(ncol = partition_length)
err <- list()
for(i in 1:3){

  bnd_err <- boundary(strike = strike, time_line = time_line,
                      discount = discount[i],
                      boundary_kernel = boundary_kernel,
                      slope = slope, pull = pull, vol = vol,
                      errors = TRUE)
  bnd <- rbind(bnd, bnd_err$boundary)
  err[[i]] <- log(bnd_err$errors)

}
bnd <- bnd[-1, ]

pull_line <- pull(time_line)

save(time_line, bnd, err, expiration, strike, pull_line, file = "Fig8.RData")