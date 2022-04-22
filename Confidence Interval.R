x <- c(88, 76.7, 63.3, 68.4, 60.3, 57.7, 62.9)
mean(x)
qnorm(0.975, sd = 1)
68.1857-1.96*sqrt(100/7)
68.1857+1.96*sqrt(100/7)

qnorm(0.95, sd = 1)
68.1857-1.65*sqrt(100/7)
68.1857+1.65*sqrt(100/7)

qnorm(0.01)
68.1857-2.33*sqrt(100/7)
qnorm(0.96)
68.1857+1.75*sqrt(100/7)

68.1857+qnorm(0.475)*sqrt(100/7)
68.1857+qnorm(0.525)*sqrt(100/7)

68.1857+qnorm(0.875)*sqrt(100/7)

qnorm(0.98, mean = 0, sd =1)
# in class 8 g)
x <- c(114, 91,100,97,104,87,121)
6*var(x)/qchisq(0.96, 6)

# in class 10 7
pnorm(-1.57)
pnorm(-1.24)

# HW 3.3
?qgamma()
qgamma(0.975, shape = 10, scale = 2)


#########
# Bootstrap Confidence Interval
#########

set.seed(1234)
#gives the random sample from exp(lambdahat)
obs <- rexp(n = 12, rate = 0.02) # gives random exponential distribution
obs

# Number of observed value
n <- length(obs)

# Number of bootstrap resamples to collect
nBoot <- 20000

# Initialize vector of sample medians (make zeros)
#median_diff <- rep(0, nBoot)

# Calculate the bootstrap medians
for(i in 1:nBoot){
  x = sample(obs ,n, replace = TRUE)
  median_diff[i] = median(x) - median(obs)
}

# median_boot <- median(obs) - median_diff

# find the 2.5th and 97.5th percentiles
sorted_median_diff <- sort(median_diff)

q_0.025 <- sorted_median_diff[500]
q_0.975 <- sorted_median_diff[19500]

ci <- median(obs) - c(q_0.975, q_0.025)

cat('Median Confidence Interval:', ci)

hist(median_diff, nclass = 10)

# Will be the same to

set.seed(1234)
#gives the random sample from exp(lambdahat)
obs <- rexp(n = 12, rate = 0.02) # gives random exponential distribution
obs

# Number of observed value
n <- length(obs)

# Number of bootstrap resamples to collect
nBoot <- 20000

# Initialize vector of sample medians (make zeros)
median_diff <- rep(0, nBoot)

# Calculate the bootstrap medians
for(i in 1:nBoot){
  x = sample(obs ,n, replace = TRUE)
  median_diff[i] = median(x) - median(obs)
}

median_boot <- median(obs) - median_diff

quantile(median_boot, c(0.025, 0.975))

# 90%
q_0.05 <- sorted_median_diff[1000]
q_0.95 <- sorted_median_diff[19000]

ci1 <- median(obs) - c(q_0.95, q_0.05)

cat('Median Confidence Interval:', ci1)

###########3 class
set.seed(1234)
obs <- rexp(n = 12, rate = 0.02)

n <- length(obs)
nBoot <-  20000

median_diff <- rep(0, nBoot)

for(i in 1:nBoot){
  x <- sample(obs, n, replace = TRUE)
  median_diff[i] <- median(x) - median(obs)
}

median_boot <- median(obs) - median_diff

quantile(median_boot, c(0.025, 0.975))

hist(median_boot, n = 10)

#
set.seed(1234)
obs <- rexp(n = 12, rate = 0.02)

n <- length(obs)
nBoot <-  20000

q3_diff <- rep(0, nBoot)

for(i in 1:nBoot){
  x <- sample(obs, n, replace = TRUE)
  q3_diff[i] <- quantile(x, 0.75) - quantile(obs, 0.75)
}

quantile(q3_diff, c(0.95, 0.05))

q3_boot <- quantile(obs, 0.75) - q3_diff

quantile(q3_boot, c(0.05, 0.95))

hist(q3_boot, n = 10)

#

set.seed(1234)
obs <- rexp(n = 12, rate = 0.02)

n <- length(obs)
nBoot <-  20000

# sd_diff <- rep(0, nBoot)

for(i in 1:nBoot){
  x <- sample(obs, n, replace = TRUE)
  sd_diff[i] <- sd(x) - sd(obs)
}

quantile(sd_diff, c(0.95, 0.05))

sd_boot <- sd(obs) - sd_diff

quantile(sd_boot, c(0.05, 0.95))

hist(sd_boot, n = 10)

# in class 19 1 c) 
qbinom(0.95, 11, 0.52) # need to be careful to use this depends on inequality sign
1- pbinom(8, 11, 0.52)
