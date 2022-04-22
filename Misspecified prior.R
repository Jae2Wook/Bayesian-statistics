library(tidyverse)

# lesson 25

# create a sequence of points
x <- seq(0, 1, 0.001)

df <- data.frame(x=x) %>% 
  mutate(
    prior = dbeta(x, 14.4, 9.6),
    likelihood = dbeta(x, 26, 76),
    posterior = dbeta(x, 39.4, 84.6)
  ) %>% 
  pivot_longer(c(prior, likelihood, posterior), names_to = "distribution", values_to = "y")
# c(prior, likelihood, posterior) = -x

# Plot Distribution
df %>%
  ggplot(aes(x = x, y =y, color = distribution)) +
  geom_line() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())

## Changing parameters
# Successes and failures from previous observations
success_used_to_get_prior <- 300 #15
failure_used_to_get_prior <- 200 #10

# Compute prior parameters
a <- success_used_to_get_prior / (success_used_to_get_prior + failure_used_to_get_prior) * (success_used_to_get_prior + failure_used_to_get_prior -1)
b <- (success_used_to_get_prior + failure_used_to_get_prior - 1) -a

# Observed data
observed_successes <- 250 #25
observed_failures <- 750 #75

# Create a sequence of points
x <- seq(0, 1, 0.001)
df <- data.frame(x=x) %>% 
  mutate(prior = dbeta(x, a, b),
         likelihood = dbeta(x, observed_successes + 1, observed_failures + 1),
         posterior = dbeta(x, a + observed_successes, b + observed_failures)) %>% 
  pivot_longer(-x, names_to = "distribution", values_to = "y")

# Plot distributions
df %>% 
  ggplot(aes(x = x, y = y, color = distribution)) +
  geom_line() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())

######### Normal prior & likelihood
# Prior
prior_mean <- 8.5
prior_sd <- 0.7

# Observed Data
sigma <- 1
n <- 5
y_bar <- 5.45
likelihood_mean <- y_bar
likelihood_sd <- sigma/sqrt(n)

x <- seq(min(prior_mean - 3*prior_sd, likelihood_mean - 3*likelihood_sd),
         max(prior_mean + 3*prior_sd, likelihood_mean + 3*likelihood_sd), 0.001)

df <- data.frame(x = x) %>% 
  mutate(prior = dnorm(x, prior_mean, prior_sd),
         likelihood = dnorm(x, likelihood_mean, likelihood_sd),
         posterior = dnorm(x, (1/prior_sd^2)/(1/prior_sd^2 + 1/likelihood_sd^2)*prior_mean
                           + (1/likelihood_sd^2)/(1/prior_sd^2 + 1/likelihood_sd^2) *likelihood_mean,
                           sqrt(1/(1/prior_sd^2 + 1/likelihood_sd^2)))) %>% 
  pivot_longer(-x, names_to = "distribution", values_to = "y")

# Plot distribution
df %>% 
  ggplot(aes(x = x, y = y, color = distribution)) +
  geom_line() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "top") +
  theme(legend.title = element_blank())

################
# mixture posterior
###############

# HW 19 16.2 c)
f1 <- gamma(25)/(gamma(20)*gamma(5)) * (gamma(61)*gamma(64))/gamma(125) * factorial(100) / (factorial(41)*factorial(59))

f2 <- gamma(2)/(gamma(1)*gamma(1)) * (gamma(42)*gamma(60))/(gamma(102)) * factorial(100) / (factorial(41)*factorial(59))

0.95*f1 / (0.95*f1 + 0.05*f2)

0.05*f2 / (0.95*f1 + 0.05*f2)

# 16.4
mean(c(501.5, 499.1, 498.5, 499.9, 500.4, 498.9, 498.4, 497.9, 498.8, 498.6))

f0 <- 1/sqrt(1+4/10) * exp(-1/(2*(1+4/10))*(499.2-502)^(2))

f1 <- 1/sqrt(4+4/10) * exp(-1/(2*(4+4/10))*(499.2-502)^(2))

1- 0.95*f0 / (0.95*f0 + 0.05*f1)

# Computer Exercise
#16.1
library(Bolstad)
# a)
output <- binomixp(76, 100, alpha0 = c(7, 13), alpha1 =  c(1, 1), p =0.95)
output

# Create a vector containing the estimated CDF of the posterior
len <- length(output$mix$posterior)
cum <- rep(0, len)
cum[1] <- output$mix$posterior[1]

for (i in c(2:len)) {
  cum[i] <- cum[i - 1] + output$mix$posterior[i]
}

cum

df <- data.frame(output$mix)
df$cdf <- cum/max(cum)

# To find the lower bound of the 95% credible interval,
# Choose the value of pi where the cdf is closet to 0.025
cdf.value <- 0.025
df[which.min(abs(df$cdf - cdf.value)), ] #c(6,1)]

cdf.value <- 0.975
df[which.min(abs(df$cdf - cdf.value)), ]# c(6,1)]

# To find the P-value for the test, find the value of the cdf where pi = 0.5
df[which(df$pi == 0.5),]

# 16.2
output <- binomixp(36, 100, alpha0 = c(6,14), alpha1 = c(1,1), p = 0.95)

len <- length(output$mix$posterior)
cum <- rep(0, len)
cum[1] <- output$mix$posterior[1]

for (i in c(2, len)){
  cum[i] <- cum[i-1] + output$mix$posterior[i]
}

df <- data.frame(output$mix)
df$cdf <- cum / max(cum)

cdf.value <- 0.025
df[which.min(abs(df$cdf - cdf.value)), ]

cdf.value <- 0.975
df[which.min(abs(df$cdf - cdf.value)), ]

df[which(df$pi == 0.5), ]
