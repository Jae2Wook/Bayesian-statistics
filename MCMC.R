library(tidyverse)
library(RColorBrewer)

# Exercise 1
# define target function and simulate data
n <- 10^6
target <- function(x) {sqrt(1-x^2)} # unscaled target pdf (0<x<1)
candidate <- runif(n)
u <- runif(n)

x <- runif(n)
M <- 1
df <- data.frame(x=x, u=u, y = M*u*candidate) %>% 
  mutate(to_keep = ifelse(u <= target(x)/(candidate), 1, 0))

# Scatterplot of n = 10,000 points
df %>% head(40000) %>% 
  ggplot(aes(x = x, y = y, color = as.factor(to_keep))) +
  geom_point(size = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = brewer.pal(n = 12, "Paired")[c(6,2)])

# Efficiency
sum(df$to_keep) / length(df$to_keep)

# Approximating CDF
sum((df %>% filter(x < 0.75))$to_keep)/ sum(df$to_keep)

# Plot Histogram
df %>% 
  filter(to_keep == 1) %>% 
  ggplot(aes(x = x)) +
  geom_histogram(fill = brewer.pal(n = 12, "Paired")[1],
            color = brewer.pal(n = 12, "Paired")[2],
            breaks = seq(0, 1, 1/25)) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")


# Exercise 2
# define target function and simulate data
M <- 1 # Constant that assures f(x) <= M * g(x) holds for all x
candidate <- function(x) {exp(-(x-4)^(2)/16)}
target <- function(x) {0.6*exp(-(x-2)^(2)/8) + 0.4*exp(-(x-7)^(2)/6)} # unscaled target pdf (0<x<1)

# Total number of simulated points (See Step 4)
n <- 10^6

# Step 1:  Simulate a random value x from the candidate density, g(x)
x <- rnorm(n, mean = 4, sd = sqrt(8))

# Step 2:  Simulate a random value u from a continuous uniform(0,1) density
u <- runif(n)

# Step 3:  If u <= f(x)/ (M * g(x)), then we accept x; otherwise, we reject x
df <- data.frame(x=x, u=u) %>%
  mutate(to_keep = ifelse(u <= target(x) / (M * candidate(x)), 1, 0))

# Compute Summary Statistics
mean(df$to_keep) # efficiency
df_cleaned <- df %>% filter(to_keep == 1)
mean(df_cleaned$x) # posterior mean
var(df_cleaned$x) # posterior variance

sum(df$to_keep) / length(df$to_keep)# Efficiency

# Approximating CDF
sum((df %>% filter(x < 7))$to_keep)/ sum(df$to_keep)

# Credible Interval
quantile((df %>% filter(to_keep == 1))$x, c(0.025, 0.975))

# Scatterplot of n = 10,000 points
df %>% head(50000) %>% 
  ggplot(aes(x = x, y = M*u*candidate(x), color = as.factor(to_keep))) +
  geom_point(size = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = brewer.pal(n = 12, "Paired")[c(6,2)])


# Histogram
df %>% 
  filter(to_keep == 1) %>% 
  ggplot(aes(x = x)) +
  geom_histogram(fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")



# Exercise 3
# define target function and simulate data
M <- 2 # Constant that assures f(x) <= M * g(x) holds for all x
candidate <- function(x){ 1/2 * exp(-abs(x)) }
target <- function(x){ exp(-abs(x)) * exp(-(x - 0.5)^2/2) }

# Total number of simulated points (See Step 4)
n <- 10^6

# Step 1:  Simulate a random value x from the candidate density, g(x)
r <- runif(n)
x <- ifelse(r<1/2, log(2*r), -log(2*(1-r)))

# Step 2:  Simulate a random value u from a continuous uniform(0,1) density
u <- runif(n)

# Step 3:  If u <= f(x)/ (M * g(x)), then we accept x; otherwise, we reject x
df <- data.frame(x=x, u=u) %>%
  mutate(to_keep = ifelse(u <= target(x) / (M * candidate(x)), 1, 0))

# Compute Summary Statistics
mean(df$to_keep) # efficiency

df_cleaned <- df %>% filter(to_keep == 1)
mean(df_cleaned$x) # posterior mean
var(df_cleaned$x) # posterior variance

sum(df$to_keep) / length(df$to_keep) # Efficiency

# Approximating CDF #########
sum((df %>% filter(x <= 0))$to_keep)/ sum(df$to_keep)

# Credible Interval
quantile((df %>% filter(to_keep == 1))$x, c(0.025, 0.975))

# Scatterplot of n = 10,000 points
df %>% head(50000) %>% 
  ggplot(aes(x = x, y = M* u*candidate(x), color = as.factor(to_keep))) +
  geom_point(size = 0.1) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = brewer.pal(n = 12, "Paired")[c(6,2)])

# Plot Histogram
df %>% 
  filter(to_keep == 1) %>% 
  ggplot(aes(x = x)) +
  geom_histogram(fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

# c)
df_cleaned <- df %>% filter(to_keep == 1)
mean(df_cleaned$x) # posterior mean
var(df_cleaned$x) # posterior variance

# d)
mean(df$to_keep)
sum(df$to_keep) / length(df$to_keep)

###########
# HW 20
# 1 a)
x <- runif(1000000, min = 2, max = 4)
max((0.75*exp(-(x-3)^(2)/2) + 0.25*exp(-(x-6)^(2)/4)) / (exp(-(x-4)^(2)/18)))

# b)
target <- function(x) {(0.75*exp(-(x-3)^(2)/2) + 0.25*exp(-(x-6)^(2)/4))}
candidate <- function(x) {exp(-(x-4)^(2)/18)}
M <- 0.8225
n <- 10^6

x <- rnorm(n, 4, 3)

u <- runif(n)

df <- data.frame(x=x, u=u) %>%
  mutate(to_keep = ifelse(u <= target(x) / (M * candidate(x)), 1, 0))

# Scatterplot of n = 10,000 points
df %>% head(50000) %>% 
  ggplot(aes(x = x, y = M*u*candidate(x), color = as.factor(to_keep))) +
  geom_point(size = 0.1) +
  xlim(-4,6) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = brewer.pal(n = 12, "Paired")[c(6,2)])

# Histogram
df %>% 
  filter(to_keep == 1) %>% 
  ggplot(aes(x = x)) +
  geom_histogram(fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

mean(df$to_keep) # efficiency

df_clean <- df %>% filter(to_keep ==1)
mean(df_clean$x) # posterior mean
var(df_clean$x) # posterior variance

# 2)
target <- function(x) {exp(-abs(x)) * exp(-0.5*(x-0.4)^2)}
candidate <- function(x) {(x^2 + 1)^(-1)}
n <- 10^6
u <- runif(n)

# To find M
# use Desmose to see target/candidate distribution
#x <- runif(n, -2, 2)
#max(target(x) / candidate(x))

M <- 0.924

##
x <- rt(n, 1)

df <- data.frame(x=x, u=u) %>% 
  mutate(to_keep = ifelse(u <= target(x)/(M*candidate(x)), 1, 0))


# Scatter plot
df %>% head(20000) %>% 
  ggplot(aes(x = x, y = M*u*candidate(x), color = as.factor(to_keep))) + # y = M*u*candidate(x) give all the values under the candidate
  geom_point(size = 0.1) +
  xlim(-4,4) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = brewer.pal(n = 12, "Paired")[c(6,2)])


# Histogram
df %>% 
  filter(to_keep == 1) %>% 
  ggplot(aes(x = x)) +
  geom_histogram(fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

mean(df$to_keep) # efficiency

df_clean <- df %>% filter(to_keep ==1)
mean(df_clean$x) # posterior mean
var(df_clean$x) # posterior variance

# CDF
sum((df %>% filter(x <= 0))$to_keep) / sum(df$to_keep)

# Credible Interval
quantile((df %>% filter(to_keep == 1))$x, c(0.025, 0.975))


########################## Monte Carlo

######## Metroplis-Hastings 1D
set.seed(198)
f <- function(x) {0.5*exp(-x^(2)/2) + 0.3*exp(-0.5*(x-3)^(2)/0.5^(2)) + 0.2*exp(-0.5*(x+3)^(2)/0.5^(2))}
x0 <- pi 

# 1st round
x00 <- rnorm(1, x0, 1) # random candidate value

f(x0) #previous target
f(x00) # new target

dnorm(x00, mean = x0, sd = 1) # previous candidate
dnorm(x0, mean = x00, sd = 1) # new candidate

f(x00) / f(x0)

runif(1)

# round 2
x1 <- x0
x11 <- rnorm(1, x1, 1)

f(x1) # previous target
f(x11) # new target

dnorm(x11, x1, 1) # pre candidate
dnorm(x1, x11, 1) # new candidate

f(x11)/f(x1)

runif(1)

####
# Define functions
target <- function(x) {0.5*exp(-x^(2)/2) + 0.3*exp(-0.5*(x-3)^(2)/0.5^(2)) + 0.2*exp(-0.5*(x+3)^(2)/0.5^(2))}
candidate <- function(x, prev, cand_var) {dnorm(x, prev, sqrt(cand_var))}

# Initialize values
set.seed(198)
candidate_variance <- 1
num_reps <- 10^4
previous_value <- pi
df <- data.frame(draw = 0, previous_value = previous_value,
                 candidate_value = NA, alpha = NA, u = NA, accept = NA,
                 retained_value = previous_value)

# Loop to complete as many iterations as desired
for (i in c(1:num_reps)){
  draw <- i
  candidate_value <- rnorm(1, previous_value, sqrt(candidate_variance))
  alpha <- min(1,
               target(candidate_value)/target(previous_value) *
                 candidate(previous_value, candidate_value, candidate_variance) /
                 candidate(candidate_value,previous_value, candidate_variance))
  u <- runif(1)
  accept <- ifelse(u<alpha, "Yes", "No")
  retained_value <- ifelse(u < alpha, candidate_value, previous_value)
  df <- df %>% 
    bind_rows(data.frame(draw, previous_value, candidate_value, alpha, u, accept, retained_value))
  previous_value <- retained_value
}

df %>% head()

hist(df$previous_value)

df %>% 
  ggplot(aes(x = previous_value)) +
  geom_histogram(fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2],
                 bins = 50) +
  #geom_line(stat = 'count', size = 2) +
  #geom_freqpoly() +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank())# +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

########## Metropolis-Hastings 2D
# Exercise 1
set.seed(25)
target_variance <- 1.5^2
target <- function(a, b) {exp(-2*a^2 -2*a*b -b^2)}
candidate <- function(a1, b1, a2, b2) {1/(2*pi*target_variance) * exp(-0.5/target_variance * ((a1-a2)^2 + (b1-b2)^2))}

# Round 1
# step 1
x0 <- c(2, -1) #(a, b)

# step 2
rnorm(1, 2, sqrt(target_variance)) # a
rnorm(1, -1, sqrt(target_variance)) # b

# step 3
target(1.68225, -2.562387)
target(2, -1)

candidate(2, -1, 1.68225, -2.562387)
candidate(1.68225, -2.562387, 2, -1)

target(1.68225, -2.562387)/target(2, -1) * candidate(2, -1, 1.68225, -2.562387) / candidate(1.68225, -2.562387, 2, -1)

# Round 2
# step 1
c(1.68225, -2.562387)

# step 2
rnorm(1, 1.682, sqrt(candidate_variance))
rnorm(1, -2.562, sqrt(candidate_variance))

# step 3
target(4.942, -3.191)
target(1.68225, -2.562387)

candidate(1.68225, -2.562387, 4.942, -3.191)
candidate(4.942, -3.191, 1.68225, -2.562387)

target(4.942, -3.191)/target(1.68225, -2.562387) * candidate(1.68225, -2.562387, 4.942, -3.191)/candidate(4.942, -3.191, 1.68225, -2.562387)

########
# Define functions
target <- function(a, b) {exp(-2*a^2 -2*a*b-b^2)}
candidate <- function(a1, b1, a_mean, b_mean, var) {dnorm(a1, a_mean, sqrt(var)) * dnorm(b1, b_mean, sqrt(var))}

# Initialize values
set.seed(25)
candidate_variance <- 1.5^2
num_reps <- 10^4
previous_a <- 2
previous_b <- -1
df <- data.frame(draw = 0, previous_a = previous_a, previous_b = previous_b, candidate_a = NA, candidate_b = NA, alpha = NA, u = NA, accept = NA, retained_a = previous_a, retained_b = previous_b)

# Loop to complete as many iterations as desired
for (i in c(1:num_reps)){
  draw <- i
  candidate_a <- rnorm(1, previous_a, sqrt(candidate_variance))
  candidate_b <- rnorm(1, previous_b, sqrt(candidate_variance))
  alpha <- min(1, target(candidate_a, candidate_b)/target(previous_a, previous_b) *
                 candidate(previous_a, previous_b, candidate_a, candidate_b, candidate_variance)/
                 candidate(candidate_a, candidate_b, previous_a, previous_b, candidate_variance))
  u <- runif(1)
  accept <- ifelse(u < alpha, "Yes", "No")
  if (u < alpha) {retained_a <- candidate_a; retained_b <- candidate_b}
  else {retained_a <- previous_a; retained_b <- previous_b}
  df <- df %>% bind_rows(data.frame(draw, previous_a, previous_b, candidate_a, candidate_b, alpha, u, accept, retained_a, retained_b))
  previous_a <- retained_a
  previous_b <- retained_b
}

df %>% head()

ggplot(df %>% slice(-c(1: (burn_in+1))), aes(x = previous_a, y = previous_b)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white")

df %>%
  ggplot(aes(x = previous_a)) +
  geom_histogram(binwidth = 1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

df %>%
  ggplot(aes(x = previous_b)) +
  geom_histogram(binwidth = 1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

##################
# Define functions
target <- function(a, b){ exp(-2 * a^2 - 2 * a * b - b^2) }
candidate <- function(a1, b1, a_mean, b_mean, var) {dnorm(a1, a_mean, sqrt(var)) * dnorm(b1, b_mean, sqrt(var))}

# Initialize values
set.seed(seed <- 25) # Random number seed
candidate_variance <- 1.5^2 # Marginal variance of candidate distribution
num_reps <- 10^4 # Number of values to simulate
previous_a <- 2 # Initial value
previous_b <- -1
df <- data.frame(draw = 0,
                 previous_a = previous_a, previous_b = previous_b,
                 candidate_a = NA, candidate_b = NA, alpha = NA, u = NA, accept = NA,
                 retained_a = previous_a, retained_b = previous_b)
# Loop to complete as many iterations as desired
for (i in c(1:num_reps)){
  draw <- i
  candidate_a <- rnorm(1, previous_a, sqrt(candidate_variance))
  candidate_b <- rnorm(1, previous_b, sqrt(candidate_variance))
  alpha <- min( 1,
                target(candidate_a, candidate_b) / target(previous_a, previous_b) *
                  candidate(previous_a, previous_b ,candidate_a ,candidate_b, candidate_variance) /
                  candidate(candidate_a, candidate_b, previous_a, previous_b, candidate_variance)
  )
  u <- runif(1)
  accept <- ifelse(u < alpha, "Yes", "No")
  if (u < alpha) { retained_a <- candidate_a; retained_b <- candidate_b }
  else { retained_a <- previous_a; retained_b <- previous_b }
  df <- df %>%
    bind_rows(data.frame(draw, previous_a, previous_b, candidate_a, candidate_b,
                         alpha, u, accept, retained_a, retained_b))
  previous_a <- retained_a
  previous_b <- retained_b
}
##################

df[-1,] %>% group_by(accept) %>% summarize(n = n()) %>% mutate(proportion = n / sum(n))

# Burn-in
burn_in <- 100
# Acceptance Rate
df %>%
  slice(-c(1:(burn_in+1))) %>%
  group_by(accept) %>%
  summarize(n=n()) %>%
  mutate(proportion = n / sum(n))

# Approximate posterior probability that parameter a is less than -1 P(a < -1)
df %>% slice(-c(1:(burn_in + 1))) %>% 
  summarize(sum = sum(retained_a < -1), n = n()) %>%
  mutate(proportion = sum / n)

# P(b > 2)
df %>% slice(-c(1:(burn_in + 1))) %>% 
  summarize(sum = sum(retained_b > 2), n = n()) %>%
  mutate(proportion = sum / n)

# P((a < 0 & b > 0) | (a > 0 & b < 0))
# approximate probability that is in the 2nd and 4th quadrants
df %>% slice(-c(1: (burn_in+1))) %>% 
  summarize(sum = sum((retained_a < 0 & retained_b > 0) | (retained_a > 0 & retained_b < 0)), n = n()) %>% 
  mutate(proportion = sum / n)

##################
# Homework 21
library(Bolstad2)

# 2
theta0 <- c(-1, 2)
theta1 <- c(4, 1)
p <- 0.55
candidate <- c(0, 1.5)
MCMCsampleRW <- normMixMH(theta0, theta1, p, candidate, steps = 1000, type = "rw")

############ 4
target <- function(x) {
  if (x < -pi){
    0
  }
  else if (x < 3*pi){
    (cos(2 * x) + sin(x / 2))^(4)
  }
  else{
    0
  }
}
candidate <- function(x, b) {
  if (x < b - 7){
    0
  }
  else if (x < b){
    1 / 56*(x - b + 7)
  }
  else if (x < b + 9){
    -1 / 72*(x - b - 9)
  }
  else {
    0
  }
}
inverse_x <- function(previous_value, w){
  if (w <= 7/16){
    previous_value - 7 + 4 * sqrt(7 * w)
  }
  else if (w < 1){
    previous_value + 9 - 12 * sqrt(1 - w)
  }
}

set.seed(10)
num_reps <- 2*10^4
previous_value <- 0

df <- data.frame(draw = 0, w = NA, previous_value = previous_value, candidate_value = NA, alpha = NA, u = NA, accept = NA, retained_value = previous_value)

for (i in c(1 : num_reps)) {
  draw <- i
  w <- runif(1)
  candidate_value <- inverse_x(previous_value, w)
  alpha <- min(1, target(candidate_value)/target(previous_value) * candidate(previous_value, candidate_value) / candidate(candidate_value, previous_value))
  u <- runif(1)
  accept <- ifelse(u<alpha, "Yes", "No")
  ifelse(u<alpha, retained_value <-  candidate_value, retained_value <-  previous_value)
  df <- df %>% bind_rows(data.frame(draw, w, previous_value, candidate_value, alpha, u, accept, retained_value))
  previous_value <- retained_value
}

df

# c)
df %>% 
  ggplot(aes(x = previous_value)) +
  geom_histogram(binwidth = 0.1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

burn_in <- 300

# d) acceptance rate
df %>% slice(-c(1: (burn_in + 1))) %>% group_by(accept) %>% summarize(n = n()) %>% mutate(proportion = n / sum(n))

# e) P(previous_value < 0)
df %>% slice(-c(1: (burn_in + 1))) %>% summarize(sum = sum(retained_value < 0), n = n()) %>% mutate(proportion = sum / n)

# f) 95% credible interval
quantile((df %>% slice(-c(1: (burn_in + 1))) %>% filter(accept == "Yes"))$previous_value, c(0.025, 0.975))



# 5
target <- function(a, b) {exp(-(b-a^(2)/8)^(2) - b^(2)/4) + exp(-((b-5)^(2) + (abs(a)-2)^(2)))}
candidate <- function(a1, b1, a_mean, b_mean, var) {dnorm(a1, a_mean, sqrt(var)) * dnorm(b1, b_mean, sqrt(var))}

set.seed(22)
rep_nums <- 30000
previous_a <- 2
previous_b <- 2
candidate_variance <- 2

df <- data.frame(draw = 0, previous_a = previous_a, previous_b = previous_b, candidate_a = NA, candidate_b = NA, alpha = NA, u = NA, accept = NA, retained_a = previous_a, retained_b = previous_b)

for (i in(1:rep_nums)) {
  draw <- i
  candidate_a <- rnorm(1, previous_a, candidate_variance)
  candidate_b <- rnorm(1, previous_b, candidate_variance)
  alpha <- min(1, target(candidate_a, candidate_b)/target(previous_a, previous_b) *
                 candidate(previous_a, previous_b, candidate_a, candidate_b, candidate_variance)/candidate(candidate_a, candidate_b, previous_a, previous_b, candidate_variance))
  u <- runif(1)
  accept <- ifelse(u < alpha, "Yes", "No")
  if (u<alpha) {retained_a <- candidate_a; retained_b <- candidate_b}
  else{retained_a <- previous_a; retained_b <- previous_b}
  df <- df %>% bind_rows(data.frame(draw, previous_a, previous_b, candidate_a, candidate_b, alpha, u, accept, retained_a, retained_b))
  previous_a <- retained_a
  previous_b <- retained_b
}

burn_in <- 100

# a)
df %>% slice(-c(1: (burn_in+1))) %>% group_by(accept) %>% 
  summarize(n = n()) %>% mutate(proportion = n / sum(n))

# b)
df %>% slice(-c(1: (burn_in+1))) %>% summarize(sum = sum(retained_a < retained_b), n = n()) %>% mutate(proportion = sum/n)

# c)
ggplot(df %>% slice(-c(1: (burn_in+1))), aes(x = previous_a, y = previous_b)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white")

############## Gibbs Sampling

# Define functions
mu_x <- 5
mu_y <- 2
sigma_x <- 2
sigma_y <- 3
rho <- 1/3

sim_x_given_y <- function(y) {rnorm(1, mean = mu_x + rho * sigma_x/sigma_y * (y - mu_y), sd = sigma_x * sqrt(1 - rho^2))}
sim_y_given_x <- function(x) {rnorm(1, mean = mu_y + rho * sigma_y/sigma_x * (x - mu_x), sd = sigma_y * sqrt(1 - rho^2))}

# Initialize values
set.seed(69)
num_reps <- 10^4
previous_x <- 10
previous_y <- 5
df <- data.frame(draw = 0, x = previous_x, y = previous_y)

df

# Loop
for (i in c(1 : num_reps)){
  draw <- i
  x <- sim_x_given_y(df %>% select(y) %>% slice(n()) %>% pull)
  y <- sim_y_given_x(x)
  df <- df %>% bind_rows(data.frame(draw, x, y))
}

df

# Contour Plot
df %>% ggplot(aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white") +
  xlab("x") +
  ylab("y") +
  labs(title = "Density Plot") +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

### I-class Exercise 30
# 1
set.seed(69)
rep_nums <- 10^4
x <- 2
y <- 2
df <- data.frame(draw = 0, x = x, y = y)

sim_x_given_y <- function(y) {rnorm(1, mean = 4/(1 + y^2), sd = sqrt(1/(1 + y^2)))}
sim_y_given_x <- function(x) {rnorm(1, mean = 4/(1 + x^2), sd = sqrt(1/(1 + x^2)))}

for (i in c(1: rep_nums)){
  draw <- i
  x <- sim_x_given_y(df %>% select(y) %>% slice(n()) %>% pull)
  y <- sim_y_given_x(x)
  df <- df %>% bind_rows(data.frame(draw, x, y))
}

df

# Contour Plot
df %>% ggplot(aes(x = x, y = y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white") +
  xlab("x") +
  ylab("y") +
  labs(title = "Density Plot") +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

df %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

######### 3
set.seed(69)
lambda <- 25
a <- 15
b <- 5

rep_nums <- 10^4
previous_x <- 16
previous_n <- 25
previous_theta <- 0.75
df <- data.frame(draw = 0, x = previous_x, theta = previous_theta, n = previous_n)

sim_x_given_n_theta <- function(n, theta) {rbinom(1, size = n, prob = theta)}
sim_theta_given_x_n <- function(x, n) {rbeta(1, shape1 = a + x, shape2 = b + n - x)}
sim_n_given_x_theta <- function(x, theta) {x + rpois(1, lambda = (1 - theta)*lambda)}

for (i in c(1: rep_nums)){
  draw <- i
  x <- sim_x_given_n_theta(previous_n, previous_theta)
  theta <- sim_theta_given_x_n(x, previous_n)
  n <- sim_n_given_x_theta(x, theta)
  df <- df %>% bind_rows(data.frame(draw, x, theta, n))
  previous_n <- n
  previous_theta <- theta
}

df

hist(df$x, breaks = 40)

# Contour Plot
df %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

df %>%
  ggplot(aes(x = theta)) +
  geom_histogram(binwidth = 0.1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

df %>%
  ggplot(aes(x = n)) +
  geom_histogram(binwidth = 0.1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

ggplot(df %>% slice(-c(1: (burn_in+1))), aes(x = x, y = theta)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white")

ggplot(df %>% slice(-c(1: (burn_in+1))), aes(x = x, y = n)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white")

mean(df$x)


##### Homework 22
set.seed(123)

rep_nums <- 2*10^4
a <- 1
b <- 500
x <- 10 # doesn't need it.
size = 1000
pi <- 0.99

df <- data.frame(draw = 0, x = x, pi = pi)

sim_x_given_pi <- function(pi) {rbinom(1, size = size, prob = pi)}
sim_pi_given_x <- function(x) {rbeta(1, shape1 = a + x, shape2 = b + size -x)}

for (i in c(1: rep_nums)) {
  draw <- i
  x <- sim_x_given_pi(df %>% slice(n()) %>% select(pi) %>% pull)
  pi <- sim_pi_given_x(x)
  df <- df %>% bind_rows(data.frame(draw, x, pi))
}

df

burn_in <- 300

df1 <- df %>% slice(-c(1: (burn_in+1)))

# c)
mean(df1$x)
var(df1$x)

# d)
df1 %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")

df1 %>%
  ggplot(aes(x = pi)) +
  geom_histogram(binwidth = 1,
                 fill = brewer.pal(n = 12, "Paired")[1],
                 color = brewer.pal(n = 12, "Paired")[2]) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  #theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  ylab("Frequency")


## for fun
ggplot(df %>% slice(-c(1: (burn_in+1))), aes(x = x, y = pi)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white")

############### Final
# Q5)
data <- read.delim("https://emp.byui.edu/johnsonc/data/DASL-Hubble.txt")
View(data)
mean_x <- mean(data$DistanceFromEarth)
mean_y <- mean(data$RecessionVelocity)
sig_2 <- 200^2

# B
B <- sum((data$DistanceFromEarth - mean_x)*(data$RecessionVelocity - mean_y))/
  sum((data$DistanceFromEarth - mean_x)^2)
B

# getting regression variance
lik_var <- sig_2 / sum((data$DistanceFromEarth - mean_x)^2)
lik_var

prec <- 1/250^2 + 1/(sig_2 / sum((data$DistanceFromEarth - mean_x)^2))
prec

1/prec

# getting regression mean
me <- (1/lik_var)/prec*B

# CI
qnorm(c(0.025, 0.975), mean = me, sd = sqrt(1/prec))

dat_lm <- lm(data$RecessionVelocity ~ data$DistanceFromEarth)
summary(dat_lm)

data %>% 
  ggplot(aes(x = DistanceFromEarth, y = RecessionVelocity)) +
  geom_point(color = "skyblue3") +
  geom_smooth(method = lm, color = "skyblue4", se = FALSE)

confint(dat_lm)

# Q6)
set.seed(25)
n <- 21000
var <- 2

target <- function(x, y) {exp(-((x^2 - 1) + (5*y/4 - sqrt(abs(x)))^2)^2)}
candidate <- function(x, y, x_mean, y_mean, var) {dnorm(x, x_mean, sqrt(var)) * dnorm(y, y_mean, sqrt(var))}

previous_x <- 0
previous_y <- 0
df <- data.frame(draw = 0, previous_x = previous_x, previous_y = previous_y, candidate_x = NA, candidate_y = NA, alpha = NA, u = NA, accept = NA, retained_x = previous_x, retained_y = previous_y)

for (i in c(1:n)) {
  draw <- i
  candidate_x <- rnorm(1, previous_x, sqrt(var))
  candidate_y <- rnorm(1, previous_y, sqrt(var))
  alpha = min(1, target(candidate_x, candidate_y)/target(previous_x, previous_y)*
                candidate(previous_x, previous_y, candidate_x, candidate_y, var)/
                candidate(candidate_x, candidate_y, previous_x, previous_y, var))
  u <- runif(1)
  accept <- ifelse(u < alpha, "Yes", "No")
  if (u < alpha) {retained_x <- candidate_x; retained_y <- candidate_y}
  else {retained_x <- previous_x; retained_y <- previous_y}
  df <- df %>% bind_rows(data.frame(draw, previous_x, previous_y, candidate_x, candidate_y, alpha, u, accept, retained_x, retained_y))
  previous_x <- retained_x
  previous_y <- retained_y
}

df %>% head(10)

burn_in <- 1000

df %>% slice(-c(1: (burn_in+1))) %>% group_by(accept) %>% summarize(n = n()) %>% mutate(prop = n / sum(n))

plot((df %>% slice(-c(1: (burn_in+1))))$previous_x, (df %>% slice(-c(1: (burn_in+1))))$previous_y)

df %>% tail(20000) %>% 
  ggplot(aes(x = previous_x, y = previous_y)) +
  geom_point(size = 1) +
  theme_bw() +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank()) +
  scale_color_manual(values = brewer.pal(n = 12, "Paired")[c(6,2)])
  
ggplot(df %>% slice(-c(1: (burn_in+1))), aes(x = previous_x, y = previous_y)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour = "white")

