library(Bolstad)
library(tidyverse)
library(latex2exp)

# Computer exercise 6.1

q6_1 <- binodp(x = 3, n = 8, pi = seq(0, 1, 0.2), pi.prior = rep(1/6, 6))
q6_1_joint <- q6_1$prior * q6_1$likelihood
sum(q6_1_joint)
q6_1_posterior <- q6_1_joint / sum(q6_1_joint)
q6_1_posterior

# Computer exercise 6.2
prior2 <- q6_1$posterior
q6_2 <- binodp(x = 2, n = 7, pi = seq(0, 1, 0.2), pi.prior = prior2)

q6_21 <- binodp(x = 5, n = 15, pi = seq(0, 1, 0.2), pi.prior = rep(1/6, 6))

# Credible intervals
?qbeta
qbeta(0.975, shape1 = 27, shape2 = 75)

27/ (27 + 75)
sqrt((27*75) / ((27+75)^(2) * (27+75+1)))

27/ (27 + 75) - sqrt((27*75) / ((27+75)^(2) * (27+75+1))) * qnorm(0.975)
27/ (27 + 75) + sqrt((27*75) / ((27+75)^(2) * (27+75+1))) * qnorm(0.975)

# Not sure this is the correct plot
p = seq(0,1, length=100)
#create plot of Beta distribution with shape parameters 2 and 10
plot(p, dbeta(p, 27, 75), type='l')


# Computer Exercise 8.1
c1 <- binobp(x = 6, n = 15, a = 1, b =1) 

quantile(c1$posterior, c(0.025, 0.975)) # ???????
c1$quantiles["0.025"]
c1$quantiles["0.975"]

################## from the answer
lower.int <- round(exercise8.2$quantiles["0.025"], 5)
upper.int <- round(exercise8.2$quantiles["0.975"], 5)
glue::glue("The 95% Credible Interval is: ({lower.int}, {upper.int})")
# The 95% Credible Interval is: (0.19119, 0.59219)
####################

# 8.2
c2 <- binobp(x = 6, n = 15, a = 2, b =4)
c2$quantiles["0.025"]
c2$quantiles["0.975"]

# 8.3
plot(c1)
plot(c2)

ggplot() +
  geom_line(data = tibble(posterior = c1$posterior,
                          pi = c1$pi),
            aes(x = pi, y = posterior, color = "8.1")) +
  geom_vline(aes(xintercept = 0.4117647), color = "red") +
  geom_line(data = tibble(posterior = c2$posterior,
                          pi = c2$pi),
            aes(x = pi, y = posterior, color = "8.2")) +
  geom_vline(aes(xintercept = 0.3809524), color = "blue") +
  scale_color_manual("Exercise",
                     values = c("8.1" = "red", "8.2" = "blue")) +
  labs(title = "Posterior Distributions of 8.1 & 8.2",
       x = TeX("$\\pi$"), y = "Posterior") +
  theme_bw()

# class material 19
# 19.3 d)
pbeta(0.52, 8.5, 3.5) # integral to get probability

# 19.4 a)
qbeta(c(0.025, 0.975), shape1= 15, shape2 = 6) # get critical values

# HW 14
# 9.2
plot(binobp(x = 6, n = 75, a = 7, b =75))
plot(binobp(x = 6, n = 75, a = 1, b =6))
qbeta(0.95, 7, 75)
1- pbeta(0.15, 7, 75)

# 9.3
qbeta(c(0.025, 0.975), 12, 115)

# In class exercise 21 2c)
qnorm(c(0.025, 0.975), mean = 3.742, sd = sqrt(1/42))

# in lass exercise 21 3
dat <- c(3.82, 3.26, 3.22, 3.44, 4.53, 3.82, 4.06, 4.65)
pop.sig <- sqrt(0.25)
prior.mu <- 3.4
prior.sig <- sqrt(0.1)
norm.np <- normnp(dat, prior.mu, prior.sig, pop.sig)
lower.b <- round(qnorm(0.025, norm.np$mean, norm.np$sd), 3)
upper.b <- round(qnorm(0.975, norm.np$mean, norm.np$sd), 3)
glue::glue("Posterior ~ N({round(norm.np$mean, 4)}, {round(norm.np$sd, 4)}^2)")
glue::glue("95% Credible Interval: ({lower.b}, {upper.b})")

# in class exercise 21 4c)
3.514 + qt(0.025, 7)*sqrt(0.0746 / 8)
3.514 + qt(0.975, 7)*sqrt(0.0746 / 8)
# =
3.7289 + qt(c(0.025, 0.975), 7)*sqrt(0.0269)

# in class 21 7
qnorm(c(0.025, 0.957), 3.7289, sqrt(0.0269+0.25))

# HW 15 ch11.3
pnorm(35, 36.868, sqrt(0.8919))

# ch 11.4
pnorm(5.2, 4.899, sqrt(0.0119))
1 - pnorm((5.2 - 4.899)/sqrt(0.0119))

# HW 16 11.6
qnorm(c(0.025, 0.975), 1002.475, sqrt(396.0396))

qnorm(c(0.025, 0.975), 1365.933, sqrt(276.1287))

# Computer Exercise 11.5
x <- c(26.8, 26.3, 28.3, 28.5, 26.3, 31.9, 28.5, 27.2, 20.9, 27.5, 28, 18.6, 22.3, 25, 31.5)
q11.5 <- normnp(x, m.x = 20, s.x = 5, 4)
q11.5$mean
q11.5$sd
qnorm(c(0.025, 0.975), q11.5$mean, q11.5$sd)

# Computer Exercise 11.6
q11.6 <- normnp(x, m.x = 30, s.x = 4, 4)
q11.6$likelihood
q11.6$posterior

# Computer Exercise 11.7
ggplot() +
  geom_line(data = tibble(posterior = q11.5$posterior, pi = q11.5$param.x),
            aes(x = pi, y = posterior, color = "11.5")) +
  geom_vline(aes(xintercept = q11.5$mean), color = "red") +
  geom_line(data = tibble(posterior = q11.6$posterior, pi = q11.6$param.x),
            aes(x = pi, y = posterior, color = "11.6")) +
  geom_vline(aes(xintercept = q11.6$mean), color = "blue") +
  scale_color_manual("Exercise", values = c("11.5" = "red", "11.6" = "blue")) +
  labs(title = "Posterior Distribution of 11.5 & 11.6", x = TeX("$\\pi$"), y = "Posterior") +
  theme_bw()

### exam 2
# 2 b)
pnorm(-0.2014/sqrt(0.001376826))

# 3 a)
dat <- read_csv("https://emp.byui.edu/johnsonc/data/M424-Test2-RandomCityPop.csv")
View(dat)

#########
dat$x_k <- 10/dat$`Population (in thousands)`

######

nrow(dat) / (sum(log(dat$`Population (in thousands)`)) - 50*log(10))

nrow(dat) / (sum(log10(dat$`Population (in thousands)`)) - 50*log10(10))

a <- 50/sum(log(dat$`Population (in thousands)` / 10))

50/sum(log10(dat$`Population (in thousands)` / 10))

(10^(50)/prod(dat$`Population (in thousands)`))^(0.01)

f <- expression((x*10^(x))^(50)/(4.4959e+72)^(x+1))
f
D(f,'x')

f <- function(x) (x * 10^(x))^((50) - 1) * ((50) * (10^(x) + x * (10^(x) * log(10))))/(4.4959e+72)^(x + 1) - (x * 10^(x))^(50) * ((4.4959e+72)^(x + 1) * log((4.4959e+72)))/((4.4959e+72)^(x + 1))^2

uniroot(f, c(0))

# b)
dat$k11 <- k*10^(k)/(dat$`Population (in thousands)`)^(k+1)
dat$j <- dat$`Population (in thousands)`-10
dat$k2 <- 2*10^2/(dat$`Population (in thousands)`)^3
dat$k10 <- 10*10^10/(dat$`Population (in thousands)`)^11
dat$jk <- k*10^(k)/(dat$j)^(k+1)

sum(dat$jk)

1/(sum(log(dat$`Population (in thousands)`)/50)-log(10))
sum(dat$k11)

barplot(dat$k1)
prod(dat$`Population (in thousands)`)
# d)
qgamma(c(0.05, 0.95), shape = 181, scale = 0.005)

#######
x <- prod(dat$x_k)

qgamma(c(0.05, 0.95), shape = 230, scale = 1/(1/0.005 - log(x)))

qgamma(c(0.05, 0.95), shape = 230, scale = 1/(1/0.005 + log(10^(-50)*prod(dat$`Population (in thousands)`))))

