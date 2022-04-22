library(tidyverse)
library(readr)
library(Bolstad)

tesla_dat <- read_csv("https://emp.byui.edu/johnsonc/Data/M424/Tesla_Prices_LA.csv")

tesla_dat %>% 
  ggplot(aes(x = mileage, y = price)) +
  geom_point(color = "skyblue3") +
  geom_smooth(method = lm, color = "skyblue4", se = FALSE)

tesla_lm <- lm(price ~ mileage, data = tesla_dat)
summary(tesla_lm)

# in class exercise 23

mens_dat <- read_csv("https://emp.byui.edu/johnsonc/Data/BodyMeasurementsCorrected.csv")

mens_dat %>% ggplot(aes(x = Height, y = Weight)) +
  geom_point(color = "skyblue3") +
  geom_smooth(method = lm, color = "skyblue3", se = FALSE)

mens_lm <- lm(Weight ~ Height, mens_dat)
summary(mens_lm)

stats <- mens_dat %>%
  summarize(
    n = n(),
    x_bar = mean(Height),
    y_bar = mean(Weight),
    SSx = sum((Height - mean(Height))^2),
    SSxy = sum((Height - mean(Height))*(Weight - mean(Weight)))
  )

stats

B <- stats$SSxy / stats$SSx
B

# in class 24 2)
library(modelr)
mse_dat <- mens_dat %>% add_residuals(mens_lm) %>% summarize(mse = sum(resid^2) / (n() -2))

mse_dat$mse

qt(0.975, 252 -2)

5.4833 - qt(0.975, 252 -2) * sqrt(mse_dat$mse / 1709.291)
5.4833 + qt(0.975, 252 -2) * sqrt(mse_dat$mse / 1709.291)

4.7903 - qt(0.975, 252 -2)*sqrt(0.279052)
4.7903 + qt(0.975, 252 -2)*sqrt(0.279052)

pt(-4.7903/0.5283, 250)

178.9401 + 4.7903*(72-70.30754) - qt(0.975, 252 -2) * sqrt(2.5872 + 0.2791*(70 - 70.30754)^2 + 661.6046)
178.9401 + 4.7903*(72-70.30754) + qt(0.975, 252 -2) * sqrt(2.5872 + 0.2791*(70 - 70.30754)^2 + 661.6046)

# HW 18 14.4 f)
qnorm(c(0.025, 0.975), -7.77512, sqrt(4.30622))
# h)
1-pnorm(3.747)
# i)
qnorm(c(0.025, 0.975), 50.72414,sqrt(153.4491))

# Computer Exercise 14.1
# a)
x <- c(11, 9, 9, 9, 9, 12, 11, 9)
y <- c(-21.6, -16.2, -19.5, -16.3, -18.3, -24.6, -22.6, -17.7)
b <- bayes.lin.reg(y, x, "normal", "normal", 0, 3, -20, 3, 1)
sqrt(b$slope$var)
# b)
qnorm(c(0.025, 0.975), b$slope$mean, sqrt(b$slope$var))
# c)
(-3-(b$slope$mean))/sqrt(b$slope$var)
pnorm((-3-(b$slope$mean))/sqrt(b$slope$var))
# d)
bayes.lin.reg(y, x, "normal", "normal", 0, 3, -20, 3, 1, pred.x = 10)
# e)
qnorm(c(0.025, 0.975), -19.89, 1.0605)

# Computer Exercise 14.2
# a)
x <- c(30, 30, 29, 21, 37, 28, 26, 38, 32, 21)
y <- c(22.4, 16.3, 16.2, 30.6, 12.1, 17.9, 25.5, 9.8, 20.5, 29.8)
b1 <- bayes.lin.reg(y, x, "normal", "normal", 0, 3, 20, 2, 3)
# b)
qnorm(c(0.025, 0.975), b1$slope$mean, sqrt(b1$slope$var))
# c)
(1-b1$slope$mean)/sqrt(b1$slope$var)
1-pnorm(12.18098)
# d)
bayes.lin.reg(y, x, "normal", "normal", 0, 3, 20, 2, 3, pred.x = 36)
# e)
qnorm(c(0.025, 0.975), 12.41, 3.3388)
