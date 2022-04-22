library(lmtest)

set.seed(50)
x <- rbeta(50, 5, 1)
x
hist(x)

# 12.3
-2 *log((5^(50) * prod(x^(4))) / ((-50/sum(log(x)))^(50) * prod(x^((-50)/(sum(log(x))) - 1))))

-2 * log(
  ((5*sum(log(x)))/(-50))^(50)* prod(x)^(5 + 50/sum(log(x)))
)


# 12.4
set.seed(50)
y <- rpois(100, 3)
2*3*100 -2*mean(y)*100 -2*sum(y)*log(3/mean(y))

# HW 7
# 2 d)
set.seed(123)
x <- rexp(50, 0.05)
-2*50*log(mean(x) / 20) -2*50*(1 - mean(x) / 20)
mean(x)



#####  Test 1 Q2 d)
x <- c(4.5, 2.5, 2.8, 4.4, 2.4, 0.4, 2.7, 2.6, 2.0, 2.4, 2.6, 3.2)

sum((x/2)^(2))
sum(x^2)/1.5^(2)
sum(x^2)/4

sqrt(sum(x^2) / qgamma(0.975, shape = 12, scale = 2))

qgamma(0.05, shape = 12, scale = 2)
qchisq(0.95, 1)

exp(qchisq(0.95, 1) / (-2))
