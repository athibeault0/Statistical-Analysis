#Arielle Thibeault
#A13372654
#Math 185: HW 4

install.packages("moments")
library(moments)

#Question 1
alpha <- 0.05
B <- 10000
n <- 100
y <- rnorm(n)
x <- exp(y)
theta <- skewness(x)

#Non-Parametric Bootstrap
thetahat <- numeric(B)
for (b in 1:B) {
  ind <- sample(1:n, n, replace = TRUE)
  thetahat[b] <- skewness(x[ind])
}
varest <- var(thetahat)*(B-1)/B

#Confidence Interval: Non-Parametric Bootstrap
c(theta - qnorm(1-alpha/2)*sqrt(varest), theta - qnorm(alpha/2)*sqrt(varest))
#[1] 0.9277484 1.8213698

#Confidence Interval: Non-Parametric Pivotal
cpivot <- c((2*theta - quantile(theta, 1-alpha/2)), (2*theta - quantile(theta, alpha/2)))
as.numeric(cpivot)
#[1] 1.374559 1.374559

muhat <- sum(log(x))/n
summ <- 0
for (i in 1:n) {
  summ <- summ + (log(x[i] - muhat)^2)
}
sigmahat <- summ/n

#Parametric Bootstrap Resampling
thetahat <- numeric(B)
for (b in 1:B) {
  thetahat[b] <- skewness(rlnorm(n, meanlog = muhat, sdlog = sigmahat))
}
varestpar <- var(thetahat)*(B-1)/B

#Confidence Interval: Parametric Bootstrap
c(theta - qnorm(1-alpha/2)*sqrt(varestpar), theta - qnorm(alpha/2)*sqrt(varestpar))
#[1] -0.413774  3.162892

#Confidence Interval: Parametric Pivotal
cpivot <- c((2*theta - quantile(thetahat, 1-alpha/2)), (2*theta - quantile(thetahat, alpha/2)))
as.numeric(cpivot)
#[1] -1.824223  1.712208

#As we expected, the non-parametric bootstrapping is the preferred method as it allows
#you to simulate the underlying distribution. Without it, we would probably not be able
#to find/ guess that x has the distribution log-normal.


#Question 2(d)
theta <- 1
B <- 10000
n <- 1000
x2 <- runif(n, min = 0, max = theta)
theta <- max(x2)
thetahat <- numeric(B)
thetastar <- numeric(B)
for(b in 1:B){
  new <- runif(n, min = 0, max <- theta)
  index <- sample(1:n, n, replace = TRUE)
  thetastar[b] <- max(x2[index])
  thetahat[b] <- max(new)
}
 
par(mfrow = c(1,2))
plot(ecdf(n*(thetahat - theta)))
plot(ecdf(n*(thetastar - thetahat)))


#Question 3(a-c)
#Note for this problem, alpha = intercept and beta = slope
x <- c(8.3,8.3,12.1,12.1,17.0,17.0,17.0,24.3,24.3,24.3,33.6)
y <- c(224,312,362,521,640,539,728,945,738,759,663)
int <- summary(lm(y~x))$coefficients[1,1]
slope1 <- summary(lm(y~x))$coefficients[2,1]

#Bootstrap
level <- 0.05
index <- 1:11
B <- 10000
intercept <- numeric(B)
slope <- numeric(B)
for (b in 1:B) {
  ind <- sample(index, 11, replace = TRUE)
  intercept[b] <- summary(lm(y[ind]~x[ind]))$coefficients[1,1]
  slope[b] <- summary(lm(y[ind]~x[ind]))$coefficients[2,1]
}

#Confidence Interval: Pivotal for intercept
cpivotint <- c((2*int - quantile(intercept, 1-level/2)), (2*int - quantile(intercept, level/2)))
as.numeric(cpivotint)
#[1] -51.85668 475.12446

#Confidence Interval: Pivotal for slope
cpivotslope <- c((2*slope1 - quantile(slope, 1-level/2)), (2*slope1 - quantile(slope, level/2)))
as.numeric(cpivotslope)
#[1]  2.066831 32.995664

#Normal Confidence Interval Variance Est.
varestint <- var(intercept)*(B-1)/B
varestslope <- var(slope)*(B-1)/B

#Confidence Interval: Intercept
c(int - qnorm(1-level/2)*sqrt(varestint), int - qnorm(level/2)*sqrt(varestint))
#[1] -76.71712 480.25623

#Confidence Interval: Slope
c(slope1 - qnorm(1-level/2)*sqrt(varestslope), slope1 - qnorm(level/2)*sqrt(varestslope))
#[1]  4.191302 38.285096