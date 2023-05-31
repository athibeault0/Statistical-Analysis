#Arielle Thibeault
#A13372654
#Math 185: HW 3

#2
#(a)
x <- c(8.3,8.3,12.1,12.1,17.0,17.0,17.0,24.3,24.3,24.3,33.6)
y <- c(224,312,362,521,640,539,728,945,738,759,663)
plot(x,y)

#(b)
fit <- lm(y ~ x)
summary(fit)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)  201.770    114.452   1.763  0.11175  #beta hat
#x             21.238      5.859   3.625  0.00553  #alpha hat

#(c)
#Paired Bootstrap Method
index <- 1:11
B <- 5000
intercept <- numeric(B)
slope <- numeric(B)
for (b in 1:b) {
  ind <- sample(index, 11, replace = TRUE)
  intercept[b] <- summary(lm(y[ind] ~ x[ind]))$coefficients[1,1]
  slope[b] <- summary(lm(y[ind] ~ x[ind]))$coefficients[2,1]
}
sd(intercept) #the standard error of our intercept estimator
sd(slope) #the standard error of our slope estimate

#Residual Bootstrap Method
slopehat <- summary(lm(y ~ x))$coefficients[2,1]
intercepthat <- summary(lm(y ~ x))$coefficients[1,1]
resid <- y - slopehat*x - intercepthat

index <- 1:11
B <- 5000
intercept <- numeric(B)
slope <- numeric(B)
for (b in 1:B) {
  ind <- sample(index, 11, replace = TRUE)
  res <- resid[ind]
  yboot <- x*slopehat + intercepthat + res
  intercept[b] <- summary(lm(yboot - x))$coefficients[1,1]
  slope[b] <- summary(lm(yboot - x))$coefficients[2,1]
}
sd(intercept) #the standard error of intercepthat
sd(slope) #the standard error of slopehat


#3
#(a)
lambda <- 1
n <- 10
x <- rexp(n, rate = lambda)

#(b)
alpha <- 0.05
B <- 10000
n <- 10
theta <- log(n/ sum(x))
thetahat <- numeric(B)
pivot <- numeric(B)
for (b in 1:B) {
  ind <- sample(1:n, n, replace = TRUE)
  thetahat[b] <- log(n/ sum(x[ind]))
  pivot[b] <- thetahat[b] - theta
}
thetabar <- mean(thetahat)
summ <- 0
for (i in 1:B) {
  summ <- summ + (thetahat[i] - thetabar)^2
}
varianceest <- summ/ B #variance estimator
c(theta - qnorm(1- alpha/2)*sqrt(varianceest), theta - qnorm(alpha/2)*sqrt(varianceest))

ci.bootpivot <- c((2*theta - quantile(thetahat, 1-alpha/2)),(2*theta - quantile(thetahat, alpha/2)))
print(ci.bootpivot)

#(c)
alpha <- 0.05
B <- 10000
n <- 10
theta <- log(n/ sum(x))

lamdahat <- 1/ mean(x)
thetahat <- numeric(B)
pivot <- numeric(B)
for (b in 1:B) {
  a <- rexp(n, lamdahat)
  thetahat[b] <- log(n/ sum(a))
  pivot[b] <- thetahat[b] - theta
}

thetabar <- mean(thetahat)
summ <- 0
for (i in 1:B) {
  summ <- summ + (thetahat[i] - thetabar)^2
}

varianceest <- summ/ B #variance estimator
c(theta - qnorm(1- alpha/2)*sqrt(varianceest), theta - qnorm(alpha/2)*sqrt(varianceest))

ci.bootpivot <- c((2*theta - quantile(thetahat, 1-alpha/2)),(2*theta - quantile(thetahat, alpha/2)))
print(ci.bootpivot)

#(d)
B <- 10000
thetanhat <- numeric(B)
for (b in 1:B) {
  realx <- rexp(10, lambda)
  thetanhat[b] <- log(n/ sum(realx))
}
sampleave <-mean(thetanhat)
summed <- 0
for (i in 1:B) {
  summed <- summed + (thetanhat[i] - sampleave)^2
}
varest <- (1/B)*summed 
#Monte Carlo estimates for point and variance
mean(thetanhat) 
varest

#4
B <-  1000
n <-  10000
results <- c()
for (eps in c(0, 0.001, 0.002, 0.005, 0.01)){
  sdest <- c()
  madest <- c()
  for (b in 1:B){
    index <- sample(n, floor(n * (1 - eps)))
    X <- numeric(n)
    X[index] <- rnorm(floor(n * (1 - eps)), 0, 1)
    X[-index] <- rnorm(n - floor(n * (1 - eps)), 0, 3)
    sdest <- c(sdest,sd(X))
    madest <- c(madest, mad(X))
  }
  results <- c(results, var(sdest) / var(madest))
}
results
