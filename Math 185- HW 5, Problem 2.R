#Arielle Thibeault
#A13372654
#Math 185: HW 5, Problem 2


#PART a:
find_betas <- function(x,y,h){
  model <- ksmooth(x, y, kernel = 'triangular', bandwidth = h)
}
#here, ksmooth runs the NW kernel regression with our input of x, y here is the
#F_hat that is described in the problem statement, and a determined bandwidth h
#model$y gives us the empirical estimator m_hat from which we get Beta0, Beta1 
#and Beta2, in this case since the NW estimator is the LSE



#PART b:
#for x=0
x <- 0 
n <- 1000
f <- rep(1,n) #we use this because exp(-0) = 1 so any random sample drawn from 
#this distribution will be 1

for (j in 1:5000) {
  h <- 10
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='red', lwd=2)
plot(mean(Error), type='l',col='green', lwd=2)


for (j in 1:5000) {
  h <- 25
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='red', lwd=2)
plot(mean(Error), type='l',col='green', lwd=2)


for (j in 1:5000) {
  h <- 100
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='red', lwd=2)
plot(mean(Error), type='l',col='green', lwd=2)
#obviously, when the bandwidth is smaller, the smoother is "flatter or less fit to the
#data and when the bandwith is larger

#for x=0.5
x <- 0.5 
n <- 1000
f <- exp(n, rate = -0.5)
y <- integrate(f, -Inf, Inf)

for (j in 1:5000) {
  h <- 10
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='blue', lwd=2)
plot(mean(Error), type='l',col='orange', lwd=2)

for (j in 1:5000) {
  h <- 25
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='blue', lwd=2)
plot(mean(Error), type='l',col='orange', lwd=2)

for (j in 1:5000) {
  h <- 100
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='blue', lwd=2)
plot(mean(Error), type='l',col='orange', lwd=2)

#for x=1
x <- 0.5 
n <- 1000
f <- exp(n)
y <- integrate(f, -Inf, Inf)

for (j in 1:5000) {
  h <- 10
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='purple', lwd=2)
plot(mean(Error), type='l',col='gold1', lwd=2)

for (j in 1:5000) {
  h <- 25
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='purple', lwd=2)
plot(mean(Error), type='l',col='gold1', lwd=2)

for (j in 1:5000) {
  h <- 100
  MSE <- numeric(h)
  for (k in 1:h){
    MSE[k] = find_betas(x,y,h)
    Error <- abs(MSE[k] - f[k])
  }
}

plot(MSE,type='l',col='purple', lwd=2)
plot(mean(Error), type='l',col='gold1', lwd=2)