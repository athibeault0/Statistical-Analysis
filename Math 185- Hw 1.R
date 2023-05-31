#Math 185: Hw 1
#Arielle Thibeault
#A13372654

#5. Wilcoxon Rank Sum Test

cotamine <- data.frame(
  pooled <- c(8,11,12,14,20,43,111,35,56,83,92,128,150,176,208),
  group <- c(rep('Unexposed',7),rep('Exposed',8)),
  rank <- rank(pooled))

Unexposed <- c(8,11,12,14,20,43,111)
Exposed <- c(35,56,83,92,128,150,176,208)

wilcox.test(Exposed, (Unexposed+25), alternative = "greater", conf.int = TRUE)
#	Wilcoxon rank sum test

#data:  Exposed and (Unexposed + 25)
#W = 45, p-value = 0.02704
#alternative hypothesis: true location shift is greater than 0
#95 percent confidence interval:
#  14 Inf
#sample estimates:
#  difference in location 
#54

#Since the wilcoxon test calculates U from W = U + m(m+1)/2,
#we calculate W by adding the extra term:

W_obs <- 45 + 7*8/2

#The other values are unshifted by the exta term so they remain the same