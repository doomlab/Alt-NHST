M = c(2.5, 3.0, 3.5)
round = 1

library(data.table)
library(mvtnorm)

data_shape = matrix(NA, nrow = 20*100, ncol = 5)
colnames(data_shape) = c("stdev", "N", "skew1", "skew2", "skew3")
data_shape = as.data.frame(data_shape)

####sd loop here####
sdplaceholder = c(11.5, 3, .5, .10)
nplaceholder = c(10, 30, 100, 500, 1000)
for (i in 1:length(sdplaceholder)) {
  
  ####n loop here####  
  for (r in 1:length(nplaceholder)) { 
    
    
    ####sims loop here####
    for (q in 1:100) { 
      
      ####make the data here####
      sigma = matrix(c(sdplaceholder[i],0,0,0,sdplaceholder[i],0,0,0,sdplaceholder[i]), nrow = 3, ncol = 3)
      
      dataset = as.data.table(rmvnorm(nplaceholder[r], M, sigma))
      
      data_shape$skew1[round] = mean(apply(dataset, 2, skewness))
      
      dataset = round(dataset, digits = 0)
      
      data_shape$skew2[round] = mean(apply(dataset, 2, skewness))
      
      dataset[ dataset < 1] = 1
      dataset[ dataset > 7] = 7
      
      data_shape$skew3[round] = mean(apply(dataset, 2, skewness))
      
      data_shape$stdev[round] = sdplaceholder[i]
      data_shape$N[round] = nplaceholder[r]
      
      round = round + 1
      
    }
  }
}

with(data_shape, tapply(skew1, list(N,stdev), mean, na.rm = T))
with(data_shape, tapply(skew2, list(N,stdev), mean, na.rm = T))
with(data_shape, tapply(skew3, list(N,stdev), mean, na.rm = T))

      