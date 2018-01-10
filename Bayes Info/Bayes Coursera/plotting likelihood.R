##create a function - Bernoulli
##n total
##y number of deaths
##theta
likelihood = function (n, y, theta) {
  return(theta ^ y * (1-theta)^(n-y))
}

##create a series of mortality rates to plot over
theta = seq(.01, .99, .01)

plot(theta, likelihood(n = 400, y = 72, theta))
72/400
abline(v = .18)

loglike = function(n, y, theta) {
  return(y*log(theta) + (n-y)*log(1-theta))
}

plot(theta, loglike(400,72,theta))
abline(v = .18)

##do this as a line plot instead
plot(theta, loglike(400,72,theta), type = "l")
