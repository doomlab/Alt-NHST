theta = seq(0, 1, .01)
plot(theta, dbeta(theta, 1/2,1/2), type = "l")

##less than
round(pbeta(.5,1,5), 2)

# equal-tailed 95% credible interval
qbeta(.025,8,16) ##lower
round(qbeta(.975,8, 16), 2) ##upper

plot(theta, dbeta(theta, 8, 16), type = "l")
pbeta(.35, 8, 16)

plot(theta, dbeta(theta, 8, 21), type = "l")
pbeta(.35, 8, 21)

lambda = seq(0,20, 1)
plot(lambda, dgamma(lambda, 67,6), type = "l")
lines(lambda, dgamma(lambda, 8,1), lty=2 )
67/6

# equal-tailed 95% credible interval
round(qgamma(.05,67,6),2) ##lower

(gamma(14) / gamma(15)) * (gamma(6) / gamma(5))

##quiz 9
##less than

round(pgamma(.1, 6, 93.5), 2)
plot(theta, dgamma(theta, 6,93.5), type = "l")
abline(v = .1)
sum(16, 8, 114, 60, 4, 23, 30, 105)

plot(theta, dgamma(theta, 9,390), type = "l")

round(qgamma(.975,9, 390),2) ##upper

theta = seq(0,100,1)
plotthis = function(a,b, theta) { ((b^a)*a) / ((b + theta)^(a+1))  }
plot(theta, plotthis(9,390,theta), type = "l")

round(qnorm(.975, 96.17, sqrt(.042)),2) ##upper
round(qnorm(.025, 96.17, sqrt(.042)),2) ##lower
pnorm(100, 96.17, sqrt(.042), lower.tail = T)


z <- rgamma(n=300, shape=3, rate=300)
x <- 1/z
mean(x)

z <- rgamma(1000, shape=16.5, rate=6022.9)
sig2 <- 1/z
muB <- rnorm(1000, mean=609.3, sd=sqrt(sig2/27.1))
quantile(x=mu, probs=c(0.025, 0.975))

z <- rgamma(1000, shape=18, rate=6798.94)
sig2 <- 1/z
muA <- rnorm(1000, mean=622.39, sd=sqrt(sig2/30.1))
quantile(x=mu, probs=c(0.025, 0.975))
mean( muA > muB )


mean(c(94.6, 95.4, 96.2, 94.9, 95.9))
