dpois(1, 3)
ppois(1, 3)
ppois(1, 3, lower.tail = F)
pgamma(1.5, 2, 1/3) - pgamma(.5, 2, 1/3)
qnorm(.975)
pnorm(-1.96) - pnorm(1.96)
qnorm(.05)

choose(5,0) * .8^0*.2^5 ##conservative
choose(5,0) * .3^0*.7^5 ##liberal

round((choose(5,0) * .8^0*.2^5)*.5 / 
  ((choose(5,0) * .8^0*.2^5)*.5 + (choose(5,0) * .3^0*.7^5)*.5), 3)

round((choose(5,0) * .3^0*.7^5)*.5 / 
        ((choose(5,0) * .8^0*.2^5)*.5 + (choose(5,0) * .3^0*.7^5)*.5), 3)

choose(4,2) * .5^4 * .4 ##fair
choose(4,2) * .7^2 * .3^(4-2) * .3 ##heads
choose(4,2) * .3^2 * .7^(4-2) * .3 ##tails

bottom = (choose(4,2) * .5^4 * .4 + 
            choose(4,2) * .7^2 * .3^(4-2) * .3 + 
            choose(4,2) * .3^2 * .7^(4-2) * .3)

round(choose(4,2) * .5^4 * .4 / bottom, 2) ##fair

round(choose(4,2) * .7^2 * .3^(4-2) * .3 / bottom * 2, 2) 

theta = seq(.01, .99, .01)
loglike = function(theta) {
  return(2 * (1-theta))
}
plot(theta, loglike(theta))

##going from .6 loaded, .4 not

bottom = choose(1,1)*(.5*.4 + .7*.6)
choose(1,1) * (.5*.4) / bottom ##fair
choose(1,1) * (.7*.6) / bottom ##loaded 
