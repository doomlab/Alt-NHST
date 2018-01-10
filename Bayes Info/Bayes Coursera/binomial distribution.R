##give students a 40 choice exam, don't know how well they will do but should be better than guessing
####question 1) what are the parameters of interest####
##parameters are theta1 = prob student 1 will answer right
##theta2 = prob student 2 will answer right

####question 2) what is our likelihood####
##binomial(40, theta) assuming each question is independent 
##also assuming p is the same for each question for each student

####question 3) what prior should we use####
##beta prior for binomial, prior mass should be above .25, probably around 2/3
theta = seq(0, 1, .01)
plot(theta, dbeta(theta, 1, 1), type = "l")
plot(theta, dbeta(theta, 4, 2), type = "l") ##moving mass higher
plot(theta, dbeta(theta, 8, 4), type = "l") ##makes sample size bigger, concentrated

####question 4) what is the prior prob p(theta > .25), p(theta > .8)####
##use 1 - because greater than
1 - pbeta(.25,8,4)
1 - pbeta(.5,8,4)
1 - pbeta(.8,8,4)

####question 5) suppose the first student got 33 right .. what is the posterior distribution?####
##p(theta1 > .25), p(theta1 > .5), p(theta1 > .8)

# 5) Posterior is Beta(8+33,4+40-33) = Beta(41,11)
41/(41+11)  # posterior mean
33/40       # MLE

lines(theta,dbeta(theta,41,11))

# plot posterior first to get the right scale on the y-axis
plot(theta,dbeta(theta,41,11),type="l")
lines(theta,dbeta(theta,8,4),lty=2)
# plot likelihood
lines(theta,dbinom(33,size=40,p=theta),lty=3)
# plot scaled likelihood (he picked 44 because it worked well)
lines(theta,44*dbinom(33,size=40,p=theta),lty=3)
##posterior is in between prior and likelihood

# posterior probabilities
1-pbeta(.25,41,11)
1-pbeta(.5,41,11)
1-pbeta(.8,41,11) ##because data was over .8

# equal-tailed 95% credible interval
qbeta(.025,41,11)
qbeta(.975,41,11)

####question 6) suppose the second student got 24 questions right ####
##what is the posterior distribution
##p(theta2 > .25), p(theta2 > .5), p(theta2 > .8)
##what is the 95% credible interval

# 6) Posterior is Beta(8+24,4+40-24) = Beta(32,20)
32/(32+20)  # posterior mean
24/40       # MLE

plot(theta,dbeta(theta,32,20),type="l") ##posterior
lines(theta,dbeta(theta,8,4),lty=2) ##prior
lines(theta,44*dbinom(24,size=40,p=theta),lty=3) ##likelihood

1-pbeta(.25,32,20)
1-pbeta(.5,32,20)
1-pbeta(.8,32,20)

qbeta(.025,32,20)
qbeta(.975,32,20)

##overlaps with the first student, but centered very differently

####question 7) what is the posterior prob that theta1>theta2 ####
##i.e. that the first student has a better chance of getting a question right than the second student?

# 7) Estimate by simulation: draw 1,000 samples from each and see how often 
#    we observe theta1>theta2

theta1=rbeta(1000,41,11)
theta2=rbeta(1000,32,20)
mean(theta1>theta2)


# Note for other distributions:
# dgamma,pgamma,qgamma,rgamma
# dnorm,pnorm,qnorm,rnorm