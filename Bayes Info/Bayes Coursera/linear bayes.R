##http://www.math.uah.edu/stat/data/Challenger2.txt
# 23 previous space shuttle launches before the Challenger disaster
# T is the temperature in Fahrenheit, I is the O-ring damage index

oring=read.table("http://www.math.uah.edu/stat/data/Challenger2.txt",header=T)
attach(oring)
#note: masking T=TRUE
##ugh why the hell would you do this stupidness calling it T gah

plot(T,I)

oring.lm=lm(I~T)
summary(oring.lm)

# add fitted line to scatterplot
lines(T,fitted(oring.lm))            
##as temp increases, o ring damage decreases
# 95% posterior interval for the slope
##slppe plus and minus SE times t distribution with 21 DF
-0.24337 - 0.06349*qt(.975,21)
-0.24337 + 0.06349*qt(.975,21)
# note that these are the same as the frequentist confidence intervals

# the Challenger launch was at 31 degrees Fahrenheit
# how much o-ring damage would we predict?
# y-hat
18.36508-0.24337*31
coef(oring.lm)
coef(oring.lm)[1] + coef(oring.lm)[2]*31  

# posterior prediction interval (same as frequentist)
predict(oring.lm,data.frame(T=31),interval="predict")  
10.82052-2.102*qt(.975,21)*sqrt(1+1/23+((31-mean(T))^2/22/var(T))) ##lower bound

# posterior probability that damage index is greater than zero
# same predictive distribution as above 
# t distribution, what's the prob of t with this mean and df
# this is an upper interval
1-pt((0-10.82052)/(2.102*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))),21)


##http://www.math.uah.edu/stat/data/Galton.txt
# Galton's seminal data on predicting the height of children from the 
# heights of the parents, all in inches

heights=read.table("http://www.math.uah.edu/stat/data/Galton.txt",header=T)
attach(heights)
names(heights)

pairs(heights)
summary(lm(Height~Father+Mother+Gender+Kids))
summary(lm(Height~Father+Mother+Gender))
heights.lm=lm(Height~Father+Mother+Gender)

# each extra inch taller a father is correlated with 0.4 inch extra height in the child
# each extra inch taller a mother is correlated with 0.3 inch extra height in the child
# a male child is on average 5.2 inches taller than a female child
# 95% posterior interval for the the difference in height by gender
5.226 - 0.144*qt(.975,894) ##df residual
5.226 + 0.144*qt(.975,894)

# posterior prediction interval (same as frequentist)
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="M"),interval="predict")
predict(heights.lm,data.frame(Father=68,Mother=64,Gender="F"),interval="predict")


##quiz stuff
golf=read.table("http://www.stat.ufl.edu/~winner/data/pgalpga2008.dat",header=T)
colnames(golf) = c("dd", "percent", "FM")
datF <- subset(golf, FM==1, select=1:2)
datM <- subset(golf, FM==2, select=1:2)
plot(datF$dd, datF$percent)
plot(datM$dd, datM$percent)

females = lm(percent~dd, data = datF)
summary(females)
females$coefficients[1] + females$coefficients[2]*260
predict(females,data.frame(dd = 260),interval="predict") 

everyone = lm(percent ~ dd + FM, data = golf)
summary(everyone)
plot(fitted(everyone), residuals(everyone))
