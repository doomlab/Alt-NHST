

rm(list = ls())
overall_sims <- read.csv("C:/Users/John/Desktop/overall_sims.csv")
data = overall_sims

######################################################################################## Binning
#################################### Bin Omni P NHST
options(scipen=999)
data$BinOmniP = data$omniP
data$BinOmniP = replace(data$BinOmniP, data$BinOmniP<0.05, "significant")
data$BinOmniP = replace(data$BinOmniP, data$BinOmniP<0.10, "marginal")
data$BinOmniP = replace(data$BinOmniP, data$BinOmniP<=1, "non-sig")
data$BinOmniP = factor(data$BinOmniP, levels = c("non-sig","marginal","significant"))
table(data$BinOmniP)
################################# Bin Omni P NHST

################################# Bin 1v2 P NHST
data$Binp1v2 = data$p1v2
data$Binp1v2 = replace(data$Binp1v2, data$Binp1v2<0.05, "significant")
data$Binp1v2 = replace(data$Binp1v2, data$Binp1v2<0.10, "marginal")
data$Binp1v2 = replace(data$Binp1v2, data$Binp1v2<=1, "non-sig")
data$Binp1v2 = factor(data$Binp1v2, levels = c("non-sig","marginal","significant"))
table(data$Binp1v2)
################################ Bin 1v2 P NHST

############################### Bin 1v3 P NHST
data$Binp1v3 = data$p1v3
data$Binp1v3 = replace(data$Binp1v3, data$Binp1v3<0.05, "significant")
data$Binp1v3 = replace(data$Binp1v3, data$Binp1v3<0.10, "marginal")
data$Binp1v3 = replace(data$Binp1v3, data$Binp1v3<=1, "non-sig")
data$Binp1v3 = factor(data$Binp1v3, levels = c("non-sig","marginal","significant"))
table(data$Binp1v3)
############################### Bin 1v3 P NHST

############################# Bin 2v3 P NHST
data$Binp2v3 = data$p2v3
data$Binp2v3 = replace(data$Binp2v3, data$Binp2v3<0.05, "significant")
data$Binp2v3 = replace(data$Binp2v3, data$Binp2v3<0.10, "marginal")
data$Binp2v3 = replace(data$Binp2v3, data$Binp2v3<=1, "non-sig")
data$Binp2v3 = factor(data$Binp2v3, levels = c("non-sig","marginal","significant"))
table(data$Binp2v3)
############################ Bin 2v3 P NHST

############################# Bin omni quades P
data$BinquadeP = data$quadeP
data$BinquadeP = replace(data$BinquadeP, data$BinquadeP<0.05, "significant")
data$BinquadeP = replace(data$BinquadeP, data$BinquadeP<0.10, "marginal")
data$BinquadeP = replace(data$BinquadeP, data$BinquadeP<=1, "non-sig")
data$BinquadeP = factor(data$BinquadeP, levels = c("non-sig","marginal","significant"))
table(data$BinquadeP)
############################ Bin omni quades P

############################# Bin quade P 1v2
data$BinquadeP1v2 = data$quadeP1v2
data$BinquadeP1v2 = replace(data$BinquadeP1v2, data$BinquadeP1v2<0.05, "significant")
data$BinquadeP1v2 = replace(data$BinquadeP1v2, data$BinquadeP1v2<0.10, "marginal")
data$BinquadeP1v2 = replace(data$BinquadeP1v2, data$BinquadeP1v2<=1, "non-sig")
data$BinquadeP1v2 = factor(data$BinquadeP1v2, levels = c("non-sig","marginal","significant"))
table(data$BinquadeP1v2)
############################ Bin quade P 1v2

############################ Bin quade P 1v3
data$BinquadeP1v3 = data$quadeP1v3
data$BinquadeP1v3 = replace(data$BinquadeP1v3, data$BinquadeP1v3<0.05, "significant")
data$BinquadeP1v3 = replace(data$BinquadeP1v3, data$BinquadeP1v3<0.10, "marginal")
data$BinquadeP1v3 = replace(data$BinquadeP1v3, data$BinquadeP1v3<=1, "non-sig")
data$BinquadeP1v3 = factor(data$BinquadeP1v3, levels = c("non-sig","marginal","significant"))
table(data$BinquadeP1v3)
########################### Bin quade P 1v3

############################ Bin quade P 2v3
data$BinquadeP2v3 = data$quadeP2v3
data$BinquadeP2v3 = replace(data$BinquadeP2v3, data$BinquadeP2v3<0.05, "significant")
data$BinquadeP2v3 = replace(data$BinquadeP2v3, data$BinquadeP2v3<0.10, "marginal")
data$BinquadeP2v3 = replace(data$BinquadeP2v3, data$BinquadeP2v3<=1, "non-sig")
data$BinquadeP2v3 = factor(data$BinquadeP2v3, levels = c("non-sig","marginal","significant"))
table(data$BinquadeP2v3)
########################### Bin quade P 2v3


########################## Bin overall BF
data$BinoverallBF = data$overallBF
data$BinoverallBF = replace(data$BinoverallBF, data$BinoverallBF<3, "weak")
data$BinoverallBF = replace(data$BinoverallBF, data$BinoverallBF<20, "positive")
data$BinoverallBF = replace(data$BinoverallBF, data$BinoverallBF<=Inf, "strong")
data$BinoverallBF = factor(data$BinoverallBF, levels = c("weak","positive","strong"))
table(data$BinoverallBF)
######################### Bin overall BF

########################## Bin BF 1v2
data$BinBF1v2 = data$BF1v2
data$BinBF1v2 = replace(data$BinBF1v2, data$BinBF1v2<3, "weak")
data$BinBF1v2 = replace(data$BinBF1v2, data$BinBF1v2<20, "positive")
data$BinBF1v2 = replace(data$BinBF1v2, data$BinBF1v2<=Inf, "strong")
data$BinBF1v2 = factor(data$BinBF1v2, levels = c("weak","positive","strong"))
table(data$BinBF1v2)
######################### Bin BF 1v2

########################## Bin BF 1v3
data$BinBF1v3 = data$BF1v3
data$BinBF1v3 = replace(data$BinBF1v3, data$BinBF1v3<3, "weak")
data$BinBF1v3 = replace(data$BinBF1v3, data$BinBF1v3<20, "positive")
data$BinBF1v3 = replace(data$BinBF1v3, data$BinBF1v3<=Inf, "strong")
data$BinBF1v3 = factor(data$BinBF1v3, levels = c("weak","positive","strong"))
table(data$BinBF1v3)
######################### Bin BF 1v3

######################### Bin BF 2v3
data$BinBF2v3 = data$BF2v3
data$BinBF2v3 = replace(data$BinBF2v3, data$BinBF2v3<3, "weak")
data$BinBF2v3 = replace(data$BinBF2v3, data$BinBF2v3<20, "positive")
data$BinBF2v3 = replace(data$BinBF2v3, data$BinBF2v3<=Inf, "strong")
data$BinBF2v3 = factor(data$BinBF2v3, levels = c("weak","positive","strong"))
table(data$BinBF2v3)
######################### Bin BF 2v3

######################### Bin stdev
data$stdev = replace(data$stdev, data$stdev==0.1, "Large")
data$stdev = replace(data$stdev, data$stdev==0.5, "Medium")
data$stdev = replace(data$stdev, data$stdev== 3,  "Small")
data$stdev = replace(data$stdev, data$stdev==11.5, "None")
table(data$stdev)
######################### Bin stdev


########################### Bin oom chance
data$Binoomchance = data$oomchance
data$Binoomchance = replace(data$Binoomchance, data$Binoomchance<0.05, "low")
data$Binoomchance = replace(data$Binoomchance, data$Binoomchance<0.10, "medium")
data$Binoomchance = replace(data$Binoomchance, data$Binoomchance<=1, "high")
data$Binoomchance = factor(data$Binoomchance, levels = c("high","medium","low"))
table(data$Binoomchance)

data$Binoompcc = data$oompcc
data$Binoompcc = replace(data$Binoompcc, data$Binoompcc<0.50, "low")
data$Binoompcc = replace(data$Binoompcc, data$Binoompcc<1.01, "high")
table(data$Binoompcc)

data$Binoom = 0
round = 0
nsim = nrow(data)

for(i in 1:nsim){
  round = round+1
  if(data$Binoompcc[round]=="high" && data$Binoomchance[round]=="low"){
    data$Binoom[round] = "significant"
  } else if(data$Binoompcc[round]=="high" && data$Binoomchance[round]=="medium"){
    data$Binoom[round] = "marginal"
  } else if(data$Binoompcc[round]=="high" && data$Binoomchance[round]=="high"){
    data$Binoom[round] = "non-sig"
  } else if(data$Binoompcc[round]=="low"){
    data$Binoom[round] = "non-sig"
  }
}
data$Binoom = factor(data$Binoom, levels = c("non-sig","marginal","significant"))
table(data$Binoom)
########################### Bin oom chance
######################################################################################## Binning


################################################################################ percent non sig ES Large
pnonsig_Large = as.data.frame(matrix(0, nrow = 5, ncol = 15))
names(pnonsig_Large) = c("Effect", "N","Parametric NHST Omnibus", "Parametric NHST 1v2", "Parametric NHST 1v3",
                      "Parametric NHST 2v3", "Non-Parametric NHST Omnibus", "Non-Parametric 1v2",
                      "Non-Parametric NHST 1v3", "Non-Parametric 2v3", "Bayes Factor Omnibus",
                      "Bayes Factor 1v2", "Bayes Factor 1v3", "Bayes Factor 2v3", "Observation Oriented Model")
largedata = subset(data, stdev=="Large")
pnonsig_Large[1,2] = 10
pnonsig_Large[2,2] = 30
pnonsig_Large[3,2] = 100
pnonsig_Large[4,2] = 500
pnonsig_Large[5,2] = 1000
pnonsig_Large[1,1] = "Large"
pnonsig_Large[2,1] = "Large"
pnonsig_Large[3,1] = "Large"
pnonsig_Large[4,1] = "Large"
pnonsig_Large[5,1] = "Large"

#Parametric NHST omnibus
nplaceholder = c(10, 30, 100, 500, 1000)
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinOmniP[largedata$N==place])/
            sum(table(largedata$BinOmniP[largedata$N==place])))*100 
  pnonsig_Large[round,3] = xtab[1]
}

#Parametric NHST 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$Binp1v2[largedata$N==place])/
            sum(table(largedata$Binp1v2[largedata$N==place])))*100 
  pnonsig_Large[round,4] = xtab[1]
}

#Parametric NHST 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$Binp1v3[largedata$N==place])/
            sum(table(largedata$Binp1v3[largedata$N==place])))*100 
  pnonsig_Large[round,5] = xtab[1]
}

#Parametric NHST 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$Binp2v3[largedata$N==place])/
            sum(table(largedata$Binp2v3[largedata$N==place])))*100 
  pnonsig_Large[round,6] = xtab[1]
}

#Non-Parametric Quade's Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinquadeP[largedata$N==place])/
            sum(table(largedata$BinquadeP[largedata$N==place])))*100 
  pnonsig_Large[round,7] = xtab[1]
}

#Non-Parametric Quade's 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinquadeP1v2[largedata$N==place])/
            sum(table(largedata$BinquadeP1v2[largedata$N==place])))*100 
  pnonsig_Large[round,8] = xtab[1]
}

#Non-Parametric Quade's 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinquadeP1v3[largedata$N==place])/
            sum(table(largedata$BinquadeP1v3[largedata$N==place])))*100 
  pnonsig_Large[round,9] = xtab[1]
}

#Non-Parametric Quade's 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinquadeP2v3[largedata$N==place])/
            sum(table(largedata$BinquadeP2v3[largedata$N==place])))*100 
  pnonsig_Large[round,10] = xtab[1]
}

#Bayes Factor Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinoverallBF[largedata$N==place])/
            sum(table(largedata$BinoverallBF[largedata$N==place])))*100 
  pnonsig_Large[round,11] = xtab[1]
}

#Bayes Factor 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinBF1v2[largedata$N==place])/
            sum(table(largedata$BinBF1v2[largedata$N==place])))*100 
  pnonsig_Large[round,12] = xtab[1]
}

#Bayes Factor 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinBF1v3[largedata$N==place])/
            sum(table(largedata$BinBF1v3[largedata$N==place])))*100 
  pnonsig_Large[round,13] = xtab[1]
}

#Bayes Factor 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$BinBF2v3[largedata$N==place])/
            sum(table(largedata$BinBF2v3[largedata$N==place])))*100 
  pnonsig_Large[round,14] = xtab[1]
}

#OOM Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(largedata$Binoom[largedata$N==place])/
            sum(table(largedata$Binoom[largedata$N==place])))*100 
  pnonsig_Large[round,15] = xtab[1]
}
################################################################################ percent non sig ES Large


############################################################################### percent non sig ES Medium
pnonsig_Medium = as.data.frame(matrix(0, nrow = 5, ncol = 15))
names(pnonsig_Medium) = c("Effect", "N","Parametric NHST Omnibus", "Parametric NHST 1v2", "Parametric NHST 1v3",
                       "Parametric NHST 2v3", "Non-Parametric NHST Omnibus", "Non-Parametric 1v2",
                       "Non-Parametric NHST 1v3", "Non-Parametric 2v3", "Bayes Factor Omnibus",
                       "Bayes Factor 1v2", "Bayes Factor 1v3", "Bayes Factor 2v3", "Observation Oriented Model")
mediumdata = subset(data, stdev=="Medium")
pnonsig_Medium[1,2] = 10
pnonsig_Medium[2,2] = 30
pnonsig_Medium[3,2] = 100
pnonsig_Medium[4,2] = 500
pnonsig_Medium[5,2] = 1000
pnonsig_Medium[1,1] = "Medium"
pnonsig_Medium[2,1] = "Medium"
pnonsig_Medium[3,1] = "Medium"
pnonsig_Medium[4,1] = "Medium"
pnonsig_Medium[5,1] = "Medium"

#Parametric NHST omnibus
nplaceholder = c(10, 30, 100, 500, 1000)
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinOmniP[mediumdata$N==place])/
            sum(table(mediumdata$BinOmniP[mediumdata$N==place])))*100 
  pnonsig_Medium[round,3] = xtab[1]
}

#Parametric NHST 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$Binp1v2[mediumdata$N==place])/
            sum(table(mediumdata$Binp1v2[mediumdata$N==place])))*100 
  pnonsig_Medium[round,4] = xtab[1]
}

#Parametric NHST 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$Binp1v3[mediumdata$N==place])/
            sum(table(mediumdata$Binp1v3[mediumdata$N==place])))*100 
  pnonsig_Medium[round,5] = xtab[1]
}

#Parametric NHST 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$Binp2v3[mediumdata$N==place])/
            sum(table(mediumdata$Binp2v3[mediumdata$N==place])))*100 
  pnonsig_Medium[round,6] = xtab[1]
}

#Non-Parametric Quade's Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinquadeP[mediumdata$N==place])/
            sum(table(mediumdata$BinquadeP[mediumdata$N==place])))*100 
  pnonsig_Medium[round,7] = xtab[1]
}

#Non-Parametric Quade's 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinquadeP1v2[mediumdata$N==place])/
            sum(table(mediumdata$BinquadeP1v2[mediumdata$N==place])))*100 
  pnonsig_Medium[round,8] = xtab[1]
}

#Non-Parametric Quade's 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinquadeP1v3[mediumdata$N==place])/
            sum(table(mediumdata$BinquadeP1v3[mediumdata$N==place])))*100 
  pnonsig_Medium[round,9] = xtab[1]
}

#Non-Parametric Quade's 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinquadeP2v3[mediumdata$N==place])/
            sum(table(mediumdata$BinquadeP2v3[mediumdata$N==place])))*100 
  pnonsig_Medium[round,10] = xtab[1]
}

#Bayes Factor Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinoverallBF[mediumdata$N==place])/
            sum(table(mediumdata$BinoverallBF[mediumdata$N==place])))*100 
  pnonsig_Medium[round,11] = xtab[1]
}

#Bayes Factor 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinBF1v2[mediumdata$N==place])/
            sum(table(mediumdata$BinBF1v2[mediumdata$N==place])))*100 
  pnonsig_Medium[round,12] = xtab[1]
}

#Bayes Factor 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinBF1v3[mediumdata$N==place])/
            sum(table(mediumdata$BinBF1v3[mediumdata$N==place])))*100 
  pnonsig_Medium[round,13] = xtab[1]
}

#Bayes Factor 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$BinBF2v3[mediumdata$N==place])/
            sum(table(mediumdata$BinBF2v3[mediumdata$N==place])))*100 
  pnonsig_Medium[round,14] = xtab[1]
}

#OOM Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(mediumdata$Binoom[mediumdata$N==place])/
            sum(table(mediumdata$Binoom[mediumdata$N==place])))*100 
  pnonsig_Medium[round,15] = xtab[1]
}
############################################################################### percent non sig ES Medium


############################################################################### percent non sig ES Small
pnonsig_Small = as.data.frame(matrix(0, nrow = 5, ncol = 15))
names(pnonsig_Small) = c("Effect", "N","Parametric NHST Omnibus", "Parametric NHST 1v2", "Parametric NHST 1v3",
                      "Parametric NHST 2v3", "Non-Parametric NHST Omnibus", "Non-Parametric 1v2",
                      "Non-Parametric NHST 1v3", "Non-Parametric 2v3", "Bayes Factor Omnibus",
                      "Bayes Factor 1v2", "Bayes Factor 1v3", "Bayes Factor 2v3", "Observation Oriented Model")
smalldata = subset(data, stdev=="Small")
pnonsig_Small[1,2] = 10
pnonsig_Small[2,2] = 30
pnonsig_Small[3,2] = 100
pnonsig_Small[4,2] = 500
pnonsig_Small[5,2] = 1000
pnonsig_Small[1,1] = "Small"
pnonsig_Small[2,1] = "Small"
pnonsig_Small[3,1] = "Small"
pnonsig_Small[4,1] = "Small"
pnonsig_Small[5,1] = "Small"

#Parametric NHST omnibus
nplaceholder = c(10, 30, 100, 500, 1000)
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinOmniP[smalldata$N==place])/
            sum(table(smalldata$BinOmniP[smalldata$N==place])))*100 
  pnonsig_Small[round,3] = xtab[1]
}

#Parametric NHST 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$Binp1v2[smalldata$N==place])/
            sum(table(smalldata$Binp1v2[smalldata$N==place])))*100 
  pnonsig_Small[round,4] = xtab[1]
}

#Parametric NHST 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$Binp1v3[smalldata$N==place])/
            sum(table(smalldata$Binp1v3[smalldata$N==place])))*100 
  pnonsig_Small[round,5] = xtab[1]
}

#Parametric NHST 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$Binp2v3[smalldata$N==place])/
            sum(table(smalldata$Binp2v3[smalldata$N==place])))*100 
  pnonsig_Small[round,6] = xtab[1]
}

#Non-Parametric Quade's Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinquadeP[smalldata$N==place])/
            sum(table(smalldata$BinquadeP[smalldata$N==place])))*100 
  pnonsig_Small[round,7] = xtab[1]
}

#Non-Parametric Quade's 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinquadeP1v2[smalldata$N==place])/
            sum(table(smalldata$BinquadeP1v2[smalldata$N==place])))*100 
  pnonsig_Small[round,8] = xtab[1]
}

#Non-Parametric Quade's 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinquadeP1v3[smalldata$N==place])/
            sum(table(smalldata$BinquadeP1v3[smalldata$N==place])))*100 
  pnonsig_Small[round,9] = xtab[1]
}

#Non-Parametric Quade's 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinquadeP2v3[smalldata$N==place])/
            sum(table(smalldata$BinquadeP2v3[smalldata$N==place])))*100 
  pnonsig_Small[round,10] = xtab[1]
}

#Bayes Factor Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinoverallBF[smalldata$N==place])/
            sum(table(smalldata$BinoverallBF[smalldata$N==place])))*100 
  pnonsig_Small[round,11] = xtab[1]
}

#Bayes Factor 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinBF1v2[smalldata$N==place])/
            sum(table(smalldata$BinBF1v2[smalldata$N==place])))*100 
  pnonsig_Small[round,12] = xtab[1]
}

#Bayes Factor 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinBF1v3[smalldata$N==place])/
            sum(table(smalldata$BinBF1v3[smalldata$N==place])))*100 
  pnonsig_Small[round,13] = xtab[1]
}

#Bayes Factor 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$BinBF2v3[smalldata$N==place])/
            sum(table(smalldata$BinBF2v3[smalldata$N==place])))*100 
  pnonsig_Small[round,14] = xtab[1]
}

#OOM Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(smalldata$Binoom[smalldata$N==place])/
            sum(table(smalldata$Binoom[smalldata$N==place])))*100 
  pnonsig_Small[round,15] = xtab[1]
}
############################################################################### percent non sig ES Small


############################################################################### percent non sig ES None
pnonsig_None = as.data.frame(matrix(0, nrow = 5, ncol = 15))
names(pnonsig_None) = c("Effect", "N","Parametric NHST Omnibus", "Parametric NHST 1v2", "Parametric NHST 1v3",
                     "Parametric NHST 2v3", "Non-Parametric NHST Omnibus", "Non-Parametric 1v2",
                     "Non-Parametric NHST 1v3", "Non-Parametric 2v3", "Bayes Factor Omnibus",
                     "Bayes Factor 1v2", "Bayes Factor 1v3", "Bayes Factor 2v3", "Observation Oriented Model")
nonedata = subset(data, stdev=="None")
pnonsig_None[1,2] = 10
pnonsig_None[2,2] = 30
pnonsig_None[3,2] = 100
pnonsig_None[4,2] = 500
pnonsig_None[5,2] = 1000
pnonsig_None[1,1] = "None"
pnonsig_None[2,1] = "None"
pnonsig_None[3,1] = "None"
pnonsig_None[4,1] = "None"
pnonsig_None[5,1] = "None"

#Parametric NHST omnibus
nplaceholder = c(10, 30, 100, 500, 1000)
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinOmniP[nonedata$N==place])/
            sum(table(nonedata$BinOmniP[nonedata$N==place])))*100 
  pnonsig_None[round,3] = xtab[1]
}

#Parametric NHST 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$Binp1v2[nonedata$N==place])/
            sum(table(nonedata$Binp1v2[nonedata$N==place])))*100 
  pnonsig_None[round,4] = xtab[1]
}

#Parametric NHST 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$Binp1v3[nonedata$N==place])/
            sum(table(nonedata$Binp1v3[nonedata$N==place])))*100 
  pnonsig_None[round,5] = xtab[1]
}

#Parametric NHST 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$Binp2v3[nonedata$N==place])/
            sum(table(nonedata$Binp2v3[nonedata$N==place])))*100 
  pnonsig_None[round,6] = xtab[1]
}

#Non-Parametric Quade's Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinquadeP[nonedata$N==place])/
            sum(table(nonedata$BinquadeP[nonedata$N==place])))*100 
  pnonsig_None[round,7] = xtab[1]
}

#Non-Parametric Quade's 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinquadeP1v2[nonedata$N==place])/
            sum(table(nonedata$BinquadeP1v2[nonedata$N==place])))*100 
  pnonsig_None[round,8] = xtab[1]
}

#Non-Parametric Quade's 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinquadeP1v3[nonedata$N==place])/
            sum(table(nonedata$BinquadeP1v3[nonedata$N==place])))*100 
  pnonsig_None[round,9] = xtab[1]
}

#Non-Parametric Quade's 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinquadeP2v3[nonedata$N==place])/
            sum(table(nonedata$BinquadeP2v3[nonedata$N==place])))*100 
  pnonsig_None[round,10] = xtab[1]
}

#Bayes Factor Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinoverallBF[nonedata$N==place])/
            sum(table(nonedata$BinoverallBF[nonedata$N==place])))*100 
  pnonsig_None[round,11] = xtab[1]
}

#Bayes Factor 1v2
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinBF1v2[nonedata$N==place])/
            sum(table(nonedata$BinBF1v2[nonedata$N==place])))*100 
  pnonsig_None[round,12] = xtab[1]
}

#Bayes Factor 1v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinBF1v3[nonedata$N==place])/
            sum(table(nonedata$BinBF1v3[nonedata$N==place])))*100 
  pnonsig_None[round,13] = xtab[1]
}

#Bayes Factor 2v3
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$BinBF2v3[nonedata$N==place])/
            sum(table(nonedata$BinBF2v3[nonedata$N==place])))*100 
  pnonsig_None[round,14] = xtab[1]
}

#OOM Omnibus
round=0
for(i in 1:length(nplaceholder)){
  round=round+1
  place = nplaceholder[i]
  xtab = (table(nonedata$Binoom[nonedata$N==place])/
            sum(table(nonedata$Binoom[nonedata$N==place])))*100 
  pnonsig_None[round,15] = xtab[1]
}
############################################################################### percent non sig ES None


###################### View percent non significant datasets
View(pnonsig_Large)
View(pnonsig_Medium)
View(pnonsig_Small)
View(pnonsig_None)

percentnonsig = rbind(pnonsig_Large,pnonsig_Medium,
                   pnonsig_Small,pnonsig_None)
View(percentnonsig)
setwd("C:/Users/John/Desktop")
write.csv(percentnonsig, file = "PercentNonSig.csv")
###################### View percent non significant datasets








