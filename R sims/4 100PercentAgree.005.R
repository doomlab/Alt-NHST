
rm(list = ls())
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/Alt NHST/R sims")
overall_sims <- read.csv("overall_sims.csv")
data = overall_sims

######################################################################################## Binning
#################################### Bin Omni P NHST
options(scipen=999)
data$BinOmniP = data$omniP
data$BinOmniP = replace(data$BinOmniP, data$BinOmniP<0.005, "significant")
data$BinOmniP = replace(data$BinOmniP, data$BinOmniP<0.10, "marginal")
data$BinOmniP = replace(data$BinOmniP, data$BinOmniP<=1, "non-sig")
data$BinOmniP = factor(data$BinOmniP, levels = c("non-sig","marginal","significant"))
table(data$BinOmniP)
################################# Bin Omni P NHST

################################# Bin 1v2 P NHST
data$Binp1v2 = data$p1v2
data$Binp1v2 = replace(data$Binp1v2, data$Binp1v2<0.005, "significant")
data$Binp1v2 = replace(data$Binp1v2, data$Binp1v2<0.10, "marginal")
data$Binp1v2 = replace(data$Binp1v2, data$Binp1v2<=1, "non-sig")
data$Binp1v2 = factor(data$Binp1v2, levels = c("non-sig","marginal","significant"))
table(data$Binp1v2)
################################ Bin 1v2 P NHST

############################### Bin 1v3 P NHST
data$Binp1v3 = data$p1v3
data$Binp1v3 = replace(data$Binp1v3, data$Binp1v3<0.005, "significant")
data$Binp1v3 = replace(data$Binp1v3, data$Binp1v3<0.10, "marginal")
data$Binp1v3 = replace(data$Binp1v3, data$Binp1v3<=1, "non-sig")
data$Binp1v3 = factor(data$Binp1v3, levels = c("non-sig","marginal","significant"))
table(data$Binp1v3)
############################### Bin 1v3 P NHST

############################# Bin 2v3 P NHST
data$Binp2v3 = data$p2v3
data$Binp2v3 = replace(data$Binp2v3, data$Binp2v3<0.005, "significant")
data$Binp2v3 = replace(data$Binp2v3, data$Binp2v3<0.10, "marginal")
data$Binp2v3 = replace(data$Binp2v3, data$Binp2v3<=1, "non-sig")
data$Binp2v3 = factor(data$Binp2v3, levels = c("non-sig","marginal","significant"))
table(data$Binp2v3)
############################ Bin 2v3 P NHST

############################# Bin omni quades P
data$BinquadeP = data$quadeP
data$BinquadeP = replace(data$BinquadeP, data$BinquadeP<0.005, "significant")
data$BinquadeP = replace(data$BinquadeP, data$BinquadeP<0.10, "marginal")
data$BinquadeP = replace(data$BinquadeP, data$BinquadeP<=1, "non-sig")
data$BinquadeP = factor(data$BinquadeP, levels = c("non-sig","marginal","significant"))
table(data$BinquadeP)
############################ Bin omni quades P

############################# Bin quade P 1v2
data$BinquadeP1v2 = data$quadeP1v2
data$BinquadeP1v2 = replace(data$BinquadeP1v2, data$BinquadeP1v2<0.005, "significant")
data$BinquadeP1v2 = replace(data$BinquadeP1v2, data$BinquadeP1v2<0.10, "marginal")
data$BinquadeP1v2 = replace(data$BinquadeP1v2, data$BinquadeP1v2<=1, "non-sig")
data$BinquadeP1v2 = factor(data$BinquadeP1v2, levels = c("non-sig","marginal","significant"))
table(data$BinquadeP1v2)
############################ Bin quade P 1v2

############################ Bin quade P 1v3
data$BinquadeP1v3 = data$quadeP1v3
data$BinquadeP1v3 = replace(data$BinquadeP1v3, data$BinquadeP1v3<0.005, "significant")
data$BinquadeP1v3 = replace(data$BinquadeP1v3, data$BinquadeP1v3<0.10, "marginal")
data$BinquadeP1v3 = replace(data$BinquadeP1v3, data$BinquadeP1v3<=1, "non-sig")
data$BinquadeP1v3 = factor(data$BinquadeP1v3, levels = c("non-sig","marginal","significant"))
table(data$BinquadeP1v3)
########################### Bin quade P 1v3

############################ Bin quade P 2v3
data$BinquadeP2v3 = data$quadeP2v3
data$BinquadeP2v3 = replace(data$BinquadeP2v3, data$BinquadeP2v3<0.005, "significant")
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



################################################################## percent agree
######################## percent 100% agree ES Large
pAgree_Large = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Large) = c("N","Omnibus", "1v2", "1v3","2v3")
pAgree_Large[1,1] = 10
pAgree_Large[2,1] = 30
pAgree_Large[3,1] = 100
pAgree_Large[4,1] = 500
pAgree_Large[5,1] = 1000

pAgree_Sig_Large = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Sig_Large) = c("N","Omnibus", "1v2","1v3","2v3")
pAgree_Sig_Large[1,1] = 10
pAgree_Sig_Large[2,1] = 30
pAgree_Sig_Large[3,1] = 100
pAgree_Sig_Large[4,1] = 500
pAgree_Sig_Large[5,1] = 1000

pAgree_Nonsig_Large = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Nonsig_Large) = c("N","Omnibus", "1v2","1v3","2v3")
pAgree_Nonsig_Large[1,1] = 10
pAgree_Nonsig_Large[2,1] = 30
pAgree_Nonsig_Large[3,1] = 100
pAgree_Nonsig_Large[4,1] = 500
pAgree_Nonsig_Large[5,1] = 1000

largedata = subset(data, stdev=="Large")
library(stringr)
######## Overall
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(largedata, N==place)
  #make BF match
  overallmatch$tempoverallBF = overallmatch$BinoverallBF
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"weak","non-sig")
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"positive","marginal")
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$BinOmniP[round]==overallmatch$BinquadeP[round] &&
       overallmatch$BinOmniP[round]==overallmatch$Binoom[round] &&
       overallmatch$BinOmniP[round]==overallmatch$tempoverallBF[round] &&
       overallmatch$BinquadeP[round]==overallmatch$Binoom[round] &&
       overallmatch$BinquadeP[round]==overallmatch$tempoverallBF[round] &&
       overallmatch$Binoom[round]==overallmatch$tempoverallBF[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Large[xrun,2] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Large[xrun,2] = 0
  } else if(length(temp) > 1){
    pAgree_Large[xrun,2] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Large[xrun,2] = 0
    pAgree_Nonsig_Large[xrun,2] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Large[xrun,2] = x1_tab[3]
    pAgree_Nonsig_Large[xrun,2] = x1_tab[1]
  }
}
######## Overall

########### 1v2
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(largedata, N==place)
  #make BF match
  overallmatch$tempBF1v2 = overallmatch$BinBF1v2
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"weak","non-sig")
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"positive","marginal")
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp1v2[round]==overallmatch$BinquadeP1v2[round] &&
       overallmatch$Binp1v2[round]==overallmatch$tempBF1v2[round] &&
       overallmatch$BinquadeP1v2[round]==overallmatch$tempBF1v2[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Large[xrun,3] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Large[xrun,3] = 0
  } else if(length(temp) > 1){
    pAgree_Large[xrun,3] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Large[xrun,3] = 0
    pAgree_Nonsig_Large[xrun,3] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Large[xrun,3] = x1_tab[3]
    pAgree_Nonsig_Large[xrun,3] = x1_tab[1]
  }
}
########### 1v2

########### 1v3
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(largedata, N==place)
  #make BF match
  overallmatch$tempBF1v3 = overallmatch$BinBF1v3
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"weak","non-sig")
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"positive","marginal")
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp1v3[round]==overallmatch$BinquadeP1v3[round] &&
       overallmatch$Binp1v3[round]==overallmatch$tempBF1v3[round] &&
       overallmatch$BinquadeP1v3[round]==overallmatch$tempBF1v3[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Large[xrun,4] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Large[xrun,4] = 0
  } else if(length(temp) > 1){
    pAgree_Large[xrun,4] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Large[xrun,4] = 0
    pAgree_Nonsig_Large[xrun,4] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Large[xrun,4] = x1_tab[3]
    pAgree_Nonsig_Large[xrun,4] = x1_tab[1]
  }
}
########### 1v3

########### 2v3
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(largedata, N==place)
  #make BF match
  overallmatch$tempBF2v3 = overallmatch$BinBF2v3
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"weak","non-sig")
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"positive","marginal")
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp2v3[round]==overallmatch$BinquadeP2v3[round] &&
       overallmatch$Binp2v3[round]==overallmatch$tempBF2v3[round] &&
       overallmatch$BinquadeP2v3[round]==overallmatch$tempBF2v3[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Large[xrun,5] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Large[xrun,5] = 0
  } else if(length(temp) > 1){
    pAgree_Large[xrun,5] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Large[xrun,5] = 0
    pAgree_Nonsig_Large[xrun,5] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Large[xrun,5] = x1_tab[3]
    pAgree_Nonsig_Large[xrun,5] = x1_tab[1]
  }
}
########### 2v3
######################## percent 100% agree ES Large



######################## percent 100% agree ES Medium
pAgree_Medium = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Medium) = c("N","Omnibus", "1v2", "1v3","2v3")
pAgree_Medium[1,1] = 10
pAgree_Medium[2,1] = 30
pAgree_Medium[3,1] = 100
pAgree_Medium[4,1] = 500
pAgree_Medium[5,1] = 1000

pAgree_Sig_Medium = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Sig_Medium) = c("N","Omnibus", "1v2","1v3","2v3")
pAgree_Sig_Medium[1,1] = 10
pAgree_Sig_Medium[2,1] = 30
pAgree_Sig_Medium[3,1] = 100
pAgree_Sig_Medium[4,1] = 500
pAgree_Sig_Medium[5,1] = 1000

pAgree_Nonsig_Medium = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Nonsig_Medium) = c("N","Omnibus", "1v2","1v3","2v3")
pAgree_Nonsig_Medium[1,1] = 10
pAgree_Nonsig_Medium[2,1] = 30
pAgree_Nonsig_Medium[3,1] = 100
pAgree_Nonsig_Medium[4,1] = 500
pAgree_Nonsig_Medium[5,1] = 1000

Mediumdata = subset(data, stdev=="Medium")
library(stringr)
######## Overall
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Mediumdata, N==place)
  #make BF match
  overallmatch$tempoverallBF = overallmatch$BinoverallBF
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"weak","non-sig")
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"positive","marginal")
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$BinOmniP[round]==overallmatch$BinquadeP[round] &&
       overallmatch$BinOmniP[round]==overallmatch$Binoom[round] &&
       overallmatch$BinOmniP[round]==overallmatch$tempoverallBF[round] &&
       overallmatch$BinquadeP[round]==overallmatch$Binoom[round] &&
       overallmatch$BinquadeP[round]==overallmatch$tempoverallBF[round] &&
       overallmatch$Binoom[round]==overallmatch$tempoverallBF[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Medium[xrun,2] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Medium[xrun,2] = 0
  } else if(length(temp) > 1){
    pAgree_Medium[xrun,2] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Medium[xrun,2] = 0
    pAgree_Nonsig_Medium[xrun,2] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Medium[xrun,2] = x1_tab[3]
    pAgree_Nonsig_Medium[xrun,2] = x1_tab[1]
  }
}
######## Overall

########### 1v2
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Mediumdata, N==place)
  #make BF match
  overallmatch$tempBF1v2 = overallmatch$BinBF1v2
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"weak","non-sig")
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"positive","marginal")
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp1v2[round]==overallmatch$BinquadeP1v2[round] &&
       overallmatch$Binp1v2[round]==overallmatch$tempBF1v2[round] &&
       overallmatch$BinquadeP1v2[round]==overallmatch$tempBF1v2[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Medium[xrun,3] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Medium[xrun,3] = 0
  } else if(length(temp) > 1){
    pAgree_Medium[xrun,3] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Medium[xrun,3] = 0
    pAgree_Nonsig_Medium[xrun,3] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Medium[xrun,3] = x1_tab[3]
    pAgree_Nonsig_Medium[xrun,3] = x1_tab[1]
  }
}
########### 1v2

########### 1v3
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Mediumdata, N==place)
  #make BF match
  overallmatch$tempBF1v3 = overallmatch$BinBF1v3
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"weak","non-sig")
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"positive","marginal")
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp1v3[round]==overallmatch$BinquadeP1v3[round] &&
       overallmatch$Binp1v3[round]==overallmatch$tempBF1v3[round] &&
       overallmatch$BinquadeP1v3[round]==overallmatch$tempBF1v3[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Medium[xrun,4] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Medium[xrun,4] = 0
  } else if(length(temp) > 1){
    pAgree_Medium[xrun,4] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Medium[xrun,4] = 0
    pAgree_Nonsig_Medium[xrun,4] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Medium[xrun,4] = x1_tab[3]
    pAgree_Nonsig_Medium[xrun,4] = x1_tab[1]
  }
}
########### 1v3

########### 2v3
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Mediumdata, N==place)
  #make BF match
  overallmatch$tempBF2v3 = overallmatch$BinBF2v3
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"weak","non-sig")
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"positive","marginal")
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp2v3[round]==overallmatch$BinquadeP2v3[round] &&
       overallmatch$Binp2v3[round]==overallmatch$tempBF2v3[round] &&
       overallmatch$BinquadeP2v3[round]==overallmatch$tempBF2v3[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Medium[xrun,5] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Medium[xrun,5] = 0
  } else if(length(temp) > 1){
    pAgree_Medium[xrun,5] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Medium[xrun,5] = 0
    pAgree_Nonsig_Medium[xrun,5] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Medium[xrun,5] = x1_tab[3]
    pAgree_Nonsig_Medium[xrun,5] = x1_tab[1]
  }
}
########### 2v3
######################## percent 100% agree ES Medium



######################## percent 100% agree ES Small
pAgree_Small = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Small) = c("N","Omnibus", "1v2", "1v3","2v3")
pAgree_Small[1,1] = 10
pAgree_Small[2,1] = 30
pAgree_Small[3,1] = 100
pAgree_Small[4,1] = 500
pAgree_Small[5,1] = 1000

pAgree_Sig_Small = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Sig_Small) = c("N","Omnibus", "1v2","1v3","2v3")
pAgree_Sig_Small[1,1] = 10
pAgree_Sig_Small[2,1] = 30
pAgree_Sig_Small[3,1] = 100
pAgree_Sig_Small[4,1] = 500
pAgree_Sig_Small[5,1] = 1000

pAgree_Nonsig_Small = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Nonsig_Small) = c("N","Omnibus", "1v2","1v3","2v3")
pAgree_Nonsig_Small[1,1] = 10
pAgree_Nonsig_Small[2,1] = 30
pAgree_Nonsig_Small[3,1] = 100
pAgree_Nonsig_Small[4,1] = 500
pAgree_Nonsig_Small[5,1] = 1000

Smalldata = subset(data, stdev=="Small")
library(stringr)
######## Overall
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Smalldata, N==place)
  #make BF match
  overallmatch$tempoverallBF = overallmatch$BinoverallBF
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"weak","non-sig")
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"positive","marginal")
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$BinOmniP[round]==overallmatch$BinquadeP[round] &&
       overallmatch$BinOmniP[round]==overallmatch$Binoom[round] &&
       overallmatch$BinOmniP[round]==overallmatch$tempoverallBF[round] &&
       overallmatch$BinquadeP[round]==overallmatch$Binoom[round] &&
       overallmatch$BinquadeP[round]==overallmatch$tempoverallBF[round] &&
       overallmatch$Binoom[round]==overallmatch$tempoverallBF[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Small[xrun,2] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Small[xrun,2] = 0
  } else if(length(temp) > 1){
    pAgree_Small[xrun,2] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Small[xrun,2] = 0
    pAgree_Nonsig_Small[xrun,2] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Small[xrun,2] = x1_tab[3]
    pAgree_Nonsig_Small[xrun,2] = x1_tab[1]
  }
}
######## Overall

########### 1v2
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Smalldata, N==place)
  #make BF match
  overallmatch$tempBF1v2 = overallmatch$BinBF1v2
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"weak","non-sig")
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"positive","marginal")
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp1v2[round]==overallmatch$BinquadeP1v2[round] &&
       overallmatch$Binp1v2[round]==overallmatch$tempBF1v2[round] &&
       overallmatch$BinquadeP1v2[round]==overallmatch$tempBF1v2[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Small[xrun,3] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Small[xrun,3] = 0
  } else if(length(temp) > 1){
    pAgree_Small[xrun,3] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Small[xrun,3] = 0
    pAgree_Nonsig_Small[xrun,3] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Small[xrun,3] = x1_tab[3]
    pAgree_Nonsig_Small[xrun,3] = x1_tab[1]
  }
}
########### 1v2

########### 1v3
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Smalldata, N==place)
  #make BF match
  overallmatch$tempBF1v3 = overallmatch$BinBF1v3
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"weak","non-sig")
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"positive","marginal")
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp1v3[round]==overallmatch$BinquadeP1v3[round] &&
       overallmatch$Binp1v3[round]==overallmatch$tempBF1v3[round] &&
       overallmatch$BinquadeP1v3[round]==overallmatch$tempBF1v3[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Small[xrun,4] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Small[xrun,4] = 0
  } else if(length(temp) > 1){
    pAgree_Small[xrun,4] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Small[xrun,4] = 0
    pAgree_Nonsig_Small[xrun,4] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Small[xrun,4] = x1_tab[3]
    pAgree_Nonsig_Small[xrun,4] = x1_tab[1]
  }
}
########### 1v3

########### 2v3
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Smalldata, N==place)
  #make BF match
  overallmatch$tempBF2v3 = overallmatch$BinBF2v3
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"weak","non-sig")
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"positive","marginal")
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp2v3[round]==overallmatch$BinquadeP2v3[round] &&
       overallmatch$Binp2v3[round]==overallmatch$tempBF2v3[round] &&
       overallmatch$BinquadeP2v3[round]==overallmatch$tempBF2v3[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_Small[xrun,5] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_Small[xrun,5] = 0
  } else if(length(temp) > 1){
    pAgree_Small[xrun,5] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_Small[xrun,5] = 0
    pAgree_Nonsig_Small[xrun,5] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_Small[xrun,5] = x1_tab[3]
    pAgree_Nonsig_Small[xrun,5] = x1_tab[1]
  }
}
########### 2v3
######################## percent 100% agree ES Small



######################## percent 100% agree ES None
pAgree_None = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_None) = c("N","Omnibus", "1v2", "1v3","2v3")
pAgree_None[1,1] = 10
pAgree_None[2,1] = 30
pAgree_None[3,1] = 100
pAgree_None[4,1] = 500
pAgree_None[5,1] = 1000

pAgree_Sig_None = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Sig_None) = c("N","Omnibus", "1v2","1v3","2v3")
pAgree_Sig_None[1,1] = 10
pAgree_Sig_None[2,1] = 30
pAgree_Sig_None[3,1] = 100
pAgree_Sig_None[4,1] = 500
pAgree_Sig_None[5,1] = 1000

pAgree_Nonsig_None = as.data.frame(matrix(0, nrow = 5, ncol = 5))
names(pAgree_Nonsig_None) = c("N","Omnibus", "1v2","1v3","2v3")
pAgree_Nonsig_None[1,1] = 10
pAgree_Nonsig_None[2,1] = 30
pAgree_Nonsig_None[3,1] = 100
pAgree_Nonsig_None[4,1] = 500
pAgree_Nonsig_None[5,1] = 1000

Nonedata = subset(data, stdev=="None")
library(stringr)
######## Overall
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Nonedata, N==place)
  #make BF match
  overallmatch$tempoverallBF = overallmatch$BinoverallBF
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"weak","non-sig")
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"positive","marginal")
  overallmatch$tempoverallBF = str_replace_all(overallmatch$tempoverallBF,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$BinOmniP[round]==overallmatch$BinquadeP[round] &&
       overallmatch$BinOmniP[round]==overallmatch$Binoom[round] &&
       overallmatch$BinOmniP[round]==overallmatch$tempoverallBF[round] &&
       overallmatch$BinquadeP[round]==overallmatch$Binoom[round] &&
       overallmatch$BinquadeP[round]==overallmatch$tempoverallBF[round] &&
       overallmatch$Binoom[round]==overallmatch$tempoverallBF[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_None[xrun,2] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_None[xrun,2] = 0
  } else if(length(temp) > 1){
    pAgree_None[xrun,2] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_None[xrun,2] = 0
    pAgree_Nonsig_None[xrun,2] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_None[xrun,2] = x1_tab[3]
    pAgree_Nonsig_None[xrun,2] = x1_tab[1]
  }
}
######## Overall

########### 1v2
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Nonedata, N==place)
  #make BF match
  overallmatch$tempBF1v2 = overallmatch$BinBF1v2
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"weak","non-sig")
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"positive","marginal")
  overallmatch$tempBF1v2 = str_replace_all(overallmatch$tempBF1v2,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp1v2[round]==overallmatch$BinquadeP1v2[round] &&
       overallmatch$Binp1v2[round]==overallmatch$tempBF1v2[round] &&
       overallmatch$BinquadeP1v2[round]==overallmatch$tempBF1v2[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_None[xrun,3] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_None[xrun,3] = 0
  } else if(length(temp) > 1){
    pAgree_None[xrun,3] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_None[xrun,3] = 0
    pAgree_Nonsig_None[xrun,3] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_None[xrun,3] = x1_tab[3]
    pAgree_Nonsig_None[xrun,3] = x1_tab[1]
  }
}
########### 1v2

########### 1v3
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Nonedata, N==place)
  #make BF match
  overallmatch$tempBF1v3 = overallmatch$BinBF1v3
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"weak","non-sig")
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"positive","marginal")
  overallmatch$tempBF1v3 = str_replace_all(overallmatch$tempBF1v3,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp1v3[round]==overallmatch$BinquadeP1v3[round] &&
       overallmatch$Binp1v3[round]==overallmatch$tempBF1v3[round] &&
       overallmatch$BinquadeP1v3[round]==overallmatch$tempBF1v3[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_None[xrun,4] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_None[xrun,4] = 0
  } else if(length(temp) > 1){
    pAgree_None[xrun,4] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_None[xrun,4] = 0
    pAgree_Nonsig_None[xrun,4] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_None[xrun,4] = x1_tab[3]
    pAgree_Nonsig_None[xrun,4] = x1_tab[1]
  }
}
########### 1v3

########### 2v3
matchN = c(10,30,100,500,1000)
xrun=0
for(i in 1:length(matchN)){
  place = matchN[i]
  xrun=xrun+1
  overallmatch = subset(Nonedata, N==place)
  #make BF match
  overallmatch$tempBF2v3 = overallmatch$BinBF2v3
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"weak","non-sig")
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"positive","marginal")
  overallmatch$tempBF2v3 = str_replace_all(overallmatch$tempBF2v3,"strong","significant")
  nsim = 1000
  round=0
  overallmatch$percentAgreeOverall = "NA"
  for(j in 1:nsim){
    round=round+1
    if(overallmatch$Binp2v3[round]==overallmatch$BinquadeP2v3[round] &&
       overallmatch$Binp2v3[round]==overallmatch$tempBF2v3[round] &&
       overallmatch$BinquadeP2v3[round]==overallmatch$tempBF2v3[round]){
      
      overallmatch$percentAgreeOverall[round] = "Agree"
    } else{
      overallmatch$percentAgreeOverall[round] = "No"
    }
  }
  temp = (table(overallmatch$percentAgreeOverall)/sum(table(overallmatch$percentAgreeOverall)))*100
  if(length(temp) == 1 && names(temp[1]) == "Agree"){
    pAgree_None[xrun,5] = temp[1]
  } else if(length(temp) == 1 && names(temp[1]) != "Agree"){
    pAgree_None[xrun,5] = 0
  } else if(length(temp) > 1){
    pAgree_None[xrun,5] = temp[1]
  }
  #### sig/nonsig agree
  x1 = subset(overallmatch, percentAgreeOverall=="Agree")
  if(nrow(x1)==0){
    pAgree_Sig_None[xrun,5] = 0
    pAgree_Nonsig_None[xrun,5] = 0
  } else{
    x1$s_ns = x1$BinOmniP
    x1_tab = (table(x1$s_ns)/(sum(table(x1$s_ns))))*100
    pAgree_Sig_None[xrun,5] = x1_tab[3]
    pAgree_Nonsig_None[xrun,5] = x1_tab[1]
  }
}
########### 2v3
######################## percent 100% agree ES None 
################################################################## percent agree

pAgree_Large$Effect = "Large"
pAgree_Large = pAgree_Large[ , c(6,1,2,3,4,5)]
pAgree_Medium$Effect = "Medium"
pAgree_Medium = pAgree_Medium[ , c(6,1,2,3,4,5)]
pAgree_Small$Effect = "Small"
pAgree_Small = pAgree_Small[ , c(6,1,2,3,4,5)]
pAgree_None$Effect = "None"
pAgree_None = pAgree_None[ , c(6,1,2,3,4,5)]
Percent100agree = rbind(pAgree_Large, pAgree_Medium,
                        pAgree_Small, pAgree_None)

pAgree_Sig_Large$Effect = "Large"
pAgree_Sig_Large = pAgree_Sig_Large[ , c(6,1,2,3,4,5)]
pAgree_Sig_Medium$Effect = "Medium"
pAgree_Sig_Medium = pAgree_Sig_Medium[ , c(6,1,2,3,4,5)]
pAgree_Sig_Small$Effect = "Small"
pAgree_Sig_Small = pAgree_Sig_Small[ , c(6,1,2,3,4,5)]
pAgree_Sig_None$Effect = "None"
pAgree_Sig_None = pAgree_Sig_None[ , c(6,1,2,3,4,5)]
PercentOfAgreementIsSig = rbind(pAgree_Sig_Large, pAgree_Sig_Medium,
                                pAgree_Sig_Small, pAgree_Sig_None)

pAgree_Nonsig_Large$Effect = "Large"
pAgree_Nonsig_Large = pAgree_Nonsig_Large[ , c(6,1,2,3,4,5)]
pAgree_Nonsig_Medium$Effect = "Medium"
pAgree_Nonsig_Medium = pAgree_Nonsig_Medium[ , c(6,1,2,3,4,5)]
pAgree_Nonsig_Small$Effect = "Small"
pAgree_Nonsig_Small = pAgree_Nonsig_Small[ , c(6,1,2,3,4,5)]
pAgree_Nonsig_None$Effect = "None"
pAgree_Nonsig_None = pAgree_Nonsig_None[ , c(6,1,2,3,4,5)]
PercentOfAgreementIsNonSig = rbind(pAgree_Nonsig_Large, pAgree_Nonsig_Medium,
                                   pAgree_Nonsig_Small, pAgree_Nonsig_None)

#setwd("C:/Users/John/Desktop")
write.csv(Percent100agree, file = "Percent100Agree.005np.csv")
write.csv(PercentOfAgreementIsSig, file = "PercentIsSig.005np.csv")
write.csv(PercentOfAgreementIsNonSig, file = "PercentIsNonSig.005np.csv")




