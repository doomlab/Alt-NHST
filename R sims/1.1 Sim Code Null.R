
ptm = proc.time()

####cohen's d dependent t differences####
d.deptdiff = function (mdiff = 0, sddiff = 1, n = 10, a = .05) {
  d = mdiff / sddiff
  se = sddiff / sqrt(n)
  t = mdiff / se
  ncpboth = conf.limits.nct(t, (n-1), conf.level = (1-a))
  dlow = ncpboth$Lower.Limit / sqrt(n)
  dhigh = ncpboth$Upper.Limit / sqrt(n)
  Mlow = mdiff - se*qt(a/2, n-1, lower.tail = FALSE)
  Mhigh = mdiff + se*qt(a/2, n-1, lower.tail = FALSE)
  p = pt(abs(t), n-1, lower.tail = F)*2
  
  output = list("d" = d, 
                "dlow" = dlow, 
                "dhigh" = dhigh, 
                "Mlow" = Mlow, 
                "Mhigh" = Mhigh)
  return(output)
  
}

###oom function####
OOM <- function(x, y, xlev = NULL, ties.method = "first", ambiguous.error=FALSE) {
  # Computes main OOM results; this is the central function
  #
  # Args:
  # x: the vector to be conformed to the target vector,
  #    ie, consisting of observed data, the "as-is" pattern,
  #    can be integer, numeric, or factor (max. 30 unique values)
  # y: the target vector consisting of the expected ("true") pattern
  #    can be integer, numeric, or factor (max. 30 unique values)
  # xlev: levels of the x variable
  # ties.method: for breaking ties when conforming matrices. Allowed value:
  #              "random": breaks ties randomly to one class
  #               "first": break ties to most frequent class
  #               "last": allowed, but not recommended
  # ambiguous.error: Shall ambiguous observations be classified as error?
  #
  # Returns:
  # Most important OOM results: PCC, conforming matrix, vector of predicted
  #                             classes
  # Error handling
  allowed_ties_methods <- c("random", "first", "last")
  if (!(ties.method %in% allowed_ties_methods)) stop("unknown ties.method.\n")
  sample_size <- length(x)
  # Compute Deep Structure matrices
  Xds <- ds(x, lev = xlev)
  Yds <- ds(y)
  # init vector for classification results (correct/false/ambiguous) for each
  # observation (case)
  classification <- vector(length = sample_size)
  # Frequencies of events (where indicator variable equals 1)
  Ybaseline <- colSums(Yds)
  Xbaseline <- colSums(Xds)
  # Sort Deep Structure of y according to a priori if ties.method="first",
  # i.e. choose in case of ambiguity most frequent class
  if (ties.method == "first") Yds <- Yds[, order(Ybaseline, decreasing = TRUE)]
  # Normalizing for Pseudoinverse
  iXbase <- 1 / Xbaseline
  # Correction for events with 0 frequency
  iXbase[Xbaseline == 0] <- 0
  # Projection (analog to Grice's "Binary Procustres Rotation Matrix")
  # P is the normalized matrix
  P <- diag(iXbase) %*% t(Xds) %*% Yds
  rownames(P) <- colnames(Xds)
  # Conforming Matrix
  Yconf <- Xds %*% P
  # Identify ambigious cases
  yamb <- apply(Yconf, 1, nomax)
  Amb <- which(yamb >= 2)
  # Conform the events, i.e., create vector of precicted classes
  yconf <- factor(colnames(Yds)[max.col(Yconf, ties.method = ties.method)],
                  levels = colnames(Yds))
  # True Events
  ytrue <- factor(colnames(Yds)[max.col(Yds)], levels = colnames(Yds))
  
  x <- factor(colnames(Xds)[max.col(Xds)], levels = colnames(Xds))
  # Calculation of PCC (accuracy)
  PCC <- mean(yconf == ytrue)
  # Calculation of PCC (accuracy), ambiguous
  yconfa <-  colnames(Yds)[max.col(Yconf, ties.method = ties.method)]
  yconfa[Amb] <- "ambiguous"
  yconfa <- factor(yconfa, levels = c(colnames(Yds), "ambiguous"))
  levels(ytrue) <- c(levels(ytrue), "ambiguous")  # same levels for both vectors
  PCCAmb <- mean(yconfa == ytrue)
  ytrue <- droplevels(ytrue)
  
  # Override PCC if   ambiguous.error=TRUE
  if (ambiguous.error) PCC <- PCCAmb
  # Conforming strength
  CSI <- apply(Yconf, 1, max)
  CSI_mean <- mean(CSI, na.rm = T)
  # Baseline PCC
  PCCbase <- max(Ybaseline)/sum(Ybaseline)
  # Conforming Table
  Conforming_matrix <- table(x, yconf)
  ambiguity <- table(x[yamb > 1])
  Conforming_matrix <- cbind(Conforming_matrix)
  # What to return
  return(structure(list(Xds = Xds, Yds = Yds, P = P, Yconf = Yconf, yconf = yconf,
                        PCC = PCC, PCCbase = PCCbase, PCCAmb=PCCAmb, CSI = CSI,
                        CSI_mean = CSI_mean,
                        Amb = Amb, Conforming_matrix = Conforming_matrix,
                        ytrue = ytrue,
                        x = x, y = y,
                        sample_size = sample_size, ties.method = ties.method, 
                        ambiguous.error=ambiguous.error),
                   class = "OOM"))
}
# Compute number of maxima 
nomax <- function(x) {
  # Helper function: calculation of number of maxima
  # Arg:
  # vector (numeric)
  #
  # Returns:
  # Number of maxima (scalar)
  mx <- max(x)
  nmx <- sum(x == mx)
  return(nmx)
}

# Compute "deep structure" matrices, ie., indicator matrices
ds <- function(x, lev = NULL)
{
  # Convert Matrix to Deep Structure (ds), i.e. Indicator Matrix of Events
  # where x  = raw matrix to be converted to deep structure; lev = names of levels
  if (any(is.na(x)))    stop("Currently NA not supported")
  if (!is.factor(x))
  {
    if (is.null(lev))
    {
      if (is.numeric(x))
      {
        if (typeof(x) != "integer")
          stop("Currently only discrete values of x supported ",
               "(integer or factor). Consider binning,",
               "Ranking, or conversion to integer.")
        lev <- c(min(x):max(x))
      } else lev <- unique(x) # in case of numeric values (not integer)
    }
    x <- factor(x, levels = lev)
  }
  if (length(lev) >= 30)
    warning("Number of distinct events >= 30. Consider Binning?")
  xds <- model.matrix(~x - 1)
  attr(xds, "assign") <- NULL
  attr(xds, "contrasts") <- NULL
  colnames(xds) <- levels(x)
  return(xds)
}
# predict method for OOM
predict.OOM <- function(oom, newdata)
{
  # predict method for OOM
  if (!missing(newdata))
    Xds <- ds(newdata, lev = colnames(oom$Xds)) else Xds <- oom$Xds
    posterior <- Xds %*% oom$P
    
    class <- factor(colnames(posterior)[max.col(posterior, ties.method = oom$ties.method)],
                    levels = colnames(posterior))
    
    # Handling of ambiguous observation
    if (oom$ambiguous.error)
    {
      yamb <- apply(posterior, 1, nomax)
      Amb <- which(yamb >= 2)
      levels(class) <- c(levels(class), "ambiguous")
      class[Amb] <- "ambiguous"
    }
    
    return(list(posterior = posterior, class = class))
}
# print method for OOM
print.OOM <- function(oom)
{
  # print method for oom
  cat("Observation Oriented Modelling \n")
  cat("Conforming Table :\n")
  print(oom$Conforming_matrix)
  cat("PCC Value: ", oom$PCC, "\n")
  cat("PCC Baseline : ", oom$PCCbase, "\n")
}
# summary method for OOM
summary.OOM <- function(oom)
{
  # summary method for oom
  cat("Observation Oriented Modeling \n\n")
  cat("Conforming Table :\n")
  print(oom$Conforming_matrix)
  cat("\n")
  cat("Confusion Matrix :\n")
  ytrue <- factor(colnames(oom$Yds)[max.col(oom$Yds)], levels = colnames(oom$Yds))
  print(table(ytrue, oom$yconf))
  cat("PCC Value: ", oom$PCC, "\n")
  cat("PCC Baseline : ", oom$PCCbase, "\n")
  cat("PCC Ambiguous as Error: ", oom$PCCAmb, "\n")
  cat("OOM Accuracy Gain : ", (oom$PCC/oom$PCCbase), "\n\n")
  cat("Projection Matrix: \n")
  print(oom$P)
}
# compute chance (c) value for OOM 
OOMc <- function(oom, B = 1000, alpha = 0.95)
{
  # Calculation of c-Value
  
  # Observed PCC
  PCCt = oom$PCC
  
  # Vector of permutated data PCC
  PCCp <- length(B)
  
  n <- nrow(oom$Xds)
  ap <- (1 - alpha)/2
  Yds <- oom$Yds
  ytrue <- max.col(Yds)
  
  for (i in 1:B)
  {
    # Permutation sample (x)
    bs <- sample(1:n)
    # OOM Analysis
    Xbs <- oom$Xds[bs, ]
    Xbaseline <- colSums(Xbs)
    iXbase <- 1/Xbaseline
    iXbase[Xbaseline == 0] <- 0
    yconf <- (Xbs %*% diag(iXbase) %*% t(Xbs) %*% Yds)
    yhat <- max.col(yconf, ties.method = oom$ties.method)
    
    # Handling of ambiguous observation
    if (oom$ambiguous.error)
    {
      yamb <- apply(yconf, 1, nomax)
      Amb <- which(yamb >= 2)
      yhat[Amb] <- 0
    }                   
    
    PCCp[i] <- mean(yhat == ytrue)
  }
  
  PCCps <- sort(PCCp)
  
  # c Value
  cvalue <- mean(PCCp >= PCCt)
  #cat("PCC : ", PCCt, "\n")
  #cat("Number of Permutations: ", B, "\n")
  #cat("c Value: ", cvalue, "\n\n")
  
  #cat("PCC Permutation Interval, alpha=", alpha, "\n")
  #cat("Lower Bound: ", PCCps[floor(B * ap)], ", Upper Bound: ",
  #    PCCps[ceiling(B *(1 - ap))], "\n")
  return(list(PCCt = oom$PCC, PCCp = PCCp, B = B, chancevalue = cvalue))
}

# perform cross validation (cv) for OOM results
OOMcv <- function(oom)
{
  # Leave-one-out Cross Validation for oom
  y <- factor(colnames(oom$Yds)[max.col(oom$Yds)], levels = colnames(oom$Yds))
  x <- factor(colnames(oom$Xds)[max.col(oom$Xds)], levels = colnames(oom$Xds))
  xlev <- levels(x)
  n <- length(x)
  PCCcv <- numeric(n)
  yt <- y
  
  # Handling of ambiguous observation
  if (oom$ambiguous.error) levels(yt) <- c(levels(yt), "ambiguous")  
  # Looping
  for (i in 1:n)
  {
    ommi <- OOM(x[-i], y[-i], xlev = xlev, ties.method=oom$ties.method, ambiguous.error=oom$ambiguous.error)
    pommi <- predict(ommi, x[i])
    PCCcv[i] <- (pommi$class == yt[i])
  }
  
  cat("PCC Value: ", oom$PCC, "\n")
  cat("PCC Baseline : ", oom$PCCbase, "\n")
  cat("PCC Cross Validated: ", mean(PCCcv), "\n")
  return(list(PCCcv = mean(PCCcv), PCCcvv = PCCcv))
}
####end oom stuff

####simulations for data####
library(mvtnorm)
library(data.table)
library(ez)
library(reshape)
library(PMCMR)
library(BayesFactor)
library(MBESS)
##rmvnorm(n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)),
##        method=c("eigen", "svd", "chol"), pre0.9_9994 = FALSE)

##n = number of observations, we will want to rotate through
##10, 30, 100, 500, 1000

##sigma = covariance column, we will want to rotate through SDs
##6 = no effect size
##3 = small effect size
##.5 = medium effect size
##.10 = large effect size

##code should cycle through N and SD
##creates fake dataset
##rounds and windsorizes

##run rm anova
##run quade's test

##OMNIBUS - calculate percent p (3 decimals) that fall into .050, .051 - .100, .100 and up
##compare 1 to 2 and 2 to 3, ignore 1 to 3 
##calculate average M diff, CI of M diff, d, CI for d
##p values for each pairwise comparison
##calculate number p value bins

##bayes
##calculate mean difference and HDI of the difference
##calculate number of HDI bins using percent 

nsims = 1000
totrows = nsims * 20
####loop should start here####
M = c(3.0, 3.0, 3.0)
mydata = data.table(sim = 1:totrows, N = 1:totrows, 
                    effect1v2 = 1:totrows, effect2v3 = 1:totrows, 
                    effect1v3 = 1:totrows, stdev = 1:totrows,
                    dlow1v2 = 1:totrows, dhigh1v2 = 1:totrows,
                    dlow1v3 = 1:totrows, dhigh1v3 = 1:totrows,
                    dlow2v3 = 1:totrows, dhigh2v3 = 1:totrows,
                    mdiff1v2 = 1:totrows, mdiff1v3 = 1:totrows,
                    mdiff2v3 = 1:totrows, m1 = 1:totrows,
                    m2 = 1:totrows, m3 = 1:totrows,
                    mlow1v2 = 1:totrows, mhigh1v2 = 1:totrows,
                    mlow1v3 = 1:totrows, mhigh1v3 = 1:totrows,
                    mlow2v3 = 1:totrows, mhigh2v3 = 1:totrows,
                    omniP = 1:totrows, p1v2 = 1:totrows,
                    p2v3 = 1:totrows, p1v3 = 1:totrows, quadeP = 1:totrows,
                    quadeP1v2 = 1:totrows, quadeP2v3 = 1:totrows,
                    quadeP1v3 = 1:totrows, overallBF = 1:totrows, BF1v2 = 1:totrows,
                    BF1v3 = 1:totrows, BF2v3 = 1:totrows, oompcc = 1:totrows, 
                    oomchance = 1:totrows)
round = 0

####sd loop here####
##sdplaceholder = c(11.5, 3, .5, .10)
sdplaceholder = c(.5, .10)
nplaceholder = c(10, 30, 100, 500, 1000)
for (i in 1:length(sdplaceholder)) {
  
  ####n loop here####  
  for (r in 1:length(nplaceholder)) { 
    
    
    ####sims loop here####
    for (q in 1:nsims) { 
      
      ####make the data here####
      sigma = matrix(c(sdplaceholder[i],0,0,0,sdplaceholder[i],0,0,0,sdplaceholder[i]), nrow = 3, ncol = 3)
      
      dataset = as.data.table(rmvnorm(nplaceholder[r], M, sigma))
      dataset = round(dataset, digits = 0)
      dataset[ dataset < 1] = 1
      dataset[ dataset > 7] = 7
      
      ####make sure SDdiff is not zero####
      while (sd(dataset$V1 - dataset$V2) == 0 | 
             sd(dataset$V1 - dataset$V3) == 0 | 
             sd(dataset$V2 - dataset$V3) == 0)
      {
        dataset = as.data.table(rmvnorm(nplaceholder[r], M, sigma))
        dataset = round(dataset, digits = 0)
        dataset[ dataset < 1] = 1
        dataset[ dataset > 7] = 7
        
        if (sd(dataset$V1 - dataset$V2) != 0 &&
            sd(dataset$V1 - dataset$V3) != 0 &&
            sd(dataset$V2 - dataset$V3) != 0) break 
        
      }
      
      ####put in the basic statistics here####
      round = round + 1
      mydata$sim[round] = q
      mydata$N[round] = nplaceholder[r]
      mydata$stdev[round] = sdplaceholder[i]
      mydata$m1[round] = mean(dataset$V1)
      mydata$m2[round] = mean(dataset$V2)
      mydata$m3[round] = mean(dataset$V3)
      
      ####effect size calculations + Mdiff/CI####
      onetotwo = d.deptdiff(mdiff = mean(dataset$V1 - dataset$V2), 
                            sddiff = sd(dataset$V1 - dataset$V2), 
                            n = length(dataset$V1))
      onetothree = d.deptdiff(mdiff = mean(dataset$V1 - dataset$V3), 
                              sddiff = sd(dataset$V1 - dataset$V3), 
                              n = length(dataset$V1))
      twotothree = d.deptdiff(mdiff = mean(dataset$V2 - dataset$V3), 
                              sddiff = sd(dataset$V2 - dataset$V3), 
                              n = length(dataset$V1))
      mydata$effect1v2[round] = onetotwo$d
      mydata$dlow1v2[round] = onetotwo$dlow
      mydata$dhigh1v2[round] = onetotwo$dhigh
      mydata$mdiff1v2[round] = mean(dataset$V1 - dataset$V2)
      mydata$mlow1v2[round] = onetotwo$Mlow
      mydata$mhigh1v2[round] = onetotwo$Mhigh
      
      mydata$effect1v3[round] = onetothree$d
      mydata$dlow1v3[round] = onetothree$dlow
      mydata$dhigh1v3[round] = onetothree$dhigh
      mydata$mdiff1v3[round] = mean(dataset$V1 - dataset$V3)
      mydata$mlow1v3[round] = onetothree$Mlow
      mydata$mhigh1v3[round] = onetothree$Mhigh
      
      mydata$effect2v3[round] = twotothree$d
      mydata$dlow2v3[round] = twotothree$dlow
      mydata$dhigh2v3[round] = twotothree$dhigh
      mydata$mdiff2v3[round] = mean(dataset$V2 - dataset$V3)
      mydata$mlow2v3[round] = twotothree$Mlow
      mydata$mhigh2v3[round] = twotothree$Mhigh
      
      
      ###########################begin RM ANOVA####
      dataset2 = as.data.table(dataset)
      dataset2$partno = as.factor(1:nrow(dataset2))
      longdataset = as.data.table(melt(dataset2,
                                       id = "partno",
                                       measured = c("V1", "V2", "V3")))
      rmoutput = ezANOVA(data = longdataset,
                         wid = partno,
                         within = variable,
                         dv = value,
                         type = 3)
      mydata$omniP[round] = rmoutput$ANOVA$p
      posthocoutput = pairwise.t.test(longdataset$value,
                                      longdataset$variable,
                                      paired = T,
                                      p.adjust.method = "bonferroni")
      mydata$p1v2[round] = posthocoutput$p.value[1,1]
      mydata$p1v3[round] = posthocoutput$p.value[2,1]
      mydata$p2v3[round] = posthocoutput$p.value[2,2]
      ############################End RM ANOVA###
      
      #########################begin Quade####
      Quade = quade.test(value~variable|partno, data = longdataset)
      mydata$quadeP[round] = Quade$p.value
      
      posthocquadeoutput = posthoc.quade.test(longdataset$value,
                                              groups = longdataset$variable,
                                              blocks = longdataset$partno,
                                              p.adjust.method = "bonferroni")
      mydata$quadeP1v2[round] = posthocquadeoutput$p.value[1,1]
      mydata$quadeP1v3[round] = posthocquadeoutput$p.value[2,1]
      mydata$quadeP2v3[round] = posthocquadeoutput$p.value[2,2]
      ###########################end quade###
      
      ########################begin Bayes####
      ##anova uses Jeffries Prior of 1/2 for fixed effects, 1 for nuisance
      BFoutput = anovaBF(value~variable+partno, data = longdataset,
                         whichRandom = "partno",
                         rscaleFixed = 0.5, rscaleRandom = 1,
                         iterations = 1000)
      ##t tests uses a Cauchy Prior of sqrt(2)/2
      BFttest1v2 = ttestBF(x = longdataset$value[longdataset$variable=="V1"],
                           y = longdataset$value[longdataset$variable=="V2"],
                           paired = T, rscale = 0.707, iterations = 1000)
      BFttest1v3 = ttestBF(x = longdataset$value[longdataset$variable=="V1"],
                           y = longdataset$value[longdataset$variable=="V3"],
                           paired = T, rscale = 0.707, iterations = 1000)
      BFttest2v3 = ttestBF(x = longdataset$value[longdataset$variable=="V2"],
                           y = longdataset$value[longdataset$variable=="V3"],
                           paired = T, rscale = 0.707, iterations = 1000)
      mydata$overallBF[round] = unname(as.vector(BFoutput))
      mydata$BF1v2[round] = unname(as.vector(BFttest1v2))
      mydata$BF1v3[round] = unname(as.vector(BFttest1v3))
      mydata$BF2v3[round] = unname(as.vector(BFttest2v3))
      #######################end bayes###
      
      ###########################begin OOM####
      ##1 --> 2 --> 3 ordering
      longdataset$value = as.integer(longdataset$value)
      oom1 = OOM(longdataset$value, longdataset$variable)
      oom1$PCC #percentage correct classification
      oom1c <- OOMc(oom1)
      mydata$oompcc[round] = oom1c$PCCt #PCC
      mydata$oomchance[round] = oom1c$chancevalue #chance value
      ############################end OOM###
      
        } ##sim loop
      
    filename = paste(round, ".csv", sep = "")
    datalines = (abs(round-999):round)
    write.csv(mydata[ datalines, ], file = filename)
    
       } ##n loop
      
    } ##sd loop
    
    proc.time() - ptm
