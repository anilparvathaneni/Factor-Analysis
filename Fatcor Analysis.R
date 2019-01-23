
setwd("C:/Users/ANIL PARVATHANENI/Desktop/MKTA")

#install.packages("readxl")
library(readxl)
data = read_excel("Exercise - Dimensionality Reduction.xlsx", 2)

any(is.na(data[-1]))
data = data[-1]
head(data)

# Determine Number of Factors to Extract
#install.packages("nFactors")
library(nFactors)
ev <- eigen(cor(data)) # get eigenvalues
ap <- parallel(subject=nrow(data),var=ncol(data),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

#install.packages("psych")
library(psych)
corrmat1<- cor(data, method = "pearson")
cortest.bartlett(corrmat1, n = dim(data)[1])

#install.packages("GPArotation")
library(GPArotation)

#install.packages("psych")
library(psych)

fa2 <- fa(corrmat1, nfactors = 5, n.obs = nrow(data),  fm = "pa", max.iter = 200, rotate = "none")

fa2$values
fa2$loadings

fa2v <- fa(corrmat1, nfactors = 5, fm = "pa", max.iter = 200, rotate = "varimax")

fa2v$loadings
fa2v$communality
