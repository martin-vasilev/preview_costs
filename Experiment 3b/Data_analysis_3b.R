
rm(list= ls())

library(ggplot2)
library(lme4)
library(MASS)

# load data

load("D:/R/preview_costs/Experiment 3b/data/target.Rda")
load("D:/R/preview_costs/Experiment 3b/data/pre-target.Rda")


# Contrast coding:
N1$deg<- as.factor(N1$deg)
contrasts(N1$deg)<- c(1,-1)
contrasts(N1$deg)

N1$prev<- as.factor(N1$prev)
levels(N1$prev)<- c("invalid", "orth", "valid")

N1$prev<- factor(N1$prev, levels= c("valid", "invalid", "orth"))
contrasts(N1$prev)

cmat<- matrix(data = c(-1,1,0,0,1, -1), nrow=3, ncol=2,
              dimnames = list(c('valid', 'invalid', 'orth'),
                              c(".PB", ".Orth_PB")))

# need to use the generalised inverse to get the correct comparisons
inv.cmat<- fractions(t(ginv(cmat))) 

# copy row and column names over from cmat to make interpretation easier
colnames(inv.cmat) <- colnames(cmat)
rownames(inv.cmat)<- rownames(cmat)
contrasts(N1$prev)<- inv.cmat
contrasts(N1$prev)

contrasts(N1$deg)

summary(LM1<- lmer(log(FFD)~ prev*deg +(deg+prev|sub) + (deg|item), data= N1))

summary(LM2<- lmer(log(SFD)~ prev*deg +(deg|sub) + (deg|item), data= N1))

summary(LM3<- lmer(log(GD)~ prev*deg +(deg|sub) + (deg|item), data= N1))





