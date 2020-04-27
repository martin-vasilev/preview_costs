
# Martin R. Vasilev, 2020

# Statistical comparison of Experiment 2a and Experiment 2b

rm(list=ls())

# load Experiment 1 data:
load('Experiment 2a/data/target.Rda')
datE1<- N1_2a; rm(N1_2a)
datE1$Experiment<- 1

# load Experiment 2 data:
load('Experiment 2b/data/target.Rda')
datE2<- N1_2b; rm(N1_2b)
datE2$Experiment<- 2

dat<- rbind(datE1, datE2)

library(lme4)
library(MASS)

cmat<- matrix(data = c(-1,1,0,0,1, -1), nrow=3, ncol=2,
              dimnames = list(c('valid', 'invalid', 'orth'),
                              c(".PB", ".Orth_PB")))

# need to use the generalised inverse to get the correct comparisons
inv.cmat<- fractions(t(ginv(cmat))) 

# copy row and column names over from cmat to make interpretation easier
colnames(inv.cmat) <- colnames(cmat)
rownames(inv.cmat)<- rownames(cmat)


dat$Experiment<- as.factor(dat$Experiment)
contrasts(dat$Experiment)<- c(-1, 1)

dat$prev<- as.factor(dat$prev)
levels(dat$prev)<- c("invalid", "orth", "valid")

dat$prev<- factor(dat$prev, levels= c("valid", "invalid", "orth"))
contrasts(dat$prev)

contrasts(dat$prev)<- inv.cmat

dat$deg<- as.factor(dat$deg)
contrasts(dat$deg)<- c(1,-1)

# let's check contrast coding
contrasts(dat$Experiment)
contrasts(dat$prev)
contrasts(dat$deg)

## FFD:
if(!file.exists('Experiment 2b/Models/Comb_FFD.Rda')){
  summary(Comb_FFD<- lmer(log(FFD)~prev*deg*Experiment+ (1|sub)+ (deg|item), REML = T, data=dat))
  save(Comb_FFD, file= 'Experiment 2b/Models/Comb_FFD.Rda')
}else{
  load('Experiment 2b/Models/Comb_FFD.Rda')
}
summary(Comb_FFD)

SFFD<- round(coef(summary(Comb_FFD)),2)
write.csv(SFFD, "Experiment 2b/Models/2exp_FFD.csv")


## SFD:
if(!file.exists('Experiment 2b/Models/Comb_SFD.Rda')){
  summary(Comb_SFD<- lmer(log(SFD)~prev*deg*Experiment+ (deg|sub)+ (1|item), REML = T, data=dat))
  save(Comb_SFD, file= 'Experiment 2b/Models/Comb_SFD.Rda')
}else{
  load('Experiment 2b/Models/Comb_SFD.Rda')
}
summary(Comb_SFD)

SSFD<- round(coef(summary(Comb_SFD)),2)
write.csv(SSFD, "Experiment 2b/Models/2exp_SFD.csv")

## GD:
if(!file.exists('Experiment 2b/Models/Comb_GD.Rda')){
  summary(Comb_GD<- lmer(log(GD)~prev*deg*Experiment+ (deg|sub)+ (1|item), REML = T, data=dat))
  save(Comb_GD, file= 'Experiment 2b/Models/Comb_GD.Rda')
}else{
  load('Experiment 2b/Models/Comb_GD.Rda')
}
summary(Comb_GD)

SGD<- round(coef(summary(Comb_GD)),2)
write.csv(SGD, "Experiment 2b/Models/2exp_GD.csv")


library(effects)

plot(effect('deg', Comb_SFD))
plot(effect('Experiment', Comb_SFD))
plot(effect('deg:Experiment', Comb_SFD))
plot(effect('prev:Experiment', Comb_SFD))
