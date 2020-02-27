
# Martin R. Vasilev, 2019

# statistical comparison of Experiment 1 and Experiment 2

rm(list=ls())

# load Experiment 1 data:
load('Experiment 1/data/data.Rda')
datE1<- data; rm(data)
datE1$Experiment<- 1

# load Experiment 2 data:
load('Experiment 2/data/data.Rda')
datE2<- data; rm(data)
datE2$bnd<- NULL
datE2$Experiment<- 2

dat<- rbind(datE1, datE2)

library(lme4)

dat$Experiment<- as.factor(dat$Experiment)
contrasts(dat$Experiment)<- c(-1, 1)

dat$prev<- as.factor(dat$prev)
dat$prev<- factor(dat$prev, levels= c("valid", "invalid", "phon", "orth"))
dat$deg<- as.factor(dat$deg)

contrasts(dat$deg)<- c(1,-1)

cmat<- matrix(data = c(-1,1,0,0,0,1,0,-1,0,0,-1,1), nrow=4, ncol=3,
              dimnames = list(c('valid', 'invalid', 'phon', 'orth'),
                              c(".PB", ".Orth_PB", ".Phon_PB")))

library(MASS)
# need to use the generalised inverse to get the correct comparisons
inv.cmat<- fractions(t(ginv(cmat))) 

# copy row and column names over from cmat to make interpretation easier
colnames(inv.cmat) <- colnames(cmat)
rownames(inv.cmat)<- rownames(cmat)
contrasts(dat$prev)<- inv.cmat

# let's check contrast coding
contrasts(dat$Experiment)
contrasts(dat$prev)
contrasts(dat$deg)

## FFD:
if(!file.exists('Experiment 2/Models/Comb_FFD.Rda')){
  summary(Comb_FFD<- lmer(log(FFD_N1)~prev*deg*Experiment+ (deg|subj)+ (deg|item), REML = T, data=dat))
  save(Comb_FFD, file= 'Experiment 2/Models/Comb_FFD.Rda')
}else{
  load('Experiment 2/Models/Comb_FFD.Rda')
}
summary(Comb_FFD)

SFFD<- round(coef(summary(Comb_FFD)),3)
write.csv(SFFD, "Experiment 2/Models/2exp_FFD.csv")


## SFD:
if(!file.exists('Experiment 2/Models/Comb_SFD.Rda')){
  summary(Comb_SFD<- lmer(log(SFD_N1)~prev*deg*Experiment+ (deg|subj)+ (1|item), REML = T, data=dat))
  save(Comb_SFD, file= 'Experiment 2/Models/Comb_SFD.Rda')
}else{
  load('Experiment 2/Models/Comb_SFD.Rda')
}
summary(Comb_SFD)

SSFD<- round(coef(summary(Comb_SFD)),3)
write.csv(SSFD, "Experiment 2/Models/2exp_SFD.csv")

## GD:
if(!file.exists('Experiment 2/Models/Comb_GD.Rda')){
  summary(Comb_GD<- lmer(log(GD_N1)~prev*deg*Experiment+ (deg|subj)+ (deg|item), REML = T, data=dat))
  save(Comb_GD, file= 'Experiment 2/Models/Comb_GD.Rda')
}else{
  load('Experiment 2/Models/Comb_GD.Rda')
}
summary(Comb_GD)

SGD<- round(coef(summary(Comb_GD)),3)
write.csv(SGD, "Experiment 2/Models/2exp_GD.csv")


library(effects)

plot(effect('deg', Comb_SFD))
plot(effect('Experiment', Comb_SFD))
plot(effect('deg:Experiment', Comb_SFD))
plot(effect('prev:Experiment', Comb_SFD))
