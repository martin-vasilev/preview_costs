
rm(list= ls())

library(ggplot2)
library(lme4)
library(MASS)

# load data

load("D:/R/preview_costs/Experiment 3b/data/target.Rda")
load("D:/R/preview_costs/Experiment 3b/data/pre-target.Rda")


#-------------------
# Question Accuracy:
#-------------------

load("Experiment 2b/data/Quest2b.Rda")

Quest2b$deg<- as.factor(Quest2b$deg)
contrasts(Quest2b$deg)<- c(1,-1)
contrasts(Quest2b$deg)


cmat<- matrix(data = c(-1,1,0,0,1, -1), nrow=3, ncol=2,
              dimnames = list(c('valid', 'invalid', 'orth'),
                              c(".PB", ".Orth_PB")))

# need to use the generalised inverse to get the correct comparisons
inv.cmat<- fractions(t(ginv(cmat))) 

# copy row and column names over from cmat to make interpretation easier
colnames(inv.cmat) <- colnames(cmat)
rownames(inv.cmat)<- rownames(cmat)


Quest2b$prev<- as.factor(Quest2b$prev)
levels(Quest2b$prev)<- c("invalid", "orth", "valid") 
levels(Quest2b$prev)
Quest2b$prev<- factor(Quest2b$prev, levels= c("valid", "invalid", "orth"))
contrasts(Quest2b$prev)<- inv.cmat
contrasts(Quest2b$prev)

# Comprehension accuracy:
if(!file.exists("Experiment 2b/Models/G1.Rda")){
  
  G1<- glmer(accuracy ~  prev*deg+ (1|sub)+ (1|item), family= binomial, data= Quest2b)

  save(G1, file= "Experiment 2b/Models/G1.Rda")
  summary(G1)
  
}else{
  load("Experiment 2b/Models/G1.Rda")
  summary(G1)
}
max(abs(unname(coef(summary(G1))[2:6,3])))



# Contrast coding:
N1$deg<- as.factor(N1$deg)
contrasts(N1$deg)<- c(1,-1)
contrasts(N1$deg)

N1$prev<- as.factor(N1$prev)
levels(N1$prev)<- c("invalid", "orth", "valid")

N1$prev<- factor(N1$prev, levels= c("valid", "invalid", "orth"))
contrasts(N1$prev)


contrasts(N1$prev)<- inv.cmat
contrasts(N1$prev)

contrasts(N1$deg)

summary(LM1<- lmer(log(FFD)~ prev*deg +(deg+prev|sub) + (deg|item), data= N1))

summary(LM2<- lmer(log(SFD)~ prev*deg +(deg|sub) + (deg|item), data= N1))

summary(LM3<- lmer(log(GD)~ prev*deg +(deg|sub) + (deg|item), data= N1))


#####################################################################
# Global analyses:                                                  #
#####################################################################

library(reshape)
library(lme4)

#------------------------#
# sentence reading time: #
#------------------------#
load("Experiment 2b/data/Trial_time2b.Rda")

# descriptives
DesTime<- melt(Trialt2b, id=c('sub', 'item', 'deg'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, deg ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

Trialt2b$deg<- as.factor(Trialt2b$deg)
contrasts(Trialt2b$deg)<- c(1, -1)
contrasts(Trialt2b$deg)


if(!file.exists("Experiment 2b/Models/GL1.Rda")){
  GL1<- lmer(log(duration_ms) ~ deg +(deg|sub)+ (1|item), data =Trialt2b, REML = T)
  summary(GL1)
  save(GL1, file= 'Experiment 2b/Models/GL1.Rda')
}else{
  load('Experiment 2b/Models/GL1.Rda')
}


#--------------------------#
# Fixation duration (all): #
#--------------------------#
load("Experiment 2b/data/raw_fix2b.Rda")

# descriptives
DesFix<- melt(rf2b, id=c('sub', 'item', 'deg'), 
               measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, deg ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

rf2b$deg<- as.factor(rf2b$deg)
contrasts(rf2b$deg)<- c(1, -1)
contrasts(rf2b$deg)


if(!file.exists("Experiment 2b/Models/GL2.Rda")){
  GL2<- lmer(log(fix_dur) ~ deg +(deg|sub)+ (1|item), data =rf2b, REML = T)
  summary(GL2)
  save(GL2, file= 'Experiment 2b/Models/GL2.Rda')
}else{
  load('Experiment 2b/Models/GL2.Rda')
}


#------------------#
# Saccade length : #
#------------------#

# descriptives
DesLen<- melt(rf2b, id=c('sub', 'item', 'deg'), 
              measure=c("sacc_len"), na.rm=TRUE)
mLen<- cast(DesLen, deg ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))


if(!file.exists("Experiment 2b/Models/GL3.Rda")){
  GL3<- lmer(sacc_len ~ deg +(1|sub)+ (1|item), data =rf2b, REML = T)
  summary(GL3)
  save(GL3, file= 'Experiment 2b/Models/GL3.Rda')
}else{
  load('Experiment 2b/Models/GL3.Rda')
}


#--------------------------#
# number of fixations:     #
#--------------------------#
load("Experiment 2b/data/num_fix2b.Rda")

# descriptives
DesNFix<- melt(nFix2b, id=c('sub', 'item', 'deg'), 
              measure=c("Nfix_all"), na.rm=TRUE)
mNFix<- cast(DesNFix, deg ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

nFix2b$deg<- as.factor(nFix2b$deg)
contrasts(nFix2b$deg)<- c(1, -1)
contrasts(nFix2b$deg)


if(!file.exists("Experiment 2b/Models/GL4.Rda")){
  GL4<- lmer(Nfix_all ~ deg +(deg|sub)+ (1|item), data =nFix2b, REML = T)
  summary(GL4)
  save(GL4, file= 'Experiment 2b/Models/GL4.Rda')
}else{
  load('Experiment 2b/Models/GL4.Rda')
}
