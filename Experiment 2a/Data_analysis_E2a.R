
# Martin R. Vasilev, 2018

# EXPERIMENT 2a:

rm(list=ls())


### MEANING OF DATA COLUMNS:

# "sub"=          Subject number
# "item"=         Item number
# "cond"=         Condition number
# "seq" =         Trial sequence in the experiment
# "word"=         Word number in the sentence
# "WordID"        The target word string
# "FFD"=          First fixation duration
# "SFD"=          Single fixation duration
# "GD"=           Gaze duration
# "TVT"=          Total viewing time
# "nfix1"=        Number of 1st-pass fixations
# "nfix2"=        Number of 2nd-pass fixations
# "nfixAll"=      Number of all fixations
# regress=        A logica indicating whether there were regressive (2nd-pass) fixations (1= yes; 0= no)
# "skip_1st"=     First-pass skipping probability
# "skip_total"=   Total skipping probability
# "prev"=         Preview condition
# "deg"=          Degradation condition 
# "regIN"         Regression-in probability
# "regOUT"        Regression-out probability


library(ggplot2)
library(lme4)
library(MASS)

# load data

load("Experiment 2a/data/target.Rda")
load("Experiment 2a/data/pre-target.Rda")


#-------------------#
# Target word plot  #
#-------------------#

###################################

library(reshape)
DesN1<- melt(N1_2a, id=c('sub', 'item', 'cond', 'deg', 'prev'), 
             measure=c("FFD", "SFD", "GD"), na.rm=TRUE)
mF<- cast(DesN1, deg+prev ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

df<- data.frame(c(mF$FFD_M, mF$SFD_M, mF$GD_M),
                c(mF$FFD_SD, mF$SFD_SD, mF$GD_SD),
                c(rep(mF$prev,3)), c(rep(mF$deg,3)),
                c(rep('FFD', 6), rep('SFD', 6), rep('GD', 6)))
colnames(df)<- c('Mean', 'SD', 'Preview', 'Degradation', 'Measure')
df$SE<- df$SD/sqrt(length(unique(N1_2a$sub)))

df$Preview<- as.factor(df$Preview)
df$Preview<- factor(df$Preview, levels= c("valid", 'orth', 'mask'))
#levels(df$Preview)<- c("valid", 'phon', 'orth', 'mask')
df$Measure<- as.factor(df$Measure)
df$Measure<- factor(df$Measure, levels= c('FFD', 'SFD', 'GD'))
levels(df$Degradation)<- c(" 0 %", " 20 %")

### graph:
library(ggplot2)
limits <- aes(ymax = df$Mean + df$SE, ymin=df$Mean - df$SE)


Dplot<- ggplot(data= df, aes(x=Preview, y= Mean, color=Degradation,
                             fill= Degradation, group=Degradation, shape=Degradation,
                             linetype=Degradation, ymax = Mean + SE, ymin= Mean - SE))+ 
  #scale_y_continuous(breaks=c(200, 250, 300, 350, 400, 450))+
  scale_fill_brewer(palette="Dark2")+ scale_colour_brewer(palette="Dark2")+
  theme_bw(24) + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
                       axis.line = element_line(colour = "black", size=1),
                       panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
  geom_line(size=2)+ scale_y_continuous(limits = c(210, 330), breaks = c(225, 250, 275, 300, 325))+
  geom_point(size=6)+ 
  scale_shape_manual(values=c(15, 17))+
  xlab("Parafoveal preview of word N+1\n")+ ylab("Mean fixation duration")+ 
  theme(legend.position=c(0.15, 0.85), legend.title=element_text(size=26, face="bold", family="serif"),
        legend.text=element_text(size=26,family="serif"),legend.key.width=unit(2,"cm"),
        legend.key.height=unit(1,"cm"), strip.text=element_text(size=26, family="serif"),
        title=element_text(size=26, family="serif"),
        axis.title.x = element_text(size=26, face="bold", family="serif"), 
        axis.title.y = element_text(size=26, face="bold", family="serif"), 
        axis.text=element_text(size=26, family="serif"), 
        panel.border = element_rect(linetype = "solid", colour = "black"), 
        legend.key = element_rect(colour = "#000000", size=1),
        plot.title = element_text(hjust = 0.5))+
  facet_grid(.~ Measure) + theme(strip.text.x = element_text(size = 22,  face="bold",family="serif"),
                                 strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
                                 legend.key = element_rect(colour = "#000000", size=1)) + geom_ribbon(alpha=0.10, 
                                                                                                      colour=NA) +
  ggtitle("Experiment 2a\n[all words degraded]")

Dplot

E1plot<- Dplot
save(E1plot, file= "Experiment 2a/Plots/TW.Rda")




### Preview costs (size):
dfN<- mF[, c('deg', 'prev', 'FFD_M', 'SFD_M', 'GD_M')]
dfN<- subset(dfN, prev== "mask")

# FFD cost:
(FFDc<- dfN$FFD_M[1]-dfN$FFD_M[2])

# SFD cost:
(SFDc<- dfN$SFD_M[1]-dfN$SFD_M[2])

# GD cost:
(GDc<- dfN$GD_M[1]-dfN$GD_M[2])

### Preview benefit (size):
dfB<- mF[, c('deg', 'prev', 'FFD_M', 'SFD_M', 'GD_M')]
dfB<- subset(dfB, prev== "valid")

## FFD benefit:
(FFDb<- dfB$FFD_M[2]- dfB$FFD_M[1])

## SFD benefit:
(SFDb<- dfB$SFD_M[2]- dfB$SFD_M[1])

## GD benefit:
(GDb<- dfB$GD_M[2]- dfB$GD_M[1])


# Percentage cost:

# FFD:
(FFDpc<- (FFDc/(dfN$FFD_M[1] - dfB$FFD_M[1]))*100)

# SFD:
(SFDpc<- (SFDc/(dfN$SFD_M[1] - dfB$SFD_M[1]))*100)

# GD:
(GDpc<- (GDc/(dfN$GD_M[1] - dfB$GD_M[1]))*100)

mean(c(FFDpc, SFDpc, GDpc))
sd(c(FFDpc, SFDpc, GDpc))

# two experiments (original + replication):
mean(c(FFDpc, SFDpc, GDpc, 28.57143, 45.45455, 23.68421))
sd(c(FFDpc, SFDpc, GDpc, 28.57143, 45.45455, 23.68421))

range(c(FFDpc, SFDpc, GDpc, 28.57143, 45.45455, 23.68421))

#-------------------
# Question Accuracy:
#-------------------

load("Experiment 2a/data/Quest2a.Rda")

DesQ<- melt(Quest2a, id=c('sub', 'item', 'cond', 'deg', 'prev'), 
             measure=c("accuracy"), na.rm=TRUE)
mQ<- cast(DesQ, prev ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

Quest2a$deg<- as.factor(Quest2a$deg)
contrasts(Quest2a$deg)<- c(1,-1)
contrasts(Quest2a$deg)


cmat<- matrix(data = c(-1,1,0,0,1, -1), nrow=3, ncol=2,
              dimnames = list(c('valid', 'invalid', 'orth'),
                              c(".PB", ".Orth_PB")))

# need to use the generalised inverse to get the correct comparisons
inv.cmat<- fractions(t(ginv(cmat))) 

# copy row and column names over from cmat to make interpretation easier
colnames(inv.cmat) <- colnames(cmat)
rownames(inv.cmat)<- rownames(cmat)


Quest2a$prev<- as.factor(Quest2a$prev)
levels(Quest2a$prev)<- c("invalid", "orth", "valid") 
levels(Quest2a$prev)
Quest2a$prev<- factor(Quest2a$prev, levels= c("valid", "invalid", "orth"))
contrasts(Quest2a$prev)<- inv.cmat
contrasts(Quest2a$prev)

# Comprehension accuracy:
if(!file.exists("Experiment 2a/Models/G1.Rda")){
  
  G1<- glmer(accuracy ~  prev*deg+ (1|sub)+ (deg|item), family= binomial, data= Quest2a)
  
  save(G1, file= "Experiment 2a/Models/G1.Rda")
  summary(G1)
  
}else{
  load("Experiment 2a/Models/G1.Rda")
  summary(G1)
}
max(abs(unname(coef(summary(G1))[3:6,3])))

library(effects)
effect('prev', G1)


# Contrast coding:
N1_2a$deg<- as.factor(N1_2a$deg)
contrasts(N1_2a$deg)<- c(1,-1)
contrasts(N1_2a$deg)

N1_2a$prev<- as.factor(N1_2a$prev)
levels(N1_2a$prev)<- c("invalid", "orth", "valid")

N1_2a$prev<- factor(N1_2a$prev, levels= c("valid", "invalid", "orth"))
contrasts(N1_2a$prev)


contrasts(N1_2a$prev)<- inv.cmat
contrasts(N1_2a$prev)

contrasts(N1_2a$deg)

### FFD:

if(!file.exists("Experiment 2a/Models/LM1.Rda")){
  summary(LM1<- lmer(log(FFD)~ prev*deg +(deg|sub) + (1|item), data= N1_2a))
  
  save(LM1, file= "Experiment 2a/Models/LM1.Rda")
  summary(LM1)
}else{
  load("Experiment 2a/Models/LM1.Rda")
  summary(LM1)
}

S_LM1<- round(coef(summary(LM1)),2)

library(effects)
effect('prev:deg', LM1)

### SFD:
if(!file.exists("Experiment 2a/Models/LM2.Rda")){
  summary(LM2<- lmer(log(SFD)~ prev*deg +(prev|sub) + (1|item), data= N1_2a))
  
  save(LM2, file= "Experiment 2a/Models/LM2.Rda")
  summary(LM2)
}else{
  load("Experiment 2a/Models/LM2.Rda")
  summary(LM2)
}

S_LM2<- round(coef(summary(LM2)),2)
effect('prev:deg', LM2)

### GD:
if(!file.exists("Experiment 2a/Models/LM3.Rda")){
  summary(LM3<- lmer(log(GD)~ prev*deg +(deg|sub) + (1|item), data= N1_2a))
  
  save(LM3, file= "Experiment 2a/Models/LM3.Rda")
  summary(LM3)
}else{
  load("Experiment 2a/Models/LM3.Rda")
  summary(LM3)
}

S_LM3<- round(coef(summary(LM3)),2)
effect('prev:deg', LM3)


# write model results to csv:
write.csv(S_LM1, 'Experiment 2a/Models/FFD.csv')
write.csv(S_LM2, 'Experiment 2a/Models/SFD.csv')
write.csv(S_LM3, 'Experiment 2a/Models/GD.csv')



#####################################################################
# Global analyses:                                                  #
#####################################################################

library(reshape)
library(lme4)

#------------------------#
# sentence reading time: #
#------------------------#
load("Experiment 2a/data/Trial_time2a.Rda")

# descriptives
DesTime<- melt(Trialt2a, id=c('sub', 'item', 'deg'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, deg ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

Trialt2a$deg<- as.factor(Trialt2a$deg)
contrasts(Trialt2a$deg)<- c(1, -1)
contrasts(Trialt2a$deg)


if(!file.exists("Experiment 2a/Models/GL1.Rda")){
  GL1<- lmer(log(duration_ms) ~ deg +(deg|sub)+ (1|item), data =Trialt2a, REML = T)
  summary(GL1)
  save(GL1, file= 'Experiment 2a/Models/GL1.Rda')
}else{
  load('Experiment 2a/Models/GL1.Rda')
  summary(GL1)
}


#--------------------------#
# Fixation duration (all): #
#--------------------------#
load("Experiment 2a/data/raw_fix2a.Rda")

# descriptives
DesFix<- melt(rf2a, id=c('sub', 'item', 'deg'), 
              measure=c("fix_dur"), na.rm=TRUE)
mFix<- cast(DesFix, deg ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

rf2a$deg<- as.factor(rf2a$deg)
contrasts(rf2a$deg)<- c(1, -1)
contrasts(rf2a$deg)


if(!file.exists("Experiment 2a/Models/GL2.Rda")){
  GL2<- lmer(log(fix_dur) ~ deg +(1|sub)+ (1|item), data =rf2a, REML = T)
  summary(GL2)
  save(GL2, file= 'Experiment 2a/Models/GL2.Rda')
}else{
  load('Experiment 2a/Models/GL2.Rda')
  summary(GL2)
}


#------------------#
# Saccade length : #
#------------------#

# descriptives
DesLen<- melt(rf2a, id=c('sub', 'item', 'deg'), 
              measure=c("sacc_len"), na.rm=TRUE)
mLen<- cast(DesLen, deg ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))


if(!file.exists("Experiment 2a/Models/GL3.Rda")){
  GL3<- lmer(sacc_len ~ deg +(1|sub)+ (1|item), data =rf2a, REML = T)
  summary(GL3)
  save(GL3, file= 'Experiment 2a/Models/GL3.Rda')
}else{
  load('Experiment 2a/Models/GL3.Rda')
  summary(GL3)
}


#--------------------------#
# number of fixations:     #
#--------------------------#
load("Experiment 2a/data/num_fix2a.Rda")

# descriptives
DesNFix<- melt(nFix2a, id=c('sub', 'item', 'deg'), 
               measure=c("Nfix_all"), na.rm=TRUE)
mNFix<- cast(DesNFix, deg ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

nFix2a$deg<- as.factor(nFix2a$deg)
contrasts(nFix2a$deg)<- c(1, -1)
contrasts(nFix2a$deg)


if(!file.exists("Experiment 2a/Models/GL4.Rda")){
  GL4<- lmer(Nfix_all ~ deg +(deg|sub)+ (1|item), data =nFix2a, REML = T)
  summary(GL4)
  save(GL4, file= 'Experiment 2a/Models/GL4.Rda')
}else{
  load('Experiment 2a/Models/GL4.Rda')
  summary(GL4)
}



########################################################################################################
#                                            Post-hoc analyses                                         #
########################################################################################################

#-------------------#
# Pre-target word   #
#-------------------#

library(reshape)
DesN<- melt(N_2a, id=c('sub', 'item', 'cond', 'deg', 'prev'), 
             measure=c("FFD", "SFD", "GD"), na.rm=TRUE)
mN<- cast(DesN, deg+prev ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))



# Contrast coding:
N_2a$deg<- as.factor(N_2a$deg)
contrasts(N_2a$deg)<- c(1,-1)
contrasts(N_2a$deg)

N_2a$prev<- as.factor(N_2a$prev)
levels(N_2a$prev)<- c("invalid", "orth", "valid")

N_2a$prev<- factor(N_2a$prev, levels= c("valid", "invalid", "orth"))
contrasts(N_2a$prev)


contrasts(N_2a$prev)<- inv.cmat
contrasts(N_2a$prev)
contrasts(N_2a$deg)


### FFD:
if(!file.exists("Experiment 2a/Models/PoF/POF_LM1.Rda")){
  summary(POF_LM1<- lmer(log(FFD)~ prev*deg +(deg|sub) + (deg|item), data= N_2a))
  save(POF_LM1, file= "Experiment 2a/Models/PoF/POF_LM1.Rda")
}else{
  load("Experiment 2a/Models/PoF/POF_LM1.Rda")
  summary(POF_LM1)
}


### SFD:
if(!file.exists("Experiment 2a/Models/PoF/POF_LM2.Rda")){
  summary(POF_LM2<- lmer(log(SFD)~ prev*deg +(deg|sub) + (deg|item), data= N_2a))
  save(POF_LM2, file= "Experiment 2a/Models/PoF/POF_LM2.Rda")
}else{
  load("Experiment 2a/Models/PoF/POF_LM2.Rda")
  summary(POF_LM2)
}


### GD:
if(!file.exists("Experiment 2a/Models/PoF/POF_LM3.Rda")){
  summary(POF_LM3<- lmer(log(GD)~ prev*deg +(deg|sub) + (deg|item), data= N_2a))
  save(POF_LM3, file= "Experiment 2a/Models/PoF/POF_LM3.Rda")
}else{
  load("Experiment 2a/Models/PoF/POF_LM3.Rda")
  summary(POF_LM3)
}


# write model results to csv:

write.csv(round(coef(summary(POF_LM1)),2), 'Experiment 2a/Models/PoF/PoF_FFD.csv')
write.csv(round(coef(summary(POF_LM2)),2), 'Experiment 2a/Models/PoF/PoF_SFD.csv')
write.csv(round(coef(summary(POF_LM3)),2), 'Experiment 2a/Models/PoF/PoF_GD.csv')


#---------------------#
# Additional measures #
#---------------------#

### descriptives:
library(reshape)
DesOther<- melt(N1_2a, id=c('sub', 'item', 'cond', 'deg', 'prev'), 
            measure=c("skip_1st","regIN", "regOUT"), na.rm=TRUE)
mO<- cast(DesOther, deg+prev ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))
contrasts(N1_2a$prev)
contrasts(N1_2a$deg)

### Skipping:
if(!file.exists("Experiment 2a/Models/Prob/Skip.Rda")){
  summary(Skip<- glmer(skip_1st ~ prev*deg + (deg|sub)+ (1|item), 
                       data = N1_2a, family = binomial))
  
  save(Skip, file= "Experiment 2a/Models/Prob/Skip.Rda")
  write.csv(round(coef(summary(Skip)),2), 'Experiment 2a/Models/Prob/Skip.csv')
}else{
  load("Experiment 2a/Models/Prob/Skip.Rda")
  summary(Skip)
}

library(effects)

effect('prev', Skip)


### Reg in:
if(!file.exists("Experiment 2a/Models/Prob/RegIN.Rda")){
  summary(RegIN<- glmer(regIN ~ prev*deg + (deg|sub)+ (1|item), 
                       data = N1_2a, family = binomial))
  
  save(RegIN, file= "Experiment 2a/Models/Prob/RegIN.Rda")
  write.csv(round(coef(summary(RegIN)),2), 'Experiment 2a/Models/Prob/RegIN.csv')
}else{
  load("Experiment 2a/Models/Prob/RegIN.Rda")
  summary(RegIN)
}

effect('deg', RegIN)



### Reg out:
if(!file.exists("Experiment 2a/Models/Prob/RegOUT.Rda")){
  summary(RegOUT<- glmer(regOUT ~ prev*deg + (1|sub)+ (deg|item), 
                        data = N1_2a, family = binomial))
  
  save(RegOUT, file= "Experiment 2a/Models/Prob/RegOUT.Rda")
  write.csv(round(coef(summary(RegOUT)),2), 'Experiment 2a/Models/Prob/RegOUT.csv')
}else{
  load("Experiment 2a/Models/Prob/RegOUT.Rda")
  summary(RegOUT)
}

effect('prev', RegOUT)
effect('deg', RegOUT)

effect('prev:deg', RegOUT)
