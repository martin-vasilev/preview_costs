
# Martin R. Vasilev, 2018

# EXPERIMENT 1a:

rm(list=ls())


### MEANING OF DATA COLUMNS:

# "seq" =         Trial sequence in the experiment
# "subj"=         Subject number
# "item"=         Item number
# "cond"=         Condition number
# "FFD_N"=        First fixation duration on the pre-target word (N)
# "FFD_N1"=       First fixation duration on the target word (N+1)
# "SFD_N"=        Single fixation duration on the pre-target word (N)
# "SFD_N1"        Single fixation duration on the target word (N+1) 
# "GD_N"=         Gaze duration on the pre-target word (N)
# "GD_N1"=        Gaze duration on the target word (N+1)    
# "TVT_N"=        Total viewing time on the pre-target word (N)
# "TVT_N1"=       Total viewing time on the target word (N+1)
# "nFix_N"=       Number of fixations on the pre-target word (N)
# "nFix_N1"=      Number of fixations on the target word (N+1) 
# "prev"=         Preview condition
# "deg"=          Degradation condition 
# "pRefixN"=      Probability of re-fixating the pre-target word (N)
# "pRefixN1"=     Probability of re-fixating the target word (N+1)
# "preBndFix"=    Duration of the fixation immediately before crossing the boundary
# "preBndX"=      Location of the fixation immediately before crossing the boundary (in pixels)  
# "dist_Bnd"=     Distance of the pre-boundary fixation from the boundary location (in letters)
# "RegIN_N"=      Regression-in probability on the pre-target word (N)
# "RegIN_N1"=     Regression-in probability on the target word (N+1)
# "RegOUT_N"=     Regression-out probability on the pre-target word (N)
# "RegOUT_N1"=    Regression-out probability on the target word (N+1)
# "Skip_N"=       First-pass skipping of the pre-target word (N)
# "Skip_N1"=      First-pass skipping of the target word (N+1)


### MEANS:

load('Experiment 1a/data/means.Rda')
load('Experiment 1a/data/data.Rda')

df<- data.frame(c(mF$FFD_N1_M, mF$SFD_N1_M, mF$GD_N1_M, mF$TVT_N1_M),
                c(mF$FFD_N1_SD, mF$SFD_N1_SD, mF$GD_N1_SD, mF$TVT_N1_SD),
                c(rep(mF$prev,4)), c(rep(mF$deg,4)),
                c(rep('FFD', 8), rep('SFD', 8), rep('GD', 8), rep('TVT', 8)))
colnames(df)<- c('Mean', 'SD', 'Preview', 'Degradation', 'Measure')
df$SE<- df$SD/sqrt(length(unique(data$subj)))

df$Preview<- as.factor(df$Preview)
df$Preview<- factor(df$Preview, levels= c("valid", 'phon', 'orth', 'invalid'))
levels(df$Preview)<- c("valid", 'phon', 'orth', 'mask')
df$Measure<- as.factor(df$Measure)
df$Measure<- factor(df$Measure, levels= c('FFD', 'SFD', 'GD', 'TVT'))

dT<- subset(df, Measure=="TVT" |  Measure=="SFD")
df<- subset(df, Measure!= "TVT") #& Measure!= "SFD")

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
  geom_line(size=2)+ scale_y_continuous(limits = c(210, 320))+
  geom_point(size=7)+ 
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
        colour=NA) + ggtitle("Experiment 1")

ggsave("Experiment 1a/Plots/TW.png", Dplot, width= 14, height=8, units= "in", dpi=200)
ggsave("Experiment 1a/Plots/TW.pdf", Dplot, width= 12, height=8, units= "in")

E1plot<- Dplot
save(E1plot, file= "Experiment 1a/Plots/TW.Rda")


### Preview costs (size):
dfN<- mF[, c('prev', 'deg', 'FFD_N1_M', 'SFD_N1_M', 'GD_N1_M')]
dfN<- subset(dfN, prev== "invalid")

# FFD cost:
(FFDc<- dfN$FFD_N1_M[1]-dfN$FFD_N1_M[2])

# SFD cost:
(SFDc<- dfN$SFD_N1_M[1]-dfN$SFD_N1_M[2])

# GD cost:
(GDc<- dfN$GD_N1_M[1]-dfN$GD_N1_M[2])

### Preview benefit (size):
dfB<- mF[, c('prev', 'deg', 'FFD_N1_M', 'SFD_N1_M', 'GD_N1_M')]
dfB<- subset(dfB, prev== "valid")

## FFD benefit:
(FFDb<- dfB$FFD_N1_M[2]- dfB$FFD_N1_M[1])

## SFD benefit:
(SFDb<- dfB$SFD_N1_M[2]- dfB$SFD_N1_M[1])

## GD benefit:
(GDb<- dfB$GD_N1_M[2]- dfB$GD_N1_M[1])


# Percentage cost:

# FFD:
(FFDpc<- (FFDc/(dfN$FFD_N1_M[1] - dfB$FFD_N1_M[1]))*100)

# SFD:
(SFDpc<- (SFDc/(dfN$SFD_N1_M[1] - dfB$SFD_N1_M[1]))*100)

# GD:
(GDpc<- (GDc/(dfN$GD_N1_M[1] - dfB$GD_N1_M[1]))*100)


#----------------------
# Statistical analysis:
#----------------------

# set-up contrast coding:
library(lme4)

data$prev<- as.factor(data$prev)
data$prev<- factor(data$prev, levels= c("valid", "invalid", "phon", "orth"))
data$deg<- as.factor(data$deg)

contrasts(data$deg)<- c(1,-1)
contrasts(data$prev)

cmat<- matrix(data = c(-1,1,0,0,0,1,0,-1,0,0,-1,1), nrow=4, ncol=3,
              dimnames = list(c('valid', 'invalid', 'phon', 'orth'),
                              c(".PB", ".Orth_PB", ".Phon_PB")))

library(MASS)
# need to use the generalised inverse to get the correct comparisons
inv.cmat<- fractions(t(ginv(cmat))) 

# copy row and column names over from cmat to make interpretation easier
colnames(inv.cmat) <- colnames(cmat)
rownames(inv.cmat)<- rownames(cmat)
contrasts(data$prev)<- inv.cmat
contrasts(data$prev)



### COMPREHENSION ACCURACY:
load("Experiment 1a/data/quest_accuracy.Rda")

q$deg<- as.factor(q$deg)
contrasts(q$deg)<- c(1,-1)
contrasts(q$deg)

q$prev<- as.factor(q$prev)
q$prev<- factor(q$prev, levels= c("valid", "invalid", "phon", "orth"))
contrasts(q$prev)<- inv.cmat
contrasts(q$prev)

if(!file.exists("Experiment 1a/Models/G1.Rda")){
  # does not converge with any random effects
  G1<- glmer(accuracy ~ deg* prev + (1|subject)+ (1|item), family= binomial, data= q,
             glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
  save(G1, file= "Experiment 1/Models/G1.Rda")
  summary(G1)
}else{
  load("Experiment 1a/Models/G1.Rda")
  summary(G1)
}
max(abs(unname(coef(summary(G1))[2:8,3])))


###### TARGET WORD ANALYSIS:
library(effects)

# FFD:
if(!file.exists("Experiment 1a/Models/FFDN1.Rda")){
  # Does not converge with preview slopes and deg slope for items (variance too low)
  summary(FFDN1<- lmer(log(FFD_N1)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(FFDN1, file= "Experiment 1a/Models/FFDN1.Rda")
}else{
  load("Experiment 1a/Models/FFDN1.Rda")
  summary(FFDN1)
}

(SFFD<- round(coef(summary(FFDN1)),2))
write.csv(SFFD, "Experiment 1a/Models/TW_FFD.csv")


plot(effect('deg', FFDN1))
plot(effect('prev', FFDN1))
plot(effect('prev:deg', FFDN1))

orthInt<- data.frame(deg= c(0, 0, 20, 20), preview= c("orth", "invalid", "orth", "invalid"),
                     FFD= c(5.408311, 5.464535, 5.444203, 5.446641))
orthInt$deg<- as.factor(orthInt$deg)
orthInt$preview<- as.factor(orthInt$preview)

library(ggplot2)

ggplot(orthInt,aes(y= FFD, x= deg, group= preview, shape= preview, color= preview)) +
  geom_point(size=5)+ geom_line(size=1.5) + theme(legend.position = "right") +theme_bw(16)

############
PBInt<- data.frame(deg= c(0, 0, 20, 20), preview= c("valid", "invalid", "valid", "invalid"),
                     FFD= c(5.362744, 5.464535, 5.425815, 5.446641))
PBInt$deg<- as.factor(PBInt$deg)
PBInt$preview<- as.factor(PBInt$preview)

ggplot(PBInt,aes(y= FFD, x= deg, group= preview, shape= preview, color= preview)) +
  geom_point(size=5)+ geom_line(size=1.5) + theme(legend.position = "right") +theme_bw(16)



# SFD:
if(!file.exists("Experiment 1a/Models/SFDN1.Rda")){
  # Does not converge with a preview slope for items
  summary(SFDN1<- lmer(log(SFD_N1)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(SFDN1, file= "Experiment 1a/Models/SFDN1.Rda")
}else{
  load("Experiment 1a/Models/SFDN1.Rda")
  summary(SFDN1)
}

(SSFD<- round(coef(summary(SFDN1)),2))
write.csv(SSFD, "Experiment 1a/Models/TW_SFD.csv")


# GD:
if(!file.exists("Experiment 1a/Models/GDN1.Rda")){
  summary(GDN1<-lmer(log(GD_N1)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(GDN1, file= "Experiment 1a/Models/GDN1.Rda")
}else{
  load("Experiment 1a/Models/GDN1.Rda")
  summary(GDN1)
}

(SGD<- round(coef(summary(GDN1)),2))
write.csv(SGD, file= "Experiment 1a/Models/TW_GD.csv")


#---------------------------------------------------#
#       Post-hoc analyses on pre-target word        #
#---------------------------------------------------#

# FFD:
if(!file.exists("Experiment 1a/Models/PoF_FFD.Rda")){
  summary(PoF_FFD<- lmer(log(FFD_N)~prev*deg+ (deg|subj)+ (deg|item), REML = T, data=data))
  save(PoF_FFD, file= "Experiment 1a/Models/PoF_FFD.Rda")
}else{
  load("Experiment 1a/Models/PoF_FFD.Rda")
  summary(PoF_FFD)
}

# SFD:
if(!file.exists("Experiment 1a/Models/PoF_SFD.Rda")){
  summary(PoF_SFD<- lmer(log(SFD_N)~prev*deg+ (deg|subj)+ (deg|item), REML = T, data=data))
  save(PoF_SFD, file= "Experiment 1a/Models/PoF_SFD.Rda")
}else{
  load("Experiment 1a/Models/PoF_SFD.Rda")
  summary(PoF_SFD)
}


# GD:
if(!file.exists("Experiment 1a/Models/PoF_GD.Rda")){
  summary(PoF_GD<- lmer(log(GD_N)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(PoF_GD, file= "Experiment 1a/Models/PoF_GD.Rda")
}else{
  load("Experiment 1a/Models/PoF_GD.Rda")
  summary(PoF_GD)
}

# write model results to csv:
write.csv(round(coef(summary(PoF_FFD)),2), 'Experiment 1a/Models/PoF_FFD.csv')
write.csv(round(coef(summary(PoF_SFD)),2), 'Experiment 1a/Models/PoF_SFD.csv')
write.csv(round(coef(summary(PoF_GD)),2), 'Experiment 1a/Models/PoF_GD.csv')



######################################################
#               Global reading analysis:             #
######################################################

#------------------------------------------------------
# Sentence reading time as a function of degradation: #
#------------------------------------------------------
load("Experiment 1a/data/t.Rda")

library(reshape)
DesRT<- melt(t, id=c('subj', 'item', 'cond', 'deg'), 
                  measure=c('duration_ms') , na.rm=TRUE)

mRT<- cast(DesRT, deg ~ variable
                , function(x) c(M=signif(mean(x),3)
                                , SD= sd(x) ))

t$deg<- as.factor(t$deg)
contrasts(t$deg)<- c(1, -1)
contrasts(t$deg)

if(!file.exists("Experiment 1a/Models/GR1.Rda")){
  summary(GR1<- lmer(log(duration_ms)~deg+ (deg|subj)+ (1|item), REML = T, data=t))
  save(GR1, file= "Experiment 1a/Models/GR1.Rda")
} else{
  load("Experiment 1a/Models/GR1.Rda")
  summary(GR1)
}

round(coef(summary(GR1)), 2)

#----------------------
# fixations duration: #
#----------------------
load("Experiment 1a/data/raw_fix.Rda")

library(reshape)
DesFix<- melt(raw_fix, id=c('sub', 'item', 'cond', 'deg'), 
             measure=c('fix_dur') , na.rm=TRUE)

mFix<- cast(DesFix, deg ~ variable
           , function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

raw_fix$deg<- as.factor(raw_fix$deg)
contrasts(raw_fix$deg)<- c(1, -1)
contrasts(raw_fix$deg)


if(!file.exists("Experiment 1a/Models/GR2.Rda")){
  summary(GR2<- lmer(log(fix_dur)~deg+ (deg|sub)+ (1|item), REML = T, data=raw_fix))
  save(GR2, file= "Experiment 1a/Models/GR2.Rda")
} else{
  load("Experiment 1a/Models/GR2.Rda")
  summary(GR2)
}

round(coef(summary(GR2)), 3)


#-----------------------
# number of fixations: #
#-----------------------
load("Experiment 1a/data/FX.Rda")

DesFX<- melt(FX, id=c('sub', 'item', 'cond', 'deg'), 
              measure=c('nfix') , na.rm=TRUE)

mFX<- cast(DesFX, deg ~ variable
            , function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

FX$deg<- as.factor(FX$deg)
contrasts(FX$deg)<- c(1, -1)
contrasts(FX$deg)


if(!file.exists("Experiment 1a/Models/GR3.Rda")){
  summary(GR3<- lmer(nfix~deg+ (deg|sub)+ (1|item), REML = T, data=FX))
  save(GR3, file= "Experiment 1a/Models/GR3.Rda")
}else{
  load("Experiment 1a/Models/GR3.Rda")
  summary(GR3)
}


round(coef(summary(GR3)), 2)


#-----------------#
# saccade length: #
#-----------------#

DesSACC<- melt(raw_fix, id=c('sub', 'item', 'cond', 'deg'), 
               measure=c('sacc_len') , na.rm=TRUE)

mSACC<- cast(DesSACC, deg ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

raw_fix$deg<- as.factor(raw_fix$deg)
contrasts(raw_fix$deg)<- c(1, -1)


if(!file.exists("Experiment 1a/Models/GR4.Rda")){
  summary(GR4<- lmer(sacc_len~deg+ (1|sub)+ (1|item), REML = T, data=raw_fix))
  save(GR4, file= "Experiment 1a/Models/GR4.Rda")
}else{
  load("Experiment 1a/Models/GR4.Rda")
  summary(GR4)
}

round(coef(summary(GR4)), 2)


#----------------------#
# Global reading plot: #
#----------------------#

db<- data.frame(c(mRT$duration_ms_M, mFix$fix_dur_M, mFX$nfix_M), c(mRT$duration_ms_SD, mFix$fix_dur_SD, mFX$nfix_SD),
                rep(c("0 %", "20 %"),3), c("Sentence reading time (in ms)", "Sentence reading time (in ms)",
                                       "Fixation duration (in ms)", "Fixation duration (in ms)",
                                       "Number of fixations", "Number of fixations"))
colnames(db)<- c("Mean", "SD", "Degradation", "Measure")

db$SE<- db$SD/sqrt(length(unique(t$subj)))


library(ggplot2)

limits <- aes(ymax = db$Mean + db$SE, ymin=db$Mean - db$SE)
#family="serif"
Dplot<- ggplot(data= db, aes(x=Degradation, y= Mean, fill= Degradation, group=Degradation))+ 
  #scale_y_continuous(breaks=c(100, 200, 300, 400, 500))+
  scale_fill_brewer(palette="Pastel2")+ #scale_colour_brewer(palette="Pastel2")+
  theme_bw() + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
                     axis.line = element_line(colour = "black", size=1),
                     panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
  geom_bar(stat = "identity", aes(fill = Degradation, x = Degradation), position = "dodge", width=0.6, color= "#737373")+
  #geom_line(size=2)+
  #geom_point(size=7)+ 
  xlab("\n Degradation")+ ylab("Mean")+ 
  theme(legend.position="none", legend.title=element_text(size=20, face="bold", family="serif"), legend.text=element_text(size=20,family="serif"),legend.key.width=unit(2,"cm"),
        legend.key.height=unit(1,"cm"), strip.text=element_text(size=20, family="serif"),
        title=element_text(size=20, family="serif"),
        axis.title.x = element_text(size=20, face="bold", family="serif"), axis.title.y = element_text(size=20, face="bold", family="serif"), 
        axis.text=element_text(size=20, family="serif"), 
        panel.border = element_rect(linetype = "solid", colour = "black"), 
        legend.key = element_rect(colour = "#000000", size=1))+
  facet_wrap(facets = ~ Measure, nrow = 1, scales="free") + theme(strip.text.x = element_text(size = 16,  face="bold",family="serif"),
  strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
  legend.key = element_rect(colour = "#000000", size=1)) + geom_errorbar(mapping = aes(ymin=Mean-SE, ymax=Mean+SE),
  position=position_dodge(width=0.6), width=0.2, color= "#737373")

ggsave("Experiment 1a/Plots/Gen.png", Dplot, width= 12, height=5, units= "in", dpi=300)



#------------------------------------------------------------------#
#         Additional analyses requested by a Reviewer:             #
#------------------------------------------------------------------#
library(reshape2)

DesProb<- melt(data, id=c('subj', 'item', 'prev', 'deg'), 
               measure=c('Skip_N1','RegIN_N1', 'RegOUT_N1') , na.rm=TRUE)

mProb<- cast(DesProb, prev+deg ~ variable
             , function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mProb$prev<- as.factor(mProb$prev)
levels(mProb$prev)
mProb$prev<- factor(mProb$prev, levels= c("valid", "phon", "orth", "invalid"))


##---------------------
## statistical models :
##---------------------

#check coding
contrasts(data$prev)
contrasts(data$deg)


# skipping:
if(!file.exists("Experiment 1a/Models/PRM1.Rda")){
  summary(PRM1<- glmer(Skip_N1 ~ prev*deg + (1|subj)+ (1|item), data = data, family= binomial,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))))
  save(PRM1, file= "Experiment 1a/Models/PRM1.Rda")
}else{
  load("Experiment 1a/Models/PRM1.Rda")
  summary(PRM1)
}

(SPRM1<- round(coef(summary(PRM1)),2))
rowS<- c('Intercept', "Invalid PV", "Orth PV", "Phon PV", "Deg",             
                    "Invalid PV:Deg", "Orth PV:Deg", "Phon PV:Deg")
rownames(SPRM1)<- rowS
write.csv(SPRM1, 'Experiment 1a/Models/PR_skip.csv')


# regression-in:
if(!file.exists("Experiment 1a/Models/PRM2.Rda")){
  summary(PRM2<- glmer(RegIN_N1 ~ prev*deg + (1|subj)+ (1|item), data = data, family= binomial))
  save(PRM2, file= "Experiment 1a/Models/PRM2.Rda")
  write.csv(round(coef(summary(PRM2)),2), 'Experiment 1a/Models/PR_regIN.csv')
}else{
  load("Experiment 1a/Models/PRM2.Rda")
  summary(PRM2)
}


# regression-out:
if(!file.exists("Experiment 1a/Models/PRM3.Rda")){
  summary(PRM3<- glmer(RegOUT_N1 ~ prev*deg + (deg|subj)+ (1|item), data = data, family= binomial,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))))
  save(PRM3, file= "Experiment 1a/Models/PRM3.Rda")
  write.csv(round(coef(summary(PRM3)),2), 'Experiment 1a/Models/PR_regOUT.csv')
}else{
  load("Experiment 1a/Models/PRM3.Rda")
  summary(PRM3)
}
