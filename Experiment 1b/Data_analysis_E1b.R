# Martin R. Vasilev, 2019

# EXPERIMENT 1b

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


load('Experiment 1b/data/means.Rda')
load('Experiment 1b/data/data.Rda')

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

dT<- subset(df, Measure=="TVT" | Measure=="SFD")
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
                                 colour=NA) + ggtitle("Experiment 1b\n[target and rest of sentence degraded]")
ggsave("Experiment 1b/Plots/TW.png", Dplot, width= 14, height=8, units= "in", dpi=200)
ggsave("Experiment 1b/Plots/TW.pdf", Dplot, width= 12, height=9, units= "in")

E2plot<- Dplot
save(E2plot, file= "Experiment 1b/Plots/TW.Rda")

#ggsave("Plots/TW.tiff", Dplot, width= 15, height=7, units= "in")

library(ggpubr)
load("Experiment 1a/Plots/TW.Rda")
figure <- ggarrange(E1plot, E2plot, ncol = 1, nrow = 2,
                    common.legend = TRUE, legend = "bottom")
ggsave("Experiment 1b/Plots/Merged.pdf", figure, width= 14, height=16, units= "in")
ggsave("Experiment 1b/Plots/Merged.eps", figure, width= 12, height=16, units= "in", device = "eps")



#----------------------
# Statistical analysis:
#----------------------

library(lme4)

data$prev<- as.factor(data$prev)
data$prev<- factor(data$prev, levels= c("valid", "invalid", "phon", "orth"))
data$deg<- as.factor(data$deg)

contrasts(data$deg)<- c(1,-1)
contrasts(data$deg)

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
load("Experiment 1b/data/quest_accuracy.Rda")

q$deg<- as.factor(q$deg)
contrasts(q$deg)<- c(1,-1)
contrasts(q$deg)

q$prev<- as.factor(q$prev)
q$prev<- factor(q$prev, levels= c("valid", "invalid", "phon", "orth"))
contrasts(q$prev)<- inv.cmat
contrasts(q$prev)

# Comprehension accuracy:
if(!file.exists("Experiment 1b/Models/G1.Rda")){

  G1<- glmer(accuracy ~  prev*deg+ (1|subject)+ (1|item), family= binomial, data= q, 
             glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
  save(G1, file= "Experiment 1b/Models/G1.Rda")
  summary(G1)
  
  # 
  #summary(G1<- glmer(accuracy ~ prev*deg + (1|item), family= binomial, data= q))
  
  ## post-hoc:
  # check of valid vs invalid difference is significant after removing sub 32, 64
  
  q2<- subset(q, !is.element(subject, c(32,64, 60)))
  summary(glmer(accuracy ~  prev*deg+ (1|subject)+ (1|item), family= binomial, data= q2, 
        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))))
  
  
}else{
  load("Experiment 1b/Models/G1.Rda")
  summary(G1)
}
max(abs(unname(coef(summary(G1))[2:8,3])))

library(reshape)
DesQ<- melt(q, id=c('subject', 'item', 'cond', 'prev', 'deg'), 
                measure=c("accuracy") , na.rm=TRUE)

mQ<- cast(DesQ, prev+deg ~ variable
              , function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))

mQ2<- cast(DesQ, prev ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

mQ3<- cast(DesQ, prev+deg+subject ~ variable
           , function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

mQ3v<- subset(mQ3, prev== 'valid' & deg== '0') # valid 0 deg cond
mQ3v20<- subset(mQ3, prev== 'valid' & deg== '20') # valid 20 deg cond

mQ3m<- subset(mQ3, prev== 'invalid' & deg== '0') # mask 0 deg cond
mQ3m20<- subset(mQ3, prev== 'invalid' & deg== '20') # mask 20 deg cond


min(mQ3v$accuracy_M)

# FFD:
if(!file.exists("Experiment 1b/Models/FFDN1.Rda")){
  summary(FFDN1<- lmer(log(FFD_N1)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(FFDN1, file= "Experiment 1b/Models/FFDN1.Rda")
}else{
  load("Experiment 1b/Models/FFDN1.Rda")
  summary(FFDN1)
}

SFFD<- round(coef(summary(FFDN1)),2)
write.csv(SFFD, file= "Experiment 1b/Models/TW_FFD.csv")


# SFD: 
if(!file.exists("Experiment 1b/Models/SFDN1.Rda")){
  summary(SFDN1<-lmer(log(SFD_N1)~prev*deg+ (1|subj)+ (deg|item), REML = T, data=data))
  save(SFDN1, file= "Experiment 1b/Models/SFDN1.Rda")
  summary(SFDN1)
}else{
  load("Experiment 1b/Models/SFDN1.Rda")
  summary(SFDN1)
}

SSFD<- round(coef(summary(SFDN1)),2)
write.csv(SSFD, file= "Experiment 1b/Models/TW_SFD.csv")



# GD:
if(!file.exists("Experiment 1b/Models/GDN1.Rda")){
  summary(GDN1<-lmer(log(GD_N1)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(GDN1, file= "Experiment 1b/Models/GDN1.Rda")
  summary(GDN1)
}else{
  load("Experiment 1b/Models/GDN1.Rda")
  summary(GDN1)
}

SGD<- round(coef(summary(GDN1)),2)
write.csv(SGD, file= "Experiment 1b/Models/TW_GD.csv")



#---------------------------------------------------#
#       Post-hoc analyses on pre-target word        #
#---------------------------------------------------#

# FFD:
if(!file.exists("Experiment 1b/Models/PoF_FFD.Rda")){
  summary(PoF_FFD<- lmer(log(FFD_N)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(PoF_FFD, file= "Experiment 1b/Models/PoF_FFD.Rda")
}else{
  load("Experiment 1b/Models/PoF_FFD.Rda")
  summary(PoF_FFD)
}

# SFD:
if(!file.exists("Experiment 1b/Models/PoF_SFD.Rda")){
  summary(PoF_SFD<- lmer(log(SFD_N)~prev*deg+ (1|subj)+ (1|item), REML = T, data=data))
  save(PoF_SFD, file= "Experiment 1b/Models/PoF_SFD.Rda")
}else{
  load("Experiment 1b/Models/PoF_SFD.Rda")
  summary(PoF_SFD)
}


# GD:
if(!file.exists("Experiment 1b/Models/PoF_GD.Rda")){
  summary(PoF_GD<- lmer(log(GD_N)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(PoF_GD, file= "Experiment 1b/Models/PoF_GD.Rda")
}else{
  load("Experiment 1b/Models/PoF_GD.Rda")
  summary(PoF_GD)
}


# write model results to csv:
write.csv(round(coef(summary(PoF_FFD)),2), 'Experiment 1b/Models/PoF_FFD.csv')
write.csv(round(coef(summary(PoF_SFD)),2), 'Experiment 1b/Models/PoF_SFD.csv')
write.csv(round(coef(summary(PoF_GD)),2), 'Experiment 1b/Models/PoF_GD.csv')





######################################################
#               Global reading analysis:             #
######################################################

#------------------------------------------------------
# Sentence reading time as a function of degradation: #
#------------------------------------------------------

load("Experiment 2/data/t.Rda")
library(reshape)

DesRT<- melt(t, id=c('subj', 'item', 'cond', 'deg'), 
                  measure=c('duration_ms') , na.rm=TRUE)

mRT<- cast(DesRT, deg ~ variable
                , function(x) c(M=signif(mean(x),3)
                                , SD= sd(x) ))

t$deg<- as.factor(t$deg)
contrasts(t$deg)<- c(1,-1)
contrasts(t$deg)

if(!file.exists("Experiment 2/Models/GR1.Rda")){
  summary(GR1<- lmer(log(duration_ms)~deg+ (deg|subj)+ (1|item), REML = T, data= t))
  save(GR1, file= "Experiment 2/Models/GR1.Rda")
}else{
  load("Experiment 2/Models/GR1.Rda")
  summary(GR1)
}

round(coef(summary(GR1)),2)


#----------------------
# fixations duration: #
#----------------------

load("Experiment 2/data/raw_fix.Rda")


DesFix<- melt(raw_fix, id=c('sub', 'item', 'cond', 'deg'), 
             measure=c('fix_dur') , na.rm=TRUE)

mFix<- cast(DesFix, deg ~ variable
           , function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

raw_fix$deg<- as.factor(raw_fix$deg)
contrasts(raw_fix$deg)<- c(1,-1)
contrasts(raw_fix$deg)


if(!file.exists("Experiment 2/Models/GR2.Rda")){
  summary(GR2<- lmer(log(fix_dur)~deg+ (1|sub)+ (1|item), REML = T, data=raw_fix))
  save(GR2, file= "Experiment 2/Models/GR2.Rda")
}else{
  load("Experiment 2/Models/GR2.Rda")
  summary(GR2)
}

round(coef(summary(GR2)),3)


#-----------------------
# number of fixations: #
#-----------------------

load("Experiment 2/data/FX.Rda")

DesFX<- melt(FX, id=c('sub', 'item', 'cond', 'deg'), 
              measure=c('nfix') , na.rm=TRUE)

mFX<- cast(DesFX, deg ~ variable
            , function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

FX$deg<- as.factor(FX$deg)
contrasts(FX$deg)<- c(1,-1)
contrasts(FX$deg)


if(!file.exists("Experiment 2/Models/GR3.Rda")){
  summary(GR3<- lmer(nfix~deg+ (deg|sub)+ (1|item), REML = T, data=FX))
  save(GR3, file= "Experiment 2/Models/GR3.Rda")
}else{
  load("Experiment 2/Models/GR3.Rda")
  summary(GR3)
}

round(coef(summary(GR3)),2)


#-----------------#
# saccade length: #
#-----------------#

DesSACC<- melt(raw_fix, id=c('sub', 'item', 'cond', 'deg'), 
             measure=c('sacc_len') , na.rm=TRUE)

mSACC<- cast(DesSACC, deg ~ variable
           , function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

contrasts(raw_fix$deg)<- c(1,-1)
contrasts(raw_fix$deg)


if(!file.exists("Experiment 2/Models/GR4.Rda")){
  summary(GR4<- lmer(sacc_len~ deg+ (1|sub)+ (1|item), REML = T, data=raw_fix))
  save(GR4, file= "Experiment 2/Models/GR4.Rda")
}else{
  load("Experiment 2/Models/GR4.Rda")
  summary(GR4)
}

round(coef(summary(GR4)),2)


####################################################
# Global reading plot:

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

ggsave("Experiment 1/Plots/Gen.png", Dplot, width= 12, height=5, units= "in", dpi=300)



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
if(!file.exists("Experiment 1b/Models/PRM1.Rda")){
  summary(PRM1<- glmer(Skip_N1 ~ prev*deg + (deg|subj)+ (1|item), data = data, family= binomial,
                       glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000))))
  save(PRM1, file= "Experiment 1b/Models/PRM1.Rda")
}else{
  load("Experiment 1b/Models/PRM1.Rda")
  summary(PRM1)
}

library(effects)

effect('deg', PRM1)

(SPRM1<- round(coef(summary(PRM1)),2))
rowS<- c('Intercept', "Invalid PV", "Orth PV", "Phon PV", "Deg",             
         "Invalid PV:Deg", "Orth PV:Deg", "Phon PV:Deg")
rownames(SPRM1)<- rowS
write.csv(SPRM1, 'Experiment 1b/Models/PR_skip.csv')


# regression-in:
if(!file.exists("Experiment 1b/Models/PRM2.Rda")){
  summary(PRM2<- glmer(RegIN_N1 ~ prev*deg + (1|subj)+ (1|item), data = data, family= binomial))
  save(PRM2, file= "Experiment 1b/Models/PRM2.Rda")
  write.csv(round(coef(summary(PRM2)),2), 'Experiment 1b/Models/PR_regIN.csv')
}else{
  load("Experiment 1b/Models/PRM2.Rda")
  summary(PRM2)
}


# regression-out:
if(!file.exists("Experiment 1b/Models/PRM3.Rda")){
  summary(PRM3<- glmer(RegOUT_N1 ~ prev*deg + (1|subj)+ (1|item), data = data, family= binomial))
  save(PRM3, file= "Experiment 1b/Models/PRM3.Rda")
  write.csv(round(coef(summary(PRM3)),2), 'Experiment 1b/Models/PR_regOUT.csv')
}else{
  load("Experiment 1b/Models/PRM3.Rda")
  summary(PRM3)
}

effect('prev', PRM3)
effect('deg', PRM3)
effect('prev:deg', PRM3)


###---------------------------------------------
### Additional analysis requested by Reviewer 4:
###---------------------------------------------

which_subs<- c(8,9,13,15,19,22,23,49,60)

# map subjects who were NOT aware by degradation changes (1= aware, 0= not aware):
data$DC_deg<- ifelse(is.element(data$subj, which_subs), 1, 0)


# calculate means:
library(reshape2)
DesDF2<- melt(data, id=c('subj', 'item', 'cond', 'prev', 'deg', 'DC_deg'), 
            measure=c("FFD_N1", "SFD_N1", "GD_N1") , na.rm=TRUE)

mF2<- cast(DesDF2, prev+deg+DC_deg ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

df2<- rbind(mF2[,c(1:3)], mF2[,c(1:3)], mF2[,c(1:3)])
df2$Mean<- c(mF2$FFD_N1_M, mF2$SFD_N1_M, mF2$GD_N1_M)
df2$SD<- c(mF2$FFD_N1_SD, mF2$SFD_N1_SD, mF2$GD_N1_SD)
df2$Measure<- c(rep("FFD", 16), rep("SFD", 16), rep("GD", 16))
df2$SE<- NA
df2$SE[which(df2$DC_deg==0)]<- df2$SD[which(df2$DC_deg==0)]/sqrt(length(unique(data$subj))- length(which_subs))
df2$SE[which(df2$DC_deg==1)]<- df2$SD[which(df2$DC_deg==1)]/sqrt(length(which_subs))


colnames(df2)<- c("Preview", "Degradation", "DC_deg", "Mean", "SD", "Measure", "SE")

df2$Preview<- as.factor(df2$Preview)
df2$Preview<- factor(df2$Preview, levels= c("valid", 'phon', 'orth', 'invalid'))
levels(df2$Preview)<- c("valid", 'phon', 'orth', 'mask')
df2$Measure<- as.factor(df2$Measure)
df2$Measure<- factor(df2$Measure, levels= c('FFD', 'SFD', 'GD'))

df2$Degradation<- as.factor(df2$Degradation)
levels(df2$Degradation)<- c(" 0 %", " 20 %")

df2$DC_deg<- as.factor(df2$DC_deg)
levels(df2$DC_deg)<- c("noticed degradation", "did not notice degradation")



### graph:
library(ggplot2)
limits <- aes(ymax = df2$Mean + df2$SE, ymin=df2$Mean - df2$SE)


DCplot<- ggplot(data= df2, aes(x=Preview, y= Mean, color=Degradation,
                             fill= Degradation, group=Degradation, shape=Degradation,
                             linetype=Degradation, ymax = Mean + SE, ymin= Mean - SE))+ 
  #scale_y_continuous(breaks=c(200, 250, 300, 350, 400, 450))+
  scale_fill_brewer(palette="Dark2")+ scale_colour_brewer(palette="Dark2")+
  theme_bw(24) + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
                       axis.line = element_line(colour = "black", size=1),
                       panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
  geom_line(size=2)+ scale_y_continuous(limits = c(210, 360))+
  geom_point(size=6)+ 
  scale_shape_manual(values=c(15, 17))+
  xlab("Parafoveal preview of word N+1\n")+ ylab("Mean fixation duration")+ 
  theme(legend.position=c(0.15, 0.88), legend.title=element_text(size=26, face="bold", family="serif"),
        legend.text=element_text(size=26,family="serif"),legend.key.width=unit(2,"cm"),
        legend.key.height=unit(1,"cm"), strip.text=element_text(size=26, family="serif"),
        title=element_text(size=26, family="serif"),
        axis.title.x = element_text(size=26, face="bold", family="serif"), 
        axis.title.y = element_text(size=26, face="bold", family="serif"), 
        axis.text=element_text(size=26, family="serif"), 
        panel.border = element_rect(linetype = "solid", colour = "black"), 
        legend.key = element_rect(colour = "#000000", size=1),
        plot.title = element_text(hjust = 0.5))+
  facet_grid(DC_deg ~ Measure) + theme(strip.text.x = element_text(size = 22,  face="bold",family="serif"),
                                 strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
                                 legend.key = element_rect(colour = "#000000", size=1)) + geom_ribbon(alpha=0.10, 
                                                                                                      colour=NA) + ggtitle("Experiment 1b\n[target and rest of sentence degraded]")

DCplot


ggsave(filename = "Experiment 1b/Plots/R4_plot.pdf", width = 12, height = 12)


### statistical analysis

library(lme4)

contrasts(data$deg)
contrasts(data$prev)

data$DC_deg<- as.factor(data$DC_deg)
contrasts(data$DC_deg)<- c(1, -1)
contrasts(data$DC_deg)

if(!file.exists("Experiment 1b/Models/R4_LM.Rda")){
 
  #save(FFDN1, file= "Experiment 1b/Models/FFDN1.Rda")
}

summary(R4_LM<- lmer(log(FFD_N1)~prev*deg*DC_deg+ (1|subj)+ (1|item), REML = T, data=data))

summary(R4_LM<- lmer(log(SFD_N1)~prev*deg*DC_deg+ (1|subj)+ (1|item), REML = T, data=data))

summary(R4_LM<- lmer(log(GD_N1)~prev*deg*DC_deg+ (1|subj)+ (1|item), REML = T, data=data))



