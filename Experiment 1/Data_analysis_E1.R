
# Martin R. Vasilev, 2018

# EXPERIMENT 1:

rm(list=ls())


### MEANS:

load('Experiment 1/data/means.Rda')
load('Experiment 1/data/data.Rda')

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

ggsave("Experiment 1/Plots/TW.png", Dplot, width= 14, height=8, units= "in", dpi=200)
ggsave("Experiment 1/Plots/TW.pdf", Dplot, width= 12, height=8, units= "in")

E1plot<- Dplot
save(E1plot, file= "Experiment 1/Plots/TW.Rda")

#ggsave("Plots/TW.tiff", Dplot, width= 15, height=7, units= "in")


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
load("Experiment 1/data/quest_accuracy.Rda")

q$deg<- as.factor(q$deg)
contrasts(q$deg)<- c(1,-1)
contrasts(q$deg)

q$prev<- as.factor(q$prev)
q$prev<- factor(q$prev, levels= c("valid", "invalid", "phon", "orth"))
contrasts(q$prev)<- inv.cmat
contrasts(q$prev)

if(!file.exists("Experiment 1/Models/G1.Rda")){
  # does not converge with any random effects
  G1<- glmer(accuracy ~ deg* prev + (1|subject)+ (1|item), family= binomial, data= q,
             glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
  save(G1, file= "Experiment 1/Models/G1.Rda")
  summary(G1)
}else{
  load("Experiment 1/Models/G1.Rda")
  summary(G1)
}
max(abs(unname(coef(summary(G1))[2:8,3])))


###### TARGET WORD ANALYSIS:
library(effects)

# FFD:
if(!file.exists("Experiment 1/Models/FFDN1.Rda")){
  # Does not converge with preview slopes and deg slope for items (variance too low)
  summary(FFDN1<- lmer(log(FFD_N1)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(FFDN1, file= "Experiment 1/Models/FFDN1.Rda")
}else{
  load("Experiment 1/Models/FFDN1.Rda")
  summary(FFDN1)
}

(SFFD<- round(coef(summary(FFDN1)),2))
write.csv(SFFD, "Experiment 1/Models/TW_FFD.csv")


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
if(!file.exists("Experiment 1/Models/SFDN1.Rda")){
  # Does not converge with a preview slope for items
  summary(SFDN1<- lmer(log(SFD_N1)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(SFDN1, file= "Experiment 1/Models/SFDN1.Rda")
}else{
  load("Experiment 1/Models/SFDN1.Rda")
  summary(SFDN1)
}

(SSFD<- round(coef(summary(SFDN1)),2))
write.csv(SSFD, "Experiment 1/Models/TW_SFD.csv")


# GD:
if(!file.exists("Experiment 1/Models/GDN1.Rda")){
  summary(GDN1<-lmer(log(GD_N1)~prev*deg+ (deg|subj)+ (1|item), REML = T, data=data))
  save(GDN1, file= "Experiment 1/Models/GDN1.Rda")
}else{
  load("Experiment 1/Models/GDN1.Rda")
  summary(GDN1)
}

(SGD<- round(coef(summary(GDN1)),2))
write.csv(SGD, file= "Experiment 1/Models/TW_GD.csv")


## SFD doesn't converge with all random slopes; we remove the prev slope for items
#summary(SFDN1<-lmer(log(SFD_N1)~prev*deg+ (deg+prev|subj)+ (deg|item), REML = T, data=data))





#data$GD_N_c<- scale(data$GD_N)

#summary(FFDN1ph<- lmer(log(FFD_N1)~prev*deg*GD_N_c+ (deg+prev|subj)+ (deg|item), REML = T, data=data))
#summary(GDN1ph<- lmer(log(GD_N1)~prev*deg*GD_N_c+ (deg+prev|subj)+ (deg|item), REML = T, data=data))


# Skipping probability:

data$skip<- NA
for(i in 1:nrow(data)){
  if(data$nFix_N1[i]==0){
    data$skip[i]<- 1
  }else{
    data$skip[i]<- 0
  }
}


library(reshape)

DesSkip<- melt(data, id=c('subj', 'item', 'cond', 'prev', 'deg'), 
                measure=c("skip") , na.rm=TRUE)

mSkip<- cast(DesSkip, prev+deg ~ variable
              , function(x) c(M=signif(mean(x),3)
                              , SD= sd(x) ))

if(!file.exists("Experiment 1/Models/GMskip.Rda")){
  summary(GMskip<- glmer(skip ~ prev*deg+ (prev|subj)+ (prev|item), family= binomial, data=data))
  save(GMskip, file= "Experiment 1/Models/GMskip.Rda")
}else{
  load("Experiment 1/Models/GMskip.Rda")
  summary(GMskip)
}



########## Post-hoc analysis:
# Preview benefit as a function of distance from boundary:

# GD:
if(!file.exists("Experiment 1/Models/PGDN1.Rda")){
  summary(PGDN1<-lmer(log(GD_N1)~prev*deg*dist_Bnd+ (prev+deg|subj)+ (prev+deg|item), REML = T, data=data))
  save(PGDN1, file= "Experiment 1/Models/PGDN1.Rda")
}else{
  load("Experiment 1/Models/PGDN1.Rda")
}


library(effects)
plot(effect('prev:dist_Bnd', PGDN1))
plot(effect('prev:deg', PGDN1))

# FFD:
summary(PFFDN1<- lmer(log(FFD_N1)~prev*deg*dist_Bnd+ (1|subj)+ (1|item), REML = T, data=data))




##########################################################
data2<- subset(data, prev=="orth"| prev== "invalid")
data2$prev<- droplevels(data2$prev)
contrasts(data2$deg)
contrasts(data2$prev)<- c(-1,1)


summary(lmer(log(GD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=data2))
summary(lmer(log(SFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=data2))
summary(lmer(log(FFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=data2))


data3<- subset(data, deg!='20')
data3<- subset(data3, prev=="orth"| prev== "phon")
data3$prev<- droplevels(data3$prev)
(contrasts(data3$prev)<- c(-1,1))


summary(lmer(log(GD_N1)~prev+ (prev|subj)+ (prev|item), REML = T, data=data3))
summary(lmer(log(SFD_N1)~prev+ (1|subj)+ (1|item), REML = T, data=data3))
summary(lmer(log(FFD_N1)~prev+ (1|subj)+ (1|item), REML = T, data=data3))


######################################################
#               Global reading measures:             #
######################################################

#------------------------------------------------------
# Sentence reading time as a function of degradation: #
#------------------------------------------------------
load("Experiment 1/data/t.Rda")

library(reshape)
DesRT<- melt(t, id=c('subj', 'item', 'cond', 'deg'), 
                  measure=c('duration_ms') , na.rm=TRUE)

mRT<- cast(DesRT, deg ~ variable
                , function(x) c(M=signif(mean(x),3)
                                , SD= sd(x) ))

t$deg<- as.factor(t$deg)
contrasts(t$deg)<- c(1, -1)
contrasts(t$deg)

if(!file.exists("Experiment 1/Models/GR1.Rda")){
  summary(GR1<- lmer(log(duration_ms)~deg+ (deg|subj)+ (1|item), REML = T, data=t))
  save(GR1, file= "Experiment 1/Models/GR1.Rda")
} else{
  load("Experiment 1/Models/GR1.Rda")
  summary(GR1)
}

round(coef(summary(GR1)), 2)

#----------------------
# fixations duration: #
#----------------------
load("Experiment 1/data/raw_fix.Rda")

library(reshape)
DesFix<- melt(raw_fix, id=c('sub', 'item', 'cond', 'deg'), 
             measure=c('fix_dur') , na.rm=TRUE)

mFix<- cast(DesFix, deg ~ variable
           , function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

raw_fix$deg<- as.factor(raw_fix$deg)
contrasts(raw_fix$deg)<- c(1, -1)
contrasts(raw_fix$deg)


if(!file.exists("Experiment 1/Models/GR2.Rda")){
  summary(GR2<- lmer(log(fix_dur)~deg+ (deg|sub)+ (1|item), REML = T, data=raw_fix))
  save(GR2, file= "Experiment 1/Models/GR2.Rda")
} else{
  load("Experiment 1/Models/GR2.Rda")
  summary(GR2)
}

round(coef(summary(GR2)), 3)


#-----------------------
# number of fixations: #
#-----------------------
load("Experiment 1/data/FX.Rda")

DesFX<- melt(FX, id=c('sub', 'item', 'cond', 'deg'), 
              measure=c('nfix') , na.rm=TRUE)

mFX<- cast(DesFX, deg ~ variable
            , function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

FX$deg<- as.factor(FX$deg)
contrasts(FX$deg)<- c(1, -1)
contrasts(FX$deg)


if(!file.exists("Experiment 1/Models/GR3.Rda")){
  summary(GR3<- lmer(nfix~deg+ (deg|sub)+ (1|item), REML = T, data=FX))
  save(GR3, file= "Experiment 1/Models/GR3.Rda")
}else{
  load("Experiment 1/Models/GR3.Rda")
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


if(!file.exists("Experiment 1/Models/GR4.Rda")){
  summary(GR4<- lmer(sacc_len~deg+ (1|sub)+ (1|item), REML = T, data=raw_fix))
  save(GR4, file= "Experiment 1/Models/GR4.Rda")
}else{
  load("Experiment 1/Models/GR4.Rda")
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

ggsave("Experiment 1/Plots/Gen.png", Dplot, width= 12, height=5, units= "in", dpi=300)


#################################################################################################################
#                                           Prediction graph                                                    #
#################################################################################################################

#------------------------#
#  Prediction - Panel 1  #
#------------------------#

library(pBrackets)
colours<- c('darkred', 'darkblue', '#8a8a8a', '#decfa0')

pdf('Experiment 1/Plots/pred_plot.pdf', width = 10, height = 10, paper='special')
layout(mat = matrix(c(1,2,3,4,5,6),nrow = 3,ncol = 2,byrow = TRUE), heights = c(0.4,0.2, 0.4))
par(mar=c(4,4,4,4))

plot(NA, type= 'l', ylab= "", xlab= "Preview", lwd=2, cex.main=2,family="serif",
     xlim = c(1, 2), ylim = c(200, 265), yaxt='n', xaxt='n', main= 'Predicted [Exp.1]', cex.main=2, cex.lab=2)

axis(1, at= c( 1.2, 1.8), labels = c('valid', 'invalid'), cex.axis=2, family="serif")

mtext(text= 'FFD', side = 2, line = 0.5, family="serif", cex = 2)


# Plot data points
deg0<- c(220, 250)
deg20<- c(240, 240)

# 0% degradation:
lines(y = deg0, x= c(1.2, 1.8), lwd=3, lty= 1, col= colours[1])
points(y = deg0, x= c(1.2, 1.8), pch= 16, cex=2, col= colours[1])

# 20% degradation:
lines(y = deg20, x= c(1.2, 1.8), lwd=3, lty= 2, col= colours[2])
points(y = deg20, x= c(1.2, 1.8), pch= 17, cex=2, col= colours[2])

# Cost & benefit labels:
# benefit:
brackets(x1 = 1.165, y1 = deg0[1], x2 = 1.165, y2 = deg20[1], h = 0.08, lwd=2, col = colours[3])
text(x = 1.05, y = 230, labels= 'preview benefit', srt= 90, family="serif", cex = 1.85, col = colours[3])

# cost:
brackets(x1 = 1.835, y1 = deg0[2], x2 = 1.835, y2 = deg20[2], h = 0.06, lwd=2, col = colours[3])
text(x = 1.925, y = mean(c(deg0[2], deg20[2])), labels= 'preview cost', srt= -90, family="serif", cex = 1.85, col = colours[3])

# legend:
op <- par(family = "serif")
legend(x = 1.7, y = 220, title = expression(bold('Degradation')), legend=c("0 %", "20 %"), col = colours[1:2], lwd= 2,
       lty = c(1,2), pch = c(16, 17), seg.len=2.5, cex = 1.6, box.col = "white")

#------------------------#
#  Obserced - Panel 1    #
#------------------------#
plot(NA, type= 'l', ylab= "", xlab= "Preview", lwd=2, cex.main=2,family="serif",
     xlim = c(1, 2), ylim = c(200, 275), yaxt='n', xaxt='n', main= 'Observed [Exp.1]', cex.main=2, cex.lab=2)
axis(1, at= c( 1.2, 1.8), labels = c('valid', 'invalid'), cex.axis=2, family="serif")
#------------------------#
#  Prediction - Panel 2  #
#------------------------#

plot(NA, type= 'n', ylab= "", xlab= "", lwd=2, cex.main=2,family="serif",
     xlim = c(1, 2), ylim = c(200, 275), axes=FALSE,ann=FALSE, main= '+', cex.main=2, cex.lab=2)
#axis(1, at= c( 1.2, 1.8), labels = c('valid', 'invalid'), cex.axis=2, family="serif")

# Draw Uniform preview cost
rect(xleft = 1.2, ybottom = 210, xright = 1.8, ytop = 260, col = colours[4])
#text(x = 1.5, y = 245, labels= 'display change awareness cost', srt= 0, family="serif", cex = 1.85, col = colours[3])

mtext(text= '+', side = 3, line = 0, family="serif", cex = 2, font = 2)
par(xpd = TRUE) 
text(x = 1.5, y = 190, labels= '=', srt= 90, family="serif", cex = 3.5, font=2)

brackets(x1 = 1.17, y1 = 210, x2 = 1.17, y2 = 260, h = 0.06, lwd=2, col = colours[3])

text(x = 1.02, y = mean(c(210, 260)), labels= 'display change', srt= 90, family="serif", cex = 1.85, col = colours[3])
text(x = 1.08, y = mean(c(210, 260)), labels= 'awareness cost', srt= 90, family="serif", cex = 1.85, col = colours[3])

#------------------------#
#  Observed - Panel 2    #
#------------------------#
# blank plot:
plot.new()


#------------------------#
#  Predicted - Panel 3   #
#------------------------#
plot(NA, type= 'l', ylab= "", xlab= "Preview", lwd=2, cex.main=2,family="serif",
     xlim = c(1, 2), ylim = c(200, 265), yaxt='n', xaxt='n', main= 'Predicted [Exp.2]', cex.main=2, cex.lab=2)

axis(1, at= c( 1.2, 1.8), labels = c('valid', 'invalid'), cex.axis=2, family="serif")

mtext(text= 'FFD', side = 2, line = 0.5, family="serif", cex = 2)

rect(xleft = 1.2, ybottom = deg20[1], xright = 1.8, ytop = deg20[1]+15, col = colours[4], border = NA)
arrows(x0 = 1.5, y0 = deg20[1], x1 = 1.5, y1 = deg20[1]+10, length = 0.1, angle = 30, col = colours[2], lty = 1, lwd=1)
lines(x = c(1.2, 1.8), y= c(deg20[1], deg20[1]), lty= 2, col= colours[2])


# 0% degradation:
lines(y = deg0, x= c(1.2, 1.8), lwd=3, lty= 1, col= colours[1])
points(y = deg0, x= c(1.2, 1.8), pch= 16, cex=2, col= colours[1])

# 20% degradation:
lines(y = deg20+15, x= c(1.2, 1.8), lwd=3, lty= 2, col= colours[2])
points(y = deg20+15, x= c(1.2, 1.8), pch= 17, cex=2, col= colours[2])



dev.off()



##########################################################################################################
#                                             OLD STUFF:                                                 #
##########################################################################################################

# #-----------
# # TVT alone:
# #-----------
# 
# limits <- aes(ymax = dT$Mean + dT$SE, ymin=dT$Mean - dT$SE)
# 
# 
# Dplot2<- ggplot(data= dT, aes(x=Preview, y= Mean, color=Degradation, fill= Degradation, group=Degradation, shape=Degradation, linetype=Degradation))+ 
#   #scale_y_continuous(breaks=c(200, 250, 300, 350, 400, 450))+
#   scale_fill_brewer(palette="Dark2")+ scale_colour_brewer(palette="Dark2")+
#   theme_bw() + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
#                      axis.line = element_line(colour = "black", size=1),
#                      panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
#   geom_line(size=2)+
#   geom_point(size=7)+ ggtitle("TVT")+
#   xlab("\n Parafoveal preview of word N+1")+ ylab("Mean fixation duration")+ 
#   theme(legend.position=c(0.15, 0.85), legend.title=element_text(size=20, face="bold", family="serif"), legend.text=element_text(size=20,family="serif"),legend.key.width=unit(2,"cm"),
#         legend.key.height=unit(1,"cm"), strip.text=element_text(size=20, family="serif"),
#         title=element_text(size=20, family="serif"),
#         axis.title.x = element_text(size=20, face="bold", family="serif"), axis.title.y = element_text(size=20, face="bold", family="serif"), 
#         axis.text=element_text(size=20, family="serif"), 
#         panel.border = element_rect(linetype = "solid", colour = "black"), 
#         legend.key = element_rect(colour = "#000000", size=1))+
#         geom_ribbon(limits, alpha=0.10, colour=NA)
# 
# ggsave("Experiment 1/Plots/TVT.png", Dplot2, width= 8, height=8, units= "in", dpi=600)
# 
# save(Dplot, file= "Experiment 1/Output/Dplot.Rda")
# save(Dplot2, file= "Experiment 1/Output/Dplot2.Rda")
# save(df, file= "Experiment 1/Output/df.Rda")
# 
# 
# 
# library(reshape)
# 
# DesRefix<- melt(data, id=c('subj', 'item', 'cond', 'prev', 'deg'), 
#                 measure=c("pRefixN1") , na.rm=TRUE)
# 
# mRefix<- cast(DesRefix, prev+deg ~ variable
#               , function(x) c(M=signif(mean(x),3)
#                               , SD= sd(x) ))
# 
# write.table(mRefix, file= 'Experiment 1/RefixN1.txt', sep = "\t")
# 
# 
# ### N+1 refixation graph:
# colnames(mRefix)<- c('Parafoveal preview', 'Degradation', 'Mean', 'SD')
# mRefix$SE<- mRefix$SD/sqrt(length(unique(data$subj)))
# mRefix$`Parafoveal preview`<- as.factor(mRefix$`Parafoveal preview`)
# mRefix$`Parafoveal preview`<- factor(mRefix$`Parafoveal preview`, levels= c('valid', 'phon', 'orth', 'invalid'))
# 
# 
# limits <- aes(ymax = mRefix$Mean + mRefix$SE, ymin=mRefix$Mean - mRefix$SE)
# 
# 
# Dplot3<- ggplot(data= mRefix, aes(x=`Parafoveal preview`, y= Mean, color=Degradation, fill= Degradation,
#                                   group=Degradation, shape=Degradation, linetype=Degradation))+ 
#   #scale_y_continuous(breaks=c(200, 250, 300, 350, 400, 450))+
#   scale_fill_brewer(palette="Dark2")+ scale_colour_brewer(palette="Dark2")+
#   theme_bw() + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
#                      axis.line = element_line(colour = "black", size=1),
#                      panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
#   geom_line(size=2)+
#   geom_point(size=7)+ 
#   xlab("\n Parafoveal preview of word N+1")+ ylab("Mean re-fixation probability (1st-pass)")+ 
#   theme(legend.position=c(0.18, 0.85), legend.title=element_text(size=20, face="bold", family="serif"),
#         legend.text=element_text(size=20,family="serif"),legend.key.width=unit(2,"cm"),
#         legend.key.height=unit(1,"cm"), strip.text=element_text(size=20, family="serif"),
#         title=element_text(size=20, family="serif"),
#         axis.title.x = element_text(size=20, face="bold", family="serif"), axis.title.y = element_text(size=20, face="bold", family="serif"), 
#         axis.text=element_text(size=20, family="serif"), 
#         panel.border = element_rect(linetype = "solid", colour = "black"), 
#         legend.key = element_rect(colour = "#000000", size=1))+
#   geom_ribbon(limits, alpha=0.10, colour=NA)
# 
# ggsave("Experiment 1/Plots/pRefix.png", Dplot3, width= 8, height=8, units= "in", dpi=200)

# 
# #-----------------------
# # Pre-boundary fixation:
# #-----------------------
# 
# library(reshape)
# 
# DesPbnd<- melt(data, id=c('subj', 'item', 'cond', 'prev', 'deg'), 
#                measure=c("preBndFix") , na.rm=TRUE)
# 
# mPbnd<- cast(DesPbnd, prev+deg ~ variable
#              , function(x) c(M=signif(mean(x),3)
#                              , SD= sd(x) ))
# colnames(mPbnd)<- c('Parafoveal preview', 'Degradation', 'Mean', 'SD')
# mPbnd$SE<- mPbnd$SD/sqrt(length(unique(data$subj)))
# mPbnd$`Parafoveal preview`<- as.factor(mPbnd$`Parafoveal preview`)
# mPbnd$`Parafoveal preview`<- factor(mPbnd$`Parafoveal preview`, levels= c('valid', 'phon', 'orth', 'invalid'))
# 
# 
# limits <- aes(ymax = mPbnd$Mean + mPbnd$SE, ymin=mPbnd$Mean - mPbnd$SE)
# 
# 
# Dplot4<- ggplot(data= mPbnd, aes(x=`Parafoveal preview`, y= Mean, color=Degradation, fill= Degradation,
#                                  group=Degradation, shape=Degradation, linetype=Degradation))+ 
#   #scale_y_continuous(breaks=c(200, 250, 300, 350, 400, 450))+
#   scale_fill_brewer(palette="Dark2")+ scale_colour_brewer(palette="Dark2")+
#   theme_bw() + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
#                      axis.line = element_line(colour = "black", size=1),
#                      panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
#   geom_line(size=2)+
#   geom_point(size=7)+ 
#   xlab("\n Parafoveal preview of word N+1")+ ylab("Mean Pre-boundary fixation duration")+ 
#   theme(legend.position=c(0.18, 0.85), legend.title=element_text(size=20, face="bold", family="serif"),
#         legend.text=element_text(size=20,family="serif"),legend.key.width=unit(2,"cm"),
#         legend.key.height=unit(1,"cm"), strip.text=element_text(size=20, family="serif"),
#         title=element_text(size=20, family="serif"),
#         axis.title.x = element_text(size=20, face="bold", family="serif"), axis.title.y = element_text(size=20, face="bold", family="serif"), 
#         axis.text=element_text(size=20, family="serif"), 
#         panel.border = element_rect(linetype = "solid", colour = "black"), 
#         legend.key = element_rect(colour = "#000000", size=1))+
#   geom_ribbon(limits, alpha=0.10, colour=NA)
# 
# ggsave("Experiment 1/Plots/Pre-boundary_fix.png", Dplot4, width= 8, height=8, units= "in", dpi=200)
# 
# 
# #-----------------------
# # Pre-boundary location:
# #-----------------------
# library(reshape)
# 
# DesPbndLoc<- melt(subset(data, dist_Bnd>0), id=c('subj', 'item', 'cond', 'prev', 'deg'), 
#                   measure=c("dist_Bnd") , na.rm=TRUE)
# 
# mPbndLoc<- cast(DesPbndLoc, prev+deg ~ variable
#                 , function(x) c(M=signif(mean(x),3)
#                                 , SD= sd(x) ))
# 
# colnames(mPbndLoc)<- c('Parafoveal preview', 'Degradation', 'Mean', 'SD')
# mPbndLoc$SE<- mPbndLoc$SD/sqrt(length(unique(data$subj)))
# mPbndLoc$`Parafoveal preview`<- as.factor(mPbndLoc$`Parafoveal preview`)
# mPbndLoc$`Parafoveal preview`<- factor(mPbndLoc$`Parafoveal preview`, levels= c('valid', 'phon', 'orth', 'invalid'))
# 
# 
# limits <- aes(ymax = mPbndLoc$Mean + mPbndLoc$SE, ymin=mPbndLoc$Mean - mPbndLoc$SE)
# 
# 
# Dplot5<- ggplot(data= mPbndLoc, aes(x=`Parafoveal preview`, y= Mean, color=Degradation, fill= Degradation,
#                                     group=Degradation, shape=Degradation, linetype=Degradation))+ 
#   #scale_y_continuous(breaks=c(200, 250, 300, 350, 400, 450))+
#   scale_fill_brewer(palette="Dark2")+ scale_colour_brewer(palette="Dark2")+
#   theme_bw() + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
#                      axis.line = element_line(colour = "black", size=1),
#                      panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
#   geom_line(size=2)+
#   geom_point(size=7)+ 
#   xlab("\n Parafoveal preview of word N+1")+ ylab("Mean distance (in letters) before crossing boundary")+ 
#   theme(legend.position=c(0.18, 0.85), legend.title=element_text(size=20, face="bold", family="serif"),
#         legend.text=element_text(size=20,family="serif"),legend.key.width=unit(2,"cm"),
#         legend.key.height=unit(1,"cm"), strip.text=element_text(size=20, family="serif"),
#         title=element_text(size=20, family="serif"),
#         axis.title.x = element_text(size=20, face="bold", family="serif"), axis.title.y = element_text(size=20, face="bold", family="serif"), 
#         axis.text=element_text(size=20, family="serif"), 
#         panel.border = element_rect(linetype = "solid", colour = "black"), 
#         legend.key = element_rect(colour = "#000000", size=1))+
#   geom_ribbon(limits, alpha=0.10, colour=NA)
# 
# ggsave("Experiment 1/Plots/Pre-boundary_loc.png", Dplot5, width= 8, height=8, units= "in", dpi=200)
# 

