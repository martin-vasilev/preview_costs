
# Martin R. Vasilev, 2019

rm(list= ls())


# Install/ load R package used in preprocessing:

if('EMreading' %in% rownames(installed.packages())==FALSE){
  if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
  install_github('martin-vasilev/EMreading')
}else{
  library(EMreading)
}


data_dir= "D:/Data/E1DEG" # Martin

###########################
# Comprehension accuracy: #
###########################

if(!file.exists("Experiment 2a/data/Quest2a.Rda")){
  Quest2a<- Question(data_list = data_dir, maxtrial = 138)
  
  for(i in 1:nrow(Quest2a)){
    # degradation:
    if(Quest2a$cond[i]<4){
      Quest2a$deg[i]<- '0'
    }else{
      Quest2a$deg[i]<- '20'
    }
    
    # preview:
    if(is.element(Quest2a$cond[i], c(1,4))){
      Quest2a$prev[i]<- 'valid'
    }
    
    if(is.element(Quest2a$cond[i], c(2,5))){
      Quest2a$prev[i]<- 'orth'
    }
    
    if(is.element(Quest2a$cond[i], c(3,6))){
      Quest2a$prev[i]<- 'mask'
    }
    
  }
  
  save(Quest2a, file= "Experiment 2a/data/Quest2a.Rda")
  write.csv(Quest2a, "Experiment 2a/data/Quest2a.csv")
} else{
  load("Experiment 2a/data/Quest2a.Rda")
}


library(reshape)
DesQuest<- melt(Quest2a, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuest

min(mQuest$accuracy_M)
max(mQuest$accuracy_M)
mean(mQuest$accuracy_M)
sd(mQuest$accuracy_M)


##################
# Raw Fixations: #
##################

if(!file.exists("Experiment 2a/preproc/raw_fix.Rda")){
  
  # extract raw data & merge it with da1 files:
  raw_fix<- SingleLine(data_list = data_dir, ResX = 1024, ResY = 768, maxtrial = 138, tBlink = 100,
                       addNonFixatedWords = T)
  save(raw_fix, file= "Experiment 2a/preproc/raw_fix.Rda")
  write.csv(raw_fix, file= "Experiment 2a/preproc/raw_fix.csv")
}else{
  load("Experiment 2a/preproc/raw_fix.Rda")
}


##############################
# Preprocessing of raw data: #
##############################

# merge fixations shorter than 80 ms within 1 char of another fixation:
rf<- cleanData(raw_fix = raw_fix, removeOutsideText = F, removeBlinks = F, combineNearbySmallFix =T,
               combineMethod = 'char', combineDist = 1, removeSmallFix = F, smallFixCutoff = 80, 
               removeOutliers = F)

# remove short (non-merged fixations):
rf<- rf[-which(rf$fix_dur<80), ] 


# calculate word FD measures
FD<- wordMeasures(rf)
FD$RS<- NULL
FD$RS_type<- NULL

FD$deg<- NULL
FD$prev<- NULL

for(i in 1:nrow(FD)){
  # degradation:
  if(FD$cond[i]<4){
    FD$deg[i]<- '0'
  }else{
    FD$deg[i]<- '20'
  }
  
  # preview:
  if(is.element(FD$cond[i], c(1,4))){
    FD$prev[i]<- 'valid'
  }
  
  if(is.element(FD$cond[i], c(2,5))){
    FD$prev[i]<- 'orth'
  }
  
  if(is.element(FD$cond[i], c(3,6))){
    FD$prev[i]<- 'mask'
  }
  
}

sent <- read.delim("Experiment 2b/Corpus/Corpus2.txt")

for(i in 1:nrow(sent)){
  s<- as.character(sent$Sentence[i])
  words<- unlist(strsplit(s, ' '))
  TW<- words[sent$N1_pos[i]]
  
  if(TW== as.character(sent$Word[i])){
    cat('okay')
    cat('\n')
  }else{
    cat('NOT okay')
    cat('\n')
  }
}

FD$N1<- 0
FD$N<- 0

for(i in 1:nrow(FD)){
  n1_loc<- sent$N1_pos[FD$item[i]]
  n_loc<- sent$N_pos[FD$item[i]]
  
  if(FD$word[i]==n1_loc){
    FD$N1[i]<- 1
  }
  
  if(FD$word[i]==n_loc){
    FD$N[i]<- 1
  }
  
}

N<- subset(FD, N==1)
N1<- subset(FD, N1==1)

### Clean rf for global measures analysis:
rf<- rf[-which(rf$blink==1 | rf$prev_blink==1 | rf$after_blink==1), ]
table(rf$blink); table(rf$prev_blink); table(rf$after_blink)

rf$blink<- NULL; rf$prev_blink<- NULL; rf$after_blink<- NULL
rf$hasText<- NULL

rf<- rf[-which(rf$outOfBnds==1),]
rf$outOfBnds<- NULL

rf<- rf[-which(rf$fix_dur>800), ]

rf$prev<- NA
rf$deg<- NA

for(i in 1:nrow(rf)){
  # degradation:
  if(rf$cond[i]<4){
    rf$deg[i]<- '0'
  }else{
    rf$deg[i]<- '20'
  }
  
  # preview:
  if(is.element(rf$cond[i], c(1,4))){
    rf$prev[i]<- 'valid'
  }
  
  if(is.element(rf$cond[i], c(2,5))){
    rf$prev[i]<- 'orth'
  }
  
  if(is.element(rf$cond[i], c(3,6))){
    rf$prev[i]<- 'mask'
  }
  
}




## save raw data:
rf2a<- rf
save(rf2a, file= 'Experiment 2a/data/raw_fix2a.Rda')
write.csv(rf2a, 'Experiment 2a/data/raw_fix2a.csv')

library(reshape)
DesGen<- melt(rf2a, id=c('sub', 'item', 'cond', 'deg', 'prev'), 
             measure=c("fix_dur", "sacc_len"), na.rm=TRUE)
mG<- cast(DesGen, deg ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

nFix<- num_fix(rf2a)

nFix2a<- nFix
save(nFix2a, file= 'Experiment 2a/data/num_fix2a.Rda')
write.csv(nFix2a, 'Experiment 2a/data/num_fix2a.csv')

DesNfix<- melt(nFix2a, id=c('sub', 'item', 'cond', 'deg', 'prev'), 
              measure=c("Nfix_all"), na.rm=TRUE)
mNfix<- cast(DesNfix, deg ~ variable
          ,function(x) c(M=signif(mean(x),3)
                         , SD= sd(x) ))

#-------------------------------------
#### Trial time:
if(!file.exists("Experiment 2a/data/Trial_time2a.Rda")){
  
  load('Experiment 2a/data/raw_fix2a.Rda')
  
  Trialt2a<- trialTime(data_list = data_dir, maxtrial = 138)
  
  for(i in 1:nrow(Trialt2a)){
    # degradation:
    if(Trialt2a$cond[i]<4){
      Trialt2a$deg[i]<- '0'
    }else{
      Trialt2a$deg[i]<- '20'
    }
    
    # preview:
    if(is.element(Trialt2a$cond[i], c(1,4))){
      Trialt2a$prev[i]<- 'valid'
    }
    
    if(is.element(Trialt2a$cond[i], c(2,5))){
      Trialt2a$prev[i]<- 'orth'
    }
    
    if(is.element(Trialt2a$cond[i], c(3,6))){
      Trialt2a$prev[i]<- 'mask'
    }
    
  }
  
  Trialt2a$keep<- 0
  
  for(i in 1:nrow(Trialt2a)){
    a<- which(rf2a$sub== Trialt2a$sub[i] & rf2a$item== Trialt2a$item[i])
    
    if(length(a)>0){
      Trialt2a$keep[i]<- 1
    }
  }
  
  table(Trialt2a$keep)
  
  Trialt2a<- subset(Trialt2a, keep==1)
  
  Trialt2a$keep<- NULL
  
  
  save(Trialt2a, file= "Experiment 2a/data/Trial_time2a.Rda")
  write.csv(Trialt2a, "Experiment 2a/data/Trial_time2a.csv")
}else{
  load("Experiment 2a/data/Trial_time2a.Rda")
}

DesTime<- melt(Trialt2a, id=c('sub', 'item', 'deg'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, deg ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
mTime






##############################################################
# Target word pre-proc:

# remove trials with blinks:
blinksN<- which(N$blinks_1stPass==1)
blinksN1<- which(N1$blinks_1stPass==1)

B1<- (length(blinksN1)/ (138*60))*100

N<- N[- blinksN,]
N1<- N1[- blinksN1,]



# target:
out<- which(N1$FFD>800 | N1$SFD>800 |N1$GD>1600)
N1<- N1[-out,]

# pre-target:
out2<- which(N$FFD>800 | N$SFD>800 |N$GD>1600)
N<- N[-out2,]

Outliers<- (length(out)/ (136*60))*100


####################
# Boundary change: #
####################

source("Experiment 2a/preproc/BoundaryN.R")

if(!file.exists("Experiment 2a/preproc/DC.Rda")){
  DC<- BoundaryN(data_list = data_dir, maxtrial = 138) 
  save(DC, file= "Experiment 2a/preproc/DC.Rda")
}else{
  load("Experiment 2a/preproc/DC.Rda")
}

## remove trials that were already excluded due to blinks:

DC$blink2<- NA

for(i in 1:nrow(DC)){
  a<- which(N1$sub== DC$sub[i] & N1$item== DC$item[i])
  
  if(length(a)>0){
    DC$blink2[i]<- 0
  }else{
    DC$blink2[i]<- 1
  }
  
  if(length(a)>1){
    stop("lol")
  }
  
}

DCblink<- which(DC$blink==1 | DC$blink2==1)

Blinks<- (length(DCblink)/(138*60))*100

DC<- DC[-DCblink,]

DCbad<- which(DC$tChangetoFixOnset-1>6)
DC<- DC[-DCbad,]

DChook<- which(DC$hook==1)

DC<- DC[-DChook,]

Changes<- ((length(DCbad)+length(DChook))/(138*60))*100 

mean(DC$tChange)+ ((1/150)/2)*1000
sd(DC$tChange)


#### subset fixation data frames keeping only trials still in DC:
N1$keep<- NA

for(i in 1:nrow(N1)){
  a<- which(DC$sub== N1$sub[i] & DC$item== N1$item[i])
  
  if(length(a)>0){
    N1$keep[i]<- 1
  }else{
    N1$keep[i]<- 0
  }
  
  if(length(a)>1){
    stop("lol")
  }
  
}

table(N1$keep)
N1<- subset(N1, keep==1)


N$keep<- NA

for(i in 1:nrow(N)){
  a<- which(DC$sub== N$sub[i] & DC$item== N$item[i])
  
  if(length(a)>0){
    N$keep[i]<- 1
  }else{
    N$keep[i]<- 0
  }
  
  if(length(a)>1){
    stop("lol")
  }
  
}

table(N$keep)
N<- subset(N, keep==1)


# clear unnecessary columns:

N$blinks_1stPass<- NULL
N1$blinks_1stPass<- NULL

N$blinks_2ndPass<- NULL
N1$blinks_2ndPass<- NULL

N$word_line<- NULL
N1$word_line<- NULL

N$keep<- NULL
N1$keep<- NULL

N$N<- NULL
N1$N<- NULL

N$N1<- NULL
N1$N1<- NULL

N$line<- NULL
N1$line<- NULL

N_2a<- N
N1_2a<- N1

save(N_2a, file= 'Experiment 2a/data/pre-target.Rda')
write.csv(N_2a, 'Experiment 2a/data/pre-target.csv')

save(N1_2a, file= 'Experiment 2a/data/target.Rda')
write.csv(N1_2a, 'Experiment 2a/data/target.csv')


###################################

library(reshape)
DesN1<- melt(N1, id=c('sub', 'item', 'cond', 'deg', 'prev'), 
                measure=c("FFD", "SFD", "GD"), na.rm=TRUE)
mF<- cast(DesN1, deg+prev ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

df<- data.frame(c(mF$FFD_M, mF$SFD_M, mF$GD_M),
                c(mF$FFD_SD, mF$SFD_SD, mF$GD_SD),
                c(rep(mF$prev,3)), c(rep(mF$deg,3)),
                c(rep('FFD', 6), rep('SFD', 6), rep('GD', 6)))
colnames(df)<- c('Mean', 'SD', 'Preview', 'Degradation', 'Measure')
df$SE<- df$SD/sqrt(length(unique(N1$sub)))

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
  geom_line(size=2)+ #scale_y_continuous(limits = c(210, 320))+
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
                                                                                                      colour=NA) +
  ggtitle("Experiment 2b")

Dplot

