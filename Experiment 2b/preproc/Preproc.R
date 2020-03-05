
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


data_dir= "D:/Data/E2DEG" # Martin

###########################
# Comprehension accuracy: #
###########################

if(!file.exists("Experiment 2b/data/Quest.Rda")){
  Quest<- Question(data_list = data_dir, maxtrial = 138)
  save(Quest, file= "Experiment 2b/data/Quest.Rda")
  write.csv(Quest, "Experiment 2b/data/Quest.csv")
} else{
  load("Experiment 2b/data/Quest.Rda")
}


library(reshape)
DesQuest<- melt(Quest, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuest

min(mQuest$accuracy_M)
mean(mQuest$accuracy_M)
sd(mQuest$accuracy_M)


################
# Trial times: #
################

if(!file.exists("Experiment 2b/data/Trial_time.Rda")){
  
  Trialt<- trialTime(data_list = data_dir, maxtrial = 138)
  save(Trialt, file= "Experiment 2b/data/Trial_time.Rda")
  write.csv(Trialt, "Experiment 2b/data/Trial_time.csv")
}else{
  load("Experiment 2b/data/Trial_time.Rda")
}

DesTime<- melt(Trialt, id=c('sub', 'item', 'cond'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, cond ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
mTime


##################
# Raw Fixations: #
##################

if(!file.exists("Experiment 2b/preproc/raw_fix.Rda")){
  
  # extract raw data & merge it with da1 files:
  raw_fix<- SingleLine(data_list = data_dir, ResX = 1024, ResY = 768, maxtrial = 138, tBlink = 100,
                       addNonFixatedWords = T)
  save(raw_fix, file= "Experiment 2b/preproc/raw_fix.Rda")
  write.csv(raw_fix, file= "Experiment 2b/preproc/raw_fix.csv")
}else{
  load("Experiment 2b/preproc/raw_fix.Rda")
}


####################
# Boundary change: #
####################

DC<- Boundary(data_list = data_dir, maxtrial = 138)

length(which(DC$tChangetoFixOnset>5))/nrow(DC)
  
save(DC, file= 'Experiment 3b/preproc/DC.Rda')
write.csv(DC, file= 'Experiment 3b/preproc/DC.csv')

##############################
# Preprocessing of raw data: #
##############################

# merge fixations shorter than 80 ms within 1 char of another fixation:
rf<- cleanData(raw_fix = raw_fix, removeOutsideText = T, removeBlinks = T, combineNearbySmallFix =T,
               combineMethod = 'char', combineDist = 1, removeSmallFix = T, smallFixCutoff = 80, 
               removeOutliers = F)

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

sent <- read.delim("Experiment 3b/Corpus/Corpus2.txt")

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

# target:
out<- which(N1$FFD>800 | N1$SFD>800 |N1$GD>1600)
N1<- N1[-out,]

# pre-target:
out2<- which(N$FFD>800 | N$SFD>800 |N$GD>1600)
N<- N[-out2,]

save(N, file= 'Experiment 3b/data/pre-target.Rda')
write.csv(N, 'Experiment 3b/data/pre-target.csv')

save(N1, file= 'Experiment 3b/data/target.Rda')
write.csv(N1, 'Experiment 3b/data/target.csv')


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
  ggtitle("Experiment 3b")

Dplot

