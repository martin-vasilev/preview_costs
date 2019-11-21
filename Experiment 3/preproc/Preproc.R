
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

#if(!file.exists("Experiment 3/data/Quest.Rda")){
  Quest<- Question(data_list = data_dir, maxtrial = 138)
  save(Quest, file= "Experiment 3/data/Quest.Rda")
  write.csv(Quest, "Experiment 3/data/Quest.csv")
#} else{
#  load("Experiment 3/data/Quest.Rda")
#}


library(reshape)
DesQuest<- melt(Quest, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuest


################
# Trial times: #
################

#if(!file.exists("Experiment 3/data/Trial_time.Rda")){
  
  Trialt<- trialTime(data_list = data_dir, maxtrial = 138)
  save(Trialt, file= "Experiment 3/data/Trial_time.Rda")
  write.csv(Trialt, "Experiment 3/data/Trial_time.csv")
#}else{
#  load("Experiment 3/data/Trial_time.Rda")
#}

DesTime<- melt(Trialt, id=c('sub', 'item', 'cond'), 
               measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, cond ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))
mTime


##################
# Raw Fixations: #
##################

#if(!file.exists("Experiment 3/preproc/raw_fix.Rda")){
  # extract raw data & merge it with da1 files:
  raw_fix<- SingleLine(data_list = data_dir, ResX = 1024, ResY = 768, maxtrial = 138, tBlink = 150)
  save(raw_fix, file= "Experiment 3/preproc/raw_fix.Rda")
  write.csv(raw_fix, file= "Experiment 3/preproc/raw_fix.csv")
#}


####################
# Boundary change: #
####################

DC<- Boundary(data_list = data_dir, maxtrial = 138)

length(which(DC$tChangetoFixOnset>5))/nrow(DC)
  
save(DC, file= 'Experiment 3/preproc/DC.Rda')
write.csv(DC, file= 'Experiment 3/preproc/DC.csv')

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

sent <- read.delim("D:/R/preview_costs/Experiment 3/Corpus/Corpus2.txt")

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

save(N, file= 'Experiment 3/data/pre-target.Rda')
write.csv(N, 'Experiment 3/data/pre-target.csv')

save(N1, file= 'Experiment 3/data/target.Rda')
write.csv(N1, 'Experiment 3/data/target.csv')



