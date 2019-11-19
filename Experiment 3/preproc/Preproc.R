
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

if(!file.exists("Experiment 3/data/Quest.Rda")){
  Quest<- Question(data_list = data_dir, maxtrial = 138)
  save(Quest, file= "Experiment 3/data/Quest.Rda")
  write.csv(Quest, "Experiment 3/data/Quest.csv")
} else{
  load("Experiment 3/data/Quest.Rda")
}


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

if(!file.exists("Experiment 3/data/Trial_time.Rda")){
  
  Trialt<- trialTime(data_list = data_dir, maxtrial = 138)
  save(Trialt, file= "Experiment 3/data/Trial_time.Rda")
  write.csv(Trialt, "Experiment 3/data/Trial_time.csv")
}else{
  load("Experiment 3/data/Trial_time.Rda")
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

if(!file.exists("Experiment 3/preproc/raw_fix.Rda")){
  # extract raw data & merge it with da1 files:
  raw_fix<- SingleLine(data_list = data_dir, ResX = 1024, ResY = 768, maxtrial = 138, tBlink = 150)
  save(raw_fix, file= "Experiment 3/preproc/raw_fix.Rda")
  write.csv(raw_fix, file= "Experiment 3/preproc/raw_fix.csv")
}


####################
# Boundary change: #
####################

DC<- Boundary(data_list = data_dir, maxtrial = 138)

save(DC, file= 'Experiment 3/preproc/DC.Rda')
write.csv(DC, file= 'Experiment 3/preproc/DC.csv')

##############################
# Preprocessing of raw data: #
##############################




