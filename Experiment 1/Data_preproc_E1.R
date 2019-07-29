
# Martin R. Vasilev, 2017
# Data pre-processing script


rm(list=ls())

source("https://raw.githubusercontent.com/martin-vasilev/R_scripts/master/useful_functions.R")

### Question accuracy:
load_package("readr")
q <- read_delim("Experiment 1/preproc/q.txt", 
                     "\t", escape_double = FALSE, trim_ws = TRUE)

# remove practice trials
q<- subset(q, item<81)

load_package("reshape")
DesQ<- melt(q, id=c('subject', 'item'), 
            measure=c("accuracy"), na.rm=TRUE)
mQ<- cast(DesQ, subject ~ variable
          , function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))
min(mQ$accuracy_M)
mean(mQ$accuracy_M)
mean(mQ$accuracy_SD)

## Assign cond:
q$prev<-NA
q$deg<- NA
q$cond<- q$sentcond

for(i in 1:nrow(q)){
  if(is.element(q$cond[i], c(1,2))){
    q$prev[i]<- "valid"
  }
  if(is.element(q$cond[i], c(3,4))){
    q$prev[i]<- "phon"
  }
  if(is.element(q$cond[i], c(5,6))){
    q$prev[i]<- "orth"
  }
  if(is.element(q$cond[i], c(7,8))){
    q$prev[i]<- "invalid"
  }
  
  if(q$cond[i]%%2==0){
    q$deg[i]<- "0"
  } else{
    q$deg[i]<- "20"
  }
  
}

save(q, file="Experiment 1/data/quest_accuracy.Rda")


table(q$item, q$sentcond)


### check flags:
#source("functions/checkFlags.R")
#db<- checkFlags()

#save(db, file= "data/db.Rda")

#write.csv(db, "bnd.txt")
load('Experiment 1/data/db.Rda')


#TW<- subset(raw, TW=="yes")

#DC<- subset(TW, insaccade=="yes")

#bad<- subset(TW, insaccade=="no")


loc<- NULL; trials<- NULL
bnd<- subset(db, problem=="yes")
bnd<- subset(db, !flags_equal=="yes")# remove 16 bad trials

#for(i in 1:nrow(bnd)){
#  a<- which(raw$subject==bnd$sub[i]& raw$condition== bnd$cond[i]& raw$item==bnd$item[i])
#  trials<- c(trials, a)
#}

#raw_bad<- raw[trials,]

#raw<- raw[-trials,]


##### Eyedry output:
library(readr)
FFD <- read_csv("Experiment 1/preproc/FFD.txt")
SFD <- read_csv("Experiment 1/preproc/SFD.txt")
GD<- read_csv("Experiment 1/preproc/GD.txt")
TVT<- read_csv("Experiment 1/preproc/TVT.txt")
nFix<- read_csv("Experiment 1/preproc/nFix.txt") # number of fixations (1st-pass)
nFix1<- read_csv("Experiment 1/preproc/nFix1st.txt") # number of fixations (1st-pass)


## FFD:
FFD$R1<- NULL
FFD$R4<- NULL
colnames(FFD)<- c("seq", "subj", "item", "cond", "FFD_N", "FFD_N1")

## SFD:
SFD$R1<- NULL
SFD$R4<- NULL
colnames(SFD)<- c("seq", "subj", "item", "cond", "SFD_N", "SFD_N1")


## GD:
GD$R1<- NULL
GD$R4<- NULL
colnames(GD)<- c("seq", "subj", "item", "cond", "GD_N", "GD_N1")

## TVT:
TVT$R1<- NULL
TVT$R4<- NULL
colnames(TVT)<- c("seq", "subj", "item", "cond", "TVT_N", "TVT_N1")


for(i in 1:nrow(GD)){
  if(GD$GD_N[i]==0){
    GD$GD_N[i]= NA
  }
  if(GD$GD_N1[i]==0){
    GD$GD_N1[i]= NA
  }
}

## nFix:
nFix$R1<- NULL
nFix$R4<- NULL
colnames(nFix)<- c("seq", "subj", "item", "cond", "nFix_N", "nFix_N1")


data<- FFD
data$SFD_N<- SFD$SFD_N
data$SFD_N1<- SFD$SFD_N1
data$GD_N<- GD$GD_N
data$GD_N1<- GD$GD_N1
data$TVT_N<- TVT$TVT_N
data$TVT_N1<- TVT$TVT_N1
data$nFix_N<- nFix$nFix_N
data$nFix_N1<- nFix$nFix_N1

data$prev<-NA
data$deg<- NA

for(i in 1:nrow(data)){
  if(is.element(data$cond[i], c(1,2))){
    data$prev[i]<- "valid"
  }
  if(is.element(data$cond[i], c(3,4))){
    data$prev[i]<- "phon"
  }
  if(is.element(data$cond[i], c(5,6))){
    data$prev[i]<- "orth"
  }
  if(is.element(data$cond[i], c(7,8))){
    data$prev[i]<- "invalid"
  }
  
  if(data$cond[i]%%2==0){
    data$deg[i]<- "0"
  } else{
    data$deg[i]<- "20"
  }
  
}

outFFD<- which(data$FFD_N>800 | data$FFD_N1>800)
outGD<- which(data$GD_N>1600 | data$GD_N1>1600)
out<- unique(outFFD, outGD)

sprintf("%f percent of trials were excluded as outliers", (length(out)/(64*80))*100)
data<- data[-out,]


##########################
# bad boundary crossings:
##########################

library(readr)
jmartin <- read_delim("Experiment 1/preproc/jmartin.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)

bnd_out<- jmartin[which(jmartin$comment!="good"),]
# removing infixes only if timing>5ms leads to a saving of only 0.6% of trials
# thus, better to use 0ms cut-off for infixes to be consistent with previous 2 studies
#bnd_out<- bnd_out[-which(jmartin$comment!="good" & jmartin$timing<=5),]

data$bnd<- 0

for(i in 1:nrow(bnd_out)){
  a<- which(data$subj== bnd_out$subject[i] & data$item== bnd_out$item[i])
  if(length(a)>0){
    data$bnd[a]<- 1
  }
}


#######

######--- problematic trials (bnd from db):
data$db<-0

for(i in 1:nrow(bnd)){
  a<- which(data$subj== bnd$sub[i] & data$item== bnd$item[i])
  
  if(length(a)>0){
    data$db[a]<- 1
    cat("yes\n")
  }
  
}


sprintf("%f percent of trials were excluded due to bad boundary crossing",
        ((table(data$bnd)[2]+length(which(data$db==1)))/5120)*100)

data<- subset(data, bnd==0)

data<- subset(data, db==0)

data$bnd<- NULL
data$db<- NULL

jmartin<- jmartin[which(jmartin$comment=="good"),]
save(jmartin, file= 'Experiment 1/data/jmartin.Rda')
##-----------------------


sprintf("%f percent of data left for analysis", (nrow(data)/5120)*100)


data$pRefixN<- NULL
data$pRefixN1<- NULL

for(i in 1:nrow(data)){
  if(data$nFix_N[i]>1){
    data$pRefixN[i]<- 1
  } 
  
  if(data$nFix_N[i]==1){
    data$pRefixN[i]<- 0
  }
  
  if(data$nFix_N[i]==0){
    data$pRefixN[i]<- NA
  }
  ###
  
  
  if(data$nFix_N1[i]>1){
    data$pRefixN1[i]<- 1
  }
  
  if(data$nFix_N1[i]==1){
    data$pRefixN1[i]<- 0
  }
  
  if(data$nFix_N1[i]==0){
    data$pRefixN1[i]<- NA
  }
  
}



####
# pre-boundary fixation:
data$preBndFix<- NULL
data$preBndX<- NULL

for(i in 1:nrow(data)){
  a<- which(jmartin$subject==data$subj[i]& jmartin$item==data$item[i])
  data$preBndFix[i]<- jmartin$prefix[a]
  data$preBndX[i]<- jmartin$preX[a]
}

# get distance in letters:
data$dist_Bnd<- data$preBndX/11 # 11 pixels per letter



save(data, file= "Experiment 1/data/data.Rda")
write.csv(data, "Experiment 1/data/data.csv")
######-------------------------------

library(reshape)
library(reshape2)

DesFixFFD<- melt(data, id=c("subj", "item", "cond", "prev", "deg"), 
              measure=c("FFD_N", "FFD_N1", "SFD_N", "SFD_N1", "GD_N", "GD_N1", "TVT_N", "TVT_N1")
              , na.rm=TRUE)
mF<- cast(DesFixFFD, prev+deg ~ variable
            , function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

write.csv(mF, file= 'Experiment 1/data/means.csv')
save(mF, file= 'Experiment 1/data/means.Rda')


#### DC timing:
if(!file.exists("Experiment 1/data/DC.Rda")){
  source("Experiment 1/functions/DC.R")
  tDC<- DC()
  tDC$t<- tDC$tcompleted- tDC$tstarted
  save(tDC, file= "Experiment 1/data/DC.Rda")
}else{
  load("Experiment 1/data/DC.Rda")
  mean(tDC$t +(1000/150)/2 + 2)
  sd(tDC$t+ (1000/150)/2 + 2)
}


###### trials excluded for blinks:

Len<- NULL

for(i in 1:64){
  filename<- paste("C:/Users/Martin Vasilev/Documents/data MW/PD", i, ".ada1", sep= "")
  a<- readLines(filename)
  Len[i]<- length(a)
  cat(i); cat(" ")
}

Len<- as.numeric(Len)

1-sum(Len)/(64*80)


######################################################
# Sentence reading time as a function of degradation:

source("Experiment 1/functions/get_time.R")

t<- get_time()

t$deg<- NULL
for(i in 1:nrow(t)){
  if(t$cond[i]%%2==0){
    t$deg[i]<- "0"
  } else{
    t$deg[i]<- "20"
  }
}

save(t, file= "Experiment 1/data/t.Rda")


###### fixations duration:
library(EMreading)

raw_fix_t<- SLpreproc(data_list = "C:/Users/Martin Vasilev/Documents/MW data/data", ResX = 1024,
                      ResY = 768, maxtrial = 80)

save(raw_fix_t, file= "Experiment 1/data/raw_fix_t.Rda")

raw_fix_t<- subset(raw_fix_t, fix_dur>0)

raw_fix<- cleanData(raw_fix_t, combineMethod = "pix", combineDist = 11)


raw_fix$deg<- NULL
for(i in 1:nrow(raw_fix)){
  if(raw_fix$cond[i]%%2==0){
    raw_fix$deg[i]<- "0"
  } else{
    raw_fix$deg[i]<- "20"
  }
}

raw_fix$sent<- NULL
raw_fix$word<- NULL
raw_fix$wordID<- NULL
raw_fix$char_trial<- NULL
raw_fix$regress<- NULL
raw_fix$land_pos<- NULL
raw_fix$outsideText<- NULL
raw_fix$hasText<- NULL

for(i in 1:nrow(raw_fix)){
  if(i==1){
    next;
  }
  
  if(raw_fix$item[i]== raw_fix$item[i-1]){
    raw_fix$sacc_len[i]<- abs((raw_fix$xPos[i]-raw_fix$xPos[i-1])/11) 
  }
}

save(raw_fix, file= "Experiment 1/data/raw_fix.Rda")



############## number of fixations:

source("Experiment 1/functions/nFix.R")

FX<- nFix(raw_fix)

FX$deg<- NULL
for(i in 1:nrow(FX)){
  if(FX$cond[i]%%2==0){
    FX$deg[i]<- "0"
  } else{
    FX$deg[i]<- "20"
  }
}

save(FX, file= "Experiment 1/data/FX.Rda")

#####################################
# map pre-boundary fixations:

load("Experiment 1/data/data.Rda")


