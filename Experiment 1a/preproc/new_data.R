
rm(list= ls())

load("Experiment 1a/preproc/old_data.Rda")

RegIN <- read.csv("D:/R/preview_costs/Experiment 1a/preproc/RegIN.txt")
RegOUT <- read.csv("D:/R/preview_costs/Experiment 1a/preproc/RegOUT.txt")

RegIN$R1<- NULL
RegIN$R4<- NULL
colnames(RegIN)<- c("seq", "subj", "item", "cond", "RegIN_N", "RegIN_N1")

RegOUT$R1<- NULL
RegOUT$R4<- NULL
colnames(RegOUT)<- c("seq", "subj", "item", "cond", "RegOUT_N", "RegOUT_N1")

RegIN<- subset(RegIN, cond>0)
RegOUT<- subset(RegOUT, cond>0)

dat<- RegIN
dat$RegOUT_N<- RegOUT$RegOUT_N
dat$RegOUT_N1<- RegOUT$RegOUT_N1

for(i in 1:nrow(dat)){
  if(is.element(dat$cond[i], c(1,2))){
    dat$prev[i]<- "valid"
  }
  if(is.element(dat$cond[i], c(3,4))){
    dat$prev[i]<- "phon"
  }
  if(is.element(dat$cond[i], c(5,6))){
    dat$prev[i]<- "orth"
  }
  if(is.element(dat$cond[i], c(7,8))){
    dat$prev[i]<- "invalid"
  }
  
  if(dat$cond[i]%%2==0){
    dat$deg[i]<- "0"
  } else{
    dat$deg[i]<- "20"
  }
  
}


old_data$RegIN_N<- NA
old_data$RegIN_N1<- NA
old_data$RegOUT_N<- NA
old_data$RegOUT_N1<- NA

for(i in 1:nrow(old_data)){
  a<- which(dat$subj== old_data$subj[i] & dat$item== old_data$item[i])
  
  if(length(a)>0){
    old_data$RegIN_N[i]<- dat$RegIN_N[a]
    old_data$RegIN_N1[i]<- dat$RegIN_N1[a]
    
    old_data$RegOUT_N[i]<- dat$RegOUT_N[a]
    old_data$RegOUT_N1[i]<- dat$RegOUT_N1[a]
  }
  
  
  
  if(length(a)>1){
    stop('oops!')
  }
  
}

data<- old_data

data$Skip_N<- ifelse(data$nFix_N>0, 0, 1)
data$Skip_N1<- ifelse(data$nFix_N1>0, 0, 1)

save(data, file= 'Experiment 1a/data/data.Rda')
write.csv(data, 'Experiment 1a/data/data.csv')
