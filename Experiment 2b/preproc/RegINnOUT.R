##### Regressions in and out:

 RegINnOUT<- function(data){
   
 newDat<- NULL
 
 subs<- unique(data$sub)   

 cat("Subject: ")
 
 for(i in 1:length(subs)){
   n<- subset(data, sub== subs[i])
   
   items<- unique(n$item)
   
   for(j in 1:length(items)){
     m<- subset(n, item== items[j])
     out<- which(is.na(m$SFIX))
     
     if(length(out)>0){
       m<- m[-out,]
     }
    
     
     if(nrow(m)<2){
       next
     }
     
     m$regressOUT<- NA
     m$regressIN<- NA
     
     max_word<- 1
     
     # map previous fixation characteristics:
     m$prev_word<- NA
     m$prev_word[2:nrow(m)]<- m$word[1:(nrow(m)-1)] 
     m$type<- ifelse(m$prev_word> m$word, "regressive", ifelse(m$prev_word== m$word, "same", "progressive"))
     
     m$next_type<- NA
     m$next_type[1:(nrow(m)-1)]<- m$type[2:(nrow(m))]
     
     
     
     m$RegIN<- ifelse(m$regress==1 & m$type=="regressive", 1, 
                      ifelse(is.na(m$prev_word), NA, 
                             ifelse(is.na(m$SFIX), NA, 0)))
     
    m$RegOUT<- ifelse(m$regress==0 & m$next_type=="regressive", 1, 0)
    m$RegOUT_all<- ifelse(m$next_type=="regressive", 1, 0)
    
    s<- m[,c('word', 'prev_word', 'type', 'next_type', 'regress', 'RegIN', 'RegOUT', 'RegOUT_all')]
    
    o<- sort(unique(m$word))
       
    for(k in 1:length(o)){
      temp<- m[1, 1:4]
      temp$word<- o[k]
      temp$RegIn<- NA
      temp$RegOut<- NA
      temp$RegOut_all<- NA   
      
      f<- subset(m, word== o[k])
      
      temp$RegIn<- ifelse(is.na(sum(f$RegIN)), NA, 
                          ifelse(sum(f$RegIN)>0, 1, 0))
      
      temp$RegOut<- ifelse(is.na(sum(f$RegOUT)), NA, 
                          ifelse(sum(f$RegOUT)>0, 1, 0))
      
      temp$RegOut_all<- ifelse(is.na(sum(f$RegOUT_all)), NA, 
                           ifelse(sum(f$RegOUT_all)>0, 1, 0))
      
      newDat<- rbind(newDat, temp)
       
    } # end of k
      
   }
  
   cat(i); cat(" ")
    
 }
   
 return(newDat)


 } # end of function



