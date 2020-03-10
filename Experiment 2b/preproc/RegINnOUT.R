##### Regressions in and out:

 RegINnOUT<- function(data){
   
 newDat<- NULL
 
 subs<- unique(data$sub)   

 for(i in 1:length(subs)){
   n<- subset(data, sub== subs[i])
   
   items<- unique(n$item)
   
   for(j in 1:length(items)){
     m<- subset(n, item== items[j])
    
     m$regressOUT<- NA
     m$regressIN<- NA
     
     RegStart<- 0
     RegEnd<- 1
     
     for(k in 1:nrow(m)){
       
       if(is.na(m$regress[k])){
         next
       }
       
      if(m$regress[k]==1){
        if(RegStart==0){
          RegStart==1
          m$regressOUT[k]<- 1
        }else{
          m$regressOUT[k]<- 0
        }
        
        if(k>1){
          if(RegStart==1 & m$word[k]> m$word[k-1]){
            RegStart<- 0
          }
        }

        
      }
       
     } # end of k
      
   }
   
 }
   
   
   
 data<- subset(raw_fix, sub==1 & item==1)
 nsent<- unique(data$sent)

 new_dat<- NULL

 for(i in 1:length(nsent)){
   s<- subset(data, sent== nsent[i])
   s$regIN<- NA
   s$regOUT<- NA

   inReg<- 0

   for(j in 1:nrow(s)){

     if(s$regress[j]==1){
       inReg<- 1
     }

     if(inReg){
       s$regOUT[j]<- 1
     }
   }

 }


 } # end of function



