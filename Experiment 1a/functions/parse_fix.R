
parse_fix<- function(data){
  
  sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; w<- NULL; p<- NULL
  nitems<- NULL; n<- NULL; m<- NULL; p1<- NULL; p2<- NULL;
  dataN<- NULL; dataT<- NULL; q<- NULL; r<- NULL; sent<- NULL
  FFD<- NULL; SFD<- NULL; GD<-NULL; TVT<- NULL
  dataN<- NULL; t<- NULL
  
  cat("Processing data for subject... ");
  
  data$FFD<-NA
  data$SFD<-NA
  data$GD<- NA
  data$TVT<- NA
  
  for(i in 1:length(unique(data$subject))){ # for each subect..
    
    nitems<- unique(data$item[data$sub==i])# trials that participant saw
    nitems<- sort(nitems)
    cat(paste(i, " ", sep=""));
    
    for(j in 1: length(nitems)){ # for each item of subect i
      
      n<- subset(data, subject==i & item==nitems[j]) # subset data for subect i & item j
      w<- sort(unique(n$currentword))
      
      for(k in 1:length(w)){
        m<- subset(n, currentword==w[k])
        
        p1<- subset(m, regress==0)
        p2<- subset(m, regress==1)
        
        t<- m[1,]
        
        if(nrow(p1)==0){ # if no first pass reading:
          # nothing happens, default value is already NA
        }
        
        if(nrow(p1)==1){ # if only 1 fixation on first pass reading
          t$FFD<- p1$fixationduration[1] # FFD is that fixation
          t$GD<- p1$fixationduration[1] # GD == FFD
        }
        
        if(nrow(p1)>1){ # if more than one first-pass fixation..
          t$FFD<- p1$fixationduration[1] # FFD is always going to be the first fixation
          t$GD<- sum(p1$fixationduration) # GD is sum of first-pass fixations
        }
        
        if(nrow(p2)==0){
          t$TVT<- t$GD #  TVT= GD if there are no second-pass fixations
        }
        
        if(nrow(p2)>0){
          if(!is.na(t$GD)){
            t$TVT<- t$GD + sum(p2$fixationduration) # sum of first-pass and second pass fixations
          }else{
            t$TVT<- sum(p2$fixationduration)
          }
          
        }
        
        if(!is.na(t$FFD) & !is.na(t$GD)){ # if first pass is not NA
          if(t$FFD==t$GD){
            t$SFD<- t$FFD
          }
        }
    
        
        dataN<- rbind(dataN, t)
        
      } # end of k
      
    } # end of j
    
  } # end of i
  
  return(dataN)
}
