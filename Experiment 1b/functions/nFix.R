nFix<- function(data){
  
  sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL
  nitems<- NULL; n<- NULL; p1<- NULL; p2<- NULL;
  dataN<- NULL; dataT<- NULL; 
  nfix1<- NULL; nfix2<-NULL; nfix<- NULL; sound<- NULL
  
  cat("Processing data for subject... ");
  
  for(i in 1:length(unique(data$sub))){ # for each subect..
    
    nitems<- unique(data$item[data$sub==i])# trials that participant saw
    nitems<- sort(nitems)
    cat(paste(i, " ", sep=""));
    
    
    for(j in 1: length(nitems)){ # for each item of subect i
      
      n<- subset(data, sub==i & item==nitems[j]) # subset data for subect i & item j
        
      sub<- n$sub[1]
      item<- n$item[1]
      seq<- n$seq[1]
      cond<- n$cond[1]
          
  
      nfix<- nrow(n)
          
        
      dataT<- data.frame(sub, item, seq, cond, nfix)
      sub<- NULL; item<- NULL; seq<- NULL; cond<- NULL; word<- NULL; p<- NULL;
      p1<- NULL; p2<- NULL; sound<- NULL
      nfix1<- NULL; nfix2<- NULL; nfix<-NULL; 
        
      dataN<- rbind(dataN, dataT)

      
    } # end of j
    
  } # end of i
  
  return(dataN)
}