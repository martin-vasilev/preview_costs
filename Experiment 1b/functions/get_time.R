# Martin Vasilev, 2016

get_time<- function(Hz= 1000, office=FALSE){
  
  options(warn=2)

  endS<- function(string){as.numeric(gsub(" DISPLAY OFF", "", gsub("MSG\t", "", string)))}
  startS<- function(string){as.numeric(gsub(" DISPLAY ON", "", gsub("MSG\t", "", string)))} 
  
   
  message("Extracting trial durations:")
  
  # initialize variables:
  file<- NULL; ID<- NULL; trial<- NULL; start<- NULL; end<- NULL
  duration_ms<- NULL; duration_s<- NULL; subj<- NULL; type<- NULL
  dependnum<- NULL; cond<- NULL; item<- NULL; t<- NULL; tI<-NULL
  item_pos<- NULL; line<-  NULL; space<- NULL; trial<- NULL
  
  for(i in 1:64){
    
    file <- readLines(paste('C:\\Users\\mvasilev\\Desktop\\PD2_data\\NPD', toString(i), ".asc", sep='')) # load up file

    
#    if(i==28){
#      file<-  file[-c(845199:849223)] 
#      # problematic trial which was seen twice
#    }
    
    # Get start of trial ID
    ID <- which(grepl('TRIALID', file))
    
    # get trial name:
    #trial<- trial_name(file, ID)
    for(j in 1:length(ID)){
      line<- file[ID[j]] # extract line:
      space<-  unlist(gregexpr(' ', line)); # find empty space
      space<- space[length(space)] # find space before trial name begins
      trial[j]<- substr(line,space+1, nchar(line)) # extract trial name
    }
    trial<- unique(trial) # to avoid problems with loaded & repeated trials
    
    # get start of sentence presentation:
    start2<-  which(grepl('DISPLAY ON', file))
    start<- startS(file[start2])
    
    # get trial end (when button was pressed)
    end2 <- which(grepl('DISPLAY OFF', file))
    end <- endS(file[end2]) 

    # get trial duration in ms:
    duration_ms<- endS(file[end2]) - startS(file[start2])
    
    # get trial duration in s:
    duration_s<- duration_ms/Hz
    
    # process trial string: 
    subj<- rep(i, length(trial))
    type<- substr(trial,0,1)
    dependnum<- as.numeric(substr(trial, nchar(trial), nchar(trial)))
    cond<- as.numeric(substr(trial, 2,2))
    item_pos<- unlist(gregexpr('D', trial))
    item<- as.numeric(substr(trial, 4,item_pos-1))
    
    # merge variables in a data frame
    
    tI<- data.frame(subj, cond, item, type, dependnum, 
                    start, end, duration_ms, duration_s)
    
    tI<- subset(tI, type!="P" & dependnum!=1)
    tI<- tI[,-c(4,5)]
    
    t<- rbind(t, tI)
    t$duration_s<- round(t$duration_s,1)
    
    print(sprintf("Subject %d done", i))
    
  }

  return(t)
  
}


