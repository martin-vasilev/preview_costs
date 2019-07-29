checkFlags<- function(maxtrial=80, list_asc= "preproc/files_fxd.txt"){
  
  trial_info<- function(file, maxtrial, data){ # extracts information for processing trials
    ### get trial names:
    ID<- which(grepl('TRIALID', file));
    trial_text<- file[ID]
    trials<- substr(trial_text, unlist(gregexpr(pattern =' ',trial_text[1]))[2]+1, nchar(trial_text))
    #trials<- trials[which(letter!="P" &  letter!="F")] # remove practice items and questions
    trials<- gsub(" ", "", trials)
    # sometimes there is an extra empty space that can mess up detection of duplicates
    
    ### get condition:
    I<- unlist(gregexpr(pattern ='I',trials)) # start of item info
    cond<- as.numeric(substr(trials, 2, I-1)) # extract condition number
    
    ### get item:
    D<- unlist(gregexpr(pattern ='D',trials)) # start of dependent info
    item<- as.numeric(substr(trials, I+1, D-1)) # extract condition number
    depend<- as.numeric(substr(trials, nchar(trials), nchar(trials)))
    
    ### get sequence:
    #seq<- 1:length(trials)
    
    ### get start & end times
    start<- which(grepl('DISPLAY ON', file))
    end <- which(grepl('DISPLAY OFF', file))
    
    duplicated<- trials[duplicated(trials)]
    
    if(length(duplicated)>0){ # if there were aborted trials..
      message(paste(" Diplicated trial", duplicated, "for file:", data, "\n"))
      message("Analysing only last attempt at the trial!")
      
      toBeRemoved<- NULL
      
      for(i in 1:length(duplicated)){
        dup_rem<- which(trials==duplicated[i])
        
        for(j in 1:length(dup_rem)){
          if(j!=length(dup_rem)){
            toBeRemoved[length(toBeRemoved)+1]= dup_rem[j]
          }
        } # end of j
      } # end of i
      
      #start<- start[-toBeRemoved]
      # end<- end[-toBeRemoved]
      cond<- cond[-toBeRemoved]
      item<- item[-toBeRemoved]
      # seq<- seq[-toBeRemoved]
      depend<- depend[-toBeRemoved]
      ID<- ID[-toBeRemoved]
    } # end of aborted conditional
    
    trial_db<- data.frame(cond, item, depend, start, end, ID)
    trial_db<- subset(trial_db, depend==0 & item< maxtrial+1)
    trial_db$seq<- 1:nrow(trial_db)
    
    ###
    
    
    # trials<- trials[which(!is.element(trials, duplicated))]
    
    return(trial_db)
  }
  
  
  # load fail info here:
  library(readr)
  asc<- readLines(list_asc, warn= F)
  wrds<- read_delim("preproc/nwords.txt", 
                            "\t", escape_double = FALSE, col_names = c("sent", "n"), 
                            trim_ws = TRUE)
  
  db<- NULL
  temp<- data.frame(sub=NA, item=NA, cond=NA, nstarted=NA, ncompleted=NA, nboundary=NA)
  
  for(i in 1:length(asc)){ # for each subject
    
    dataF<- readLines(asc[i]) # load asc file;
    
    trial_db<- suppressMessages(trial_info(dataF, maxtrial, asc[i])) # get info about trials)
    
    for(j in 1:nrow(trial_db)){ # for each item
      max<- wrds$n[trial_db$item[j]] # number of boundaries in trial
      
      started<- which(grepl('DISPLAY CHANGE STARTED', dataF[trial_db$start[j]:trial_db$end[j]]))
      completed<- which(grepl('DISPLAY CHANGE COMPLETED', dataF[trial_db$start[j]:trial_db$end[j]]))
      
      temp$sub<- i
      temp$item<- trial_db$item[j]
      temp$cond<- trial_db$cond[j]
      temp$nstarted<- length(started)
      temp$ncompleted<- length(completed)
      temp$nboundary<- max

      
      db<- rbind(db, temp)
    }
    cat(i); cat(" ")
  }
  
  db$problem<-NULL
  db$flags_equal<-NULL
  for(i in 1:nrow(db)){
    if(db$ncompleted[i]!=db$nstarted[i] | db$nstarted[i]>db$nboundary[i]| db$ncompleted[i]>db$nboundary[i]){
      db$problem[i]<- "yes"
    } else{
      db$problem[i]<- "no"
    }
    
    if(db$ncompleted[i]==db$nstarted[i]){
      db$flags_equal[i]<- "yes"
    } else{
      db$flags_equal[i]<- "no"
    }
    
  }

  
  return(db)
}