
DC<- function(maxtrial=80, list_asc= "Experiment 2/preproc/files_fxd_L.txt"){
  
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
  
  get_num<- function(string){as.numeric(unlist(gsub("[^0-9]", "", unlist(string)), ""))}
  
  
  # load fail info here:
  library(readr)
  asc<- readLines(list_asc, warn= F)
  
  db<- NULL
  #temp<- data.frame(sub=NA, item=NA, cond=NA, tstarted=NA, tcompleted=NA, nboundary=NA)
  
  for(i in 1:length(asc)){ # for each subject
    
    dataF<- readLines(asc[i]) # load asc file;
    
    trial_db<- suppressMessages(trial_info(dataF, maxtrial, asc[i])) # get info about trials)
    
    for(j in 1:nrow(trial_db)){ # for each item
      
      sub<- NULL
      item<- NULL
      cond<- NULL
      tstarted<- NULL
      tcompleted<- NULL
      nboundary<- NULL
      
      started<- which(grepl('DISPLAY CHANGE STARTED', dataF[trial_db$start[j]:trial_db$end[j]]))
      completed<- which(grepl('DISPLAY CHANGE COMPLETED', dataF[trial_db$start[j]:trial_db$end[j]]))
      
      str_start<- get_num(dataF[trial_db$start[j]+started-1])
      str_end<- get_num(dataF[trial_db$start[j]+completed-1])
      
      if(length(str_start)!= length(str_end)){
        next;
      }
      
      sub<- rep(i, length(str_start))
      item<- rep(trial_db$item[j], length(str_start))
      cond<- rep(trial_db$cond[j], length(str_start))
      tstarted<- str_start
      tcompleted<- str_end
      nboundary<- 1:length(str_start)
      
      if(length(cond)==0){
        next;
      }

      temp<- data.frame(sub, item, cond, tstarted, tcompleted, nboundary)
      rm(sub, item, cond, tstarted, tcompleted, nboundary)
      db<- rbind(db, temp)
    }
    cat(i); cat(" ")
  }
  

  return(db)
}