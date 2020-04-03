
#' Checks the display change implementation in Boundary experiments (Rayner, 1975)
#'
#' Checks the implementation of 1 boundary change during sentence reading. Checks the timing of the display change,
#' as well as whether the boundary was crossed in a forwards saccade.
#'
#' @author Martin R. Vasilev
#'
#' @param data_list Input of data files to be processed. This can be specified in three ways:
#' 1) a directory that contains all the files (it will select all files ending with ".asc",
#' and order them by participant number, if present).
#' 2) Directory to a txt file that contains all the .asc data file names inside:
#' e.g., data_list= "C:/My Data/data_list.txt".
#' In the .txt file, the directory for each .asc data file should appear on a separate row,
#' e.g.: C:/My Data/subject1.asc /n
#' C:/My Data/subject2.asc
#' 3) A directory to a single .asc file: e.g., data_list= "C:/My Data/subject1.asc".
#' 
#' @param maxtrial Maximum number of experimental trials in the experiment (default= 9999)
#' 
#' @param boundary_loc The name of the message flag in the data file for the pixel location of the boundary (if present)
#' 
#' @param start_flag start flag for the beginning of the display change
#' 
#' @param end_flag end flag indicating the completion of the display change
#' 
#' @param tBlink check data samples before and after the boundary for data loss (e.g., blinks). Indicate how many ms
#' to check on either side of the boundary
#' 
#' @include utility.R


BoundaryN<- function(data_list= '', maxtrial= 999, boundary_loc= 'TW BOUNDARY', 
                    start_flag= 'DISPLAY CHANGE STARTED', end_flag= 'DISPLAY CHANGE COMPLETED',
                    tBlink= 150){
  
  source("https://raw.githubusercontent.com/martin-vasilev/EMreading/master/R/utility.R")
  
  # check if user provided data dir:
  if(length(data_list)==0){
    data_list= file.choose() # make them chose a file
    message("To process multiple files, please specify a directory in 'data_list'")
  }
  
  # check file input:
  if(grepl('.txt', data_list)){
    data<- readLines(data_list, warn=F) # process multiple files
  }else{
    if(grepl('.asc', data_list)){ # if a single .asc file was provided...
      data<- data_list # process only 1 file
    } else{ # otherwise, it must be a dir of files
      data<- get_files(data_list)
    }
  }
  
  dat<- NULL
  
  for (i in 1:length(data)){ # for each subject..
    
    cat(sprintf("\nProcessing subject %i", i)); cat("\n")
    cat(sprintf("Loading data %s ...", data[i]));
    filename= data[i] #strsplit(data[i], "\\")
    
    file<- readLines(data[i]) # load file
    cat(" Done"); cat("\n")
    trial_db<- trial_info(file, maxtrial= maxtrial, selectEXP = T) # extract info about trials to be processed
    cat("Trial... ")
    
    for(j in 1:nrow(trial_db)){ # for each item
      
      trialFile<- file[trial_db$ID[j]:trial_db$end[j]]
      
      temp<- data.frame(sub=NA, item= NA, cond=NA, seq= NA, Bnd_loc= NA, tCross=NA, tStarted=NA, tCompleted=NA, 
                       tChange=NA, prevFlag= NA, nextFlag= NA, tPrevFlag= NA, tNextFlag=NA,
                       tChangetoFixOnset= NA)
      temp$prevFlag<- NULL
      temp$tPrevFlag<- NULL
      
      temp$sub<- i
      temp$item<- trial_db$item[j]
      temp$cond<- trial_db$cond[j]
      temp$seq<- j
      
      # Try to find boundary location:
      Bnd_txt<- trialFile[which(grepl(boundary_loc, trialFile))]
      loc<- substr(Bnd_txt[1], unlist(gregexpr(' ', Bnd_txt[1]))[1]+nchar(boundary_loc)+1, nchar(Bnd_txt[1]))
      loc<- as.numeric(loc) 
      temp$Bnd_loc<- loc
      
      # All boundaries:
      textBnds<- trialFile[3:30]
      txt<- textBnds[which(grepl('BOUNDARY', textBnds))]
      txt<- txt[1:(length(txt)-2)]
      loc<- as.data.frame(do.call( rbind, strsplit( txt, ' ' ) ), stringsAsFactors = F)
      loc<- loc$V3
      loc<- as.numeric(loc)
      
      tw_num<- which(loc== temp$Bnd_loc)
      
      SFlag<- paste(start_flag, " W", tw_num, sep= '')
      EFlag<- paste(end_flag, " W", tw_num, sep= '')
      
      # Find the start flag:
      Start_text<- trialFile[which(grepl(SFlag, trialFile))]
      start_time<-  substr(Start_text, 1, unlist(gregexpr(' ', Start_text))[1])
      start_time<- get_num(start_time)
      if(length(start_time)>0){
        temp$tStarted<- start_time
      }
      
      # Find the end flag:
      End_text<- trialFile[which(grepl(EFlag, trialFile))]
      end_time<-  substr(End_text, 1, unlist(gregexpr(' ', End_text))[1])
      end_time<- get_num(end_time)
      if(length(end_time)>0){
        temp$tCompleted<- end_time 
      }
      
      
      # difference between started and completed:
      temp$tChange<- temp$tCompleted - temp$tStarted
      
      # what is the actual time the boudary was crossed?
      whichRow<- which(grepl(SFlag, trialFile))
      
      if(length(whichRow)>0){
        samples<- trialFile[(whichRow-51): whichRow-1] 
        
        # remove flags from samples data:
        samples<- samples[!grepl("EFIX", samples)]
        samples<- samples[!grepl("SFIX", samples)]
        samples<- samples[!grepl("ESACC", samples)]
        samples<- samples[!grepl("SSACC", samples)]
        samples<- samples[!grepl("MSG", samples)]
        
        samples <-  as.data.frame(do.call( rbind, strsplit( samples, '\t' ) )) # V2 is xpos
        samples$V2= as.numeric(as.character(samples$V2)) # x pos vector
        samples$V1= as.numeric(as.character(samples$V1))
        
        time_cross<- which(samples$V2>= temp$Bnd_loc)
        if(length(time_cross)>0){
          temp$tCross<- samples$V1[time_cross[1]]
        }else{
          temp$tCross<- temp$tStarted
        }
        
      }else{
        temp$tCross<- NA
        temp$nextFlag<- NA
        temp$tNextFlag<- NA
        temp$tChangetoFixOnset<- NA
        next
      }

      AfterBnd= trialFile[whichRow:length(trialFile)]
      
      # Get next flag:
      nextSFIX<- which(grepl(c('SFIX') , AfterBnd))
      nextEFIX<- which(grepl(c('EFIX') , AfterBnd))
      
      if(length(nextSFIX)>0 & length(nextEFIX)>0){
        
        if(nextSFIX[1]< nextEFIX[1]){
          temp$nextFlag<- 'SFIX'
          tNextFlag= AfterBnd[nextSFIX[1]]
          #tNextFlag= get_num(unlist(strsplit(tNextFlag, '\t'))[1])
          temp$tNextFlag<- get_num(tNextFlag)
        }else{
          temp$nextFlag<- 'EFIX'
          tNextFlag= AfterBnd[nextEFIX[1]]
          tNextFlag= get_num(unlist(strsplit(tNextFlag, '\t'))[1])
          temp$tNextFlag<- tNextFlag
        }
        
      }else{
        if(length(nextSFIX)>0){
          temp$nextFlag<- 'SFIX'
          tNextFlag= AfterBnd[nextSFIX[1]]
          temp$tNextFlag<- get_num(tNextFlag)
        }else{
          if(length(nextEFIX)>0){
            temp$nextFlag<- 'EFIX'
            tNextFlag= AfterBnd[nextEFIX[1]]
            tNextFlag= get_num(unlist(strsplit(tNextFlag, '\t'))[1])
            temp$tNextFlag<- tNextFlag
          }else{
            temp$nextFlag<- NA
            temp$tNextFlag<- NA
          }
        }
      }
      
      # Find location of next fixation (for hooks):
      
      if(is.na(temp$nextFlag) | length(temp$nextFlag)<1){
        temp$nextLoc<- NA
        temp$tChangetoFixOnset<- NA
      }else{
        
        if(temp$nextFlag== "SFIX"){
          loc_flag= AfterBnd[nextEFIX[1]]
          temp$nextLoc<- as.numeric(unlist(strsplit(loc_flag, '\t'))[4])
          
        }else{
          if(temp$nextFlag== "EFIX"){
            loc_flag= AfterBnd[nextEFIX[2]]
            temp$nextLoc<- as.numeric(unlist(strsplit(loc_flag, '\t'))[4])
          }else{
            temp$nextLoc<- NA
          }
        }
        
        #
        # timing of change relative to next fixation onset?
        
        if(temp$nextFlag== "SFIX"){
          temp$tChangetoFixOnset<- temp$tStarted - temp$tNextFlag
        }else{ # EFIX flag
          tNextSFIX= AfterBnd[nextSFIX[2]]
          tNextSFIX<- get_num(tNextSFIX) 
          temp$tChangetoFixOnset<- tNextSFIX-temp$tStarted
        }
        
        
      }
      
      ## Hook?
      
      if(!is.na(temp$nextLoc)){
        if(temp$nextLoc< temp$Bnd_loc){
          temp$hook<- 1
        }else{
          temp$hook<- 0
        }
      }else{
        temp$hook<- NA
      }
      
      # check data around boundary for loss (i.e, blinks):
      
      # location of boundary crossing in data file:
      Bnd_st<- which(grepl(temp$tStarted, trialFile))[1]
      Bnd_samples<- NULL
      try(Bnd_samples<- trialFile[(Bnd_st- tBlink-2): (Bnd_st+ tBlink+2)])
      
      Bnd_samples<- Bnd_samples[!grepl("EFIX", Bnd_samples)]
      Bnd_samples<- Bnd_samples[!grepl("SFIX", Bnd_samples)]
      Bnd_samples<- Bnd_samples[!grepl("ESACC", Bnd_samples)]
      Bnd_samples<- Bnd_samples[!grepl("SSACC", Bnd_samples)]
      Bnd_samples<- Bnd_samples[!grepl("MSG", Bnd_samples)]
      Bnd_samples<- Bnd_samples[!grepl("SBLINK", Bnd_samples)]
      Bnd_samples<- Bnd_samples[!grepl("EBLINK", Bnd_samples)]
      
      try(Bnd_samples <-  as.data.frame(do.call( rbind, strsplit( Bnd_samples, '\t' ) ), 
                                        stringsAsFactors= F)) # V2 is xpos
      Bnd_samples$V4= as.numeric(as.character(Bnd_samples$V4))
      
      
      which_blink<- which(Bnd_samples$V4==0)
      if(length(which_blink)>0){
        temp$blink<- 1
      }else{
        temp$blink<- 0
      }
      
      
      
      dat<- rbind(dat, temp)
      
      cat(toString(j)); cat(" ") 
      
    } # end of j
    
  } # end of i
  
  return(dat)
    
}