
# Martin R. Vasilev, 2017



########################
#  UTILITY FUNCTIONS   #
########################


# Extract text containing stimuli info:
get_text<- function(file){ ## extracts the loaded text material                         #
  
  start<- which(grepl("DISPLAY TEXT", file))+1 # start point
  end<- which(grepl("START", file))
  
  if(length(start)==0){ # no start of text detected
    return(0) # don't map fixations to stimuli
    
  } else{ # extract text containing stimuli info
    text<- file[start:end] # subset file
    a= which(gregexpr(pattern ='BUTTON', text)==1)
    
    if(length(a)>0){
      text<- text[-a]
      # text<- text[-a] # because one line is removed
    }
    
    #input<- which(grepl("INPUT", text)); input<- input[1]-1 # end point
    end2<- which(grepl("REGION", text))
    end2<- end2[length(end2)]
    
    text<- text[1:end2]
    
    #end<- which(grepl("INPUT", file)); end<- end[1]-1 # end point
    #text<- file[start:end] # subset file
    return(text)
    
  }
  
  }


# Create a database with position of text on screen
get_coord<- function(string){ # extracts text coordinates from trial info
  
  # check to make sure there are no button press flags here..
  a= which(gregexpr(pattern ='BUTTON', string)==1)
  
  if(length(a)>0){
    string<- string[-a]
  }
  
  #if(which(grepl('   ', string))==1){
  #  loc<- as.numeric(gregexpr("CHAR", string))
  #  part1<- substr(string, 1, loc+7)
  #  part2<- substr(string, loc+9, nchar(string))
  #  string2<- paste(part1, part2, sep="")
  #}
  #else{
  #out <- data.frame( do.call( rbind, strsplit( string, ' ' ) ) )
  out <-  do.call( rbind, strsplit( string, '\t' ) )
  out<- out[,2]
  out <-  data.frame(do.call( rbind, strsplit( out, ' ' ) ) )
  #}
  
  out<- subset(out, X2!="DELAY") # Remove DELAY 1ms lines
  
  out$X7<- as.numeric(as.character(out$X7))
  out$X8<- as.numeric(as.character(out$X8))
  out$X9<- as.numeric(as.character(out$X9))
  out$X10<- as.numeric(as.character(out$X10))
  out$X11<- as.numeric(as.character(out$X11))
  
  fix_spaces<- function(out){
    out$space<- NULL
    a<- which(out$X6=="") # find position of empty spaces
    out$space<- NA
    out$space[a]<- 1
    # Re-arrange misplaced coords
    out$X7[a]<- out$X8[a]
    out$X8[a]<- out$X9[a]
    out$X9[a]<- out$X10[a]
    out$X10[a]<- out$X11[a]
    out<- out[,-11]
  }
  
  out<- fix_spaces(out)
  
  
  # clean_MSG<- function(msg){as.numeric(unlist(gsub("[^0-9]", "", unlist(msg)), ""))}
  # out$X1<- clean_MSG(out$X1) # get rid of MSG text
  out<- out[,-c(1,2,3,5)] # remove useless columns
  
  # map sentences:
  map_sent<- function(out){
    sent_bnd<-  which(out$X6=="."| out$X6=="?");
    sent<- NULL
    
    if(length(sent_bnd)>0){
      for(i in 1:length(sent_bnd)){
        if(i==1){
          sent[1:sent_bnd[i]]<- 1
        }
        else{
          sent<- c(sent, c(rep(i, length(sent_bnd[i-1]+1:sent_bnd[i])- length(sent))))
          #  sent[sent_bnd[i-1]+1:sent_bnd[i]]<- i
        }
        if(i==length(sent_bnd)){
          sent<- c(sent, c(rep(i+1, length(sent_bnd[i]+1:nrow(out))- length(sent))))
        }
      }
      out$sent<- sent
    } else{
      out$sent<- 1
    }

    return(out)
  }
  
  out<- map_sent(out)
  
  # map lines:
  map_line<- function(out){
    line<- NULL
    lines<- sort(unique(out$X8));
    # as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
    #lines<- as.numeric.factor(lines)
    
    for(i in 1:length(lines)){
      loc_lines<- which(out$X8==lines[i])
      line<- c(line, rep(i, length(loc_lines)))
      out$space[length(line)]<- 2
    }
    out$line<- line
    
    return(out)
  }
  
  out<- map_line(out)
  
  
  # map words:
  map_words<- function(out){
    
    out$X6<- as.character(out$X6)
    out$word<- NULL
    curr_sent=1
    curr_word=1
    sent_line<- which(out$space==2); sent_line<- sent_line+1
    
    for(i in 1:nrow(out)){
      
      newSent<- curr_sent!= out$sent[i]
      
      out$word[i]<- curr_word
      if(out$X6[i]== ""& !newSent){
        curr_word<- curr_word+1
        out$word[i]<- curr_word
      }
      
      
      if(is.element(i, sent_line)){
        if(out$X6[i]!="."){
          curr_word<- curr_word+1
        }
        out$word[i]<- curr_word
      }
      
      if(newSent){
        curr_sent<- curr_sent+1
        curr_word<- 1
        out$word[i]<- curr_word
      }
      
    }
    
    return(out)
  }
  
  out<- map_words(out)
  
  # change column names:
  colnames(out)<- c("char", "letter", "x1", "y1", "x2", "y2", "space", "sent",
                    "line", "word")
  
  # map characters per line (for Eye Doctor):
  out$line_char<- NA
  unq_line<- unique(out$line)
  for(i in 1:length(unq_line)){
    line<- which(out$line==unq_line[i])
    out$line_char[line[1]:line[length(line)]]<- 1:length(line)
  }
  
  return(out)
}


# Use stimuli information to create a coordinate map for each pixel on the screen
# This makes it possible to find exactly what participants were fixating
coord_map<- function(coords, x=ResX, y= ResY){ 
  #as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
  #coords$x1<- as.numeric.factor(coords$x1); coords$x2<- as.numeric.factor(coords$x2)
  #coords$y1<- as.numeric.factor(coords$y1); coords$y2<- as.numeric.factor(coords$y2)
  coords$id<- 1:nrow(coords)
  
  map<- data.frame(matrix(NA, nrow = y, ncol = x))
  
  for(i in 1:nrow(coords)){
    map[coords$y1[i]:coords$y2[i],coords$x1[i]:coords$x2[i]]<- coords$id[i]
    
  }
  
  return(map)
}


# Create a data frame with information about trials, to be used for pre-processing
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
  #  message(paste(" Diplicated trial", duplicated, "for file:", data, "\n"))
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



# Basic pre-processing and extraction of fixations from data file:
parse_fix<- function(file, map, coords, trial_db, i, ResX, ResY, keepLastFix, hasText=TRUE){
  
  get_FIX_stamp<- function(string){as.numeric(substr(string, 1, unlist(gregexpr(pattern ='\t', string))[1]-1))}
  
  
  loc_blinks<- function(blink_time, s_time){findInterval(blink_time, s_time)+1 }
  #loc_blinks<- function(blink_time, s_time){which.min(abs(s_time - blink_time))+1 }
  # +1 because it affects the next fixation
  
  
  get_x_pixel<- function(string){
    #tab_loc<- unlist(gregexpr(pattern ='\t', string))[3:4]
    #sub_str<- substr(string, tab_loc[1], tab_loc[2])
    #x<- as.numeric(unlist(gsub("[^0-9.]", "", unlist(sub_str)), ""))
    #x<- floor(x)
    a<- data.frame(do.call( rbind, strsplit( string, '\t' ) ) )
    x<- as.numeric(as.character(unlist(a$X4)))
    
    return(x)
  }
  
  get_y_pixel<- function(string){
    #tab_loc<- unlist(gregexpr(pattern ='\t', string))[4:5]
    #sub_str<- substr(string, tab_loc[1], tab_loc[2])
    #y<- as.numeric(unlist(gsub("[^0-9.]", "", unlist(sub_str)), ""))
    #y<- floor(y)
    a<- data.frame(do.call( rbind, strsplit( string, '\t' ) ) )
    y<- as.numeric(as.character(unlist(a$X5)))
    
    return(y)
  }
  
  # trial_db<- trial_info(file)
  
  
  # for(i in 1:nrow(trial_db)){
  # get position of fixation stamps:
  SFIX_stamps<- which(grepl('SFIX', file[trial_db$start:trial_db$end]))
  EFIX_stamps<- which(grepl('EFIX', file[trial_db$start:trial_db$end]))
  
  if(length(SFIX_stamps)==0 | length(EFIX_stamps)==0){
    # means that there was no complete fixation on this trial (i.e,
    # participant likely pressed end button by mistake)
    raw_fix<- NULL
    return(raw_fix)
    message(sprintf("No fixations in Trial %d: excluded", j))
  }
  
  if(EFIX_stamps[1]<SFIX_stamps[1]){ # removes fixation that triggered gaze box
    EFIX_stamps<- EFIX_stamps[-1]
  }
  
  if(EFIX_stamps[length(EFIX_stamps)]< SFIX_stamps[length(SFIX_stamps)]){
    SFIX_stamps<- SFIX_stamps[-length(SFIX_stamps)]
  } # fixation was not terminated before the end of trial
  
  # get start and end time of fixations:
  s_time<- get_FIX_stamp(file[SFIX_stamps+ trial_db$start])  # start time of fixation
  e_time<- get_FIX_stamp(file[EFIX_stamps+ trial_db$start-2]) # end time of fixation 
  
  # calculate fixation durations:
  fixDur<- e_time- s_time
  
  # get x pixel position:
  x<- get_x_pixel(file[EFIX_stamps+ trial_db$start-1])
  
  # get y pixel position:
  y<- get_y_pixel(file[EFIX_stamps+ trial_db$start-1])
  
  # find blinks:
  blink_stamp<- which(grepl('EBLINK', file[trial_db$start:trial_db$end]))
  blink_time<- get_FIX_stamp(file[blink_stamp+ trial_db$start])-1
  blink_out<- which(blink_time<s_time[1]| blink_time>e_time[length(e_time)])
  
  if(length(blink_out>0)){ # blinks outside time window that is analysed
    blink_time<- blink_time[-blink_out]  # remove them
  }
  
  if(i!= 48){
    blink_pos<- loc_blinks(blink_time, s_time)
  } else{
    blink_pos<- NA
  }
 
  blink<- rep(0, length(s_time))
  blink[blink_pos]<-1
  
  #if(length(blink)>length(e_time)){
  #  blink<- blink[-length(blink)]
  #}
  
  # merge into a dataframe:
  fix<- data.frame(s_time, e_time, fixDur, x, y, blink)
  
  #-----------------------------------------------#
  #    map fixations to stimuli on the screen:    #
  #-----------------------------------------------#
  
  loc<- NULL; raw_fix<- NULL; temp<- NULL; sub<- NULL
  s_time<- NULL; e_time<- NULL; xPos<- NULL; yPos<- NULL
  item<- NULL; cond<- NULL; seq<- NULL; fix_num<- NULL; fix_dur<- NULL
  sent<- NULL; line<- NULL; word<- NULL; char_trial<- NULL
  max_sent<- NULL; max_word<- NULL; intersent_regr<- NULL
  intrasent_regr<- NULL; blink<- NULL; outOfBnds<- NULL; outsideText<- NULL
  
  if(hasText){
    # max word for each sentence:
    curr_sent<- matrix(0, max(coords$sent),2)
    curr_sent[,1]<- c(1:max(coords$sent))
  }

  
  for(j in 1:nrow(fix)){
    
    if(hasText){
      loc<- map[fix$y[j], fix$x[j]] # locate fixation
    }
    
    # general info:
    sub[j]<- i
    item[j]<- trial_db$item
    cond[j]<- trial_db$cond
    seq[j]<- trial_db$seq
    fix_num[j]<- j
    fix_dur[j]<- fix$fixDur[j]
    
    # info from asc file:
    s_time[j]<- fix$s_time[j];
    e_time[j]<- fix$e_time[j]
    xPos[j]<- fix$x[j];
    yPos[j]<- fix$y[j]
    blink[j]<- fix$blink[j]
    
    
      if(xPos[j]<1 | xPos[j]> ResX | yPos[j]< 1 | yPos[j]>ResY){ # fixation is out of bounds
        outOfBnds[j]<- 1
      } else{
        outOfBnds[j]<- 0
        
        if(hasText){
          # outside of text area?
          if(is.na(loc)){
            outsideText[j]<- 1
          } else{
            outsideText[j]<- 0
          }
        }
      }
    
    if(fix$x[j]<0){
      loc<- NA
      outOfBnds[j]<- 1
      outsideText[j]<- 1
    }
   
    
    if(hasText){   
      # stimuli info:
      if(!is.null(loc)){
        sent[j]<- coords$sent[loc]
        line[j]<- coords$line[loc]
        word[j]<- coords$word[loc]
        char_trial[j]<- as.numeric(as.character(levels(coords$char[loc])[coords$char[loc]]))+1
        # +1 bc Eyetrack counts from 0
      } else{
        sent[j]<- NA; line[j]<- NA; word[j]<- NA; char_trial[j]<- NA
      }
      
      # saccade stuff:
      if(j==1){
        max_sent[j]<- sent[j]
      } else{
        max_sent[j]<- max_sent[j-1]
        
        if(!is.na(max_sent[j])& !is.na(sent[j]) & sent[j]> max_sent[j]){
          max_sent[j]<- sent[j]
        }
      }
      
      # maximum word:
      if(j==1){
        max_word[j]<- abs(word[j])
        curr_sent[sent[j],2]<- abs(word[j])
      } else{
        max_word[j]<- curr_sent[sent[j],2]
        if(!is.na(word[j])& abs(word[j])>curr_sent[sent[j],2]){
          max_word[j]<- abs(word[j])
          curr_sent[sent[j],2]<- abs(word[j]) # replace new max word
        }
      }
      
      # Regression stuff:
      if(!is.na(max_sent[j])& !is.na(sent[j]) & sent[j]< max_sent[j]){
        intersent_regr[j]<- 1
      } else{
        intersent_regr[j]<- 0
      }
      
      # intra-sentence regressions:
      if(!is.na(word[j])& abs(word[j])<max_word[j]){
        intrasent_regr[j]<- 1
      }else{
        intrasent_regr[j]<- 0
      }
      
    } else{ # end of if hasText
      sent[j]=NA; max_sent[j]=NA; line[j]=NA; word[j]=NA; max_word[j]=NA;
      char_trial[j]=NA; intrasent_regr[j]=NA; intersent_regr[j]=NA; outsideText[j]=NA
    } 
    
    
   
    
  } # end of j loop
  
  if(length(outsideText)!= length(item)){
    outsideText[length(outsideText):length(item)]<- NA
  }
  
  raw_fix<- data.frame(sub,item, cond, seq, s_time, e_time,xPos, yPos, fix_num, fix_dur,
                       sent, max_sent, line, word, max_word, char_trial, intrasent_regr, intersent_regr, blink,
                       outOfBnds, outsideText)
  if(keepLastFix==FALSE){
    # remove last fixation on trial (due to participants' making a decision to press the button)
    raw_fix<- raw_fix[-nrow(raw_fix),]
  }
  
  if(hasText==TRUE){
    raw_fix$hasText<-1
  }else{
    raw_fix$hasText<-0
  }

  
  return(raw_fix)
}


#############
# PLOTFIX.R #
#############

plot_fix<- function(coords, raw_fix_temp, i, j, ResX, ResY, hasText=TRUE, plotSecondPass=FALSE, reAligned=FALSE){
  
  remap_letters<- function(letter, y){ # adjusted y position for easier reading
    letter<- as.character(letter)
    ascenders<- c("b", "d", "f", "h", "i", "k", "l", "t")
    descenders<- c("g", "j", "p", "q", "y")
    punct<- c(",", ".")
    caps<- which(grepl("[A-Z]",letter))
    
    which_asc<- which(is.element(letter, ascenders))
    which_desc<- which(is.element(letter, descenders))
    which_punct<- which(is.element(letter, punct))
    
    y[which_desc]<- y[which_desc]- 2
    y[which_asc]<- y[which_asc]+1
    y[which_punct]<- y[which_punct]-4
    y[caps]<- y[caps]+1
    return(y)
  }
  
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2, 
          function(x) 
            rgb(x[1], x[2], x[3], alpha=alpha))  
  }
  
  # create new directory for images:
  mainDir <- getwd()
  imgDir <- "img"
  subDir<- paste("s", i, sep= "")
  
  if (!file.exists(imgDir)){
    dir.create(file.path(mainDir, imgDir), showWarnings = FALSE)
  }
  
  if (!file.exists(paste(imgDir, "/", subDir, sep=""))){
    dir.create(file.path(imgDir, subDir), showWarnings = FALSE)
  } 
  
  # create output string
  if(!reAligned){
    output= paste(imgDir, "/", subDir, "/", "Sub_", i, "_", "item_", j, ".png", sep="")
  }else{
    output= paste(imgDir, "/", subDir, "/", "Sub_", i, "_", "item_", j, "_ReAligned.png", sep="")
  }
   
  
  
  # open file for plotting:
  png(filename = output, width = ResX, height = ResY, 
      units = "px", pointsize = 12, bg="white", res = 100)          
  par(mar=c(0,0,0,0)) # set plot margins to 0
  
  
  # create empty plot:
  plot(NA, axes=F, main="", xlab="" ,ylab="", col="black", ylim=c(0, ResY), xlim=c(0, ResX), xaxs="i", yaxs="i")
  
  if(hasText){
    # convert coordinates to numbers:
    coords$y1<- as.numeric(as.character(coords$y1))
    coords$y2<- as.numeric(as.character(coords$y2))
    
    coords$x1<- as.numeric(as.character(coords$x1))
    coords$x2<- as.numeric(as.character(coords$x2))
    
    # Plot stimuli that appeared on screen:
    rect(coords$x1, ResY-coords$y1, coords$x2, ResY-coords$y2, col= "white", border= "#CFCBCB")
    
    xpos= coords$x1+ (coords$x2- coords$x1)/2
    ypos= ResY-coords$y1- (coords$y2- coords$y1)/2
    
    # correct y pos of some letter for better readibility:
    ypos<- remap_letters(coords$letter, ypos)
    
    # print letters:
    text(xpos, ypos, coords$letter, col= "black")
    
    #symbols(raw_fix$xPos, raw_fix$yPos, circles=rep(0.1, nrow(raw_fix)))
    
    # add saccades
    # for (k in 1:nrow(raw_fix_temp)){
    #   if(k==1){ next}
    #   x1<- raw_fix_temp$xPos[k-1]+6
    #   y1<- ResY-raw_fix_temp$yPos[k-1]
    
    #   if(x2>x1){
    #     x2<- raw_fix_temp$xPos[k]-10
    #   } else{
    #    x2<- raw_fix_temp$xPos[k]+10
    #   }
    
    #   y2<- ResY-raw_fix_temp$yPos[k]
    
    # fixPosX<- x1+ (x2-x1)/2
    # fixPosY<- y1+ (y2-y1)/2
    
    #    arrows(x1, y1, x2, y2, col = add.alpha("blue",0.2), lty= 1, lwd=2, length=0.10)
    
    #text(fixPosX, fixPosY, raw_fix_temp$fix_num, col= add.alpha("blue",0.3))
    # }
    
    if(plotSecondPass){ # plot first- and second-pass fixations in different colours 
      first<- subset(raw_fix_temp, intrasent_regr==0 & intersent_regr==0)
      second<- subset(raw_fix_temp, intrasent_regr==1 | intersent_regr==1)
      
      # first-pass:
      points(x= first$xPos, y=ResY-first$yPos, pch = 16,  col= add.alpha("green",0.20),
             cex= 0.7*(first$fix_dur/75))
      points(x= first$xPos, y=ResY-first$yPos, pch = 16, cex=0.7, col="green")
      
      # second-pass:
      points(x= second$xPos, y=ResY-second$yPos, pch = 16,  col= add.alpha("orange",0.20),
             cex= 0.7*(second$fix_dur/75))
      points(x= second$xPos, y=ResY-second$yPos, pch = 16, cex=0.7, col="orange")
      
    } else{ # plot all fixations with the same colour
      # plot fixation durations:
      if(!reAligned){
        points(x= raw_fix_temp$xPos, y=ResY-raw_fix_temp$yPos, pch = 16,  col= add.alpha("green",0.20),
               cex= 0.7*(raw_fix_temp$fix_dur/75))
        points(x= raw_fix_temp$xPos, y=ResY-raw_fix_temp$yPos, pch = 16, cex=0.7, col="green")
      } else{
        for(i in 1:nrow(raw_fix_temp)){
          #whichCol<- "green"
          if(raw_fix_temp$reAligned[i]=="No"){
            whichCol<- "green"
          }
          if(raw_fix_temp$reAligned[i]=="Yes"){
            whichCol<- "blue"
          }
          
          if(!exists('whichCol')){
            whichCol<- "black"
          }
          
          #if(raw_fix_temp$reAligned[i]=="Yes" & abs(raw_fix_temp$line[i]- raw_fix_temp$prevLine[i])>1){
          #  whichCol<- "red"
          #}
          points(x= raw_fix_temp$xPos[i], y=ResY-raw_fix_temp$yPos[i], pch = 16,  col= add.alpha(whichCol,0.20),
                 cex= 0.7*(raw_fix_temp$fix_dur[i]/75))
          points(x= raw_fix_temp$xPos[i], y=ResY-raw_fix_temp$yPos[i], pch = 16, cex=0.7, col=whichCol)
        }
      }

    }
    
    # plot fixation numbers:
    text(raw_fix_temp$xPos, ResY-raw_fix_temp$yPos+15, raw_fix_temp$fix_num, col= add.alpha("red",0.6))
    
    
    
  } else{ # end of hasText
    
    # plot fixations:
    points(x= raw_fix_temp$xPos, y=ResY-raw_fix_temp$yPos, pch = 16,  col= add.alpha("green",0.20),
           cex= 0.7*(raw_fix_temp$fix_dur/75))
    points(x= raw_fix_temp$xPos, y=ResY-raw_fix_temp$yPos, pch = 16, cex=0.7, col="green")
    
    # fixation numbers:
    text(raw_fix_temp$xPos, ResY-raw_fix_temp$yPos+15, raw_fix_temp$fix_num, col= add.alpha("red",0.6))
  }
  
  # print original label:
  
  if(!reAligned){
    text(ResX - 0.05*ResX, ResY -ResY*0.02, 'ORIGINAL', col= 'black', face=2)
  } else{
    text(ResX - 0.05*ResX, ResY -ResY*0.02, 'REALIGNED', col= 'black', face=2)
  }
  
  
  dev.off() # close file
  
}


reAlign<- function(rawfix, coords, map, ResX, ResY){
  
  #------------------------------------------
  #                Functions:
  #------------------------------------------
  
  # Check for a return sweep (RS):
  RS<- function(i, rawfix, coords, reqYthresh=TRUE, reqXthresh=TRUE, Ythresh= 1/4,
                Xthresh= 8, threshSimilar= 2/3){
    
    if(i==1){ # first fixation can't be a return sweep
      return(0) 
    }
    
    ### settings:
    lw<- coords$x2[1]-coords$x1[1] # letter width
    lh<- coords$y2[1]-coords$y1[1] # letter height
    meetXthresh<- FALSE
    meetYthresh<- FALSE
    nextSimilar<- FALSE
    
    
    #############################
    #         conditions
    
    # leftward saccade?
    leftSacc<- rawfix$xPos[i]< rawfix$xPos[i-1]
    
    # downward saccade?
    downSacc<- rawfix$yPos[i]> rawfix$yPos[i-1]
    
    if(downSacc & reqYthresh){ # x pixels downwards required for RS 
      Ydiff<- lh*Ythresh # threshold to be met
      trueYdiff<- rawfix$yPos[i]- rawfix$yPos[i-1] # Y distance traveled 
      meetYthresh<- trueYdiff >= Ydiff
    }
    
    if(leftSacc & reqXthresh){ # x pixels leftwards required for RS 
      Xdiff<- lw*Xthresh # threshold to be met
      trueXdiff<- rawfix$xPos[i-1]- rawfix$xPos[i] # X distance traveled
      meetXthresh<- trueXdiff >= Xdiff
    }
    
    # next fixation with 'similar' Y pos?
   # if(i!= nrow(rawfix)){ # only if not the last fixation in trial..
   #   howSimilar<- threshSimilar*lh # within x/yth of a letter height
   #   nextSimilar<- rawfix$yPos[i+1]>= rawfix$yPos[i]- howSimilar | rawfix$yPos[i+1]<= rawfix$yPos[i]+ howSimilar
   # }
    
    ###### Point system:
    #   - leftward saccade: 1 point
    #   - leftward saccade meeting threshold: 1 point
    #   - downward saccade: 2 points
    #   - downward saccade meeting threshold: 1 point
    #   = 5 max points
    
    maxPoints<- 1 +2
    if(reqYthresh){
      maxPoints<- maxPoints+1
    }
    
    if(reqXthresh){
      maxPoints<- maxPoints+1
    }
    
    #----------------------------
    currPoints<- 0 # start with 0
    
    if(leftSacc){
      currPoints<- currPoints + 1/maxPoints
      if(meetXthresh){
        currPoints<- currPoints+ 1/maxPoints
      }
    }
    
    if(downSacc){
      currPoints<- currPoints +2/maxPoints
      if(meetYthresh){
        currPoints<- currPoints+ 1/maxPoints
      }
    }
    
    #if(nextSimilar){
    #  currPoints<- currPoints +1/maxPoints
    #}
    
   # prob<- currPoints/maxPoints
    return(round(currPoints, 2))
    
  }
  
  
  # Check if fixation is still on the same line (SL):
  SL<- function(i, rawfix, coords, curr_line, xstart, xend, RSthresh= 0.8, SaccThresh=16){
    
    if(i==1){ # first fixation will be on the first line (assuming gaze box)
      return(1)
    }
    
    # Fixation is already assigned to the current line?
    isAssigned<- rawfix$outsideText[i]==0 & rawfix$line[i]== curr_line
    
    # Is current fixation a likely return sweep?
    likelyRS<- rawfix$pRS[i]>= RSthresh
    
    #how much skipped info if fixation is no longer on the same line?
    ppl<- coords$x2[1]- coords$x1[1] # pixels per letter
    pixLeft<- xend[curr_line, 2] - rawfix$xPos[i] # pixels left until end of line
    letLeft<- pixLeft/ppl # letters left until end of line
    totalLetters<-  (xend[curr_line, 2]- xstart[curr_line, 2])/ ppl # num of letters on line
    textLeft<-  totalLetters- (totalLetters- letLeft) # num letters from current xPos until end of line
    propLeft<- textLeft/totalLetters # prop of text left on screen
    
    #### How long was the previous saccade?
    saccLen<- abs(rawfix$xPos[i] - rawfix$xPos[i-1])/ppl

    
    
    
    ###### Point system:
    #   - Fixation is already assigned to the current line: 2 points
    #   - Fixation is not likely a return sweep: 1 point
    #   - Discourage skipping text on the line: 1 points max (function of remaining text)
    #   - The preceding saccade is short (<16 letters): 2 points max (weighted)
    #   = max 6 points
    
    maxPoints<- 2+1+1+2
    currPoints<- 0
    
    if(isAssigned){ # already assigned to same line
      currPoints<- currPoints + 2/maxPoints
    }
    
    if(!likelyRS){ # not likely a RS
      currPoints<- currPoints + 1/maxPoints
    }
    
    # weight points by amount of text left on the line (max 1)
    currPoints<- currPoints+ (1-propLeft)/maxPoints
    
    # weight by saccade length:
    if(saccLen>SaccThresh){
      currPoints<- currPoints+0 # i.e., no additional points
    } else{
      # weight points by distance traveled from previous fixation
      currPoints<- currPoints+ (2*(1-(saccLen/SaccThresh)))/maxPoints
    }
    
    currPoints<- round(currPoints,2)
    
    return(abs(currPoints))
    
  }
  
  # Re-map fixation info after re-aligning it:
  reMap<- function(rawfix, i, map, coords, newX=NULL, newY=NULL){
    rawfix$reAligned[i]<- 1
    rawfix$prevLine[i]<- rawfix$line[i]
    rawfix$prevX[i]<- rawfix$xPos[i]
    rawfix$prevY[i]<- rawfix$yPos[i]
    
    if(hasArg(newX)){ # if new x to be replaced..
      rawfix$xPos[i]<- newX
    }
    
    if(hasArg(newY)){ # if new y to be replaced..
      rawfix$yPos[i]<- newY
    }
    
    # new location on the screen:
    loc<- map[rawfix$yPos[i], rawfix$xPos[i]]
    
    rawfix$sent[i]<- coords$sent[loc]
    rawfix$word[i]<- coords$word[loc]
    rawfix$line[i]<- coords$line[loc]
    
    return(rawfix)
  }
  
  #---------------------------------------
  # get some info about position of text:
  #---------------------------------------
  
  ystart<- coords$y1[1] # start of first line on y-axis
  yend<- coords$y2[nrow(coords)] # end of last line on y-axis
  nlines<- max(coords$line) # number of lines in trial
  letterHeight<- coords$y2[1]- coords$y1[1]
  
  # start position of each line
  xstart<- matrix(nrow = nlines, ncol = 2, data = 0)
  xstart[1:nlines,1]<- 1:nlines
  
  ystart<- matrix(nrow = nlines, ncol = 2, data = 0)
  ystart[1:nlines,1]<- 1:nlines
  
  
  # end position of each line
  xend<- matrix(nrow = nlines, ncol = 2, data = 0)
  xend[1:nlines,1]<- 1:nlines
  
  yend<- matrix(nrow = nlines, ncol = 2, data = 0)
  yend[1:nlines,1]<- 1:nlines
  

  
  for(i in 1:nlines){
    a<- subset(coords, line==i)
    xstart[i,2]<- a$x1[1]
    xend[i,2]<- a$x2[nrow(a)]
    
    ystart[i,2]<- a$y1[1]
    yend[i,2]<- a$y2[1]
  }
  
  lineCenter<- ystart[,2] + letterHeight/2
  
  
  ####################################################
  #            Process fixations here:               #
  ####################################################
  
  rawfix$pRS<- NULL
  rawfix$pILS<- NA
  #rawfix$pSL<- NULL # prob fixation is still on the same line
  rawfix$reAligned<- '0'
  rawfix$prevX<- NA
  rawfix$prevY<- NA
  rawfix$prevLine<- NA
  
  # check first fixation:
 # curr_line<- 1 # start at line 1 (assuming gaze box)
  
#  if(rawfix$line[1]!=1){
#    if(rawfix$yPos[1]< ystart[1,2]){ # first fix is above first one
#      rawfix<- reMap(rawfix, 1, map, coords, newY= ystart[1,2]+1)
#    }
    
#    if(rawfix$yPos[1]> yend[1,2]){ # first fix is below first line
#      rawfix<- reMap(rawfix, 1, map, coords, newY= yend[1,2]-1)
#    }
#  }
  
  ## Calculate probability of return sweep for each fixation:
  for(i in 1:nrow(rawfix)){
    rawfix$pRS[i]<- RS(i, rawfix, coords)
    
    if(i>1){
      if(rawfix$pRS[i]<1 & rawfix$yPos[i]> rawfix$yPos[i-1]+letterHeight/2){
        rawfix$pRS[i]<- 1
      }
    }
    
    rawfix$prevX[i]<- rawfix$xPos[i]
    rawfix$prevY[i]<- rawfix$yPos[i]
    
   # check for interline saccade (i.e., returning to previous lines)
    if(i>1){
      if(rawfix$yPos[i]< rawfix$yPos[i-1]-letterHeight/2){
        rawfix$pILS[i]<- 1
      }else{
        rawfix$pILS[i]<- 0
      }
    }
    
  }
  
# find return sweep fixations:
RsweepFix<- c(which(rawfix$pRS==1), which(rawfix$pILS==1))
RsweepFix<- sort(RsweepFix)

### re-align fixations according to line passes.
# A line pass is defined as all the fixations that occured between two return sweeps.
# By definition, these must have occured on the same line:

for(i in 0:length(RsweepFix)+1){
  if(i==1){ # first line pass is between first fixation in trial and first return sweep
    linePass<- rawfix[1:RsweepFix[i]-1,]
  } else{
    if(i>length(RsweepFix)){ # fixations after last return sweep on trial
      linePass<- rawfix[RsweepFix[i-1]:nrow(rawfix),]
    }else{
      linePass<- rawfix[RsweepFix[i-1]:RsweepFix[i],]
      linePass<- linePass[-nrow(linePass),]
    }

  }
  
  if(nrow(linePass)==1){ # skip if only one fix on line (likely exploratory behaviour)
    next
  }
  
  # take the average y position of all line pass fixations:
  avgYpos<- mean(linePass$yPos,na.rm=T)
  
  # On which line are we?
  #whichLine<-  findInterval(avgYpos, lineCenter)
  whichLine<- which.min(abs(lineCenter - avgYpos))
  
  #print(i)
  # re-align fixations (if necessary):
  for(j in 1:nrow(linePass)){
    onLine<- linePass$yPos[j]> ystart[whichLine,2] & linePass$yPos[j]< yend[whichLine,2]
    
    if(!onLine){
      if(linePass$yPos[j]< ystart[whichLine,2]){ # fixation is above line
          rawfix<- reMap(rawfix, linePass$fix_num[j], map, coords, newY= ystart[whichLine,2]+5)
        
       }else{ # fixation is below line
          rawfix<- reMap(rawfix, linePass$fix_num[j], map, coords, newY= yend[whichLine,2]-5)
       }
      rawfix$reAligned[linePass$fix_num[j]]<- "Yes" # mark as re-aligned
    } else{
      rawfix$reAligned[linePass$fix_num[j]]<- "No" # mark as not re-aligned
    }
    
    
  } # end of re-align loop
  
  
} # end of line pass loop


  
  
  
#  for(i in 1:nrow(rawfix)){
#    rawfix$pSL[i]<- SL(i, rawfix, coords, curr_line, xstart, xend)
#    rawfix$prevX[i]<- rawfix$xPos[i]
#    rawfix$prevY[i]<- rawfix$yPos[i]
#    
#    
#    if(rawfix$pSL[i]> rawfix$pRS[i]){ # greater prob. of same line fixation than RS prob.
#      # check if fixation is located on current line:
#      onLine<- rawfix$yPos[i]> ystart[curr_line,2] & rawfix$yPos[i]< yend[curr_line,2]
#      if(!onLine){
#        if(rawfix$yPos[i]< ystart[curr_line,2]){ # fixation is above line
#          rawfix<- reMap(rawfix, i, map, coords, newY= ystart[curr_line,2]+1)
#          #rawfix$yPos[i]<- ystart[curr_line,2]+1 # bring fixation down
#        }else{ # fixation is below line
#          rawfix<- reMap(rawfix, i, map, coords, newY= yend[curr_line,2]-1)
#          #rawfix$yPos[i]<- yend[curr_line,2]-1 # bring fixation up
#        }
#      }
#      
#    } else{ # there is evidence that a RS was made
#      #if(!is.na(rawfix$line[i])){
#      #  curr_line<- rawfix$line[i]
#      #} else{
#        curr_line<- findInterval(rawfix$yPos[i], ystart[,2]+letterHeight/2)
#      #}
#      
#    }
#    
#   # Was fixation re-aligned?
#   if(rawfix$prevX[i]== rawfix$xPos[i] & rawfix$prevY[i]== rawfix$yPos[i]){
#     rawfix$reAligned[i]<- "No"
#   }else{
#     rawfix$reAligned[i]<- "Yes"
#   }  
#    
#  }
  

  return(rawfix)
  
}
