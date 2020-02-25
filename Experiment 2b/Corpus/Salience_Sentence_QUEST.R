## SCRIPT TO DEGRADE TARGET WORDS IN SENTENCES #####################
## IF YOU CREATED YOUR STIM'S WITH THE SCRIPT, PLS CITE:           #
## Marx, C., Hawelka, S., Schuster, S., & Hutzler, F. (2015).      #
## An incremental boundary study on parafoveal preprocessing in    #
## children reading aloud: Parafoveal masks overestimate the       #
## preview benefit. Journal Cognitive Psychology, 27, 2044-5911.   #
## doi:10.1080/20445911.2015.1008494                               #
##                                                                 #
## AS YET ONLY RUNS ON UNIX SYSTEMS, BC IT REQUIRES pngtopnm       #
####################################################################

require("pixmap")

#################################
## DEFINE STIM PROPERTIES HERE ##
#################################
crtv <- 0   # % pixles replaced
pxpi <- 60   # pixles per inch (ppi) - treat me thoughtfully!!!!!!!
wdth <- 1024  # width of the png
hght <- 40   # height of the png
pnts <- 20   # font size (pointsize)
wsch <- 10    # width of a single character

## maximal vertical and horizontal distance for replacing pixels
## only useful for PNGs with a reasonable high resolution, but
## do not set the limits too narrow (danger of not breaking out of loop)
## defaults are the width and height of the png (true randomness)
mxDst_v <- wdth 
mxDst_h <- hght

## THE SCRIPT PRODUCES ANTI-ALIASES PNGs
## YOU CAN PROBABLY IMPROVE THE "TARGET-ABILITY" OF THE DEGRADED STIMs
## BY TURNING THE ANTIALIASES PIXEL TO BLACK - CHECK IT OUT
## HERE IS THE OPTION - default should be FALSE
## define an alternative mono-chrome color, if you wish (but most probably, 
## you will want black (i.e., 0) 
## IF YOU CHOOSE 1, THEN THE SCRIPT PERFORMS PIXEL REMOVAL!!!!!!!!!
no_antiAlias <- FALSE
altern_value <- 0 

## STIMS HERE ...
wrds <- c("Were any people injured in the accident?", "Did the customer want a dress for a wedding?", 
"Did Jesse cycle in the rain?", "Did John buy his wife a coat?", 
"Did the police try to improve the security of prisons?", "Did the painting show the ocean?", 
"Is there more interest in solar power technology these days?", 
"Was the money raised by private companies?", "Did the speaker try to tell a joke?", 
"Were the students taught how to spell the alphabet?", "Did Jessica buy a new shirt?", 
"Did the man join the army to make money?", "Did the campers return home?", 
"Did Sam feel guilty for buying an expensive car?", "Did the students go on a holiday?", 
"Did Katy show her new dress to her sister?", "Did the teen make a plea?", 
"Did the tailor repair a pair of jeans?", "Did the couple want to sweep the chimney?", 
"Did the car have problems with the engine?", "Did the youngsters make it into the movie theatre?", 
"Was the hoover broken?", "Was the meal of the day omelette?", 
"Did the astronomer investigate a new star?", "Did the old man go out for a walk?", 
"Did the actress frequently appear in commercials?", "Did the cyclists ride their bikes?", 
"Did Jane's grandmother prepare a cheesecake for the holidays?", 
"Did the camper have a hatchet?", "Were the clothes ruined?", 
"Did the cat chase a bird?", "Did the police suspect that the man was drunk?", 
"Did the gardener forget to trim his beard?", "Did the cold weather last longer than a day?", 
"Was the first part of the exam composed of essay questions?", 
"Did the comedian use a funny lisp in his act?", "Were the tourists in danger?", 
"Did the couple see dolphins during their holiday?", "Did the customers like the new talc?", 
"Did Tommy have the money to cover his losses?", "Did Susan injure herself riding a bike?", 
"Was the sword newly forged?", "Was the chain very weak?", "Did somebody die in the book?", 
"Did the old woman complain?", "Was George very unsatisfied with his result?", 
"Was Harriet afraid of a ghost?", "Could Karl share his secret with everyone?", 
"Did Joe lose his jacket?", "Did Dolly survive infancy? ", "Did the chef sharpen his pencil? ", 
"Did the crack need to be repaired?", "Did the police find a gun in the car?", 
"Did the couple go on a cruise for their honeymoon?", "Does she write poetry books?", 
"Was the show cancelled?", "Was it easy to recover from the recession?"
)

wrds= paste("QUESTION/n/n", wrds, sep= '')
  
# NUMBER OF CHARACTERS (INCLUDING SPACES) PRIOR TO THE TARGET WORD:          
Nchars <- rep(0, length(wrds))


  
# HERE YOU MUST SPECIFY UNIQUE CODES FOR THE FILENAME OF THE PNG: 
flnm <- paste(1:length(wrds), '_valid', sep= '')
  

for (i in (1:length(wrds))) {

  countChngdM = 0
  countBlackM = 0
  
  # create initial full contrast png
  png(filename = "temp.png", width = wdth, height = hght, 
      units = "px", pointsize = pnts, bg = "white", res = pxpi)          
  par(mar=c(0,0,0,0))
  plot(c(0,1), axes=F, main="", xlab="" ,ylab="", col="white", xlim=c(0,1024), ylim=c(0,768))
  text(0,768/2, wrds[i], pos=4,  family="mono", font=2)
  dev.off()

  # make and read pnm-file and extract the pxl-matrix
  system('pngtopnm ./temp.png >./temp.pnm') 
  my_matrix <- read.pnm("temp.pnm")

  # get the area - as a rectangle - where there are black pixels (i.e., the word boundaries)
  for (upperBorder in c(1:nrow(my_matrix@grey))) {
    if (sum(my_matrix@grey[upperBorder,]) < ncol(my_matrix@grey)) break  
  }
  for (lowerBorder in c(nrow(my_matrix@grey):1)) {
    if (sum(my_matrix@grey[lowerBorder,]) < ncol(my_matrix@grey)) break  
  }
  for (leftBorder in c(1:ncol(my_matrix@grey))) {
    if (sum(my_matrix@grey[,leftBorder]) < nrow(my_matrix@grey)) break  
  }
  for (rightBorder in c(ncol(my_matrix@grey):1)) {
    if (sum(my_matrix@grey[,rightBorder]) < nrow(my_matrix@grey)) break  
  }

  leftBorder <- leftBorder + Nchars[i] * wsch

  # count non-white pixels in the matrix
  for (spalte in c(leftBorder:ncol(my_matrix@grey))) {
    countBlackM <- countBlackM + length(my_matrix@grey[,spalte][my_matrix@grey[,spalte] < 1])
  }  
  
  while (1) {
    zeile <- sample(c(upperBorder:lowerBorder))[1]  
    spalte <- sample(c(leftBorder:rightBorder))[1]
    if (my_matrix@grey[zeile, spalte] < 1) {
      if (no_antiAlias) {
        remCol <- altern_value
      } else { remCol <- my_matrix@grey[zeile,spalte] }
      rand = sample.int(100, 100)
      if (rand[50] <= crtv) {
        countChngdM <- countChngdM + 1 
        # re-insert the non-white spalte elsewhere within the word boundaries
        while (1) {
          newPxlRow <- sample(c(upperBorder:lowerBorder))[1]
          newPxlCol <- sample(c(leftBorder:rightBorder))[1] 
            if (my_matrix@grey[newPxlRow,newPxlCol] == 1 &
                abs(newPxlRow-zeile) <= mxDst_v  & 
                abs(newPxlCol-spalte) <= mxDst_h) {
              my_matrix@grey[newPxlRow,newPxlCol] <- remCol
              break
            } 
         }
         my_matrix@grey[zeile,spalte] <- 1
       }
    }
    if (countChngdM/countBlackM >= crtv/100) break  
  }

  print(countChngdM/countBlackM)
  
  # PNG Files: change name here
  bmp(filename = paste(flnm[i],"_", crtv, ".bmp", sep=""), width = wdth, height = hght)
    par(mar=c(0,0,0,0))
    plot(my_matrix, axes=F, main="", xlab="" ,ylab="")
  dev.off()
  
}  
  
   
