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
wrds <- c("The reporter announced that no people were hort in the car accident this morning.", 
          "The client asked for another tepe of dress that would be more suitable for a wedding.", 
          "Jesse had to remove dort from his clothes after he cycled home in the rain.", 
          "John surprised his wife with a designer porse that he bought on his trip to Italy.", 
          "In the past year, the local police took storn measures to improve safety on the roads.", 
          "The painting depicted how the two streams morge to form the magnificent river.", 
          "The government tried to support the recent sorge of interest in solar power technology.", 
          "The money for a new library was raised by the local charch and members of the public.", 
          "The speaker tried to make a joke after he let out a loud borp during his presentation.", 
          "In their lesson, the students were taught what a proper noin was and how to spell it.", 
          "Jessica bought a fancy skart that she wanted to wear to the beach party.", 
          "By joining the army, the young man hoped he could sorve and protect his country.", 
          "Not long after their warst rain storm in the mountain, the campers decided to go home.", 
          "For Sam, getting a well-paid job helped parge the guilt of buying an expensive car.", 
          "After the summer tarm ended, the students went on holiday to Spain.", 
          "Katy put on her new dress and did a quick tworl to show it to her mother.", 
          "The teen made a dramatic pler to the judge in the hope of avoiding a severe sentence.", 
          "The tailor repaired the ripped slerve of the expensive leather jacket.", 
          "The couple ordered a chimney swerp before they moved into their new house.", 
          "While on the road, the car made a strange jark before the engine died.", 
          "Despite the security, the youngsters managed to quietly snerk into the movie theatre.", 
          "Michael made a mental note to bring the broken sterm hoover to the repair shop.", 
          "The restaurant offered tender berf steak as a meal of the day.", 
          "The astronomer investigated the strange barst of energy that was detected from Earth.", 
          "Because the weather seemed blerk and uninviting, the old man decided to stay at home.", 
          "Thanks to her long slerk hair, the actress was often invited to be in commercials.", 
          "Because the road looked sterp and difficult to climb, the cyclists pushed their bikes.", 
          "Jane liked the crisp verl pie that her grandmother prepared for the holidays.", 
          "The skillful camper was able to quickly clerve the logs with his hatchet.", 
          "Unfortunately, the large blerch stains had completely ruined the new clothes.", 
          "The cat chased the brown shree that had strayed from the forest looking for food.", 
          "The man spoke with a distinct slar which made the police suspicious that he was drunk.", 
          "The new gardener forgot to trim the green bish near the front door of the manor.", 
          "Susan could not believe the record colc weather they had been having all week.", 
          "The students were happy that the first parl of the exam had multiple-choice questions.", 
          "The comedian spoke with a funny losp during all his stand-up acts.", 
          "The tourists scrambled to escape the path of the charging herf of water buffalo.", 
          "The couple got to see zebras running wilk during their safari holiday.", 
          "The barber was trying a new scented tilc and his customers liked it.", 
          "Tommy had placed a large bet on the losing teap and didn’t have the money to cover it.", 
          "Susan slipped on the bottom slep of the spiral staircase and twisted her ankle.", 
          "The newly forged sword's edge was still blunk and needed to be sharpened.", 
          "The very strong chaim was made by the most expert blacksmith.", 
          "The book ended with the sudden deach of one of the main protagonists.", 
          "The old woman complained that the cold drafs was making her back ache.", 
          "George had only come fefth in the horse race but was still happy with his result.", 
          "Harriet was frightened by the evil ghist she thought lived in her house.", 
          "Karl knew he couldn't trost anyone with his secret.", "Joe was upset that he had lost his special spoot which had been gifted to him.", 
          "Dolly the sheep was the world's first clome of a mammal to survive infancy.", 
          "The TV chef always made sure he sharpened his carving knike before the show.", 
          "The inspector noticed a small creck in the building's foundation that needed repairing.", 
          "The police found an empty flosk beneath the front seat that smelled of liquor.", 
          "The married couple decided to go on a luxury cruive for their second honeymoon.", 
          "Sarah always found it embarrassing to see people flort on the street.", 
          "The children told their story in mutual morth while sitting next to the camp fire.", 
          "The lorry driver made a sudden sworve as he was about to miss the motorway exit.", 
          "The shaky boat made Carla's head sworl as she struggled to keep her balance.", 
          "The boy had a tiny smark on his face when he saw his mother scold his brother.", 
          "The admiral would never sperk badly of his superiors even if he didn't agree with them.", 
          "Before her morning run, Ana put on a thick fleeme jacket to keep herself warm.", 
          "The young athlete was happy after he finished thard in the international competition.", 
          "The workers gave the sofa a good herve and moved it up the staircase.", 
          "The fact that the next competition was on their home tarf raised the swimmers' morale.", 
          "The new type of toothpaste promised white terth just after a few days of use.", 
          "The instructor demonstrated how to properly harl a cricket ball to the children.", 
          "The little girl danced with utter gler to the sounds of pop music.", 
          "After Nick finished the race first, there was a small glerm of triumph in his eyes.", 
          "The man decided to wear a blue twerd jacket for his job interview.", 
          "The laws in the 20th century meant that people could no longer lanch suspects of crime.", 
          "The surgeon was careful to avoid narve tissue while performing the operation.", 
          "The possibility of eternal yoath has fascinated mankind for centuries.", 
          "According to the announcement, the expected slert may cause disturbances on the road.", 
          "The climber used a short hoil of rope to secure his tent in the mountain.", 
          "The recently hired dern made a commitment to improve the diversity in the department.", 
          "The students were expected to somehow glern the information for their class report.", 
          "The client was advised to contact tece support if the software was not working properly.", 
          "Rob's teachers were impressed by his constant thorst for knowledge.", 
          "The landlady asked the tenants to keep the house clern and tidy after they move in.", 
          "The old lady thanked the friendly narse who was checking on her every other week.", 
          "Dan challenged his older brother to finish the game without chert codes of any kind.", 
          "Karen got a small scar on her right cherk after she fell off her bike.", 
          "The sailor secured his boat to the brass clert on the dock and then headed home.", 
          "The CEO gave his secretary several tarse instructions and then left the office.", 
          "The cottage was surrounded by dense farn vegetation that had not been trimmed in years.", 
          "Jasmine found a great derl online and treated herself to a spa holiday.", 
          "The artist wore a satin dress with a slight shern that was made for the performance.", 
          "The children's most vivid memory of the zoo was the wild lonx sitting on a tree.", 
          "The report warned that without protection the species would cerse to exist in a decade.", 
          "Marina had a good werp after watching the sad movie drama last night.", 
          "Brad was a very meck person who never got into arguments with his colleagues.", 
          "Mary's children were old enough to earn their kerp and decided to rent their own flat.", 
          "The girl always tried to keep her clothes nert and clean when playing outside.", 
          "The news of the rising grosh sales inspired confidence in the future of the company.", 
          "Lisa spent her holiday in a social wharl of activities that freed her mind from work.", 
          "Betty thought that the light terl curtains would be a good addition to her bedroom.", 
          "According to some people, investing in real estate is well warth the money.", 
          "The children were afraid when they saw the ocean serthe restlessly beneath them.", 
          "After he left wark on Friday, Alex headed to the pub to catch up with his friends.", 
          "The new movie features a peaceful dwerf who tries to unite his kingdom.", 
          "The scientists could finally prive that the newly discovered substance is not toxic.", 
          "Visiting the dentist always made Zoe's stomach charn for the whole morning.", 
          "Johan gave his younger brother a fist bemp after he won the swimming competition.", 
          "The team needs to play much better during the last nipe minutes if they want to win.", 
          "The sound from the large harn echoed through the green valley.", 
          "The beauty queen wore a gorgeous sush over her elegant red gown.", 
          "The teacher told the child to first spim out his gum and then answer the question.", 
          "He bought a large plastic ruke to gather the fallen leaves in his front garden.", 
          "The athlete believed she would reat the rewards of all her training when she competed.", 
          "The child enjoyed eating the ripe prum she had picked from the tree.", 
          "The CEO arrived just before noom and took the seat at the head of the table.", 
          "The man applied musp to the collar of his shirt before his date.", 
          "The engineers made sure the dynamite would blask straight through the rock.", 
          "James loved a tavern blawl despite having often landed in hospital afterwards.", 
          "The boy knew he shouldn't have eaten chalf again and began to cry.", 
          "The prospector knew that his land claic would require a lot of paperwork.", 
          "The stranger began to feel failt at the sight of his own blood.", 
          "Jenny saw the first snow flate of winter and braced herself for the cold weather.", 
          "All the towns people knew about the great floop that had destroyed the town in 1912.", 
          "The fraudster knew that he could frape his brother for his many crimes.", 
          "The deer were grazing in the grassy glane in the middle of the woods.", 
          "The detective found the horrible treth when he opened the door.", 
          "The sailor fastened his large trenk so that it would not shift when the boat swayed.", 
          "Carmen thought her husband’s gift was very sweec and she thanked him for it.", 
          "The quarry produced swate to be used in the construction of houses.", 
          "The roof had fallen in after a support strot had been damaged by termites.", 
          "The car raced around the corner with great speeb and roared up the road.", 
          "The accident victim's pulve was shallow but steady when he was put in the ambulance.", 
          "The bandit was lying pryne so that the police would not see him.", 
          "It was unwise to go out at night as the salt mersh around the house was dangerous.", 
          "Abby was going to take down the ugly shelk in the corner of her living room.", 
          "The restaurant critic noted the clean plafe and silverware on the table.", 
          "Even though it was just a short drise to the store, Mike ran out of petrol.", 
          "The bully watched the child flince when he acted like he was going to punch him.", 
          "When Jane saw the slow clonch of his fists, she realised that the man is getting angry.", 
          "They lost millions when the wealthy crient decided to take his business elsewhere.", 
          "They saw the judge slooch in her chair just before she fell asleep.", 
          "Edmond thought the music had a really funky groose that made him want to dance."
)
  
# NUMBER OF CHARACTERS (INCLUDING SPACES) PRIOR TO THE TARGET WORD:          
Nchars <- rep(0, length(wrds))

# Nchars<- c(43, 29, 20, 40, 40, 42, 43, 52, 57, 56, 23, 50, 21, 40, 17, 
#            42, 25, 31, 29, 42, 56, 47, 30, 40, 27, 19, 24, 21, 40, 25, 25, 
#            30, 42, 35, 39, 32, 58, 37, 36, 43, 28, 40, 16, 31, 39, 21, 35, 
#            22, 43, 38, 54, 30, 26, 45, 49, 40, 31, 33, 19, 24, 43, 46, 33, 
#            53, 42, 44, 34, 54, 31, 63, 33, 27, 44, 25, 19, 38, 39, 46, 49, 
#            34, 60, 36, 41, 35, 36, 22, 44, 57, 60, 18, 16, 46, 42, 23, 35, 
#            29, 59, 49, 14, 34, 29, 47, 38, 51, 25, 33, 36, 26, 31, 34, 28, 
#            16, 43, 21, 37, 34, 27, 25, 42, 33, 36, 33, 30, 43, 20, 39, 43, 
#            22, 21, 45, 37, 38, 32, 28, 23, 36, 19, 44)

  
# HERE YOU MUST SPECIFY UNIQUE CODES FOR THE FILENAME OF THE PNG: 
flnm <- paste(1:length(wrds), '_orth', sep= '')
  

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
  
   
