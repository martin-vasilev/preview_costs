
# Martin R. Vasilev, 2017

rm(list= ls())

library(readr)

design<- NULL

for(i in 1:66){
  t<- read.delim(paste("des/RND_design", toString(i), ".dat", sep=''), header=FALSE)
 # t <- read_delim(paste("des/RND_design", toString(i), ".dat", sep=''), 
  #                     " ", escape_double = FALSE, trim_ws = TRUE)
  t$sub<- i
  design<- rbind(design, t)
}

#design<- subset(design, item<181) # remove practice items

colnames(design)<- c('item', 'cond', 'sub')
table(design$item, design$cond)
