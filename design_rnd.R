
rm(list= ls())

design <- read.delim("Experiment 3b/E2DEG/E2DEG/design/design_same.dat", header=FALSE, stringsAsFactors=FALSE)
design$V1<- NULL


for(i in 1:66){
  
  d<- design[, c(1, i+1)]
  rows <- sample(nrow(d))
  rd <- d[rows, ]
  
  write.table(rd, paste('des/RND_design', i, '.dat', sep= ''), row.names = F, col.names = F, sep = '\t')
  
}