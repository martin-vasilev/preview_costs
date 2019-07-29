
# Power analysis:

# Here, I take the meta-analyzed effect sizes from Vasilev & Angele (2017, PBR).
# The effect sizes are the mean difference (in ms) between the valid preview and the two parafoval previews 
# (phonological and orthographical).

source('https://raw.githubusercontent.com/martin-vasilev/reading_sounds/master/Functions/effect_sizes.R')

load('pwr/OrthGD.Rda')
load('pwr/PhonGD.Rda')


# Phonological preview benefit effect size (in Cohen's d):
ES= (PhonGD$statistics[1,1] - OrthGD$statistics[1,1])/sqrt(OrthGD$statistics[1,2]^2+PhonGD$statistics[1,2]^2)

mean(c(10.5,7, 13, 20))/ mean(c(16, 5.2, 15.5,8))
