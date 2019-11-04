
library(simr)

load("Experiment 1/data/data.Rda")

# remove phon condition:
data<- subset(data, prev!= "phon")
#data$prev<- droplevels(data$prev)

# set-up contrast coding:
library(lme4)

data$prev<- as.factor(data$prev)
levels(data$prev)
data$prev<- factor(data$prev, levels= c("valid", "invalid", "orth"))
data$deg<- as.factor(data$deg)

contrasts(data$deg)<- c(1,-1)
contrasts(data$prev)

cmat<- matrix(data = c(-1,1,0,0,1, -1), nrow=3, ncol=2,
              dimnames = list(c('valid', 'invalid', 'orth'),
                              c(".PB", ".Orth_PB")))

library(MASS)
# need to use the generalised inverse to get the correct comparisons
inv.cmat<- fractions(t(ginv(cmat))) 

# copy row and column names over from cmat to make interpretation easier
colnames(inv.cmat) <- colnames(cmat)
rownames(inv.cmat)<- rownames(cmat)
contrasts(data$prev)<- inv.cmat
contrasts(data$prev)

contrasts(data$deg)


## FFD:
summary(FFDN1<- lmer(log(FFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=data))


#############
library(simr)

SimData<- NULL
NSim= 100 # number of simulations
nsub= 64
nitem= 8

# parameters:
b <- coef(summary(FFDN1))[,1] # fixed intercept and slope
RE <- VarCorr(FFDN1) # random effects
s <- 0.28325 # residual sd

sub <- c(1:nsub)
item<- c(1:nitem)
prev <- c('valid', 'invalid', 'orth')
deg<- c('0', '20')

X <- expand.grid(prev=prev, deg=deg, sub= sub, item= item)

X$prev<- as.factor(X$prev)
contrasts(X$prev)<- inv.cmat
X$deg<- as.factor(X$deg)
contrasts(X$deg)<- c(1, -1)

# make fake data:
model1 <- makeLmer(y ~ prev*deg + (1|sub) + (1|item), fixef=b, VarCorr= RE, sigma=s, data=X)
summary(model1)

pow<- powerSim(model1, test = fixed('deg','anova'), nsim= 100)
pow<- powerSim(model1, test = fixed('prev','anova'), nsim= 100)
pow<- powerSim(model1, test = fixed('prev:deg','lr'), nsim= 100)

summary(pow)
pow$errors
