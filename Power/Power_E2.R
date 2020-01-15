
rm(list= ls())

library(simr)


######### EXPERIMENT 2

load("Experiment 2/data/data.Rda")

# set-up contrast coding:
library(lme4)

# remove phon condition:
data<- subset(data, prev!= "phon")
#data$prev<- droplevels(data$prev)

data$deg<- as.factor(data$deg)
contrasts(data$deg)<- c(1,-1)
contrasts(data$deg)

data$prev<- as.factor(data$prev)
levels(data$prev)

data$prev<- factor(data$prev, levels= c("valid", "invalid", "orth"))
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

######################

dat1<- subset(data, prev== "valid" | prev== "invalid")
dat1$prev<- as.factor(dat1$prev)
dat1$prev<- droplevels(dat1$prev)
contrasts(dat1$prev)
contrasts(dat1$prev)<- c(1, -1)
contrasts(dat1$prev)

dat2<- subset(data, prev== "invalid" | prev== "orth")
dat2$prev<- as.factor(dat2$prev)
dat2$prev<- droplevels(dat2$prev)
contrasts(dat2$prev)
contrasts(dat2$prev)<- c(1, -1)
contrasts(dat2$prev)


#dat1$prev<- as.factor(dat1$prev)
#levels(data$prev)

#data$prev<- factor(data$prev, levels= c("valid", "invalid", "orth"))

#contrasts(data$prev)

#cmat<- matrix(data = c(-1,1,0,0,1, -1), nrow=3, ncol=2,
#              dimnames = list(c('valid', 'invalid', 'orth'),
#                              c(".PB", ".Orth_PB")))

#library(MASS)
# need to use the generalised inverse to get the correct comparisons
#inv.cmat<- fractions(t(ginv(cmat))) 

# copy row and column names over from cmat to make interpretation easier
#colnames(inv.cmat) <- colnames(cmat)
#rownames(inv.cmat)<- rownames(cmat)
#contrasts(data$prev)<- inv.cmat
#contrasts(data$prev)

#contrasts(data$deg)


## FFD:
summary(FFD_PB<- lmer(log(FFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=dat1))
summary(FFD_Orth<- lmer(log(FFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=dat2))
summary(FFD_Deg<- lmer(log(FFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=data))



#############
library(simr)

SimData<- NULL
NSim= 500 # number of simulations
nsub= 60
nitem= 17

# parameters:
b <- coef(summary(FFD_PB))[,1] # fixed intercept and slope
RE <- VarCorr(FFD_PB) # random effects
s <- 0.29492 # residual sd

sub <- c(1:nsub)
item<- c(1:nitem)
prev <- c('valid', 'invalid')
deg<- c('0', '20')

X <- expand.grid(prev=prev, deg=deg, sub= sub, item= item)

X$prev<- as.factor(X$prev)
contrasts(X$prev)<- c(-1, 1)
X$deg<- as.factor(X$deg)
contrasts(X$deg)<- c(1, -1)

# make fake data:
model1 <- makeLmer(y ~ prev*deg + (1|sub) + (1|item), fixef=b, VarCorr= RE, sigma=s, data=X)
summary(model1)

#summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))
summary(pow2<- powerSim(model1, test = fixed('prev','anova'), nsim= NSim))
summary(pow3<- powerSim(model1, test = fixed('prev:deg','lr'), nsim= NSim))

#####
SimData<- NULL


# parameters:
b <- coef(summary(FFD_Orth))[,1] # fixed intercept and slope
RE <- VarCorr(FFD_Orth) # random effects
s <- 0.30168 # residual sd

sub <- c(1:nsub)
item<- c(1:nitem)
prev <- c('orth', 'invalid')
deg<- c('0', '20')

X <- expand.grid(prev=prev, deg=deg, sub= sub, item= item)

X$prev<- as.factor(X$prev)
contrasts(X$prev)<- c(-1, 1)
X$deg<- as.factor(X$deg)
contrasts(X$deg)<- c(1, -1)

# make fake data:
model1 <- makeLmer(y ~ prev*deg + (1|sub) + (1|item), fixef=b, VarCorr= RE, sigma=s, data=X)
summary(model1)

#summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))
summary(pow2<- powerSim(model1, test = fixed('prev','anova'), nsim= NSim))
summary(pow3<- powerSim(model1, test = fixed('prev:deg','lr'), nsim= NSim))

##### Deg:

SimData<- NULL


# parameters:
b <- coef(summary(FFD_Deg))[,1] # fixed intercept and slope
RE <- VarCorr(FFD_Orth) # random effects
s <- 0.29693  # residual sd

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

summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))




########################################################################
# SFD:
########################################################################

## SFD:
summary(SFD_PB<- lmer(log(SFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=dat1))
summary(SFD_Orth<- lmer(log(SFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=dat2))
summary(SFD_Deg<- lmer(log(SFD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=data))

SimData<- NULL

# parameters:
b <- coef(summary(SFD_PB))[,1] # fixed intercept and slope
RE <- VarCorr(SFD_PB) # random effects
s <- 0.27956  # residual sd

sub <- c(1:nsub)
item<- c(1:nitem)
prev <- c('valid', 'invalid')
deg<- c('0', '20')

X <- expand.grid(prev=prev, deg=deg, sub= sub, item= item)

X$prev<- as.factor(X$prev)
contrasts(X$prev)<- c(-1, 1)
X$deg<- as.factor(X$deg)
contrasts(X$deg)<- c(1, -1)

# make fake data:
model1 <- makeLmer(y ~ prev*deg + (1|sub) + (1|item), fixef=b, VarCorr= RE, sigma=s, data=X)
summary(model1)

#summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))
summary(pow2<- powerSim(model1, test = fixed('prev','anova'), nsim= NSim))
summary(pow3<- powerSim(model1, test = fixed('prev:deg','lr'), nsim= NSim))

#####
SimData<- NULL


# parameters:
b <- coef(summary(SFD_Orth))[,1] # fixed intercept and slope
RE <- VarCorr(SFD_Orth) # random effects
s <- 0.28771  # residual sd

sub <- c(1:nsub)
item<- c(1:nitem)
prev <- c('orth', 'invalid')
deg<- c('0', '20')

X <- expand.grid(prev=prev, deg=deg, sub= sub, item= item)

X$prev<- as.factor(X$prev)
contrasts(X$prev)<- c(-1, 1)
X$deg<- as.factor(X$deg)
contrasts(X$deg)<- c(1, -1)

# make fake data:
model1 <- makeLmer(y ~ prev*deg + (1|sub) + (1|item), fixef=b, VarCorr= RE, sigma=s, data=X)
summary(model1)

#summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))
summary(pow2<- powerSim(model1, test = fixed('prev','anova'), nsim= NSim))
summary(pow3<- powerSim(model1, test = fixed('prev:deg','lr'), nsim= NSim))

##### Deg:

SimData<- NULL


# parameters:
b <- coef(summary(SFD_Deg))[,1] # fixed intercept and slope
RE <- VarCorr(SFD_Deg) # random effects
s <- 0.28256   # residual sd

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

summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))


########################################################################
# GD:
########################################################################

summary(GD_PB<- lmer(log(GD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=dat1))
summary(GD_Orth<- lmer(log(GD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=dat2))
summary(GD_Deg<- lmer(log(GD_N1)~prev*deg+ (1|subj)+ (1|item), REML = T, data=data))

SimData<- NULL

# parameters:
b <- coef(summary(GD_PB))[,1] # fixed intercept and slope
RE <- VarCorr(GD_PB) # random effects
s <- 0.33144   # residual sd

sub <- c(1:nsub)
item<- c(1:nitem)
prev <- c('valid', 'invalid')
deg<- c('0', '20')

X <- expand.grid(prev=prev, deg=deg, sub= sub, item= item)

X$prev<- as.factor(X$prev)
contrasts(X$prev)<- c(-1, 1)
X$deg<- as.factor(X$deg)
contrasts(X$deg)<- c(1, -1)

# make fake data:
model1 <- makeLmer(y ~ prev*deg + (1|sub) + (1|item), fixef=b, VarCorr= RE, sigma=s, data=X)
summary(model1)

#summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))
summary(pow2<- powerSim(model1, test = fixed('prev','anova'), nsim= NSim))
summary(pow3<- powerSim(model1, test = fixed('prev:deg','lr'), nsim= NSim))

#####
SimData<- NULL


# parameters:
b <- coef(summary(GD_Orth))[,1] # fixed intercept and slope
RE <- VarCorr(GD_Orth) # random effects
s <- 0.33646   # residual sd

sub <- c(1:nsub)
item<- c(1:nitem)
prev <- c('orth', 'invalid')
deg<- c('0', '20')

X <- expand.grid(prev=prev, deg=deg, sub= sub, item= item)

X$prev<- as.factor(X$prev)
contrasts(X$prev)<- c(-1, 1)
X$deg<- as.factor(X$deg)
contrasts(X$deg)<- c(1, -1)

# make fake data:
model1 <- makeLmer(y ~ prev*deg + (1|sub) + (1|item), fixef=b, VarCorr= RE, sigma=s, data=X)
summary(model1)

#summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))
summary(pow2<- powerSim(model1, test = fixed('prev','anova'), nsim= NSim))
summary(pow3<- powerSim(model1, test = fixed('prev:deg','lr'), nsim= NSim))


##### Deg:

SimData<- NULL


# parameters:
b <- coef(summary(GD_Deg))[,1] # fixed intercept and slope
RE <- VarCorr(GD_Deg) # random effects
s <- 0.33164    # residual sd

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

summary(pow1<- powerSim(model1, test = fixed('deg','anova'), nsim= NSim))
