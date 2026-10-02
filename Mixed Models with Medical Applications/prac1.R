# Load and preview the galaxies data set:
setwd("C:/Athang/MSc DSA/MMMA")
load(file = "galaxies.Rdata")


galaxies[1:10, ]


modelVB <- lm(Vdispersion ~ BlackHoleMass, data = galaxies)
summary(modelVB)


par(mfrow = c(1,2))

betahat <- modelVB$coeff
e <- modelVB$resid

plot(Vdispersion ~ BlackHoleMass, data = galaxies)
abline(betahat)

hist(e)


par(mfrow = c(1,2))

plot(e ~ galaxies$BlackHoleMass)
abline(h = 0)

qqnorm(e)

### 
modelVT <- lm(Vdispersion ~ Type, data = galaxies)
summary(modelVT)

###EX1
###
galaxies$Spiral <- NA

galaxies$Spiral <- ifelse(galaxies$Type=='Spiral', 1, 0)

modelNS <- lm(BlackHoleMass ~ Spiral , data = galaxies)
summary(modelNS)

##
op=par(mfrow = c(1,2))

betahat <- modelNS$coeff
e <- modelNS$resid

plot(BlackHoleMass ~ Spiral, data = galaxies)
abline(betahat)

hist(e)
par(op)
op=par(mfrow = c(1,2))

plot(e ~ galaxies$BlackHoleMass)
abline(h = 0)

qqnorm(e)
par(op)

##
boxplot(BlackHoleMass~Spiral, data = galaxies)


##
modelSD<- lm(Vdispersion ~ Spiral + Distance, data = galaxies)
summary(modelSD)


op=par(mfrow = c(1,2))

betahat <- modelSD$coeff
e <- modelSD$resid

plot(Vdispersion ~ Spiral + Distance, data = galaxies)

abline(betahat)

hist(e)
par(op)