library(lme4)
load("testing.Rdata")

head(testing)
testing$hospital <- as.factor(testing$hospital)

ModelA <- lm(cost ~ ageover60, data = testing)

ModelA

ModelA$coefficients
plot(cost ~ ageover60, data = testing)

abline(a = ModelA$coefficients[1], b = ModelA$coefficients[2])

colours <- c("blue", "red3", "goldenrod4", "darkgreen","magenta3", "darkorchid4")

plot(cost ~ ageover60, data = testing, col = colours[hospital], pch = as.character(hospital))

# Estimate using lmer():
#Random Intercept Model
ModelB <- lmer(cost ~ ageover60 + (1 | hospital), data = testing, REML = FALSE)

ModelB@beta

beta0 <- ModelB@beta[1]
beta1 <- ModelB@beta[2]


randomeffects <- ranef(ModelB)

u <- randomeffects$hospital$"(Intercept)"

u

#EX1

hist(u,breaks=3)

qqnorm(u)
qqline(u, col = "steelblue", lwd = 2)

#EX2

plot(cost ~ ageover60, data = testing, 
     col = colours[hospital],  
     pch = as.character(hospital))

for(j in 1:6){ 
  abline(a = beta0 + u[j], b = beta1, col = colours[j]) 
}

legend(title = "hospital", x = "topright", col = colours, legend = 1:6, lty = 1, bg = "white")


ModelC <- lmer(cost ~ ageover60 + misdiagnosed + (1 | hospital), data = testing, REML = FALSE)
ModelC@beta
randomeffects <- ranef(ModelC)
beta0 <- ModelC@beta[1]
beta1 <- ModelC@beta[2]
u <- randomeffects$hospital$"(Intercept)"
u

ModelBC <- lmer(cost ~ ageover60 + (1 | hospital)+ (1| misdiagnosed), data = testing, REML = FALSE)

ModelBC@beta

plot(cost ~ ageover60, data = testing, 
     col = colours[hospital],  
     pch = as.character(hospital))

for(j in 1:6){ 
  abline(a = beta0 + u[j], b = beta1, col = colours[j]) 
}

beta0 <- ModelBC@beta[1]
beta1 <- ModelBC@beta[2]
randomeffects <- ranef(ModelBC)

u <- randomeffects$hospital$"(Intercept)"
u
for(j in 1:6){ 
  abline(a = beta0 + u[j], b = beta1, col = "black") 
}

legend(title = "hospital", x = "topright", col = colours, legend = 1:6, lty = 1, bg = "white")

