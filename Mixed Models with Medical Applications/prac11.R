library(lme4)

load("water.Rdata")

water$station <- as.factor(water$station)
water$pump <- as.factor(water$pump)

head(water)

Makueni <- water[water$station == "Makueni", ]
RM1 <- lmer(abstraction ~ days + (1 + days | pump), data = Makueni, REML = FALSE)

RM2 <- lmer(abstraction ~ days + (1 | pump) + (0 + days | pump), data = Makueni, REML = FALSE)

Makueni$months <- Makueni$days/30

RM3 <- lmer(abstraction ~ months + (1 | pump) + (0 + months | pump), data = Makueni, REML = FALSE)

RM4 <- lmer(abstraction ~ months + (1 + months | pump), data = Makueni, REML = FALSE)

colours <- c("black", "blue", "red3", "magenta3")

randomeffects <- ranef(RM4)

u0 <- randomeffects$pump$"(Intercept)"
u1 <- randomeffects$pump$"months"

beta0 <- RM4@beta[1]
beta1 <- RM4@beta[2]

plot(Makueni$months, Makueni$abstraction, col = colours[Makueni$pump], main = "Makueni", ylab = "abstraction", xlab = "time (months)", cex = 0.7, pch = 16)

for(i in 1:4){abline(a = beta0  + u0[i], b = beta1 + u1[i], col = colours[i], lwd = 2)}

legend(title = "pump", x = "topright", col = colours, legend = 1:4, lwd = 2)

Makueni$monthssquared <- Makueni$months^2

RM5 <- lmer(abstraction ~ months + monthssquared + (1 + months | pump) + (0 + monthssquared | pump), data = Makueni, REML = FALSE)
summary(RM5)

RM6 <- lmer(abstraction ~ months + monthssquared + (1 + months | pump), data = Makueni, REML = FALSE)

colours <- c("black", "blue", "red3", "magenta3")

randomeffects <- ranef(RM6)

u0 <- randomeffects$pump$"(Intercept)"
u1 <- randomeffects$pump$"months"

beta0 <- RM6@beta[1]
beta1 <- RM6@beta[2]
beta2 <- RM6@beta[3]

plot(Makueni$months, Makueni$abstraction, col = colours[Makueni$pump], main = "Makueni", ylab = "abstraction", xlab = "time (months)", cex = 0.7, pch = 16)

for(i in 1:4){
  
  curve(beta0+ u0[i] + (beta1 + u1[i])*x + (beta2)*x^2, 
        from = 0, to = 6, add = TRUE, 
        col = colours[i], lwd = 2)
} 

legend(title = "pump", x = "topright", col = colours, legend = 1:4, lwd = 2)


#EX1
Makueni$monthscube <- Makueni$months^3

RM7 <- lmer(abstraction ~ months + monthssquared + monthscube + (1 + monthssquared | pump) + (0 + monthscube | pump), data = Makueni, REML = FALSE)
summary(RM7)

RM8 <- lmer(abstraction ~ months + monthssquared + monthscube
           + (1 + months| pump)
            , data = Makueni, REML = FALSE)

summary(RM8)

colours <- c("black", "blue", "red3", "magenta3")

randomeffects <- ranef(RM8)
randomeffects$pump
u0 <- randomeffects$pump$"(Intercept)"
u1 <- randomeffects$pump$"months"
RM8@beta
beta0 <- RM8@beta[1]
beta1 <- RM8@beta[2]
beta2 <- RM8@beta[3]
beta3 <- RM8@beta[4]
plot(Makueni$months, Makueni$abstraction, col = colours[Makueni$pump], 
     main = "Makueni (Cubic)", ylab = "abstraction", xlab = "time (months)", cex = 0.7, pch = 16)

for(i in 1:4){
  
  curve(beta0+ u0[i] + (beta1 + u1[i])*x + (beta2)*x^2 +(beta3)*x^3, 
        from = 0, to = 6, add = TRUE, 
        col = colours[i], lwd = 2)
} 

legend(title = "pump", x = "topright", col = colours, legend = 1:4, lwd = 2)



#Ex2

head(water)
water$months <- water$days/30
water$monthssquared <- water$months^2
RM9 <- lmer(abstraction ~ months + monthssquared +rain + (0 + months | pump)+ (1|pump) 
            + (1|station), data = water, REML = FALSE)
summary(RM9)

randomeffects <- ranef(RM9)
randomeffects
randomeffects$pump
u0 <- randomeffects$station$"(Intercept)"
u1 <- randomeffects$pump$"months"
u0
RM9@beta
beta0 <- RM9@beta[1]
beta1 <- RM9@beta[2]
beta2 <- RM9@beta[3]
beta3 <- RM9@beta[4]
colours <- palette.colors(n = 12, palette = "Polychrome36", recycle = FALSE)
plot(water$months, water$abstraction, col = colours[water$pump], 
     main = "Water", ylab = "abstraction", xlab = "time (months)", cex = 0.7, pch = 16)

for(i in 1:4){
  
  curve(beta0+ u0[i] + (beta1 + u1[i])*x + (beta3)*x^2 , 
        from = 0, to = 12, add = TRUE, 
        col = colours[i], lwd = 2)
} 

legend(title = "pump", x = "topright", col = colours, legend = 1:12, lwd = 2)


