library("lme4")
load("angel.Rdata")

head(angel)

# Estimate Model B:
modelB <- lm(Cooling ~ Vic_Number, data = angel)

# Set the margins for the plot:
op=par(mar = c(5,5,1,1))

# Create the plot:
plot(angel$Cooling ~ angel$Vic_Number, 
     xlab = "Number of victims", 
     ylab = "Cooling-off period (weeks)")

# Add the regression line:
abline(modelB$coeff, lwd = 2, col = "blue")

par(op)


# Estimate Model D:
modelD <- lmer(Cooling ~ Vic_Number + (1 | Killer), data = angel)

modelD@beta

randomeffects <- ranef(modelD)

u <- randomeffects$Killer$"(Intercept)"

u

#Store our estimates:
beta0 <- modelD@beta[1]
beta1 <- modelD@beta[2]

# Choose colours for each cluster:
colours <- c("black", "blue", "gray45", "red3", "darkgreen", "slateblue", "deepskyblue3",  "goldenrod4", "magenta3")

# Create labels for each cluster:
killers <- unique(paste(angel$Killer_ID, angel$Killer))

op=par(mar = c(5,5,1,11))

plot(angel$Cooling ~ angel$Vic_Number, 
     xlab = "Number of victims", 
     ylab = "Cooling-off period (weeks)", 
     pch = as.character(angel$Killer_ID), 
     col = colours[angel$Killer_ID])

legend(10, 50, legend = killers, text.col = colours, xpd = TRUE)

# Use a loop to add lines for all 7 clusters:

for(j in 1:7){
  abline(a = beta0 + u[j], b = beta1, col = colours[j], lwd = 2) 
}

par(op)


modelB <- lm(Cooling ~ 1 + Vic_Number, data = angel)

modelD <- lmer(Cooling ~ 1 + Vic_Number + (1 | Killer), 
               data = angel, REML = FALSE)
L1=logLik(modelB) 
L2=logLik(modelD) 

D= 2* (L2-L1)
D
qchisq(0.95, 1)
anova(modelD, modelB)

#Chi-Bar Test
p <- function(D){
  0.5*(1 - pchisq(D, 1) + 1 - pchisq(D, 2))
} 

curve(p, from = 0, to = 10, xlab = "D", ylab = "p")

grid()

abline(h = 0.05, col = "blue")

D <- seq(0, 10, by = 0.001)
c <-  min(D[p(D) < 0.05])

abline(v = c, col = "blue")

qchisq(0.95, 1)
c
l <- D[p(D) < 0.05]

