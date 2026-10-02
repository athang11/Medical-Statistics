library(lme4)
library(dplyr)
library(resample)
# load("cancer.Rdata")
# 
# makesample(2928)
# 
# save(CWsample, file = "CWsample.RData")

load("CWsample.RData")

head(CWsample)
table(CWsample$SiteID)
names(CWsample)

CWsample$KidneyOrLung <- (CWsample$Type == "Kidney") | (CWsample$Type == "Lung") 


# summarise(CWsample, c = count(CWsample,SiteID))

CWsample %>%
  group_by(SiteID) %>%
  summarise(c = n())
op=par( mfrow= c(2,1))
hist(CWsample$AgeDiagnosis, xlab= 'Patient Age at Diagnosis (in years)',
     main= "Histograms of Patient's Age at Diagnosis", xlim=c(30,80))
abline(v=mean(CWsample$AgeDiagnosis), col="red3")

hist(CWsample$AgeDeath, xlab= 'Patient Age at Death (in years)',
     main= "Histograms of Patient's Age at Death", xlim=c(30,80))
abline(v=mean(CWsample$AgeDeath), col="red3")
par(op)
graphics.off()
plot(CWsample$AgeDiagnosis, CWsample$AgeDeath, col='Blue',ylab='Patient Age at Death (in years)',
     xlab='Patient Age at Diagnosis (in years)', main='Diagnosis vs Death', xlim=c(30,80),ylim=c(30,80) )
abline(h=mean(CWsample$AgeDeath), col="red3")
abline(v=mean(CWsample$AgeDiagnosis), col="red3")




#STEP 2
model1 <- lm(AgeDeath ~ AgeDiagnosis , data = CWsample)
summary(model1)

betahat <- model1$coeff
e <- model1$resid
betahat
plot(AgeDeath ~ AgeDiagnosis , data = CWsample)
abline(betahat)
hist(e)
qqnorm(e)
qqline(e)
#STEP 3
#colVars(CWsample)
CWsample$KidneyOrLung <- (CWsample$Type == "Kidney") | (CWsample$Type == "Lung") 
model2 <- lm(AgeDeath ~ AgeDiagnosis + Metastases , data = CWsample)
summary(model2)
# model2 <- lm(AgeDeath ~ AgeDiagnosis +  , data = CWsample)
# summary(model2)
betahat <- model2$coeff
e <- model2$resid
mean(e)
sd(e)
betahat
plot(AgeDeath ~ AgeDiagnosis , data = CWsample)
abline(a=betahat[1],b=betahat[2] )
#CWsample[["Metastases"]] <- encode_ordinal(CWsample[["Metastases"]])

model21 <- lmer(AgeDeath ~ AgeDiagnosis + (1|Metastases) , data = CWsample)
summary(model21)

model21@beta

beta0 <- model21@beta[1]
beta1 <- model21@beta[2]
randomeffects <- ranef(model21)

u <- randomeffects$Metastases$"(Intercept)"

u

colours <-c("blue", "red3")
plot(AgeDeath ~ AgeDiagnosis, 
     col = colours[1:2],  
     pch = as.character(Metastases), data = CWsample)

for(j in 1:2){ 
  abline(a = beta0 + u[j], b = beta1, col = colours[j]) 
}

legend(title = "metastases", x = "bottomright", col = colours, legend = unique(CWsample$Metastases), lty = 1, bg = "white")
#STEP 4
#Clustering by Site
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
CWsample[["SiteID1"]] <- encode_ordinal(CWsample[["SiteID"]])
colours <- palette.colors(n = 12, palette = "Polychrome36", recycle = FALSE)
#c("blue", "red3", "goldenrod4", "darkgreen","magenta3", "darkorchid4",)
model3 <- lmer(AgeDeath ~ AgeDiagnosis + (1| SiteID1) ,data = CWsample, REML = FALSE)
summary(model3)
model3@beta

beta0 <- model3@beta[1]
beta1 <- model3@beta[2]

randomeffects <- ranef(model3)
#graphics.off()
u <- randomeffects$SiteID$"(Intercept)"

u

plot(AgeDeath ~ AgeDiagnosis, 
     col = colours[1:12],  
     pch = as.character(SiteID1), data = CWsample)

for(j in 1:12){ 
  abline(a = beta0 + u[j], b = beta1, col = colours[j]) 
}

legend(title = "sites", x = "topright", col = colours, legend = unique(CWsample$SiteID1), lty = 1, bg = "white")

# Extract variance components
variance_components <- as.data.frame(VarCorr(model3))
variance_components
# Calculate ICC
between_group_variance <- variance_components$vcov[1]  # Assuming the first row corresponds to the random intercept
between_group_variance
residual_variance <- summary(model3)$sigma^2  # Residual variance
residual_variance
ICC <- between_group_variance / (between_group_variance + residual_variance)
ICC

#plot(AgeDeath ~ AgeDiagnosis,  data = CWsample)

op=par( mfrow= c(1,2))
e <- resid(model3)
qqnorm(e,  main='QQ Plot of Residuals')
qqline(e, col='red3')

random_intercepts <- ranef(model3)$SiteID1[,1]

qqnorm(random_intercepts,  main='QQ Plot of Random Intercepts')
qqline(random_intercepts, col='red3')
par(op)

residuals <- residuals(model3)
fitted_values <- fitted(model3)

# Create a scatter plot of residuals vs. fitted values
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values Plot")
abline(h = 0, col = "red") 
#STEP5
model4<- lmer(AgeDeath ~ AgeDiagnosis + Metastases+ (1| SiteID1) , data = CWsample, REML=F)
summary(model4)


beta0 <- model4@beta[1]
beta1 <- model4@beta[2]

randomeffects <- ranef(model4)
#graphics.off()
u <- randomeffects$SiteID$"(Intercept)"

u
plot(AgeDeath ~ AgeDiagnosis, 
     col = colours[1:12],  
     pch = as.character(SiteID1), data = CWsample)

for(j in 1:12){ 
  abline(a = beta0 + u[j], b = beta1, col = colours[j]) 
}

legend(title = "sites", x = "topright", col = colours, legend = unique(CWsample$SiteID1), lty = 1, bg = "white")

anova(model4,model2)
p =  0.5 * (1 - pchisq(7.3842, df = 1))
p


#LVL5
alpha <- 0.025  # Type I error rate
beta <- 0.2     # Type II error rate
delta <- 2.5   
sigma <- sd(CWsample$AgeDeath)     # Example value, replace with your estimate


# Calculate sample size
n <- 2*sigma^2 * (qnorm(1 - alpha) - qnorm(beta))^2 / delta^2

# Print the result
cat("Required sample size:", ceiling(n), "\n")

#power.t.test(n=NULL, delta=2.5,sd=sd(CWsample$AgeDeath),power=0.8)
calc_pow <- function(n, delta, sd)
{
  # Get the critical value used in the test
  crit <- qnorm(1 - 0.05/2, mean = 0, sd = 1)
  # Calculate the power
  1 - pnorm(crit - delta/sqrt(2*sd^2/n), mean = 0, sd = 1)
  # Note - an R function will return the result of the last executed line
}

#calc_pow(n=9618, delta= -1:4, sd=sd(CWsample$AgeDeath))

curve(calc_pow(n=1000, delta= x, sd=sd(CWsample$AgeDeath)), from= -1, to=4,
      xlab='Treatment Effect',ylab= 'Power',col='Blue',panel.first = grid(NULL,NULL, lty = 1),
      main=expression('Power as a function of Treatment Effect' ~(delta)))
curve(calc_pow(n=200, delta= x, sd=sd(CWsample$AgeDeath)), from= -1, to=4,
      xlab='Treatment Effect',ylab= 'Power',col='Red3', add=TRUE)
#abline(v=2.5,lty='dotted')
legend(title = "Sample Size(n)", x = "bottomright", col = c('Blue','Red3'), legend = c(1000,200), lty = 1, bg = "white")
# power.t.test(n = NULL, delta = 2.5, sd = sd(CWsample$AgeDeath), power = 1)#
####
curve(calc_pow(n=x, delta= 2.5, sd=sd(CWsample$AgeDeath)), from= 100, to=1000,
      xlab='Sample Size',ylab= 'Power',col='Blue',panel.first = grid(NULL,NULL, lty = 1),
      main='Power as a function of Sample Size (n)')
#abline(v=2.5,lty='dotted')

