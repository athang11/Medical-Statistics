library("lme4")
load("C:/Athang/MSc DSA/Mixed Models with Medical Applications/exam.Rdata")

exam[1:6, 1:7]
#EX1
table(exam$Group)

par(mfrow = c(1,2))

qqnorm(exam$Year2exam)
boxplot(exam$Year2exam ~ exam$Group)

par(mfrow = c(4,2))

for(i in 1:8){
  qqnorm(exam$Year2exam[exam$Group == i])
}

par(mfrow = c(2,2))

boxplot(exam$Year2exam ~ exam$Tutorial1)
boxplot(exam$Year2exam ~ exam$Tutorial2)
boxplot(exam$Year2exam ~ exam$Tutorial3)
boxplot(exam$Year2exam ~ exam$Tutorial4)

exam$Group <- as.factor(exam$Group)

model <- lmer(Year2exam ~ Tutorial1 + Tutorial2 + Tutorial3 + Tutorial4 + (1 | Group), data = exam, REML = FALSE)

sigmahat_u0 <- 7.156   
sigmahat_e0 <- 20.318  

# Store the beta estimates as a 5x1 matrix:
betahat <- matrix(model@beta, nrow = 5, ncol = 1) 

n <- nrow(exam)  # Sample size.

intercept <- rep(1, n)  # Column of ones.

covariates <- exam[, c("Tutorial1", "Tutorial2", "Tutorial3", "Tutorial4")]

X <- cbind(intercept, covariates)  # Design matrix.

X <- as.matrix(X)  # Makes sure R treats X as a matrix
# not as a data frame.
# Identity matrix:

I <- diag(n)

# Set up an empty n by n matrix:

G <- matrix(NA, nrow = n, ncol = n)

# Fill in the elements of G:

for(h in 1:n){
  for(k in 1:n){
    
    G[h, k] <-  as.numeric(exam$Group[h] == exam$Group[k]) 
  }  
}

Sigmahat <- (sigmahat_e0^2)*I + (sigmahat_u0^2)*G 

C <- c(0, 0, 1, 0, 0)

C <- matrix(C, ncol = 5, nrow = 1) # Make sure this is a row vector.

#EX2
alpha <- 0.1  # significance level
critical_value <- qnorm(1 - alpha/2)  

confidence_int1 <- betahat[2] - (critical_value * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) 
                                                         %*%t(C)))
confidence_int2 <- betahat[2] + (critical_value * sqrt(C %*% solve(t(X) %*% solve(Sigmahat) %*% X) 
                                                         %*%t(C)))
cat(confidence_int1, confidence_int2)

#Since 0 is in confidence interval, failed to reject null hypothesis at 10% level



#EX3
confint(model, method = "Wald", level = 0.9)


#EX4

q <- 4  # degrees of freedom

d <- sqrt(qchisq(1 - 0.1, df = q))  # square root of 90% quantile

C <- matrix(data = 0, nrow = 4, ncol = 5) # matrix of zeros

# Plug in some ones:

C[1, 2] <- 1
C[2, 3] <- 1
C[3, 4] <- 1
C[4, 5] <- 1

C

k = matrix(c(0,0,0,0), ncol = 1, nrow = 4)

k

V <- C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C)

# Mahalanobis distance between estimate and proposed k:

D = sqrt( t(C%*%betahat - k) %*% solve(V) %*% (C%*%betahat - k) )

D

d  

# First 2 tutorials have no effect
# Hypothesis- H0: b1=b2=0

k = matrix(c(0,0,1,1), ncol = 1, nrow = 4)

k

V <- C %*% solve(t(X) %*% solve(Sigmahat) %*% X) %*% t(C)

# Mahalanobis distance between estimate and proposed k:

D = sqrt( t(C%*%betahat - k) %*% solve(V) %*% (C%*%betahat - k) )
D
d

