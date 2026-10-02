#EX1
I <- diag(3)
J <- rep(1, 9)
dim(J) <- c(3,3)
#EX2
KP= I %x% J

Sigma= 4 * diag(9) + 16* KP
Sigma

#EX3

beta <- c(40, 60)

x <- c(-0.5,  0.8, -0.1,  1.8, 0.7,  0.0,  0.2, -1.3,  0.7)
intercept <- rep(1, 9)

X <- cbind(intercept, x)

X

E.Y= X %*% beta
E.Y

#EX4
library("MASS")

y <- mvrnorm(n = 1, mu = X %*% beta, Sigma = Sigma)

#EX5
I <- diag(3) #no of clusters
J <- rep(1, 100*100)
dim(J) <- c(100,100)

KP= I %x% J

Sigma= 4 * diag(300) + 16* KP
Sigma


beta <- c(40, 60)

x <- runif(300, -1, 1) 
intercept <- rep(1, 300)

X <- cbind(intercept, x)

X

y <- mvrnorm(n = 1, mu = X %*% beta, Sigma = Sigma)
qqnorm(y)
#EX6
library(lme4)
group <- sort(rep(1:3, 100))
lmer(y~x + (1|group),REML = FALSE)


