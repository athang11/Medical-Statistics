
library("MASS")
sigma_e <- 1
sigma_u <- 2
beta <- c(2, -1)


# Set a seed so that our random results can be reproduced:
set.seed(1212)

# Set the sample size:
n <- 90

# Generate some covariate values from x ~ N(0, 4):
x <- rnorm(n = n, mean = 0, sd = 2) 

# A vector of 90 ones, the first column in our design matrix:
intercept <- rep(1, n)

# Design matrix:
X <- cbind(intercept, x)
# Create the necessary matrices:

I90 <- diag(90)
I9 <- diag(9)
J10 <- rep(1, 10^2)
dim(J10) <- c(10, 10)

# Covariance:
Sigma <- (sigma_e^2) * I90 + (sigma_u^2) * I9 %x% J10

# Expectation:
Xbeta <- X %*% beta  


y <- mvrnorm(n = 1, mu = Xbeta, Sigma = Sigma)

Z0 <- I90 

dim(Z0) <- c(n^2, 1)

Z1 <- I9 %x% J10

dim(Z1) <- c(n^2, 1)

Z <- cbind(Z0, Z1)


#EX1¬=--
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat


Sigma_hat <- diag(n)

y_estim <- y - X  %*% beta_hat
y_estim
#EX3
residual<-y_estim  %*%  t(y_estim)


residual <- c(n^2,1)

#EX4
Omega_inv= Sigma_hat  %x% Sigma_hat

Sigma_hat <- solve(t(Z) %*% Omega_inv %*% Z) %*% t(Z) %*% Omega_inv %*% as.vector(y_estim  %*%  t(y_estim))


#EX5


