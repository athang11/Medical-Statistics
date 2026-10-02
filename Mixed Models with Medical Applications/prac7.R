load("exam.Rdata")
library('brms')
exam[1:6, 1:8]

exam$y <- exam$Year2exam - exam$Year1exam 

exam$x <- exam$Tutorial1 + exam$Tutorial2 + exam$Tutorial3 + exam$Tutorial4 

beta0prior <- set_prior("normal(-10, 4)", class = "Intercept")
beta1prior <- set_prior("normal(0, 20)", class = "b", coef = "x")

beta0prior
beta1prior

set.seed = (999)

model1 <- brm(y ~ x + (1 | Group) + (0 + x | Group), 
              data = exam,
              prior = c(beta0prior, beta1prior),
              init = "random",
              chains = 2,
              warmup = 500,
              iter = 1000,
              file = "model1results")

#fit <- readRDS("model1results.rds")

plot(model1)

summary(model1)



library('lme4')
Modellme <- lmer(y ~ x + (1 | Group) + (0 + x | Group), 
                 data = exam, REML = FALSE)
Modellme
