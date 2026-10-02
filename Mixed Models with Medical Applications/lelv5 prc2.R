df <- readRDS("ROLARR_sim.rds")

head(df)


#EX2
fit <- readRDS("p2_fit.rds")
plot(fit)

summary(fit, priors=TRUE)

fit_lmer <- lmer(y ~ trt + u + (1 | u), REML = F, 
                 data = df)

confint(fit_lmer, method = "Wald", level = 0.87)


fit_b <- readRDS("p2_fit_b.rds")
plot(fit_b)
