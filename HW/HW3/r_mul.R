dta <- read.csv("df_lr.csv", header = TRUE)

# Before subsetting with Income
summary(lm(satisfaction~income,data=dta))
summary(m2 <- lm(controllability~income,data=dta))
summary(lm(satisfaction~controllability,data=dta))
summary(m4 <- lm(satisfaction~income+controllability,data=dta))
res_f <- lapply(list(m2, m4), summary)
a <- c(Est = res_f[[1]]$coef['income', 'Estimate'], 
       SE = res_f[[1]]$coef['income', 'Std. Error'])
b <- c(Est = res_f[[2]]$coef['controllability', 'Estimate'], 
       SE = res_f[[2]]$coef['controllability', 'Std. Error'])
ab <- a['Est'] * b['Est']
abse <- sqrt(a['Est']^2 * b['SE']^2 + b['Est']^2 * a['SE']^2)
c(ab, z_ab = ab/abse, pz_ab = 2 * (1 - pnorm(abs(ab/abse))))

# Spline Regression
dta$gap = ifelse(dta$income>5,1,0)
dta$diff = dta$income - 5
dta$cp_income = dta$gap*dta$diff
dta$cp_controllability = dta$gap*dta$controllability

m1 <- lm(satisfaction ~ income+cp_income+controllability+cp_controllability, data = dta)
m2 <- update(m1, . ~ .+income:controllability+cp_income:cp_controllability)
summary(m1)
summary(m2)

summary(lm(satisfaction~income+cp_income,data=dta))
summary(m2 <- lm(controllability~income+cp_income,data=dta))
summary(lm(satisfaction~controllability+cp_controllability,data=dta))
summary(m4 <- lm(satisfaction~income+controllability+cp_income+cp_controllability,data=dta))

res_f <- lapply(list(m2, m4), summary)
a <- c(Est = res_f[[1]]$coef['income', 'Estimate'], 
       SE = res_f[[1]]$coef['income', 'Std. Error'])
b <- c(Est = res_f[[2]]$coef['controllability', 'Estimate'], 
       SE = res_f[[2]]$coef['controllability', 'Std. Error'])
ab <- a['Est'] * b['Est']
abse <- sqrt(a['Est']^2 * b['SE']^2 + b['Est']^2 * a['SE']^2)
c(ab, z_ab = ab/abse, pz_ab = 2 * (1 - pnorm(abs(ab/abse))))

a <- c(Est = res_f[[1]]$coef['cp_income', 'Estimate'], 
       SE = res_f[[1]]$coef['cp_income', 'Std. Error'])
b <- c(Est = res_f[[2]]$coef['cp_controllability', 'Estimate'], 
       SE = res_f[[2]]$coef['cp_controllability', 'Std. Error'])
ab <- a['Est'] * b['Est']
abse <- sqrt(a['Est']^2 * b['SE']^2 + b['Est']^2 * a['SE']^2)
c(ab, z_ab = ab/abse, pz_ab = 2 * (1 - pnorm(abs(ab/abse))))

library(pequod)
summary(rst <- lmres(satisfaction ~ income+cp_income+controllability+cp_controllability+income*controllability+cp_income*cp_controllability, data = dta))
summary(sl<-simpleSlope(rst,pred="income",mod1="controllability"))
PlotSlope(sl)

summary(s2<-simpleSlope(rst,pred="cp_income",mod1="cp_controllability"))
PlotSlope(s2)
# Visualization
plot(satisfaction~income,dta)
lines(dta$income,predict(reg))

# Subsetting
dta_h <- subset(dta, income >= 6)
dta_l <- subset(dta, income < 6)

# High Income
summary(lm(satisfaction~income,data=dta_h))
summary(m2 <- lm(controllability~income,data=dta_h))
summary(lm(satisfaction~controllability,data=dta_h))
summary(m4 <- lm(satisfaction~income+controllability,data=dta_h))
res_f <- lapply(list(m2, m4), summary)
a <- c(Est = res_f[[1]]$coef['income', 'Estimate'], 
       SE = res_f[[1]]$coef['income', 'Std. Error'])
b <- c(Est = res_f[[2]]$coef['controllability', 'Estimate'], 
       SE = res_f[[2]]$coef['controllability', 'Std. Error'])
ab <- a['Est'] * b['Est']
abse <- sqrt(a['Est']^2 * b['SE']^2 + b['Est']^2 * a['SE']^2)
c(ab, z_ab = ab/abse, pz_ab = 2 * (1 - pnorm(abs(ab/abse))))

# Low Income
summary(lm(satisfaction~income,data=dta_l))
summary(m2 <- lm(controllability~income,data=dta_l))
summary(lm(satisfaction~controllability,data=dta_l))
summary(m4 <- lm(satisfaction~income+controllability,data=dta_l))
res_f <- lapply(list(m2, m4), summary)
a <- c(Est = res_f[[1]]$coef['income', 'Estimate'], 
       SE = res_f[[1]]$coef['income', 'Std. Error'])
b <- c(Est = res_f[[2]]$coef['controllability', 'Estimate'], 
       SE = res_f[[2]]$coef['controllability', 'Std. Error'])
ab <- a['Est'] * b['Est']
abse <- sqrt(a['Est']^2 * b['SE']^2 + b['Est']^2 * a['SE']^2)
c(ab, z_ab = ab/abse, pz_ab = 2 * (1 - pnorm(abs(ab/abse))))

# Moderation
dta$altruism <- 6-dta$altruism
dta$individuality <- 4-dta$individuality
m1 <- lm(altruism ~ age+individuality+edu, data = dta)
m2 <- update(m1, . ~ .+edu:individuality)
summary(m1)
summary(m2)
anova(m1,m2)
library(pequod)
summary(rst <- lmres(altruism~age+individuality+edu+edu*individuality, data=dta))
summary(sl<-simpleSlope(rst,pred="individuality",mod1="edu"))
PlotSlope(sl)
library(interactions)
johnson_neyman(model = m2, pred = "individuality", modx = "edu")


summary(lm(altruism~edu,data=dta))
summary(m2 <- lm(individuality~edu,data=dta))
summary(m4 <- lm(altruism~edu+individuality,data=dta))
res_f <- lapply(list(m2, m4), summary)
a <- c(Est = res_f[[1]]$coef['edu', 'Estimate'], 
       SE = res_f[[1]]$coef['edu', 'Std. Error'])
b <- c(Est = res_f[[2]]$coef['individuality', 'Estimate'], 
       SE = res_f[[2]]$coef['individuality', 'Std. Error'])
ab <- a['Est'] * b['Est']
abse <- sqrt(a['Est']^2 * b['SE']^2 + b['Est']^2 * a['SE']^2)
c(ab, z_ab = ab/abse, pz_ab = 2 * (1 - pnorm(abs(ab/abse))))
