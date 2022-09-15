
dta <- read.csv('df_for_r_hlm.csv',header=T, fileEncoding = 'UTF-8-BOM')

library(lme4)
library(lmerTest)
library(MuMIn)

result0 <- lmer(math ~ 1+(1|school),data = dta)
summary(result0)
anova(result0)
tab_model(result0)

result <- lmer(math ~ homework
               
               +(1+homework|school),data = dta)
summary(result)

library(sjPlot)
library(sjmisc)
library(ggplot2)

anova(result)
tab_model(result)

# ICC = Interclass Variance / Total Variance

write.csv(round(coef(summary(result)),3),'hlm1_table.csv')