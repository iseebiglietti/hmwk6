# Libraries
library(tidyverse)
library(knitr)
library(stargazer)
library(haven)
library(descr)
library(broom)
library(AER)
library(wooldridge)
library(plm)

# Imports
data(wagepan)


##################
### Question a ###
##################

# Estimate the regression model using pooled OLS
pooled_model <- lm(lwage ~ union + exper + expersq + married + educ + black + hisp + factor(year), data = wagepan)

# Print the summary of the model
summary(pooled_model)
stargazer(pooled_model, type = "latex", out = "OUTPUT/table_pooled.tex")

# I find a positive statistically significant effect, wich is rather logical 
#(well I least this is what I wish for the effect of a union presence in a firm: an increase in wages, and an increase in employees rights)

# clustered standard errors : 
robust_pm <- plm(lwage ~ union + exper + expersq + married + educ + black + hisp + factor(year), data = wagepan, model = "pooling")
summary(robust_pm)
robust_HC1 <- coeftest(robust_pm, vcov = vcovHC(robust_pm, type = "HC1"))
robust_sss <- coeftest(robust_pm, vcov = vcovHC(robust_pm, type = "sss"))
# ça marche pas le dernier jsp pq 

stargazer(robust_pm, type = "latex", out = "OUTPUT/table_robust.tex")
stargazer(robust_HC1, type = "latex", out = "OUTPUT/table_robustHC1.tex")
stargazer(robust_sss, type = "latex", out = "OUTPUT/table_robustsss.tex")

# ça change rien ! fin si le résultat ets mtn moins précis, on a une sd qui augmente. 
# results with pooled_model: 0.017157
# results with robust pooled_model: 0.0274214 pour HC1 et 0.0274435 pour sss


##################
### Question b ###
##################

ols_re <- plm(lwage ~ union + exper + expersq + married + educ + black + hisp + factor(year), data = wagepan, model = "random", index = c("nr","year"))
ols_resss <- coeftest(ols_re, vcov = vcovHC(ols_re,type="sss"))
stargazer(ols_resss, type = "latex", out = "OUTPUT/table_RE.tex")

# ça réduit le point estimate de 0.1824613 à 0.10613443, toujours positive effect and significative one, but smaller one, 
# and our result is more precise this time as it's diminishing the SD to 0.02084397.


##################
### Question c ###
##################

ols_fe <- plm(lwage ~ union + exper + expersq + married + educ + black + hisp + factor(year), data = wagepan, model = "within", index = c("nr","year"))
ols_fesss <- coeftest(ols_fe, vcov = vcovHC(ols_fe,type="sss"))
stargazer(ols_fesss, type = "latex", out = "OUTPUT/table_FE.tex")

# on passe a un point estimate 0.08000186, avec une sd qui augmente à nouveau par rapport à b : 0.02274049


##################
### Question d ###
##################

# ça a pas drop exper, par contre ça à drop le reste. 
# In the case of the variable exper being dropped from the fixed effect regression, even though it is time-varying, 
#it suggests that it is perfectly collinear with the fixed effects, which are usually included to capture individual-specific effects. 
#This could happen if the variable exper varies only within individuals over time, but not across individuals.



##################
### Question e ###
##################

ols_fd <- plm(lwage ~ union + exper + expersq + married + educ + black + hisp + factor(year), data = wagepan, model = "fd", index = c("nr","year"))
ols_fdsss <- coeftest(ols_fd, vcov = vcovHC(ols_fd,type="sss"))
stargazer(ols_fdsss, type = "latex", out = "OUTPUT/table_FD.tex")

# point estimate : 0.041
# sd : 0.022
# même précision mais notre effet est encore plus minable franchement...
# de moins en moins significatif


##################
### Question f ###
##################

# While first differences help control for time-invariant unobserved heterogeneity, they may not fully address simultaneity bias if 
#there are dynamic effects, measurement error, omitted variables, or reverse causality present in the model.

# Create first-difference variables
wagepan$c_exper <- with(wagepan, exper - lag(exper))
wagepan$c_expersq <- with(wagepan, expersq - lag(expersq))
wagepan$c_married <- with(wagepan, married - lag(married))
wagepan$c_educ <- with(wagepan, educ - lag(educ))
wagepan$c_black <- with(wagepan, black - lag(black))
wagepan$c_hisp <- with(wagepan, hisp - lag(hisp))

# compute the IV (lagged) model
iv_model <- plm(lwage ~ lag(union) + c_exper + c_expersq + c_married + c_educ + c_black + c_hisp + factor(year), data = wagepan, model = "pooling")
summary(iv_model)
iv_sss <- coeftest(iv_model, vcov = vcovHC(iv_model, type = "sss"))
stargazer(iv_sss, type = "latex", out = "OUTPUT/table_IV.tex")
# point estimate : 0.158912  // sd : 0.030174 
# plus précis et plus d'effet, que demande le peuple ? y'avait bien encore un simultaneity bias en question (e)...


# C PAS BON ALED CA DROP PRESQUE TT IL FAUT MODIFIER

##################
### Question g ###
##################

# Create our explanatory variable
wagepan$diff_union <- with(wagepan, lag(union) - lag(lag(union)))


# compute the IV (lagged) model n°2
iv_model_2 <- plm(lwage ~ diff_union + c_exper + c_expersq + c_married + c_educ + c_black + c_hisp + factor(year), data = wagepan, model = "pooling")
summary(iv_model_2)
iv_2_sss <- coeftest(iv_model_2, vcov = vcovHC(iv_model, type = "sss"))
stargazer(iv_2_sss, type = "latex", out = "OUTPUT/table_IV2.tex")
# point estimate : rien  // sd : rien 

# C PAS BON ALED CA DROP TT MÊME DIFF_UNION LUI MÊME --> IL FAUT MODIFIER