### Add packages we use in Assignment 2

library("plyr", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")
library("nlme", lib.loc="~/R/win-library/3.5")
library("car", lib.loc="~/R/win-library/3.5")
library("hexbin", lib.loc="~/R/win-library/3.5")
library("xtable", lib.loc="~/R/win-library/3.5")
library("knitr", lib.loc="~/R/win-library/3.5")
library("multcomp", lib.loc="~/R/win-library/3.5")
library("lmerTest", lib.loc="~/R/win-library/3.5")
library("foreign", lib.loc="~/R/win-library/3.5")


############################################################################################################################################################

#### 2. Testing for the random intercept

## Import the Classrrom data into R.

Classroom <- read.csv(file = 'Classroom.csv')

## Factor the Classid variable

Classroom <- mutate(Classroom, Classid.f = factor(Classid))
View(Classroom)

## Testing for the random intercept using the REML based likelihood ratio test

Model1 <- lme(MG ~ YE + MK + HP + YE:MK + YE:HP + MK:HP, random = ~ 1 | Classid.f , method = "REML", data = Classroom)

Model2 <- gls(MG ~ YE + MK + HP + YE:MK + YE:HP + MK:HP, method = "REML", data = Classroom)

LRT <- anova(Model1, Model2)$L.Ratio[-1] # Obtain LRT = 2ln(LRR) - 2ln(LNR)

## Calculating the p-value for the REML based likelihood ratio test

p_value <- (0.5 * pchisq(LRT, df = 1, lower.tail = FALSE)) + (0.5 * pchisq(LRT, df = 0, lower.tail = FALSE))
p_value


############################################################################################################################################################

#### 3. Variance-covariance estimates of the final linear mixed model

## D matrix

D_hat <- getVarCov(Model1)[-3,] # Gives the D matrix
D_hat

## R matrix

R_hat <- getVarCov(Model1, type = "conditional", individuals = 3)[[1]]
R_hat

## Var(Y)

Var_Y <- getVarCov(Model1, type = "marginal", individuals = 3)[[1]]
Var_Y

######################################################################################################################################################

#### 4. Predicted values and residuals of the final linear mixed model

## Table 1: The estimates of the fixed effects in model 1.

Table_1 <- summary(Model1)$tTable
round(Table_1, 4)

