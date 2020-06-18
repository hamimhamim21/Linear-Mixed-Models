### Add packages we use in Assignment 1

library("nlme", lib.loc="~/R/win-library/3.5")
library("foreign", lib.loc="~/R/win-library/3.5")
library("plyr", lib.loc="~/R/win-library/3.5")
library("tidyverse", lib.loc="~/R/win-library/3.5")
library("car", lib.loc="~/R/win-library/3.5")
library("hexbin", lib.loc="~/R/win-library/3.5")
library("xtable", lib.loc="~/R/win-library/3.5")
library("knitr", lib.loc="~/R/win-library/3.5")
library("multcomp", lib.loc="~/R/win-library/3.5")
library("lmerTest", lib.loc="~/R/win-library/3.5")


###################################################################################################################################################################################

### 1. Graphical analysis

## Import the HourlyWage.csv file into R.

HourlyWage <- read.csv(file = "HourlyWage.csv")
View(HourlyWage)

## A numerical and graphical summary for the HourlyWage data that provides an indication for whether the interaction effects of Year * Race
## should be included in our linear mixed model.

# Table 1: Summary of HW grouped by Year and Race.

Table_1 <- ddply(HourlyWage, c("Race", "Year"), summarise, n = length(HW), HW_mean = mean(HW), HW_stdev = sd(HW))
Table_1

# Figure 1: Plot of the values of HW vs Year grouped by Race.

ggplot(data = Table_1, aes(x = Year, y = HW_mean)) +
  geom_point(aes(colour = Race), size = 4, shape = 15) +
  geom_line(aes(colour = Race)) +
  scale_x_continuous(breaks=seq(1980, 1987, 1)) +
  ylim(4, 8)

## A graphical summary for the HourlyWage data that provides an indication for whether random effects specific to
## subjects should be included in our linear mixed model.

## Factor the variable Subject and add this variable to the data frame.

HourlyWage <- mutate(HourlyWage, Subject.f = factor(Subject))
View(HourlyWage)

# Figure 2: Plot of the values of HW vs Year for the first five subjects.

ggplot(data = HourlyWage[1:48,], aes(x = Year, y = HW)) +
  geom_point(aes(colour = Subject.f), size = 4) +
  geom_line(aes(colour = Subject.f)) +
  scale_x_continuous(breaks=seq(1980, 1987, 1)) +
  scale_y_continuous(breaks=seq(0, 10, 2))

## Compute the variable Y = Year - 1980

HourlyWage <- mutate(HourlyWage, Y = Year - 1980)
View(HourlyWage)


####################################################################################################################################################################

#### 3. Testing for random effects

### Testing the null hypothesis that the random effect of Y is equal to zero, using the REML based likelihood ratio test.

# Model1 <- lme(HW ~ Y + B + H + Y:B + Y:H, random = ~ Y | Subject.f, correlation = corAR1(form = ~ 1 | Subject.f),
#              method = "REML", data = HourlyWage) # Fitting model 1 using REML

# or

Model1 <- lme(HW ~ Y + B + H + Y:B + Y:H, random = ~ Y | Subject.f, correlation = corAR1(form = ~ Y | Subject.f),
              method = "REML", data = HourlyWage) # Fitting model 1 using REML

Model2 <- lme(HW ~ Y + B + H + Y:B + Y:H, random = ~ 1 | Subject.f, correlation = corAR1(form = ~ Y | Subject.f),
              method = "REML", data = HourlyWage) # Fitting model 2 using REML

LRT <- anova(Model1, Model2)$L.Ratio[-1] # Obtain LRT = 2ln(LRR) - 2ln(LNR)

# Calculating the p-value for the REML based likelihood ratio test

p_value <- (0.5 * pchisq(LRT, df = 2, lower.tail = FALSE)) + (0.5 * pchisq(LRT, df = 1, lower.tail = FALSE))
p_value

# We choose Model 1

## Testing the null hypothesis that the variance of the random intercept is zero, using REML-based likelihood ratio test.

Model3 <- lme(HW ~ Y + B + H + Y:B + Y:H, random = ~ Y - 1 | Subject.f, correlation = corAR1(form = ~ Y | Subject.f),
              method = "REML", data = HourlyWage) # Fitting model 3 using REML

# Obtaining the likelihood ratio observed test statistic, LRT = 2ln(LRR) - 2ln(LNR).

LRT <- anova(Model1, Model3)$L.Ratio[-1] # Obtain LRT = 2ln(LRR) - 2ln(LNR)

# Calculating the p-value for the REML based likelihood ratio test

p_value <- (0.5 * pchisq(LRT, df = 2, lower.tail = FALSE)) + (0.5 * pchisq(LRT, df = 1, lower.tail = FALSE))
p_value

# We choose Model 1

#################################################################################################################################################################

#### 4. Testing for fixed effects

## Testing the null hypothesis that there is no interaction effect of Year * Race, using ML-based likelihood ratio test.

Model1 <- lme(HW ~ Y + B + H + Y:B + Y:H, random = ~ Y | Subject.f, correlation = corAR1(form = ~ Y | Subject.f),
              method = "ML", data = HourlyWage) # Fitting model 1 using ML

Model4 <- lme(HW ~ Y + B + H, random = ~ Y | Subject.f, correlation = corAR1(form = ~ Y | Subject.f),
              method = "ML", data = HourlyWage) # Fitting model 1 using ML

anova(Model1, Model4)

# We choose Model 1

##############################################################################################################################################################

#### 5. Diagnostics of our final linear mixed model

Model1 <- lme(HW ~ Y + B + H + Y:B + Y:H, random = ~ Y | Subject.f, correlation = corAR1(form = ~ Y | Subject.f),
              method = "REML", data = HourlyWage) # Fitting model 1 using REML

## Checking the agreement between the predicted marginal values of HW and the observed mean
## values of HW as a function of Year grouped by Race.

# Generating the predicted marginal values of HW.

HourlyWage <- mutate(HourlyWage, Marg_HW_hat = fitted(Model1, level = 0)[-4361])
View(HourlyWage)

# Figure 3: Plot of the predicted marginal values of HW and the observed mean values of HW
# as a function of Year grouped by Race.

ggplot(data = HourlyWage, aes(x = Year, y = Marg_HW_hat)) +
  geom_line(aes(colour = Race), size = 1) +
  geom_point(data = Table_1, aes(Year, HW_mean, colour = Race), size = 4, shape = 15) +
  scale_x_continuous(breaks=seq(1980, 1987, 1)) +
  ylim(4, 8) +
  labs(x = "Year", y = "HW")

## Checking the agreement between the predicted conditional values of HW and the observed
## values of HW as a function of Year for the first 6 subjects in the data set.

# Generating the predicted conditional values of HW.

HourlyWage <- mutate(HourlyWage, Cond_HW_hat = fitted(Model1, level = 1)[-4361])
View(HourlyWage)

# Figure 4: Plot of the predicted conditional values of HW and the observed values of HW
# as a function of Year for the first 6 subjects in the data set.

ggplot(data = HourlyWage[1:48, ], aes(x = Year, y = Cond_HW_hat)) +
  geom_line(aes(colour = Subject.f), size = 1) +
  geom_point(aes(Year, HW, colour = Subject.f), size = 4) +
  scale_x_continuous(breaks=seq(1980, 1987, 1)) +
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  labs(x = "Year", y = "HW")

## Checking the distribution of the random effect vector mu = [mu0, mu1]'

# Table 2: EBLUPs of the random effects mu1 and mu2.

Table_2 <- ranef(Model1)
View(Table_2)

# Figure 5: Hexagonal 2-D plot of [mu0_hat, mu1_hat]' - Checking for bivariate normal.

ggplot(data = Table_2, aes(x = Table_2[,1], y = Table_2[,2])) +
  geom_hex() +
  scale_x_continuous(breaks=seq(-2, 10, 2)) +
  scale_y_continuous(breaks=seq(-0.5, 2, 0.5)) +
  labs(x = "mu0", y = "mu1")

## Checking whether the variance of the random errors is constant ovwer time

HourlyWage <- mutate(HourlyWage, Cond_Resid = HW - Cond_HW_hat) # Obtaining conditional residuals
View(HourlyWage)

# Figure 6: Scatter plot of the conditional residuals vs Year - Checking for constant variance.

ggplot(data = HourlyWage, aes(x = Year, y = Cond_Resid)) +
  geom_point() +
  scale_x_continuous(breaks=seq(1980, 1987, 1)) +
  scale_y_continuous(breaks=seq(-10, 30, 5)) +
  labs(x = "Year", y = "Conditional Residuals")

##########################################################################################################################################################

#### 6. Fixed effect estimates for your final linear mixed model

## Table 3: The estimates of the fixed effects in model 1.

Table_3 <- summary(Model1)$tTable
round(Table_3, 4)

## Table 4: The approximate 95% confidence intervals for the fixed effects in model 1.

Table_4 <- intervals(Model1, level = 0.95, which = "fixed")
Table_4

## Estimating the contrast tau1 = -beta2 - 7beta4 and testing the null hypothesis tau1 = 0.

C <- matrix(c(0, 0, -1, 0, -7, 0), 1)
tau1 <- glht(Model1, linfct = C)
summary(tau1)

## Estimating the contrast tau2 = -beta2 + beta3 - 7beta4 + 7beta5 and testing the null hypothesis tau2 = 0.

C <- matrix(c(0, 0, -1, 1, -7, 7), 1)
tau2 <- glht(Model1, linfct = C)
summary(tau2)
