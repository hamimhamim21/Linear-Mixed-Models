### Add packages we use in Assignment 3

library("nlme", lib.loc="~/R/win-library/3.5")
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

## Import the WageRace.csv file into R.

WageRace <- read.csv(file = "WageRace.csv")

## A numerical and graphical summary for the WageRace data that provides an indication for whether the interaction effects of Year * Race
## should be included in our marginal model.

# Table 1: Summary of HW grouped by Year and Race.

Table_1 <- ddply(WageRace, c("Race", "Year"), summarise, n = length(HW), HW_mean = mean(HW), HW_stdev = sd(HW))
Table_1

# Figure 1: Plot of the values of HW vs Year grouped by Race.

ggplot(data = Table_1, aes(x = Year, y = HW_mean)) +
  geom_point(aes(colour = Race), size = 4, shape = 15) +
  geom_line(aes(colour = Race)) +
  scale_x_discrete(limits = c(1984, 1985, 1986, 1987)) +
  ylim(5,7.5)

## A graphical summary for the WageRace data that provides an indication for whether different variances should 
## be modelled in the R matrix.

# Figure 2: Plot of the values of HW vs Year grouped by Race.

ggplot(data = WageRace, aes(x = Year, y = HW)) +
  geom_point() + 
  scale_x_discrete(limits = c(1984, 1985, 1986, 1987)) +
  scale_y_continuous(breaks=seq(0, 25, 5)) +
  facet_grid(.~ Race) + 
  theme(legend.position="none")

####################################################################################################################################################################

#### 3. Choosing the appropriate R matrix

## Create the index variable.

WageRace <- mutate(WageRace, Index = if_else(Year == 1984, 1, 
                                             if_else(Year == 1985, 2, 
                                                     if_else(Year == 1986, 3, 4))))

## Fitting model 1A using gls() and REML

Model1A <- gls(HW ~ Y2 + Y3 + Y4 + H + Y2:H + Y3:H + Y4:H, 
               correlation = corSymm(form = ~ Index | Subject),
               method = "REML", data = WageRace)

## Fitting model 1B using gls() and REML

Model1B <- gls(HW ~ Y2 + Y3 + Y4 + H + Y2:H + Y3:H + Y4:H, 
               correlation = corAR1(form = ~ Index | Subject),
               method = "REML", data = WageRace) 

## Fitting model 1C using gls() and REML

Model1C <- gls(HW ~ Y2 + Y3 + Y4 + H + Y2:H + Y3:H + Y4:H, 
               correlation = corCompSymm(form = ~ Index | Subject),
               method = "REML", data = WageRace)

## Calculating AIC for models 1A, 1B and 1C.

AIC(Model1A, Model1B, Model1C)

## Calculating BIC for models 1A, 1B and 1C.

AIC(Model1A, Model1B, Model1C, k = log(592))

##########################################################################################################################################################

#### 4. Fixed effect estimates for your final marginal model

## Table 2: The estimates of the fixed effects in model 1A.

Table_2 <- summary(Model1A)$tTable
round(Table_2, 4)

## Estimating the contrast tau1 - tau2 = beta6 - beta5 and testing the null hypothesis tau1 - tau2 = 0.

C <- matrix(c(0, 0, 0, 0, 0, -1, 1, 0), 1)
tau1tau2 <- glht(Model1A, linfct = C)
summary(tau1tau2)

## Estimating the contrast eta1 - eta2 =  beta7 - beta5 and testing the null hypothesis eta1 - eta2 = 0.

C <- matrix(c(0, 0, 0, 0, 0, -1, 0, 1), 1)
eta1eta2 <- glht(Model1A, linfct = C)
summary(eta1eta2)


