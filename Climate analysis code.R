####################################################################
# R Code for the analysis in P.L. Hooper,
# "Human social complexity was significantly lower 
#  during climate cooling events of the past 10 millenia"

# The original Seshat data can be found at sehatdatabank.info

######
# Load libraries

library(lme4)
library(MASS)

######
# Set working directory

setwd("~/Documents/R/climate/") 

######
# Load the data

S0 <- read.csv("Seshat data for Climate analysis.csv") # Load dataset
head(S0) # Preview of data
names(S0) # Column names

table(S0$NGA) # Observations by location (aka NGA = natural geographic area)
table(S0$OldWorld) # Observations in Old World vs. not
table(S0$InNorthernRegion) # Regions north of the tropic of cancer and adjacent to North Atlantic or Arctic
table(S0$NorthernRegion) # Observations by northern region
table(S0$CoolingPeriod) # Observations occuring during the last 200 years of cooling events

summary(S0$Time) # Time, aka Year BCE/CE. TimeK is Time/1000.
summary(S0$Age) # Location age. AgeK is Age/1000.
summary(S0$PC1) # Measure of social complexity from Turchin et al. (2017)

####################################################################
# Regressions for Table 2

eq <- PC1 ~ TimeK * AgeK + I(AgeK^2)  + OldWorld + CoolingPeriod  + (1 | NGA)

# Table 2A - full sample
summary(res <- lmer(eq,data=S0))
confint(res)

# Based on the regression, how much does PC1 change in 1 year around the mean TimeK and AgeK?
T1 <- (mean(S0$TimeK)-0.0005) * fixef(res)[2] + (mean(S0$AgeK)-0.0005) * fixef(res)[3] + (mean(S0$AgeK)-0.0005)^2 * fixef(res)[4] + (mean(S0$TimeK)-0.0005) * (mean(S0$AgeK)-0.0005) * fixef(res)[7] # PC1 half a year before the mean TimeK and AgeK. Ignore terms that don't changes with time.
T2 <- (mean(S0$TimeK)+0.0005) * fixef(res)[2] + (mean(S0$AgeK)+0.0005) * fixef(res)[3] + (mean(S0$AgeK)+0.0005)^2 * fixef(res)[4] + (mean(S0$TimeK)+0.0005) * (mean(S0$AgeK)+0.0005) * fixef(res)[7] # PC1 half a year after the mean TimeK and AgeK. Ignore terms that don't changes with time.
T2 - T1 # What is the expected change in PC1 over 1 year?

# Table 2B - early subsample
summary(res <- lmer(eq,data=S0,subset=TimeK<=1))
confint(res)

####################################################################
# Regressions for Table 3

eq <- PC1 ~ TimeK * AgeK +I(AgeK^2)  + OldWorld + InNorthernRegion + CoolingPeriod:I(1-InNorthernRegion) + CoolingPeriod:InNorthernRegion + (1 | NGA)

# Table 3A - full sample
summary(res <- lmer(eq,data=S0))
confint(res)

# Table 3B - early subsample
summary(res <- lmer(eq,data=S0,subset=TimeK<=1))
confint(res)

####################################################################
# Regression for Table 4

S0$NorthernRegion <- relevel(factor(S0$NorthernRegion),ref="No")
eq <- PC1 ~ TimeK * AgeK+I(AgeK^2)  + OldWorld + NorthernRegion + CoolingPeriod:NorthernRegion + (1 | NGA)

# Table 4A - full sample
summary(res <- lmer(eq,data=S0))
confint(res)

# Table 4B - early subsample
summary(res <- lmer(eq,data=S0,subset=TimeK<=1))
confint(res)

####################################################################
