# Brazilian Portuguese L1
# self-paced reading
# critical region: segment 4
# statistical analysis

# independent variables
# quantifier: s (singular) and p (plural)
# noun: c (count) and m (mass)

# experimental conditions
# singular quantifier + count noun: s + c (much feather)
# singular quantifier + mass noun:  s + m (much silver)
# plural quantifier + count noun:   p + c (many feathers)
# plural quantifier + mass noun:    p + m (many silvers)

# regression mixed linear model

# open packages

library(tidyverse)
library(dplyr)
library(png)
library(ggplot2)
library(nortest)
library(som)
library(lattice)
library(effects)
library(sjPlot)
library(car)
library(lme4)
library(lmerTest)
library(rms)
library(ordinal)
library(emmeans)
library(readr)

# working directory

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS4 - Quantifier") 

# bring the data set that we have cleaned up and prepared for statistical analysis
# in this case, is the dataon3

dataon3 <- read.csv("dataon3.csv")

# check the structure of the data

str(dataon3)

# exclude the column 'X' and change the classes of the relevant columns 

dataon3 <- dataon3 %>% select(-X)
dataon3$segment <- as.numeric(dataon3$segment)
dataon3$Reading.time <- as.numeric(dataon3$Reading.time)
dataon3 <- dataon3 %>% mutate_if(sapply(dataon3, is.character), as.factor)
dataon3$Item <- as.factor(dataon3$Item)

# establish 'S' (singular) as the reference level of the variable 'quantifier'

dataon3$Quantifier = relevel(dataon3$Quantifier, ref = "S")

# linear mixed regression model

# we are building a model including:
# fixed effects (independent variables that are quantifier and noun) and random 
# effects (participants and items)

# we are starting from the most complex model and then we start to simplify it
# in order to find which one better explains the data

# we test the effect of the models through a comparison by nested models
# (through ANOVA)

# the most complex model (with interaction)
# consider Reading.time as a dependent variable

model1 <- lmer(Reading.time ~ Quantifier*Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model1)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: Reading.time ~ Quantifier * Noun + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#9738.0   9770.7  -4862.0   9724.0      774 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.1041 -0.6578 -0.1485  0.4414  4.4340 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  8683.7   93.19  
#Item        (Intercept)   160.2   12.66  
#Residual                12633.4  112.40  
#Number of obs: 781, groups:  Participant, 51; Item, 16

#Fixed effects:
#                 Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)       437.2041    15.6414  80.8960  27.952   <2e-16 ***
#  QuantifierP        10.0479    11.3785 716.9715   0.883    0.378    
#NounM               0.5938    11.3728 715.4089   0.052    0.958    
#QuantifierP:NounM   4.6089    16.1164 715.1737   0.286    0.775    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfrP NounM 
#QuantifierP -0.362              
#NounM       -0.362  0.498       
#QuntfrP:NnM  0.256 -0.706 -0.706

# no significant effect for the interaction

model2 <- lmer(Reading.time ~ Quantifier + Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model2)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: Reading.time ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#9736.1   9764.1  -4862.1   9724.1      775 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.1150 -0.6596 -0.1531  0.4477  4.4234 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  8683     93.18  
#Item        (Intercept)   160     12.65  
#Residual                12635    112.41  
#Number of obs: 781, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)  436.060     15.121  70.771  28.838   <2e-16 ***
#  QuantifierP   12.345      8.061 717.925   1.531    0.126    
#NounM          2.890      8.055 715.104   0.359    0.720    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfrP
#QuantifierP -0.265       
#NounM       -0.265 -0.001

# no significant effect

# comparing the nested models in order to check if the interaction significantly 
# adds to the model

anova(model1, model2)

#Data: dataon3
#Models:
#  model2: Reading.time ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#model1: Reading.time ~ Quantifier * Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model2    6 9736.1 9764.1 -4862.1   9724.1                     
#model1    7 9738.0 9770.7 -4862.0   9724.0 0.0818  1     0.7749

# there is not a statistically significant difference between model 1
# and model 2. model 2 is better without the interaction between the quantifier and noun

# simpler model less complex (without the independent variable 'noun')

model3 <- lmer(Reading.time ~ Quantifier + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model3)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: Reading.time ~ Quantifier + (1 | Participant) + (1 | Item)
#   Data: dataon3

#     AIC      BIC   logLik deviance df.resid 
#  9734.2   9757.5  -4862.1   9724.2      776 

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-2.1262 -0.6555 -0.1464  0.4520  4.4361 

#Random effects:
# Groups      Name        Variance Std.Dev.
# Participant (Intercept)  8682.0   93.18  
# Item        (Intercept)   161.3   12.70  
# Residual                12636.5  112.41  
#Number of obs: 781, groups:  Participant, 51; Item, 16

#Fixed effects:
#            Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)  437.497     14.584  61.250  29.998   <2e-16 ***
#QuantifierP   12.348      8.062 717.924   1.532    0.126    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr)
#QuantifierP -0.275

# no effect for quantifier

# the intercept refers to the singular quantifier that it is used to compare. the 
# positive value is the additional time in ms above the value of the intercept.

# comparing between nested models to check whether the noun adds to the model

anova(model2, model3)

#Data: dataon3
#Models:
#  model3: Reading.time ~ Quantifier + (1 | Participant) + (1 | Item)
#model2: Reading.time ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model3    5 9734.2 9757.5 -4862.1   9724.2                     
#model2    6 9736.1 9764.1 -4862.1   9724.1 0.1287  1     0.7198

# no difference between nested models. model 3 is better.

# model 4 without the quantifier

model4 <- lmer(Reading.time ~ Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model4)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: Reading.time ~ Noun + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#9736.5   9759.8  -4863.2   9726.5      776 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.1351 -0.6520 -0.1713  0.4364  4.3581 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  8668.8   93.11  
#Item        (Intercept)   161.6   12.71  
#Residual                12676.0  112.59  
#Number of obs: 781, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)  442.194     14.578  61.262   30.33   <2e-16 ***
#  NounM          2.907      8.068 715.097    0.36    0.719    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#NounM -0.275

# no effect for noun

# comparing between nested models to check whether the quantifier contributes to the model

anova(model2, model4)

#Data: dataon3
#Models:
#  model4: Reading.time ~ Noun + (1 | Participant) + (1 | Item)
#model2: Reading.time ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model4    5 9736.5 9759.8 -4863.2   9726.5                     
#model2    6 9736.1 9764.1 -4862.1   9724.1 2.3411  1      0.126

# no difference between models

# plotting 

# plot the table with the coefficients for the significant model (model3)

tab_model(model4)

# plot the graph

plot(allEffects(model4), 
     grid = TRUE, 
     main = "Values predicted by the model",
     colors = c("#FA8072", "#26BCC9", "#006600")) 

# save the graph

# graph for models with interaction

# plot(allEffects(model1), 
#     grid = T, 
#     multiline = T, 
#     main = "Values predicted by the model")

# intercept is the baseline value for the dependent variable (in this case, 
# reading time which is significant)

# in model3, the fixed intercept was the quantifier since was the best fitting 
# model 

# random intercepts were participants and items

# getting the graphs with the full labels

dataon3$Noun <- factor(dataon3$Noun, 
                       levels = c("C", "M"),  # Original labels
                       labels = c("Count", "Mass"))  # New labels

plot(allEffects(model4), 
     grid = TRUE, 
     main = "Values predicted by the model",
     colors = c("#FA8072", "#26BCC9", "#006600")) 
