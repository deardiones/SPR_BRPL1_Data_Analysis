# Brazilian Portuguese
# segment 6 post critical
# statistical analysis

# independent variables
# quantifier: S (singular) e P (plural)
# noun: C (count) e M (mass)

# experimental conditions
# singular quantifier + count noun: s + c (much feather)
# singular quantifier + mass noun:  s + m (much silver)
# plural quantifier + count noun:   p + c (many feathers)
# plural quantifier + mass noun:    p + m (many silvers)

# mixed linear model

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

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS6 - Post Critical")

# bring the data set that we have cleaned up and prepared for statistical analysis
# in this case, is the dataon3

dataon3 <- read.csv("dataon3.csv")

# check the structure of the data

str(dataon3)

# exclude the column 'X' and change the classes of the relevant columns 

dataon3 <- dataon3 %>% select(-X)
dataon3$Segment <- as.numeric(dataon3$segment)
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

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: Reading.time ~ Quantifier * Noun + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#9452.1   9484.6  -4719.1   9438.1      761 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.4662 -0.6409 -0.1552  0.4566  4.0375 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  5030.86  70.929 
#Item        (Intercept)    69.53   8.338 
#Residual                11042.26 105.082 
#Number of obs: 768, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)       457.1902    12.6840  93.1097  36.045   <2e-16 ***
#  QuantifierP         8.2851    10.7128 706.3351   0.773    0.440    
#NounM              -3.3333    10.6989 703.5192  -0.312    0.755    
#QuantifierP:NounM   0.9743    15.2007 703.9169   0.064    0.949    
#---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfrP NounM 
#QuantifierP -0.426              
#NounM       -0.426  0.505       
#QuntfrP:NnM  0.300 -0.704 -0.704

# less complex model (without interaction between the quantifier and the noun)

model2 <- lmer(Reading.time ~ Quantifier + Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model2)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: Reading.time ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#9450.1   9478.0  -4719.1   9438.1      762 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.4640 -0.6431 -0.1542  0.4578  4.0400 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  5030.48  70.93  
#Item        (Intercept)    69.55   8.34  
#Residual                11042.36 105.08  
#Number of obs: 768, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)  456.946     12.098  77.362  37.770   <2e-16 ***
#  QuantifierP    8.769      7.603 707.244   1.153    0.249    
#NounM         -2.850      7.596 703.698  -0.375    0.708    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfrP
#QuantifierP -0.317       
#NounM       -0.316  0.017

# comparing the nested models in order to check if the interaction significantly 
# adds to the model

anova(model1, model2)

#Data: dataon3
#Models:
#  model2: Reading.time ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#model1: Reading.time ~ Quantifier * Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model2    6 9450.1 9478.0 -4719.1   9438.1                     
#model1    7 9452.1 9484.6 -4719.1   9438.1 0.0041  1     0.9489

# model 2 is better

# simpler model less complex (without the independent variable 'noun')

model3 <- lmer(Reading.time ~ Quantifier + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model3)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: Reading.time ~ Quantifier + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#9448.3   9471.5  -4719.1   9438.3      763 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.4491 -0.6371 -0.1552  0.4604  4.0252 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  5033.30  70.946 
#Item        (Intercept)    69.11   8.313 
#Residual                11044.48 105.093 
#Number of obs: 768, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)  455.510     11.478  62.695   39.69   <2e-16 ***
#  QuantifierP    8.817      7.603 707.261    1.16    0.247    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#QuantifierP -0.328

# comparing the nested models to check whether 'noun' significantly contributes
# to the model

anova(model2, model3)

#Data: dataon3
#Models:
#  model3: Reading.time ~ Quantifier + (1 | Participant) + (1 | Item)
#model2: Reading.time ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model3    5 9448.3 9471.5 -4719.1   9438.3                     
#model2    6 9450.1 9478.0 -4719.1   9438.1 0.1408  1     0.7075

# model 3 is better

# simpler model with 'noun' but without the independent variable 'quantifier'

model4 <- lmer(Reading.time ~ Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model4)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: Reading.time ~ Noun + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#9449.5   9472.7  -4719.7   9439.5      763 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.5009 -0.6296 -0.1649  0.4381  4.0810 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept)  5023.48  70.877 
#Item        (Intercept)    65.92   8.119 
#Residual                11066.52 105.198 
#Number of obs: 768, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)  461.366     11.461  62.628  40.255   <2e-16 ***
#  NounM         -2.998      7.603 703.754  -0.394    0.693    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#NounM -0.329

anova(model2, model4)

#Data: dataon3
#Models:
#  model4: Reading.time ~ Noun + (1 | Participant) + (1 | Item)
#model2: Reading.time ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model4    5 9449.5 9472.7 -4719.7   9439.5                     
#model2    6 9450.1 9478.0 -4719.1   9438.1 1.3275  1     0.2492

# model 4 is better

# plotting 

# plot the table with the coefficients for the best model (model3)

tab_model(model4)

# plot the graph

plot(allEffects(model4), 
     grid = TRUE, 
     main = "Values predicted by the model",
     colors = c("#FA8072", "#26BCC9")) 

# save the graph

# plotting the graph with full labels

dataon3$Quantifier <- factor(dataon3$Quantifier, 
                             levels = c("S", "P"),  # Original labels
                             labels = c("Singular", "Plural"))  # New labels

dataon3$Noun <- factor(dataon3$Noun, 
                       levels = c("C", "M"),  # Original labels
                       labels = c("Count", "Mass"))  # New labels

plot(allEffects(model4), 
     grid = TRUE, 
     main = "Values predicted by the model",
     colors = c("#FA8072", "#26BCC9")) 
