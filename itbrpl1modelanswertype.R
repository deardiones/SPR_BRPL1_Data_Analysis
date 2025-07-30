# brazilian portuguese
# interpretation task
# statistical analysis
# answer type

# independent variables
# quantifier: S (singular) e P (plural)
# noun: C (count) e M (mass)

# experimental conditions
# singular quantifier + count noun: s + c (much feather)
# singular quantifier + mass noun:  s + m (much silver)
# plural quantifier + count noun:   p + c (many feathers)
# plural quantifier + mass noun:    p + m (many silvers)

# linear generalized mixed model

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

# working directory

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Interpretation Task")

# bringuing the treat data

dataoff3 <- read.csv("dataoFF3.csv")

str(dataoff3)

# cleaning up unnecessary columns

dataoff3 <- dataoff3 %>% select(-X)

# renaming

dataoff3$Response.time <- as.numeric(dataoff3$Response.time)
dataoff3 <- dataoff3 %>% mutate_if(sapply(dataoff3, is.character), as.factor)
dataoff3$Item <- as.factor(dataoff3$Item)

# establishing 'S' (singular) as the reference level for the variable 'quantifier'

dataoff3$Quantifier = relevel(dataoff3$Quantifier, ref = "S")

# consulting the reference level of the dependent variable

levels(dataoff3$Answer)

# [1] 'cardinal' 'volume'
# the reference level will be cardinal

# model1 with the interaction between quantifier and noum
# fixed effects (independent variables)
# random effects (participants and items)
# answer is the dependent variable

model1 = glmer(Answer ~ Quantifier*Noun + (1|Participant) + (1|Item), data = dataoff3, family = binomial)

summary(model1)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Answer ~ Quantifier * Noun + (1 | Participant) + (1 | Item)
# Data: dataoff3

# AIC      BIC   logLik deviance df.resid 
# 660.6    688.2   -324.3    648.6      734 

# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
#-7.6487 -0.3789 -0.1666  0.3656  5.4739 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 4.4793   2.1164  
#Item        (Intercept) 0.3771   0.6141  
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        -0.6458     0.3987  -1.620   0.1052    
#QuantifierP        -1.9233     0.3418  -5.627 1.83e-08 ***
#  NounM               1.4732     0.2973   4.955 7.22e-07 ***
#  QuantifierP:NounM  -1.0002     0.4548  -2.199   0.0279 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfrP NounM 
#QuantifierP -0.285              
#NounM       -0.367  0.319       
#QuntfrP:NnM  0.239 -0.644 -0.639

# the interaction between quantifier and the noun is statistically significant
# p < 0.02

# model without interaction

model2 <- glmer(Answer ~ Quantifier + Noun + (1|Participant) + (1|Item), data = dataoff3, family = binomial)

summary(model2)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
#Family: binomial  ( logit )
#Formula: Answer ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#Data: dataoff3

#AIC      BIC   logLik deviance df.resid 
#663.4    686.4   -326.7    653.4      735 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-6.9328 -0.3983 -0.1674  0.3491  6.3740 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 4.433    2.1054  
#Item        (Intercept) 0.372    0.6099  
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -0.4414     0.3843  -1.149    0.251    
#QuantifierP  -2.4424     0.2627  -9.299  < 2e-16 ***
#  NounM         1.0722     0.2288   4.686 2.79e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfrP
#QuantifierP -0.162       
#NounM       -0.283 -0.202

# comparing between nested models

anova(model1, model2)

# Data: dataoff3
#Models:
#  model2: Answer ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#model1: Answer ~ Quantifier * Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
#model2    5 663.41 686.44 -326.70   653.41                       
#model1    6 660.55 688.19 -324.28   648.55 4.8569  1    0.02754 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# there is a statistical significant difference between the two models p < 0.02

# model 3 without the variable noun

model3 <- glmer(Answer ~ Quantifier + (1|Participant) + (1|Item), data = dataoff3, family = binomial)

summary (model3)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Answer ~ Quantifier + (1 | Participant) + (1 | Item)
# Data: dataoff3

# AIC      BIC   logLik deviance df.resid 
# 684.9    703.4   -338.5    676.9      736 

# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
#-4.6903 -0.4362 -0.1903  0.4046  5.2439 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 3.9923   1.9981  
#Item        (Intercept) 0.3158   0.5619  
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.07298    0.34944   0.209    0.835    
#QuantifierP -2.31770    0.25079  -9.242   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#QuantifierP -0.238

anova (model2, model3)

# Data: dataoff3
#Models:
#  model3: Answer ~ Quantifier + (1 | Participant) + (1 | Item)
#model2: Answer ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
#model3    4 684.94 703.37 -338.47   676.94                         
#model2    5 663.41 686.44 -326.70   653.41 23.531  1  1.229e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# model 4 with the variable noun, but without the quantifier

model4 <- glmer(Answer ~ Noun + (1|Participant) + (1|Item), data = dataoff3, family = binomial)

summary(model4)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
#Formula: Answer ~ Noun + (1 | Participant) + (1 | Item)
#Data: dataoff3

#AIC      BIC   logLik deviance df.resid 
#779.2    797.7   -385.6    771.2      736 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.2320 -0.5235 -0.3028  0.5712  3.4992 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 2.8640   1.6923  
#Item        (Intercept) 0.2543   0.5042  
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.3361     0.3101  -4.309 1.64e-05 ***
#  NounM         0.8428     0.1932   4.363 1.29e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#NounM -0.344

anova(model2, model4)

#Data: dataoff3
#Models:
#  model4: Answer ~ Noun + (1 | Participant) + (1 | Item)
#model2: Answer ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
#model4    4 779.23 797.65 -385.61   771.23                         
#model2    5 663.41 686.44 -326.70   653.41 117.82  1  < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# post hoc analysis to verify the levels in each variable

post.hoc = emmeans(model1, ~ Quantifier*Noun)

pairs(post.hoc, adjust="tukey")

#  contrast  estimate    SE  df z.ratio p.value
# S C - P C    1.923 0.342 Inf   5.627  <.0001
# S C - S M   -1.473 0.297 Inf  -4.955  <.0001
# S C - P M    1.450 0.323 Inf   4.494  <.0001
# P C - S M   -3.396 0.375 Inf  -9.068  <.0001
# P C - P M   -0.473 0.350 Inf  -1.351  0.5301
# S M - P M    2.923 0.352 Inf   8.315  <.0001

# bonferroni correction

pairs(post.hoc, adjust="bonferroni")

# contrast  estimate    SE  df z.ratio p.value
# S C - P C    1.923 0.342 Inf   5.627  <.0001
# S C - S M   -1.473 0.297 Inf  -4.955  <.0001
# S C - P M    1.450 0.323 Inf   4.494  <.0001
# P C - S M   -3.396 0.375 Inf  -9.068  <.0001
# P C - P M   -0.473 0.350 Inf  -1.351  1.0000
# S M - P M    2.923 0.352 Inf   8.315  <.0001

# Results are given on the log odds ratio (not the response) scale. 
# P value adjustment: bonferroni method for 6 tests 

# bring the table to help to interpret

table <- table(dataoff3$condition, dataoff3$Answer)

table

#           Cardinal  Volume
# P+C       160       34
# P+M       144       39
# S+C       112       74
# S+M       68        109

# plotting

tab_model(model1)

plot(allEffects(model1), 
     grid = TRUE, 
     multiline = TRUE, 
     main = "Values Predicted by the Model", 
     colors = c("#F5564E", "#26BCC9", "#006600")) 

