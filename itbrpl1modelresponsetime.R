# interpretation task
# bilingual group
# statistical analysis
# response time (in log)

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

# model 1 with interaction between the quantifier and noun
# dependent variable log

model1 <- lmer(log ~ Quantifier*Noun + (1|Participant) + (1|Item), data = dataoff3, REML = FALSE)

summary(model1)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Quantifier * Noun + (1 | Participant) + (1 | Item)
#Data: dataoff3

#AIC      BIC   logLik deviance df.resid 
#844.9    877.2   -415.5    830.9      733 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.7354 -0.6307 -0.0728  0.5876  3.2956 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 0.080112 0.28304 
#Item        (Intercept) 0.004171 0.06459 
#Residual                0.152998 0.39115 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)         7.34674    0.05191  89.82694 141.525   <2e-16 ***
#  QuantifierP        -0.06024    0.04026 677.39830  -1.496    0.135    
#NounM               0.02930    0.04123 677.36726   0.711    0.478    
#QuantifierP:NounM   0.03763    0.05775 677.52061   0.652    0.515    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfrP NounM 
#QuantifierP -0.397              
#NounM       -0.385  0.496       
#QuntfrP:NnM  0.276 -0.695 -0.713

# there is not a significant interaction between the quantifier and the noun.

# model 2 with no interaction between quantifier and noun

model2 <- lmer(log ~ Quantifier + Noun + (1|Participant) + (1|Item), data = dataoff3, REML = FALSE)

summary(model2)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#Data: dataoff3

#AIC      BIC   logLik deviance df.resid 
#843.4    871.0   -415.7    831.4      734 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.7157 -0.6217 -0.0736  0.5859  3.2710 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 0.080036 0.28291 
#Item        (Intercept) 0.004157 0.06447 
#Residual                0.153109 0.39129 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)   7.33741    0.04988  76.89563 147.114   <2e-16 ***
#  QuantifierP  -0.04200    0.02896 679.87617  -1.451   0.1474    
#NounM         0.04846    0.02891 677.55283   1.676   0.0941 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfrP
#QuantifierP -0.297       
#NounM       -0.280  0.001

# nested models

anova (model1, model2)

# Data: dataoff3
#Models:
#  model2: log ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#model1: log ~ Quantifier * Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model2    6 843.36 871.00 -415.68   831.36                     
#model1    7 844.93 877.18 -415.47   830.93 0.4245  1     0.5147

model3 <- lmer(log ~ Quantifier + (1|Participant) + (1|Item), data = dataoff3, REML = FALSE)

summary(model3)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Quantifier + (1 | Participant) + (1 | Item)
#Data: dataoff3

#AIC      BIC   logLik deviance df.resid 
#844.2    867.2   -417.1    834.2      735 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.6344 -0.6141 -0.0894  0.5787  3.2034 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 0.079636 0.28220 
#Item        (Intercept) 0.004072 0.06381 
#Residual                0.153822 0.39220 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)   7.36080    0.04776  65.35766 154.123   <2e-16 ***
#  QuantifierP  -0.04206    0.02902 679.95927  -1.449    0.148    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#QuantifierP -0.310

anova(model2, model3)

#Data: dataoff3
#Models:
#  model3: log ~ Quantifier + (1 | Participant) + (1 | Item)
#model2: log ~ Quantifier + Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
#model3    5 844.16 867.19 -417.08   834.16                       
#model2    6 843.36 871.00 -415.68   831.36 2.8015  1    0.09417 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# the variable noun contributites marginally to the model

model4 <- lmer(log ~ Noun + (1|Participant) + (1|Item), data = dataoff3, REML = FALSE)

summary(model4)

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Noun + (1 | Participant) + (1 | Item)
#Data: dataoff3

#AIC      BIC   logLik deviance df.resid 
#843.5    866.5   -416.7    833.5      735 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.7509 -0.6238 -0.0720  0.5610  3.2117 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 0.079763 0.28242 
#Item        (Intercept) 0.004219 0.06496 
#Residual                0.153582 0.39190 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)   7.31591    0.04762  64.07757 153.618   <2e-16 ***
#  NounM         0.04849    0.02895 677.56956   1.675   0.0944 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#NounM -0.294

anova (model2, model4)

#Data: dataoff3
#Models:
#  model3: log ~ Quantifier + (1 | Participant) + (1 | Item)
#model4: log ~ Noun + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model3    5 844.16 867.19 -417.08   834.16                     
#model4    5 843.46 866.49 -416.73   833.46 0.7016  0 

# plotting

tab_model(model4)

# ploting the graph with the interaction in log

plot(allEffects(model4), 
     grid = TRUE, 
     multiline = TRUE, 
     main = "Values in log", 
     colors = c("#F5564E", "#26BCC9", "#006600")) 

# plotting the graph with full labels

dataoff3$Quantifier <- factor(dataoff3$Quantifier, 
                             levels = c("S", "P"),  # Original labels
                             labels = c("Singular", "Plural"))  # New labels

dataoff3$Noun <- factor(dataoff3$Noun, 
                       levels = c("C", "M"),  # Original labels
                       labels = c("Count", "Mass"))  # New labels

plot(allEffects(model4), 
     grid = TRUE, 
     main = "Values predicted by the model",
     colors = c("#FA8072", "#26BCC9")) 


