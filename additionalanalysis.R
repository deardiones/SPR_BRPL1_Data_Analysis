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

# adding the column interpretation

dataoff3$Interpretation <- ifelse(dataoff3$Answer == "Cardinal", "Cardinal", "Volume")

table(dataoff3$Interpretation)

# checking the data

dataoff3$Interpretation <- factor(dataoff3$Answer, levels = c("Cardinal", "Volume"))

table(dataoff3$Interpretation)

table(dataoff3$Answer, useNA = "ifany")

str(dataoff3$Answer)
head(dataoff3$Answer)

dataoff3$Interpretation <- factor(dataoff3$Answer, levels = c("Cardinal", "Volume"))


# Convert 'Condition' and 'Interpretation' to numeric
dataoff3$condition_numeric <- as.numeric(dataoff3$condition)
dataoff3$Interpretation_numeric <- as.numeric(dataoff3$Interpretation)

# Now calculate the correlation
cor(dataoff3$Condition_numeric, dataoff3$Interpretation_numeric, use = "complete.obs")

# Check the structure of the data
str(dataoff3$condition)
str(dataoff3$Interpretation)

# Compute correlation
cor(dataoff3$Condition_numeric, dataoff3$Interpretation_numeric, use = "complete.obs")

# Convert 'Condition' factor to numeric
dataoff3$condition_numeric <- as.numeric(dataoff3$condition)

# Check the conversion
head(dataoff3$condition_numeric)

# Convert 'Interpretation' factor to numeric (Cardinal = 1, Volume = 2)
dataoff3$Interpretation_numeric <- as.numeric(dataoff3$Interpretation)

# Check the conversion
head(dataoff3$Interpretation_numeric)

# Compute correlation
cor(dataoff3$Condition_numeric, dataoff3$Interpretation_numeric, use = "complete.obs")

# Check levels of Condition and Interpretation
levels(dataoff3$condition)
levels(dataoff3$Interpretation)

# Convert 'Condition' factor to numeric (assigning numeric values explicitly)
dataoff3$condition_numeric <- as.numeric(factor(dataoff3$condition, levels = c("P+C", "P+M", "S+C", "S+M")))

# Convert 'Interpretation' factor to numeric (Cardinal = 1, Volume = 2)
dataoff3$Interpretation_numeric <- as.numeric(factor(dataoff3$Interpretation, levels = c("Cardinal", "Volume")))

# Check the conversion
head(dataoff3$condition_numeric)
head(dataoff3$Interpretation_numeric)

cor(dataoff3$condition_numeric, dataoff3$Interpretation_numeric, use = "complete.obs")

# Subset data for each condition
data_S_plus_M <- subset(dataoff3, condition == "S+M")
data_S_plus_C <- subset(dataoff3, condition == "S+C")
data_P_plus_C <- subset(dataoff3, condition == "P+C")
data_P_plus_M <- subset(dataoff3, condition == "P+M")

# Perform Chi-square tests for each subset
chisq_S_plus_M <- chisq.test(table(data_S_plus_M$Interpretation))
chisq_S_plus_C <- chisq.test(table(data_S_plus_C$Interpretation))
chisq_P_plus_C <- chisq.test(table(data_P_plus_C$Interpretation))
chisq_P_plus_M <- chisq.test(table(data_P_plus_M$Interpretation))

# Print results
chisq_S_plus_M # X-squared = 9.4972, df = 1, p-value = 0.002058
chisq_S_plus_C # X-squared = 7.7634, df = 1, p-value = 0.005331
chisq_P_plus_C # not significant
chisq_P_plus_M # not significant

# Fit logistic regression model
model1 <- glmer(Interpretation ~ condition + (1 | Participant) + (1 | Item), data = dataoff3, family = binomial)

# Summarize model results
summary(model1)

# Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) ['glmerMod']
# Family: binomial  ( logit )
# Formula: Interpretation ~ condition + (1 | Participant) + (1 | Item)
# Data: dataoff3

# AIC      BIC   logLik deviance df.resid 
# 660.6    688.2   -324.3    648.6      734 

# Scaled residuals: 
# Min      1Q  Median      3Q     Max 
# -7.6487 -0.3789 -0.1666  0.3656  5.4739 

# Random effects:
# Groups      Name        Variance Std.Dev.
# Participant (Intercept) 4.4792   2.1164  
# Item        (Intercept) 0.3771   0.6141  
# Number of obs: 740, groups:  Participant, 50; Item, 16

# Fixed effects:
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -2.5691     0.4451  -5.772 7.85e-09 ***
# conditionP+M   0.4730     0.3500   1.351    0.177    
# conditionS+C   1.9233     0.3418   5.627 1.83e-08 ***
# conditionS+M   3.3964     0.3746   9.068  < 2e-16 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
# (Intr) cndP+M cndS+C
# conditinP+M -0.434              
# conditinS+C -0.513  0.565       
# conditinS+M -0.534  0.531  0.659

# there is a significant difference between cardinal and volume responses when
# we have bare singulars, but the difference is not significant for bare plurals

tab_model(model1)

plot(allEffects(model1), 
     grid = TRUE, 
     main = "Values predicted by the model",
     colors = c("#FA8072", "#26BCC9")) 

################################################################################

# now investigating if the answer (cardinal and volume) is affected by response
# time (log)

model2 <- lmer(log ~ Quantifier*Noun*Answer + (1 | Participant) + (1 | Item), data = dataoff3)

summary (model2)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Quantifier * Noun * Answer + (1 | Participant) + (1 | Item)
#   Data: dataoff3

#REML criterion at convergence: 850.2

#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-2.71583 -0.63746 -0.07437  0.57194  3.03217 

#Random effects:
# Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.088172 0.29694 
# Item        (Intercept) 0.003212 0.05668 
# Residual                0.151369 0.38906 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#                                 Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                     7.303e+00  5.889e-02  1.260e+02 124.016   <2e-16 ***
#QuantifierP                    -6.310e-02  4.871e-02  6.803e+02  -1.295   0.1956    
#NounM                          -6.840e-05  6.115e-02  6.837e+02  -0.001   0.9991    
#AnswerVolume                    1.102e-01  6.536e-02  7.193e+02   1.686   0.0923 .  
#QuantifierP:NounM               9.878e-02  7.591e-02  6.801e+02   1.301   0.1936    
#QuantifierP:AnswerVolume        1.486e-01  9.756e-02  6.909e+02   1.523   0.1282    
#NounM:AnswerVolume              1.044e-02  8.687e-02  6.908e+02   0.120   0.9044    
#QuantifierP:NounM:AnswerVolume -2.066e-01  1.358e-01  6.890e+02  -1.522   0.1285    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr) QntfrP NounM  AnswrV QnP:NM QnP:AV NnM:AV
#QuantifierP -0.502                                          
#NounM       -0.385  0.466                                   
#AnswerVolum -0.443  0.486  0.360                            
#QuntfrP:NnM  0.308 -0.633 -0.803 -0.284                     
#QntfrP:AnsV  0.243 -0.514 -0.237 -0.547  0.326              
#NnM:AnswrVl  0.296 -0.346 -0.723 -0.670  0.578  0.414       
#QntfP:NM:AV -0.176  0.367  0.459  0.400 -0.577 -0.702 -0.628

# only a marginal difference for answer volume

model3 <- lmer(log ~ Quantifier+Noun+Answer + (1 | Participant) + (1 | Item), data = dataoff3)

summary (model3)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Quantifier + Noun + Answer + (1 | Participant) + (1 | Item)
#   Data: dataoff3

#REML criterion at convergence: 842

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-2.6697 -0.6246 -0.0898  0.5664  3.1771 

#Random effects:
# Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.086272 0.29372 
# Item        (Intercept) 0.003122 0.05588 
# Residual                0.151812 0.38963 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)    7.283838   0.053499  91.210150 136.149  < 2e-16 ***
#QuantifierP   -0.006005   0.031241 690.352984  -0.192  0.84764    
#NounM          0.033378   0.029211 678.290070   1.143  0.25358    
#AnswerVolume   0.122404   0.040548 730.882065   3.019  0.00263 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr) QntfrP NounM 
#QuantifierP -0.383              
#NounM       -0.199 -0.065       
#AnswerVolum -0.334  0.385 -0.170

# significant effect for volume

anova (model2, model3)

#Data: dataoff3
#Models:
#  model3: log ~ Quantifier + Noun + Answer + (1 | Participant) + (1 | Item)
#model2: log ~ Quantifier * Noun * Answer + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model3    7 836.54 868.79 -411.27   822.54                     
#model2   11 839.68 890.36 -408.84   817.68 4.8601  4      0.302

# model 3 is better (Chisq = 4.8601, p = 0.302)


model4 <- lmer(log ~ Quantifier+Answer + (1 | Participant) + (1 | Item), data = dataoff3)

summary(model4)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Quantifier + Answer + (1 | Participant) + (1 | Item)
#   Data: dataoff3

#REML criterion at convergence: 838

#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-2.61440 -0.61929 -0.08504  0.57046  3.12851 

#Random effects:
# Groups      Name        Variance Std.Dev.
# Participant (Intercept) 0.086386 0.29391 
# Item        (Intercept) 0.003002 0.05479 
# Residual                0.151929 0.38978 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#               Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)    7.295886   0.052379  83.722033 139.291  < 2e-16 ***
#QuantifierP   -0.003631   0.031186 690.734189  -0.116  0.90735    
#AnswerVolume   0.130597   0.039960 732.174053   3.268  0.00113 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr) QntfrP
#QuantifierP -0.405       
#AnswerVolum -0.382  0.380

anova(model3,model4)

#Data: dataoff3
#Models:
#  model4: log ~ Quantifier + Answer + (1 | Participant) + (1 | Item)
#model3: log ~ Quantifier + Noun + Answer + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
#model4    6 835.85 863.49 -411.92   823.85                    
#model3    7 836.54 868.79 -411.27   822.54 1.305  1     0.2533

# model 4 is better (Chisq = 1.305, p = 0.2533)

model5 <- lmer(log ~ Noun+Answer + (1 | Participant) + (1 | Item), data = dataoff3)

summary(model5)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Noun + Answer + (1 | Participant) + (1 | Item)
#Data: dataoff3

#REML criterion at convergence: 836.9

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.6750 -0.6218 -0.0892  0.5663  3.1699 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 0.086373 0.29389 
#Item        (Intercept) 0.003106 0.05573 
#Residual                0.151596 0.38935 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)    7.27989    0.04942  67.71649 147.304  < 2e-16 ***
#  NounM          0.03301    0.02913 678.47121   1.133 0.257538    
#AnswerVolume   0.12547    0.03740 732.86495   3.355 0.000834 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) NounM 
#NounM       -0.243       
#AnswerVolum -0.219 -0.158

anova(model3, model5)

# Data: dataoff3
#Models:
#  model5: log ~ Noun + Answer + (1 | Participant) + (1 | Item)
#model3: log ~ Quantifier + Noun + Answer + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model5    6 834.58 862.22 -411.29   822.58                     
#model3    7 836.54 868.79 -411.27   822.54 0.0374  1     0.8467

#model 5 is better (Chisq = 0.034, p = 0.8467)

model6 <- lmer(log ~ Answer + (1 | Participant) + (1 | Item), data = dataoff3)

summary(model6)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log ~ Answer + (1 | Participant) + (1 | Item)
#Data: dataoff3

#REML criterion at convergence: 832.9

#Scaled residuals: 
#  Min       1Q   Median       3Q      Max 
#-2.61871 -0.62343 -0.08339  0.56715  3.12541 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 0.086447 0.29402 
#Item        (Intercept) 0.002994 0.05472 
#Residual                0.151708 0.38950 
#Number of obs: 740, groups:  Participant, 50; Item, 16

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)    7.29341    0.04788  59.66260 152.315  < 2e-16 ***
#  AnswerVolume   0.13241    0.03693 733.83259   3.585 0.000359 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#AnswerVolum -0.269

anova (model3, model6)

#Data: dataoff3
#Models:
#  model6: log ~ Answer + (1 | Participant) + (1 | Item)
#model3: log ~ Quantifier + Noun + Answer + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model6    5 833.86 856.90 -411.93   823.86                     
#model3    7 836.54 868.79 -411.27   822.54 1.3189  2     0.5171

# model 6 is better (Chisq = 1.3189, p = 0.5171)

tab_model(model6)

plot(allEffects(model6), 
     grid = TRUE, 
     main = "Values predicted by the model",
     colors = c("#FA8072", "#26BCC9")) 

model7 <- lmer(log ~ 1 + (1 | Participant) + (1 | Item), data = dataoff3)
summary (model7)

anova (model6, model7)
