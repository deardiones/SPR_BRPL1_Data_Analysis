# Brazilian Portuguese L1
# self-paced reading
# critical region: segment 5 (in log)
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

# setting the working directory

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS5 - Noun")

# bring the treated data

dataon3 <- read.csv("dataon3.csv")

# checking the structure of the data

str(dataon3)

# organizing

dataon3 <- dataon3 %>% select(-X)
dataon3$segmento <- as.numeric(dataon3$segmento)
dataon3$Tempo.leitura <- as.numeric(dataon3$Tempo.leitura)
dataon3 <- dataon3 %>% mutate_if(sapply(dataon3, is.character), as.factor)
dataon3$Item <- as.factor(dataon3$Item)
dataon3$Quantificador = relevel(dataon3$Quantificador, ref = "S")

# mixed regression linear model
# model 1 with the interaction between the quantifier and the noun

model1 <- lmer(log.tempo ~ Quantificador*Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model1)

# RESULTADO

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: log.tempo ~ Quantificador * Nome + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#327.6    360.1   -156.8    313.6      754 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.6259 -0.6003 -0.0891  0.5425  3.6449 

#Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 0.057937 0.24070 
#Item        (Intercept) 0.000521 0.02282 
#Residual                0.074190 0.27238 
#Number of obs: 761, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)            6.10275    0.03947  77.49254 154.607   <2e-16 ***
#  QuantificadorP        -0.01919    0.02803 697.51851  -0.684   0.4939    
#NomeM                 -0.05822    0.02792 695.69995  -2.085   0.0374 *  
#  QuantificadorP:NomeM   0.09886    0.03958 695.75477   2.498   0.0127 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) QntfcP NomeM 
#QuantifcdrP -0.351              
#NomeM       -0.352  0.496       
#QntfcdrP:NM  0.248 -0.708 -0.705

# model 2 without the interaction

model2 <- lmer(log.tempo ~ Quantificador + Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model2)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: log.tempo ~ Quantificador + Nome + (1 | Participant) + (1 | Item)
#   Data: dataon3

#     AIC      BIC   logLik deviance df.resid 
#   331.8    359.6   -159.9    319.8      755 

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-3.5195 -0.6229 -0.1048  0.5311  3.7128 

#Random effects:
# Groups      Name        Variance  Std.Dev.
# Participant (Intercept) 0.0579805 0.24079 
# Item        (Intercept) 0.0005231 0.02287 
# Residual                0.0748364 0.27356 
#Number of obs: 761, groups:  Participant, 51; Item, 16

#Fixed effects:
#                 Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)      6.078256   0.038282  68.422520 158.775   <2e-16 ***
#QuantificadorP   0.030375   0.019886 699.507749   1.527    0.127    
#NomeM           -0.009073   0.019890 697.422808  -0.456    0.648    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr) QntfcP
#QuantifcdrP -0.257       
#NomeM       -0.259 -0.006

# comparing between nested models to check if the interaction contributes 
# significantly to the model 

anova(model1, model2)

# Data: dataon3
# Models:
#  model2: log.tempo ~ Quantificador + Nome + (1 | Participant) + (1 | Item)
#model1: log.tempo ~ Quantificador * Nome + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
#model2    6 331.84 359.65 -159.92   319.84                       
#model1    7 327.63 360.07 -156.81   313.63 6.2116  1    0.01269 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# there were differences in the models and this difference is significant (p<0.01)
# this means that the interaction significantly contributes to the model

# model 3 simpler with only quantifier as fixed effect

model3 <- lmer(log.tempo ~ Quantificador + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model3)

# RESULTADO

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: log.tempo ~ Quantificador + (1 | Participant) + (1 | Item)
#Data: dataon3

#AIC      BIC   logLik deviance df.resid 
#330.0    353.2   -160.0    320.0      756 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.5030 -0.6283 -0.1123  0.5246  3.6969 

#Random effects:
#  Groups      Name        Variance  Std.Dev.
#Participant (Intercept) 0.0579694 0.24077 
#Item        (Intercept) 0.0005163 0.02272 
#Residual                0.0748644 0.27361 
#Number of obs: 761, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)      6.07374    0.03697  59.58739 164.282   <2e-16 ***
#  QuantificadorP   0.03032    0.01989 699.52702   1.524    0.128    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#   (Intr)
# QuantifcdrP -0.268

# comparing the nested models to verify whether noun contributes significantly to
# the model

anova(model2, model3)

# RESULTADO

# Data: dataon5
# Models:
# model3: log.tempo ~ Quantificador + (1 | Participant) + (1 | Item)
#model2: log.tempo ~ Quantificador + Nome + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
#model3    5 330.05 353.22 -160.02   320.05                     
#model2    6 331.84 359.65 -159.92   319.84 0.2079  1     0.6484

# there wasnt any difference between the models (p<0.6)
# the varibale noun does not contributes to the model

# model 4 with only the noun (without the quantifier)

model4 <- lmer(log.tempo ~ Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

summary(model4)

# Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method [
# lmerModLmerTest]
# Formula: log.tempo ~ Nome + (1 | Participant) + (1 | Item)
# Data: dataon3

# AIC      BIC   logLik deviance df.resid 
# 332.2    355.3   -161.1    322.2      756 

# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.5674 -0.6169 -0.0909  0.5518  3.7644 

# Random effects:
#  Groups      Name        Variance Std.Dev.
#Participant (Intercept) 0.05795  0.24073 
#Item        (Intercept) 0.00051  0.02258 
#Residual                0.07509  0.27403 
#Number of obs: 761, groups:  Participant, 51; Item, 16

#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)   6.093276   0.036986  59.685775 164.744   <2e-16 ***
#  NomeM        -0.008877   0.019924 697.451961  -0.446    0.656    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr)
#NomeM -0.270

# comparing nested model to verify whether the quantifier contributes significantly
# to the model

anova(model2, model4)

# RESULTADO

# Data: dataon5
# Models:
#model4: log.tempo ~ Nome + (1 | Participant) + (1 | Item)
#model2: log.tempo ~ Quantificador + Nome + (1 | Participant) + (1 | Item)
#npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)
#model4    5 332.17 355.34 -161.08   322.17                    
#model2    6 331.84 359.65 -159.92   319.84 2.329  1      0.127

# no difference as well (p<0.1)
# the quantifier does not contribute to the model
# model 1 with the interaction is the best to explain the data
# post-hoc to better understand the interaction

# post-hoc

post.hoc = emmeans(model1, ~ Quantificador*Noun)

pairs(post.hoc, adjust="tukey")

# contrast  estimate     SE  df t.ratio p.value
# S C - P C   0.0192 0.0281 701   0.683  0.9036
# S C - S M   0.0582 0.0280 699   2.081  0.1604
# S C - P M  -0.0215 0.0280 702  -0.767  0.8694
# P C - S M   0.0390 0.0282 702   1.386  0.5086
# P C - P M  -0.0406 0.0281 701  -1.444  0.4722
# S M - P M  -0.0797 0.0280 701  -2.843  0.0238

# Degrees-of-freedom method: kenward-roger 
# P value adjustment: tukey method for comparing a family of 4 estimates

pairs(post.hoc, adjust="bonferroni")

#contrast  estimate     SE  df t.ratio p.value
#S C - P C   0.0192 0.0281 701   0.683  1.0000
#S C - S M   0.0582 0.0280 699   2.081  0.2269
#S C - P M  -0.0215 0.0280 702  -0.767  1.0000
#P C - S M   0.0390 0.0282 702   1.386  0.9973
#P C - P M  -0.0406 0.0281 701  -1.444  0.8950
#S M - P M  -0.0797 0.0280 701  -2.843  0.0276

#Degrees-of-freedom method: kenward-roger 
#P value adjustment: bonferroni method for 6 tests 

# A COMPARAÇÃO PAR A PAR EVIDENCIA QUE HÁ UM EFEITO DE QUANTIFICADOR APENAS QUANDO O NOME É MASSIVO: 
# S+M vs P+M: beta= 21.73, SE = 8.88, t = 2.44, p< 0.0224.          
# O PLURAL MASSIVO É SIGNIFICATIVAMENTE MAIS LENTO DO QUE O SINGULAR MASSIVO: 491ms vs 449ms
# "(MUITAS) PRATAS" É MAIS CUSTOSO DO QUE "(MUITA) PRATA"

## COMO REPORTAR OS RESULTADOS 

# “Tomando o tempo de leitura (em log) como variável dependente, ajustamos um modelo linear misto com Quantificador,
# Nome e interação entre os dois fatores como efeitos fixos e interceptos aleatórios por participante e item. 
# Uma comparação com modelos aninhados mostrou que o melhor modelo ajustado continha a interação entre Quantificador 
# e Nome - a interação se mostrou significativa (Chisq = 8.6193, p<0.003)."

# "Análises post-hoc evidenciaram que o tipo de quantificador influencia o tempo de leitura apenas quando o nome é massivo, 
# com a condição P+M sendo significativamente mais lenta do que a condição S+M (beta= 21.73, SE = 8.88, t = 2.44, p< 0.0224)."

## PLOTAR UM GRÁFICO DOS VALORES PREVISTOS PELO MODELO

# GERAR TABELA DOS COEFICIENTES DO MODELO SIGNIFICATIVO (MODEL1)

tab_model(model1)

# plotting a graph in log

plot(allEffects(model1), 
     grid = T, 
     multiline = T, 
     main = "Values predicted by the model")

# plotting a graph in reading times

# rename the labels

dataon3 <- rename(dataon3, Quantifier = Quantificador) 
dataon3 <- rename(dataon3, Noun = Nome)
dataon3 <- rename(dataon3, Reading.time = Tempo.leitura)

modelms <- lmer(Reading.time ~ Quantifier*Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

plot(allEffects(modelms), 
     grid = T, 
     multiline = T, 
     main = "Values predicted by the model")

model1 <- lmer(log.tempo ~ Quantifier*Noun + (1|Participant) + (1|Item), data = dataon3, REML = FALSE)

# inserting the labels in the graphs

dataon3$Quantifier <- factor(dataon3$Quantifier, 
                                levels = c("S", "P"),  # Original labels
                                labels = c("Singular", "Plural"))  # New labels

dataon3$Noun <- factor(dataon3$Noun, 
                       levels = c("C", "M"),  # Original labels
                       labels = c("Count", "Mass"))  # New labels

plot(allEffects(model1), 
     grid = TRUE, 
     multiline = TRUE, 
     main = "Values Predicted by the Model", 
     colors = c("#FA8072", "#26BCC9")) 
