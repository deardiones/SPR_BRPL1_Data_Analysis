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

model1 <- lmer (log ~ Quantifier * Noun * Answer + (1 | Participant) + (1 | Item), data = dataoff3)
summary (model1)

model2 <- lmer (log ~ Quantifier + Noun + Answer + (1 | Participant) + (1 | Item), data = dataoff3)
summary (model2)

anova (model1, model2)

model3 <- lmer (log ~ Quantifier * Answer + (1 | Participant) + (1 | Item), data = dataoff3)
summary(model3)

anova (model2, model3)

model4 <- lmer (log ~ Quantifier + Answer + (1 | Participant) + (1 | Item), data = dataoff3)
summary(model4)

anova (model2, model4)

model5 <- lmer (log ~ Noun * Answer + (1 | Participant) + (1 | Item), data = dataoff3)
summary(model5)

anova (model2, model5)

model6 <- lmer (log ~ Noun + Answer + (1 | Participant) + (1 | Item), data = dataoff3)
summary(model6)

anova (model2, model6)

model7 <- lmer (log ~ Answer + (1 | Participant) + (1 | Item), data = dataoff3)
summary(model7)

anova (model2, model7)

post.hoc = emmeans(model1, ~ Quantifier * Noun * Answer)

pairs(post.hoc, adjust="bonferroni")

# P C Cardinal - P C Volume   -2.59e-01 0.0828 715  -3.126  0.0515
