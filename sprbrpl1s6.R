# Brazilian Portuguese L1
# self-paced reading
# critical region: segment 4
# data treatment

# independent variables
# quantifier: s (singular) and p (plural)
# noun: c (count) and m (mass)

# experimental conditions
# singular quantifier + count noun: s + c (much feather)
# singular quantifier + mass noun:  s + m (much silver)
# plural quantifier + count noun:   p + c (many feathers)
# plural quantifier + mass noun:    p + m (many silvers)

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

# in case it did not open, instal the function: install.packages("") 

# folder path

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS6 - Post Critical")

## load function "READ.PCIBEX"

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

# read the data

dataon1 <- read.pcibex("results.csv")

# check the data

View(dataon1)

# clean up the table - isolate the targets and the post critical segment

dataon1 <- dataon1 %>%
  filter(Label %in% "alvos",
         Parameter == "6") %>%
  select(ParticipantID, group, quantifier, noun, condition, item, Parameter, Reading.time)

# check the data

View(dataon1)

# change participant's number F79 from group D to D79 
# (pcibex internal error generated the same number to two different participants)

dataon1$'ParticipantID'[1:16] <- sub("F79", "C79", dataon1$'ParticipantID'[1:16])

# removing participants there were not brpl1 according to the questionnaire
# H190
# N69

# selecting the participants number H190

participant_to_remove <- "ParticipantID_H190"

# removing the participant

dataon1 <- dataon1[dataon1$ParticipantID != "H190", ]

# selecting the participants number H190

participant_to_remove <- "ParticipantID_N69"

# removing the participant

dataon1 <- dataon1[dataon1$ParticipantID != "N69", ]

# check the data

View(dataon1)

# renamings

dataon1 <- rename(dataon1, Participant = ParticipantID) 
dataon1 <- rename(dataon1, Item = item)
dataon1 <- rename(dataon1, Quantifier = quantifier) 
dataon1 <- rename(dataon1, Noun = noun)
dataon1 <- rename(dataon1, segment = Parameter)
dataon1 <- rename(dataon1, Reading.time = Reading.time)

# recodings

dataon1$Quantifier <- recode_factor(dataon1$Quantifier, QS = "S", QP = "P")
levels(dataon1$Quantifier) # or unique(dataoff2$Quantificador)
dataon1$Noun <- recode_factor(dataon1$Noun, NC = "C", NCP = "C", NM = "M", NMP = "M")
levels(dataon1$Noun) 
dataon1$condition <- recode_factor(dataon1$condition, "QS+NC" = "S+C", "QS+NM" = "S+M",
                                   "QP+NCP" = "P+C", "QP+NMP" = "P+M")
levels(dataon1$condition)

# transforming

dataon1$segment <- as.numeric(dataon1$segment)
dataon1$Reading.time <- as.numeric(dataon1$Reading.time)
dataon1 <- dataon1 %>% mutate_if(sapply(dataon1, is.character), as.factor)
dataon1$Item <- as.factor(dataon1$Item)

# checking the structure

str(dataon1)

# general summary

summary(dataon1)

# Participant  group   Quantifier Noun    condition      Item        segment   Reading.time   
#A200   : 16   A:192   S:408      C:408   S+C:204   1      : 51   Min.   :6   Min.   : 170.0  
#C151   : 16   B:224   P:408      M:408   S+M:204   2      : 51   1st Qu.:6   1st Qu.: 376.0  
#C79    : 16   C:192                      P+C:204   3      : 51   Median :6   Median : 452.0  
#E200   : 16   D:208                      P+M:204   4      : 51   Mean   :6   Mean   : 496.3  
#E75    : 16                                        5      : 51   3rd Qu.:6   3rd Qu.: 556.5  
#F106   : 16                                        6      : 51   Max.   :6   Max.   :2342.0  
#(Other):720                                        (Other):510

# explore reading time for the segment 6, post critical

summary(dataon1$Reading.time)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#170.0   376.0   452.0   496.3   556.5  2342.0 

# reading time mean and standard deviation (report on presentations)

Reading.time = dataon1 %>%
  group_by(condition) %>% 
  summarise(Mean = mean(Reading.time),
            Standard.deviation = sd(Reading.time))

# reading time mean for each variable, in this case the quantifier

aggregate(Reading.time ~ Quantifier, data = dataon1, mean)

#            Quantifier           Reading.time
# 1          Singular             490.4975
# 2          Plural               502.0196

# reading time mean for each variable, in this case the noun

aggregate(Reading.time ~ Noun, data = dataon1, mean)

#               Noun            Reading.time
# 1             Count           495.6936
# 2             Mass            496.8235

# reading time mean per participant 

dataon1 %>%
  group_by(Participant) %>%
  summarise(Mean = mean(Reading.time)) %>%
  arrange(desc(Mean)) # organize in decrescent order

# reading time mean per item

dataon1 %>%
  group_by(Item) %>%
  summarise(Mean = mean(Reading.time)) %>%
  arrange(desc(Mean))

# graphs: boxplot, histogram, and qq
# by checking these graphs we can observe the general distribution of the data
# by looking at them, we can observe if they follow a normal distribution or not
# by visual inspection

# boxplot

ggplot(dataon1, aes(x = condition, y = Reading.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + # the cross indicates where is the mean
  labs(x = "Condition", y = "Reading Time") + 
  theme_classic() # here you can change an array of options

# histogram

ggplot(dataon1, aes(x = Reading.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Reading Time",
       y = "Frequency") +
  theme_classic()

# qq

ggplot(dataon1, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Reading Time") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataon1, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataon1, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality task

lillie.test(dataon1$Reading.time)

# D = 0.15161, p-value < 2.2e-16

shapiro.test(dataon1$Reading.time)

# W = 0.76129, p-value < 2.2e-16

summary(dataon1$Reading.time)  

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 170.0   376.0   452.0   496.3   556.5  2342.0 

# formula: limitesuperior <- Q3 + 3*(Q3 - Q1). 

limitesuperior <- 556.5 + 3*(556.5 - 376.0)

limitesuperior

# 1098

# filter the data, excluding values above 1098

dataon2 <- dataon1 %>% filter(Reading.time < 1098)

# check the structure of the data and compare it with the previous one 
# (check how much data has been excluded from dataon1, comparing dataon2)

summary(dataon2)

#Participant  group   Quantifier Noun    condition      Item        segment   Reading.time   
#A200   : 16   A:187   S:399      C:400   S+C:199   1      : 51   Min.   :6   Min.   : 170.0  
#C151   : 16   B:214   P:400      M:399   S+M:200   9      : 51   1st Qu.:6   1st Qu.: 375.0  
#C79    : 16   C:191                      P+C:201   15     : 51   Median :6   Median : 450.0  
#E200   : 16   D:207                      P+M:199   2      : 50   Mean   :6   Mean   : 474.8  
#E75    : 16                                        5      : 50   3rd Qu.:6   3rd Qu.: 545.0  
#F106   : 16                                        6      : 50   Max.   :6   Max.   :1095.0  
#(Other):703                                        (Other):496 

# how many sets of data were considered outliers? 
# how many sets of data have been excluded by variable? 
# which condition has lost less data? 
# which condition has lost more data? 

# plot the graphs again with the new data set (dataon2)

# boxplot (dataon2)

ggplot(dataon2, aes(x = condition, y = Reading.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condition", y = "Reading Time") + 
  theme_classic()

# histogram (dataon2)

ggplot(dataon2, aes(x = Reading.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Reading Time",
       y = "Frequency") +
  theme_classic()

# qq (dataon2)

ggplot(dataon2, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Reading Time") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant (dataon2)

ggplot(dataon2, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item (dataon2)

ggplot(dataon2, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# nomality test

lillie.test(dataon2$Reading.time)

# D = 0.096934, p-value < 2.2e-16

shapiro.test(dataon2$Reading.time)

# W = 0.93348, p-value < 2.2e-16

# formula: limitesuperior <- Q3 + 1.5*(Q3 - Q1) 

limitesuperior2 <- 556.5 + 1.5*(556.5 - 376.0)

limitesuperior2

# 827.25

#  filter the data excluding  data that is above 827.25

dataon3 <- dataon1 %>% filter(Reading.time < 827.25)

# check the structure of the data to compare it with the previous one (check how much data has been deleted)

summary(dataon3)

# results from dataon3

# Participant  group   Quantifier Noun    condition      Item        segment   Reading.time  
# A200   : 16   A:183   S:387      C:387   S+C:192   1      : 51   Min.   :6   Min.   :170.0  
# E200   : 16   B:207   P:381      M:381   S+M:195   15     : 50   1st Qu.:6   1st Qu.:372.0  
# F106   : 16   C:181                      P+C:195   3      : 49   Median :6   Median :445.0  
# F156   : 16   D:197                      P+M:186   6      : 49   Mean   :6   Mean   :456.8  
# F20    : 16                                        12     : 49   3rd Qu.:6   3rd Qu.:531.2  
# F79    : 16                                        13     : 49   Max.   :6   Max.   :825.0  
# (Other):672                                        (Other):471

# how many sets of data were considered outliers? 
# how many sets of data have been excluded by variable? 
# which condition has lost less data? 
# which condition has lost more data? 

# plot the graphs with the new data set (dataon3)

# boxplot (dataon3)

ggplot(dataon3, aes(x = condition, y = Reading.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condition", y = "Reading Time") + 
  theme_classic() 

# histogram (dataon3)

ggplot(dataon3, aes(x = Reading.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Reading Time",
       y = "Frequency") +
  theme_classic()

# qq (dataon3)

ggplot(dataon3, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Reading Time") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant (dataon3)

ggplot(dataon3, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item (dataon3)

ggplot(dataon3, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality tests (dataon3)

lillie.test(dataon3$Reading.time)

# D = 0.060988, p-value = 3.95e-07

shapiro.test(dataon3$Reading.time)

# W = 0.97736, p-value = 1.575e-09

# linear model to check the normality of the residues (dataon3)

modelON = lm(Reading.time ~ Quantifier + Noun, dataon3) # adjust the model

summary(modelON) # see the results from the model

# Call:
#lm(formula = Reading.time ~ Quantifier + Noun, data = dataon3)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-285.03  -83.58  -11.90   72.67  373.22 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  455.030      7.924  57.421   <2e-16 ***
#  QuantifierP    7.745      9.151   0.846    0.398    
#NounM         -4.255      9.151  -0.465    0.642    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 126.8 on 765 degrees of freedom
#Multiple R-squared:  0.001234,	Adjusted R-squared:  -0.001377 
#F-statistic: 0.4725 on 2 and 765 DF,  p-value: 0.6236

lillie.test(modelON$residuals)

# D = 0.059172, p-value = 1.084e-06

shapiro.test(modelON$residuals)

# W = 0.9773, p-value = 1.508e-09

# analyzing the residuals

head(modelON$fitted.values)

ajustados = modelON$fitted.values
residuos = modelON$residuals

aj.residuos = data.frame(ajustados, residuos)

ggplot(aj.residuos, aes(y=residuos)) +
  geom_boxplot()

ggplot(aj.residuos, aes(x = residuos)) +
  geom_histogram()

ggplot(aj.residuos, aes(sample=residuos)) +
  stat_qq() +
  stat_qq_line()

ggplot(aj.residuos, aes(x = ajustados, y = residuos)) +
  geom_point(size=2)

# logarithmic transformation

dataon3$log.time <- log(dataon3$Reading.time)

# plotting the graphs immediatly after logarithmic transformation

# boxplot (dataon3) after log

ggplot(dataon3, aes(x = condition, y = log.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condition", y = "Reading Time") + 
  theme_classic()

# histogram (dataon3)

ggplot(dataon3, aes(x = log.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Reading Time",
       y = "Frequency") +
  theme_classic()

# qq (dataon3)

ggplot(dataon3, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Reading Time") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant (dataon3)

ggplot(dataon3, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item (dataon3)

ggplot(dataon3, aes(sample = Reading.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

lillie.test(dataon3$log.time)

# D = 0.03537, p-value = 0.02378

shapiro.test(dataon3$log.time)

# W = 0.99008, p-value = 4.766e-05

# based of boxplot and histogram, the distribution did not improve. the set of 
# of dataon3 before the logarithmic transformation is better than after the
# transformation. In this way, I decided to work with the set of dataon3 before 
# the logarithmic transformation. In this way, I will exclude the log column 

# excluding log column

dataon3 <- dataon3 %>% select(- log.time) #this one excludes the log column 

# after checking the normality test with the residues and seeing that they are 
# not normal, calculate the lower limit.

summary(dataon3$Reading.time)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 170.0   372.0   445.0   456.8   531.2   825.0

# conclusions after inspecting the data:
# two cuts were made on the data: (- observations)
# in dataon3  sets of data from  were considered outliers and have been 
# excluded (less % of data)

# sets of data excluded per variable: 
# singular quantifier:  less sets of data
# plural quantifier:  less sets of data
# count noun:  less sets of data
# mass noun:  less sets of data

# the condition that has lost less data: 
# quantifier +  noun: ( remaining sets of data)
# the condition that has lost more data: 
# quantifier +  noun: ( remaining sets of data)

# keep working with dataon3 seems to be the best choice in order to preserve 
# as much data as possible, even though the data were still not normal and neither 
# the residuals. The data after the logarithmic transformation did
# not seem to improve by visual inspection on the boxplot and histogram.

# what it is missing: 
# verify whether the difference between each variable and condition is significant.
# bonferroni correction
# select one sentence to see the means of each word segment (select the scrit of
# the noun region only)

# final graphs for reporting

# generating the graph related to reading time mean and standard deviation

Reading.time2 = dataon3 %>%
  group_by(condition) %>% 
  summarise(Média = mean(Reading.time),
            Desvio.padrão = sd(Reading.time))

# organizing the data to plot the graph regarding the critical segment reading time

grafico <- dataon3 %>%  
  group_by(Quantifier, Noun) %>%  
  summarise(media = mean(Reading.time), se = sd(Reading.time)/sqrt(n()))

# using the previous object to plot the graph

ggplot(grafico , aes(x = Quantifier, y = media, fill = Noun)) + 
  geom_col(alpha = 0.8, position = "dodge") + 
  geom_errorbar(position = position_dodge(width = 0.9),
                aes(ymax = media + se, ymin = media - se), width = 0.25, alpha = 0.8) + ylim(0,800) + 
  labs(x = "Quantifier", y = "Reading Time (ms)") +
  theme_light()

# salving the graph as an image 

# export dataon3 to work on other script 

write.csv(dataon3, "~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS6 - Post Critical/dataon3.csv")

# additional inspection

# report the treated means and st
Reading.time = dataon3 %>%
  group_by(condition) %>% 
  summarise(Mean = mean(Reading.time),
            Standard.deviation = sd(Reading.time))
# report the means for each variable after treatment
aggregate(Reading.time ~ Quantifier, data = dataon3, mean)
aggregate(Reading.time ~ Noun, data = dataon3, mean)
# summary
summary(dataon3)
summary(dataon1)

# sets of data excluded per variable
# S = 21
# P = 27
# C = 21
# M = 27
9600/1632
# sets of data excluded per condition
# S+C = 12
# S+M = 9
# P+C = 9
# P+M = 18

# inserting the labels in the graphs

dataon3$Quantifier <- factor(dataon3$Quantifier, 
                             levels = c("S", "P"),  # Original labels
                             labels = c("Singular", "Plural"))  # New labels

dataon3$Noun <- factor(dataon3$Noun, 
                       levels = c("C", "M"),  # Original labels
                       labels = c("Count", "Mass"))  # New labels

# save w: 510 r: 350

grafico <- dataon3 %>%  
  group_by(Quantifier, Noun) %>%  
  summarise(media = mean(Reading.time), se = sd(Reading.time)/sqrt(n()))

ggplot(grafico, aes(x = Quantifier, y = media, fill = Noun)) + 
  geom_col(alpha = 0.8, position = position_dodge(width = 0.9)) + 
  geom_errorbar(aes(ymax = media + se, ymin = media - se), 
                position = position_dodge(width = 0.9),
                width = 0.25, alpha = 0.8) + 
  ylim(0, 800) + 
  labs(x = "Quantifier", y = "Reading Time (ms)") + 
  theme_light() +
  scale_fill_manual(values = c("#FA8072", "#26BCC9"))
