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

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS4 - Quantifier")

# load function "READ.PCIBEX"

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

# clean up the table - isolate the targets and the critical segment 4

dataon1 <- dataon1 %>%
  filter(Label %in% "alvos",
         Parameter == "4") %>%
  select(ParticipantID, group, quantifier, noun, condition, item, Parameter, Reading.time)

# check the data

View(dataon1)

# change participant's number F79 from group D to D79 
# (pcibex internal error generated the same number to two different participants)

dataon1$'ParticipantID'[1:16] <- sub("F79", "C79", dataon1$'ParticipantID'[1:16])

# removing participants that were not brpL1 according to the questionnaire
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

# Participant  group   Quantifier  Noun    condition      Item        segmento
# A200   : 16   A:192   S:408         C:408   S+C:204   1      : 51   Min.   :4  
# C151   : 16   B:224   P:408         M:408   S+M:204   2      : 51   1st Qu.:4  
# C79    : 16   C:192                         P+C:204   3      : 51   Median :4  
# E200   : 16   D:208                         P+M:204   4      : 51   Mean   :4  
# E75    : 16                                           5      : 51   3rd Qu.:4  
# F106   : 16                                           6      : 51   Max.   :4  
# (Other):720                                           (Other):510              

# Reading.time  
# Min.   : 131.0  
# 1st Qu.: 346.0  
# Median : 428.5  
# Mean   : 476.6  
# 3rd Qu.: 551.0  
# Max.   :3971.0 

# explore reading time for the segment 4, the quantifier

summary(dataon1$Reading.time)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#131.0   346.0   428.5   476.6   551.0  3971.0

# reading time mean and standard deviation (report on presentations)

Reading.time = dataon1 %>%
  group_by(condition) %>% 
  summarise(Mean = mean(Reading.time),
            Standard.deviation = sd(Reading.time))

# reading time mean for each variable, in this case the quantifier

aggregate(Reading.time ~ Quantifier, data = dataon1, mean)

#           Quantifier    Reading.time
#1          S             464.3578
#2          P             488.8039

# reading time mean for each variable, in this case the noun

aggregate(Reading.time ~ Noun, data = dataon1, mean)

#       Noun  Reading.time
# 1     C     474.0564
# 2     M     479.1054

# reading time mean for participant

dataon1 %>%
  group_by(Participant) %>%
  summarise(Mean = mean(Reading.time)) %>%
  arrange(desc(Mean)) 

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

# the visual inspection allow us to observe that there are outliers in the sample, 
# affecting the distribution of the data. In this sense, we will conduct a 
# normality tasks

# normality tasks
# linear mixed models usually assume that the residues follow a normal distribution
# which is easier when the data, in general, follow a normal distribution.
# we have access to the residues only after adjusting them. For this, we will 
# use two normality tasks: Kolmogorov-Smirnov: lillie.test() and 
# Shapiro-Wilk: shapiro.test()

# the distribution is normal when the p value of the test is bigger than 0.05

# normality task

lillie.test(dataon1$Reading.time)

#D = 0.15006, p-value < 2.2e-16

shapiro.test(dataon1$Reading.time)

# W = 0.69743, p-value < 2.2e-16

# the data is not normal since the p value is way to above 0.05. In this sense,
# we should remove outliers that are data that are way too different from the 
# other data from the sample. They are numbers way too big or way too low that 
# show up out of the boxplots and give to histogram a tail, out of the upper 
# or lower limit

# one way to determine the values to cut the outliers is delete values that 
# exceed the upper limit or lower limit from the boxplot, calculating the 
# parameters of 1.5 to 3 times the interquartile range 
# (range 1.5 ou range 3)

# calculate the upper limit (boxplot)
# cut criteria

summary(dataon1$Reading.time)

#Min.     1st Qu.  Median    Mean     3rd Qu.     Max. 
#131.0    346.0     428.5     476.6   551.0       3971.0 

# calculate range 3 which excludes less data
# exclude less outliers, including more data. includes two deviations.

# formula: limitesuperior <- Q3 + 3*(Q3 - Q1). 
# take the values from the previous summary: Q1 é o "1st Qu." e Q3 é o "3rd Qu."

limitesuperior <- 551.0 + 3*(551.0 - 346.0)

limitesuperior

# 1166

# filter the data, excluding values above 1166

dataon2 <- dataon1 %>% filter(Reading.time < 1166)

# check the structure of the data and compare it with the previous one 
# (check how much data has been excluded from dataon1, comparing dataon2)

summary(dataon2)

# results from dataon2

# Participant  group   Quantifier Noun    condition      Item        Segment 
# A200   : 16   A:188   S:416      C:415   S+C:209   7      : 53   Min.   :4  
# E200   : 16   B:219   P:412      M:413   S+M:207   11     : 53   1st Qu.:4  
# E75    : 16   C:203                      P+C:206   15     : 53   Median :4  
# F156   : 16   D:218                      P+M:206   16     : 53   Mean   :4  
# F160   : 16                                        3      : 52   3rd Qu.:4  
#F20    : 16                                        4      : 52   Max.   :4  
# (Other):732                                        (Other):512              

# Reading.time   
# Min.   : 131.0  
# 1st Qu.: 350.0  
# Median : 430.5  
# Mean   : 457.2  
# 3rd Qu.: 547.2  
# Max.   :1170.0  

# previous results from dataon1

#   Participant  group   Quantifier Noun    condition      Item        segment 
# A200   : 16   A:188   S:400      C:399   S+C:201   7      : 51   Min.   :4  
# E200   : 16   B:219   P:397      M:398   S+M:199   11     : 51   1st Qu.:4  
# E75    : 16   C:187                      P+C:198   15     : 51   Median :4  
# F156   : 16   D:203                      P+M:199   16     : 51   Mean   :4  
# F160   : 16                                        2      : 50   3rd Qu.:4  
# F20    : 16                                        3      : 50   Max.   :4  
# (Other):701                                        (Other):493              

# Reading.time   
# Min.   : 131.0  
# 1st Qu.: 343.0  
# Median : 425.0  
# Mean   : 450.4  
#3rd Qu.: 533.0  
# Max.   :1133.0  

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

# nomality task on the new data set (dataon2)

# kolmogorov-smirnov dataon2

lillie.test(dataon2$Reading.time)

# D = 0.084279, p-value = 1.178e-14

shapiro.test(dataon2$Reading.time)

# W = 0.95707, p-value = 1.655e-14

# the data show signs of improvement, but it is still not normal.
# the graphs still display outliers in the sample and the tests are not normal.

# calculate rage 1.5 which is less conservative, excluding more data

# formula: limitesuperior <- Q3 + 1.5*(Q3 - Q1) 

limitesuperior2 <- 551.0 + 1.5*(551.0 - 346.0)

limitesuperior2

# 858.5

# filter the data excluding  data that is above 858.5

dataon3 <- dataon1 %>% filter(Reading.time < 858.5)

# check the structure of the data to compare it with the previous one (check how much data has been deleted)

summary(dataon3)

# results from dataon3

# Participant  group   Quantifier Noun    condition      Item        Segment   Reading.time  
# A200   : 16 A:187   S:392      C:392   S+C:197   7      : 51   Min.   :4   Min.   :131.0  
# E200   : 16   B:214   P:389      M:389   S+M:195   3      : 50   1st Qu.:4   1st Qu.:342.0  
# F156   : 16   C:182                      P+C:195   8      : 50   Median :4   Median :421.0  
# F20    : 16   D:198                      P+M:194   9      : 50   Mean   :4   Mean   :440.2  
# G185   : 16                                        10     : 50   3rd Qu.:4   3rd Qu.:522.0  
# H200   : 16                                        12     : 50   Max.   :4   Max.   :846.0  
# (Other):685                                        (Other):480     

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

# D = 0.069748, p-value = 1.263e-09

shapiro.test(dataon3$Reading.time)

# W = 0.97512, p-value = 2.875e-10

# the data is still not normal, but it is less abnormal
# linear model to check the normality of the residues (dataon3)

modelON = lm(Reading.time ~ Quantifier + Noun, dataon3) # adjust the model

summary(modelON) # see the results from the model

# Call:
#  lm(formula = Reading.time ~ Quantifier + Noun, data = dataon3)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-316.36  -95.36  -17.21   80.82  412.82 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  433.184      9.008  48.087   <2e-16 ***
#  QuantifierP   11.030     10.433   1.057    0.291    
#NounM          3.148     10.433   0.302    0.763    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 145.8 on 778 degrees of freedom
#Multiple R-squared:  0.001552,	Adjusted R-squared:  -0.001014 
#F-statistic: 0.6048 on 2 and 778 DF,  p-value: 0.5464

lillie.test(modelON$residuals)

#D = 0.06599, p-value = 1.454e-08

shapiro.test(modelON$residuals)

#W = 0.97538, p-value = 3.381e-10

# residuals are not normal

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

# the graphs and the tests are still not normal.
# check if the distribution of the data and the residuals become better with the 
# logarithmic transformation to see if the distribution improves.

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

# D = 0.048205, p-value = 0.0001929

shapiro.test(dataon3$log.time)

# W = 0.98673, p-value = 1.603e-06

# based on the new graphs after log, the distribution did not improve. the set of 
# of dataon3 before the logarithmic transformation is better than after the
# transformation. Only for smirnof normality test the data appers to be normal.
# Comparing with the previous critical segment 5 (noun) from other
# script, the data seems to be way better as presented by the dataon3 here. Thus,
# it is similar to the data set from the post critical segment 6.
# the data behavior from segment 4 and 6 seems to be similar
# In this way, I decided to work with the set of dataon3 before 
# the logarithmic transformation. In this way, I will exclude the log column 

# excluding log column

dataon3 <- dataon3 %>% select(- log.time) #this one excludes the log column 

# after checking the normality test with the residues and seeing that they are 
# not normal, calculate the lower limit.

summary(dataon3$Reading.time)

# result

# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 131.0   342.0   421.0   440.2   522.0   846.0 

# retaking the final results

Reading.time = dataon3 %>%
  group_by(condition) %>% 
  summarise(Mean = mean(Reading.time),
            Standard.deviation = sd(Reading.time))

aggregate(Reading.time ~ Quantifier, data = dataon3, mean)

aggregate(Reading.time ~ Noun, data = dataon3, mean)

summary(dataon3)
summary(dataon1)
# conclusions after inspecting the data

# two cuts were made on the data: 
# in dataon3  sets of data from  were considered outliers and have been 
# excluded (less % of data)

# sets of data excluded per variable: 
# singular quantifier: 16  less sets of data
# plural quantifier:  19 less sets of data
# count noun:  26 less sets of data
# mass noun: 19 less sets of data

# sets of data excluded per condition
# S+C = 7
# S+M = 9
# P+C = 9
# P+M = 10
7000/1632
# keep working with dataon3 seems to be the best choice in order to preserve 
# as much data as possible, even though the data were still not normal and neither 
# the residuals. The data after the logarithmic transformation did
# not seem to improve by visual inspection on the boxplot and histogram.

# final graphs for reporting

# generating the graph related to reading time mean and standard deviation

Reading.time = dataon3 %>%
  group_by(condition) %>% 
  summarise(Média = mean(Reading.time),
            Desvio.padrão = sd(Reading.time))

# organizing the data to plot the graph regarding the critical segment reading time

grafico <- dataon3 %>%  
  group_by(Quantifier, Noun) %>%  
  summarise(media = mean(Reading.time), se = sd(Reading.time)/sqrt(n()))

# using the previous object to plot the graph

ggplot(grafico, aes(x = Quantifier, y = media, fill = Noun)) + 
  geom_col(alpha = 0.8, position = "dodge") + 
  geom_errorbar(position = position_dodge(width = 0.9),
                aes(ymax = media + se, ymin = media - se), 
                width = 0.25, alpha = 0.8) + 
  ylim(0, 800) + 
  labs(x = "Quantifier", y = "Reading Time (ms)") + 
  theme_light() +
  scale_fill_manual(values = c("#FA8072", "#26BCC9", "#006600"))  # Use scale_fill_manual for colors

# salving the graph as an image 

# export dataon3 to work on other script 

write.csv(dataon3, "~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS4 - Quantifier/dataon3.csv")

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
