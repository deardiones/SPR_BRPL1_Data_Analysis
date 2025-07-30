# Brazilian Portuguese L1
# self-paced reading
# critical region: segment 5
# filtering the data

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

# install.packages("") 

# working directory

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS5 - Noun")

# "READ.PCIBEX"

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

# checking the data

View(dataon1)

# filtering the data

dataon1 <- dataon1 %>%
  filter(Label %in% "alvos",
         Parameter == "5") %>%
  select(ParticipantID, group, quantifier, noun, condition, item, Parameter, Reading.time)

# check the data

View(dataon1)

# change participant's code from F79 group D to D79 

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

# double check the data

View(dataon1)

# renamings

dataon1 <- rename(dataon1, Participant = ParticipantID) 
dataon1 <- rename(dataon1, Item = item)
dataon1 <- rename(dataon1, Quantificador = quantifier) 
dataon1 <- rename(dataon1, Nome = noun)
dataon1 <- rename(dataon1, segmento = Parameter)
dataon1 <- rename(dataon1, Tempo.leitura = Reading.time)

# recodings

dataon1$Quantificador <- recode_factor(dataon1$Quantificador, QS = "S", QP = "P")
levels(dataon1$Quantificador) # or unique(dataoff2$Quantificador)
dataon1$Nome <- recode_factor(dataon1$Nome, NC = "C", NCP = "C", NM = "M", NMP = "M")
levels(dataon1$Nome) 
dataon1$condition <- recode_factor(dataon1$condition, "QS+NC" = "S+C", "QS+NM" = "S+M",
                                    "QP+NCP" = "P+C", "QP+NMP" = "P+M")
levels(dataon1$condition)

# transforming

dataon1$segmento <- as.numeric(dataon1$segmento)
dataon1$Tempo.leitura <- as.numeric(dataon1$Tempo.leitura)
dataon1 <- dataon1 %>% mutate_if(sapply(dataon1, is.character), as.factor)
dataon1$Item <- as.factor(dataon1$Item)

# checking the structure

str(dataon1)

# general summary

summary(dataon1)

# Participant  group   Quantificador Nome    condition      Item        segmento
# A200   : 16   A:192   S:408         C:408   S+C:204   1      : 51   Min.   :5  
# C151   : 16   B:224   P:408         M:408   S+M:204   2      : 51   1st Qu.:5  
# C79    : 16   C:192                         P+C:204   3      : 51   Median :5  
# E200   : 16   D:208                         P+M:204   4      : 51   Mean   :5  
# E75    : 16                                           5      : 51   3rd Qu.:5  
# F106   : 16                                           6      : 51   Max.   :5  
# (Other):720                                           (Other):510              

# Tempo.leitura   
# Min.   : 143.0  
# 1st Qu.: 343.0  
# Median : 434.0  
# Mean   : 526.3  
# 3rd Qu.: 603.0  
# Max.   :4452.0 

# exploring reading times

summary(dataon1$Tempo.leitura) 

# Delivered values
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 143.0   343.0   434.0   526.3   603.0  4452.0  

# raw reading times mean and standard deviation (it needs to be reported)

Tempo.de.leitura = dataon1 %>%
  group_by(condition) %>% 
  summarise(Média = mean(Tempo.leitura),
            Desvio.padrão = sd(Tempo.leitura))

# raw reading time mean per variable

aggregate(Tempo.leitura ~ Quantificador, data = dataon1, mean)

#               Quantificador   Tempo.leitura
# 1             S               518.8358
# 2             P               533.8480

# raw means per participant and item

aggregate(Tempo.leitura ~ Nome, data = dataon1, mean)

#       Nome    Tempo.leitura
# 1     C       521.9020
# 2     M       530.7819

# raw means per participant

dataon1 %>%
  group_by(Participant) %>%
  summarise(Média = mean(Tempo.leitura)) %>%
  arrange(desc(Média)) # coloca em ordem decrescente

# raw means per item

dataon1 %>%
  group_by(Item) %>%
  summarise(Média = mean(Tempo.leitura)) %>%
  arrange(desc(Média))

# plotting graphs to check the general distribution of the data

# boxplot

ggplot(dataon1, aes(x = condition, y = Tempo.leitura)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + # a cruz diz onde está a média
  labs(x = "Condição", y = "Tempo de leitura") + 
  theme_classic() # aqui pode escolher várias opções

# histogram

ggplot(dataon1, aes(x = Tempo.leitura)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Tempo de leitura",
       y = "Frequência") +
  theme_classic()

# qq

ggplot(dataon1, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Tempo de leitura") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataon1, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataon1, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality tests to check if the data is normal

lillie.test(dataon1$Tempo.leitura) 

# D = 0.16288, p-value < 2.2e-16

shapiro.test(dataon1$Tempo.leitura) 

# W = 0.66418, p-value < 2.2e-16

# removing outliers

# calculating the upper limit

summary(dataon1$Tempo.leitura)  

# Min.    1st Qu.  Median     Mean    3rd Qu.     Max. 
# 143.0   343.0     434.0     526.3   603.0       4452.0

# formula: limitesuperior <- Q3 + 3*(Q3 - Q1).

limitesuperior <- 603.0 + 3*(603.0 - 343.0)

limitesuperior 

# 1383

# filtering the data that is above 1383

dataon2 <- dataon1 %>% filter(Tempo.leitura < 1383)

# check the structure and report how much data have been excluded

summary(dataon2) 

# Participant  group   Quantificador Nome    condition      Item        segmento
# A200   : 16   A:190   S:402         C:404   S+C:203   1      : 51   Min.   :5  
# C79    : 16   B:216   P:401         M:399   S+M:199   2      : 51   1st Qu.:5  
# E200   : 16   C:192                         P+C:201   4      : 51   Median :5  
# E75    : 16   D:205                         P+M:200   5      : 51   Mean   :5  
# F106   : 16                                           6      : 51   3rd Qu.:5  
# F107   : 16                                           9      : 51   Max.   :5  
# (Other):707                                           (Other):497              

# Tempo.leitura   
# Min.   : 143.0  
# 1st Qu.: 342.5  
# Median : 432.0  
# Mean   : 500.5  
# 3rd Qu.: 594.5  
# Max.   :1370.0  

# plotting the graphs and applying the normality tests with the new data set

# boxplot

ggplot(dataon2, aes(x = condition, y = Tempo.leitura)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + # a cruz diz onde está a média
  labs(x = "Condição", y = "Tempo de leitura") + 
  theme_classic() # aqui pode escolher várias opções

# histogram

ggplot(dataon2, aes(x = Tempo.leitura)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Tempo de leitura",
       y = "Frequência") +
  theme_classic()

# qq

ggplot(dataon2, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Tempo de leitura") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataon2, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataon2, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality tests to check if the data is normal

lillie.test(dataon2$Tempo.leitura) 

# D = 0.12983, p-value < 2.2e-16

shapiro.test(dataon2$Tempo.leitura) 

# W = 0.87776, p-value < 2.2e-16

# the graphs improve the distribution, but the data is not normal. new cut

# formula: limitesuperior <- Q3 + 1.5*(Q3 - Q1) 

limitesuperior2 <- 603.0 + 1.5*(603.0 - 343.0) 

limitesuperior2 

# 993

dataon3 <- dataon1 %>% filter(Tempo.leitura < 993)

# check the structure and report how much data have been excluded

summary(dataon3) 

# Participant  group   Quantificador Nome    condition      Item        segmento
# A200   : 16   A:187   S:382         C:380   S+C:192   9      : 50   Min.   :5  
# C79    : 16   B:202   P:379         M:381   S+M:190   14     : 50   1st Qu.:5  
# F156   : 16   C:176                         P+C:188   1      : 49   Median :5  
# F20    : 16   D:196                         P+M:191   4      : 49   Mean   :5  
# G77    : 16                                           10     : 49   3rd Qu.:5  
# H200   : 16                                           11     : 49   Max.   :5  
# (Other):665                                           (Other):465              

# Tempo.leitura  
# Min.   :143.0  
# 1st Qu.:340.0  
# Median :422.0  
# Mean   :464.5  
# 3rd Qu.:563.0  
# Max.   :987.0 

# plotting the graphs and applying the normality teste with the new data set

# boxplot

ggplot(dataon3, aes(x = condition, y = Tempo.leitura)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condição", y = "Tempo de leitura") + 
  theme_classic() 

# histogram

ggplot(dataon3, aes(x = Tempo.leitura)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Tempo de leitura",
       y = "Frequência") +
  theme_classic()

# qq per condition

ggplot(dataon3, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Tempo de leitura") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataon3, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataon3, aes(sample = Tempo.leitura)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality tests to check if the data is normal

lillie.test(dataon3$Tempo.leitura) 

# D = 0.10853, p-value < 2.2e-16

shapiro.test(dataon3$Tempo.leitura) 

# W = 0.93781, p-value < 2.2e-16

# reporting

# the distribution keeps improving, but the tests are still not normal

# linear model to check the residues

modelON = lm(Tempo.leitura ~ Quantificador + Nome, dataon3) 

summary(modelON) 

# Call:
# lm(formula = Tempo.leitura ~ Quantificador + Nome, data = dataon3)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -316.36 -129.78  -42.23   97.22  522.22 

# Coefficients:
#                   Estimate    Std. Error  t value   Pr(>|t|)    
#   (Intercept)     456.7809    10.8705     42.020    <2e-16 ***
#   QuantificadorP  16.1290     12.5909     1.281     0.201    
#   NomeM           -0.5542     12.5908     -0.044    0.965    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 173.7 on 758 degrees of freedom
# Multiple R-squared:  0.002162,	Adjusted R-squared:  -0.0004709 
# F-statistic: 0.8211 on 2 and 758 DF,  p-value: 0.4403

lillie.test(modelON$residuals) 

# D = 0.11175, p-value < 2.2e-16

shapiro.test(modelON$residuals) 

# W = 0.93881, p-value < 2.2e-16

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

# the graphs show that the distribution is not normal

# logarithmic transformation

dataon3$log.tempo <- log(dataon3$Tempo.leitura)

# plotting the graphs and applying the normality tests with the new data set after log

# boxplot

ggplot(dataon3, aes(x = condition, y = log.tempo)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + 
  labs(x = "Condição", y = "Tempo de leitura") + 
  theme_classic() 

# histogram

ggplot(dataon3, aes(x = log.tempo)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Tempo de leitura",
       y = "Frequência") +
  theme_classic()

# qq per condition

ggplot(dataon3, aes(sample = log.tempo)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Tempo de leitura") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataon3, aes(sample = log.tempo)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataon3, aes(sample = log.tempo)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality tests to check if the data is normal

lillie.test(dataon3$log.tempo) 

# D = 0.042609, p-value = 0.002308

shapiro.test(dataon3$log.tempo) 

# W = 0.99319, p-value = 0.001534

# the graphs show improvement in distribution and the normality tests show that 
# the data is normal

# checking the residues after log
# linear model to check the residues

# reporting the data

summary(dataon3) 
summary(dataon1)

# after log dataset 3

#Participant  group   Quantificador Nome    condition      Item        segmento Tempo.leitura  
#A200   : 16   A:187   S:382         C:380   S+C:192   9      : 50   Min.   :5   Min.   :143.0  
#C79    : 16   B:202   P:379         M:381   S+M:190   14     : 50   1st Qu.:5   1st Qu.:340.0  
#F156   : 16   C:176                         P+C:188   1      : 49   Median :5   Median :422.0  
#F20    : 16   D:196                         P+M:191   4      : 49   Mean   :5   Mean   :464.5  
#G77    : 16                                           10     : 49   3rd Qu.:5   3rd Qu.:563.0  
#H200   : 16                                           11     : 49   Max.   :5   Max.   :987.0  
#(Other):665                                           (Other):465 

# lost data sets
# quantifier  s=26 p=29 
# noun        c=28 m=27
# S+C=12 / S+M=14 / P+C=16 / P+M=13 = 55
# 1632 - 1522 = 110 6,74%

# original dataset 1

# Participant  group   Quantificador Nome    condition      Item        segmento
# A200   : 16   A:192   S:408         C:408   S+C:204   1      : 51   Min.   :5  
# C151   : 16   B:224   P:408         M:408   S+M:204   2      : 51   1st Qu.:5  
# C79    : 16   C:192                         P+C:204   3      : 51   Median :5  
# E200   : 16   D:208                         P+M:204   4      : 51   Mean   :5  
# E75    : 16                                           5      : 51   3rd Qu.:5  
# F106   : 16                                           6      : 51   Max.   :5  
# (Other):720                                           (Other):510              

# Tempo.leitura   
# Min.   : 143.0  
# 1st Qu.: 343.0  
# Median : 434.0  
# Mean   : 526.3  
# 3rd Qu.: 603.0  
# Max.   :4452.0 

aggregate(Tempo.leitura ~ Quantificador, data = dataon3, mean)

# Quantificador Tempo.leitura
#1             S      456.5052
#2             P      472.6306

aggregate(Tempo.leitura ~ Nome, data = dataon3, mean)

#Nome Tempo.leitura
#1    C      464.7605
#2    M      464.3123


modelONlog = lm(log.tempo ~ Quantificador + Nome, dataon3) 

summary(modelONlog) 

# Call:
# lm(formula = log.tempo ~ Quantificador + Nome, data = dataon3)

# Residuals:
#  Min       1Q   Median       3Q      Max 
# -1.10077 -0.25569 -0.02963  0.25365  0.82292 

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     6.063614   0.022780 266.178   <2e-16 ***
#  QuantificadorP  0.029802   0.026386   1.129    0.259    
# NomeM          -0.007343   0.026385  -0.278    0.781    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.3639 on 758 degrees of freedom
# Multiple R-squared:  0.001777,	Adjusted R-squared:  -0.0008571 
# F-statistic: 0.6746 on 2 and 758 DF,  p-value: 0.5097

lillie.test(modelONlog$residuals) 

# D = 0.042041, p-value = 0.002838

shapiro.test(modelONlog$residuals) 

# W = 0.99344, p-value = 0.002062

# residuals are normal

head(modelONlog$fitted.values)

ajustados = modelONlog$fitted.values
residuos = modelONlog$residuals

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

# the distribution of the residuals also improved after log

# reading times mean and standard deviation after treatment (it needs to be reported)

Tempo.de.leitura2 = dataon3 %>%
  group_by(condition) %>% 
  summarise(Média = mean(Tempo.leitura),
            Desvio.padrão = sd(Tempo.leitura))

# plotting the graphs with reading times od the critical segment 5 (noun)

# organizing the data 
dataon3 <- rename(dataon3, Noum = Nome) 
dataon3 <- rename(dataon3, Noun = Noum) 

grafico <- dataon3 %>%  
  group_by(Quantificador, Noun) %>%  
  summarise(media = mean(Tempo.leitura), se = sd(Tempo.leitura)/sqrt(n()))

# DEPOIS USAR O OBJETO ANTERIOR PARA PLOTAR O GRÁFICO

ggplot(grafico, aes(x = Quantificador, y = media, fill = Noun)) + 
  geom_col(alpha = 0.8, position = position_dodge(width = 0.9)) + 
  geom_errorbar(aes(ymax = media + se, ymin = media - se), 
                position = position_dodge(width = 0.9),
                width = 0.25, alpha = 0.8) + 
  ylim(0, 800) + 
  labs(x = "Quantifier", y = "Reading Time (ms)") + 
  theme_light() +
  scale_fill_manual(values = c("#FA8072", "#26BCC9"))


# save the graph

# export the treated dataset to be worked in other script

write.csv(dataon3, "~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) CS5 - Noun/dataon3.csv")

# inserting the labels in the graphs

dataon3$Quantificador <- factor(dataon3$Quantificador, 
                             levels = c("S", "P"),  # Original labels
                             labels = c("Singular", "Plural"))  # New labels

dataon3$Noun <- factor(dataon3$Noun, 
                                levels = c("C", "M"),  # Original labels
                                labels = c("Count", "Mass"))  # New labels

grafico <- dataon3 %>%  
  group_by(Quantificador, Noun) %>%  
  summarise(media = mean(Tempo.leitura), se = sd(Tempo.leitura)/sqrt(n()))

ggplot(grafico, aes(x = Quantificador, y = media, fill = Noun)) + 
  geom_col(alpha = 0.8, position = position_dodge(width = 0.9)) + 
  geom_errorbar(aes(ymax = media + se, ymin = media - se), 
                position = position_dodge(width = 0.9),
                width = 0.25, alpha = 0.8) + 
  ylim(0, 800) + 
  labs(x = "Quantifier", y = "Reading Time (ms)") + 
  theme_light() +
  scale_fill_manual(values = c("#FA8072", "#26BCC9"))

# save w: 510 r: 350