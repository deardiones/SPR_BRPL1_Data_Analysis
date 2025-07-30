# brazilian portuguese
# interpretation task
# response time & answer type
# treating the data

# independent variables
# quantifier: S (singular) e P (plural)
# noun: C (count) e M (mass)

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

# working directory

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Interpretation Task")

# load the function "READ.PCIBEX"

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

dataoff1 <- read.csv("dataoffraw.csv")

# renamings

dataoff1 <- rename(dataoff1, Participant = ParticipantID)
dataoff1 <- rename(dataoff1, Item = item)
dataoff1 <- rename(dataoff1, Quantifier = quantifier) 
dataoff1 <- rename(dataoff1, Noun = noun)
dataoff1 <- rename(dataoff1, Answer = selection) 
dataoff1 <- rename(dataoff1, Response.time = RT)

# recoding

dataoff1$Quantifier <- recode_factor(dataoff1$Quantifier, QS = "S", QP = "P")
levels(dataoff1$Quantifier) 
dataoff1$Noun <- recode_factor(dataoff1$Noun, NC = "C", NCP = "C", NM = "M", NMP = "M")
levels(dataoff1$Noun) 
dataoff1$condition <- recode_factor(dataoff1$condition, "QS+NC" = "S+C", "QS+NM" = "S+M",
                                    "QP+NCP" = "P+C", "QP+NMP" = "P+M")
levels(dataoff1$condition)

# check the structure

str(dataoff1)

# transforming

dataoff1$Response.time <- as.numeric(dataoff1$Response.time)
dataoff1 <- dataoff1 %>% mutate_if(sapply(dataoff1, is.character), as.factor)
dataoff1$Item <- as.factor(dataoff1$Item)

# check the structure

str(dataoff1)

# mutate column x

dataoff1 <- dataoff1 %>%
  select(-X, - T1, - T2)

# summary of the data

summary(dataoff1)

#Participant  group      Label          Item     condition Quantifier Noun         Answer   
# F79    : 32   A:192   alvos:816   1      : 51   S+C:204   S:408      C:408   Cardinal:531  
# A200   : 16   B:224               2      : 51   S+M:204   P:408      M:408   Volume  :285  
# C151   : 16   C:192               3      : 51   P+C:204                                    
# E200   : 16   D:208               4      : 51   P+M:204                                    
# E75    : 16                       5      : 51                                              
# F106   : 16                       6      : 51                                              
# (Other):704                       (Other):510                                              

# Response.time  
# Min.   :  435  
# 1st Qu.: 1077  
# Median : 1632  
# Mean   : 2218  
# 3rd Qu.: 2412  
# Max.   :24149 

# summary of response time

summary(dataoff1$Response.time) 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 435    1077    1632    2218    2412   24149

# reading time mean and standard deviation (important: report this raw means and sd)

Tempo.de.resposta = dataoff1 %>%
  group_by(condition) %>% 
  summarise(Média = mean(Response.time),
            Desvio.padrão = sd(Response.time))

# mean and by variable

aggregate(Response.time ~ Quantifier, data = dataoff1, mean)

#           Quantifier  Response.time
# 1          S          2386.424
# 2          P          2049.539

aggregate(Response.time ~ Noun, data = dataoff1, mean)

#         Noun    Response.time
# 1       C       2051.659
# 2       M       2384.304

# mean per participant

dataoff1 %>%
  group_by(Participant) %>%
  summarise(Média = mean(Response.time)) %>%
  arrange(desc(Média)) # decescent order

# mean per item

dataoff1 %>%
  group_by(Item) %>%
  summarise(Média = mean(Response.time)) %>%
  arrange(desc(Média))

# plotting

# boxplot

ggplot(dataoff1, aes(x = condition, y = Response.time)) +
geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + # a cruz diz onde está a média
  labs(x = "Condition", y = "Response Time") + 
  theme_classic() # aqui pode escolher várias opções

# histogram

ggplot(dataoff1, aes(x = Response.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Response Time",
       y = "Frequency") +
  theme_classic()

# qq 

ggplot(dataoff1, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Response Time") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataoff1, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataoff1, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality test

lillie.test(dataoff1$Response.time) 

# D = 0.21577, p-value < 2.2e-16

shapiro.test(dataoff1$Response.time)

# W = 0.61651, p-value < 2.2e-16

# the graphs and the normality test show that the data is not normal. 

# calculating the upper limit

summary(dataoff1$Response.time)  

#   Min.    1st Qu.   Median      Mean    3rd Qu.     Max. 
#   435    1077       1632        2218    2412      24149 

# formula: limitesuperior <- Q3 + 3*(Q3 - Q1) 

limitesuperior <- 2412 + 3*(2412 - 1077)

limitesuperior

# 6417

# filtering the data

dataoff2 <- dataoff1 %>% filter(Response.time < 6417)

summary(dataoff2)

# Participant  group     Label          Item     condition Quantifier Noun         Answer   
# F79    : 32   A:182   alvos:780   9      : 51   S+C:196   S:387      C:395   Cardinal:510  
# A200   : 16   B:212               1      : 50   S+M:191   P:393      M:385   Volume  :270  
# C151   : 16   C:184               2      : 50   P+C:199                                    
# E200   : 16   D:202               3      : 50   P+M:194                                    
# F106   : 16                       5      : 50                                              
# F156   : 16                       13     : 50                                              
# (Other):668                       (Other):479                                              

# Response.time 
# Min.   : 435  
# 1st Qu.:1054  
# Median :1572  
# Mean   :1863  
# 3rd Qu.:2305  
# Max.   :6404 

# plotting

# boxplot

ggplot(dataoff2, aes(x = condition, y = Response.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + # a cruz diz onde está a média
  labs(x = "Condition", y = "Response Time") + 
  theme_classic() # aqui pode escolher várias opções

# histogram

ggplot(dataoff2, aes(x = Response.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Response Time",
       y = "Frequency") +
  theme_classic()

# qq 

ggplot(dataoff2, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Response Time") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataoff2, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataoff2, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality test

lillie.test(dataoff2$Response.time) 

# D = 0.12335, p-value < 2.2e-16

shapiro.test(dataoff2$Response.time)

# W = 0.85578, p-value < 2.2e-16

# removing more outliers
# formula: limitesuperior <- Q3 + 1.5*(Q3 - Q1)

limitesuperior2 <- 2412 + 1.5*(2412 - 1077)

limitesuperior2

# [1] 4414.5

dataoff3 <- dataoff1 %>% filter(Response.time < 4414.5)

summary(dataoff3)

# Participant  group      Label          Item     condition Quantifier Noun         Answer   
# F79    : 32   A:174   alvos:740   2      : 50   S+C:186   S:363      C:380   Cardinal:484  
# A200   : 16   B:199               1      : 49   S+M:177   P:377      M:360   Volume  :256  
# E200   : 16   C:175               9      : 49   P+C:194                                    
# F156   : 16   D:192               13     : 49   P+M:183                                    
# G185   : 16                       3      : 48                                              
# I156   : 16                       4      : 46                                              
# (Other):628                       (Other):449                                              

# Response.time 
# Min.   : 435  
# 1st Qu.:1035  
# Median :1500  
# Mean   :1680  
# 3rd Qu.:2150  
# Max.   :4382 

# plotting

# boxplot

ggplot(dataoff3, aes(x = condition, y = Response.time)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + # a cruz diz onde está a média
  labs(x = "Condition", y = "Response Time") + 
  theme_classic() # aqui pode escolher várias opções

# histogram

ggplot(dataoff3, aes(x = Response.time)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Response Time",
       y = "Frequency") +
  theme_classic()

# qq 

ggplot(dataoff3, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Response Time") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataoff3, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataoff3, aes(sample = Response.time)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality test

lillie.test(dataoff3$Response.time) 

# D = 0.090571, p-value = 8.445e-16

shapiro.test(dataoff3$Response.time)

# W = 0.9354, p-value < 2.2e-16

# the data is still not normal according to the normality tests and there are still
# outliers on the sample

# logarithmic transformation (this should generate the dataoff4)

dataoff3$log <- log(dataoff3$Response.time)

# plotting

# boxplot

ggplot(dataoff3, aes(x = condition, y = log)) +
  geom_boxplot(fill = "lightblue") +
  stat_summary(color = "red", size = 0.2, shape = 3) + # a cruz diz onde está a média
  labs(x = "Condition", y = "Response Time") + 
  theme_classic() # aqui pode escolher várias opções

# histogram

ggplot(dataoff3, aes(x = log)) + 
  geom_histogram(bins = 30, fill = "lightblue",
                 color = "blue") +
  labs(x = "Response Time",
       y = "Frequency") +
  theme_classic()

# qq 

ggplot(dataoff3, aes(sample = log)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "", y = "Response Time") +
  facet_wrap(~condition) +
  theme_light()

# qq per participant

ggplot(dataoff3, aes(sample = log)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Participant) +
  labs(x = "", y = "") +
  theme_light()

# qq per item

ggplot(dataoff3, aes(sample = log)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~Item) +
  labs(x = "", y = "") +
  theme_light()

# normality test

lillie.test(dataoff3$log) 

# D = 0.031326, p-value = 0.08212

shapiro.test(dataoff3$log)

# W = 0.98988, p-value = 5.62e-05

# after log, the distribution of outliers improve and smirnov normality tests show
# that the data follows a normal distribution.

# new means with after the log

Tempo.de.resposta = dataoff3 %>%
  group_by(condition) %>% 
  summarise(Média = mean(Response.time),
            Desvio.padrão = sd(Response.time))

# summary of the data after log

summary(dataoff3)

# Participant  group      Label          Item     condition Quantifier Noun         Answer   
# F79    : 32   A:174   alvos:740   2      : 50   S+C:186   S:363      C:380   Cardinal:484  
# A200   : 16   B:199               1      : 49   S+M:177   P:377      M:360   Volume  :256  
# E200   : 16   C:175               9      : 49   P+C:194                                    
# F156   : 16   D:192               13     : 49   P+M:183                                    
# G185   : 16                       3      : 48                                              
# I156   : 16                       4      : 46                                              
# (Other):628                       (Other):449                                              

# Response.time       log       
# Min.   : 435   Min.   :6.075  
# 1st Qu.:1035   1st Qu.:6.942  
# Median :1500   Median :7.313  
# Mean   :1680   Mean   :7.309  
# 3rd Qu.:2150   3rd Qu.:7.673  
# Max.   :4382   Max.   :8.385 

# plotting the final graphs

grafico <- dataoff3 %>%  
  group_by(Quantifier, Noun) %>%  
  summarise(media = mean(Response.time), se = sd(Response.time)/sqrt(n()))

ggplot(grafico , aes(x = Quantifier, y = media, fill = Noun)) + 
  geom_col(alpha = 0.8, position = "dodge") + 
  geom_errorbar(position = position_dodge(width = 0.9),
                aes(ymax = media + se, ymin = media - se), width = 0.25, alpha = 0.8) + ylim(0,2200) + 
  labs(x = "Quantifier", y = "Response Time (ms)") +
  theme_light() 

# checking the answer type

table <- table(dataoff3$condition, dataoff3$Answer)

table

#           Cardinal  Volume
# S+C       112       74
# S+M       68        109
# P+C       160       34
# P+M       144       39

table1 <- addmargins(table)

table1

#           Cardinal  Volume  Sum
# S+C       112       74      186
# S+M       68        109     177
# P+C       160       34      194
# P+M       144       39      183
# Sum       484       256     740

# salving the table1

write.table(table1, "~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Interpretation Task/table1answer.csv")

# percentages per answer type

table2 <- prop.table(table, 1)*100

table2

#     Cardinal    Volume
# S+C 60.21505    39.78495
# S+M 38.41808    61.58192
# P+C 82.47423    17.52577
# P+M 78.68852    21.31148

write.table(table2, "~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Interpretation Task/answer%.csv")

# inferrential chi-square

chisq.test(table2)

# data:  table2
# X-squared = 53.687, df = 3, p-value = 1.308e-11

# there is a statistical significance in the answer type

histogram(~ Answer | condition, data = dataoff3, layout=c(2,2), col = c( "lightblue", "darkblue"), 
          ylab = "% of answers", xlab = "Answer", main = "Percentage of answer per condition")

# plotting a better graph

grafico = dataoff3 %>% # %
  group_by(condition, Answer) %>% 
  count() %>% 
  group_by(condition) %>% 
  mutate(prop = n / sum(n)) %>% 
  ungroup()

grafico 

ggplot(data = grafico , aes(x = condition, y = prop, fill = Answer)) + 
  geom_col(position = position_dodge()) +
  labs(x = "Condition", y = "% of answers") + 
  scale_y_continuous(labels=scales::percent) +
  theme_light() +
  scale_fill_manual(values = c("#FA8072", "#26BCC9"))

# linear model to check the residues

modelOFF = lm(log ~ Quantifier + Noun, dataoff3)

summary(modelOFF)

# Call:
# lm(formula = log ~ Quantifier + Noun, data = dataoff3)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.26956 -0.36397  0.00041  0.36865  1.07459 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7.30942    0.03127 233.788   <2e-16 ***
#  QuantifierP -0.03500    0.03617  -0.968    0.333    
#NounM        0.03549    0.03617   0.981    0.327    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4918 on 737 degrees of freedom
#Multiple R-squared:  0.002576,	Adjusted R-squared:  -0.000131 
#F-statistic: 0.9516 on 2 and 737 DF,  p-value: 0.3866

# the model shows that neither the quantifier nor the noun significantly predicts 
# log

lillie.test(modelOFF$residuals) 

# D = 0.029569, p-value = 0.1205

shapiro.test(modelOFF$residuals)

#W = 0.98998, p-value = 6.17e-05

# the residuals are not normal

# analysing the residues

head(modelOFF$fitted.values)

ajustados = modelOFF$fitted.values
residuos = modelOFF$residuals

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

# neither the graphs nor the normality test points that the residues are normal
# the statistical analysis will be done with the data after log transformation

# saving the transformed data

write.csv(dataoff3, "~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Interpretation Task/dataoff3.csv")

# summary
summary(dataoff3)
summary(dataoff1)

# sets of data excluded per variable
# S = 45
# P = 31
# C = 28
# M = 48
408-363
# sets of data excluded per condition
# S+C = 18
# S+M = 27
# P+C = 10
# P+M = 21
15200/1632

# inserting the labels in the graphs

dataoff3$Quantifier <- factor(dataoff3$Quantifier, 
                             levels = c("S", "P"),  # Original labels
                             labels = c("Singular", "Plural"))  # New labels

dataoff3$Noun <- factor(dataoff3$Noun, 
                       levels = c("C", "M"),  # Original labels
                       labels = c("Count", "Mass"))  # New labels

# save w: 510 r: 350

grafico <- dataoff3 %>%  
  group_by(Quantifier, Noun) %>%  
  summarise(media = mean(Response.time), se = sd(Response.time)/sqrt(n()))

ggplot(grafico , aes(x = Quantifier, y = media, fill = Noun)) + 
  geom_col(alpha = 0.8, position = "dodge") + 
  geom_errorbar(position = position_dodge(width = 0.9),
                aes(ymax = media + se, ymin = media - se), width = 0.25, alpha = 0.8) + ylim(0,2200) + 
  labs(x = "Quantifier", y = "Response Time (ms)") +
  theme_light() 
