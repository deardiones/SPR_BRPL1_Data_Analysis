# whole sentence processing BrP L1

# online data

# independent variables
# quantifier: s (singular) and p (plural)
# noun: c (count) and m (mass)

# experimental conditions
# singular quantifier + count noun: s + c (muita boina)
# singular quantifier + mass noun:  s + m (muita prata)
# plural quantifier + count noun:   p + c (muitas boinas)
# plural quantifier + mass noun:    p + m (muitas pratas)

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

# if it does not open, install.packages("") 

# working directory

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Whole Sentence Processing")

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

dataon1 <- read.pcibex("results.csv")

# check the data

View(dataon1)

# removing participants there were not brpl1 according to the questionnaire
# H190
# N69

# selecting the participants number H190

participant_to_remove <- "ParticipantID_H190"
dataon1 <- dataon1[dataon1$ParticipantID != "H190", ]
participant_to_remove <- "ParticipantID_N69"
dataon1 <- dataon1[dataon1$ParticipantID != "N69", ]

# double check the data

View(dataon1)

# clean up the table (isolate the targets and the critical segment)

dataon1 <- dataon1 %>%
  filter(Label == "alvos",
         Parameter %in% c("1", "2", "3", "4", "5", "6", "7")) %>%
  select(ParticipantID, group, quantifier, noun, condition, item, Parameter, Reading.time)

# check the data

View(dataon1)

# change participant's coding F79 from group D to D79 (coding error in the pcibex platform)

dataon1$'ParticipantID'[1:16] <- sub("F79", "C79", dataon1$'ParticipantID'[1:16])

View(dataon1)

# rename column "PARTICIPANTID" to "Participant" and the column "item" to "Item" 

dataon1 <- rename(dataon1, Participant = ParticipantID) # first the new name
dataon1 <- rename(dataon1, Item = item)

# renaming the column "quantifier" to "Quantifier"

dataon1 <- rename(dataon1, Quantifier = quantifier) 

# renaming the column "noun" to "Noun"

dataon1 <- rename(dataon1, Noun = noun)

# renaming the column "parameter" to "Segment"

dataon1 <- rename(dataon1, Segment = Parameter)

# renaming the column "Reading.time" to "Reading.time"

dataon1 <- rename(dataon1, Reading.time = Reading.time)

# check the data

View(dataon1)

# recode "QP" to "P" and "QS" to "S" 

dataon1$Quantifier <- recode_factor(dataon1$Quantifier, QS = "S", QP = "P")

# check recoding

levels(dataon1$Quantifier) # or unique(dataoff2$Quantificador)

# recode "NC" and "NCP" to "C", and "NM" and "NMP" to "M"

dataon1$Noun <- recode_factor(dataon1$Noun, NC = "C", NCP = "C", NM = "M", NMP = "M")

# check recoding

levels(dataon1$Noun)

# recode "QP+NCP" to "P+C", "QP+NMP" to "P+M", "QS+NC" to "S+C", "QS+NM" to "S+M"

dataon1$condition <- recode_factor(dataon1$condition, "QS+NC" = "S+C", "QS+NM" = "S+M",
                                   "QP+NCP" = "P+C", "QP+NMP" = "P+M")

# check recoding

levels(dataon1$condition)

# check all data columns

str(dataon1)

# transform the columns "Segment" and "Reading.time" to numeric

dataon1$Segment <- as.numeric(dataon1$Segment)
dataon1$Reading.time <- as.numeric(dataon1$Reading.time)

# transform the categorical variables in factors  so that the characters 
# sequence be read as variable levels

dataon1 <- dataon1 %>% mutate_if(sapply(dataon1, is.character), as.factor)

# transform the item column into factor

dataon1$Item <- as.factor(dataon1$Item)

# check structure of the data

str(dataon1)

# general sum of the data

summary(dataon1)

# results

#Participant   group    Quantifier Noun     condition       Item         Segment   Reading.time   
#F79    : 208   A:1344   S:2856     C:2856   S+C:1428   1      : 357   Min.   :1   Min.   :  83.0  
#A200   : 112   B:1568   P:2856     M:2856   S+M:1428   2      : 357   1st Qu.:2   1st Qu.: 350.8  
#C151   : 112   C:1344                       P+C:1428   3      : 357   Median :4   Median : 437.0  
#E200   : 112   D:1456                       P+M:1428   4      : 357   Mean   :4   Mean   : 514.7  
#E75    : 112                                           5      : 357   3rd Qu.:6   3rd Qu.: 578.0  
#F106   : 112                                           6      : 357   Max.   :7   Max.   :7387.0  
#(Other):4944                                           (Other):3570  

# explore reading times

summary(dataon1$Reading.time) 

# results

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 83.0   350.8   437.0   514.7   578.0  7387.0 


# reading times mean and standard deviation (IMPORTANT: it needs to be reported)

Reading.time = dataon1 %>%
  group_by(condition) %>% 
  summarise(mean = mean(Reading.time),
            standard.deviation = sd(Reading.time))

# reading times mean per variable (IMPORTANT: it needs to be reported)

aggregate(Reading.time ~ Quantifier, data = dataon1, mean)

# results 

#           Quantifier  Reading.time
# Quantifier Reading.time
#1          S     515.7525
#2          P     513.6131

aggregate(Reading.time ~ Noun, data = dataon1, mean)

# results

#       Noun  Reading.time
#1    C     508.4968
#2    M     520.8687

# reading time mean per participant and per item

dataon1 %>%
  group_by(Participant) %>%
  summarise(mean = mean(Reading.time)) %>%
  arrange(desc(mean)) # decrescent order

dataon1 %>%
  group_by(Item) %>%
  summarise(mean = mean(Reading.time)) %>%
  arrange(desc(mean))

################################################################################

# creading a table with the data frame

View(dataon1)

# calculating the mean of each segment

mean_RT_by_condition_region <- dataon1 %>%
  group_by(condition, Segment) %>%
  summarise(MeanRT = mean(Reading.time, na.rm = TRUE))

# View the resulting table

print(mean_RT_by_condition_region, n = 28)

# generating the dataset with 4 conditions and 7 regions

mean_RT_by_condition_region <- data.frame(
  Subject = rep(1:10, each = 28),  # 4 conditions x 7 regions for each participant
  Condition = rep(c("S+C", "S+M", "P+C", "P+M"), each = 7, times = 10),
  Region = rep(1:7, times = 40),  # 7 regions
  RT = c(449, 472, 460, 457, 516, 502, 702, 
         425, 483, 469, 472, 522, 479, 814,
         412, 466, 468, 491, 528, 490, 707,
         442, 461, 482, 487, 539, 514, 704) # + rnorm(280, 0, 50)  # RTs with some noise
)

# Plot the region-by-region RTs
ggplot(mean_RT_by_condition_region, aes(x = Region, y = RT, color = Condition, group = Condition)) +
  geom_line(size = 1.2) +      # Line plot
  geom_point(size = 3) +       # Add points on the lines
  scale_x_continuous(breaks = 1:7) +  # Ensure the x-axis is numbered from 1 to 7
  labs(title = "Segment-by-Segment Reading Times",
       x = "Segment",
       y = "RT (ms)",
       color = "Condition") +
  theme_minimal() +            # A clean theme
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Center the title and increase its size
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

# observations

# i did not remove the outliers nor normalized the data. this is a graph only 
# with raw data

