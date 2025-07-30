# interpretation task
# bilingual group
# filtering the data

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

dataoffraw <- read.pcibex("results.csv")

# check the data

View(dataoffraw)

# change participant's number F79 from group D to D79 
# (pcibex internal error generated the same number to two different participants)

dataoffraw$'ParticipantID'[1:16] <- sub("F79", "C79", dataoffraw$'ParticipantID'[1:16])

# removing participants there were not brpl1 according to the questionnaire

participant_to_remove <- "ParticipantID_H190"
dataoffraw <- dataoffraw[dataoffraw$ParticipantID != "H190", ]
participant_to_remove <- "ParticipantID_N69"
dataoffraw <- dataoffraw[dataoffraw$ParticipantID != "N69", ]

# check the structure

str(dataoffraw)

# removing columns

dataoffraw <- dataoffraw %>%
  filter(Label == "alvos",
         Parameter %in% c("Selection","Print") |
           PennElementName == "respostaalvos") %>%
  select(ParticipantID, group, Label, item, Parameter, condition, quantifier, noun, Value, EventTime) %>%
  group_by(ParticipantID, item) %>%
  mutate(event = case_when(Parameter == "Print" ~ "T1",
                           Parameter == "Selection" ~ "T2"),
         selection = case_when(Value == "imga" ~ "Volume",
                               Value == "imgb" ~ "Cardinal")) %>%
  fill(selection, .direction = "up") %>%
  ungroup() %>%
  select(-Parameter, - Value) %>% 
  pivot_wider(names_from = event, values_from = EventTime) %>% 
  mutate(RT = T2 - T1) 
#ungroup() %>%
#mutate_if(is.character, as.factor)

# response time mean

MeanRTdataoffraw = dataoffraw %>%
  group_by(condition) %>% 
  summarise(MeanRT = mean(RT))

# saving the tables to csv files

write.csv(dataoffraw, "~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Interpretation Task/dataoffraw.csv")

write.csv(MeanRTdataoffraw, "~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Interpretation Task/meanRTraw.csv")
