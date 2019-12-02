###packages###
library(sjmisc)
library(foreign)
library(tidyverse)
library(ggplot2)
library(Hmisc)
require(dplyr)
library(car)
library(essurvey)


###setMail for access via essurvey###
set_email("YOUR EMAIL")

R4 <- import_rounds(4)
R4 <- recode_missings(R4)
R4 <- recode_missings(R4, c("Don't know", "Refusal"))
R5 <- import_rounds(5)
R5 <- recode_missings(R5)
R5 <- recode_missings(R5, c("Don't know", "Refusal"))
R6 <- import_rounds(6)
R6 <- recode_missings(R6)
R6 <- recode_missings(R6, c("Don't know", "Refusal"))
R7 <- import_rounds(7)
R7 <- recode_missings(R7)
R7 <- recode_missings(R7, c("Don't know", "Refusal"))
R8 <- import_rounds(8)
R8 <- recode_missings(R8)
R8 <- recode_missings(R8, c("Don't know", "Refusal"))
R9 <- import_rounds(9)
R9 <- recode_missings(R9)
R9 <- recode_missings(R9, c("Don't know", "Refusal"))
summary(R9$lrscale)

#If neccessary, recode left-right scale for R9
R9$lrscale_2 <- as.numeric(R9$lrscale)
R9$lrscale_2 <- R9$lrscale_2-1
summary(R9$lrscale_2)


######Use Weights######
library(survey)
library(srvyr)

R9_w <- R9 %>%
  as_survey(weights = c(dweight, pweight))

R8_w <- R8 %>%
  as_survey(weights = c(dweight, pweight))

R7_w <- R7 %>%
  as_survey(weights = c(dweight, pweight))

R6_w <- R6 %>%
  as_survey(weights = c(dweight, pweight))

R5_w <- R5 %>%
  as_survey(weights = c(dweight, pweight))

R4_w <- R4 %>%
  as_survey(weights = c(dweight, pweight))


#Extract weited scores and create dataframe
R4_t <- prop.table(svytable(~lrscale, R4_w))
R5_t <- prop.table(svytable(~lrscale, R5_w))
R6_t <- prop.table(svytable(~lrscale, R6_w))
R7_t <- prop.table(svytable(~lrscale, R7_w))
R8_t <- prop.table(svytable(~lrscale, R8_w))
R9_t <- prop.table(svytable(~lrscale_2, R9_w))

dat_r <- data.frame(R4_t, R5_t, R6_t, R7_t, R8_t, R9_t)


#reshape the dataframe
library("reshape2")
df_pl <- melt(dat_r, id.vars = c("lrscale"),
     measure.vars = c("Freq", "Freq.1", "Freq.2", "Freq.3", "Freq.4", "Freq.5"))


#Recode the lables
df_pl$Round = recode(df_pl$variable, "'Freq' = 'Round_2008'; 'Freq.1' = 'Round_2010'; 'Freq.2' = 'Round_2012'; 'Freq.3' = 'Round_2014'; 'Freq.4' = 'Round_2016';'Freq.5' = 'Round_2018' ")

df_pl$Pooled = recode(df_pl$variable, "'Freq' = '2008-12'; 'Freq.1' = '2008-12'; 'Freq.2' = '2008-12'; 'Freq.3' = '2014-18'; 'Freq.4' = '2014-18';'Freq.5' = '2014-18' ")


#If neccessary, recode lrscale
df_pl$lrscale =df_pl$lrscale-1


#Plot the data
ggplot(df_pl, aes(x = lrscale, y = value, fill=Pooled)) +
geom_bar(position="dodge",stat = "summary", fun.y = "mean") +
scale_fill_manual(values = c("#ee9435", "#074A81"))+
xlab("Left-Right Scale")+ylab("Percent")+theme_bw()+
theme(legend.position= c(0.8, 0.8),legend.background = element_rect(fill = "transparent"))


ggplot(df_pl, aes(x = lrscale, y = value)) +
geom_bar(position="dodge", stat="identity", fill="#074A81") +
facet_wrap(~Round) +
theme(legend.position="none") +
xlab(NULL)+ylab("Percent")+theme_bw()

