# zscore interaction analysis
rm(list=ls())
library(plotrix)
library(lme4)

##READ IN DATA
setwd("~/Desktop/Abacus_gesture/*first_paper/abacus_gesture_analyses/Study2/zscore analysis") # SET WORKING DIRECTORY TO FOLDER CONTAINING MATLAB OUTPUT AND DEMOGRAPHICS DATA

novices <- read.csv("novice_zscores.csv")
novices$pop <- "novices"
experts <- read.csv("expert_zscores.csv")
experts$pop <- "experts"

all <- rbind(novices, experts)
summary(all) #need to make subnum and order factors
all$subnum <- factor(all$subnum)
all$order <- factor(all$order)
all$pop <- factor(all$pop)

z.lm <- lmer(zthresh ~ condition * pop + (1 |subnum), data=all)
2*(1-pnorm(abs((fixef(z.lm)/sqrt(diag(vcov(z.lm))))))) #interaction significant


# REPEAT BUT ONLY INCLUDING EXPERTS OVER 11: 13 participants
sort(experts$age)
older <- experts[experts$age > 11,]
sort(novices$age)

all.old <- rbind(novices, older)
summary(all.old)
all.old$subnum <- factor(all.old$subnum)
all.old$order <- factor(all.old$order)
all.old$pop <- factor(all.old$pop)

z.lm <- lmer(zthresh ~ condition * pop + (1 |subnum), data=all.old)
2*(1-pnorm(abs((fixef(z.lm)/sqrt(diag(vcov(z.lm))))))) #interaction significant


# REPEAT BUT ONLy INCLUDING EXPERTS OVER 12: 8 participants
sort(experts$age)
older <- experts[experts$age > 12,]
sort(novices$age)

all.old <- rbind(novices, older)
summary(all.old)
all.old$subnum <- factor(all.old$subnum)
all.old$order <- factor(all.old$order)
all.old$pop <- factor(all.old$pop)

z.lm <- lmer(zthresh ~ condition * pop + (1 |subnum), data=all.old)
2*(1-pnorm(abs((fixef(z.lm)/sqrt(diag(vcov(z.lm))))))) #interaction significant

