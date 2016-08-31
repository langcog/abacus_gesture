# Study 2 Novices: Data Preprocessing
rm(list=ls())
setwd("~/Desktop/Abacus_gesture/*first_paper/abacus_gesture_analyses/Study2/novices/rawdata")
# SET WORKING DIRECTORY TO FOLDER CONTAINING MATLAB OUTPUT AND DEMOGRAPHICS DATA

# import computer data from all subjects (exported from matlab)
matlab_output <- read.csv('2013_USA_Audio_output data15-Jul-2015.csv')
# import demographics data
demographics <- read.csv('demographics_usa.csv') 
#merge computer data and demographics data
data <- merge(matlab_output, demographics, by="subnum", all.x=T, all.y=F)

## LIMIT DATA TO SAME NUMBER OF TRIALS FOR EACH TASK WITHIN EACH SUBJECT (starting with first level 2 trial)
# get max trials for each task
n <- length(levels(factor(data$subnum)))
max.trials <- data.frame(subnum = levels(factor(data$subnum)), baseline=rep(0,n), blindfold=rep(0,n),stillhands=rep(0,n),interference=rep(0,n))
for (i in 1:n) {
  sub = levels(factor(data$subnum))[i]
  max.trials$baseline[i] <- max(data[data$subnum==sub & data$condition=="baseline", 5], na.rm=T)
  max.trials$blindfold[i] <-  max(data[data$subnum==sub & data$condition=="blindfold", 5], na.rm=T)
  max.trials$stillhands[i] <-  max(data[data$subnum==sub & data$condition=="stillhands", 5], na.rm=T)
  max.trials$interference[i] <-  max(data[data$subnum==sub & data$condition=="interference", 5], na.rm=T)
}

# first trial where they get to level 2
first.l2 <- data.frame(subnum = levels(factor(data$subnum)), baseline=rep(0,n), blindfold=rep(0,n),stillhands=rep(0,n),interference=rep(0,n))
for (i in 1:n) {
  sub = levels(factor(data$subnum))[i]
  first.l2$baseline[i] <- min(data[data$subnum==sub & data$condition=="baseline" & data$threshold==2, 5], na.rm=T)
  first.l2$blindfold[i] <- min(data[data$subnum==sub & data$condition=="blindfold" & data$threshold==2, 5], na.rm=T)
  first.l2$stillhands[i] <- min(data[data$subnum==sub & data$condition=="stillhands" & data$threshold==2, 5], na.rm=T)
  first.l2$interference[i] <- min(data[data$subnum==sub & data$condition=="interference" & data$threshold==2, 5], na.rm=T)
}

#smallest number of viable trials:
trials <- data.frame(subnum = levels(factor(data$subnum)), ntrials = rep(0,n))
good.trials <- max.trials-first.l2 #ignore warning, we can add subnum back in.
for (i in 1:n) {
  trials$ntrials[i] <- min(good.trials[i,2:5], na.rm=T)
}

trial.sheet <- cbind(trials, first.l2[2:5])


### create a dataset that averages by task, but only between first.l2 and the smallest number of trials
data.cons <- data.frame()
for (i in 1:n){
  sub = levels(factor(data$subnum))[i]
  temp.base = data[data$subnum == sub & data$condition=="baseline" & data$trial >= trial.sheet[i,3] & data$trial < trial.sheet[i,2]+trial.sheet[i,3],]
  temp.blind = data[data$subnum == sub & data$condition=="blindfold" & data$trial >= trial.sheet[i,4] & data$trial < trial.sheet[i,2]+trial.sheet[i,4],]
  temp.still = data[data$subnum == sub & data$condition=="stillhands" & data$trial >= trial.sheet[i,5] & data$trial < trial.sheet[i,2]+trial.sheet[i,5],]
  temp.int = data[data$subnum == sub & data$condition=="interference" & data$trial >= trial.sheet[i,6] & data$trial < trial.sheet[i,2]+ trial.sheet[i,6],]
  data.cons = rbind(data.cons, temp.base, temp.blind, temp.still, temp.int)
}

data.cons$gender.12 <- as.numeric(data.cons$gender)
bysub.cons <-  aggregate(data.cons, by=list(data.cons$subnum,data.cons$condition),FUN=mean, na.rm=T)
names(bysub.cons)[1] <- c("subnum")
names(bysub.cons)[2] <- c("condition")
bysub.cons$condition <- relevel(bysub.cons$condition, "stillhands")
bysub.cons$condition <- relevel(bysub.cons$condition, "blindfold")
bysub.cons$condition <- relevel(bysub.cons$condition, "baseline")

tapply(data.cons$threshold, list(data.cons$condition, data.cons$subnum), length)
#remove subjects that don't have all observations
# bysub.cons = bysub.cons[bysub.cons$subnum !=2304,]

bysub.cons$subnum <- factor(bysub.cons$subnum)

#Exclude finger counters - using a different motor strategy
nof <- data.cons[data.cons$finger.counter==0,]
bysub.final <- bysub.cons[bysub.cons$finger.counter==0,]

#exclude subject 16 because he had a broken arm - wasn't able to freely move one hand during baseline and was unable to do 2-handed motor interference
nofno16 <- nof[nof$subnum!=16,]
bysub.final <- bysub.final[bysub.final$subnum!=16,]

tapply(bysub.final$threshold, list(bysub.final$condition, bysub.final$subnum), length)


#OUTPUT DATA
write.csv(bysub.final, "MAstudy2_novices_bysub.csv")
write.csv(nofno16[is.na(nofno16$subnum)==F,], "MAstudy2_novices_bytrial.csv")


