# Study 2 Experts: Data Preprocessing
rm(list=ls())
setwd("~/Desktop/Abacus_gesture/*first_paper/abacus_gesture_analyses/Study2/experts/rawdata")
# SET WORKING DIRECTORY TO FOLDER CONTAINING MATLAB OUTPUT AND DEMOGRAPHICS DATA

# import computer data from all subjects (exported from matlab)
matlab_output <- read.csv('2013_India_Audio_output data9-13-2013.csv')
# import demographics data
demographics <- read.csv('demographics.csv')[,c("subnum","birthdate","testing.date","age","gender")]
#merge computer data and demographics data
data <- merge(matlab_output, demographics, by="subnum", all.x=T, all.y=F)

## LIMIT DATA TO SAME NUMBER OF TRIALS FOR EACH TASK WITHIN EACH SUBJECT (starting with first level 2 trial)
# get number of trials completed for each task by subject
n <- length(levels(factor(data$subnum)))
max.trials <- data.frame(subnum = levels(factor(data$subnum)), baseline=rep(0,n), blindfold=rep(0,n),stillhands=rep(0,n),interference=rep(0,n))
for (i in 1:n) {
	sub = levels(factor(data$subnum))[i]
	max.trials$baseline[i] <- max(data[data$subnum==sub & data$condition=="baseline", 5], na.rm=T)
	max.trials$blindfold[i] <-  max(data[data$subnum==sub & data$condition=="blindfold", 5], na.rm=T)
	max.trials$stillhands[i] <-  max(data[data$subnum==sub & data$condition=="stillhands", 5], na.rm=T)
	max.trials$interference[i] <-  max(data[data$subnum==sub & data$condition=="interference", 5], na.rm=T)
} #missing data will appear as "Inf" and indicates that a subject will need to be excluded

# get first trial where they get to level 2
first.l2 <- data.frame(subnum = levels(factor(data$subnum)), baseline=rep(0,n), blindfold=rep(0,n),stillhands=rep(0,n),interference=rep(0,n))
for (i in 1:n) {
	sub = levels(factor(data$subnum))[i]
	first.l2$baseline[i] <- min(data[data$subnum==sub & data$condition=="baseline" & data$threshold==2, 5], na.rm=T)
	first.l2$blindfold[i] <- min(data[data$subnum==sub & data$condition=="blindfold" & data$threshold==2, 5], na.rm=T)
	first.l2$stillhands[i] <- min(data[data$subnum==sub & data$condition=="stillhands" & data$threshold==2, 5], na.rm=T)
	first.l2$interference[i] <- min(data[data$subnum==sub & data$condition=="interference" & data$threshold==2, 5], na.rm=T)
}#missing data will appear as "Inf" and indicates that a subject will need to be excluded

#smallest number of viable trials: max.trials - first.l2
trials <- data.frame(subnum = levels(factor(data$subnum)), ntrials = rep(0,n))
good.trials <- max.trials-first.l2
for (i in 1:n) {
	trials$ntrials[i] <- min(good.trials[i,2:5], na.rm=T)
}
trial.sheet <- cbind(trials, first.l2[2:5])

## create a dataset that averages level by task, but only between first.l2 and the smallest number of trials
data.cons <- data.frame() #conservative dataframe (with matched # of trials for each subject across tasks)
for (i in 1:n){
	sub = levels(factor(data$subnum))[i]
	temp.base = data[data$subnum == sub & data$condition=="baseline" & data$trial >= trial.sheet[i,"baseline"] & data$trial < trial.sheet[i,"ntrials"]+trial.sheet[i,"baseline"],] #all baseline trials to include for this subject
	temp.blind = data[data$subnum == sub & data$condition=="blindfold" & data$trial >= trial.sheet[i,"blindfold"] & data$trial < trial.sheet[i,"ntrials"]+trial.sheet[i,"blindfold"],] #all blindfold trials to include
	temp.still = data[data$subnum == sub & data$condition=="stillhands" & data$trial >= trial.sheet[i,"stillhands"] & data$trial < trial.sheet[i,"ntrials"]+trial.sheet[i,"stillhands"],] #all stillhands trials to include
	temp.int = data[data$subnum == sub & data$condition=="interference" & data$trial >= trial.sheet[i,"interference"] & data$trial < trial.sheet[i,"ntrials"]+ trial.sheet[i,"interference"],] #all interference trials to include
	data.cons = rbind(data.cons, temp.base, temp.blind, temp.still, temp.int) #add all trials for this subject to the master data file
}
tapply(data.cons$subnum, list(data.cons$subnum, data.cons$condition), length) # verifies that we have the same number of trials for each task for each subject!
#note: subjects that had 0 viable trials on any task (2309, 2334, 2414, and 2470) are now excluded from the dataset entirely.

##Aggregate data.cons to get a dataframe with each subject's average level on each task
# add a numeric gender variable that will show up in aggregated data
data.cons$gender.12 <- as.numeric(data.cons$gender)
# aggregate by subject number and condition
bysub.cons <-  aggregate(data.cons, by=list(data.cons$subnum,data.cons$condition),FUN=mean, na.rm=T)
#clean up bysub.cons:
#rename variables
names(bysub.cons)[1] <- c("subnum")
names(bysub.cons)[2] <- c("condition")
# structure condition levels so that baseline is the reference level and the others apper in a sensible order on graphs
bysub.cons$condition <- relevel(bysub.cons$condition, "stillhands")
bysub.cons$condition <- relevel(bysub.cons$condition, "blindfold")
bysub.cons$condition <- relevel(bysub.cons$condition, "baseline")
#rename conditions with full condition names
levels(bysub.cons$condition) <- c("Baseline","Blindfold","Sit on Hands","Motor Interference")
# make subnum a factor
bysub.cons$subnum <- factor(bysub.cons$subnum)

#OUTPUT DATA
write.csv(bysub.cons, "finaldata/MAstudy2_experts_bysub.csv")
write.csv(data.cons[is.na(data.cons$subnum)==F,], "finaldata/MAstudy2_experts_bytrial.csv")
