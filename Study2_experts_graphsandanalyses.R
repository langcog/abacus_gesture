### Study 2 Experts: Graphs and analyses
rm(list=ls())
library(plotrix)
library(lme4)

##READ IN DATA
setwd("~/Desktop/Abacus_gesture/*first_paper/abacus_gesture_analyses/Study2/experts/finaldata") # SET WORKING DIRECTORY TO FOLDER CONTAINING MATLAB OUTPUT AND DEMOGRAPHICS DATA
data.bysub <- read.csv("MAstudy2_experts_bysub.csv")
data.bytrial <- read.csv("MAstudy2_experts_bytrial.csv")

##reformat factors
# structure condition levels so that baseline is the reference level and the others apper in a sensible order on graphs
data.bysub$condition <- relevel(data.bysub$condition, "Sit on Hands")
data.bysub$condition <- relevel(data.bysub$condition, "Blindfold")
data.bysub$condition <- relevel(data.bysub$condition, "Baseline")
levels(data.bysub$condition) <- c("Baseline", "Blindfold","No Hands", "Motor Interference")
# make subnum a factor
data.bysub$subnum <- factor(data.bysub$subnum)

##GRAPH DATA (graphs 1 and 2 not included in paper)
#Make bar graphs showing performance on each task, with error bars
#Graph 1: by addend size
threshBar <- tapply(data.bysub$max, data.bysub$condition, FUN=mean, na.rm=T)
plot <- barplot(threshBar, ylim=c(0,550), col=c("Black","dark gray","light gray","white"), main=c("Abacus Subjects: Mean Addend Size by Condition"))
ses <- tapply(data.bysub$max, data.bysub$condition, FUN=std.error)
plotCI(plot, threshBar, ses, add=T, col=c("gray"))

# graph 2: by threshold
threshBar <- tapply(data.bysub$threshold, data.bysub$condition, FUN=mean, na.rm=T)
plot <- barplot(threshBar,ylim=c(0,7), ylab="Mean Threshold Level", main=c("MA Experts: Threshold Level by Condition"), col=c("Black","dark gray","light gray","white"))
ses <- tapply(data.bysub$threshold, data.bysub$condition, FUN=std.error)
plotCI(plot, threshBar, ses, add=T, col=c("gray"))

#Graph 3: overall threshold graph by trial
par(cex=1.1, lwd=2)
means <- tapply(data.bytrial$threshold, list(data.bytrial$condition, data.bytrial$trial),mean,na.rm=T)
plot(means[1,1:20],type="l",col="blue", xlab=c("Trial"), ylab=c("Level"),main=c("MA Experts: Average Performance Over First 20 Trials"), lty=1)
legend("topleft",c("Baseline","Blindfold", "No Hands","Motor Interference"),lty=c(1,2,3,4), col=c("blue","green","orange","red"))
points(means[2,1:20],type="l", col="green", lty=2)
points(means[4,1:20],type="l", col="orange", lty=3)
points(means[3,1:20],type="l", col="red", lty=4)

## ANALYSES: CONDUCT MIXED EFFECTS REGRESSION MODEL OF PERFORMANCE
# For analyses, consider all data with random intercept & slopes for subject
data.bytrial$subnum <- factor(data.bytrial$subnum)
thresh.lmr <- lmer(threshold ~ condition+ (1+condition|subnum), data=data.bytrial)
# TO GET SIMPLE EFFECTS P VALUES: USE t-as-z heuristic
 2*(1-pnorm(abs((fixef(thresh.lmr)/sqrt(diag(vcov(thresh.lmr)))))))

# check about other factors to possibly include in model
#age
lm.1 <- lmer(threshold ~ condition+ age + (1+condition|subnum) + (1|age), data=data.bytrial) #t=.12
pval <- 2*(1-pnorm(abs((fixef(lm.1)/sqrt(diag(vcov(lm.1)))))))#p=0.91, ns
# gender
lm.1 <- lmer(threshold ~ condition+ gender + (1+condition |subnum) + (1|gender), data=data.bytrial) #t=1.13
pval <- 2*(1-pnorm(abs((fixef(lm.1)/sqrt(diag(vcov(lm.1)))))))#p=0.26, ns
# task order
data.bytrial$order <- factor(data.bytrial$order)
lm.1 <- lmer(threshold ~ condition + order + (1+ condition|subnum) + (1|order), data=data.bytrial) #ts<=0.56
pval <- 2*(1-pnorm(abs((fixef(lm.1)/sqrt(diag(vcov(lm.1)))))))#ps>=0.58, ns
# order * condition interaction
lm.2 <- lmer(threshold ~ condition * order + (1+ condition|subnum) + (1|order), data=data.bytrial) #t=0.151, ns
Anova(lm.2) #p=0.40, ns

# get Z scoes on thresholds to do a population X condition interaction:
# I think we want to conduct this using a single z-score for each participant for each task, rather than using every trial
mean <- mean(data.bysub$threshold)
data.bysub$zthresh <- scale(data.bysub$threshold)

experts.zscores <- data.bysub[,c("subnum","condition","order","zthresh", "age")]
write.csv(experts.zscores, "~/Desktop/Abacus_gesture/*first_paper/abacus_gesture_analyses/Study2/expert_zscores.csv")

