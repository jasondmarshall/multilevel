# The following R Script is meant to augment Bliese's (2016) multilevel package tutorial
# https://cran.r-project.org/doc/contrib/Bliese_Multilevel.pdf

# Clean space
remove(list=ls())
dev.off()
cat("\014")

install.packages("multilevel")
library(multilevel)

# Let's load the data
data("univbct")

# Let's look at the variables 
names(univbct)

# Making a wide version of the data for illustrative purposes - NOT NORMALLY PART OF THE STEPS 
GROWDAT<-univbct[!duplicated(univbct$SUBNUM),c(22,8,9,12,15)] 
GROWDAT[1:3,]

#### PREP STEP I: CONVERT DATA TO LONG "STACKED" FORM ####
UNIV.GROW<-make.univ(GROWDAT,GROWDAT[,c("JOBSAT1","JOBSAT2","JOBSAT3")], # Create a new JOBSAT Column with JOBSAT1,2,3 Stacked
                     outname = "JOBSAT") # By default, this culumn will be labeled MULTDV. This simply changes the name to "JOBSAT"
UNIV.GROW[1:9,] # Look at the first 9 rows of data 

#### PREP STEP II: LOOK AT THE SLOPES FOR A SUBSET OF THE INDIVIDUALS ####
library(lattice)
xyplot(JOBSAT~TIME|as.factor(SUBNUM),data=UNIV.GROW[1:90,], # Rows 1:90, with 3 data points, gives you 30 individuals' data
       type=c("p","r","g"),col="blue",col.line="black",
              xlab="Time",ylab="Job Satisfaction")

##### STEP 1: EXAMINE THE DV ####
null.model<-lme(JOBSAT~1,random=~1|SUBNUM,data=UNIV.GROW, na.action=na.omit, control=list(opt="optim"))

### Check ICC (how much variance is explained by the between individual level?)
VarCorr(null.model)
0.4337729/(0.4337729+0.4319055) 

### Option 2 for ICC
tmod<-aov(JOBSAT~as.factor(SUBNUM),data=UNIV.GROW) # Creates a model comparing well-being among individuals
ICC1(tmod)
ICC2(tmod)

### Option 3 for ICC 
library(psychometric)

ICC1.lme(JOBSAT, SUBNUM, UNIV.GROW) # Format is (DV, grouping variable, data-frame)
ICC2.lme(JOBSAT, SUBNUM, UNIV.GROW, weighted = FALSE)

#### STEP 2: MODEL TIME ####
### Linear model 
model.2<-lme(JOBSAT~TIME,random=~1|SUBNUM,data=UNIV.GROW,
             na.action=na.omit,control=list(opt="optim"))
summary(model.2)

### Quadratic model - Skip model.2b because the power polynomial version is superior 
model.2c<-lme(JOBSAT~poly(TIME,2),random=~1|SUBNUM,data=UNIV.GROW,
              na.action=na.omit,control=list(opt="optim"))
summary(model.2c)

#### STEP 3: MODEL SLOPE VARIABILITY ####
### Should we include random slopes? 
model.3<-update(model.2,random=~TIME|SUBNUM)
summary(model.3)
### OR ### 
#model.3x<-lme(JOBSAT~TIME,random=~TIME|SUBNUM,data=UNIV.GROW,
             #na.action=na.omit,control=list(opt="optim"))
#summary(model.3x)
anova(model.2,model.3)

#### STEP 4: MODELING ERROR STRUCTURES ####
### Check for autocorrelation 
model.4a<-update(model.3,correlation=corAR1())
anova(model.3,model.4a)

### Look at the autocorrelation estimate (Phi coefficient)
summary(model.4a)

### Check for heteroskedasticity 
model.4b<-update(model.4a,weights=varExp(form=~TIME))
anova(model.4a,model.4b)

#### STEP 5: PREDICTING THE INTERCEPT VARIATION ####
### Grand-mean Center the Level-2 Predictor
#UNIV.GROW$AGE.C<-scale(UNIV.GROW$AGE,center=TRUE,scale=FALSE)

### Model AGE as a predictor 
model.5<-lme(JOBSAT~TIME+AGE,random=~TIME|SUBNUM, correlation=corAR1(),
             na.action=na.omit,data=UNIV.GROW, control=list(opt="optim"))
summary(model.5)

#### STEP 6: PREDICTING SLOPE VARIATION ####
model.6<-lme(JOBSAT~TIME*AGE,random=~TIME|SUBNUM, correlation=corAR1(),
             na.action=na.omit,data=UNIV.GROW, control=list(opt="optim"))
summary(model.6)

#### PLOTTING CROSS-LEVEL INTERACTIONS ####
### Create a Temporary Dataframe
TDAT<-data.frame(COEFS=(summary(model.6)$tTable)[,1], 
                 CASE1=c(1,0,21,0),
                 CASE2=c(1,0,31,0), 
                 CASE3=c(1,2,21,42),
                 CASE4=c(1,2,31,62))
TDAT

### Calculate High and Low Values 
low.1<-sum(TDAT[,1]*TDAT[,2])
low.1
high.1<-sum(TDAT[,1]*TDAT[,3])
high.1
low.2<-sum(TDAT[,1]*TDAT[,4])
low.2
high.2<-sum(TDAT[,1]*TDAT[,5])
high.2
low<-c(low.1,low.2)
high<-c(high.1,high.2)
print(low)
print(high)
plot(low, type="b", col="blue",pch=22,ylim=c(2.5,4.0),main = "Job Satisfaction Over Time",xlab="Time",ylab="Job Satisfaction",
     axes=FALSE, ann=T,lty=1,lwd=2)
axis(1, at=1:2, lab=c("Time0","Time2"))
axis(2,labels = TRUE)
lines(high, type="b", lty=1, col="dark red",pch=25,lwd=2)
legend(1.3, 3.9,c("21 Years Old","31 Years Old"), cex=1,col=c("blue","dark red"), pch=c(22,25), lty=c(0,0),box.lwd=0,lwd=c(2,2))


### Simple Slope Test
model.6<-lme(JOBSAT~TIME*AGE,random=~TIME|SUBNUM, correlation=corAR1(),
             na.action=na.omit,data=UNIV.GROW, control=list(opt="optim"))
summary(model.6)$tTable

# High
library(car)
high.AGE<-c(0,1,0,31)
linearHypothesis(model.6,high.AGE, test="F")
print(as.numeric(summary(model.6)$tTable[2]+31*summary(model.6)$tTable[4]),dig=3)
# Low
library(car)
low.AGE<-c(0,1,0,21)
linearHypothesis(model.6,low.AGE, test="F")
print(as.numeric(summary(model.6)$tTable[2]+21*summary(model.6)$tTable[4]),dig=3)
