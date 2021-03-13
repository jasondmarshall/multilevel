### THIS IS THE R SCRIPT FOR: 
# Wegmann, J., Marshall, J., Tsai, C. Y., & Dionne, S. (2020). 
# Health education and changing stress mindsets: The moderating role of personality. 
# American Journal of Health Education, 51(4), 244–256. https://doi.org/10.1080/19325037.2020.1767002

### THE DATA IS PUBLICLY AVAILABLE FOR DOWNLOAD AT: 
# https://osf.io/p9367

#### Clean Space
remove(list=ls())
dev.off()
cat("\014")

#### Load Data
mydata<-readxl::read_excel("~/Desktop/Multilevel Workshop for MDSA/Wegmann et al. - Stress Mindset Data.xlsx")
names(mydata) 

#### Create a Construct-level Dataframe from Items
newdata<-dplyr::mutate(mydata,
                       SMM.T1=rowMeans(mydata[,16:23],na.rm = TRUE), # Stress Mindset Measure T1
                       SMM.T2=rowMeans(mydata[,24:31],na.rm = TRUE), # Stress Mindset Measure T2
                       BC.T1=rowMeans(mydata[,60:81],na.rm = TRUE), # Brief Cope T1
                       BC.T2=rowMeans(mydata[,82:103],na.rm = TRUE), # Brief Cope T2
                       WB.T1=rowMeans(mydata[,104:129],na.rm = TRUE), # Well-Being T1_26 items
                       WB.T2=rowMeans(mydata[,130:155],na.rm = TRUE), # Well-Being T1_26 items +1 (not sure)
                       PS.T1=rowMeans(mydata[,32:45],na.rm = TRUE), # Perceived Strss T1_14 items
                       PS.T2=rowMeans(mydata[,46:59],na.rm = TRUE), # Perceived Strss T2_14items
                       ANX=rowMeans(mydata[,217:223],na.rm = TRUE), # Anxiety T2_7 items
                       Pro=rowMeans(mydata[,224:243],na.rm = TRUE),# Procrastination T2_20 items
                       TAPP=rowMeans(mydata[,244:251],na.rm = TRUE),# Threat Appraisal T2_8 items (specific to a external event)
                       Neuroticism=rowMeans(mydata[,c(157,162,167,172,177,182,187,192,197,202,207,212)],na.rm = TRUE),
                       Extraversion=rowMeans(mydata[,c(158,163,168,173,178,183,188,193,198,203,208,213)],na.rm = TRUE),
                       Openness=rowMeans(mydata[,c(159,164,169,174,179,184,189,194,199,204,209,214)],na.rm = TRUE),
                       Agreeableness=rowMeans(mydata[,c(160,165,170,175,180,185,190,195,200,205,210,215)],na.rm = TRUE),
                       Conscientiousness=rowMeans(mydata[,c(161,166,171,176,181,186,191,196,201,206,211,216)],na.rm = TRUE),
                       age=as.numeric(Age),
                       sex=as.numeric(Gender),
                       Gender=as.factor(Gender.1),
                       Race=as.factor(Ethnicity.1), 
                       Major=as.factor(Major.1),
                       YearCollege=as.numeric(Years.in.College)
                       )
attach(newdata)

#### Calculate Cronbach Alphas ####
SMM.T1.A<-ltm::cronbach.alpha(cbind(mydata[,16:23]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
SMM.T2.A<-ltm::cronbach.alpha(cbind(mydata[,24:31]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
BC.T1.A<-ltm::cronbach.alpha(cbind(mydata[,60:81]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
BC.T2.A<-ltm::cronbach.alpha(cbind(mydata[,82:103]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
WB.T1.A<-ltm::cronbach.alpha(cbind(mydata[,104:129]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
WB.T2.A<-ltm::cronbach.alpha(cbind(mydata[,130:155]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
PS.T1.A<-ltm::cronbach.alpha(cbind(mydata[,32:45]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
PS.T2.A<-ltm::cronbach.alpha(cbind(mydata[,46:59]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
Neuroticism.A<- ltm::cronbach.alpha((mydata[,c(157,162,167,172,177,182,187,192,197,202,207,212)]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
Extraversion.A<- ltm::cronbach.alpha((mydata[,c(158,163,168,173,178,183,188,193,198,203,208,213)]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
Openness.A<- ltm::cronbach.alpha((mydata[,c(159,164,169,174,179,184,189,194,199,204,209,214)]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
Agreeableness.A<- ltm::cronbach.alpha((mydata[,c(160,165,170,175,180,185,190,195,200,205,210,215)]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)
Conscientiousness.A<- ltm::cronbach.alpha((mydata[,c(161,166,171,176,181,186,191,196,201,206,211,216)]), standardized = T, CI = T, probs = c(0.025, 0.975), na.rm = TRUE)

#### Cronbach.Alpha Table ####
Alpha.table<-as.matrix(as.numeric(rbind(SMM.T1.A[1],SMM.T2.A[1],BC.T1.A[1],BC.T2.A[1],WB.T1.A[1],WB.T2.A[1],PS.T1.A[1],PS.T2.A[1],Neuroticism.A[1],Extraversion.A[1],Openness.A[1],Agreeableness.A[1],Conscientiousness.A[1])))
row.names(Alpha.table) <- c("SMM.T1","SMM.T2","BC.T1","BC.T2","WB.T1","WB.T2","PS.T1","PS.T2","Neuroticism", "Etraversion", "Openness", "Agreeableness", "Conscientiousness")
colnames(Alpha.table)<-c("Cronbach alpha")
print(Alpha.table,dig=2)

### Correlation tables
Corr.table<-data.frame(newdata[,c(252:253,263:267)])
names(Corr.table)
psych::describe(Corr.table)
psych::pairs.panels(Corr.table)
Hmisc::rcorr(as.matrix(Corr.table), type="pearson")

#### Create a Correlation Matrix ####
Cor <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", ifelse(p < .10, "+ ", " "))))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}
print(Cor(Corr.table))

#### Export APA Formatted Correlation Table in Word ####
library(apaTables)
apa.cor.table(newdata[,c(252:253,263:267)], file="~/Desktop/Multilevel Workshop for MDSA/SMM_sample_cortable.doc")

#### Growth Modeling ####
newdata$ID <- seq.int(nrow(newdata))# create ID

#### Center Predictors ####
newdata$Neuroticism.C<-scale(newdata$Neuroticism,center=TRUE,scale=FALSE)[,]
newdata$Extraversion.C<-scale(newdata$Extraversion,center=TRUE,scale=FALSE)[,]
newdata$Openness.C<-scale(newdata$Openness,center=TRUE,scale=FALSE)[,]
newdata$Agreeableness.C<-scale(newdata$Agreeableness,center=TRUE,scale=FALSE)[,]
newdata$Conscientiousness.C<-scale(newdata$Conscientiousness,center=TRUE,scale=FALSE)[,]

#### Create a Wide-formatted, stacked dataframe ####
names(newdata[,252:253])
library(multilevel)
UNIV.GROW<-make.univ(newdata,newdata[,252:253])
UNIV.GROW[1:6,(length(UNIV.GROW)-1):(length(UNIV.GROW))] # Look at the data for 3 individuals  
newdata[,252:253]

#### View the Slopes ####
library(lattice)
library(MASS)
xyplot(MULTDV~TIME|as.factor(ID),data=UNIV.GROW[1:60,], # Look at the first 30 individuals
       type=c("p","r","g"),col="blue",col.line="black",
       xlab="Time",ylab="PS")

#### Slope Varaiation Check ####
model.2<-lme(MULTDV~TIME,random=~1|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
model.3<-lme(MULTDV~TIME,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))#Model Slope Variability
anova(model.2,model.3)
summary(model.3)

#### Check for Autocorrelation ####
model.4<-update(model.3,correlation=corAR1())
anova(model.3,model.4)

#### Check for Heterskedasticity ####
model.4a<-update(model.3,weights=varExp(form=~TIME))
anova(model.3,model.4a)

#### PLOTTING ####

### Neuroticism
### Cross-level interaction (Neuro & SMM)
model.6N<-lme(MULTDV~TIME*Neuroticism.C,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
summary(model.6N)
round(summary(model.6N)$tTable,digits = 4)

### Plotting
#par(mfrow=c(1,1)) # Make sure it's one plot window 
TDAT<-data.frame(COEFS=(summary(model.6N)$tTable)[,1],
                 CASE1=c(1,0,(mean(newdata$Neuroticism.C)-sd(newdata$Neuroticism.C)),0), # Time 0 low-Neuro
                 CASE2=c(1,0,(mean(newdata$Neuroticism.C)+sd(newdata$Neuroticism.C)),0), # Time 0 high-Neuro
                 CASE3=c(1,1,(mean(newdata$Neuroticism.C)-sd(newdata$Neuroticism.C)),(mean(newdata$Neuroticism.C)-sd(newdata$Neuroticism.C))), # Time 1 low-Neuro
                 CASE4=c(1,1,(mean(newdata$Neuroticism.C)+sd(newdata$Neuroticism.C)),(mean(newdata$Neuroticism.C)+sd(newdata$Neuroticism.C)))) # Time 1 High-Neuro
TDAT

low.1<-sum(TDAT[,1]*TDAT[,2])
high.1<-sum(TDAT[,1]*TDAT[,3])
low.2<-sum(TDAT[,1]*TDAT[,4])
high.2<-sum(TDAT[,1]*TDAT[,5])
low<-c(low.1,low.2)
high<-c(high.1,high.2)
print(low)
print(high)
plot(low, type="b", col="blue",pch=22,ylim=c(1.5,2.5),main = "Stress Mindset Over Time",xlab="Time",ylab="SMM",
     axes=FALSE, ann=T,lty=1,lwd=2)
axis(1, at=1:2, lab=c("Time0","Time1"))
axis(2,labels = TRUE)
lines(high, type="b", lty=1, col="dark red",pch=25,lwd=2)
legend(1.3, 2.35,c("Low Neuroticism","High Neuroticism"), cex=1,col=c("blue","dark red"), pch=c(22,25), lty=c(0,0),box.lwd=0,lwd=c(2,2))

### Simple Slope Test
model.6N<-lme(MULTDV~TIME*Neuroticism.C,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
summary(model.6N)$tTable
# High
library(car)
high.Neuroticism<-c(0,1,0,(mean(newdata$Neuroticism.C)+sd(newdata$Neuroticism.C)))
linearHypothesis(model.6N,high.Neuroticism, test="F")
print(as.numeric(summary(model.6N)$tTable[2]+((mean(newdata$Neuroticism.C)+sd(newdata$Neuroticism.C))*summary(model.6N)$tTable[4])),dig=3)
# Low
library(car)
low.Neuroticism<-c(0,1,0,(mean(newdata$Neuroticism.C)-sd(newdata$Neuroticism.C)))
linearHypothesis(model.6N,low.Neuroticism, test="F")
print(as.numeric(summary(model.6N)$tTable[2]+((mean(newdata$Neuroticism.C)-sd(newdata$Neuroticism.C))*summary(model.6N)$tTable[4])),dig=3)

### Extraversion
### Cross-level interaction (Neuro & SMM)
model.6E<-lme(MULTDV~TIME*Extraversion.C,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
summary(model.6E)
round(summary(model.6E)$tTable,digits = 4)

### OPENNESS
### Cross-level interaction (Openness & SMM)
model.6O<-lme(MULTDV~TIME*Openness.C,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
summary(model.6O)
round(summary(model.6O)$tTable,digits = 4)

###Plotting
TDAT<-data.frame(COEFS=(summary(model.6O)$tTable)[,1],
                 CASE1=c(1,0,(mean(newdata$Openness.C)-sd(newdata$Openness.C)),0), # Time 0 low-Con
                 CASE2=c(1,0,(mean(newdata$Openness.C)+sd(newdata$Openness.C)),0), # Time 0 high-Con
                 CASE3=c(1,1,(mean(newdata$Openness.C)-sd(newdata$Openness.C)),(mean(newdata$Openness.C)-sd(newdata$Openness.C))), #Time 1 low-Openness
                 CASE4=c(1,1,(mean(newdata$Openness.C)+sd(newdata$Openness.C)),(mean(newdata$Openness.C)+sd(newdata$Openness.C)))) # Time 2 high-Openness
TDAT

low.1<-sum(TDAT[,1]*TDAT[,2])
high.1<-sum(TDAT[,1]*TDAT[,3])
low.2<-sum(TDAT[,1]*TDAT[,4])
high.2<-sum(TDAT[,1]*TDAT[,5])
low<-c(low.1,low.2)
high<-c(high.1,high.2)
low
high
plot(low, type="b", col="blue",pch=22,ylim=c(1.5,2.5),main = "Stress Mindset Over Time",xlab="Time",ylab="SMM",
     axes=FALSE, ann=T,lty=1,lwd=2)
axis(1, at=1:2, lab=c("Time0","Time1"))
axis(2,labels = TRUE)
lines(high, type="b", lty=1, col="dark red",pch=25,lwd=2)
legend(1.3, 2.3,c("Low Openness","High Openness"), cex=1,col=c("blue","dark red"), pch=c(22,25), lty=c(0,0),box.lwd=0,lwd=c(2,2))

### Simple Slope Test
model.6O<-lme(MULTDV~TIME*Openness.C,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
round(summary(model.6O)$tTable,digits = 4)
# High
library(car)
high.Openness<-c(0,1,0,(mean(newdata$Openness.C)+sd(newdata$Openness.C)))
linearHypothesis(model.6O,high.Openness, test="F")
print(as.numeric(summary(model.6O)$tTable[2]+((mean(newdata$Openness.C)+sd(newdata$Openness.C))*summary(model.6O)$tTable[4])),dig=3)
# Low
library(car)
low.Openness<-c(0,1,0,(mean(newdata$Openness.C)-sd(newdata$Openness.C)))
linearHypothesis(model.6O,low.Openness, test="F")
print(as.numeric(summary(model.6O)$tTable[2]+((mean(newdata$Openness.C)-sd(newdata$Openness.C))*summary(model.6O)$tTable[4])),dig=3)

### Agreeableness
### Cross-level interaction (Neuro & SMM)
model.6A<-lme(MULTDV~TIME*Agreeableness.C,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
summary(model.6A)
round(summary(model.6A)$tTable,digits = 4)

### Conscientiousness
### Cross-level interaction (Conscientiousness & SMM)
model.6C<-lme(MULTDV~TIME*Conscientiousness.C,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
summary(model.6C)
round(summary(model.6C)$tTable,digits = 4)

###Plotting
TDAT<-data.frame(COEFS=(summary(model.6C)$tTable)[,1],
                 CASE1=c(1,0,(mean(newdata$Conscientiousness.C)-sd(newdata$Conscientiousness.C)),0), # Time 0 low-Con
                 CASE2=c(1,0,(mean(newdata$Conscientiousness.C)+sd(newdata$Conscientiousness.C)),0), # Time 0 high-Con
                 CASE3=c(1,1,(mean(newdata$Conscientiousness.C)-sd(newdata$Conscientiousness.C)),(mean(newdata$Conscientiousness.C)-sd(newdata$Conscientiousness.C))), #Time 1 low-Con
                 CASE4=c(1,1,(mean(newdata$Conscientiousness.C)+sd(newdata$Conscientiousness.C)),(mean(newdata$Conscientiousness.C)+sd(newdata$Conscientiousness.C)))) # Time 2 high-Con
TDAT

low.1<-sum(TDAT[,1]*TDAT[,2])
high.1<-sum(TDAT[,1]*TDAT[,3])
low.2<-sum(TDAT[,1]*TDAT[,4])
high.2<-sum(TDAT[,1]*TDAT[,5])
low<-c(low.1,low.2)
high<-c(high.1,high.2)
low
high
plot(low, type="b", col="blue",pch=22,ylim=c(1.5,2.5),main = "Stress Mindset Over Time",xlab="Time",ylab="SMM",
     axes=FALSE, ann=T,lty=1,lwd=2)
axis(1, at=1:2, lab=c("Time0","Time1"))
axis(2,labels = TRUE)
lines(high, type="b", lty=1, col="dark red",pch=25,lwd=2)
legend(1.3, 2.3,c("Low Conscientiousness","High Conscientiousness"), cex=1,col=c("blue","dark red"), pch=c(22,25), lty=c(0,0),box.lwd=0,lwd=c(2,2))

### Simple Slope Test
model.6C<-lme(MULTDV~TIME*Conscientiousness.C,random=~TIME|ID,data=UNIV.GROW,na.action=na.omit,control=list(opt="optim"))
summary(model.6C)$tTable
summary(model.6C)$tTable[2]
# High
library(car)
high.Conscientiousness<-c(0,1,0,(mean(newdata$Conscientiousness.C)+sd(newdata$Conscientiousness.C)))
linearHypothesis(model.6C,high.Conscientiousness, test="F")
print(as.numeric(summary(model.6C)$tTable[2]+((mean(newdata$Conscientiousness.C)+sd(newdata$Conscientiousness.C))*summary(model.6C)$tTable[4])),dig=3)
# Low
library(car)
low.Conscientiousness<-c(0,1,0,(mean(newdata$Conscientiousness.C)-sd(newdata$Conscientiousness.C)))
linearHypothesis(model.6C,low.Conscientiousness, test="F")
print(as.numeric(summary(model.6C)$tTable[2]+((mean(newdata$Conscientiousness.C)-sd(newdata$Conscientiousness.C))*summary(model.6C)$tTable[4])),dig=3)





