setwd("C:/Users/travi/OneDrive/Desktop")
library(tidyverse)
library(data.table)
install.packages("stargazer")
install.packages("corrplot")
library(stargazer)
library(MASS)

#preprocessing
varnames<-c("ID","AGE","HT","Sex","SURVIVE","Shock_TYP","SBP","MAP","HR","DBP","MCVP","BSI","CI","AT","MCT","UO","PVI","RCI","HG","HCT","RECORD")
DAR<-read_csv("DATA-FILEsp2020.csv",col_names = varnames)
DAR$HCT<-DAR$HCT/1000
DAR$Is_Shock<-ifelse(DAR$Shock_TYP!=2,1,0)
DAR$Is_Shock<-as.factor(DAR$Is_Shock)
DAR$SURVIVE<-as.factor(DAR$SURVIVE)
DAR$Shock_TYP<-as.factor(DAR$Shock_TYP)
DAR$Sex<-as.factor(DAR$Sex)
dar_unique<-DAR[!duplicated(DAR$ID),]
###Summary Statistics
#Demographics
summary(dar_unique[c(2,3)])
#Num Males, Females 58/54
table(dar_unique$Sex)
#Num Survived, Died 69/43
table(dar_unique$SURVIVE)
#shock Types NonShock, Hypovolemic, Cardiogenic, Bacterial, Neurogenic, Other 
#34/17/20/15/16/10
dar_unique$Shock_TYP<-as.factor(dar_unique$Shock_TYP)
dar_unique$SURVIVE<-as.factor(dar_unique$SURVIVE)

#####EDA#####
###Shock Analysis
DAR_Shock_Initial<-DAR[which(DAR$Is_Shock==1&DAR$RECORD==1),]
ggplot(DAR_Shock_Initial, aes(x=SURVIVE, y=UO)) +geom_boxplot()
DAR_Shock_Final<-DAR[which(DAR$Is_Shock==1&DAR$RECORD==2),]
ggplot(DAR_Shock_Final, aes(x=SURVIVE, y=UO)) +geom_boxplot()
dar_unique$Shock_TYP<-revalue(dar_unique$Shock_TYP, c("2"="Non-Shock", "3"="Hypovolemic","4"="Cardiogenic","5"="Bacterial","6"="Neurogenic","7"="Other"))
table(dar_unique$Shock_TYP)
ggplot(dar_unique,aes(fill=SURVIVE,x=Shock_TYP))+geom_bar(position="dodge")+labs(x="Shock Type")+
  theme_classic()+theme(legend.position="none")


#Initial Readings
DAR_Initial<-DAR[which(DAR$RECORD==1),]
summary(DAR_Initial[7:20])
colnames(DAR_Initial) <- paste("Initial", colnames(DAR_Initial), sep = "_")
#Final Readings
DAR_Final<-DAR[which(DAR$RECORD==2),]
summary(DAR_Final[7:20])
colnames(DAR_Final) <- paste("Final", colnames(DAR_Final), sep = "_")
DAR_Final<-DAR_Final[-c(1:6)]
#Combined
Wide_Data<-cbind(DAR_Initial,DAR_Final)
Indata<-DAR_Initial[-c(1,3,4,5,6,7,10,14,15,17,20,21,22)]
CorIn<-DAR_Initial[-c(1,4,5,6,21,22)]

#Based on above results, we want to exclude SBP, DBP, AT, MCT, PVI, HCT
AllData<-Wide_Data[-c(1,3,4,5,6,7,10,14,15,17,20,21,22,23,26,28,30,31,34,35,37,38)]
Final_Data<-Wide_Data %>% select(Initial_SURVIVE,Initial_Is_Shock,Initial_MAP,Initial_MCVP,Initial_BSI,Initial_UO,Final_HR, Final_CI, Final_UO, Final_HG)
M<-cor(AllData)
M<-cor(CorIn)
#Correlation
library(corrplot)
corrplot(M, method="number")
###T Testing
attach(DAR_Initial)
t.test(SBP[SURVIVE==1],SBP[SURVIVE==3]) #.000116, but highly correlated with MAP so remove
t.test(MAP[SURVIVE==1],MAP[SURVIVE==3]) #really small
t.test(HR[SURVIVE==1],HR[SURVIVE==3]) #.3006 too high of a p value, remove
t.test(DBP[SURVIVE==1],DBP[SURVIVE==3]) #.00034, but highly correlated with MAP so remove
t.test(MCVP[SURVIVE==1],MCVP[SURVIVE==3]) #.004267
t.test(BSI[SURVIVE==1],BSI[SURVIVE==3]) #.02
t.test(CI[SURVIVE==1],CI[SURVIVE==3]) #.2308 too high so remove
t.test(AT[SURVIVE==1],AT[SURVIVE==3]) #.3418 too high so remove
t.test(MCT[SURVIVE==1],MCT[SURVIVE==3]) #.0888 too high so remove
t.test(UO[SURVIVE==1],UO[SURVIVE==3]) #.0004
t.test(PVI[SURVIVE==1],PVI[SURVIVE==3]) #.5683 too high so remove
t.test(RCI[SURVIVE==1],RCI[SURVIVE==3]) #.2342 too high so remove
t.test(HG[SURVIVE==1],HG[SURVIVE==3]) #.5729 too high so remove
t.test(HCT[SURVIVE==1],HCT[SURVIVE==3]) #.6216 too high so remove
t.test(AGE[SURVIVE==1],AGE[SURVIVE==3]) #.1882 too high so remove
t.test(HT[SURVIVE==1],HT[SURVIVE==3]) #.06965 remove, BSI is more relevant
chisq.test(x=SURVIVE,y=Shock_TYP) #.0007616
chisq.test(x=SURVIVE,y=Is_Shock) #really small

chisq.test(x=SURVIVE,y=Sex) #.06376 remove
detach(DAR_Initial)

attach(DAR_Final)
t.test(SBP[Initial_SURVIVE==1],SBP[Initial_SURVIVE==3]) #really small, but highly correlated with MAP so remove
t.test(MAP[SURVIVE==1],MAP[SURVIVE==3]) #really small
t.test(HR[SURVIVE==1],HR[SURVIVE==3]) #.0233 
t.test(DBP[SURVIVE==1],DBP[SURVIVE==3]) #really small, but highly correlated with MAP so remove
t.test(MCVP[SURVIVE==1],MCVP[SURVIVE==3]) #.000158
t.test(BSI[SURVIVE==1],BSI[SURVIVE==3]) #.098 Duplicate
t.test(CI[SURVIVE==1],CI[SURVIVE==3]) #really small
t.test(AT[SURVIVE==1],AT[SURVIVE==3]) #.0224
t.test(MCT[SURVIVE==1],MCT[SURVIVE==3]) #.001412 
t.test(UO[SURVIVE==1],UO[SURVIVE==3]) #really small
t.test(PVI[SURVIVE==1],PVI[SURVIVE==3]) #.1887 Remove
t.test(RCI[SURVIVE==1],RCI[SURVIVE==3]) #.3863 Remove
t.test(HG[SURVIVE==1],HG[SURVIVE==3]) #.0003911
t.test(HCT[SURVIVE==1],HCT[SURVIVE==3]) #.002484
t.test(AGE[SURVIVE==1],AGE[SURVIVE==3]) #.1882 too high so remove
t.test(HT[SURVIVE==1],HT[SURVIVE==3]) #.002579 Remove, correlated

chisq.test(x=SURVIVE,y=Shock_TYP) #.0007616
chisq.test(x=SURVIVE,y=Sex) #.06376 remove
detach(DAR_Final)
###Conditional plots to determine relationships between measurements
Died<-DAR[which(DAR$SURVIVE==3),]
DAR$RECORD[which(DAR$RECORD==1)]<-""
ggplot(DAR, aes(x=RECORD, y=HG, fill = SURVIVE)) +geom_boxplot(aes(fill=SURVIVE))+labs(x="Measurement", y = "HG")+
  theme_classic()+theme(legend.position="none")
ggplot(DAR, aes(x=RECORD, y=MAP, fill = SURVIVE)) +geom_boxplot(aes(fill=SURVIVE))+labs(x="Measurement", y = "MAP")+
  theme_classic()+theme(legend.position="none")
#Histograms and boxplots
ggplot(Wide_Data, aes(x=Initial_SURVIVE, y=Final_CI)) +geom_boxplot()
ggplot(Wide_Data, aes(x=Initial_SURVIVE, y=Final_AT)) +geom_boxplot()
ggplot(Wide_Data, aes(x=Initial_SURVIVE, y=Final_MCT)) +geom_boxplot()
ggplot(Wide_Data, aes(x=Initial_SURVIVE, y=Final_HG)) +geom_boxplot()
ggplot(Wide_Data, aes(x=Initial_SURVIVE, y=Final_HCT)) +geom_boxplot()
hist(Data_Initial$Initial_UO)
hist(Data_Initial$Initial_MAP,breaks=5)
hist(Data_Initial$Initial_MCVP)
hist(Data_Initial$Initial_BSI)
##Model Fitting
Data_Initial<-DAR_Initial[-c(1,6,7,10,14,13,17,20,21)]
Data_Final<-Wide_Data[-c(1,3,6,7,10,13,14,15,17,20,21,23,26,27,28,30,31,32,34,35,37,38)]
library(MASS)
install.packages("car")
library(car)
Final_Data<-Wide_Data %>% select(Initial_SURVIVE,Initial_Is_Shock,Initial_MAP,Initial_MCVP,Initial_BSI,Initial_UO,Final_HR, Final_CI, Final_HG)
fitallvars<-glm(Initial_SURVIVE~.,family=binomial(link=logit),data=Final_Data)
vif(fitallvars)
#fitallvars<-glm(Initial_SURVIVE~.,family=binomial(link=logit),data=DAR_Initial)
#Final_Data<-Wide_Data %>% select(Initial_SURVIVE,Initial_Is_Shock,Initial_MAP,Initial_MCVP,Initial_BSI,Initial_UO)

#Using Initial Data Only
Model_Initial<-glm(Initial_SURVIVE~.,family=binomial(link=logit),data=Data_Initial)
summary(Model_Initial)
vif(Model_Initial)
Initial_fit=stepAIC(Model_Initial,direction="both")
summary(Initial_fit)
Best_Initial<-glm(Data_Initial$Initial_SURVIVE~Initial_MAP+Initial_MCVP+Initial_BSI+Initial_UO+Initial_Is_Shock,family=binomial(link=logit),data=Data_Initial)
Plus_Sex<-glm(Data_Initial$Initial_SURVIVE~Initial_MAP+Initial_MCVP+Initial_BSI+Initial_UO+Initial_Is_Shock+Initial_Sex,family=binomial(link=logit),data=Data_Initial)
Plus_MCT<-glm(Data_Initial$Initial_SURVIVE~Initial_MAP+Initial_MCVP+Initial_BSI+Initial_UO+Initial_Is_Shock+Initial_MCT,family=binomial(link=logit),data=Data_Initial)
summary(Best_Initial)
summary(Plus_Sex)
summary(Plus_MCT)
anova(Best_Initial,Plus_Sex,test="Chisq")
anova(Best_Initial,Plus_MCT,test="Chisq")

vif(Best_Initial)
#using Final Data Also
Model_All<-glm(Initial_SURVIVE~.,family=binomial(link=logit),data=Data_Final)
All_Fit<-stepAIC(Model_All,direction="both")
vif(All_Fit)
summary(All_Fit)
All_Final<-glm(Data_Final$Initial_SURVIVE~Initial_AGE+Initial_MCVP+Initial_Is_Shock+Final_MAP+Final_PVI+Final_HCT,family=binomial(link=logit),data=Data_Final)
summary(All_Final)
###Diagnostics

one.fourth.root=function(x){
  x^0.25
}
source("examine.logistic.reg.R")
attach(Data_Initial)

#bin continuous covariates to create EVPs
hist(Initial_UO,breaks=2)
breaks <- c(unique(quantile(Initial_UO)))
UO_interval = cut(Initial_UO, breaks, include.lowest = TRUE)
hist(Initial_MAP)
hist(Initial_MAP, breaks=6)
g=4
MAP_interval = cut(Initial_MAP, quantile(Initial_MAP, 0:g/g), include.lowest = TRUE)
hist(Initial_MCVP)
hist(Initial_MCVP,breaks=3)
g=3
MCVP_interval = cut(Initial_MCVP, quantile(Initial_MCVP, 0:g/g), include.lowest = TRUE)
hist(Initial_BSI)
g=4
BSI_interval = cut(Initial_BSI, quantile(Initial_BSI, 0:g/g), include.lowest = TRUE)
Data_Initial$Initial_SURVIVE<-as.numeric(Data_Initial$Initial_SURVIVE)
Data_Initial$Initial_SURVIVE[Data_Initial$Initial_SURVIVE==2]<-0

# Diagnostic plots
w <- aggregate(formula = Initial_SURVIVE ~ BSI_interval+MCVP_interval+MAP_interval + UO_interval + Initial_Is_Shock, data = Data_Initial, FUN = sum)
n <- aggregate(formula = Initial_SURVIVE ~ BSI_interval+MCVP_interval+MAP_interval + UO_interval+ Initial_Is_Shock, data = Data_Initial, FUN = length)

w.n <- data.frame(w, trials = n$Initial_SURVIVE, prop = round(w$Initial_SURVIVE/n$Initial_SURVIVE,2))
mod.prelim1 <- glm(formula = Initial_SURVIVE/trials ~ BSI_interval+MCVP_interval+MAP_interval + UO_interval + Initial_Is_Shock,
                   family = binomial(link = logit), data = w.n, weights = trials)
save1=examine.logistic.reg(mod.prelim1, identify.points=T, scale.n=one.fourth.root, scale.cookd=sqrt)

w.n.diag1=data.frame(w.n, pi.hat=round(save1$pi.hat, 2), std.res=round(save1$stand.resid, 2), 
                     cookd=round(save1$cookd, 2), h=round(save1$h, 2))
p=length(mod.prelim1$coef) # number of parameters in model (# coefficients)
ck.out=abs(w.n.diag1$std.res)>2 | w.n.diag1$cookd>4/nrow(w.n) | w.n.diag1$h > 3*p/nrow(w.n)
extract.EVPs=w.n.diag1[ck.out, ]
extract.EVPs
#Seeing if removing outliers changes anything. Rerun model after this
#80 BSI Interval >150 <=164 and MCVP_Interval >80 <=111, MAP >50.9<=60 UO Interval>1<41.2 Is Shock
#96 <=109 >=150 >111 <=142 >=15 <=50.9 >41.2 <=510 Is Shock
Data_Initial<-DAR_Initial[-c(1,3,6,7,10,14,15,17,20,21)]

Data_Initial<-Data_Initial[-which(Initial_BSI>150 & Initial_BSI<=164 & Initial_MCVP > 80 & Initial_MCVP <=111 & Initial_MAP>50.9 & Initial_MAP<=60 & Initial_UO>1 & Initial_UO<41.2 & Initial_Is_Shock==1 & Initial_SURVIVE==1),]
Data_Initial<-Data_Initial[-which(Initial_BSI>109 & Initial_BSI<=150 & Initial_MCVP > 111 & Initial_MCVP <=142 & Initial_MAP>=15 & Initial_MAP<=50.9 & Initial_UO>41.2 & Initial_UO<510 & Initial_Is_Shock==1 & Initial_SURVIVE==1),]

####Final Model
betahat = format(signif(Best_Initial$coeff , digits =3),digits=2, format ="f", flag ="#")
OR = format(signif(exp(Best_Initial$coeff), digits =3),digits=2, format ="f", flag ="#")
SE = format(signif(summary(Best_Initial)$coeff[,2],digits =3),  digits=2, format ="f", flag ="#")
cibounds = format(signif(exp(confint(Best_Initial)), digits =3),digits=2, format ="f", flag ="#")
pval = format(signif(summary(Best_Initial)$coeff[,4], digits =4),digits=2, format ="f", flag ="#")

matrix=cbind(betahat , OR, SE, pval ,matrix(paste ("(",  cibounds [,1], ",", cibounds [,2], ")")))
colnames(x) = cbind (" Coefficient", "Odds  Ratio","Std. Error", "P-value", "95% CI")

SBP             MAP               HR              DBP        
Min.   : 26.0   Min.   : 15.00   Min.   : 25.00   Min.   : 10.00  
1st Qu.: 85.0   1st Qu.: 59.00   1st Qu.: 85.75   1st Qu.: 44.00  
Median :104.5   Median : 72.50   Median :101.50   Median : 59.00  
Mean   :106.5   Mean   : 73.53   Mean   :104.17   Mean   : 58.61  
3rd Qu.:131.0   3rd Qu.: 88.00   3rd Qu.:119.25   3rd Qu.: 71.25  
Max.   :171.0   Max.   :124.00   Max.   :217.00   Max.   :108.00  
MCVP             BSI              CI              AT              MCT       
Min.   :  2.00   Min.   :109.0   Min.   : 17.0   Min.   : 20.00   Min.   : 81.0  
1st Qu.: 44.00   1st Qu.:157.2   1st Qu.:140.8   1st Qu.: 64.75   1st Qu.:151.0  
Median : 80.00   Median :169.0   Median :226.0   Median : 92.50   Median :194.0  
Mean   : 88.32   Mean   :168.2   Mean   :255.1   Mean   :102.46   Mean   :228.4  
3rd Qu.:125.00   3rd Qu.:180.2   3rd Qu.:348.2   3rd Qu.:132.25   3rd Qu.:296.2  
Max.   :302.00   Max.   :224.0   Max.   :763.0   Max.   :261.00   Max.   :590.0  
UO              PVI              RCI              HG        
Min.   :  0.00   Min.   : 207.0   Min.   :107.0   Min.   : 66.00  
1st Qu.:  0.00   1st Qu.: 377.8   1st Qu.:167.0   1st Qu.: 97.75  
Median :  1.00   Median : 461.5   Median :205.0   Median :112.00  
Mean   : 54.91   Mean   : 486.0   Mean   :214.7   Mean   :114.79  
3rd Qu.: 41.25   3rd Qu.: 563.8   3rd Qu.:241.2   3rd Qu.:133.25  
Max.   :510.00   Max.   :1066.0   Max.   :858.0   Max.   :180.00  
HCT        
Min.   :0.2000  
1st Qu.:0.2975  
Median :0.3375  
Mean   :0.3501  
3rd Qu.:0.4113  
Max.   :0.5400  
