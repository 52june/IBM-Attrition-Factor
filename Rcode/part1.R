dat<-read.csv("IBM data.csv",header=T,stringsAsFactors = T)
View(dat)
dat$Education<-factor(dat$Education,ordered=T)
dat$EnvironmentSatisfaction<-factor(dat$EnvironmentSatisfaction,ordered=T)
dat$JobInvolvement<-factor(dat$JobInvolvement,ordered=T)
dat$RelationshipSatisfaction<-factor(dat$RelationshipSatisfaction,ordered=T)
dat$WorkLifeBalance<-factor(dat$WorkLifeBalance,ordered=T)
dat$PerformanceRating<-factor(dat$PerformanceRating,ordered=T)
dat$JobLevel<-factor(dat$JobLevel,ordered=T)
dat$StockOptionLevel<-factor(dat$StockOptionLevel,ordered=T)

####visualization
library(scales)
yes<-dat[dat$Attrition=="Yes",]
no<-dat[dat$Attrition=="No",]
library(ggplot2)

ggplot(dat, aes(BusinessTravel, group = Attrition)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  facet_grid(~Attrition)

ggplot(dat,aes(x=Attrition,fill=BusinessTravel))+
  geom_bar(position='fill',color="black")+ scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = percent_format())

ggplot(dat,aes(x=Attrition,fill=EducationField))+
  geom_bar(position='fill',color="black")+ scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = percent_format())

ggplot(dat,aes(x=Attrition,fill=EnvironmentSatisfaction))+
  geom_bar(position='fill',color="black")+ scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = percent_format())

ggplot(dat,aes(x=Attrition,fill=JobInvolvement))+
  geom_bar(position='fill',color="black")+ scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = percent_format())

ggplot(dat,aes(x=Attrition,fill=MaritalStatus))+
  geom_bar(position='fill',color="black")+ scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = percent_format())

ggplot(dat,aes(x=Attrition,fill=OverTime))+
  geom_bar(position='fill',color="black")+ scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = percent_format())

ggplot(dat,aes(x=Attrition,fill=RelationshipSatisfaction))+
  geom_bar(position='fill',color="black")+ scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = percent_format())

ggplot(dat,aes(x=Attrition,fill=WorkLifeBalance))+
  geom_bar(position='fill',color="black")+ scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels = percent_format())

ggplot(dat, aes(DistanceFromHome, group = Attrition)) + 
  geom_histogram(bins=9,aes(y = ..prop..),stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  facet_grid(~Attrition)

ggplot(yes,aes(x=DistanceFromHome))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")
ggplot(no,aes(x=DistanceFromHome))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")

ggplot(yes,aes(x=Age))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")
ggplot(no,aes(x=Age))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")

ggplot(yes,aes(x=TotalWorkingYears))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")
ggplot(no,aes(x=TotalWorkingYears))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")

ggplot(yes,aes(x=MonthlyIncome))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")
ggplot(no,aes(x=MonthlyIncome))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")

ggplot(yes,aes(x=YearsAtCompany))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")
ggplot(no,aes(x=YearsAtCompany))+
  geom_histogram(aes(y=(..count..)/sum(..count..)),bins=9,fill="white",color="black")

####logistic
dat$Over18<-unclass(dat$Over18)

null<-glm(Attrition~1,data=dat,family="binomial")
full<-glm(Attrition~.,data=dat,family="binomial")
step(null,scope = list(lower=null,upper=full),direction = "both")
aic.fit<-glm(formula = Attrition ~ OverTime + JobRole + StockOptionLevel + 
           JobLevel + EnvironmentSatisfaction + BusinessTravel + JobInvolvement + 
           JobSatisfaction + DistanceFromHome + NumCompaniesWorked + 
           TotalWorkingYears + WorkLifeBalance + RelationshipSatisfaction + 
           YearsSinceLastPromotion + YearsInCurrentRole + TrainingTimesLastYear + 
           EducationField + Gender + Age + YearsWithCurrManager + YearsAtCompany + 
           DailyRate, family = "binomial", data = dat)

step(null,scope = list(lower=null,upper=aic.fit),direction = "both",k=log(nrow(dat)))

bic.fit<-glm(formula = Attrition ~ OverTime + StockOptionLevel + JobSatisfaction + 
               JobInvolvement + BusinessTravel + DistanceFromHome + YearsInCurrentRole + 
               YearsSinceLastPromotion + EnvironmentSatisfaction + NumCompaniesWorked + 
               TotalWorkingYears, family = "binomial", data = dat)

summary(bic.fit)
library(lmtest)
lrtest(bic.fit)
dwtest(bic.fit)
