install.packages("readxl")
library(readxl)
hospital<- read_excel("C:/Users/Anantharaj/Desktop/R Project/Healthcare/Hospitalcosts.xlsx")
hospital

#1. To record the patient statistics, the agency wants to find the age category of people who frequently visit the hospital and has the maximum expenditure.
#Frequency of Age using Histogram
#histogram syntax = hist(v,main,xlab,xlim,ylim,breaks,col,border)

hist(hospital$AGE, main = "Frequrncy of Age", xlab = "Age",col = "yellow")
summary(as.factor(hospital$AGE))

#TO find maximum expenditure based on frequency of age
summary(hospital) #summary of table
max(table(hospital$AGE)) #maximum of age freq from table hospital
max(summary(as.factor(hospital$AGE))) #maximum of age freq from table HOSPITAL AS FACTOR r 
which.max(table(hospital$AGE)) #to check which range of age has max freq (0-1 == 307) from the factor

#aggregate: Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form
#method 1 - using aggregate  func, to find max TOTCHG for max age freq (0-1)
age<-aggregate(TOTCHG~AGE, data = hospital, sum)
max(age)

#method 2 - using tappply func (tapply() computes a measure (mean, median, min, max, etc..) or a function for each factor variable in a vector)

tapply(hospital$TOTCHG, hospital$AGE, sum)
which.max(tapply(hospital$TOTCHG, hospital$AGE, sum))

#Result: The max age freq with max expenditure is 0-1 with TOTCHG= 678118

#2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.

APRDRG_factor<- (as.factor(hospital$APRDRG)) #convert table to factor format
APRDRG_factor
summary(APRDRG_factor) #summary value of factor along with levels and Group values
which.max(APRDRG_factor) #max factor value
which.max(summary(APRDRG_factor)) #max of summary of factor value

#method 1 -tapply func used to find the sum value of APRDRG Vs TOTCHG
tapply(hospital$TOTCHG, hospital$APRDRG, sum ) 
which.max(tapply(hospital$TOTCHG, hospital$APRDRG, sum ))
max (tapply(hospital$TOTCHG, hospital$APRDRG, sum ))


#method 2 - aggregate func to find the sum of APRDRG vS  max of TOTCHG
x<- aggregate(APRDRG~TOTCHG, data = hospital, sum )
x
max(x)
which.max(x$TOTCHG)
x[which.max(x$TOTCHG),]

#Result: Thus the diagnosis group with max hospitalization is 911 with total charge of 48388

s#3.To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.
#ANOVA

#ho = the race of the patient is related to the hospitalization costs
#h1 = the race of the patient is not related to the hospitalization costs
library(stats)
race <- (as.factor(hospital$RACE))
race

#to omit NA values from factor
hospitalNA <- na.omit(hospital)
hospitalNA
modelused<-aov(TOTCHG~RACE, data = hospital)
modelused
summary(modelused)
summary(race$RACE)

#since the P value is greater we can reject the null hypothesis
#REsult: no relation between race of patient and hospitalization cost

#4.To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources.
#Linear regression
#h0:there is severity of the hospital costs by age and gender
#h1:there is no severity of the hospital costs by age and gender

modellm1<- lm(TOTCHG~AGE+FEMALE, data=hospital) #using LR for Totchg Vs Age+gender 
modellm1
plot(modellm1, col = "red", main = "TOTCHG Vs AGE+GENDER")

summary(modellm1)
plot(summary(modellm1))

##display gender as factor for female Vs male
hospfemalefact<- as.factor(hospital$FEMALE) 
hospfemalefact
plot(hospfemalefact, col = "green", main = "Expense of female Vs male")

#from the significant levels and P value, there is severity of the hospital costs by age than gender(female)
#The no of females has incurred lesser expense when compared to males


#5.Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.
#h0: There is relation between LOS and AGE+FEMALE+RACE
#H1: There is NO relation between LOS and AGE+FEMALE+RACE

modellm2<- lm(LOS~AGE+FEMALE+RACE, data = hospital)
modellm2
summary(modellm2)
plot(modellm2, col= "blue", main = "LOS Prediction based on Age,Female,Race")

#Since the p value of intercept is less when compared to Age, gender, race
#there is no significance of all independent variables between dependent variables, we can reject null hypothesis


#6. To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs

modellm3<-lm(TOTCHG~AGE+FEMALE+LOS+RACE+APRDRG, data = hospital)
modellm3
summary(modellm3)
plot(modellm3, col = "yellow", main = "Complete analysis of Totchg")

#Result:the variables which affect hospitalization costs are age,los,aprdrg
