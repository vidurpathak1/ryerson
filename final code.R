# Import the dataset and renamed the dataset to 'df3'
# It is important to note that for certain diseases, where there were NA's, those have been removed
#   Not removing them could have potentially caused a bias
#   towards other diseases which had zero NAs. 
modified_ccdss.scsmc.eng <- read.csv("C:/Users/VP/Desktop/all desktop/ryerson/CKME 136/initial code/modified_ccdss-scsmc-eng.csv")
df3<- modified_ccdss.scsmc.eng

# Characteristics of the Dataset
df3
str(df3)
names(df3)
unique(df3)
class(df3)
head(df3)
tail(df3)
unique(df3$Disease)
unique(df3$Fiscal.Year)
unique(df3$Age.Group)
unique(df3$Gender)
unique(df3$Age.Group)

# Some graphs to identify the trends within the dataset
class(df3$Physician.Visits.with.the.disease.case.definition.person.count)

# Graph showing the various diseases in various years
plot(df3$Disease, df3$Fiscal.Year, col, main = "Disease vs Year",xlab = "Disease", ylab = "Year", col = rainbow(9))

# Physician visits to those who have a disease  
plot(df3$Disease, df3$Physician.Visits.with.the.disease.case.definition...Visit.Count., col = rainbow (4),main = "Disease vs Physician visits (to those patients who have the disease)",xlab = "Disease", ylab = "Physician Visits to patients that have the disease" )

# Disease vs Age 
plot(df3$Disease, df3$Age.Group, main = "Disease vs Age",xlab = "Disease", ylab = "Age Category")

# Disease Vs Gender 
plot(df3$Disease, df3$Gender,main = "Disease vs Gender",xlab = "Disease", ylab = "Gender")

# Replace any/all the "N/A"s with "NA" in the dataset
df3[df3=="N/A"]<- NA

# Check the number of NA's within each attribute

is.na(df3)
na_count<- sapply(df,function(y) sum(length(which(is.na(y)))))
na_count<- data.frame(na_count)
na_count

# Graph of the various Diseases
number_of_diseases<-table(df3$Disease) 
barplot(number_of_diseases, col = rainbow(18)) 

#With the plyr package
library(plyr)
count(df3, "Disease") # Counting the total frequency of each disease
count(df3, c("Disease", "Fiscal.Year")) # counts the frequency of the diseases based on Fiscal Year.
aggregate(df3$Disease ~ df3$Fiscal.Year, unique, data = df3)# lists unique diseases per year
aggregate(df3$Age.Group ~ df3$Disease + df3$Fiscal.Year, unique, data = df3) # lists aggregate of ages with diseases and year
unique(df3$Age.Group) # lists the various Age Group Levels. There are 18 in total


#Aggregation of male and female with each Disease + Fiscal Year
aggregate(df3$Age.Group ~ df3$Disease, unique, data = df3) # aggregation of age group with disease
count(df3, c("Disease", "Gender", "Fiscal.Year")) # count of Diseases based on Gender and Fiscal Year


#Correlations

# Here are the correlations among Physicians, General Physicians, Specialists for disease (Person Count).
#Attributes # 14, 18,22
a<- data.frame(df3$Physician.Visits.with.the.disease.case.definition.person.count, df3$General.Physician.Visits.with.the.disease.case.definition.person.count,df3$Specialist.Physician.Visits.with.the.disease.case.definition.person.count)
cor(a)
# Here it can be seen that the correlations are quite high. All the three attributes had a correlation of minimum 0.988. 
# Hence choosing the physican as the predictor variable and the general physician and specialist physican were part of the regression testing.

# Here are the correlations among Physicians, General Physicians, Specialists for disease (Visit Count).
#Attributes # 14,15,19,23
b<- data.frame(df3$Physician.Visits.with.the.disease.case.definition.person.count, df3$Physician.Visits.with.the.disease.case.definition...Visit.Count., df3$General.Physician.Visits.with.the.disease.case.definition..Visit.Count., df3$Specialist.Physician.Visits.with.the.disease.case.definition..Visit.Count.)
cor(b)
#In this scenario, the correlations ranged from 0.899 to 0.992

# Here are the correlations among Physicians, General Physicians, Specialists those who saw patients without the disease (Visit Count + Person Count) with Physician who saw patients with the disease.
#Attributes # 14,16,17,20,21,24,25
c<- data.frame(df3$Physician.Visits.with.the.disease.case.definition.person.count, df3$Physician.Visits.without.the.disease.case.definition..Visit.Count.,df3$General.Physician.Visits.without.the.disease.case.definition.person.count, df3$General.Physician.Visits.without.the.disease.case.definition..Visit.Count., df3$Specialist.Physician.Visits.without.the.disease.case.definition.person.count, df3$Specialist.Physician.Visits.without.the.disease.case.definition..Visit.Count.)
cor(c)
#This resulted in negative correlations, thereby indicating that the attributes in correlation a were the highest and hence those attributes must be used. 

# Here are the correlations between hospital visits, physician visits, general physician visits and specialists vists
d<- data.frame(df3$Hospitalizations.with.the.disease.case.definition.person.count, df3$Physician.Visits.with.the.disease.case.definition.person.count, df3$General.Physician.Visits.with.the.disease.case.definition.person.count, df3$Specialist.Physician.Visits.with.the.disease.case.definition.person.count)
cor(d)

##
# Through various analyses, it was discovered that Asthma was one of the diseases
#   that was most prevlaent in Canada, in terms of frequency of visiting Physicians, 
#   high incidence numbers, etc. so the remaining part of this analysis is based on Asthma.

# Disease: Asthma (for this disease, just as a random pick (using the random generator below) - all males are chosen here)
# Starting with model 65 for the asthma disease
# Here asthma is being divided into training and testing groups (and models) based on 
#   age groups, gender, fiscal year. 

random_generator_gender<- sample(c("Male", "Female"), size = 1, replace = TRUE)

asthma1_training_2<- df3[1661:1948,]
asthma1_testing_2<- df3[1949:2092,]

# Age.Group = 20 to 24; Gender = M
asthma1_training_4 <- asthma1_training_2[asthma1_training_2$Gender=='M' & asthma1_training_2$Age.Group=='20 to 24',]
asthma1_testing_4<- asthma1_testing_2[asthma1_testing_2$Gender=='M' & asthma1_testing_2$Age.Group=='20 to 24',]

# Age. Group = 55 to 59; Gender = M
asthma1_training_3 <- asthma1_training_2[asthma1_training_2$Gender=='M' & asthma1_training_2$Age.Group=='55 to 59',]
asthma1_testing_3<- asthma1_testing_2[asthma1_testing_2$Gender=='M' & asthma1_testing_2$Age.Group=='55 to 59',]

# Age.Group = 75 to 79; Gender = M
asthma1_training_5 <- asthma1_training_2[asthma1_training_2$Gender=='M' & asthma1_training_2$Age.Group=='75 to 79',]
asthma1_testing_5<- asthma1_testing_2[asthma1_testing_2$Gender=='M' & asthma1_testing_2$Age.Group=='75 to 79',]

# Below are the age groups for various age groups in this order: 55 to 59 (model65i); 20 to 24 (model65ii); 75 to 79 (model65iii)
model65i<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_3)
model65ii<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_4)
model65iii<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_5)

model66<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year + Age.Group, data = asthma1_training_2)
model67<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year + Gender, data = asthma1_training_2)
model68<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year + Gender + Age.Group, data = asthma1_training_2)

result65i<- predict(model65i, newdata = asthma1_testing_3, interval = "pred")
result65ii<- predict(model65ii, newdata = asthma1_testing_4, interval = "pred")
result65iii<- predict(model65iii, newdata = asthma1_testing_5, interval = "pred")

result66<- predict(model66, newdata = asthma1_testing_2, interval = "pred")
result67<- predict(model67, newdata = asthma1_testing_2, interval = "pred")
result68<- predict(model68, newdata = asthma1_testing_2, interval = "pred")

# Regression Models for asthma (Output Attribute = Physician Visits with the disease case definition person count)
# Below are the age groups for various age groups in this order: 55 to 59 (model69i); 20 to 24 (model53ii); 75 to 79 (model53iii)

model69i<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_3)
model69ii<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_4)
model69iii<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_5)

model70<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Age.Group, data = asthma1_training_2)
model71<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Gender, data = asthma1_training_2)
model72<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Gender + Age.Group, data = asthma1_training_2)

result69i<- predict(model69i, newdata = asthma1_testing_3, interval = "pred")
result69ii<- predict(model69ii, newdata = asthma1_testing_4, interval = "pred")
result69iii<- predict(model69iii, newdata = asthma1_testing_5, interval = "pred")

result70<- predict(model70, newdata = asthma1_testing_2, interval = "pred")
result71<- predict(model71, newdata = asthma1_testing_2, interval = "pred")
result72<- predict(model72, newdata = asthma1_testing_2, interval = "pred")

# Regression Models for asthma (Output Attribute = General Physician Visits with the disease case definition person count)
# Below are the age groups for various age groups in this order: 55 to 59 (model73i); 20 to 24 (model73ii); 75 to 79 (model73iii)

model73i<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_3)
model73ii<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_4)
model73iii<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_5)

model74<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Age.Group, data = asthma1_training_2)
model75<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Gender, data = asthma1_training_2)
model76<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Gender + Age.Group, data = asthma1_training_2)

result73i<- predict(model73i, newdata = asthma1_testing_3, interval = "pred")
result73ii<- predict(model73ii, newdata = asthma1_testing_4, interval = "pred")
result73iii<- predict(model73iii, newdata = asthma1_testing_5, interval = "pred")

result74<- predict(model74, newdata = asthma1_testing_2, interval = "pred")
result75<- predict(model75, newdata = asthma1_testing_2, interval = "pred")
result76<- predict(model76, newdata = asthma1_testing_2, interval = "pred")

# Regression Models for asthma (Output Attribute = Specialist Physician Visits with the disease case definition person count)
# Below are the age groups for various age groups in this order: 55 to 59 (model77i); 20 to 24 (model77ii); 75 to 79 (model77iii)

model77i<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_3)
model77ii<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_4)
model77iii<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year, data = asthma1_training_5)

model78<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Age.Group, data = asthma1_training_2)
model79<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Gender, data = asthma1_training_2)
model80<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + Gender + Age.Group, data = asthma1_training_2)

result77i<- predict(model77i, newdata = asthma1_testing_3, interval = "pred")
result77ii<- predict(model77ii, newdata = asthma1_testing_4, interval = "pred")
result77iii<- predict(model77iii, newdata = asthma1_testing_5, interval = "pred")

result78<- predict(model78, newdata = asthma1_testing_2, interval = "pred")
result79<- predict(model79, newdata = asthma1_testing_2, interval = "pred")
result80<- predict(model80, newdata = asthma1_testing_2, interval = "pred")

# Plotting various graphs to have a better understanding

#Age group = 55 to 59, gender = male
plot(asthma1_training_3$Fiscal.Year, asthma1_training_3$Physician.Visits.with.the.disease.case.definition.person.count, xlab = "Fiscal Year", ylab = "Physician Visits (with Disease)", main = "Trend of Physician Visits (Ages: 55 to 59) with Asthma")
plot(asthma1_training_3$Fiscal.Year, asthma1_training_3$General.Physician.Visits.with.the.disease.case.definition.person.count)
plot(asthma1_training_3$Fiscal.Year, asthma1_training_3$Specialist.Physician.Visits.with.the.disease.case.definition.person.count)

#Age group = 20 to 24, gender = male
plot(asthma1_training_4$Fiscal.Year, asthma1_training_4$Physician.Visits.with.the.disease.case.definition.person.count)
plot(asthma1_training_4$Fiscal.Year, asthma1_training_4$General.Physician.Visits.with.the.disease.case.definition.person.count)
plot(asthma1_training_4$Fiscal.Year, asthma1_training_4$Specialist.Physician.Visits.with.the.disease.case.definition.person.count)

#Age group = 75 to 79, gender = male
plot(asthma1_training_5$Fiscal.Year, asthma1_training_5$Physician.Visits.with.the.disease.case.definition.person.count)
plot(asthma1_training_5$Fiscal.Year, asthma1_training_5$General.Physician.Visits.with.the.disease.case.definition.person.count)
plot(asthma1_training_5$Fiscal.Year, asthma1_training_5$Specialist.Physician.Visits.with.the.disease.case.definition.person.count)
  

# Predicting what the outcome would be in future years 
#   This Chronic Disease dataset ranges from 1999/2000 to 2010/2011.

pred.frame<- data.frame(Fiscal.Year = c(2012,2013,2014)) #prediction model

modelpred65i<-predict(model65i, interval = "pred", newdata = pred.frame)
modelpred65ii<-predict(model65ii, interval = "pred", newdata = pred.frame)
modelpred65iii<-predict(model65iii, interval = "pred", newdata = pred.frame)

modelpred69i<-predict(model69i, interval = "pred", newdata = pred.frame)
modelpred69ii<-predict(model69ii, interval = "pred", newdata = pred.frame)
modelpred69iii<-predict(model69iii, interval = "pred", newdata = pred.frame)

modelpred73i<-predict(model73i, interval = "pred", newdata = pred.frame)
modelpred73ii<-predict(model73ii, interval = "pred", newdata = pred.frame)
modelpred73iii<-predict(model73iii, interval = "pred", newdata = pred.frame)

modelpred77i<-predict(model77i, interval = "pred", newdata = pred.frame)
modelpred77ii<-predict(model77ii, interval = "pred", newdata = pred.frame)
modelpred77iii<-predict(model77iii, interval = "pred", newdata = pred.frame)

modelpred78<- predict(model78, interval = "pred", newdata = pred.frame) # why is this not working?

#The following predictions are using the whole dataset for Asthma (aggregate in nature), 
# whereas the dataset above uses the training set to make predictions.

all_training1<- df3[1:3344,]
all_testing1<- df3[2509:3344,]

#Hospitalizations - Person Count
all_model1<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Disease + Fiscal.Year + Gender + Age.Group, data=all_training1)
all_pred.frame1i<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="55 to 59")
all_pred.frame1ii<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="20 to 24")
all_pred.frame1iii<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="75 to 79")

all_pred1i<- predict(all_model1, interval = "pred", newdata= all_pred.frame1i)
all_pred1ii<- predict(all_model1, interval = "pred", newdata= all_pred.frame1ii)
all_pred1iii<- predict(all_model1, interval = "pred", newdata= all_pred.frame1iii)
all_pred1i

#Physician - Person Count
all_model2<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Disease + Fiscal.Year + Gender + Age.Group, data=all_training1)
all_pred.frame2i<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="55 to 59")
all_pred.frame2ii<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="20 to 24")
all_pred.frame2iii<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="75 to 79")
all_pred.frame2i
all_pred2i<- predict(all_model2, interval = "pred", newdata= all_pred.frame2i)
all_pred2ii<- predict(all_model2, interval = "pred", newdata= all_pred.frame2ii)
all_pred2iii<- predict(all_model2, interval = "pred", newdata= all_pred.frame2iii)
all_pred2i

#General Physician - Person Count
all_model3<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Disease + Fiscal.Year + Gender + Age.Group, data=all_training1)
all_pred.frame3i<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="55 to 59")
all_pred.frame3ii<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="20 to 24")
all_pred.frame3iii<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="75 to 79")

all_pred3i<- predict(all_model3, interval = "pred", newdata= all_pred.frame3i)
all_pred3ii<- predict(all_model3, interval = "pred", newdata= all_pred.frame3ii)
all_pred3iii<- predict(all_model3, interval = "pred", newdata= all_pred.frame3iii)
all_pred3i

#Specialist - Person Count
all_model4<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Disease + Fiscal.Year + Gender + Age.Group, data=all_training1)
all_pred.frame4i<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="55 to 59")
all_pred.frame4ii<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="20 to 24")
all_pred.frame4iii<- data.frame(Disease = "Asthma", Fiscal.Year = c(2012,2013,2014), Gender = "M", Age.Group ="75 to 79")

all_pred4i<- predict(all_model4, interval = "pred", newdata= all_pred.frame4i)
all_pred4ii<- predict(all_model4, interval = "pred", newdata= all_pred.frame4ii)
all_pred4iii<- predict(all_model4, interval = "pred", newdata= all_pred.frame4iii)
all_pred4i

modelpred65i
all_result1<- predict(all_model1, interval = "pred", newdata = all_testing1) # rough point
summary(all_pred1)
all_pred1
head(all_pred1)

# Cross Validation (CV) of the Asthma dataset using the K-fold CV technique for 
#   the model numbers correspond to the ones as mentioned above - 
#   based on age groups, Hospitlizations and different types of doctors
#   Used the DAAG package to use this kk-fold CV technique

mydata_asthma1<- data.frame(df3[1661:2092,])
library(DAAG)

# Below is the CV for Hospitalization Person Count (with disease)
# Below are the age groups for various age groups in this order: 55 to 59 (model65i); 20 to 24 (model65ii); 75 to 79 (model65iii)

cv_1i<- cv.lm(data = df3[1661:2092,] , model65i, m=3) 
cv_1ii<- cv.lm(data = df3[1661:2092,] , model65ii, m=3)
cv_1iii<- cv.lm(data = df3[1661:2092,] , model65iii, m=3)

# Below is the CV for Physician Person Count (with disease)
# Below are the age groups for various age groups in this order: 55 to 59 (model69i); 20 to 24 (model69ii); 75 to 79 (model69iii)

cv_2i<- cv.lm(data = df3[1661:2092,] , model69i, m=3)
cv_2ii<- cv.lm(data = df3[1661:2092,] , model69ii, m=3)
cv_2iii<-cv.lm(data = df3[1661:2092,] , model69iii, m=3)

# Below is the CV for General Physician Person Count (with disease)
# Below are the age groups for various age groups in this order: 55 to 59 (model73i); 20 to 24 (model73ii); 75 to 79 (model73iii)

cv_3i<-cv.lm(data = df3[1661:2092,] , model73i, m=3)
cv_3ii<-cv.lm(data = df3[1661:2092,] , model73ii, m=3)
cv_3iii<-cv.lm(data = df3[1661:2092,] , model73iii, m=3)

# Below is the CV for Hospitalization Person Count (with disease)
# Below are the age groups for various age groups in this order: 55 to 59 (model77i); 20 to 24 (model77ii); 75 to 79 (model77iii)

cv_4i<-cv.lm(data = df3[1661:2092,] , model77i, m=3)
cv_4ii<-cv.lm(data = df3[1661:2092,] , model77ii, m=3)
cv_4iii<-cv.lm(data = df3[1661:2092,] , model77iii, m=3)

# summary of a CV model
a100<- cv.lm(data = df3[1661:2092,c(1:4,8,14,18,22)] , model65i, m=3) 
summary(a100)
plot(a100)

# Using the forecast model1 - a new model to forecast the data in the upcoming years with a new package 
#   called 'forecast'. This helped verify the manner (direction) in which the data shall moving, i.e. 
#   whether there will be an increase or decrease in the number of patients in the coming years
library(forecast) 

# Forecast for Specialists for the upcoming years (2012 to 2014)
value1<- df3$Specialist.Physician.Visits.with.the.disease.case.definition.person.count
sensor1<- ts(value1, frequency = 1999,2012)
fit1<- auto.arima(sensor1)
fcast1<- forecast(fit1)
plot(fcast1, xlab = "Fiscal Year", ylab = "Specialist Physician (Person Count)")

# Forecast for Physicians for the upcoming years (2012 to 2014)
value2<- df3$Physician.Visits.with.the.disease.case.definition.person.count
sensor2<- ts(value2, frequency = 1999,2012)
fit2<- auto.arima(sensor2)
fcast2<- forecast(fit2)
plot(fcast2, xlab = "Fiscal Year", ylab = "Physician (Person Count)")

# Forecast for General Physicians for the upcoming years (2012 to 2014)
value3<- df3$General.Physician.Visits.with.the.disease.case.definition.person.count
sensor3<- ts(value3, frequency = 1999,2012)
fit3<- auto.arima(sensor3)
fcast3<- forecast(fit3)
plot(fcast3, xlab = "Fiscal Year", ylab = "General Physician (Person Count)")

# Forecast for Hospital Visits (Person Count) for the upcoming years (2012 to 2014)
value4<- df3$Hospitalizations.with.the.disease.case.definition.person.count
sensor4<- ts(value4, frequency = 1999,2012)
fit4<- auto.arima(sensor4)
fcast4<- forecast(fit4)
plot(fcast4, xlab = "Fiscal Year", ylab = "Hospitalization (Person Count)")

# In the following, in trying to improve the predictions (above) by checking if polynomial regression models
# would help in reducing the RMSE. 

# Below are the polynomials models for Hospitalizations Person Count (with disease)
# Below are the age groups for various age groups in this order: 55 to 59 (model65i); 20 to 24 (model65ii); 75 to 79 (model65iii)

m1_model65i<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_3)
m1_model65ii<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_4)
m1_model65iii<- lm(Hospitalizations.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_5)

m1_result65i<- predict(m1_model65i, interval = "pred", newdata = asthma1_testing_3)
m1_result65ii<- predict(m1_model65ii, interval = "pred", newdata = asthma1_testing_4)
m1_result65iii<- predict(m1_model65iii, interval = "pred", newdata = asthma1_testing_5)

# Below are the polynomials models for Physician Person Count (with disease)
# Below are the age groups for various age groups in this order: 55 to 59 (model69i); 20 to 24 (model69ii); 75 to 79 (model69iii)

m2_model69i<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_3)
m2_model69ii<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_4)
m2_model69iii<- lm(Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_5)

m2_result69i<- predict(m2_model69i, interval = "pred", newdata = asthma1_testing_3)
m2_result69ii<- predict(m2_model69ii, interval = "pred", newdata = asthma1_testing_4)
m2_result69iii<- predict(m2_model69iii, interval = "pred", newdata = asthma1_testing_5)

# Below are the polynomials models for General Physician Person Count (with disease)
# Below are the age groups for various age groups in this order: 55 to 59 (model73i); 20 to 24 (model73ii); 75 to 79 (model73iii)

m3_model73i<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_3)
m3_model73ii<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_4)
m3_model73iii<- lm(General.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_5)

m3_result73i<- predict(m3_model73i, interval = "pred", newdata = asthma1_testing_3)
m3_result73ii<- predict(m3_model73ii, interval = "pred", newdata = asthma1_testing_4)
m3_result73iii<- predict(m3_model73iii, interval = "pred", newdata = asthma1_testing_5)

# Below are the polynomials models for General Physician Person Count (with disease)
# Below are the age groups for various age groups in this order: 55 to 59 (model77i); 20 to 24 (model77ii); 75 to 79 (model77iii)

m4_model77i<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_3)
m4_model77ii<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_4)
m4_model77iii<- lm(Specialist.Physician.Visits.with.the.disease.case.definition.person.count ~ Fiscal.Year + I(Fiscal.Year^2) + I(Fiscal.Year^3), data = asthma1_training_5)

m4_result77i<- predict(m4_model77i, interval = "pred", newdata = asthma1_testing_3)
m4_result77ii<- predict(m4_model77ii, interval = "pred", newdata = asthma1_testing_4)
m4_result77iii<- predict(m4_model77iii, interval = "pred", newdata = asthma1_testing_5)


