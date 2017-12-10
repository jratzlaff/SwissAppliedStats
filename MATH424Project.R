pairs(swiss)
baseModel = lm(swiss$Fertility ~ swiss$Agriculture + swiss$Examination + swiss$Education + swiss$Catholic + swiss$Infant.Mortality)
summary(baseModel)
hetBaseModel=lm(swiss$Fertility ~ swiss$Catholic)

plot(fitted(hetBaseModel),resid(hetBaseModel))
removedBaseModel = lm(swiss$Fertility ~ swiss$Education + swiss$Catholic + swiss$Infant.Mortality)
summary(removedBaseModel)

hetModel = lm(swiss$Fertility ~ swiss$Infant.Mortality)
#summary(hetModel)
plot(fitted(hetModel),resid(hetModel))

logExamModel = lm(swiss$Fertility ~ swiss$Agriculture + log(swiss$Examination) + swiss$Education + swiss$Catholic + swiss$Infant.Mortality)
summary(logExamModel)

dumDenomExam = 1/(swiss$Examination)

ExamDenomModel = lm(swiss$Fertility ~ swiss$Agriculture + dumDenomExam + swiss$Education + swiss$Catholic + swiss$Infant.Mortality)
summary(ExamDenomModel)

plot(1/(swiss$Examination), swiss$Fertility)
pairs(swiss)
resid(ExamDenomModel)
plot(fitted(ExamDenomModel),resid(ExamDenomModel))
plot(fitted(baseModel),resid(baseModel))
#Note: Try the base model excluding the swiss$Examination variable to see how it reacts.

dummy = swiss
x = c()
for(i in 1:47) {
  if (dummy$Catholic[i] >= 75) {
    x[i] = "Catholic"
  }
  else {
    x[i] = "Average"
    #Decided simple split was better than more complicated.
    #if(dummy$Catholic[i] >= 25){
    #  x[i] = "Average"
    #}
    #else{
    #  x[i] = "Protestant"
    #}
  }
}
dummy$Catholic = x

EndModel = lm(dummy$Fertility ~ dummy$Agriculture + dummy$Education + dummy$Catholic + dummy$Infant.Mortality )
summary(EndModel)
plot()
#Decided this was the best music
hetEndModel=lm(dummy$Fertility ~ dummy$Catholic)
plot(fitted(hetEndModel),resid(hetEndModel))
removedOutliers = head(swiss, 44)

RemovedExamDenomModel = lm(removedOutliers$Fertility ~ removedOutliers$Agriculture + 1/(removedOutliers$Examination) + removedOutliers$Education + removedOutliers$Catholic + removedOutliers$Infant.Mortality)
#Decided to keep outliers
RemovedNoAgExamDenomModel = lm(removedOutliers$Fertility ~ 1/(removedOutliers$Examination) + removedOutliers$Education + removedOutliers$Catholic + removedOutliers$Infant.Mortality)
#Decided to keep Ag
dummyRemovedOutliers = head(dummy, 44)
dummy = swiss
x = c()
for(i in 1:44) {
  if (dummy$Catholic[i] >= 75) {
    x[i] = "Catholic"
  }
  else {
    x[i] = "Average"
    #if(dummy$Catholic[i] >= 25){
    #  x[i] = "Average"
    #}
    #else{
    #  x[i] = "Protestant"
    #}
  }
}
dummyRemovedOutliers$Catholic = x

RemovedEndModel = lm(dummyRemovedOutliers$Fertility ~ dummyRemovedOutliers$Agriculture + 1/dummyRemovedOutliers$Examination + dummyRemovedOutliers$Education + dummyRemovedOutliers$Catholic + dummyRemovedOutliers$Infant.Mortality )
#Decided to keep the outliers



#Check Residuals
plot(fitted(EndModel),resid(EndModel))