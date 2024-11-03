library(tidyverse)
#Cleaning up the Dataset
insurance <- insurance %>% 
  mutate(smoker = ifelse(smoker == "yes", 1, 0))
insurance <- insurance %>% 
  mutate( sex = ifelse(sex == "female", 1, 0))

#Creating linear models
attach(insurance)

linear=lm(expenses~age)
model=lm(expenses~age+sex+bmi+children+smoker+factor(region))
model2=lm(expenses~age+sex+bmi+children+smoker+factor(region)+smoker*factor(region))

summary(linear)
summary(model)
summary(model2)

#Cross validation
num_sims=100
ModelvsModel2=array(dim=c(num_sims))
for(i in 1:num_sims){
  nrow_index= sample(c(1:nrow(insurance)),round(0.7*nrow(insurance),0))
  
  InsuranceTrain=insurance[nrow_index,]
  InsuranceTest=insurance[-nrow_index,]
  
  Modeltrain=lm(expenses~age+sex+bmi+children+smoker+factor(region),data = InsuranceTrain)
  Model2train=lm(expenses~age+sex+bmi+children+smoker+factor(region)+smoker*factor(region), data = InsuranceTrain)

  MeanSquareErrors=sqrt(mean((InsuranceTest$expense-predict(Modeltrain,newdata = InsuranceTest))^2)) 
  MeanSquareErrors2=sqrt(mean((InsuranceTest$expense-predict(Model2train,newdata = InsuranceTest))^2))
 
  
  ModelvsModel2[i]=MeanSquareErrors<MeanSquareErrors2
}
summary(ModelvsModel2)

summary(model2)
