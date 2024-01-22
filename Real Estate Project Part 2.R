#Price of a property is one of the most important decision criterion when people buy homes. 
#Real state firms need to be consistent in their pricing in order to attract buyers . 
#Having a predictive model for the same will be great tool to have , which in turn can also be used to tweak development of properties , 
#putting more emphasis on qualities which increase the value of the property.



#We have given you two datasets , housing_train.csv and housing_test.csv .
#You need to use data housing_train to build predictive model for response variable "Price".
#Housing_test data contains all other factors except "Price", you need to predict that using the model that you developed and submit your predicted values in a csv files.



#Evaluation Criterion : 
#Score will be calculated as:
#Score = 212467/RMSE (Note : Dont worry about change in scoring method , this is just a cosmetic change to alter scale of score , passing criterion hasn't changed and you dont need to resubmit )
#Where RMSE is root mean square error on test file. 

# Your score for test data should come out to be more than 0.51
# You are NOT required to submit R script. However in some cases , we might ask you to send your script separately in order to verify that your submissions is a result of models that you built .
#Your predictions should not contain any NA values



library(tidymodels)
library(visdat)
library(tidyr)
library(car)

house_train= read.csv(r'(D:\Raja\R\Project 1 Real Estate\housing_train.csv)',  sep = ',', stringsAsFactors = F)
house_test= read.csv(r'(D:\Raja\R\Project 1 Real Estate\housing_test.csv)',  sep = ',', stringsAsFactors = F)
House = house_train
House_t= house_test

## discuss data
names(House)
names(House_t)
## Target Variable 
setdiff(names(House), names(House_t)) 
## over view of data 
head(House) 
summary(House)
dim(House)
View(House)

vis_dat(House)

## checking relation ship between variables 
cor.test(House$Bedroom2, House$Rooms)
cor.test(House$BuildingArea, House$Landsize)

## according to cor test Rooms and bedroom are almost similar data so we can drop any one. 

## converting to character for dummy creation and creating new column for test data 

House$Postcode = as.character(House$Postcode)

House_t$Price = NA

## preparing data 
##Rooms,Distance,Car,BuildingArea,Suburb,Type,Method,CouncilArea,SellerG

dp_pipe= recipe(Price ~ . ,data=House) %>% 
  update_role(Address,Bedroom2,YearBuilt,Postcode, new_role = 'drop_vars') %>%
  update_role(Rooms,Distance,Car,Landsize,BuildingArea,Bathroom, new_role = 'to_numeric') %>% 
  update_role(Suburb, Method, Type,CouncilArea, SellerG,new_role = 'to_dummy') %>%
  step_unknown(has_role('to_dummy'), new_level = '__missing__') %>%
  step_other(has_role('to_dummy'), threshold = 0.01,other = '__other__') %>% 
  step_rm(has_role('drop_vars')) %>% 
  step_dummy(has_role('to_dummy')) %>% 
  step_mutate_at(has_role('to_numeric'),fn=as.numeric) %>%
  step_impute_median(all_numeric(),-all_outcomes())
  

dp_pipe = prep(dp_pipe)

train = bake(dp_pipe,new_data = NULL)
test = bake(dp_pipe, new_data = House_t)

view(train)

set.seed(2)
s= sample(1:nrow(train),0.8*nrow(train))
## actual model train data 
t1= train[s,]
## validation set 
t2=train[-s,]

## creating model and taking VIF cutoff = 5
fit= lm(Price ~ .-Type_X__other__ -Suburb_X__other__, data = t1 )
sort(vif(fit), decreasing = T)

alias(fit)

summary(fit)

##looking for bext fit model according to AIC value 

fit=stats::step(fit)
summary(fit)

fit= lm(Price ~ Rooms + Distance + Bathroom + Car + Landsize + BuildingArea + 
          Suburb_Balwyn + Suburb_Balwyn.North + Suburb_Bentleigh + 
          Suburb_Brighton + Suburb_Brighton.East + Suburb_Camberwell + 
          Suburb_Carnegie + Suburb_Doncaster + Suburb_Elwood + Suburb_Essendon + 
          Suburb_Glen.Iris + Suburb_Glenroy + Suburb_Hampton + Suburb_Hawthorn + 
          Suburb_Keilor.East + Suburb_Kew + Suburb_Malvern.East + Suburb_Moonee.Ponds + 
          Suburb_Newport + Suburb_Port.Melbourne + Suburb_Prahran + 
          Suburb_Preston + Suburb_Reservoir + Suburb_Richmond + Suburb_Thornbury + 
          Suburb_Toorak + Type_t + Type_u + Method_S + Method_SP + 
          Method_X__other__ + SellerG_Buxton + SellerG_Fletchers + 
          SellerG_Greg + SellerG_Jellis + SellerG_Kay + SellerG_Marshall + 
          SellerG_Miles + SellerG_RT + SellerG_Sweeney + 
          CouncilArea_Banyule + CouncilArea_Bayside + 
          CouncilArea_Boroondara + CouncilArea_Brimbank + CouncilArea_Darebin + 
          CouncilArea_Hobsons.Bay + CouncilArea_Manningham + CouncilArea_Maribyrnong + 
          CouncilArea_Melbourne + CouncilArea_Moonee.Valley + CouncilArea_Moreland + 
          CouncilArea_Stonnington + CouncilArea_Whitehorse + CouncilArea_Yarra + 
          CouncilArea_X__other__, data = t1)





summary(fit)


t2.pred= predict(fit, newdata = t2)
t1.pred= predict(fit, newdata = t1)
errors_train = t1$Price - t1.pred 
errors_test = t2$Price - t2.pred

rmse_train= errors_train**2 %>% mean() %>% sqrt()
rmse_test= errors_test**2 %>% mean() %>% sqrt()


## checking criteria
212467/rmse_train


##final fit model

fit.final = lm (Price ~ Rooms + Distance + Bathroom + Car + Landsize + BuildingArea + 
                  Suburb_Balwyn + Suburb_Balwyn.North + Suburb_Bentleigh + 
                  Suburb_Brighton + Suburb_Brighton.East + Suburb_Camberwell + 
                  Suburb_Carnegie + Suburb_Doncaster + Suburb_Elwood + Suburb_Essendon + 
                  Suburb_Glen.Iris + Suburb_Glenroy + Suburb_Hampton + Suburb_Hawthorn + 
                  Suburb_Keilor.East + Suburb_Kew + Suburb_Malvern.East + Suburb_Moonee.Ponds + 
                  Suburb_Newport + Suburb_Port.Melbourne + Suburb_Prahran + 
                  Suburb_Preston + Suburb_Reservoir + Suburb_Richmond + Suburb_Thornbury + 
                  Suburb_Toorak + Type_t + Type_u + Method_S + Method_SP + 
                  Method_X__other__ + SellerG_Buxton + SellerG_Fletchers + 
                  SellerG_Greg + SellerG_Jellis + SellerG_Kay + SellerG_Marshall + 
                  SellerG_Miles + SellerG_RT + SellerG_Sweeney + 
                  CouncilArea_Banyule + CouncilArea_Bayside + 
                  CouncilArea_Boroondara + CouncilArea_Brimbank + CouncilArea_Darebin + 
                  CouncilArea_Hobsons.Bay + CouncilArea_Manningham + CouncilArea_Maribyrnong + 
                  CouncilArea_Melbourne + CouncilArea_Moonee.Valley + CouncilArea_Moreland + 
                  CouncilArea_Stonnington + CouncilArea_Whitehorse + CouncilArea_Yarra + 
                  CouncilArea_X__other__, data = train)

sort(vif(fit.final), decreasing = T)

fit.final= stats::step(fit.final)


summary(fit.final)




test.predict = predict(fit.final, newdata = test)
write.csv(test.predict, "RajaBarman_P1_part2.csv", row.names = F)
