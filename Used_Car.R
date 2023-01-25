#normal distribution, mean, standard deviation, standard error, z score, p value
#hypothesis test, linear regression, f-statistic, p-value, r squared, SSR, SSE

#install packages and libraries in R for data importation,analysis, 
#manipulation, and visualization

install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(ggplot2)

#Used car sales

#We want to predict the price of a used car depending on it's specifications(regressors)

#Regressors:
#1. Brand (relatively expensive or relatively cheap car brand)(Dummy variables for different brands)
#2. Mileage (the higher the mileage, the less expensive the car)
#3. Engine Volume (sports cars have higher engine volume relative to economy cars)
#4.Year of production (the older the car, the cheaper it is)


#retrieve the used car data
  used_cars <- read.csv(
  "C:\\Users\\Abdul\\Desktop\\DataAnalysis22\\Portfolio\\Regression_Example_R\\Regression_Analysis_Used_Cars.csv",
  stringsAsFactors = FALSE)

#set used_cars object as a data frame
  as.data.frame(used_cars)

#convert the Prices column into a numeric type in order to perform operations on Prices vector
  used_cars$Price = as.factor(gsub(",", "",used_cars$Price))
  used_cars$Price = as.numeric(gsub("\\$", "", used_cars$Price))

#save Prices column in a new vector & save log_prices column in a another vector
  Prices <- used_cars$Price
  log_Ps <- used_cars$log_price

#A check for a linear relationship between the dependent variable Prices
#and the independent variable mileage
#The first plots shows an exponential relationship, where as the second plot shows
#a linear relationship. 
  
  plot(used_cars$Mileage,Prices)
  plot(used_cars$Mileage,log_Ps)
  plot(used_cars$log_mileage,log_Ps)
  
  #We will use logarithm of prices instead of prices from here on
  
  
    #Find min and max values
       min(log_Ps)
       max(log_Ps)
  
#hypothesis test (Ho) the mean Price for a used car is less than or equal to $20000 
 
  H_null=log(19999,base=10)
  
#Find the sample mean value of the Prices data
  xbar=mean(log_Ps) 
  
  if( xbar > H_null){
    print('Null hypothesis is false ')
  } else {print('Null hypothesis is true, the mean Price for a used car is less than $20000 ')}

  
  
#find the variance of the Price data
  var(log_Ps)

#standard deviation of Price data
  sd(log_Ps)  
  
#standard error of log Price data
  se_used_car=sd(log_Ps)/sqrt(length(log_Ps))

  
    
#Check the confidence level for null hypothesis (Ho <= $20000)
  check_it=log(20000, base=10)
  
  z=(check_it-H_null)/se_used_car
 
  #p value of Price data
  p_value=pnorm(z)
  
  #test for different confidence levels
  
    #90%
    if(p_value < (1-0.9)){
      print('reject null hypothesis at 90%')
    }else{print('null hypothesis acceptable at 90% confidence')}
  
   #95%
    if(p_value < (1-0.95)){
      print('reject null hypothesis at 95%')
    }else{print('null hypothesis already acceptable at 90% confidence')}
  
    #99%
    if(p_value < (1-0.99)){
      print('reject null hypothesis at 99%')
    }else{print('null hypothesis already acceptable at 90% confidence')}



#Normal distribution plot of log of prices, where the mean is centered around zero
#and the standard deviation is set to one
  
  norm_log_Ps= log_Ps-mean(log_Ps)
  y<-dnorm(norm_log_Ps, mean = 0, sd = 1.0)
  plot(norm_log_Ps,y, main = "Normal Distribution", col = "blue")
  

  
  
  
#Linear regression (This data uses dummy variables for different used car brands)
          
          miles=used_cars$Mileage
          log_miles=log(miles,base = 10)
          
  #Check for linearity 
          
          plot(miles,Prices) # this plot shows an exponential relationship
          abline(lm(Prices~miles,data=used_cars),col='red')
          
          plot(log_miles,log_Ps) # this plot shows a linear relationship
          abline(lm(log_Ps~log_miles,data=used_cars),col='red') #regression line
          
          l.interval <- lm(log_Ps ~ 1, data=used_cars)
          confint(l.interval, level=0.95)
          
 
          
  #Check for normality and homoscedasticity
          
          #plot logarithms of both x and y variables
          #variance looks similar on either side of regression line
          #log-log model
          
          log_miles=log(miles, base=10)
          
          plot(log_miles,log_Ps,main ="log-log plot")
          
          abline(lm(log_Ps ~ log_miles,data=used_cars),col='red')#regression line
          
  
  #check for no autocorrelation
          #this data is not time series data
          #it is cross sectional data (no serial correlation)
  
  #check for no multi-colinearity
    
          years= used_cars$Year
          engine_volume = used_cars$EngineV
          
          
          #using the correlation coefficient, we can demonstrates no linearity between 
          #between any of the dependent variables
          cor(log_miles, years)
          cor(engine_volume, years)
          cor(miles, engine_volume)

  
  #Check for no endogeneity
          
         
          #The r squared value indicates that the mileage, years, and engine volume
          #regressors explain 76% of the price of a used car. This is a strong indicator
          
         
  #linear regression equation
          #dummy variables
          audi=used_cars$Audi_D
          bmw=used_cars$BMW_D
          mercedes=used_cars$Mercedes_D
          mitsubishi=used_cars$Mitsubishi_D
          renault=used_cars$Renault_D
          
          lm.usedcars<- lm(
            formula = log_Ps ~ log_miles+years+engine_volume+audi+bmw+mercedes+mitsubishi+renault,
            data=used_cars)

          anova(lm.usedcars)
          summary(lm.usedcars)
            
            #the above provides a summary of standard error, degrees of freedom
            #the R-squared value, the F-statistic, the p-value
          
            #the very very low p-value at 2.2e-16,the high F-statistic value
            #of 4166, and the equality of SST and SSR indicates that these regressors 
            #explain the price and capture all variability in the population













