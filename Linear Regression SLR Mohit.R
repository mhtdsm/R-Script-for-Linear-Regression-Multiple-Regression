## this is a R Scriot to understand the Linear Regression Modelthrough the SLR.SV file in whih we have 
## 2 variables Sales & Advertisement. Here we want to make a linear Regression model to predict the Sales using the Advertisement Value
# The data in Both the Variables is Integer 
# We are taking Sales on y ais as it is Dependent Variable
# We are taking Advt on x asis as it is independent Variable
#  HEre Advt is a predictor & Sales is the response Variable
## The very 1st step is to import SLR.csv into R environment
# then the next step is to plot the scatter Plot between these two variables
plot(slr$Advt,slr$Sales,type="o",col="red")
# this red line basically indicates the plot line , we need to make a Regression line
# Regression line is the line of best fit whih will predict us the value of Sales in the domain of Advt in the sample given
# We an also opy the Plot and paste at our Required location in Exel
## We can Make the line of BEst fit without using any model but just by plotting satter plot & licking Line of BEst Fit /Trend line

# the funtion we use to make a iner model is " lm" 
# let us make a linear Regression odel named "mod" between advt & Sales
# always remeber the dependent varaible will be 1st in the model then the independent Variable
# use ~ sign b/w variables

mod<- lm(slr$Sales~slr$Advt)
mod
# the result we get is Coefficients:
# (Intercept)     slr$Advt  
# -852.08        19.07
# the above result means that the equation of the regression line is " yhat=  -852.08 + 19.07x "
# this is the line of best fit an we need to chek for its fit using various statistis and results

summary(mod)

#this funtion on model mod has given us many outome statistics which are very important to understand in order to understand  Regression Model
# like Residuals, coefficients, Standard Error, t value, p value, standard error Residuals
# Multiple R squared,Adjusted R square , F statistics & p value of overall model

# Now let us create file on Exel Sheet names Linear Regression SLR

# after we have reated a model now we need to plot the Regression line in the satter plot 

abline(mod)
# now we also need to plot y bar i.e. mean y line in plot as below
mean_sales<- mean(slr$Sales)
abline(h=mean_sales)

# now in the Plot our red line is the line joining the y points
# horizontal line is mean Line whih is y bar 
# the third line passing between the red line is Regression line 

# we also need to check the fit of the Regression model & Regression Line

# after we have find the Regression equation we will Find the Predited values of sales using the Advt values
# We will get the Point Estimates known as Yhat for the Advt values that lies on Regression Line

# After that we will Calculate the Residuals Values which is error i.e. (y - yhat)  OR (actual SalesValue- Predicted Sales Value)
# the function in R is pred

pred<- predict(mod)
pred
 # or 
pred1<- predict(lm(slr$Sales~slr$Advt))
pred1
# these are the predicted values we get through the regression line after puttingthe Advt values in it

#after finding predicted values we find out the Residuals or Errors 

# the R function of finding Residuals or R is a s giiven below
error<- residuals(mod)
error
 # Or
error1<- residuals(lm(slr$Sales~slr$Advt))
error1

#these errors are the same found in  Exel file

summary(error)
# we also need to check whether these erros have normal distribution or not,linear relationship or not,equal error variance or not

## after doing all this we need to Find out SSE , SSR ,SST 
# SSE=  Sum of all (y- yhat)^2
# SST = sum of all ( y -  y bar)^2
# SSR = Sum of all ( yhat - y hat bar)^2

# now we need to understand the Residual Standard Error which in our Model is 37.11

# It is denoted by Se and which means the standard deviation of our errors
# using this we can check that whether 68% of our errors are in 0+- 1.se , 
# or 95% or our errors are in 0+- 2.Se which will tell that our data is normally distributed
# Also Se= (SSE/n-k-1)^0.5
# predicted value y hat are point estimates but Se helps us find the Confidence interval
# practial use of Se is to find the range in which an sales value will lie for a partiular Advt Value
# It gives us Upper limit & lower limit using the below formulae

## Upper limit (sales(x))  = yhat + ( Z-value at 95% onfidence level * Advt)
## Lower limit (sales(x))  = yhat - ( Z-value at 95% onfidence level * Advt)
# Z value at 95% Confidence is 1.96


# now let us understand test for Slope
# this is hypothesis testing
# H0: Beta1 = 0 (test for beta1 ,beta1 is the slpoe of the population )  OR 
#H0: The slope of advertisement with sales is not significant  i.e there is no linear relationship
#as we have alulated slope for sample data so we need to chek for sample slope using the t test

# Ha: Beta1 not equal to zero  ,OR 
# Ha: The slope of advertisement with sales is  significant  i.e there is linear relationship

# the alternate hypothesis can mean +ve or  -ve relationship
# for +ve we need to check Ha: Beta1 > 0  & -ve Ha: Beta1< 0  whicch will be one tailed test and we need to find t values at given LOS

# the three terms in output Coefft of ADvt [ t-statistics (9.535), Standard error of Regression Coeffiient  (2.00) , p-value ] are related 
# to t tests following Null Hypothesis

# first we calulate T-stats by " t=(b1- Beta1)/Sb "
# Sb= " (Se/ (Sxx)^0.5) "

# after finding t value we need to check it with table t value to see the hypothesis is accepted or not
# table t value(alpha/2,n-2) two tailes test
#  table t value(alpha,n-2) one tailed test\
# if observed t value within table t value range  we will acept the H0:slope is not significant   else will reject H0

## also if pr(>t) < 0.05 -> Ho is rejected  & conclude slope is significant else accepted

# Multiple  r Square =0.909  whih means that 90.9% of the variance in Sales can be explained by Advertisement and remaining 
#10% is by other factors outside the model ( is  unexplained variance )

## Multiple R square = SSR/SST
cor<- cor(slr$Sales,slr$Advt)
cor
cor*cor # it is multiple r squared


# adjusted r square = (1-  (ssE/n-k-1)/(ssT/n-1))
# it comes out to be .8910087

# Now we need to understand F-stats & p-values
# F value is used to test the overall model

# by running Anova test we get F value = (SSreg/dfreg)/(sserr/dferr)  whih equals (MSreg/ MSerr)

# f value of 90.93 means 90.93% model is fitting the data as a good preditor


# From all the above stats we an say that the Regression model is a good fit in the given sample due to multiple r square ,F value etc

# Now we move towards assumptions of Simple linear regression
 # 1. Assumption of Normal distribution of errors
# 2 Linearity of Model 
# 3 Eual error variance
# 4. Indepenent Error terms

## We need to heck these assumptions of our model to know it is a good fit or not.  
# we an see residuaal s fitted graph plot to heck constant error variane too

# if model doesnot follow any of the assumption our model will be discarded or of little use for us to predit future outomes
summary(mod)
mod


# Happy learning




