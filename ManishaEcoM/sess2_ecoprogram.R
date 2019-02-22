

library(lmtest)
library(car)
library(MASS)
library(gvlma)
library(sandwich)



### Read the file from the saved location ### 
Airtel_data <- data.frame(read.csv("Airtel.csv"))

### Store the columns as seperate variables ###
Subsc <- Airtel_data$Subscription
TVC <- Airtel_data$SUC

### Summarize the data ###
summary(Airtel_data)
### Standard Deviation ###
sapply(Airtel_data, sd)

### Fit a regression line between Subscribers and SUC ###
fit1 <- lm(Subsc ~ TVC, data = Airtel_data)
summary(fit1)

### Plot the regression line ###
plot(Subsc ~ TVC, data = Airtel_data, xlab= "Total Variable Costs (Million INR)", ylab = "Subscribers", main = "Subscription vs. TVC for Airtel")
abline(fit1)  



## ramsey test for  misspecification
resettest(fit1,power=2:3,type="fitted")


# another Data set
mydata <- read.csv("aj.csv",header=T)
dim(mydata)


attach(mydata)
names(mydata)


## creating time series objects starting from 1963 till 2005
y=ts(ngdp,frequency=1,start=1963)
x1=ts(m2,frequency=1,start=1963)
x2=ts(fp,frequency=1,start=1963)

par(mfcol=c(3,1)) # four plots in one page

z=cbind(y,x1,x2)
plot(z)



## Fitting OLS and plotting residuals. 
library(dynlm)

model=dynlm(y~L((x1),1)+L((x2),1))

summary(model)


plot(model$residual)




library(lmtest)
## ramsey test for omission of variables and or misspecification
resettest(model,power=2:3,type="fitted")

## Now install ma package


library(devtools)
install_github('JeffreyRacine/R-Package-ma')
library(crs)
library(ma)

model.lm.ma <- lm.ma(Subsc ~ TVC, data = Airtel_data)
summary(model.lm.ma)



