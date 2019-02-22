# setting working directory

getwd()

# if the data is in this  default working directory fine else you can declare your directory as working directory

#setwd("/Users/")

# Reading data and stored in da matrix. If the title head exists header=T else header=F





#da=read.csv("nonlinear.csv",header=T)

# dimension of data matrix

#dim(da)



# printing last  row of data matrix



#da[15,]

#names(da)

# attaching the data and hence all commands can now take the name of the variables 

#attach(da)

# simple bivariate plot

#plot(x,y, col='red')



# calculating correlation

#cor(x,y)


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
