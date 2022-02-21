install.packages("dplyr")
install.packages("readxl")
install.packages("forecast) #autoplot
install.packages("fpp2")
install.packages("tidyr")
install.packages("stringr")

library("readxl")
library("dplyr")
library("forecast")
library(fpp2)
library(tidyr)
library(stringr)

mydata<-read_excel("C:\\Users\\stef_\\Downloads\\KP\\Transactional Volumes Data.xlsx", col_names =TRUE)
head(mydata)

mydata2=select(filter(mydata, STATUS == 'Active'),c(1,4,7:9))
names(mydata2)<-str_replace_all(names(mydata2), c(" " = "." , "," = "", "-" = "_"))
mydata2$MONTH = as.numeric(mydata2$MONTH)

# Get unique list of WorkStreamIDs
unique(mydata2$Workstream.ID)

#mydata3=select(filter(mydata2, Workstream.ID==13),c(2:3))
mydata3= mydata2 %>% group_by(Year, MONTH, MM_YYYY) %>% summarise_at(vars(Workstream.Count),funs(sum(.,na.rm=TRUE))) %>% arrange(Year, MONTH, MM_YYYY)
mydata3=mydata3[,3:4]
head(mydata3)

# Each WorkStream ID has one record per MM-YYYY
colnames(mydata2)
#[1] "Workstream ID"    "Workstream Count" "MM-YYYY"

myts <- ts(mydata3[,2], start = c(2017, 1), frequency = 12)
autoplot(myts, facets = TRUE)

# Find the outlier as the smallest index of the maximum value
outlier <-which.max(myts)

# Determine frequency of Seasonal Time Series
frequency(myts) #12

# create a seasonal plot (each year is a separate color)
ggseasonplot(myts)

# Produce a polar coordinate plot (each year is a separate color)
ggseasonplot(myts,polar=TRUE)

# Window function to subset data
sub_myts <- window(myts, start=2017, end=2018)
dim(sub_myts)

# Produce a time series by month
ggsubseriesplot(myts)

# Autocorrelation of present time vs. previous lags
# When data are either seasonal or cyclic, the ACF will peak around the seasonal lags or at the average cycle length.
gglagplot(myts)
ggAcf(myts) +
xlab("YY-MMMM") +
ylab("Transaction Volume") +
ggtitle('Autocorrelation Function for white noise') #autocorrelation function  -- some exists around month 2, 11-12

# Plot monthly change in values to look for significant changes from zero
autoplot(diff(myts))
ggAcf(diff(myts))

# Ljung-Box test of the differenced series
Box.test(diff(myts), lag = 10, type = "Ljung")
#Ho: white noise series
#p-value = 0.495 -- so departures are white noise

# Simple forecast based on last value
fore=naive(myts,h=13)
autoplot(fore)
summary(fore)

# Simple seasonal forecase base on last season
fore=snaive(myts,h=13)
autoplot(fore)
summary(fore)


# Residuals should look like Gaussian white noise
# uncorrelated and zero mean
# constant variance and normally distributed
# Ho: residuals are white noise
myts %>% snaive() %>% checkresiduals()
#pval = 0.5308, so no concern on residuals

# split data into training and test data
training=window(myts, end=c(2019,12))
# can alternatively use index values and subset function
test=window(myts, start=c(2020,1))
fore=snaive(training, h=13)
autoplot(fore) + autolayer(test, series="Test data")


# forecasts as the mean of all observations
mean_fc <- meanf(training, h = 12)
fore=snaive(training, h=12)

# compare RMSE
accuracy(mean_fc, myts)
accuracy(fore,myts)
accuracy(fore,myts)["Test set", "RMSE"]









save.image("~/R/KP.RData")