#analyze data
#install.packages("ISwR")
library("ISwR")
library(forecast)
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\Dad's Friend")

StaffWork = read.csv("Rinput.csv", header = TRUE)
  
  
#Data Analysis  
dim(StaffWork)
names(StaffWork)
str(StaffWork)
attributes(StaffWork)
head(StaffWork)
tail(StaffWork)
summary(StaffWork)
#quantile(StaffWork$Monthly.Revenue)
#var(StaffWork$Monthly.Revenue)
#hist(StaffWork$Monthly.Revenue)


#Slice Data for Timeseries
input = c("Count","NumberofBranches","MonthlyRevenue")
data = StaffWork[input]

#TimeSeries Analysis
myts <- ts(data["MonthlyRevenue"], start=c(2010, 1), end=c(2013, 12), frequency=12) 
plot(myts)
dmyts = decompose(myts)
plot(dmyts)
#adjusted
dmytsa = myts -??? dmyts$seasonal
plot(dmytsa)

# simple exponential - models level
 fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
 # double exponential - models level and trend
 fit <- HoltWinters(myts, gamma=FALSE)
 # triple exponential - models level, trend, and seasonal components
 fit <- HoltWinters(dmytsa)

 # predictive accuracy

 accuracy(fit)

 # predict next three future values
 forecast(fit, 12)
 plot(forecast(fit, 36)) 




#CorrelationAnalysis


cor(StaffWork[input])
pairs(StaffWork)

heatmap(as.matrix(dist(cor(StaffWork[input]))))
