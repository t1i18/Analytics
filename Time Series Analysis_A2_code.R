
rm (list=ls())
library(TSA)
library(tseries)
#1. load data
setwd("/Users/momo/Documents/Master of Analytics/SEM1 2022/Time Series Analysis")
ICE <- read.csv("assignment2Data2022.csv")
#2. check data structure and rename variables
str(ICE)
names(ICE)[2] <- "Changes"
colnames(ICE)
#3. create ts object
ICEts <- ts(data=ICE$Changes, start = "1", frequency = 1)
#4. descriptive analysis
#4.1 data summary
summary(ICEts)
#4.2 plot time series data
plot(ICEts, ylab='Changes in Ice Mass (Billion Metric Tons)',xlab="Time (Years)", type = "o",
     main = "Time Series Plot for Yearly Changes in Antarctica Land Ice Mass")


plotandcheck <- function(ts,  scatter_plot_title, qq_plot_title){
    
    #scatter plot and check correlation of of neighbouring measurement
    plot(y=ts,x=zlag(ts),col = "blue", xlab = "Previous Year Changes in Antarctica Land Ice Mass",
         ylab = "Day Changes in Antarctica Land Ice Mass", main = scatter_plot_title)
    
    y = ts
    x = zlag(ts) 
    index = 2:length(x)
    print('Correlation Index:')
    print(cor(y[index],x[index]))
    
    #normality check
    qqnorm(ts, main=qq_plot_title)
    qqline(ts, col = "red")
    hist(ts)
    print(shapiro.test(ts))
 
}
plotandcheck(ICEts,'Scatter Plot of Changes in Antarctica Land Ice Mass', 
        'QQ Plot of Yearly Changes in Antarctica Land Ice Mass'
        )
apacor <- function(ts, acf_title, pacf_title){
  par(mfrow=c(1,2))
  acf(ts, main=acf_title)
  pacf(ts, main=pacf_title)
  par(mfrow=c(1,1))
}
apacor(ICEts, "ACF Plot of ICEts Data", "PACF Plot of ICEts Data")

stats.test <- function(ts) 
{print(adf.test(ts))
  print(pp.test(ts))
  print(kpss.test(ts))
}
stats.test(ICEts)
#5.Box-Cox transformation to see if it improves stationarity and normality
#5.1 Box-Cox transformation:find values of the first and third vertical lines
ICEtsP <- ICEts + abs(min(ICEts)) + 0.01
BC = BoxCox.ar(y=ICEtsP, lambda = seq(-1,2,0.01))
BC$ci
#find the middle vertical line -> lambda value
BC<- BoxCox.ar(y=ICEts, lambda = seq(-2,2,0.01))
lambda <- BC$lambda[which(max(BC$loglike)==BC$loglike)]
lambda
ICEtsBC <-((ICEtsP^lambda)-1)/lambda #apply Box-Cox transformation with lambda
plot(ICEtsBC, ylab='Changes in Ice Mass (Billion Metric Tons)', xlab="Time (Years)", type = "o",
     main = "Time Series Plot for Box-Cox Transformed Yearly Changes in Antarctica Land Ice Mass")


#5.2 plot Box-Cox transformed ts data    


plotandcheck(ICEtsBC, 'Scatter Plot of the Box-Cox Transformed Changes in Antarctica Land Ice Mass', 
        'QQ Plot of the Box-Cox Transformed Yearly Changes in Antarctica Land Ice Mass' 
        )
apacor(ICEtsBC, "ACF Plot of ICEtsBC Data", "PACF Plot of ICEtsBC Data")

#5.3 statistical test on Box-Cox transformed ts data 
stats.test(ICEtsBC)

#6. apply differencing for stationarity

ICEtsdif1=diff(ICEts, differences = 1)
plot(ICEtsdif1, ylab='Changes in Ice Mass (Billion Metric Tons)',xlab="Time (Years)", type = "o",
     main = "Time Series Plot of the First Difference of Yearly Changes in Antarctica Land Ice Mass")
apacor(ICEtsdif1, "ACF Plot of ICEtsdif1 Data", "PACF Plot of ICEtsdif1 Data")

stats.test(ICEtsdif1)


#{from acf q=0,from pacf p=0}

#7. model specification

#EACF
eacf(ICEtsdif1, ar.max = 3, ma.max = 3)
#BIC table
res = armasubsets(y=ICEtsdif1, nar = 3, nma = 3, y.name = "p", ar.method = "ols" )
plot(res)

