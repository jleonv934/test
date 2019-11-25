title: "GROUP PROJECT"
author: "Jesus Leon"
date: "11/16/2019"
output: html_document
---

```{r}
MyData <- read.csv('/Users/jesusleon/Downloads/london_merged.csv', row.names=1)
```


```{r}
library(lubridate)
time= as_datetime(rownames(MyData[0]))
MyData<-data.frame(time, MyData)
```


#stationarity 
```{r}
library(tseries)
adf.test(MyData$cnt) # p-value < 0.05 indicates the TS is stationary
kpss.test(MyData$cnt)
```
So we can conclude that the time series is stationanry 



###Regression model with trend, seasonality, and white noise to make future predictions
```{r}
library('dplyr')
library('lubridate')
library('forecast')
```


#creating a time series with xts library  and making a zoo object 
```{r}
library('TSstudio')
library('zoo')
library('xts')
```

#linear regression forecasting 
```{r}
#creating series trend
MyData$trend <- 1:nrow(MyData)
#creating seasonality based on hours 
MyData$seasonal <- factor(hour(MyData$time), ordered = FALSE)
```


# setting a testing partition length
```{r}
 h <- length(MyData$time)/8 # setting a testing partition length
   train <- MyData[1:(nrow(MyData) - h), ]
   test <- MyData[(nrow(MyData) - h + 1):nrow(MyData), ]
```



#TREND
```{r}
md_trend <- lm(cnt ~ trend, data = train)
summary(md_trend)
```

#SEASONALITY
```{r}
md_seasonality <- lm(cnt ~ seasonal, data = train)
summary(md_seasonality)
length(md_seasonality$residuals)
```



```{r}
md_trend.2 <- lm(cnt ~ trend + seasonal + t1 + t2+ hum + wind_speed + factor(weather_code) + factor(season)+ I(is_weekend) +I(is_holiday), data = train)
summary(md_trend.2)
```
```{r}


train$yhat <- predict(md_trend.2, newdata = train)
test$yhat <- predict(md_trend.2, newdata = test)
#MyData
library(plotly)
   plot_lm <- function(data, train, test, title = NULL){
    p <- plot_ly(data = data,
            x = ~ time,
            y = ~ cnt,
            type = "scatter",
            mode = "line",
            name = "Actual") %>%
      add_lines(x =  ~ train$time,
                y = ~ train$yhat,
                line = list(color = "red"),
                name = "Fitted") %>%
      add_lines(x =  ~ test$time,
                y = ~ test$yhat,
                line = list(color = "green", dash = "dot", width = 3),
                name = "Forecasted") %>%
      layout(title = title,
             xaxis = list(title = "Year"),
             yaxis = list(title = "Number of Rides booked"),
             legend = list(x = 0.05, y = 0.95))
return(p) }
```


```{r}
plot_lm(data = MyData,
            train = train,
            test = test,
            title = "Predicting the Trend (Polynomial) and Seasonal Components of the Series")
```
#Accuracy of Model: 1.30% Prediction Error on the testing set
```{r}

mape_md2 <- c(mean(abs(train$cnt - train$yhat) / train$cnt),
                 mean(abs(test$cnt - test$yhat) / test$cnt))
mape_md2
```

#ANOVA
```{r}
anova(md_trend.2)
```

#REsiduals Analysis
```{r}
checkresiduals(md_trend.2)

```


#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################
#######################################################################################################################################



We can see that from the residual lag dependance that our model did not capture all of the variation in the data 
#we try ofseeting this effect by estimating residuals with ARIMA errors




#goal estimate residuals with ARMA process and add it to the model 
#make auto arima on residulas 



#######################################################################################################################################
#################################################################  MK I  ##############################################################
#######################################################################################################################################
#######################################################################################################################################





```{r}
library('forecast')
MyData.ts<-ts(MyData)
#MyData.ts
cnt_md<-auto.arima(MyData.ts[,2])
#cnt_md$residuals

MyDATA.prot.2 <- data.frame(MyData)
MyDATA.prot.2$residuals <- cnt_md$residuals
#head(MyDATA.prot.2)

#uses ARIMA(2,1,1) to model noise 

h <- length(MyDATA.prot.2$time)/8 # setting a testing partition length
   train <- MyDATA.prot.2[1:(nrow(MyDATA.prot.2) - h), ]
   test <- MyDATA.prot.2[(nrow(MyDATA.prot.2) - h + 1):nrow(MyDATA.prot.2), ]

md_pred.res<-lm(cnt ~ trend + seasonal + t1 + hum + wind_speed + factor(weather_code) + factor(season)+ I(is_weekend) +I(is_holiday)+ residuals , data = train) 


train$yhat <- predict(md_pred.res, newdata = train) #+ train$residuals
test$yhat <- predict(md_pred.res, newdata = test) #+ test$residuals


plot_lm(data =MyDATA.prot.2,
            train = train,
            test = test,
            title = "Predicting the number of rides booked ")
```

```{r}
summary(md_pred.res)
plot(md_pred.res)
checkresiduals(md_pred.res)

mape_md_pred.res <- c(mean(abs(train$cnt - train$yhat) / train$cnt),
                 mean(abs(test$cnt - test$yhat) / test$cnt))
mape_md_pred.res

#residuals are modeleled by an ARIMA((2,1,1)) linear process 
#not actual residulas 
```




