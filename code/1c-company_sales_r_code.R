library(forecast)
library(DataCombine)
library(ggplot2)
library(readr)
library(data.table)
library(lubridate)
library(dplyr)
library(tseries)
library(R.utils)
library(tidyverse)
library(MASS)
library(fpp)
library(corrplot)
library(magrittr)
library(zoo)
library(RColorBrewer)
library(gridExtra)
library(Amelia)
library(ggthemes)
library(plotly)
library(caTools)
library(class)
library(scales)
library(caret)
library(e1071)

setwd("~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project")

item_categories <- data.frame(fread('~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/item_categories.csv', stringsAsFactors = FALSE, data.table = FALSE))
items <- data.frame(fread('~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/items.csv', stringsAsFactors = FALSE, data.table = FALSE))
shops <- data.frame(fread('~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/shops.csv', stringsAsFactors = FALSE, data.table = FALSE))
gunzip("~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/sales_train.csv.gz")
gunzip("~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/test.csv.gz")
gunzip("~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/sample_submission.csv.gz")
sales_train <- data.frame(fread('~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/sales_train.csv', stringsAsFactors = FALSE, data.table = FALSE))
test <- data.frame(fread('~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/test.csv', stringsAsFactors = FALSE, data.table = FALSE))
sample_submission <- data.frame(fread('~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Midterm Project/sample_submission.csv', stringsAsFactors = FALSE, data.table = FALSE))

salesdata <- sales_train %>% left_join(items, by = c("item_id"))
salesdata$item_name <- NULL
rm(sales_train)
rm(items)
salesdata <- as.data.frame(salesdata)
str(salesdata)
salesdata$date <- dmy(salesdata$date)
salesdata$year <- year(salesdata$date)
salesdata$month <- month(salesdata$date)
salesdata$weekday <- weekdays(salesdata$date)
salesdata$year <- as.factor(salesdata$year)
salesdata$weekday <- as.factor(salesdata$weekday)
salesdata_item_cnt_month <- salesdata %>% group_by(year, month, shop_id, item_id) %>% summarise(item_cnt_month = sum(item_cnt_day)) %>% ungroup()
salesdata <- salesdata %>% left_join(salesdata_item_cnt_month, by = c("year", "month", "shop_id", "item_id"))
str(salesdata)
salesdata$shop_id <- as.factor(salesdata$shop_id)
salesdata$item_id <- as.factor(salesdata$item_id)
salesdata$item_category_id <- as.factor(salesdata$item_category_id)
str(salesdata)
rm(item_categories)
rm(shops)

colSums(is.na(salesdata))
is.null(salesdata)

year.month.wise.total.sales <- salesdata %>% group_by(year,month) %>% summarise(total.sales.pery.year = sum(item_price * item_cnt_day)) %>% arrange(year) %>% ungroup()
ggplot(data = year.month.wise.total.sales, aes(x = year, y = total.sales.per.year, fill = as.factor(month))) 
  geom_bar(stat = "identity") 
  labs(title = "Total sales per year-month", x = "Year", y = "Total sales per year", fill = "Month")
rm(year.month.wise.total.sales)

salesdata %>%
  group_by(month) %>%
  summarise(total_sales = sum(item_cnt_day)) %>%
  ggplot(aes(x = as.factor(month), y = total_sales, fill =as.factor(month))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Total unit sales', x = 'Month', title = 'Total Sales by Month')

salesdata$sale_price <- salesdata$item_price * salesdata$item_cnt_day
total.no.of.items.sold <- sum(salesdata$item_cnt_day)
total.revenue <- sum(salesdata$sale_price)
monthly.item.sales <- salesdata %>% group_by(date_block_num) %>% summarise(monthly.item.sales.frequency = round(sum(item_cnt_day) / total.no.of.items.sold, digit = 3))
ggplot(data = monthly.item.sales, aes(x = "", y = monthly.item.sales.frequency, fill = factor(date_block_num) ))
  geom_bar(width = 1, stat = "identity")
  coord_polar(theta = "y", start = 0)
  geom_col(position = 'fill')
  geom_label(aes(label = paste0(monthly.item.sales.frequency * 100, "%")), position = position_fill(vjust = 0.5))
  labs(title = "% of items sold per month", x = "", y = "Monthly item sale frequency", fill = "Months")
rm(total.no.of.items.sold, total.revenue, monthly.item.sales)

salests <- ts(salesdata$item_cnt_month, frequency = 12, start = c(2013,1))
plot(salests)

saleststrain <- subset(salests, month = c(10,11))
saleststrain2 <- subset(salests, month = c(9,10))
saleststrain3 <- subset(salests, month = c(11,12))

arimafit <- auto.arima(saleststrain)
arimafit
arimafc <- forecast(arimafit,h=214200)
plot(arimafc)
arimaacc <- accuracy(arimafc, test)
arimaacc
arimasol=data.frame(sample_submission,item_cnt_month=arimafc$mean)
arimasol$item_cnt_month <- NULL
View(arimasol)
write.csv(arimasol,file="PFS_ARIMA_Predictions.csv",row.names=F)

arimafit2 <- auto.arima(saleststrain2)
arimafit2
arimafc2 <- forecast(arimafit2,h=214200)
plot(arimafc2)
arimaacc2 <- accuracy(arimafc2, test)
arimaacc2
arimasol2=data.frame(sample_submission,item_cnt_month=arimafc2$mean)
arimasol2$item_cnt_month <- NULL
View(arimasol2)
write.csv(arimasol2,file="PFS_ARIMA.2_Predictions.csv",row.names=F)

arimafit3 <- auto.arima(saleststrain3)
arimafit3
arimafc3 <- forecast(arimafit3,h=214200)
plot(arimafc3)
arimaacc3 <- accuracy(arimafc3, test)
arimaacc3
arimasol3=data.frame(sample_submission,item_cnt_month=arimafc3$mean)
arimasol3$item_cnt_month <- NULL
View(arimasol3)
write.csv(arimasol3,file="PFS_ARIMA.3_Predictions.csv",row.names=F)

etsfit <- ets(saleststrain)
etsfit
etsfc <- forecast(etsfit,h=214200)
plot(etsfc)
etsacc <- accuracy(etsfc,test)
etsacc
etssol=data.frame(sample_submission,item_cnt_month=etsfc$mean)
etssol$item_cnt_month <- NULL
View(etssol)
write.csv(etssol,file="PFS_ETS_Predictions.csv",row.names=F)

etsfit2 <- ets(saleststrain2)
etsfit2
etsfc2 <- forecast(etsfit2,h=214200)
plot(etsfc2)
etsacc2 <- accuracy(etsfc2,test)
etsacc2
etssol2=data.frame(sample_submission,item_cnt_month=etsfc2$mean)
etssol2$item_cnt_month <- NULL
View(etssol2)
write.csv(etssol2,file="PFS_ETS.2_Predictions.csv",row.names=F)

etsfit3 <- ets(saleststrain3)
etsfit3
etsfc3 <- forecast(etsfit3,h=214200)
plot(etsfc3)
etsacc3 <- accuracy(etsfc3,test)
etsacc3
etssol3=data.frame(sample_submission,item_cnt_month=etsfc3$mean)
etssol3$item_cnt_month <- NULL
View(etssol3)
write.csv(etssol3,file="PFS_ETS.3_Predictions.csv",row.names=F)

