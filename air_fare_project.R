airlines <- read.csv("C:\\Users\\USER\\Downloads\\Concatenate_B2C_B2E.csv")
summary(airlines$NetFare)
airlines$NetFare <- as.numeric(airlines$NetFare)
airlines$NetFare <- as.numeric(as.character(airlines$NetFare))
library(lubridate)
#convert into proper format
airlines$InvoiceDate <- dmy_hm(airlines$InvoiceDate)
head(airlines$InvoiceDate)
View(airlines)
library(dplyr)
library(ggplot2)
summary(airlines$NetFare)
airlines <- arrange(airlines, airlines$InvoiceDate)
#####################################################################################################

#average analysis for hours for NETFARE for domestic air
domestic <- filter(airlines,ProductType=="Air",ItineraryType=="Domestic")
domestic$NetFare <- as.numeric(as.character(domestic$NetFare))
View(domestic)
summary(domestic$NetFare)
domestic <- arrange(domestic, domestic$NetFare)
# we arrange in the ascending order so that we can remove negative values , as per domain knowledge netfare can not be negative.
domestic <- domestic[7234:141234,]
hist(domestic$NetFare,bin=5000)
summary(domestic$NetFare)
dom_hour <- domestic %>% 
  mutate( year= year(domestic$InvoiceDate),month=month(domestic$InvoiceDate,label = TRUE),week=wday(domestic$InvoiceDate,label = TRUE),day=day(domestic$InvoiceDate),hour=hour(domestic$InvoiceDate)) %>%
  group_by(year,hour) %>%
  summarise(total=mean(NetFare),max=max(NetFare),min=min(NetFare))
View(dom_hour)
#average price during different hours of day for the year 2018 , 2019 seperately 
ggplot(data = dom_hour) + geom_smooth(aes(dom_hour$hour,dom_hour$total),size=2) + facet_wrap(~year) + geom_line(aes(dom_hour$hour,dom_hour$total),col="black",size=3)
ggplot(data = dom_hour, aes(hour,total)) + geom_bar(stat = "identity") + facet_wrap(~year) + coord_flip() 

#compare average vs min values for hour vs netfare

ggplot(data = dom_hour)  + geom_line(aes(hour,total)) + geom_line(aes(hour,min),col="blue") 
##################################################################################################

#avg analysis for months

dom_month <- domestic %>%
  mutate( year= year(domestic$InvoiceDate),month=month(domestic$InvoiceDate,label = TRUE),week=wday(domestic$InvoiceDate,label = TRUE),day=day(domestic$InvoiceDate),hour=hour(domestic$InvoiceDate)) %>% 
  group_by(year,month,day) %>% 
  summarise(total = mean(NetFare))
View(dom_month)  
# avg vs months
#april-18, may-18,may-19 had higher average price as compared to other months 
#jun - july - august comparitvely hav low average price
ggplot(data = dom_month) + geom_point(aes(month,total)) + facet_wrap(~year)

###############################################################################################

#avg analysis for weekday 
#for different for different months
dom_week <-  domestic %>%
  mutate( year= year(domestic$InvoiceDate),month=month(domestic$InvoiceDate,label = TRUE),week=wday(domestic$InvoiceDate,label = TRUE)) %>%
  group_by(year,month,week) %>%
  summarise(total=mean(NetFare))
View(dom_week) 

ggplot(data = dom_week) + geom_point(aes(dom_week$week,dom_week$total,col=year),size=2) + facet_wrap(~month)

#considering only weekday without month

dom_week <-  domestic %>%
  mutate( year= year(domestic$InvoiceDate),month=month(domestic$InvoiceDate,label = TRUE),week=wday(domestic$InvoiceDate,label = TRUE)) %>%
  group_by(year,week) %>%
  summarise(total=median(NetFare))
View(dom_week) 

ggplot(data = dom_week) + geom_point(aes(dom_week$week,dom_week$total)) + facet_wrap(~year)+ theme(plot.title=element_text(size=15))+ theme_minimal()
View(domestic)
#mostly sundays are cheap and and tuesday, friday are expensive

#############################################################
#we remove the negative values
invoice <- filter(airlines, airlines$ProductType=="Air")
View(invoice)
invoice$NetFare <- as.numeric(as.character(invoice$NetFare))
invoice$InvoiceDate <- as.Date(invoice$InvoiceDate)
invoice <- arrange(invoice, invoice$NetFare)
invoice <- invoice[7734:151995,]
invoice1<- invoice %>% group_by(InvoiceDate,ItineraryType) %>% summarise(max= max(NetFare),mean=mean(NetFare),min=min(NetFare))
View(invoice1)
#consist of maximum , mean and minimum value for that day! 
invoice1_2018 <- invoice1[1:508,]
summary(invoice1_2018$ItineraryType)
#comparision of average values for domestic and international 
ggplot(data = invoice1_2018) + stat_smooth(aes(InvoiceDate,mean,col=ItineraryType)) + scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal() 

#boxplots 
invoice_box <- invoice %>% mutate(year=year(InvoiceDate),month=month(InvoiceDate,label = TRUE),weekday=wday(InvoiceDate,label = TRUE))
View(invoice_box)
#distribution of fare based on different months for domestic and international
ggplot(data = invoice_box) + geom_boxplot(aes(month,NetFare,col=ItineraryType)) + facet_wrap(~year)
summary(invoice_box$NetFare)
#international vs domestic 
#how the fare varies of the period for domestic and international flight fare
ggplot(data = invoice_box) + stat_smooth(aes(InvoiceDate,NetFare,col=ItineraryType),size=1) 

invo_dom <- filter(invoice_box, invoice_box$ItineraryType=="Domestic")
invo_int <- filter(invoice_box, invoice_box$ItineraryType=="International")

#Domestic 
#how the fare varies on different day of the week over the period
ggplot(data = invo_dom) + stat_smooth(aes(InvoiceDate,NetFare,col=weekday),size=1)
ggplot(data = invo_dom) + geom_boxplot(aes(weekday,NetFare,col=year),size=1) + facet_wrap(~year)

#international
#how the fare varies on different day of the week over the period
ggplot(data = invo_int) + geom_boxplot(aes(weekday,NetFare,col=year),size=1) + facet_wrap(~year)
ggplot(data = invo_int) + stat_smooth(aes(InvoiceDate,NetFare,col=weekday))

########################################################################################

View(domestic)
domestic$year <- as.factor(year(domestic$InvoiceDate))
domestic$month <- as.factor(month(domestic$InvoiceDate))
domestic$day <- as.factor(day(domestic$InvoiceDate))
domestic$weekday <- as.factor(weekdays(domestic$InvoiceDate))
domestic$hour <- as.factor(hour(domestic$InvoiceDate))
total_netfare <- domestic %>% group_by(year, month) %>% summarise(totalSales =sum(NetFare))%>%ungroup() %>%arrange(desc(totalSales))
total_netfare %>% distinct(month,.keep_all=TRUE)

View(total_netfare)
#months which had most profit = sum of total netfare for that month
ggplot(data = total_netfare, aes(month,total_netfare$totalSales),col="green")  +  geom_bar(stat = 'identity',col="green") + 
  theme(legend.position = "none")+ coord_flip() +
  labs(y = 'total price', x = 'months', title = 'Avg price for different months of the year') + facet_wrap(~year) + theme(plot.title=element_text(size=15))+ theme_minimal()
class(domestic$month)
domestic$month <- as.factor(domestic$month)
ggplot(data = domestic, aes(month, NetFare)) + geom_boxplot() + facet_wrap(~year)
#variation 

ggplot(data = domestic, aes(weekday, NetFare)) + geom_boxplot() + facet_wrap(~month)

ggplot(data = domestic, aes(year, NetFare)) + geom_boxplot()

hist(domestic$NetFare)
plot(density(domestic$NetFare))

#months having highest sales of tickets 
class(domestic$month)
domestic$month <- as.numeric(domestic$month)
count <- count(domestic,domestic$year,domestic$month)
View(count)
count$`domestic$month` <- as.factor(count$`domestic$month`)
#this indicates the no. of tickets sold in that month,
ggplot(data = count) + geom_point(aes(count$`domestic$month`,count$n,col=count$`domestic$year`),size=3) + theme(plot.title=element_text(size=15))+ theme_minimal()
#may-19,april-19,march19 had higher sales of tickets 
###################################################################################################

airlines$InvoiceDate <- as.Date(airlines$InvoiceDate)

compare <- filter(airlines, airlines$ProductType=="Air" | airlines$ProductType=="Other Product" | airlines$ProductType=="Hotel"| airlines$ProductType=="payment" , airlines$ItineraryType=="Domestic")
View(compare)
compare$NetFare <- as.numeric(as.character(compare$NetFare))
summary(compare)
compare <- arrange(compare, NetFare)
#remove negative values
compare <- compare[7391:214785,]
compare <- compare %>% group_by(InvoiceDate,ProductType,ItineraryType)
View(compare)
summary(compare)

compare$year <- as.factor(year(compare$InvoiceDate))
compare$month <- as.factor(month(compare$InvoiceDate,label = TRUE))
View(compare)
compare$InvoiceDate <- as.Date(compare$InvoiceDate)
table(compare$ProductType)
ggplot(data = compare) + geom_smooth(aes(InvoiceDate,NetFare,col=compare$ProductType)) + theme(plot.title=element_text(size=15))+ theme_minimal()

####################################################################################################

#average analysis for hours 
international <- filter(airlines,ProductType=="Air",ItineraryType=="International")
View(international)
summary(international$NetFare)
international$NetFare <- as.numeric(as.character(international$NetFare))
international <- arrange(international, NetFare)
international <- international[501:10761,]
View(international)

########################################################################################################

#avg analysis for months

int_month <- international %>% 
  mutate( year= year(international$InvoiceDate),month=month(international$InvoiceDate,label = TRUE),week=wday(international$InvoiceDate,label = TRUE),day=day(international$InvoiceDate),hour=hour(international$InvoiceDate)) %>%
  group_by(year,month) %>%
  summarise(total=mean(NetFare))
View(int_month)

ggplot(data = int_month, aes(month,total,fill=as.factor(year)))  +  geom_bar(stat = 'identity',position = "dodge") + 
  theme(legend.position = "none")+ coord_flip() +
  labs(y = 'avg price', x = 'months', title = 'Avg price for different months of the year') + theme_bw()

ggplot(data = int_month) + geom_point(aes(as.factor(month),total))+  theme(legend.position = "none")+
  labs(y = 'avg price', x = 'month', title = 'Highest Selling Items') + facet_wrap(~year) + theme(plot.title=element_text(size=15))+ theme_minimal()
#2018 - may,august and dec had high prices , 2019- may and June had high prices

################################################################################################

#avg analysis for weekday 
#for different for different months

int_week <- international %>% 
  mutate( year= year(international$InvoiceDate),month=month(international$InvoiceDate,label = TRUE),week=wday(international$InvoiceDate,label = TRUE),day=day(international$InvoiceDate),hour=hour(international$InvoiceDate)) %>%
  group_by(year,week) %>%
  summarise(total=mean(NetFare))
View(int_week)
ggplot(data = int_week, aes(x=(week),y=total,fill=as.factor(year)))  +  geom_bar(stat = 'identity',position = "dodge") + 
  theme(legend.position = "none")+ coord_flip() +
  labs(y = 'avg price', x = 'weekday', title = 'Avg price for different day of the week') 

ggplot(data = int_week) + geom_point(aes(week,total))+ facet_wrap(~year)

################################################################################################

#we can also find out maximum and minimum profit for different periods

int_month <- international %>% 
  mutate( year= year(international$InvoiceDate),month=month(international$InvoiceDate,label = TRUE),week=wday(international$InvoiceDate,label = TRUE),day=day(international$InvoiceDate),hour=hour(international$InvoiceDate)) %>%
  group_by(year,month) %>%
  summarise(total=sum(NetFare))
View(int_month)

ggplot(data = int_month, aes(month,total))  +  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+ coord_flip() +
  labs(y = 'total price', x = 'hours', title = 'Avg price for different months of the year') + facet_wrap(~year)

##################################################

# compute average based on each day 

international <- filter(airlines,ProductType=="Air",ItineraryType=="International")
international$date <- date(international$InvoiceDate)
View(international)
summary(international$NetFare)
international <- arrange(international, NetFare)

international <- international[501:10761,]
View(inter)
boxplot(international$NetFare)

int_avg <- international %>% group_by(date) %>% summarise(Avg=mean(NetFare))
View(int_avg)
summary(int_avg)
hist(int_avg$Avg)

########################### ###############################################

domestic_time <- read.csv("C:\\Users\\USER\\Downloads\\FinalDomesticDataset.csv")
View(domestic_time)
summary(domestic_time)
domestic_time <- domestic_time[,1:2]
class(domestic_time$InvoiceDate)
domestic_time$InvoiceDate <- as.Date(as.character(domestic_time$InvoiceDate))
class(domestic_time$AvgNetFare)
library(forecast)
library(xts)
dom_air <- xts(domestic_time$AvgNetFare, order.by = domestic_time$InvoiceDate,frequency = 7)
plot(dom_air)
View(dom_air)
library(ggplot2)
summary(dom_air)
hist(domestic_time$AvgNetFare)
shapiro.test(domestic_time$AvgNetFare)
View(domestic_time_test)
dom_air_train <- dom_air[1:406]
dom_air_test <- dom_air[407:436]
ggAcf(dom_air)
pacf(dom_air)
library(tseries)
adf.test(dom_air)
##################### arima / auto arima ############################
dom_arima <- auto.arima(dom_air_train,d=1,D=1,stepwise = FALSE,approximation = FALSE, trace = TRUE)
checkresiduals(dom_arima)
forecast_dom <- forecast(dom_air, h=30)
autoplot(forecast_dom)
data_dom <- data.frame(forecast_dom$mean,dom_air_test)
View(data_dom)
rmse_dom <- sqrt(mean((data_dom$forecast_dom.mean-data_dom$dom_air_test)^2))
rmse_dom
#293.683

# diff order
dom_arima1 <- arima(dom_air_train, order = c(0,1,1))
pacf(dom_arima1$residuals)
checkresiduals(dom_arima1)
forecast_dom <- forecast(dom_arima1, h=30)
data_dom <- data.frame(forecast_dom$mean,dom_air_test)
View(data_dom)
plot(forecast_dom)
rmse_dom1 <- sqrt(mean((data_dom$forecast_dom.mean-data_dom$dom_air_test)^2))
rmse_dom1
#198.12 for c(0,1,0)
#266 for 0,1,1
adf.test(dom_air)
acf(dom_air)
####################### ses ###############################

dom_ses <- ses(dom_air_train,h=30)
data_dom3 <- data.frame(dom_ses$mean,dom_air_test)
rmse_dom2 <- sqrt(mean((data_dom3$dom_ses.mean-data_dom3$dom_air_test)^2))
rmse_dom2
################### neural network ####################

netforc <- nnetar(dom_air_train,scale.inputs = TRUE)
forcast_neural <- forecast(netforc, PI=TRUE, h=30)
plot(forcast_neural)
View(data_dom4)
forcast_neural
data_dom4 <- data.frame(forcast_neural$mean,dom_air_test)
rmse_dom3 <- sqrt(mean((data_dom4$forcast_neural.mean-data_dom4$dom_air_test)^2))
rmse_dom3
#195.548

#################### facebook data ####################
ds <- domestic_time$InvoiceDate
y <- domestic_time$AvgNetFare
data_facebook <- data.frame(ds,y)
data_facebook_train <- data_facebook[1:406,]
data_facebook_test <- data_facebook[407:436,]
library(prophet)
m <- prophet(data_facebook,daily.seasonality = TRUE)
future <- make_future_dataframe(m, periods = 180 , freq="day")
View(future)
predict <- predict(m,future)
View(predict)
plot(m,predict)
pred.test <- predict[407:436,]
data.dom5 <- data.frame(pred.test$yhat,data_facebook_test)
View(data.dom5)
rmse_dom5 <- sqrt(mean((data.dom5$pred.test.yhat-data.dom5$y)^2))
rmse_dom5
#588.504
################# ets ####################
dm_etc <- ets(dom_air_train,model="AAN")
forecast_ets <- forecast(dm_etc,h=30)
plot(forecast_ets)
data_dom5 <- data.frame(forecast_ets$mean,dom_air_test)
rmse_dom4 <- sqrt(mean((data_dom5$forecast_ets.mean-data_dom$dom_air_test)^2))
rmse_dom4
#253.449
View(dom_air_test)
################ naive model ####################
library(TTR)
dom_naive <- naive(dom_air_train ,h=30)
autoplot(dom_naive)
data_dom6 <- data.frame(dom_naive$mean,dom_air_test)
rmse_dom5 <- sqrt(mean((data_dom6$dom_naive.mean-data_dom6$dom_air_test)^2))
rmse_dom5
#198.3123

############ model based ####################

dom.data <- domestic_time
dom.data["t"] <- 1:436
dom.data$tsq <- dom.data$t*dom.data$t
View(dom.data)
dom.data.train <- dom.data[1:406,]
dom.data.test <- dom.data[407:436,]

dom.mod1 <- lm(dom.data.train$AvgNetFare~t+tsq , data = dom.data.train)
dom.mod1
predict.mod1 <- predict(dom.mod1,dom.data.test)
sqrt(mean((predict.mod1-dom.data.test$AvgNetFare)^2))
#261.65

#################### #####################

random <- dom.data  
random$day <- day(random$InvoiceDate)
random$week <- wday(random$InvoiceDate)
random$month <- month(random$InvoiceDate)
head(random)
library(randomForest)
forest <- randomForest(random$AvgNetFare ~ random$week+ random$day + random$month)
forest
varImpPlot(forest)

################## ######################
dom_air_train <- ts(dom_air_train, start = c(2018,4),frequency = 365)
autoplot(dom_air_train) +
  autolayer(meanf(dom_air_train, h=30),
            series="Mean", PI=FALSE) +
  autolayer(naive(dom_air_train, h=30),
            series="Naïve", PI=FALSE) 
autolayer(snaive(dom_air_train, h=30),
          series="Seasonal naïve", PI=FALSE) +
  ggtitle("Seasonal Naive Method") +
  xlab("Date") + ylab("Price") + 
  guides(colour=guide_legend(title="Forecast"))  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))

smod <- snaive(dom_air_train,h=2*15,drift=FALSE)
plot(smod)

sqrt(mean((smod$mean-dom_air_test)^2))
####################################### ###########################################

international_time <- read.csv("C:\\Users\\USER\\Downloads\\FinalInternatioalDataset.csv")
View(international_time)
international_time <- international_time[,1:2]
class(domestic_time$InvoiceDate)
international_time$InvoiceDate <- as.Date(as.character(international_time$InvoiceDate))

int_air <- xts(international_time$AvgNetFare, order.by = international_time$InvoiceDate,frequency = 365)
View(int_air)
summary(int_air)
plot(int_air)

int_air_train <- int_air[1:380]
int_air_test <- int_air[381:436]

adf.test(int_air)
#stationary 
##################### arima / auto arima ############################

int_arima <- auto.arima(int_air_train)
int_arima
int_forecast <- forecast(int_arima,h=56)
int_data <- data.frame(int_forecast$mean, int_air_test)
int_rmse <- sqrt(mean((int_data$int_forecast.mean-int_data$int_air_test)^2))
int_rmse
#2575

checkresiduals(int_arima)
autoplot(int_forecast)
summary(int_arima)

################# neural network ################################
neural_int_air <- nnetar(int_air,scale.inputs = TRUE)
int_forecast1 <- forecast(neural_int_air,PI=TRUE, h=205)
int_data1 <- data.frame(int_forecast1$mean,int_air_test)
int_rmse1 <- sqrt(mean((int_data1$int_forecast1.mean-int_data1$int_air_test)^2))
int_rmse1
#2406.02

plot(log(int_air_train))
################### manual arima #########################

acf(int_air_train)
pacf(int_air_train)
int_arima_1 <- arima(int_air_train, order = c(15,1,0))
checkresiduals(int_arima_1)
int_forecast2 <- forecast(int_arima_1,h=30)
int_data2 <- data.frame(int_forecast2$mean,int_air_test)
int_rmse2 <- sqrt(mean((int_data2$int_forecast2.mean-int_data2$int_air_test)^2))
int_rmse2
#2368.333
################## #######################

int_ses <- ses(int_air_train,h=30)
plot(int_ses)

plot(diff(int_air_train,lag = 15))
################# ################################# 

