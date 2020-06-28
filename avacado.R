suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(corrgram))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(treemap))
suppressPackageStartupMessages(library(repr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(plotrix))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tibbletime))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(smooth))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(fpp2))

df <- read.csv("C:\\Users\\USER\\Desktop\\github repo\\avocado.csv")
View(df)

summary(df)

#variation of average price
ggplot(df, aes(x=AveragePrice, fill=type)) + geom_density() + facet_wrap(~type) + 
  theme(plot.title=element_text(hjust=0.5), legend.position="bottom") + labs(title="Avocado Price by Type")

# Change the date column from factor to date
df$Date <- as.Date(df$Date, "%Y-%m-%d")
class(df$Date)

# Sort the dates
df <- df[order(as.Date(df$Date, format="%Y-%m-%d")),]
View(df)
#it is a weekly data , prices around the week .......collected for the week,

price_trend <- df %>% select(Date, AveragePrice, type) %>%
  ggplot(aes(x=Date, y=AveragePrice)) + geom_area(aes(color=type, fill=type), alpha = 0.3, position = position_dodge(0.8)) + 
  theme_minimal() +  scale_color_manual(values = c("#ED7921", "#62BE51")) + scale_fill_manual(values = c("#FD833E", "#B8FC5F"))

price_trend

# Create a Facet Wrap for each product
ggplot(data = df, aes(x = Date, y = AveragePrice, col=type)) +
  geom_line() +
  facet_wrap(~ type) + theme_minimal() + theme(legend.position="bottom")
#avg price for organic vary a lot 
library(tibbletime)

# Filter by type
organic <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "organic")
conventional <- df %>% select(Date, AveragePrice, type, Total.Volume) %>% filter(type == "conventional")

organic <- as_tbl_time(organic, index=Date)
organic <- as_period(organic, '1 month')


# Conventional Avocadoes
conventional <- as_tbl_time(conventional, index=Date)
conventional <- as_period(conventional, '1 month')

# Now let's show monthly avocadoes price

#average price varying over time 
options(repr.plot.width=8, repr.plot.height=6)
conventional_monthly <- conventional %>%
  ggplot(aes(x=Date, y=AveragePrice)) + geom_line(color="red") + theme_economist()
# we can see that there is seasonality .. increasing at the start of the year and then decreasing 
# every year it is increasing 

# Let's create a volume chart
conventional_volume <- conventional %>%
  ggplot(aes(x=Date, y=Total.Volume)) + geom_bar(stat='identity', fill="#7FB3D5", color="black")+ theme_economist()+
  geom_smooth(method="loess", color="red")

organic_monthly <- organic %>% 
  ggplot(aes(x=Date, y=AveragePrice)) + geom_line(color="black") + theme_economist() 

organic_volume <- organic %>%
  ggplot(aes(x=Date, y=Total.Volume)) + geom_bar(stat='identity', fill="#58D68D",color="black") + theme_economist() + 
  geom_smooth(method="loess", color="red")
# there is a linear increase in the trend for the volume of organic avocado. 

plot_grid(conventional_monthly, organic_monthly,conventional_volume, organic_volume, nrow=2, ncol=2)

original_df <- df

seasonal_df <- original_df

seasonal_df$month_year <- format(as.Date(original_df$Date), "%Y-%m")
seasonal_df$month <- format(as.Date(original_df$Date), "%m")
seasonal_df$year <- format(as.Date(original_df$Date), "%Y")

seasonal_df$monthabb <- sapply(seasonal_df$month, function(x) month.abb[as.numeric(x)])
seasonal_df$monthabb = factor(seasonal_df$monthabb, levels = month.abb)

View(seasonal_df)

# # Let's see if there are seasonal patterns with conventional avocadoes
ggplot(seasonal_df, aes(x = AveragePrice, fill = as.factor(year))) + 
  geom_density(alpha = .5) + 
  theme_economist_white() +
  facet_wrap(~ year) +guides(fill = FALSE) + labs(title="Distribution of Prices by year", x = 'Average Price', y = 'Density') 


# Detecting seasonality patterns
conv_patterns <- seasonal_df %>% select(monthabb, AveragePrice, type) %>% filter(type == "conventional") %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#F35D5D", aes(size=avg)) + geom_line(group=1, color="#7FB3D5") + 
  theme_economist_white() + labs(title="Conventional Avocados", x="Month", y="Average Price")


org_patterns<- seasonal_df %>% select(monthabb, AveragePrice, type) %>% filter(type == "organic") %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#F35D5D", aes(size=avg)) + geom_line(group=1, color="#58D68D") + 
  theme_economist_white()  + 
  labs(title="Organic Avocados", x="Month", y="Average Price")

plot_grid(conv_patterns, org_patterns, nrow=2)
# over the year we can see how prices vary 
# it is cheap at the start of the year and at the ned of the year 
# linearly increases over the months from the start and ends with decrease in the trend 


# Hmm let's see if the Seasonality pattern is maintained each year.
options(repr.plot.width=8, repr.plot.height=6) 
conv_pat_yearly <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#F7DC6F") + facet_wrap(~as.factor(year)) + 
  theme_minimal() + 
  labs(title="Seasonal Fluctuations \n Convenctional Avocados", x="Month", y="Average Price")

org_pat_yearly <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#E74C3C") + facet_wrap(~as.factor(year)) + 
  theme_minimal() 

plot_grid(conv_pat_yearly, org_pat_yearly, nrow=2)

# there's the same pattern in the increase and decrease over the average price over the year for all the years



options(repr.plot.width=10, repr.plot.height=7) 
r_avg <- seasonal_df %>% group_by(year, monthabb) %>%  select(type, year, monthabb, AveragePrice) %>% 
  filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>%
  summarize(avg=mean(AveragePrice))



structured_data <- spread_(r_avg, key="year", value="avg")


colnames(structured_data) <- c("Months", "First_year", "Second_year", "Third_year")


structured_data$first_pct <- NA
structured_data$second_pct <- NA

structured_data$first_pct <- (structured_data$Second_year - structured_data$First_year)/structured_data$First_year
structured_data$second_pct <- (structured_data$Third_year - structured_data$Second_year)/structured_data$Second_year


structured_data<- structured_data %>% 
  mutate(first_cond=ifelse(first_pct > 0, "Positive", "Negative"),
         second_cond=ifelse(second_pct > 0, "Positive", "Negative"))


firstp_change <- ggplot(structured_data) +
  geom_segment( aes(x=Months, xend=Months, y=First_year, yend=Second_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=First_year), color="#F74B4B", size=3 ) +
  geom_point( aes(x=Months, y=Second_year),color="#36ACD7", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#F4F6F7")
  ) +
  labs(title="Conventional Avocado Price changes \n (2015 - 2016)", x="Months", y="Price",
       caption="Red: Year of 2015, Blue: Year of 2016")


secondp_change <- ggplot(structured_data) +
  geom_segment( aes(x=Months, xend=Months, y=Second_year, yend=Third_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=Second_year), color="#36ACD7", size=3 ) +
  geom_point( aes(x=Months, y=Third_year), color="#58FA58", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#F4F6F7")
  ) +
  labs(title="Conventional Avocado Price changes \n (2016 - 2017)", x="Months", y="Price",
       caption="Blue: Year of 2016, Green: Year of 2017" )

# plot_grid(firstp_change, secondp_change, ncol=2)

first_pct_dif <- structured_data %>% select(Months, first_pct, first_cond) %>%
  ggplot(aes(fill=first_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(first_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#F4F6F7"), legend.position="bottom") + 
  labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))

second_pct_dif <- structured_data %>% select(Months, second_pct, second_cond) %>%
  ggplot(aes(fill=second_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(second_pct,2) * 100), color="black") + 
  theme_economist() + 
  theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#F4F6F7"), legend.position="bottom") + labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))



plot_grid(firstp_change, secondp_change, first_pct_dif, second_pct_dif,  nrow=2, ncol=2)

# Organic avvocados

r_avg_org <- seasonal_df %>% group_by(year, monthabb) %>%  select(type, year, monthabb, AveragePrice) %>% 
  filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>%
  summarize(avg=mean(AveragePrice))

View(r_avg_org)

structured_data_org <- spread_(r_avg_org, key="year", value="avg")

colnames(structured_data_org) <- c("Months", "First_year", "Second_year", "Third_year")

structured_data_org$first_pct <- NA
structured_data_org$second_pct <- NA

structured_data_org$first_pct <- (structured_data_org$Second_year - structured_data_org$First_year)/structured_data$First_year
structured_data_org$second_pct <- (structured_data_org$Third_year - structured_data_org$Second_year)/structured_data$Second_year


structured_data_org<- structured_data_org %>% 
  mutate(first_cond=ifelse(first_pct > 0, "Positive", "Negative"),
         second_cond=ifelse(second_pct > 0, "Positive", "Negative"))


firstp_change<- ggplot(structured_data_org) +
  geom_segment( aes(x=Months, xend=Months, y=First_year, yend=Second_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=First_year), color="#F74B4B", size=3 ) +
  geom_point( aes(x=Months, y=Second_year),color="#36ACD7", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  labs(title="Organic Avocado Price changes \n (2015 - 2016)", x="Months", y="Price",
       caption="Red: Year of 2015, Blue: Year of 2016")


secondp_change<- ggplot(structured_data_org) +
  geom_segment( aes(x=Months, xend=Months, y=Second_year, yend=Third_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=Second_year), color="#36ACD7", size=3 ) +
  geom_point( aes(x=Months, y=Third_year), color="#58FA58", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  labs(title="Organic Avocado Price changes \n (2016 - 2017)", x="Months", y="Price",
       caption="Blue: Year of 2016, Green: Year of 2017" )

plot_grid(firstp_change, secondp_change, ncol=2)

first_pct_dif_org <- structured_data_org %>% select(Months, first_pct, first_cond) %>%
  ggplot(aes(fill=first_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(first_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#DCFCE6"), legend.position="bottom") + 
  labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))

second_pct_dif_org <- structured_data_org %>% select(Months, second_pct, second_cond) %>%
  ggplot(aes(fill=second_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(second_pct,2) * 100), color="black") + 
  theme_economist() + 
  theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#DCFCE6"), legend.position="bottom") + labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))



plot_grid(firstp_change_org, secondp_change_org, first_pct_dif_org, second_pct_dif_org,  nrow=2, ncol=2)

options(repr.plot.width=8, repr.plot.height=6) 

# Let's create a seasonal column and plot a point line chart by each year.
seasonal_df$season <- ifelse(seasonal_df$month %in% c("03", "04","05"), "Spring",
                             ifelse(seasonal_df$month %in% c("06","07" ,"08"), "Summer",
                                    ifelse(seasonal_df$month %in% c("09","10","11"), "Fall", "Winter")))


options(repr.plot.width=10, repr.plot.height=8) 

conv.price <- seasonal_df %>% select(type,year, monthabb, AveragePrice) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice))
conv.price

org.price <- seasonal_df %>% select(type,year, monthabb, AveragePrice) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice))

conv.price <- ts(conv.price$avg, start=2015, frequency=12)
org.price <- ts(org.price$avg, start=2015, frequency=12)


conv.plot <- autoplot(conv.price, color="red") + 
  theme_economist()  + 
  labs(title="Average Price by Month \n Conventional Avocados", y="Average Price")

org.plot <- autoplot(org.price, color="red") + 
  theme_economist()  + 
  labs(title="Average Price by Month \n Organic Avocados", y="Average Price")


byyear.plot.conv <- ggseasonplot(conv.price, year.labels=TRUE, year.labels.left=TRUE) + theme_economist() + 
  labs(title="Average Conventional A. Price by Year \n for each month", y="Average Price") 


byyear.plot.org <- ggseasonplot(org.price, year.labels=TRUE, year.labels.left=TRUE) + theme_economist() + 
  labs(title="Average Organic A. Price by Year \n for each month", y="Average Price") 

seasonality_trends_conv <- window(conv.price, start=2015)
conv_plot_trends <- autoplot(seasonality_trends_conv) + theme_economist() + 
  labs(x="Year", y="Average Price", title="Conventional Avocados")

autocoor_conv <- ggAcf(org.price, lag=12) + labs(title="Autocorrelation for \n Conventional Avocados")


seasonality_trends_org <- window(org.price, start=2015)
org_plot_trends <- autoplot(seasonality_trends_org) + theme_economist() + 
  labs(x="Year", y="Average Price", title="Organic Avocados")

autocoor_org <- ggAcf(org.price, lag=12) + labs(title="Autocorrelation for \n Organic Avocados")

plot_grid(conv_plot_trends, autocoor_conv, org_plot_trends, autocoor_org, ncol=2, nrow=2)

# Let's get it by week the average price
weekly_df <- original_df
weekly_df$week <- format(as.Date(original_df$Date), "%w")

View(weekly_df)

conv.price.weekly <- weekly_df %>% select(type,year,AveragePrice) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) 
org.price.weekly <- weekly_df %>% select(type,year,AveragePrice) %>% filter(type == "organic", year == c("2015", "2016", "2017")) 


weekly.conv.price <- ts(conv.price.weekly$AveragePrice, start=2015, frequency=12)
weekly.org.price <- ts(org.price.weekly$AveragePrice, start=2015, frequency=12)



weekly_trends_conv <- window(weekly.conv.price, start=2015)

weekly_trends_org <- window(weekly.org.price, start=2015)

conv_plot_weekly <- autoplot(weekly_trends_conv) + theme_economist() + 
  labs(x="Time", y="Average Price", title="Conventional Avocados \n (Weekly Time Series)")

org_plot_weekly <- autoplot(weekly_trends_org) + theme_economist()  + 
  labs(x="Time", y="Average Price", title="Organic Avocados \n (Weekly Time Series)")

autocoor_conv <- ggAcf(weekly.conv.price, lag=156, fill="#48a4ff") + theme_economist()  + 
  labs(title="Autocorrelations by Weekly Lags")

autocoor_org <- ggAcf(weekly.org.price, lag=156, fill="#48a4ff") + theme_economist()  + 
  labs(title="Autocorrelations by Weekly Lags")

plot_grid(conv_plot_weekly, autocoor_conv,org_plot_weekly,  autocoor_org, ncol=2, nrow=2)


# Using Smoothing average
sma_conv <- sma(conventional$AveragePrice, h=10) + theme_economist()

library(fpp2)

# Let's declare our data as time series
conv <- df %>% select(Date, AveragePrice, type) %>% filter(type == "conventional")
org <- df %>% select(Date, AveragePrice, type) %>% filter(type == "organic")
conv
# Conventional Avocados
conventional <- as_tbl_time(conv, index=Date)
conventional <- as_period(conventional, '1 month')
conventional$type <- NULL
# Organic Avocados
organic <- as_tbl_time(org, index=Date)
organic <- as_period(organic, '1 month')
organic$type <- NULL
organic
conv_ts <- ts(conventional[,2], start=c(2015, 1), frequency=12)
org_ts <- ts(organic[,2], start=c(2015, 1), frequency=12)

# The difference from month to month
# To remove the trend we take the first difference
differences_conv <- diff(conv_ts)

main_diff <- autoplot(differences_conv) + theme_minimal()

seasonality_diff <- ggseasonplot(differences_conv) + theme_minimal()

plot_grid(main_diff, seasonality_diff, nrow=2)

# ARIMA Model
# Y has trend unlike difference, it will take the difference behind the scenes d=1
# Stepwise will only use some models instead of all possible combinations
# approximation uses the model that approximates the best result to save time
arima_model_cv <- auto.arima(conv_ts, d=1, D=1, stepwise=FALSE, approximation=FALSE, trace=TRUE)
arima_model_or <- auto.arima(org_ts, d=1, D=1, stepwise=FALSE, approximation=FALSE, trace=TRUE)


print(summary(arima_model_cv))
checkresiduals(arima_model_cv) + theme_minimal()

options(repr.plot.width=10, repr.plot.height=7)

conv_forecast_sn <- autoplot(conv_ts) +
  autolayer(meanf(conv_ts, h=24),
            series="Mean", PI=FALSE) +
  autolayer(naive(conv_ts, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(conv_ts, h=24),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Conventional Avocado \n Seasonal Naive Method") +
  xlab("Date") + ylab("Price") + scale_color_manual(values=c("#FA5858", "#00BFFF", "#FF8000")) + 
  guides(colour=guide_legend(title="Forecast"))  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7"))

org_forecast_sn <- autoplot(org_ts) +
  autolayer(meanf(org_ts, h=24),
            series="Mean", PI=FALSE) +
  autolayer(naive(org_ts, h=24),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(org_ts, h=24),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Organic Avocado \n Seasonal Naive Method") +
  xlab("Date") + ylab("Price") + scale_color_manual(values=c("#FA5858", "#00BFFF", "#FF8000")) + 
  guides(colour=guide_legend(title="Forecast"))  + theme_economist() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#DCFCE6"), legend.position="none")


plot_grid(conv_forecast_sn, org_forecast_sn, nrow=2)


p1 <- autoplot(rescv_nv, color="red") + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method \n Conventional Avocados") + theme_economist() 
p2 <- autoplot(resorg_nv, color="red") + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method \n Organic Avocados") + theme_economist()

plot_grid(p1, p2, nrow=2)

sqrt(0.05354)

forecast_cv <- forecast(arima_model_cv, h=24)
# Include means including the last 60 months in order to see closer the forecast.
autoplot(forecast_cv, include=60) + theme_minimal()
                                                                                                                       
forecast_org <- forecast(arima_model_or, h=24)
# Include means including the last 60 months in order to see closer the forecast.
autoplot(forecast_org, include=60) + theme_minimal()

