library(tidyverse) # metapackage with lots of helpful functions
library(prophet)
library(VIM)
library(mda)
library(nnet)
library(ramify)
library(xgboost)
library(countrycode)

df <- read.csv("C:\\Users\\USER\\Desktop\\github repo\\hotel_bookings.csv")
df1 <- read.csv("C:\\Users\\USER\\Desktop\\github repo\\hotel_bookings.csv",stringsAsFactors = TRUE)

view(df)
library(dplyr)
ts <- df %>% group_by(reservation_status_date) %>% summarise(n=n())
head(ts)
#no. of reservation by date
ts$reservation_status_date <- as.Date(ts$reservation_status_date)
#no of reservation over the time period
ggplot(ts, aes(reservation_status_date, n)) + geom_line() + xlab("reservation date") + ylab("no. of bookings") + theme_economist()
# we can see that there are suddent peaks which indicate some days are either holidays or had some occassion which lead to increase in bookings

hist(ts$n)
outliers <- boxplot(ts$n, range=1.5, outline=TRUE)$out
ts$n <- ifelse(ts$n %in% outliers, NA, ts$n)
ts <- ts %>% filter(!is.na(n))
outliers <- boxplot(ts$n, range=1.5, outline=TRUE)$out
hist(ts$n)
# treated the outliers by removing the third quantile 

# Time Series analysis #

#Once the data is cleaned, the TS shows some patters that can be exploded to determine seasons. On the other hand, the
#reservations descrease continues the same, meaning hotels are facing a rough time.
ggplot(ts, aes(reservation_status_date, n)) + geom_line(col="blue")+xlab("reservation date") + ylab("no. of bookings") + theme_fivethirtyeight()

View(ts)

# Frequency is set with 365 because it's daily
components <- stl(ts(ts$n, frequency=365), 'periodic')
summary(components)
# seasonal, trend, remainder
plot(components)

colnames(ts) <- c('ds','y')
m <- prophet(ts, daily.seasonality=TRUE)
future <- make_future_dataframe(m, periods=365)
forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)

#Prophet is showing us a prediction for the next year, with a clearly negative trend. Seeming that Saturday is the worst day 
#for the hotel. Now, from October until January the hotel reservations have been decreasing, maybe a low season.

#comparing the trend chart we're confirming that sales decreasing, maybe a campaign or a specific events are 
#impacting on sales. The forecast is yelling us that it'll continue.

#In this case, we can suggest to the hotel to take actions before the trend continues decreasing

# To improve reservations from October to January, the suggestion would be make a more aggresive campaing or 
#promotions looking to increase sales on that low season.

new_df <- df
# Let's remove the company column
new_df <- new_df[,!(names(new_df) %in% c('company'))]
# Now, let's replace NULL values to NA
new_df[new_df == 'NULL'] <- NA
# Once all NULL are replaced by NA, let's check that results match
apply(ifelse(is.na(new_df), 1, 0), 2, sum)
View(new_df)
summary(df)
summary(new_df)

#let's check hotel types and cancellation 
hotel_data <- new_df
# Visualize the distribution
ggplot(data = hotel_data, aes(x = hotel)) +
  geom_bar(stat = "count") +
  labs(title = "Booking Request by Hotel type",
       x = "Hotel type",
       y = "No. of bookings") +
  theme_classic() + scale_color_brewer(palette = "Set2")

# Check the distribution of hotel type for cancellation
table(hotel_data$is_canceled, hotel_data$hotel)
# Visualize the cancellation by hotel type
ggplot(data = hotel_data,
       aes(
         x = hotel,
         y = prop.table(stat(count)),
         fill = factor(is_canceled),
         label = scales::percent(prop.table(stat(count)))
       )) +
  geom_bar(position = position_dodge()) +
  geom_text(
    stat = "count",
    position = position_dodge(.9),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cancellation Status by Hotel Type",
       x = "Hotel Type",
       y = "Count") +
  theme_classic() +
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Cancelled", "Not Cancelled")
  )
#out of all the bookings that were made more than 65% were booking requests for City hotels and around 35% for resort hotels. 
#In both the hotel type, the proportion of cancellation is more than the confirmed status

# Cancellation ratio by Hotel Type based on the lead time. Lead time is the time gap between
# Booking made and the actual date of check in. We will visualize the data by using BoxPlot

ggplot(data = hotel_data, aes(
  x = hotel,
  y = lead_time,
  fill = factor(is_canceled)
)) +
  geom_boxplot(position = position_dodge()) +
  labs(
    title = "Cancellation By Hotel Type",
    subtitle = "Based on Lead Time",
    x = "Hotel Type",
    y = "Lead Time (Days)"
  ) +
  scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Cancelled", "Not Cancelled")
  ) + theme_light()
unique(hotel_data$arrival_date_year)
# since the data is only for 3 years we will go into year Visualization but we will visualize
# the Month wise analysis for hotel booking

# Organize the Month in proper order
hotel_data$arrival_date_month <-
  factor(hotel_data$arrival_date_month, levels = month.name)
# Visualize Hotel traffic on Monthly basis
ggplot(data = hotel_data, aes(x = arrival_date_month)) +
  geom_bar(fill = "steelblue") +
  geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + labs(title = "Month Wise Booking Request",
                      x = "Month",
                      y = "Count") +
  theme_classic()
# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(hotel_data, aes(arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() + geom_text(stat = "count", aes(label = ..count..), hjust = 1) +
  coord_flip() + scale_fill_discrete(
    name = "Booking Status",
    breaks = c("0", "1"),
    label = c("Cancelled", "Not Cancelled")
  ) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()
# note this data use the booking confirmation and not the check ins, so this graph shows the
# booking made for particular month and not the confirmed check ins.

ggplot(hotel_data, aes(arrival_date_month, fill = hotel)) +
  geom_bar(position = position_dodge()) +
  labs(title = "Booking Status by Month",
       x = "Month",
       y = "Count") + theme_bw()
# where are the people coming from
hotel_data_1 <- hotel_data[hotel_data$reservation_status == "Check-Out",]
# Subset the data to include the countries which has more than 1500 reservation request
# otherwise including all the country with few or occassional request to avoid the graph
# from being clumsy
sub_hotel <- hotel_data_1 %>% 
  group_by(country) %>% 
  filter(n() > 1500)

# Visualize the Travellor by Country.
sub_hotel$county_name <- countrycode(sub_hotel$country, 
                                     origin = "iso3c",
                                     destination = "country.name")

# Traveller by Country per hotel wise
ggplot(sub_hotel, aes(county_name, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Booking Status by Country",
       x = "Country",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())
# Total Stay Duration
ggplot(sub_hotel, aes(stays_in_weekend_nights + stays_in_week_nights)) + 
  geom_density(col = "red") +facet_wrap(~hotel) + theme_bw()
# Average daily rate by Hotel Type
ggplot(sub_hotel, aes(x = adr, fill = hotel, color = hotel)) + 
  geom_histogram(aes(y = ..density..), position = position_dodge(), binwidth = 20 ) +
  geom_density(alpha = 0.2) + 
  labs(title = "Average Daily rate by Hotel",
       x = "Hotel Price(in Euro)",
       y = "Count") + scale_color_brewer(palette = "Paired") + 
  theme_classic() + theme(legend.position = "top")
ggplot(sub_hotel, aes(customer_type, fill = hotel)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  labs(title = "Hotel Preference by Customer Type",
       x = "Customer Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank())
# Does the hotel charged differently for different customer type
ggplot(sub_hotel, aes(x = customer_type, y = adr, fill = hotel)) + 
  geom_boxplot(position = position_dodge()) + 
  labs(title = "Price Charged by Hotel Type",
       subtitle = "for Customer Type",
       x = "Customer Type",
       y = "Price per night(in Euro)") + theme_classic()
# Does allocation of different room lead to cancellation 
# keep only that data where reserved room is different than the room allocated

df <- subset(hotel_data, 
             as.character(hotel_data$reserved_room_type) != as.character(hotel_data$assigned_room_type))

table(df$is_canceled)
# Data Modelling

#Split data for training and test
sample_size = floor(0.8*nrow(hotel_data))
train_ind = sample(seq_len(nrow(hotel_data)), size = sample_size)
train = hotel_data[train_ind,]
test = hotel_data[-train_ind,]

train_new <- train  %>%  dplyr::select(-adr)

number_perfect_splits <- apply(X = train_new[-1], MARGIN = 2, FUN = function(col){
  t <- table(train$is_canceled, col)
  sum(t == 0)
})

# Descending order of perfect splits
order <- order(number_perfect_splits, decreasing = TRUE)
number_perfect_splits <- number_perfect_splits[order]

# Plot Graph
par(mar = c(10,2,2,2))
barplot(number_perfect_splits,
        main = "Number of perfect split vs feature",
        xlab = "", ylab = "Feature", las = 2, col = "wheat")
library(caret)
hotel_data <- df1
intrain <- createDataPartition(y = hotel_data$is_canceled,
                               p = 0.7,
                               list = FALSE)

training <- hotel_data[intrain,]
testing <- hotel_data[-intrain,]

training <- training  %>% dplyr::select(-reservation_status)
testing <- testing  %>% dplyr::select(-reservation_status)
library(party)
library(rpart)
library(rpart.plot)

str(training)

# Does allocation of different room lead to cancellation 
# keep only that data where reserved room is different than the room allocated

df2 <- subset(hotel_data, 
             as.character(hotel_data$reserved_room_type) != as.character(hotel_data$assigned_room_type))

table(df2$is_canceled)
mod_tree <- rpart(is_canceled ~ ., data = training, method = "class")
tree.pred <- predict(mod_tree, testing, type = "class")
a <- table(tree.pred, testing$is_canceled)
(sum(diag(a))/sum(a))*100
#82.338%
