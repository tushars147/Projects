camp <- read.csv("C:\\Users\\USER\\Downloads\\placement data.csv",stringsAsFactors = TRUE)
View(camp)
str(camp)
camp_df <- camp
camp_df <- camp_df[,c(-1,-15)]
View(camp_df)
dim(camp_df)
summary(camp_df)

#distribution of percentage of students for central and other boards for ssc
ssc1 <- ggplot(data = camp_df) + geom_histogram(aes(camp_df$ssc_p,fill=camp_df$ssc_b)) + facet_wrap(~camp_df$ssc_b) + theme_minimal() + theme_economist() + title(main="SSC") +guides(fill = FALSE)
#density distribution 
ssc2 <- ggplot(data = camp_df) + geom_density(aes(camp_df$ssc_p,fill=camp_df$ssc_b)) + facet_wrap(~camp_df$ssc_b) + theme_minimal() + theme_economist_white() +guides(fill = FALSE)

#distribution of percentage of students for central and other boards for hsc
hsc1 <- ggplot(data = camp_df) + geom_histogram(aes(camp_df$hsc_p,fill=camp_df$hsc_b)) + facet_wrap(~camp_df$hsc_b) + theme_minimal() + theme_economist() + title(main="SSC") + guides(fill = FALSE)
#density distribution 
hsc2 <- ggplot(data = camp_df) + geom_density(aes(camp_df$hsc_p,fill=camp_df$hsc_b,alpha=0.5)) + facet_wrap(~camp_df$hsc_b) + theme_minimal() + theme_economist_white() + guides(fill = FALSE)
#we can see the performance of central board students at hsc and ssc vs other boards students 
plot_grid(ssc1,hsc1,ssc2,hsc2,nrow =2)

#no. of students in different hsc streams
hs1 <- ggplot(data = camp_df) + geom_bar(mapping = aes(hsc_s ,fill=hsc_b))

ggplot(data=camp_df) + geom_histogram(aes(hsc_p,col="red")) + facet_wrap(~hsc_s) + theme_minimal() + ggtitle(label = "Distribution of percentage") + guides(fill=FALSE)
# we can see commerce and science students had beter percentage in 12 th std 

a <- ggplot(data = camp_df) + geom_bar(aes(hsc_s,fill=degree_t),col="red") + facet_wrap(~degree_t) + theme_minimal() + theme_bw()
#we can see some commerce students also choose sci and tech for their graduation 
# also science students choose comms and mgmt 

b <- ggplot(data = camp_df) + geom_histogram(aes(hsc_p,fill=degree_t)) + facet_wrap(~degree_t) + theme_excel_new() + theme_bw()
# we can see most of the people opt for comm& mgmt

c <- ggplot(data = camp_df) + geom_density(aes(degree_p)) + facet_wrap(~ degree_t) + theme_minimal() + theme_bw() + theme_economist()
# distribution of percentage for various degrees

d <- ggplot(data = camp_df) + geom_boxplot(aes(degree_p,fill=degree_t)) 
plot_grid(a,b,c,d, nrow=2,2)

#######

e <- ggplot(data = camp_df) + geom_bar(aes(degree_t,fill=workex)) + facet_wrap(~workex) + theme_bw()
table(camp_df$degree_t,camp_df$workex)

f <- ggplot(data = camp_df) + geom_histogram(aes(etest_p,col="pink")) + facet_wrap(~degree_t) + theme_minimal() + theme_bw()

plot_grid(e,f,nrow=2)

#####

g <- ggplot(data = camp_df) + geom_bar(aes(specialisation,col="red")) + facet_wrap(~degree_t) + theme_bw()
h <- ggplot(data = camp_df) + geom_histogram(aes(degree_p,col="green")) + facet_wrap(~specialisation) + theme_bw()

plot_grid(g,h, nrow = 2)

i <- ggplot(data = camp_df) + geom_density(aes(mba_p)) + facet_wrap(~specialisation) + theme_bw()                                     

j <- ggplot(data = camp_df) + geom_bar(aes(status)) + facet_wrap(~degree_t)

plot_grid(i,j,nrow=2)

plot_grid(a,b,c,d,e,f,g,h,i,j, nrow = 5,2)
###

cat_data = subset(camp_df, select = c(1,3,5,6,8,9,11))

cat_data %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar(stat='count', width = 0.5) + 
  geom_text(stat = 'count', aes(label=..count..), vjust = 1, size = 3.5, color ="white")+
  ggtitle("Categorical Variables Distribution")

##### ~~~ MODELING ~~~ ####

camp.mod <- camp_df
str(camp.mod)

camp.mod.train <- camp.mod[1:170,]
table(camp.mod.train$status)
camp.mod.test <- camp.mod[171:215,]
table(camp.mod.test$status)

#decision treee
library(party)
dec.mod <- ctree(status~. , data = camp.mod.train)
plot(dec.mod)

dec.mod1 <- rpart(status ~. , data = camp.mod.train)
rpart.plot::rpart.plot(dec.mod1)

pred2 <- predict(dec.mod, camp.mod.test[,1:12])
table(pred2)

acc <- table(pred2, camp.mod.test$status)
sum(diag(acc))/sum(acc)
#0.711%

# randomForest

ran.mod <- randomForest(status ~. , data = camp.mod.train)
plot(ran.mod)

pred3 <- predict(ran.mod , camp.mod.test)
acc <- table(pred3, camp.mod.test$status)
sum(diag(acc))/sum(acc)
#0.777

#caret 

ctrl <- trainControl(method = "cv",
                    number = 10)

# Simple GBM

gbmfit <- train(status ~., 
                data = camp.mod.train, 
                method = "gbm", 
                verbose = TRUE, 
                trControl = ctrl)
plot(gbmfit)
gbmpreds <- predict(gbmfit, camp.mod.test[,1:12])
acc <- table(gbmpreds, camp.mod.test$status)
sum(diag(acc))/sum(acc)
#0.8666

########## ridge and lasso regression 
install.packages("glmnet")
library(glmnet)

# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 5,
                       verboseIter = T)

# ridge Model
set.seed(1234)
library(e1071)
ridge <- train(status~.,
               data=camp.mod.train,
               method='glmnet',
               tuneGrid= expand.grid(alpha=0,
                                     lambda=seq(0.0001,1,length=5)) ,
               trControl=custom)
plot(ridge)

ridge.pred <- predict(ridge,camp.mod.test[,1:12])
acc1 <- table(ridge.pred,camp.mod.test$status)
sum(diag(acc1))/sum(acc1)
#0.8666

#lasso 
set.seed(1234)
lasso <- train(status~.,
               data=camp.mod.train,
               method='glmnet',
               tuneGrid= expand.grid(alpha=1,
                                     lambda=seq(0.0001,1,length=5)) ,
               trControl=custom)
plot(lasso)
plot(lasso$finalModel, xvar="lambda")

lasso.pred <- predict(lasso,camp.mod.test[,1:12])
table(lasso.pred,camp.mod.test$status)
(23+16)/(23+16+6)
#0.866
plot(varImp(lasso))

#elastic

en <- train(status ~. , 
            data= camp.mod.train , 
            method='glmnet',
            tuneGrid=expand.grid(alpha=seq(0,1,length=10),
                                 lambda=seq(0.0001,1,length=5)),
            trControl= custom)
summary(en)
plot(en)
en.pred <- predict(en, camp.mod.test)
table(en.pred, camp.mod.test$status)
(39)/(15+24+6)
#0.8667
plot(varImp(en))

ggplot(varImp(en)) + 
  geom_bar(stat = 'identity', fill = 'steelblue', color = 'black') + 
  scale_y_continuous(limits = c(0, 105), expand = c(0, 0)) +
  theme_light()

library(adabag)

adaboost <- boosting.cv(status~. ,data = camp.mod.train , v = 10 , boos = TRUE, mfinal = 50)
summary(adaboost)
adaboost


set.seed(123)
xgbData <- camp_df
indexes <- sample(1:nrow(xgbData), size=0.8*nrow(xgbData))
XGBtrain.Data <- xgbData[indexes,]
XGBtest.Data <- xgbData[-indexes,]

formula = status~.
fitControl <- trainControl(method="cv", number = 3)
xgbGrid <- expand.grid(nrounds = 50,
                       max_depth = 12)
        


XGB.model <- train(formula, data = XGBtrain.Data,
                   method = "xgbTree"
                   ,trControl = fitControl
                   , verbose=0
                   , maximize=FALSE)


xgb.pred <- predict(XGB.model , XGBtest.Data)
table(xgb.pred , XGBtest.Data$status)
(31+11)/(32+11)
#0.9766

importance <- varImp(XGB.model)
plot(importance)
