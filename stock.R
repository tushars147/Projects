library(tm)
library(RWeka)
library(magrittr)
library(Matrix)
library(glmnet)
library(ROCR)
library(ggplot2)

data <- read.csv("C:\\Users\\USER\\Desktop\\New Project Excelr\\Combined_News_DJIA.csv", stringsAsFactors = FALSE)
View(data)


# Make 'Date' column a Date object to make train/test splitting easier
data$Date <- as.Date(data$Date)

# Combine headlines into one text blob for each day and add sentence separation token
data$all <- paste(data$Top1, data$Top2, data$Top3, data$Top4, data$Top5, data$Top6,
                  data$Top7, data$Top8, data$Top9, data$Top10, data$Top11, data$Top12, 
                  data$Top13, data$Top14, data$Top15, data$Top16, data$Top17, data$Top18,
                  data$Top19, data$Top20, data$Top21, data$Top22, data$Top23, data$Top24,
                  data$Top25, sep=' <s> ')

data$all <- gsub('b"|b\'|\\\\|\\"', "", data$all)

# Get rid of all punctuation except headline separators
data$all <- gsub("([<>])|[[:punct:]]", "\\1", data$all)

# Reduce to only the three columns we need. 
data <- data[, c('Date', 'Label', 'all')]

View(data)

control <- list(
  removeNumbers = TRUE,
  tolower = TRUE,
  # exclude stopwords and headline tokens
  stopwords = stopwords()
)


dtm <- Corpus(VectorSource(data$all)) %>% 
  DocumentTermMatrix(control=control)

inspect(dtm[1:10,1:20])


split_index <- data$Date <= '2014-12-31'
head(split_index)

ytrain <- as.factor(data$Label[split_index])
head(ytrain)
xtrain <- Matrix(as.matrix(dtm)[split_index, ], sparse=TRUE)
xtrain[1:10,1:20]

ytest <- as.factor(data$Label[!split_index])
xtest <- Matrix(as.matrix(dtm)[!split_index, ], sparse=TRUE)

```

#Now we can fit a glmnet model using ridge regression. Here I do so using cross-validation to select the best lambda value. Then predictions are made. We are predicting probabilities in this case. 
#WHen we predict we use the lambda value returned by our cross-validation function.

```{r}

# Train the model
glmnet.fit <- cv.glmnet(x=xtrain, y=ytrain, family='binomial', alpha=0)
glmnet.fit
# Generate predictions
preds <- predict(glmnet.fit, newx=xtest, type='response', s='lambda.min')

# Put results into dataframe for plotting.
results <- data.frame(pred=preds, actual=ytest)


ggplot(results, aes(x=preds, color=actual)) + geom_density()


prediction <- prediction(preds, ytest)
perf <- performance(prediction, measure = "tpr", x.measure = "fpr")

auc <- performance(prediction, measure = "auc")
auc <- auc@y.values[[1]]


roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values))

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    ggtitle("ROC Curve") +
    ylab('True Positive Rate') +
    xlab('False Positive Rate')


# Necessary to set this option on Linux machines, otherwise the NGrameTokenizer will cause our 
# DocumentTermMatrix call to hang. 
options(mc.cores=1)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

control <- list(
    tokenize=BigramTokenizer,
    bounds = list(global = c(20, 500))

)


dtm <- Corpus(VectorSource(data$all)) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    DocumentTermMatrix(control=control)

split_index <- data$Date <= '2014-12-31'


ytrain <- as.factor(data$Label[split_index])
xtrain <- Matrix(as.matrix(dtm)[split_index, ], sparse=TRUE)

ytest <- as.factor(data$Label[!split_index])
xtest <- Matrix(as.matrix(dtm)[!split_index, ], sparse=TRUE)



#Now we can fit a glmnet model exactly the same as before, setting alpha to 0 indicating that we want to use ridge regression.



# Train the model
glmnet.fit <- cv.glmnet(x=xtrain, y=ytrain, family='binomial', alpha=0)

# Generate predictions
preds <- predict(glmnet.fit, newx=xtest, type='response', s="lambda.min")

# Put results into dataframe for plotting.
results <- data.frame(pred=preds, actual=ytest)


prediction <- prediction(preds, ytest)
perf <- performance(prediction, measure = "tpr", x.measure = "fpr")

auc <- performance(prediction, measure = "auc")
auc <- auc@y.values[[1]]


roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values))

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
    geom_ribbon(alpha=0.2) +
    geom_line(aes(y=tpr)) +
    geom_abline(slope=1, intercept=0, linetype='dashed') +
    ggtitle("ROC Curve") +
    ylab('True Positive Rate') +
    xlab('False Positive Rate')
