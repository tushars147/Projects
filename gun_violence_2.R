library(tidyverse)
library(janitor)
library(lubridate)
library(data.table)
library(zoo)
library(VIM)
library(gridExtra)
library(splitstackshape)
library(tidytext)
library(tm)
gun=read.csv("C:\\Users\\USER\\Desktop\\github repo\\gun-violence-data_01-2013_03-2018.csv",header=TRUE,stringsAsFactors = FALSE)
dim(gun)

summary(gun)


# Data Cleaning 

#Before we dive into exploratory analysis,it is important that the data is cleaned to extract meaningful
#insights.Let us address the following problems one by one 

#* Converting to factor variables & handling dates

#* Removing useless informations

#* Handling missing values 

## Converting to factor variables & handling dates


factor_explicit =c("state","city_or_county")

gun[,(factor_explicit):=lapply(.SD,factor),SDcols=factor_explicit]
sapply(gun,class)

#For other columns,the data is provided such that both the factor levels and factor is in the same column seperated by || 
#.We will use the `splitstackshape` package to deal with such columns.

#But before that let us focus on the date column and convert them into proper date format from character.
#Lets first get the top 5 row items from date field.

head(gun$date)

gun$date_formated=as.Date(gun$date,format="%Y-%m-%d")
head(gun$date_formated)

## Removing useless information 

#We find that there are some columns which might be unnecessary for our exploratory data analysis.
#According to me,I would remove sources,incident_url,source_url,incident_url_fields_missing from the dataframe.Lets remove it,

remove <- c("sources","source_url","incident_url_fields_missing","incident_url")

names(gun)
## Handling Missing Values :

na_values <-gun %>% map_dbl(~sum(is.na(.)))
round((na_values/nrow(gun))*100,2)

#From the  proportion of missing values summary it is understood that the dataset is having heavy missing values.Lets visualise them to understand better.


aggr(gun, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)


#The table gives a summary of the missing values sorted in descending order whereas the visual represents the combination of missing values in each column.

#We also find that except for n_guns_involved column which has 41 % of missing values all the other proportions of missing values in the column is lower.


# Exploratory Data Analysis:

theme_function=function() {theme_bw()+theme(legend.position="none",plot.title=element_text(size=16,hjust=0.5),axis.text.x=element_text(angle=90))}


## Distribution of Incidents over state:

#We want to get a birdseye view of the number of incidents that have taken place over the state.Lets visualise them.

ggplot(gun,aes(state,..count..))+geom_histogram(stat="count",fill="blue")+theme_function()+labs(x="State",y="Count",title="Incidents")

#We find that the states -  Illinos,California,Texas have higher incidents of gun shooting.

## Trend of incidents over time.

L#ets understand whether there were any patterns in the incidents over the year,month.

gun$year=as.factor((year(gun$date)))
gun$month=lubridate::month(gun$date,label=TRUE,abbr=TRUE)
temp=gun %>% group_by(year,month) %>% summarise(count=n())
ggplot(temp,aes(month,count,color=year))+geom_line(aes(group=year),size=0.9)+geom_point(alpha=0.5)+theme_bw()+theme(legend.position="bottom")+labs(x="Month",y="Count of incidents")

#We find that the year 2013 has recorded least incidents ( atleast according to the data or we can assume that most of the incidents were not recorded).After 2013 the number of incidents were never <2000 in any month.Therefore our assumption can be safely considered.We see a overall trend in the month of feb ,June,July where there is a decline from the immediate previous month.

T#he data is consistent for the years 2015 to 2016.For 2018 we have only 3 month data.Is there any daily trend ? Lets find out.

gun$day=lubridate::wday(gun$date,label=TRUE,abbr=TRUE)

temp=gun %>% group_by(year,day) %>% summarise(count=n())
ggplot(temp,aes(day,count,color=year))+geom_line(aes(group=year),size=0.9)+geom_point(alpha=0.5)+theme_bw()+theme(legend.position="bottom",axis.text.x = element_text(angle=90))+labs(x="Day",y="Count of incidents")
#We find that for the years 2018 and 2013 ,we cant conclude anything since the data is incomplete.
#There is a general trend observed over the years as seen from the graph.Sundays has a rise in the incidents whereas Mon and Tue are comparatively lower.

## Time Series for Number of people killed and injured:

#Let us now visualise the number of people killed or injured in the incident as a function of time.

temp = gun %>% group_by(year) %>% summarise(people_killed=sum(n_killed),people_injured=sum(n_injured),number_of_incidents=n())
g1=ggplot(temp,aes(number_of_incidents,people_killed))+geom_point(aes(size=people_killed,col=year))+theme_function()+theme(legend.position="bottom")+labs(x="Number of incidents",y="People Killed",title="",size="People Killed",col="Year")
g2=ggplot(temp,aes(number_of_incidents,people_injured))+geom_point(aes(size=people_killed,col=year))+theme_function()+theme(legend.position="bottom")+labs(x="Number of incidents",y="People Injured",title="",size="People Injured",col="Year")
grid.arrange(g1,g2,nrow=2)

#We find that there is a perfect cause and effect relationship.As the number of incidents rises ,the number of people injured or killed has rised for the year.This is nothing strange and is very normal.


## Age distribution of preparators

#We want to find out how the preparators age is distributed.

#Now lets have a sneek peak of the column where the age details are present.

head(gun$participant_age)

age=cSplit(gun,c("participant_age"),sep="||",direction="long",drop=FALSE)
age$age=gsub(".*::","",age$participant_age)
age$age=as.numeric(age$age)
head(age$age)

#Perfect.Now lets do the visualisation and infer the findings.

ggplot(age,aes(x="Age",age))+geom_boxplot(fill="blue")+theme_function()+labs(x="",y="Age")

#We see a large number of outliers and there are also wierd values like 300,220 !!!!!.


## Gun Type 

##Now let us use the same method to know about the type of gun used.


gun_type=cSplit(gun,c("gun_type"),sep="||",direction="long",drop=FALSE)
gun_type$type_split=gsub(".*:","",gun_type$gun_type)
temp= gun_type %>% group_by(type_split) %>% summarise(count=n())%>% arrange(desc(count))

## Incident Characteristics:

##Lets visualise the characteristics of each incident.

incident_chara=cSplit(gun,c("incident_characteristics"),sep="||",direction="long",drop=FALSE)
temp= incident_chara %>% group_by(incident_characteristics) %>% summarise(count=n())%>% arrange(desc(count))
ggplot(head(temp,20),aes(factor(incident_characteristics,incident_characteristics),count,fill=incident_characteristics))+geom_bar(stat="identity")+theme_function()+labs(x="Incident Characteristics",y="Count",title="")+coord_flip()

#Since the output gave 600+ types,I visualized the top 20 characteristics.We see that most of the time the person is injured or died once shot.And there were also many cases where the incident was non-shooting or persons escaped with no injuries.

## Participant status:

gun_filter= gun %>% filter(year=='2017')
type=cSplit(gun_filter,c("participant_status"),sep="||",direction="long",drop=TRUE)
type$participant_status=gsub(".*:","",type$participant_status)
head(type$participant_status)
total=length(unique(type$incident_id))
temp = type %>% group_by(participant_status) %>% summarise(count=n()) %>% mutate(perc=round((count/total)*100,2)) %>% arrange(desc(count))
ggplot(temp,aes(factor(participant_status,participant_status),count,fill=participant_status))+geom_bar(stat="identity")+theme_function()+geom_text(aes(label=paste0(perc,"%",sep=" ")),hjust=0,vjust=0.5,size=4,color='black',fontface='bold')+labs(x="Participant Status",y="Count",title='Year:2017 - Participant Status')+coord_flip()


#As infered from previous graphs,we find that nearly in half of the instances people were injured followed closely by arrest.28 % of the people were killed where as nearly same amount were unharmed.

# Text Analytics

#For the purpose of analysis,let us extract notes column into a seperate df.

library(quanteda)
text=gun$notes
head(text)
#Now for creating a wordcloud,lets create a corpus from the text.

corp=corpus(text)
head(summary(corp))
## Analysing some keywords:

kw <- c('killed','police','terrorist')
head(kwic(corp,phrase(kw),window=2,valuetype="regex"),10)

## Topic Modelling 

#Now that we are done with the keyword search lets play with the text and find out whether the words echo more than one theme.
library(topicmodels)
topic <- dfm(corp,verbose = FALSE,remove_punct=TRUE,remove=stopwords('en'))
dtm <- convert(topic,to="topicmodels")
lda <- topicmodels::LDA(dtm,k=2,control=list(seed=100))
gun_topics <- tidy(lda,matrix="beta")

#Now that we have applied LDA lets visualise,
topic_top_terms <- gun_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

topic_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#From the visuals,we understand that the top 3 words in the 2 groups have been more or less the same meaning that all the incidents have one characteristic in particular that can be expressed in only one way.


# Conclusion 

#Thus with this dataset we analysed various factors that were tied to the gun violence and made text analysis to mine some valuable insights.The dataset presented an unique challenge where there were few columns having row values that were a mini row values in themselves !!!! . We used standard available libraries in r to tackle this problem and successfully 'tidied' the data for analysis pupose.Thanks for reading through ....

