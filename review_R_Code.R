library(itunesr)

#Latest (Page 1) SC Mobile Reviews for Top Countries
scin_rev <- getReviews(460909294,'in',1)
scsg_rev <- getReviews(367337298,'sg',1)
scae_rev <- getReviews(535729255,'ae',1)
schk_rev <- getReviews(445795688,'hk',1)

#Displaying the column names 
names(scin_rev)

View(scin_rev)
View(scsg_rev)
View(scae_rev)
View(schk_rev)

#Ratings count from the 50 Reviews
table(scin_rev$Rating)
table(scsg_rev$Rating)
table(scae_rev$Rating)
table(schk_rev$Rating)

#Ratings Trend

library(highcharter)
library(dplyr)
library(lubridate)

dt_in <- scin_rev
dt_sg <- scsg_rev
dt_ae <- scae_rev
dt_hk <- schk_rev

dt_in$Date <- as.Date(dt_in$Date)
dt_sg$Date <- as.Date(dt_sg$Date)
dt_ae$Date <- as.Date(dt_ae$Date)
dt_hk$Date <- as.Date(dt_hk$Date)

dt_in$Rating <- as.numeric(dt_in$Rating)    
dt_sg$Rating <- as.numeric(dt_sg$Rating)    
dt_ae$Rating <- as.numeric(dt_ae$Rating)    
dt_hk$Rating <- as.numeric(dt_hk$Rating)    

dt_in <- dt_in %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2))
dt_sg <- dt_sg %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2))
dt_ae <- dt_ae %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2))
dt_hk <- dt_hk %>% select(Date,Rating) %>% group_by(Date) %>% summarise(Rating = round(mean(Rating),2))

highchart() %>%   hc_add_series_times_values(dt_in$Date,dt_in$Rating, name = 'Average Rating - India')
highchart() %>%   hc_add_series_times_values(dt_sg$Date,dt_sg$Rating, name = 'Average Rating - Singapore')
highchart() %>%   hc_add_series_times_values(dt_ae$Date,dt_ae$Rating, name = 'Average Rating - UAE')
highchart() %>%   hc_add_series_times_values(dt_hk$Date,dt_hk$Rating, name = 'Average Rating - Hongkong')

#Sentiment Analysis 

library(sentimentr)

rev_in <- as.character(scin_rev$Review)
rev_sg <- as.character(scsg_rev$Review)
rev_ae <- as.character(scae_rev$Review)
rev_hk <- as.character(schk_rev$Review)

in_sentiment_scores <- rev_in %>% sentiment_by(by=NULL)
sg_sentiment_scores <- rev_sg %>% sentiment_by(by=NULL)
ae_sentiment_scores <- rev_ae %>% sentiment_by(by=NULL)
hk_sentiment_scores <- rev_hk %>% sentiment_by(by=NULL)

highchart() %>% hc_xAxis(in_sentiment_scores$element_id) %>% hc_add_series(in_sentiment_scores$ave_sentiment, name = 'Reviews Sentiment Scores - India')
highchart() %>% hc_xAxis(sg_sentiment_scores$element_id) %>% hc_add_series(sg_sentiment_scores$ave_sentiment, name = 'Reviews Sentiment Scores - Singapore')
highchart() %>% hc_xAxis(ae_sentiment_scores$element_id) %>% hc_add_series(ae_sentiment_scores$ave_sentiment, name = 'Reviews Sentiment Scores - UAE')
highchart() %>% hc_xAxis(hk_sentiment_scores$element_id) %>% hc_add_series(hk_sentiment_scores$ave_sentiment, name = 'Reviews Sentiment Scores - Hongkong')
