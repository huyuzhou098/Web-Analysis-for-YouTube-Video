library(dplyr)
library(psych)

#Read File 
file = data.frame(read.csv('/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/USvideos.csv'))

#Convert Date to R Date Formate
file$trending_date <- as.Date(file$trending_date, '%Y.%d.%m') 

#Remove duplicate data
file_unique <- file %>% group_by(title) %>%
  top_n(1,views)

#Count How many days in Trending for each video
days_trending <- file %>% count(title)

#Merage
file_unique <- merge(x=file_unique, y=days_trending, by='title',all.x=TRUE )

#Clean useless columns
file_unique <-subset(file_unique , select = -c(video_id,channel_title,publish_time,thumbnail_link,comments_disabled,
                                           ratings_disabled,video_error_or_removed,Reference,description))
#Write Out Cleaned Data file
write.csv(file_unique,'/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/videos_unique.csv',fileEncoding = "UTF-8")

#Read Update.csv file
file_updated = data.frame(read.csv('/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/Updated.csv'))
tag_score = data.frame(read.csv('/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/tag_score.csv'))
tag_tf = data.frame(read.csv('/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/tag_tf.csv'))
tag_mean_tfidf_score = data.frame(read.csv('/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/tag_mean_idf.csv'))
tag_mean_tf = data.frame(read.csv('/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/tag_mean_tf.csv'))

#Check distribution
par(mfrow=c(3,3))
#Boxplot

file_updated$views_k = file_updated$views/1000
file_updated <- merge(file_updated,tag_score,'title')
file_updated <- merge(file_updated,tag_tf,'title')
file_updated <- merge(file_updated,tag_mean_tfidf_score,'title')
file_updated <- merge(file_updated,tag_mean_tf,'title')

file_updated$mean_score[is.na(file_updated$mean_score)] <- 0

plot(density(file_updated$views_k))
plot(density(file_updated$likes))
plot(density(file_updated$dislikes))
plot(density(file_updated$comment_count))
plot(density(file_updated$n))
plot(density(file_updated$tag_length))
plot(density(file_updated$mean_score))
plot(density(file_updated$mean_tf))

#Create Dummy variable for category
file_updated <- file_updated %>% 
  mutate(category = case_when(category_id == 'Autos & Vehicles' ~ 1,
                              category_id == 'Comedy' ~ 2,
                              category_id == 'Education' ~ 3,
                              category_id == 'Entertainment' ~ 4,
                              category_id == 'Film & Animation' ~ 5,
                              category_id == 'Gaming' ~ 6,
                              category_id == 'Howto & Style' ~ 7,
                              category_id == 'Music' ~ 8,
                              category_id == 'News & Politics' ~ 9,
                              category_id == 'Nonprofits & Activism' ~ 10,
                              category_id == 'Pets & Animals' ~ 11,
                              category_id == 'Science & Technology' ~ 12,
                              category_id == 'Sports' ~ 13,
                              category_id == 'Travel & Events' ~ 14,))
#Converting Date
file_without_outliers$trending_date <- as.Date(file_without_outliers$trending_date, '%Y-%m-%d') 

#Creating Trending Group Column
file_updated <- file_updated %>% 
  mutate(Trending = case_when(trending_date %like% "0017-11"  ~ 1,
                              trending_date %like% "0017-12"  ~ 2,
                              trending_date %like% "0018-01"  ~ 3,
                              trending_date %like% "0018-02"  ~ 4,
                              trending_date %like% "0018-03"  ~ 5,
                              trending_date %like% "0018-04"  ~ 6,
                              trending_date %like% "0018-05"  ~ 7,
                              trending_date %like% "0018-06"  ~ 8,))

# Remove Duplicate Rows
file_updated <- file_updated %>% distinct(title, trending_date,.keep_all = TRUE)

#Linear Regression
lm1 <- lm(views_k ~ log(likes+1) + log(dislikes+1) + log(comment_count+1) + log(n) +
             + log(mean_tf+1) + log(mean_score+1)+ as.factor(category) + as.factor(Trending), 
            data = file_updated)
summary(lm1)

glm3 <- glm(views_k ~ (likes+1) + (dislikes+1) + (comment_count+1) + n 
            + tag_length + (mean_score+1),  
            family="poisson",data = file_updated)
summary(glm3)

# Double tag_length
file_without_outliers$tag_length2 <- file_without_outliers$tag_length^2

# Tiple tag_length
file_without_outliers$tag_length3 <- file_without_outliers$tag_length^3

#Plot Tag_length and Views
plot(file_without_outliers$tag_length,file_without_outliers$views_k, pch=16, xlab = "tag_length", ylab = "views",col = "blue")
lines(lowess(file_without_outliers$tag_length,file_without_outliers$views_k), col=29)

plot(file_without_outliers$tag_length2,file_without_outliers$views_k, pch=16, xlab = "tag_length2", ylab = "views", col = "blue")
lines(lowess(file_without_outliers$tag_length2,file_without_outliers$views_k), col=29)

plot(file_without_outliers$tag_length3,file_without_outliers$views_k, pch=16, xlab = "tag_length3", ylab = "views", col = "blue")
lines(lowess(file_without_outliers$tag_length3,file_without_outliers$views_k), col=29)


#Double comment_count
file_without_outliers$comment_count2 <- file_without_outliers$comment_count^2

plot(file_without_outliers$comment_count2,file_without_outliers$views_k, pch=16, xlab = "comment_count2", ylab = "views",col = "blue")
abline(lm(views_k ~ comment_count2,data=file_without_outliers), col="red")
lines(lowess(file_without_outliers$comment_count2,file_without_outliers$views_k), col=59)



#Remove duplicates based on title & trending_date

boxplot(file$views)$out

# merge
video_tag_length <- file_updated[c('title','tag_length')]
data_without_outliers <- merge(data_without_outliers,video_tag_length,'title')

data_without_outliers <- data_without_outliers %>% distinct(title, trending_date,.keep_all = TRUE)
data_without_outliers$views_k = data_without_outliers$views/1000

boxplot(data_without_outliers$views_k)
plot(density(data_without_outliers$views_k))


# Panel Data Regression
library(plm)
file$views_k = file$views/1000
file <- file %>% distinct(title, trending_date,.keep_all = TRUE)

plm_within <- plm(views_k ~ log(likes+1) + log(dislikes+1) + log(comment_count+1),
                  index=c('title', "trending_date"),effect="twoways",
                   data = file, model = "within")
summary(plm_within)


plm_pooling <- plm(views_k ~ log(likes+1) + log(dislikes+1) + log(comment_count+1) + 
                     log(tag_length+1),index=c('title', "trending_date"),effect="twoways",
         data = data_without_outliers, model = "pooling")
summary(plm_pooling)

#Entretainment Panel Data
entretainment_panel <- subset(data_without_outliers,category_id == "Entertainment")

plm_within_entretatinment <- plm(views_k ~ log(likes+1) + log(dislikes+1) + log(comment_count+1) + 
                    log(tag_length+1),index=c('title', "trending_date"),
                  data = entretainment_panel, model = "within")
summary(plm_within_entretatinment)

#Music Panel Data
music_panel <- subset(data_without_outliers,category_id == "Music")

plm_within_music <- plm(views_k ~ log(likes+1) + log(dislikes+1) + log(comment_count+1) + 
                                   log(tag_length+1),index=c('title', "trending_date"),
                                 data = music_panel, model = "within")
summary(plm_within_music)

# News and Politics
news_panel <- subset(data_without_outliers,category_id == "News & Politics")
plm_within_news <- plm(views_k ~ log(likes+1) + log(dislikes+1) + log(comment_count+1) + 
                          log(tag_length+1),index=c('title', "trending_date"),
                        data = news_panel, model = "within")
summary(plm_within_news)


plot(data_without_outliers$tag_length,data_without_outliers$views_k, pch=16, xlab = "tag_length", ylab = "views",col = "blue")
abline(lm(views_k ~ tag_length,data=data_without_outliers), col="red")
lines(lowess(data_without_outliers$tag_length,data_without_outliers$views_k), col=59)

# Double tag_length in Panel Data
data_without_outliers$tag_length2 <- data_without_outliers$tag_length^2

plot(data_without_outliers$tag_length2,data_without_outliers$views_k, pch=16, xlab = "comment_count2", ylab = "views",col = "blue")
abline(lm(views_k ~ tag_length2,data=data_without_outliers), col="red")
lines(lowess(data_without_outliers$tag_length2,data_without_outliers$views_k), col=59)

#Write Out
write.csv(file,'/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/videos_panel.csv',fileEncoding = "UTF-8")
write.csv(file_new_updated,'/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/videos_data.csv',fileEncoding = "UTF-8")

keeps <- c("trending_date", "title","category_id","tags","views","likes","dislikes","comment_count","description")
file_new <- file[keeps]
drop <- c()
file_new_updated <- subset(file_new_updated, select = -c(X.x,X.y))
file_new_updated <- file_new_updated %>% distinct(title, trending_date,.keep_all = TRUE)

install.packages("stargazer")
library(stargazer)

stargazer(plm_within, lm1,type = 'text', title="Results", out = "/Users/mark/Desktop/Business Analysts/BANA 277 Cus & Social/youtube-new/models.txt")



