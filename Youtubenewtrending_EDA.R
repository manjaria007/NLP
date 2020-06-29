#https://www.kaggle.com/datasnaek/youtube-new


set.seed(1234)

#Data manipulation
library(data.table)
library(dplyr)
library(DT)

#Time Manipulation
library(lubridate)

#Visualizations
library(ggplot2)
library(plotrix)
library(corrplot)
library(ggdendro)
library(ggrepel)

#wordcloud
library(wordcloud)
library(RSentiment)

install.packages("RSentiment")
library(RColorBrewer)
library(tidytext)
library(textdata)

#Sentiment analysis Library
library(sentimentr)
library(SentimentAnalysis)
library(openNLP)
library(SnowballC)
library(tm)


#Loading Dataset

ru <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/RUvideos.csv")
mx <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/MXvideos.csv")
kr <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/KRvideos.csv")
jp <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/JPvideos.csv")
inn <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/INvideos.csv")
gb <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/GBvideos.csv")
fr <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/FRvideos.csv")
de <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/DEvideos.csv")
ca <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/CAvideos.csv")
us <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/CAvideos.csv")
#To view each file
head(ru)
head(mx)
head(kr)
head(jp)
head(inn)
head(gb)
head(fr)
head(de)
head(ca)


#Combined all together
all_videos <- as.data.table(rbind(ru, mx, kr, jp, inn, gb, fr, de, ca, us))

head(all_videos)
summary(all_videos)

library(lubridate)
#From lubridate package we will use ymd function that will tranform date stored in character and numeric vectors to POSIXct objects
all_videos$trending_date <- ydm(all_videos$trending_date)

#Will change publish time as well
all_videos$publish_time <- ymd(substr(all_videos$publish_time,start = 1, stop = 10))

#we will find the difference in days from trending data and publish time
all_videos$dif_days <- all_videos$trending_date - all_videos$publish_time

#lets check the correlation b/w data

corrplot.mixed(corr = cor(all_videos[,c("category_id","views","likes","dislikes","comment_count"),with=F]))

#likes and comment_count have a high correlation


##Most Viewed videos

mostviewed <- all_videos[,.("Total_views"=round(max(views, na.rm=T), digits = 2)), by=.(title,thumbnail_link)][order(-Total_views)]

mostviewed %>%
  mutate(image=paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>%
  arrange(-Total_views) %>%
  top_n(10, wt=Total_views) %>%
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't', scrollX=TRUE, autoWidth = TRUE))

##Most liked videos

mostviewed <- all_videos[,.("Total_likes"=round(max(likes, na.rm=T), digits = 2)), by=.(title,thumbnail_link)][order(-Total_likes)]

mostviewed %>%
  mutate(image=paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>%
  arrange(-Total_likes) %>%
  top_n(10, wt=Total_likes) %>%
  select(image, title, Total_likes) %>%
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't', scrollX=TRUE, autoWidth = TRUE))

##Most Disliked Videos

mostviewed <- all_videos[,.("Total_Dislikes"=round(max(dislikes, na.rm=T), digits = 2)), by=.(title,thumbnail_link)][order(-Total_Dislikes)]

mostviewed %>%
  mutate(image=paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>%
  arrange(-Total_Dislikes) %>%
  top_n(10, wt=Total_Dislikes) %>%
  select(image, title, Total_Dislikes) %>%
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't', scrollX=TRUE, autoWidth = TRUE))


##Top 10 liked videos in percentage
mostviewed <- all_videos[,.("Percentage_likes"=round(100*max(likes, na.rm=T)/max(views, na.rm=T), digits = 2)), by=.(title,thumbnail_link)][order(-Percentage_likes)]

mostviewed %>%
  mutate(image=paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>%
  arrange(-Percentage_likes) %>%
  top_n(10, wt=Percentage_likes) %>%
  select(image, title, Percentage_likes) %>%
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't', scrollX=TRUE, autoWidth = TRUE))

##Top 10 Disliked videos in percentage
mostviewed <- all_videos[,.("Percentage_dislikes"=round(100*max(dislikes, na.rm=T)/max(views, na.rm=T), digits = 2)), by=.(title,thumbnail_link)][order(-Percentage_dislikes)]

mostviewed %>%
  mutate(image=paste0('<img width="80%" height="80%" src="', thumbnail_link , '"></img>')) %>%
  arrange(-Percentage_dislikes) %>%
  top_n(10, wt=Percentage_dislikes) %>%
  select(image, title, Percentage_dislikes) %>%
  datatable(class = "nowrap hover row-border", escape = FALSE, options = list(dom = 't', scrollX=TRUE, autoWidth = TRUE))


##Top trending channels

ggplot(all_videos[,.N,by=channel_title][order(-N)][1:10],aes(reorder(channel_title,-N),N,fill=channel_title))+
  geom_bar(stat="identity")+geom_label(aes(label=N))+
  guides(fill="none")+theme(axis.text.x = element_text(angle=45,hjust=1))+ 
  labs(caption="Mohit",title="Trending_channels_in_all_countries")+
  xlab(NULL)+ylab(NULL)+coord_flip()

##Title Bigrams/Language Model
##Bigrams makes a prediction for a word based on the one before
#Two word sequence of words like "please turn", "your homework".
#A token is a meaningful unit of text, most often a word, that we are interested in 
#using for further analysis, and tokenization is the process of splitting texts into tokens



#TITLE BIGRAMS

bigra <- unnest_tokens(all_videos, bigram, title, token = "ngrams", n=2)

bigra <- as.data.table(bigra)
library(data.table)
library(ggplot2)

ggplot(bigra[,.N,by=bigram][order(-N)][1:19],aes(reorder(bigram, -N),N,fill=bigram))+
  geom_bar(stat = "identity")+geom_label(aes(label=N))+guides(fill="none")+
  theme(axis.text.x=element_text(angle = 45,hjust=1))+
  labs(caption = "Mohit",title="Top Title bigrams")+xlab(NULL)+ylab(NULL)


top_bigram <- bigra %>%
  select(bigram) %>%
  group_by(bigram) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(20)

top_bigram_bp <- ggplot(top_bigram, aes(x = reorder(bigra, n),
                                        y= n,
                                        fill = bigra))+
  geom_bar(stat = "identity")+
  labs(caption = "Mohit",
       title = "Top Title Bigrams")+
  guides(fill="none")+
  xlab(NULL)+
  ylab(NULL)+
  geom_label(aes(label = n))+
  coord_flip()

top_bigram_bp







#Sentimentr Analysis


??VectorSource
#Vectorsource takes a group of texts and makes each element of the resulting vector a document within your R
text <- array(sample(unique(us$description),1000))


txt_corpus <- Corpus(VectorSource(text))

inspect(txt_corpus)

#Replacing "/","\", "@" and "|" with space:
toSpace <- content_transformer(function(x, pattern)gsub(pattern, " ",x))
Docks <- tm_map(txt_corpus, toSpace, "/")
Docks <- tm_map(txt_corpus, toSpace, "@")
Docks <- tm_map(txt_corpus, toSpace, ",")
Docks <- tm_map(txt_corpus, toSpace, "ë")
Docks <- tm_map(txt_corpus, toSpace, "ìo")
Docks <- tm_map(txt_corpus, toSpace, "#")
Docks <- tm_map(txt_corpus, toSpace, "|")
Docks <- tm_map(txt_corpus, toSpace, ":")
Docks <- tm_map(txt_corpus, toSpace, "^")
Docks <- tm_map(txt_corpus, toSpace, "???T")
Docks <- tm_map(txt_corpus, toSpace, "???")
Docks <- tm_map(txt_corpus, toSpace, "T")
Docks <- tm_map(txt_corpus, toSpace, "T")
Docks <- tm_map(txt_corpus, toSpace, "???")
Docks <- tm_map(txt_corpus, toSpace, "xf0")
Docks <- tm_map(txt_corpus, content_transformer(tolower))


inspect(Docks)
#Remove Whitespaces
Docks <- tm_map(Docks, stripWhitespace)


#Remove Punctuations
Docks <- tm_map(Docks, removePunctuation)
  
#to remove numbers
Docks <- tm_map(Docks, removeNumbers)

#to remove stopwords(like 'as', 'the etc..)
Docks <- tm_map(Docks, removeWords, stopwords("english"))


#Remove Urls
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
Docks <- tm_map(Docks, removeURL)


#to create stem documents (Do not run while performing wordcloud)
#Docks <- tm_map(Docks, stemDocument)
#Bags <- tm_map(Bags, stemDocument)


install.packages("textmineR")
#creating term documentation Matrix
#It an table contains the frequency of words.
Matrix <- DocumentTermMatrix(Docks)

#Transpose the matrix
Matrixc <- TermDocumentMatrix(Docks)


#remove sparse terms(not running properly)
MATRIXX <- removeSparseTerms(Matrixc, sparse = 0.2)
matrix_m2 <- removeSparseTerms(Matrix, sparse=0.95)


#Organize terms by Frequency
freq <- colSums(as.matrix(Matrix))
length(freq)

ord <- order(freq) 


#Converting into Matrix
matrix_m <- as.matrix(Matrixc)
dim(matrix_m)


#Frequency data
omr <- sort(rowSums(matrix_m), decreasing = TRUE)
head(omr, 10)
md <- data.frame(word=names(omr), freq=omr)
head(md, 10)

#Clustering Terms
Scale_matrix_m <- scale(matrix_m)
distmatrix <- dist(Scale_matrix_m)

set.seed(1234)
#Creating WordCloud
word_Cloud <- wordcloud(words = md$word,freq = md$freq, max.words = 500, min.freq = 1, scale = c(7,.5), random.order = FALSE, colors = brewer.pal(8, "Dark2"))


#Exploring frequent terms and their associations
findFreqTerms(Matrix, lowfreq = 100)

findAssocs(Matrix, terms = "facebook", corlimit = 0.3)

#Plot frequencies of the first 10 frequent words:
barplot(md[1:10,]$freq, las=2, names.arg = md[1:10,]$word,
        col = "lightblue", main="Most frequent words",
        ylab="word frequencies")


#LIkes VS VIEWS: (NEed to clean the messup)
ggplot(all_videos[,.("views"=max(views),"likes"=max(likes)),by=title],aes(views,likes,colour=likes,size=likes))+
  geom_jitter()+geom_smooth()+guides(fill="none")+
  labs(caption="AMP",title="views Vs likes", subtitle = "In days")+
  theme(legend.position = "none")+
  geom_text_repel(data = subset(all_videos[,.("views"=max(views),"likes"=max(likes)),by=title],views > 5e+07),
                  
                  aes(views, likes, label=title,check_overlap=T))


##Sentiment Analysis description Field (sample)


get_sentiments("bing")
get_sentiments("bing") %>% filter(sentiment=="positive")
get_sentiments("bing") %>% filter(sentiment=="negative")

#A score greater than 0 would indicate positive sentiment and less then 0 would mean negative overall emotion
get_sentiments("afinn") %>% filter(value=="3")
get_sentiments("afinn") %>% filter(value=="-3")

sentiments2


sentiments2 <- md %>%
  inner_join(get_sentiments("bing"))

sentiments2 <- md %>%
  inner_join(get_sentiments("afinn"))



sentii <- analyzeSentiment(sentiments2$sentiment)

word <- sentiments2$word
freqq <- sentiments2$freq
senti <- sentiments2$sentiment

summary(sentiments2)

ggplot(sentiments2, aes(x=senti, y=freqq, fill=senti))+
  geom_bar(stat = "identity")+
  ggtitle("Description Sentiments")+xlab("sentiment")+ylab("% Sentiment")
  
#Count of positive and negative
table(sentiments2$sentiment)



#Replacing values postive to 1 and negative to 0
require(dplyr)
library(plyr)
sentiments2$sentiment <- revalue(sentiments2$sentiment, c("positive"=1))
sentiments2$sentiment <- revalue(sentiments2$sentiment, c("negative"=0))
head(sentiments2)
table(sentiments2$sentiment)
summary(sentiments2)

###Creating training and test datasets
set.seed(1234)
train_indexx <- createDataPartition(sentiments2, p=0.7)$Resample1
train<-sentiments2[train_indexx, ]
test<-sentiments2[-train_indexx, ]

print(table(train))

names(sentiments2)


#Learning to classify sentiments with naive bayes classifier
library(naivebayes)
library(caret)
library(e1071)
library(gmodels)
install.packages("RTextTools")
library(RTextTools)

#We no longer need the frequency, drop the feature
sentiments2$freq=NULL

#Fitting the Naive Bayes model
Naive_ba <- naive_bayes(sentiment~.,data = train)
Naive_ba

#predictions on dataset
Naive_pred <- predict(test, sentiments2)

#Confusion matrix to check accuracy
table(test, sentiments2$sentiment)

conf.mat <- confusionMatrix(test, sentiments2)
conf.mat


#Naive bayes on whole description to understand what leads video to be trending
us <- read.csv("C:/Users/mohit/OneDrive/Desktop/Northeastern/ALY6040 Data mining/Youtube_trend_video/CAvideos.csv")
glimpse(us)

set.seed(1)
us <- us[sample(nrow(us)), ]
us <- us[sample(nrow(us)), ]
glimpse(us)

is.na(us)
na.omit(us)

corpo <- Corpus(VectorSource(us$description))
inspect(corpo[1:3])

corpo.clean <- corpo %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind = "en")) %>%
  tm_map(stripWhitespace)


mmm <- DocumentTermMatrix(corpo.clean)
inspect(mmm[40:50, 10:15])


af.train <- us[1:1500,]
af.test <- us[1501:2000,]

bf.train <- mmm[1:1500, ]
bf.test <- mmm[1501:2000,]

corpo.clean.train <- corpo.clean[1:1500]
corpo.clean.test <- corpo.clean[1501:2000]

dim(bf.train)

fivefreq <- findFreqTerms(bf.train, 5)
fivefre <- findFreqTerms(bf.test, 5)
length(fivefre)
length((fivefreq))

bf.train.nb <- DocumentTermMatrix(corpo.clean.train, control = list(dictionary=fivefreq))
dim(bf.train.nb)

bf.test.nb <- DocumentTermMatrix(corpo.clean.test, control = list(dictionary=fivefreq))
dim(bf.test.nb)

#Converting word frequencies to yes(Presence) and no(absence) labels
convert_count <- function(x){
  y <- ifelse(x > 0,1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

#Apply the convert_count function to get final training and testing 
trainNB <- apply(bf.train.nb, 2,convert_count)
testNB <- apply(bf.test.nb, 2, convert_count)

str(trainNB)
str(testNB)
head(trainNB)
head(testNB)

#Training the Naive Bayes Model
#Train the Classifier
system.time(classifier <- naive_bayes(trainNB, af.train$description, laplace = 1))

#testing the predictions
system.time(predo <- predict(classifier, data = newdata))

#Create a truth table by tabulating predicted class labels with the actual class labels
table("predictions"= predo, "actual"= af.test$description)

#Confusion Matrix

conf.mat <- confusionMatrix(predo, af.test$description)
conf.mat

#Reference for particle swarm optimizer algorithm
#https://www.semanticscholar.org/paper/Optimized-Na%C3%AFve-Bayesian-Algorithm-for-Efficient-Georgina-Isah/3d436338300e212096883c945a7549231c0fb5d8?p2df
