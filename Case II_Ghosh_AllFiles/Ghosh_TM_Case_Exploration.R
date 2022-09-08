# Libs
library(tm)
library(ggplot2)
library(stringr)
library(textclean)
library(wordcloud)
library(tidytext)
library(stringi)
library(ggthemes)
library(dendextend)
library(RColorBrewer)
library(pbapply)
library(radarchart)
library(textdata)
library(tidytext)
library(dplyr)
library(qdap)

# Set the working directory
setwd("/cloud/project/Cases/II Gov Contractor")

# Options & Functions - SETTING FUNCTIONS SO NO CHANGE IS NEEDED 
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Custom cleaning function
cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

#Training Data
trainTweets<-read.csv("student_tm_case_training_data.csv", header=TRUE, encoding = "UTF-8")
view(trainTweets)
politicalTweets <- trainTweets$label[trainTweets$label==1]
view(politicalTweets)
dim(trainTweets)
mean(trainTweets$label == 1)
mean(trainTweets$label == 0)

# Create custom stop words
customStopwords <- c(stopwords('english'), 'rt', 'https', 'amp', '#', 'lol', '...', '@')


#REMOVING CHARACTERS FROM TWEETS 
trainTweets<-str_replace_all("^\\s*<U||+w+>",trainTweets$rawText)

#EXPLORATION 
trainTweets <- read.csv("student_tm_case_training_data.csv")
names(trainTweets.model[2])<-"text"
view(trainTweets.model)
head(trainTweets.model)
tail(trainTweets.model)

#FREQUENCY OF WORDS
trainTweets.model<-scan("student_tm_case_training_data.csv","character",sep="\n");

#Replace full stop and comma
trainTweets.model<-gsub("\\.","",trainTweets.model)
trainTweets.model<-gsub("\\,","",trainTweets.model)

#Split sentence
words<-strsplit(trainTweets.model," ")

#Calculate word frequencies
words.freq<-table(words);
words.freq<-table(unlist(words));
cbind.data.frame(names(words.freq),as.integer(words.freq))

#SEGREGATING JUST POLITICAL TWEETS 
politicalTweets<-trainTweets.model[trainTweets.model]
view(politicalTweets)
count(politicalTweets$label==1)

#COUNTING THE NO. 
str_count(c("#", "black lives matter", "...", "http","cnn", "cnn", "@"), fixed("."))
trainTweets$numHashtags<-str_count(trainTweets$rawText, "#")


#SUMMARY 
dim(politicalTweets)
summary(politicalTweets$label==1)
library(ggplot2)
ggplot(politicalTweets, aes(x=V1, y=V2)) + geom_bar(stat="identity") + 
  labs(x="Percentage", y="Proportion")

#GGPLOT FOR HASHTAGS
ggplot(data=trainTweets, aes(x=as.factor(label), y=numHashtags))+geom_boxplot()
          +ggtitle("Comparing hashtags") + xlab("Label where 1 = social discourse") 
          + ylab("hashtag#count")



# As of tm version 0.7-3 tabular was deprecated - #first 2 columns must be 'doc_id' & 'text'
names(trainTweets.model)[1] <- 'doc_id' 
names(trainTweets.model)[2] <- 'text'
view(trainTweets.model)

# Make a volatile corpus
txtCorpus <- VCorpus(DataframeSource(trainTweets.model))


# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

#TDM - MAKE TERM DOCUMENT MATRIX 
txtTdm  <- TermDocumentMatrix(txtCorpus)
txtTdmm <- as.matrix(txtTdm)


# Frequency Data Frame
trainingFreq <- rowSums(txtTdmm)
trainingFreq <- data.frame(word=names(trainingFreq),
                       frequency=trainingFreq, 
                       row.names = NULL)

topWords <- subset(trainingFreq, trainingFreq$freq>=100)
order(topWords$frequencey) 


#WORDCLOUD 
wordcloud(txtCorpus
          , scale=c(2,0.5)     # Set min and max scale
          , max.words=250      # Set top n words
          , random.order=FALSE # Words in decreasing freq
          , rot.per=0.35       # % of vertical words
          , use.r.layout=FALSE # Use C++ collision detection
          , colors=brewer.pal(8, "Dark2"))

# Check Meta Data; 
txtCorpus[[4]]
meta(txtCorpus[[4]])
meta(txtCorpus[4])
content(txtCorpus[4])
content(txtCorpus[[4]])

# Plain text cleaned copy?
df <- data.frame(text = sapply(txtCorpus, content),
                 stringsAsFactors=F)

#WRITING TO A FILE
write.csv(df,'clean.csv',row.names = F)

# End
