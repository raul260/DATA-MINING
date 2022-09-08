# Libs
library(text2vec)
library(caret)
library(tm)
library(glmnet)
library(pROC)
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

# Custom cleaning function
diagnosisClean<-function(xVec){
  xVec <- removePunctuation(xVec)
  xVec <- removeNumbers(xVec)
  xVec <- stripWhitespace(xVec)
  xVec <- tolower(xVec)
  return(xVec)
}


#Training Model 
trainTweets.model<-read.csv("student_tm_case_training_data.csv", header=TRUE, encoding = "UTF-8")
trainTweets.model$rawText <- diagnosisClean(trainTweets.model$rawText)

                            
#Testing Model
testTweets.model<-read.csv("student_tm_case_score_data.csv", header=TRUE, encoding = "UTF-8")
testTweets.model$rawText <- diagnosisClean(testTweets.model$rawText)


### EXPLORE
head(trainTweets.model,2)
table(trainTweets.model$label)

#Partition Data 80% train 20% validation 
trainIndex <-sample(rownames(trainTweets.model),dim(trainTweets.model)[1]*0.8)


#Training Set 80%
trainTweets.model.set <- trainTweets.model[trainIndex,]

#Validation Set 20%
validationTweets.model.set<-trainTweets.model[-as.numeric(trainIndex),]

# Create custom stop words
customStopwords <- c(stopwords('english'), 'rt', 'https', 'amp', '#', 'lol', '...')

# Initial iterator to make vocabulary - WILL NEED THIS 
iterMaker <- itoken(trainTweets.model$rawText, 
                    preprocess_function = list(tolower), 
                    progressbar         = T)
textVocab <- create_vocabulary(iterMaker, stopwords=stopwords(c(customStopwords,'SMART')))
head(textVocab)
tail(textVocab)
nrow(textVocab)

#prune vocab to make DTM smaller - WILL NEED THIS 
prunedtextVocab <- prune_vocabulary(textVocab,
                                    term_count_min = 10,
                                    doc_proportion_max = 0.5,
                                    doc_proportion_min = 0.001)
nrow(prunedtextVocab)

# Using the pruned vocabulary to declare the DTM vectors 
vectorizer <- vocab_vectorizer(prunedtextVocab)

# Take the vocabulary lexicon and the pruned text function to make a DTM  
textVocab <- create_dtm(iterMaker, vectorizer)
dim(textVocab)

# Make a Document Term Matrix or Term Document Matrix depending on analysis
txtDtm  <- create_dtm(iterMaker,vectorizer)
txtDtmM <- as.matrix(txtDtm)



### MODEL(s)
#train text only model
textModel <- cv.glmnet(txtDtmM ,
                     y=as.factor(trainTweets.model$label),
                     alpha=0.9,
                     family='binomial',
                     type.measure='auc',
                     nfolds=5,
                     intercept=F)

#ANALYSIS OF MODEL AGAINST VALIDATION SET
iterMaker <- itoken(validationTweets.model.set$rawText, 
                    preprocess_function = list(tolower), 
                    progressbar         = T)
testDtm  <- create_dtm(iterMaker,vectorizer)
testDtmM <- as.matrix(testDtm)
testPreds <- predict(textModel, testDtmM, type = 'class')

trainConfusionMatrix<-confusionMatrix(as.factor(testPreds),
                as.factor(validationTweets.model.set$label))
trainConfusionMatrix


#ANALYSIS OF MODEL AGAINST TRAINING SET
iterMaker <- itoken(trainTweets.model.set$rawText, 
                    preprocess_function = list(tolower), 
                    progressbar         = T)
testDtm  <- create_dtm(iterMaker,vectorizer)
testDtmM <- as.matrix(testDtm)
testPreds <- predict(textModel, testDtmM, type = 'class')

trainConfusionMatrix<-confusionMatrix(as.factor(testPreds),
                                      as.factor(trainTweets.model.set$label))
trainConfusionMatrix

#CREATE DTM FOR TEST 
iterMaker <- itoken(testTweets.model$rawText, 
                    preprocess_function = list(tolower), 
                    progressbar         = T)
testDtm  <- create_dtm(iterMaker,vectorizer)
testDtmM <- as.matrix(testDtm)
testPreds <- predict(textModel, testDtmM, type = 'resp')

testTweets.model$modelpredictions <- testPreds

#WRITE TO A FILE 
write.csv(testTweets.model, 'Ghosh_TM_scores.csv', row.names = T)
