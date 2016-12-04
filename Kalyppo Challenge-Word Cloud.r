# makafui fie
# Word Cloud on Facebook Kalyppo Challenge
#
# load in the text mining package
library(tm)
#
# connect to the directoryse
setwd("/Users/m/Google Drive/school/Data mining/KalyppoChallenge")

#

#read data from csv file

kalyppo <- read.csv('#KalyppoChallenge-Facebook.csv', header=TRUE)
#get column with data
#text = kalyppo$Title

# prepare to say we are in Accra for time zone purposes
Sys.setenv(TZ="GMT")

#remove non-ASCII characters
#dat2 <- grep("text", iconv(text, "latin1", "ASCII", sub="text"))
#dat3 <- text[-dat2]

#create a corpus
kalyppoCorpus <- Corpus(VectorSource(kalyppo$Title))

#function to remove specified characters
replace = content_transformer(function(x, pattern)
  gsub(pattern, "", x))
#remove non-ASCII characters
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "ðŸ˜")

kalyppoCorpus = tm_map(kalyppoCorpus, replace, "â˜")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "â€“")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "‚‚‚")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "‚‚‚‚")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "âˆš")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "âœ”ðÿ’")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "‚‚‚‚‚‚‚")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "‚‚‚‚‚‚")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "ðÿ")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "â€™")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "â€â€ž")
kalyppoCorpus = tm_map(kalyppoCorpus, replace, "ðÿ‘")



#kalyppoCorpus <- tm_map(kalyppoCorpus, removeWords, c('the', 'this', stopwords('english')))
#
# create the TDM
KalyppoChallenge.tdm <- TermDocumentMatrix(kalyppoCorpus, control = list(
   removePunctuation = TRUE,
   stopwords = TRUE,
   tolower = TRUE,
   stemming = FALSE,
   removeNumbers = TRUE,
   bounds = list(global = c(1, Inf))))
#
# preview the top ten terms across the facebook posts
inspect(KalyppoChallenge.tdm[1:10,])
#
# find the top ten most frequent words across the facebook posts
findFreqTerms(KalyppoChallenge.tdm, lowfreq=100, highfreq=Inf)
#
# load wordcloud package to draw wordcloud
library(wordcloud)
#
# put together wordcloud of whole corpus


wordcloud(kalyppoCorpus, max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))

kalyppoMatrix <- as.matrix(KalyppoChallenge.tdm)
kalyppoVector <- sort(rowSums(kalyppoMatrix),decreasing=TRUE)
dataFrame <- data.frame(word = names(kalyppoVector),frequency=kalyppoVector)
head(dataFrame, 10)
barplot(dataFrame[1:10,]$frequency, las = 2, names.arg = dataFrame[1:10,]$word,
col ="grey", main ="Top Ten Words",
ylab = "Frequencies")

