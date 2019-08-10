library(stringi)
library(tm)
library(slam)
library(tokenizers)

# This script is to download the Coursera-SwiftKey.zip
# file from the given URL and extract the contents
#

# set source file URL and the default name of the file downloaded from the source url
sourceFileURL = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
downloadedFile = paste0("./data/" , "Coursera-SwiftKey.zip")
downloadFolder = paste0("./data/" , "Coursera-SwiftKey")

# check if the downloaded file exist. Download if it does not...
if (!file.exists(downloadedFile)) {
  result <-
    tryCatch(
      download.file(sourceFileURL, downloadedFile, method = "auto"),
      error = function(e)
        1
    )
  
  if (result != 1) {
    unzip(downloadedFile, exdir = downloadFolder)
  }
}

# after extracting from archive, get all file names from the extracted folder
files <-
  list.files(downloadFolder, recursive = TRUE, full.names = TRUE)

en_files <-
  c(
    "./data/Coursera-SwiftKey/final/en_US/en_US.blogs.txt",
    "./data/Coursera-SwiftKey/final/en_US/en_US.news.txt",
    "./data/Coursera-SwiftKey/final/en_US/en_US.twitter.txt"
  )

##
## function sampleDataFile takes a filename and a sample data percent
## as input and creates a new file sample_<filename> that contains
## a sample from the main file. This is done to speed up the analysis

set.seed(238)
sampleDataFile <- function (filename, SAMPLE_DATA_PERCENT) {
  fileHandle <- file (filename, open = "r")
  contents <-
    readLines(fileHandle, encoding = "UTF-8", skipNul = TRUE)
  sampledData <-
    contents[sample(length(contents), SAMPLE_DATA_PERCENT * length(contents))]
  sampledDataFileName <-
    paste0(dirname(filename), paste0("//", paste0("sample_", basename(filename))))
  write(
    sampledData,
    file = sampledDataFileName,
    ncolumns = 1,
    append = FALSE,
    sep = "\t"
  )
  close (fileHandle)
  return (sampledDataFileName)
}


# define the sampling percent. Only this much data will be used from the files
SAMPLE_DATA_PERCENT <- 0.01
#create sample files from the main files, based on the sampling percent
samples <- sapply (en_files, sampleDataFile, SAMPLE_DATA_PERCENT)

sampleData <- sapply(samples, read.table, sep = "\t", quote = "")
sampleDataNames <- c("news", "blogs", "twitter")
sampleData <- setNames(sampleData, sampleDataNames)
sampleData <- sapply(sampleData, as.character)


# extract sentences from the news blogs and twitter data
newsData <-
  unlist(stri_split(str = sampleData$news, regex = "(?<=[?!.])\\s"))
blogsData <-
  unlist(stri_split(str = sampleData$blogs, regex = "(?<=[?!.])\\s"))
twitterData <-
  unlist(stri_split(str = sampleData$twitter, regex = "(?<=[?!.])\\s"))

removeProfanityWords <- function(sentences) {
  return(sentences[!grepl(profanityWords, sentences)])
}

removeRT <- function(textCorpus) {
  return(gsub("RT", "", textCorpus))
}
cleanCorpus <- function(myCorpus) {
  myCorpus <- tm_map(myCorpus, removeNumbers)
  myCorpus <- tm_map(myCorpus, removeRT)
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("en"))
  myCorpus <- tm_map(myCorpus, removePunctuation)
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  myCorpus <- tm_map(myCorpus, tolower)
  return(myCorpus)
}

#download a list of profanity words. These has to be removed from our data before
# further cleaning and analysis

profanityWords = "profanity.csv"
profanityFileURL = "https://gist.githubusercontent.com/tjrobinson/2366772/raw/97329ead3d5ab06160c3c7ac1d3bcefa4f66b164/profanity.csv"
if (!file.exists(profanityWords)) {
  result <-
    tryCatch(
      download.file(profanityFileURL, profanityWords, method = "auto"),
      error = function(e)
        1
    )
  
}

#read profanity words csv file
profanityWordsDF <- read.csv(profanityWords, sep = "\n")
colnames(profanityWordsDF) <- "words"
profanityWordsVector <- as.vector(profanityWordsDF$words)
profanityWords <- paste(profanityWordsVector, collapse = "|")

filteredNewsData <- removeProfanityWords(newsData)
filteredBlogsData <- removeProfanityWords(blogsData)
filteredTwitterData <- removeProfanityWords(twitterData)

combinedData <-
  c(filteredNewsData, filteredBlogsData, filteredTwitterData)
textCorpus <- Corpus(VectorSource(combinedData))

textCorpus <- cleanCorpus(textCorpus)


# create bigram, trigram  & quadgram for data 
textCorpusDF <- unlist(data.frame(text= sapply(textCorpus, as.character), stringsAsFactors = FALSE))
textCorpusBiGram <-vector(mode="character")
textCorpusTriGram <-vector(mode="character")
textCorpusQuadGram <-vector(mode="character")

for (i in 1:length(textCorpusDF)){
  biGramTokens <- tokenize_ngrams(textCorpusDF[i], n = 2, simplify= FALSE )
  textCorpusBiGram = c(textCorpusBiGram, biGramTokens[1]) 
  
  triGramTokens <- tokenize_ngrams(textCorpusDF[i], n = 3, simplify= FALSE )
  textCorpusTriGram = c(textCorpusTriGram, triGramTokens[1]) 
  
  quadGramTokens <- tokenize_ngrams(textCorpusDF[i], n = 4, simplify= FALSE )
  textCorpusQuadGram = c(textCorpusQuadGram, quadGramTokens[1]) 
  
}

textCorpusQuadGramDF <- data.frame(table(unname(unlist(textCorpusQuadGram))))
#textCorpusQuadGramDF$percentage <- (textCorpusQuadGramDF$Length/sum(textCorpusQuadGramDF$Length))
textCorpusQuadGramDF <- textCorpusQuadGramDF[order(-textCorpusQuadGramDF$Freq),]
colnames(textCorpusQuadGramDF) <- c("words","frequency")#,"percentage")

textCorpusTriGramDF <- data.frame(table(unname(unlist(textCorpusTriGram))))
#textCorpusQuadGramDF$percentage <- (textCorpusQuadGramDF$Length/sum(textCorpusQuadGramDF$Length))
textCorpusTriGramDF <- textCorpusTriGram[order(-textCorpusTriGram$Freq),]
colnames(textCorpusTriGramDF) <- c("words","frequency")#,"percentage")


textCorpusBiGramDF <- data.frame(table(unname(unlist(textCorpusBiGram))))
#textCorpusQuadGramDF$percentage <- (textCorpusQuadGramDF$Length/sum(textCorpusQuadGramDF$Length))
textCorpusBiGramDF <- textCorpusBiGramDF[order(-textCorpusBiGramDF$Freq),]
colnames(textCorpusBiGramDF) <- c("words","frequency")#,"percentage")



bigram_split <- strsplit(as.character(textCorpusBiGramDF$words),split=" ")
textCorpusBiGramDF$first <-  sapply(bigram_split,"[[",1)
textCorpusBiGramDF$second <- sapply(bigram_split,"[[",2)


write.csv(textCorpusBiGramDF[textCorpusBiGramDF$freq > 1,],"./data/bigram.csv",row.names=F)
textCorpusBiGramDF <- read.csv("./data/bigram.csv",stringsAsFactors = F)
saveRDS(textCorpusBiGramDF,"./data/bigram.RData")

trigram_split <- strsplit(as.character(textCorpusTriGramDF$words),split=" ")
textCorpusTriGramDF$first <-  sapply(trigram_split,"[[",1)
textCorpusTriGramDF$second <- sapply(trigram_split,"[[",2)
textCorpusTriGramDF$third <- sapply(trigram_split,"[[",3)

write.csv(textCorpusTriGramDF[textCorpusTriGramDF$freq > 1,],"./data/trigram.csv",row.names=F)
textCorpusTriGramDF <- read.csv("./data/trigram.csv",stringsAsFactors = F)
saveRDS(textCorpusTriGramDF,"./data/trigram.RData")

quadgram_split <- strsplit(as.character(textCorpusQuadGramDF$words),split=" ")
textCorpusQuadGramDF$first <-  sapply(quadgram_split,"[[",1)
textCorpusQuadGramDF$second <- sapply(quadgram_split,"[[",2)
textCorpusQuadGramDF$third <- sapply(quadgram_split,"[[",3)
textCorpusQuadGramDF$fourth <- sapply(quadgram_split,"[[",4)

write.csv(textCorpusQuadGramDF[textCorpusQuadGramDF$freq > 1,],"./data/quadgram.csv",row.names=F)
textCorpusQuadGramDF <- read.csv("./data/quadgram.csv",stringsAsFactors = F)
saveRDS(textCorpusQuadGramDF,"./data/quadgram.RData")
