
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)

library(NLP)
library(tm)
library(SnowballC)
library(slam)
library(RWeka)
library(wordcloud)
library(stringr)
library(stringi)
library(ngram)


blogs <- readLines("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/final/en_US/en_US.blogs.txt",skipNul = TRUE, warn = TRUE)
news <- readLines("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/final/en_US/en_US.news.txt",skipNul = TRUE, warn = TRUE)
twitter <-readLines("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/final/en_US/en_US.twitter.txt",skipNul = TRUE, warn = TRUE)


## Generating a random sapmle of all sources
sampleTwitter <- twitter[sample(1:length(twitter),10000)]
sampleNews <- news[sample(1:length(news),10000)]
sampleBlogs <- blogs[sample(1:length(blogs),10000)]
textSample <- c(sampleTwitter,sampleNews,sampleBlogs)
complete_text <- c(twitter,news,blogs)
## Save sample
writeLines(textSample, "C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/textSample.txt")
writeLines(complete_text, "C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/completeSample.txt")


### Checking the size and length of the files and calculate the word count
blogsFile <- file.info("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/final/en_US/en_US.blogs.txt")$size / 1024.0 / 1024.0
newsFile <- file.info("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/final/en_US/en_US.news.txt")$size / 1024.0 / 1024.0
twitterFile <- file.info("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/final/en_US/en_US.twitter.txt")$size / 1024.0 / 1024.0
sampleFile <- file.info("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/textSample.txt")$size / 1024.0 / 1024.0

blogsLength <- length(blogs)
newsLength <- length(news)
twitterLength <- length(twitter)
sampleLength <- length(textSample)

blogsWords <- sum(sapply(gregexpr("\\S+", blogs), length))
newsWords <- sum(sapply(gregexpr("\\S+", news), length))
twitterWords <- sum(sapply(gregexpr("\\S+", twitter), length))
sampleWords <- sum(sapply(gregexpr("\\S+", textSample), length))

fileSummary <- data.frame(
  fileName = c("Blogs","News","Twitter", "Aggregated Sample"),
  fileSize = c(round(blogsFile, digits = 2), 
               round(newsFile,digits = 2), 
               round(twitterFile, digits = 2),
               round(sampleFile, digits = 2)),
  lineCount = c(blogsLength, newsLength, twitterLength, sampleLength),
  wordCount = c(blogsWords, newsWords, twitterWords, sampleLength)                  
)

colnames(fileSummary) <- c("File Name", "File Size in Megabyte", "Line Count", "Word Count")

saveRDS(fileSummary, file = "C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/fileSummary.Rda")

fileSummaryDF <- readRDS("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/fileSummary.Rda")



## Building a clean corpus

theSampleCon <- file("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/textSample.txt")
theSample <- readLines(theSampleCon)
close(theSampleCon)

profanityWords <- read.table("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/profanityfilter.txt", header = FALSE)

## Build the corpus, and specify the source to be character vectors 
cleanSample <- Corpus(VectorSource(theSample))

##
rm(theSample)

## Make it work with the new tm package
cleanSample <- tm_map(cleanSample,
                      content_transformer(function(x) 
                        iconv(x, to="UTF-8", sub="byte")))

## Convert to lower case
cleanSample <- tm_map(cleanSample, content_transformer(tolower))

## remove punction, numbers, URLs, stop, profanity and stem wordson
cleanSample <- tm_map(cleanSample, content_transformer(removePunctuation))
cleanSample <- tm_map(cleanSample, content_transformer(removeNumbers))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x) 
cleanSample <- tm_map(cleanSample, content_transformer(removeURL))
cleanSample <- tm_map(cleanSample, stripWhitespace)
cleanSample <- tm_map(cleanSample, removeWords, stopwords("english"))
cleanSample <- tm_map(cleanSample, removeWords, profanityWords)
cleanSample <- tm_map(cleanSample, stemDocument)
cleanSample <- tm_map(cleanSample, stripWhitespace)

## Saving the final corpus
saveRDS(cleanSample, file = "C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/finalCorpus.RDS")


## Exploratory analysis 

## Budilding the n-grams

finalCorpus <- readRDS("C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/finalCorpus.RDS")
finalCorpusDF <-data.frame(text=unlist(sapply(finalCorpus,`[`, "content")), 
                           stringsAsFactors = FALSE)

## Building the tokenization function for the n-grams
ngramTokenizer <- function(theCorpus, ngramCount) {
  ngramFunction <- NGramTokenizer(theCorpus, 
                                  Weka_control(min = ngramCount, max = ngramCount, 
                                               delimiters = " \\r\\n\\t.,;:\"()?!"))
  ngramFunction <- data.frame(table(ngramFunction))
  ngramFunction <- ngramFunction[order(ngramFunction$Freq, 
                                       decreasing = TRUE),][1:10,]
  colnames(ngramFunction) <- c("String","Count")
  ngramFunction
}

unigram <- ngramTokenizer(finalCorpusDF, 1)
saveRDS(unigram, file = "C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/unigram.RDS")
bigram <- ngramTokenizer(finalCorpusDF, 2)
saveRDS(bigram, file = "C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/bigram.RDS")
trigram <- ngramTokenizer(finalCorpusDF, 3)
saveRDS(trigram, file = "C:/Users/kavis/Documents/Kavi files/Git-R Files/datasciencecoursera/FinalProject_SwiftKey/Try/trigram.RDS")

