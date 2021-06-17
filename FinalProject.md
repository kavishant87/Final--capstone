NextWordPrediction App
========================================================
author: Kavitha Sundaram
date: 06/17/2021
autosize: true


INTRODUCTION
========================================================

- The main goal of this capstone project is to build a shiny application that is able to predict the next word.
- Shiny application was developed using prediction alogorithm and HC corpora.
- Presentation provides the links to Rpubs, Shiny application and source code to github repository.
-Various methods of improving the prediction accuracy and speed were explored


Prediction Algorithm
========================================================

- A large corpus of blog, news and twitter data was loaded and analyzed
- N-grams were extracted from the corpus and then used for building the predictive model 
- Dataset was cleaned, lower-cased, removing links, twitter handles, punctuations, numbers and extra whitespaces, etc
- Those aggregated bi,tri and quadgram term frequency matrices have been transferred into frequency dictionaries.
- The resulting data.frames are used to predict the next word in connection with the text input by a user of the described application and the frequencies of underlying n-grams table.



```r
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
```

Shiny Application Works
========================================================
- Provides a text input box for user to type a word/phrase
- Detects words typed instantaneously and predicts the next word reactively
- Predicts using the longest, most frequent, matching N-gram
- All text data that is used to create a frequency dictionary and thus to predict the next words comes from a corpus called HC Corpora.
- All text mining and natural language processing was done with the usage of a variety of well-known R packages.
 

Links to RPubs,Shiny App, Github Repository
========================================================
 * The next word prediction app is hosted on shinyapps.io:[https://ks21.shinyapps.io/NextWordApp/]
 * Github link for various code files is here:     https://github.com/kavishant87/Final--capstone
 * The R Presentation is published on RPubs :  
 https://rpubs.com/KaviS/NextWordApp
