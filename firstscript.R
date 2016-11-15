# get the dataset
library('data.table')
library('tidytext')
library('magrittr')
library(dplyr)
library(dtplyr)
library("tibble")

libraries = c('tm', 'rJava')
lapply (libraries, function (x) library(x,character.only = TRUE))

# get the dataset from web and unzip it
a = download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',temp)
unzip(a)
rm(a)

# build a support table with the file info
# name of files
files = list.files(recursive = TRUE, pattern = glob2rx('*.txt'))
# length of each file
length = lapply(files, function (x) length(readLines(x)))
length = as.numeric((unlist(length)))
# language of each file (this needs a better grep in the future)
fileinfo = cbind.data.frame(files = as.character(files),length,stringsAsFactors = FALSE)
fileinfo$language = ifelse(grepl(fileinfo$files,pattern = "en_"),"en", 
                           ifelse(grepl(fileinfo$files,pattern = "de_"),"de",
                                  ifelse(grepl(fileinfo$files,pattern = "fi_"),"fi", "ru")))
                                                                                            


# selects one of the files to be used  - in the future, just replace it with a loop for all files
filename  = fileinfo$files[5]

mytokenization <- function(filename) {
  j= fread(input = filename, sep = '\n', header = FALSE, stringsAsFactors = FALSE, col.names = "stuff", colClasses = "character", nrows = round(fileinfo$length[match(filename,fileinfo$files)]*0.05,0) )
  k = j %>%  unnest_tokens(word, stuff)
  return(k)
  }

my

# make a very basic read of all files - 1% of lines per file and store in a list


sample(size = 0.01*x , x)
readLines(usfiles[1],)
length(readLines(usfiles[1]))

j= fread(input = fileinfo$files[5],sep = '\n',data.table = FALSE, header = FALSE, stringsAsFactors = FALSE, col.names = "stuff",colClasses = "character")

j %>%  unnest_tokens(word, stuff)
j = data_frame(j$stuff)


scan(fileinfo$files[1], '', skip = 6, nlines = 1, sep = '\n')



# taks
#Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function that takes a file as input and returns a tokenized version of it.

#Profanity filtering - removing profanity and other words you do not want to predict



# tips
#Loading the data in. This dataset is fairly large. We emphasize that you don't necessarily need to load the entire dataset in to build your algorithms (see point 2 below). At least initially, you might want to use a smaller subset of the data. Reading in chunks or lines using R's readLines or scan functions can be useful. You can also loop over each line of text by embedding readLines within a for/while loop, but this may be slower than reading in large chunks at a time. Reading pieces of the file at a time will require the use of a file connection in R. For example, the following code could be used to read the first few lines of the English Twitter dataset:con <- file("en_US.twitter.txt", "r") readLines(con, 1) ## Read the first line of text readLines(con, 1) ## Read the next line of text readLines(con, 5) ## Read in the next 5 lines of text close(con) ## It's important to close the connection when you are done
#See the ?connections help page for more information.

#Sampling. To reiterate, to build models you don't need to load in and use all of the data. Often relatively few randomly selected rows or chunks need to be included to get an accurate approximation to results that would be obtained using all the data. Remember your inference class and how a representative sample can be used to infer facts about a population. You might want to create a separate sub-sample dataset by reading in a random subset of the original data and writing it out to a separate file. That way, you can store the sample and not have to recreate it every time. You can use the rbinom function to "flip a biased coin" to determine whether you sample a line of text or not.


