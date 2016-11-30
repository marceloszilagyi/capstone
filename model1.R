# this is the script to create the model for the n grams

# load libraries

listpackages = c('data.table', 'tidytext', 'magrittr', 'dplyr', 'dtplyr', 'tibble', 'tm', 'stringr', 'ggplot2','scales','DT', 'tidyr', 'igraph','magrittr','gridExtra','readr')
loaded = suppressMessages(suppressWarnings(
  sapply(listpackages, function (x) library(x,character.only = T))
))

# get the files
englishfiles = list.files(recursive = TRUE, pattern = glob2rx('*en_*.txt'))

if(length(englishfiles)==0) { 
  # get the dataset from web and unzip it
  download.file('https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip',destfile = "temp")
  unzip('temp')
  file.remove('temp')}

# setup a small size for testing of the script
fraction =  0.01
filenames = str_extract(englishfiles,"(?<=.)[^.]+(?=.txt)")

  # assign to create the 3 pieces of data
  for (i in seq_along(englishfiles)) {
   assign(filenames[i], readLines(con = englishfiles[i],warn = FALSE)) }

  # create tibbles
  twitter = as_tibble(twitter);blogs = as_tibble(blogs);news = as_tibble(news)

  # add row number and origin for future use
  twitter = twitter %>% mutate(linenumber = row_number(), origin = "t") %>% sample_frac(size=fraction) 
  blogs = blogs %>% mutate(linenumber = row_number(), origin = "b") %>% sample_frac(size=fraction)
  news = news %>% mutate(linenumber = row_number(), origin = "n") %>% sample_frac(size=fraction) 
  corpo = bind_rows(twitter,news,blogs)

# consolidate and select 60% for train, 20% test 20% validation
  corpo = bind_rows(twitter,news,blogs)

  # this is the line that helps to recover loss during processing
  set.seed(12345)
  corpo = corpo %>% group_by(origin) %>% mutate(set = sample(c("train", "test","validation"), length(origin),
                                              replace = TRUE, prob = c(6,2,2))) %>% ungroup()
  
  train = corpo %>% subset(set=="train")
  test =  corpo %>% subset(set=="test")
  validation = corpo %>% subset(set=="validation")
  
# save tibbles in txt as a backup
  write_csv(train, col_names = TRUE, paste0("/data/",Sys.time(),"train.csv"))

  # cleaning- multiple cleanings
  # problem number 1 - the 's' problem
  train$value = gsub(pattern = "[`’‘]",replacement = "'", x = train$value)
  
  # problem number 2 - only numbers
  numbers = "^-?\\d+(,\\d+)*(\\.\\d+(e\\d+)?)?$"
  train$value = gsub(pattern = numbers, replacement = " ", x = train$value)
  
  # problem number 3 - web references
  web = "\\w\\.{1}\\D{2,3}$"
  train$value = gsub(pattern = web, replacement = " ", x = train$value)
  
  #problem number 4 - numbers in the middle of values
  mid = "\\d"
  train$value = gsub(pattern = mid, replacement = " ", x = train$value)
  
  #problem number 5 - values with foreign character()
  foreign = "[^0-9A-Za-z\\.']"
  train$value = gsub(pattern = foreign, replacement = " ", x = train$value)
  
  # create list of words and sort by frequency
  corpo = train %>% unnest_tokens(word, value)
  countwords = corpo %>%  ungroup() %>% count(word, sort = TRUE) 
  
  # if required, get the dictionary
  if(!file.exists("dictionary.txt")){
    download.file('https://raw.githubusercontent.com/dwyl/english-words/master/words3.txt',destfile = "dictionary.txt")
  }
  
  if(length(ls(pattern = "dictionary"))!=1) {
    dictionary = read_csv("dictionary.txt",col_names = "word")
    dictionary  = dictionary %>% mutate(dictionary="dictionary")
  }
         
  nonenglishtrain <- countwords %>% anti_join(dictionary,by = "word") %>% ungroup() %>% arrange(desc(n))
  englishtrain <- countwords %>% inner_join(dictionary,by = "word") %>% ungroup() %>% arrange(desc(n))
 
# create bigrams
  
  bigram = corpo %>% unnest_tokens(bigram,word,token = "ngrams", n=2)
  splitwords = as_tibble(str_split(bigram$bigram," ",simplify = TRUE))
  bigram  = bind_cols(bigram, splitwords) %>% rename(word1 = V1, word2 = V2)
  rm(splitwords)
  countingtrigrams = count(bigram,bigram,word1,word2,sort=TRUE)
  rm(bigram)
  
# cut the tail for bigrams
  # use the approach with dictionary words
  countingtrigrams = semi_join(semi_join(countingtrigrams,englishtrain,by = c("word1" = "word")),
                          englishtrain,by = c("word2" = "word")) %>% arrange(desc(n))
  
  countingtrigrams = countingtrigrams %>% ungroup() %>% mutate(cumuper = cumsum(n)/sum(n))
  countingtrigrams %>% filter(cumuper <0.95)

# create trigrams
  trigram = corpo %>% unnest_tokens(trigram ,word,token = "ngrams", n=3)
  trisplitwords = as_tibble(str_split(trigram$trigram," ",simplify = TRUE))
  trigram  = bind_cols(trigram, trisplitwords) %>% rename(word1 = V1, word2 = V2, word3 = V3)
  rm(trisplitwords)
  
  # basic counting
  countingtrigrams = count(trigram, trigram, word1, word2, word3, sort=TRUE)
  rm(trigram)
  
# cut the tail for trigrams
  countingtrigrams = semi_join(semi_join(semi_join(countingtrigrams,englishtrain,by = c("word1" = "word")),
                              englishtrain,by = c("word2" = "word")), 
                              englishtrain, by = c("word3"="word")) %>% arrange(desc(n))
  
  countingtrigrams = countingtrigrams %>% ungroup() %>% mutate(cumuper = cumsum(n)/sum(n))
  countingtrigrams %>% filter(cumuper <0.95)
  
# join trigrams and bigrams (sort by word and count - see if works)
  test2 = as_tibble(countingbigrams[1:100, ]) %>% mutate(bigramtotal = n)
  test3 = as_tibble(countingtrigrams[1:100, ]) %>% mutate(bigram = paste(word1,word2))
  test_out = bind_rows(test3,test2) 
  test_out %>% arrange(desc(n))
  test_out = left_join(test3,test2,by="bigram") %>% arrange(desc(n.x), desc(n.y))
  test_out = test_out 
  
  
# create function that returns based on the bigram or the trigram
