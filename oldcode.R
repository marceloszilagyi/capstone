# This is code retired/not used



countwordsall = countwords %>% group_by(word) %>% summarize(n = sum(n)) %>% ungroup %>% arrange(desc(n)) %>% mutate (origin = "corpo")

# get the number of unique words count
uniquewordscount = bind_rows(countwords %>% group_by(origin) %>% summarize(n_distinct(word)), 
                             countwordsall %>% summarize(n_distinct(word)) %>% mutate (origin="corpo"))


# select only the 1st 9000 words
topninekwords = bind_rows(countwords[,1:3],countwordsall) %>% spread(key=origin, value = n) %>% arrange(desc(corpo)) %>% top_n(n=9000, wt = corpo)

# calculates the cumulative percentage
topninekwords = topninekwords %>%  mutate(  
  perc_b = b/as.numeric(filter(totalwords, origin=='b')[2]),
  perc_t=  t/as.numeric(filter(totalwords, origin=='t')[2]),
  perc_n = n/as.numeric(filter(totalwords,origin=='n')[2]), 
  perc_corpo = corpo/as.numeric(sum(totalwords[2])))

topninekwords = topninekwords %>% mutate(cum_perc_b = cumsum(perc_b),
                                         cum_perc_t = cumsum(perc_t),
                                         cum_perc_n =  cumsum(perc_n),
                                         cum_perc_corpo = cumsum(perc_corpo))


# prepare the data for the graph
paretonumberwords = select(topninekwords,starts_with("cum_per")) %>% mutate(number_words = row_number())
paretonumberwords = gather(paretonumberwords,'origin','cumulative',1:4)
