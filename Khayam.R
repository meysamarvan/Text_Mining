library(gutenbergr) 

physics <- gutenberg_download(c(37729, 14725, 13476, 5001),  
                              meta_fields = "author") 

rumi <- gutenberg_download(c(45159)) 


khayam <- gutenberg_download(c(246)) 



persian_lit <- gutenberg_download(c(10315)) 

############rumi analysis 

rumi_words <- rumi %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() 

rumi_words <- rumi_words %>% 
  anti_join(stop_words) 




##### khayam 

khayam_words <- khayam %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() 

khayam_words <- khayam_words %>% 
  anti_join(stop_words) 

khayam_words <- khayam_words %>% filter(word %nin% c("omar")) 

set.seed(1234) 
wordcloud(words = khayam_words$word, freq = khayam_words$n, min.freq = 1, 
          max.words=50, random.order=FALSE, rot.per=0.25,  
          colors=brewer.pal(8, "Dark2")) 

##### sentiment 

khayam_words <- left_join(khayam_words, get_sentiments("bing"), by = c("word")) 
khayam_words <- left_join(khayam_words, get_sentiments("nrc"), by = c("word")) 

khayam_words <- khayam_words[!is.na(khayam_words$sentiment.x) & !is.na(khayam_words$sentiment.y),] 

ggplot(khayam_words, aes(x = sentiment.x, y = n))+geom_bar(stat = "identity") 


##correlation 







####Qdeath 


Qdeath <- gutenberg_download(c(2800)) 


Qdeath_words <- Qdeath %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() 

Qdeath_words <- Qdeath_words %>% 
  anti_join(stop_words) 

Qdeath_words <- Qdeath_words %>% filter(word %nin% c("ye", "thou", "thee", "thy", "verily", "hath")) 

Qdeath_words$word <- removeNumbers(Qdeath_words$word) 
Qdeath_words$word <- removePunctuation(Qdeath_words$word) 
Qdeath_words <- Qdeath_words %>% filter(word != "") 

set.seed(1234) 
wordcloud(words = Qdeath_words$word, freq = Qdeath_words$n, min.freq = 100, 
          max.words=50, random.order=FALSE, rot.per=0.25,  
          colors=brewer.pal(8, "Dark2")) 

#sentiment 


Qdeath_words <- left_join(Qdeath_words, get_sentiments("bing"), by = c("word")) 
Qdeath_words <- left_join(Qdeath_words, get_sentiments("nrc"), by = c("word")) 

Qdeath_words_sent <- Qdeath_words[!is.na(Qdeath_words$sentiment.x) & !is.na(Qdeath_words$sentiment.y),] 

ggplot(Qdeath_words_sent, aes(x = sentiment.x, y = n))+geom_bar(stat = "identity") 
ggplot(Qdeath_words_sent, aes(x = sentiment.y, y = n))+geom_bar(stat = "identity") 



