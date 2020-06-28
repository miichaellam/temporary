library(tidyverse)
library(rtweet)
library(tidytext)
# read data
data<- read_twitter_csv('finaldf.csv')

# clean data
df<-data%>%distinct(text,.keep_all = T) %>% 
  mutate(tweet = tolower(text),
         user = tolower(screen_name))%>% 
  filter(!str_detect(user, 'nhs'))%>%
  filter(is_retweet == F)%>%
  filter(!screen_name %in% c('lifebloodau','givebloodNHS')) %>%  # filtering out blood service tweets
  mutate(blood_service = ifelse(str_detect(mentions_screen_name, 'GiveBloodNHS'), 'United Kingdom','Australia')) %>% # blood var
  mutate(blood_service = as.factor(blood_service),
         id = row_number(),
         favourite = favorite_count,
         retweet = retweet_count,
         tweet = str_replace_all(tweet, "@\\w+"," "),           # remove user names
         tweet = str_replace_all(tweet, "http.+ |http.+$", " "), # remove links
         tweet = str_replace_all(tweet,"[[:punct:]]", " "), # punctuations
         tweet = str_replace_all(tweet, "amp", " "),
         tweet = str_replace_all(tweet, "[ |\t]{2,}", " "),
         tweet = tm::removeNumbers(tweet))# remove numbers

# convert dates to same timezone
df$date<- lubridate::ymd_hms(df$created_at) # convert to format
df$date<- lubridate::with_tz(df$date, tzone = 'Europe/London') # standardise all time

# adding words to stop-word library
# Stop words
blood_words<- tibble(word = c('blood','donation','donations'),
                     lexicon = 'mine')
# stop word dictionaries
blood_stops<- rbind(blood_words,stop_words)

# creating trigram
donor_trigram<-df%>%select(tweet,blood_service)%>%
  unnest_tokens(trigram,tweet, token = 'ngrams', n =3) %>%
  separate(trigram, c('word1','word2','word3'), sep = ' ')%>%
  filter(!word1 %in% blood_stops$word,
         !word2 %in% blood_stops$word,
         !word3 %in% blood_stops$word)

# assessing most frequent words
donorplot<-donor_trigram%>%unite(trigram,word1,word2,word3,sep = " ")%>%
  filter(!str_detect(trigram, 'emma'))%>%
  add_count(trigram)%>%
  distinct(trigram,.keep_all = T)%>%
  top_n(7)%>%
  ggplot(aes(x = reorder(trigram,n), y = n, fill = trigram))+
  geom_col(show.legend = F) +
  coord_flip() + 
  theme_light() +
  labs(x = NULL, y = 'Number of mentions', title = 'Common trigram in donor tweets',
       caption = 'Common words removed (e.g., donate, and,you)' ) +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5), axis.text.y = element_text(size = 11),
        plot.caption = element_text(face = 'italic'))
