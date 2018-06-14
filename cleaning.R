#vari step di cleaning

#tidying dataset for lexical analysis
library(tidyverse)
library(rtweet)

library(gridExtra)

library(tidytext)
library(widyr)
library(igraph)
library(ggraph)

######################################################################## DEFINING CLEANING FUNCTION
#based on https://stackoverflow.com/questions/31348453/how-do-i-clean-twitter-data-in-r
cleaning_tweets=function(unclean_tweet){
  unclean_tweet=paste(unclean_tweet, collapse=" ")
  #get rid of unnecessary spaces
  clean_tweet <- str_replace_all(unclean_tweet," "," ")
  # Get rid of URLs
  clean_tweet <- str_replace_all(clean_tweet, "https://t.co/[a-z,A-Z,0-9]*","")
  clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*","")
  # Take out retweet header, there is only one
  clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
  # Get rid of hashtags
  clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
  # Get rid of references to other screennames
  clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z,0-9]*","")  
  # Get rid of second piece of screen name
  clean_tweet <- str_replace_all(clean_tweet,"_[a-z,A-Z]*","") 
  # Get rid of numbers except 5
  clean_tweet <- str_replace_all(clean_tweet,"[1,2,3,4,6,7,8,9,0]*","")
  return(clean_tweet)
}
########################################################################STOP WORDS
# load list of stop words - from github project
italian_stop_words=read.csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-it/master/stopwords-it.txt")
colnames(italian_stop_words)="word"
italian_stop_words$word=as.character(italian_stop_words$word)
#modifico aggiungendo una "a"
italian_stop_words=rbind(italian_stop_words,"a")
########################################################################

#IMPORTING AL RAW DATA
d1=readRDS("../data/tweets/iostoconmattarella_retweet" )
d2=readRDS("../data/tweets/mattarella_retweet" )
d3=readRDS("../data/tweets/governolegam5s_retweet" )
d4=readRDS("../data/tweets/impeachmentmattarella.rds" )
d5=readRDS("../data/tweets/ilmiovotoconta_retweet" )
d6=readRDS("../data/tweets/mattarelladimmettiti.rds" )
d7=readRDS("../data/tweets/dimaio_retweet" )
d8=readRDS("../data/tweets/dimaio.rds" )
d9=readRDS("../data/tweets/impeachment_mattarella_retweet" )
d10=readRDS("../data/tweets/impeachment_mattarella_retweet" )
d11=readRDS("../data/tweets/impeachment.rds" )
d12=readRDS("../data/tweets/mattarella.rds" )
d13=readRDS("../data/tweets/salvini.rds" )
d14=readRDS("../data/tweets/salvini_retweet" )
d15=readRDS("../data/tweets/stoconmattarella.rds" )
d16=readRDS("../data/tweets/votocheconta.rds" )


dataset=rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16)
saveRDS(dataset,"./tutti_i_tweets_grezzo.RDS")
rm(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16)
dataset=readRDS("./tutti_i_tweets_grezzo.RDS")

#FIRST REMOVE DUPLICATES
#length = 1012909
dataset=distinct(dataset , status_id , .keep_all=TRUE)
#lenght = 764625
saveRDS(dataset,"../data/FINAL_DATA_DEFINITIVO/tutti_i_tweet_no_duplicati.RDS")
dataset=readRDS("../data/FINAL_DATA_DEFINITIVO/tutti_i_tweet_no_duplicati.RDS")
#problema dei retweet troncati non restituiti dall'API di twitter, risolta utilizzando il testo del tweet originale! bugguato

#soluzione buggata (#berlusconi,pro) non me lo separa
#dataset=dataset %>% mutate (hashtags = case_when(is_retweet == TRUE ~ str_extract_all(retweet_text,"#\\S+"),
#                                                 is_retweet == FALSE ~ hashtags))

#soluzione giusta
dataset=dataset %>% mutate (hashtags = case_when(is_retweet == TRUE ~ str_extract_all(retweet_text,"#[:alnum:]+"),
                                                 is_retweet == FALSE ~ hashtags))




saveRDS(dataset,"../data/FINAL_DATA_DEFINITIVO/tutti_i_tweet_no_duplicati.RDS")

#Select subset of variables
dataset=dataset%>% 
  select( status_id , created_at , user_id , screen_name , text , is_quote , is_retweet , favorite_count , retweet_count , hashtags,lang )
saveRDS(dataset,"../data/FINAL_DATA_DEFINITIVO/reduced_tweet_no_duplicati.RDS")
dataset=readRDS("../data/FINAL_DATA_DEFINITIVO/reduced_tweet_no_duplicati.RDS")
dataset=dataset_good_hash






#add stripped and cleaned words
dataset=dataset%>%rowwise()%>%
  mutate(stripped_text=cleaning_tweets(text))

saveRDS(dataset,"../data/FINAL_DATA_DEFINITIVO/reduced_tweet_no_duplicati_stripped_text.RDS")
dataset=readRDS("../data/FINAL_DATA_DEFINITIVO/reduced_tweet_no_duplicati_stripped_text.RDS")


#-----------------------------------------------------properly cleaned hashtags
#tidy hashtags
dataset=separate_rows(dataset)
#only italian language tweets
dataset=dataset%>%filter(lang=="it")
# remove punctuation, convert to lowercase all tweets
dataset_clean <- dataset %>%
  select(hashtags) %>%
  unnest_tokens(word, hashtags)
#put in original dataframe
dataset$hashtags=dataset_clean$word
dataset$hashtags=str_replace_all(dataset$hashtags,"#","")
#-----------------------------------------------------




saveRDS(dataset,"../data/FINAL_DATA_DEFINITIVO/reduced_tweet_no_duplicati_stripped_text_separated.RDS")
dataset=readRDS("../data/FINAL_DATA_DEFINITIVO/reduced_tweet_no_duplicati_stripped_text_separated.RDS")
#only italian language tweets
#dataset=dataset%>%filter(lang=="it")


#---------------------------------------------------------------------------cleaning for lexical analysis I want all different words
#cleaning for word analysis, kill all repetitions

dataset=readRDS("../data/FINAL_DATA_DEFINITIVO/reduced_tweet_no_duplicati_stripped_text.RDS")
unique_tweets=dataset%>%
  #kill all tweets equals one to the other even with links, since I use stripped words
  distinct(stripped_text,.keep_all=TRUE)%>%
  filter(is_retweet==FALSE)%>%
  filter(is_quote==FALSE)
#only italian language tweets
unique_tweets=unique_tweets%>%filter(lang=="it")
#salviamo questo dataset che contiene tutti i tweet diversi un dall'altro e che non sono retweet. in questo modo ho tutte le frasi diverse pro 
#pronunciate, che mi sembra ottimo per studiare il lessico degli utenti.
saveRDS(unique_tweets,"../data/FINAL_DATA_DEFINITIVO/all_tweets_cleaned_stripped_unique_ita.rds")







#---------------------------------------------------------------------------