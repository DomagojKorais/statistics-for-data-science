#data collection stuff copied from report

##Rtweet- get user data 


#pezzo per scaricare info utenti da elenco di user_id arbitrariamente lungo
lista=salvini_flw #lista di user_id di cui si vuole scaricare i dati
dimension=count(lista)[[1]]
tranche=list()
zompa=89000 #limite di twitter è 90000 utenti per volta, quindi mi limito
giri=dimension%/%zompa+1 #calcolo il numero di iterazioni necessarie
i=0
for (i in 1:giri){
  tranche[[i]]=lookup_users(lista[[1]][(1+(i-1)*zompa): (1+(i-1)*zompa+zompa)])
  #print((1+(i-1)*zompa)+zompa) indici ok
  print(i)
  Sys.sleep(60*5)#fermo 5 minuti per sicurezza
}
info_users_salvini=do.call("rbind",tranche) #unisco in un unico dataframe
saveRDS(info_users_salvini,"info_users_salvini.rds") #salvo il file


##Rtweet- get tweets 


#random stream of tweets from Italy, used for comparison with political ones
nuovo_governo_random_talks <- stream_tweets( lookup_coords("italy") ,timeout = 60*60)


#tweets given some key word hashtag (return a data frame of more than 40 columns)
#retryonratelimit automate the process of waiting for twitter API's restrictions

tweets <- search_tweets(
  "#hashtag", n = 300000, include_rts = TRUE,retryonratelimit = TRUE
)


