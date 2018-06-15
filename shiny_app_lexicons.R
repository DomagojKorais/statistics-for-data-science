#--------------------------------------------------preparation
library("gridExtra")
library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(shiny)

# load list of stop words - from github project
italian_stop_words=read.csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-it/master/stopwords-it.txt")
colnames(italian_stop_words)="word"
italian_stop_words$word=as.character(italian_stop_words$word)
#modifico aggiungendo una "a"
italian_stop_words=rbind(italian_stop_words,"a")


#proviamo a dividire per hashtag, qui correggo e metto gli hashtag giusti senza maiuscole e problemi vari
unique_tweets=readRDS("/home/doma/0dssc/data_managment/esame_finale/data/FINAL_DATA_DEFINITIVO/all_tweets_cleaned_stripped_unique_ita.rds")
unique_tweets_tidy0=separate_rows(unique_tweets) 
dataset_clean <- unique_tweets_tidy0 %>%
  select(hashtags) %>%
  unnest_tokens(word, hashtags)
unique_tweets_tidy0$hashtags=dataset_clean$word

#in ordine di apparizione
hashes= c(  "mattarella", "salvini","dimaio","iostoconmattarella", "ilmiovotoconta","governolegam5s","savona","mattarelladimettiti","cottarelli","impeachment","impeachmentmattarella","maratonamentana") 

#for unique words
datalist = list()
i=1
for(i in 1:length(hashes)){
  hash=hashes[i]
  
  #filtro per hashtag
  unique_tweets_tidy=unique_tweets_tidy0%>%
    filter(hashtags==hash)
  #----------------------------------------------------------pezzo che si occupa di calcolare la frequenza delle parole
  #contiamo le parole, non gli hashtag
  dataset_clean <- unique_tweets_tidy %>%
    select(stripped_text) %>%
    unnest_tokens(word, stripped_text)
  # remove stop words from your list of words
  dataset_clean <- dataset_clean %>%
    anti_join(italian_stop_words)
  #-----------------------------------------------------------
  all_words_counted=dataset_clean %>%
    count(word, sort = TRUE)
  all_words_counted=mutate(all_words_counted,hashtags=hash)
  #store data in a list to open later
  datalist[[i]]=all_words_counted
}
#binding results of counting words
counted_words = do.call(rbind, datalist)
#--------------------------------------------------------------------------------------------






#----------------------------------------------------------------------------------------SHINY SCELTA HASHTAG funziaaaa!!! ricorda le ()
# Define UI for application that plots features of movies 
ui <- fluidPage(
  
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      
      sliderInput(inputId="number2",step = 1, label="First dataset repetitions threshold", min=1,
                  max=500,
                  value=10),
      
      sliderInput(inputId="number1",step = 1, label="Second dataset repetitions threshold", min=1,
                  max=500,
                  value=10),
      
      selectInput(inputId = "hash1", 
                  label = "Select hashtag1:",
                  choices = c("dimaio", "governolegam5s", "ilmiovotoconta", "mattarella", "salvini","iostoconmattarella","mattarelladimettiti","impeachment","impeachmentmattarella"), 
                  selected = "salvini"),
      
      selectInput(inputId = "hash2", 
                  label = "Select hashtag2:",
                  choices = c("dimaio", "governolegam5s", "ilmiovotoconta", "mattarella", "salvini","iostoconmattarella","mattarelladimettiti","impeachment","impeachmentmattarella"), 
                  selected = "dimaio")
      
    ),
    
    # Outputs
    mainPanel(
      
      plotOutput(outputId ="plot1"),
      plotOutput(outputId ="plot2")
      # Show data table
      # dataTableOutput(outputId = "tweet")
    )
  )
)



# Define server function 
server <- function(input,output){
 
  
  #Common and special lexicons
 
  
  all_words_contro = reactive({ counted_words %>%
    filter(hashtags==input$hash2 & n>=input$number1)
    })
 
  all_words_pro = reactive({ counted_words %>%
    filter(hashtags==input$hash1& n>=input$number2 )
  })
  
  only_pro =  reactive({ anti_join(all_words_pro(),all_words_contro(),by="word") })
  
  only_contra =  reactive({ anti_join(all_words_contro(),all_words_pro(),by="word") })
  

  #plot the top 20 words
  output$plot1 <- renderPlot({only_pro()[1:10,] %>%
    #count(word, sort = TRUE) %>%
    #top_n(20,n) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col(aes(alpha=n),show.legend =FALSE) +
    xlab(NULL) +
    coord_flip() +
    labs(y = "Count",
         x = "Unique words",
         title = input$hash1,
         #subtitle = "Hashtags: #iostoconmattarella",
         
         caption=paste("\nN° of words:",as.character(dim(only_pro())),"out of",as.character(dim(all_words_pro())),"filter=",as.character(input$number2) ))+
    theme_minimal()+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))
  })
  #plot the top 20 words
  output$plot2 <- renderPlot({ only_contra()[1:10,] %>%
    #count(word, sort = TRUE) %>%
    #top_n(20,n) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col(aes(alpha=n),show.legend =FALSE) +
    xlab(NULL) +
    coord_flip() +
    labs(y = "Count",
         x = "Unique words",
         title = input$hash2,
         #subtitle = "  Hashtags: #impeachment,\n #mattarellaimpeachment,  #mattarelladimettiti",
         caption=paste("\nN° of words:",as.character(dim(only_contra())),"out of",as.character(dim(all_words_contro())),"filter=",as.character(input$number1) ))+
    
    theme_minimal()+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"))
  })

}

shinyApp(ui=ui, server=server)










