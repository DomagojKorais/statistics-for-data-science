#shiny DEFINITIVO correlation cloud su tutto il dataset

library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(shiny)
library(DT)




#unique_tweet:dataset che contiene tutti i tweet diversi un dall'altro e che non sono retweet. in questo modo ho tutte le frasi diverse pro 
#pronunciate, che mi sembra ottimo per studiare il lessico degli utenti.

#data_prova_words: dataset con i tweet di unique_tweet elaborati secondo il cleaning necessario a produrre il grafico delle correlazioni


#dati ripuliti in cleaning.R
#unique_tweets=readRDS("/home/doma/0dssc/data_managment/esame_finale/data/FINAL_DATA_DEFINITIVO/all_tweets_cleaned_stripped_unique_ita.rds")
#data_prova_words=readRDS("/home/doma/0dssc/data_managment/esame_finale/data/FINAL_DATA_DEFINITIVO/shiny_words_input_duplicates.rds")

#-----preparing for correlation 
# load list of stop words - from github project
italian_stop_words=read.csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-it/master/stopwords-it.txt")
colnames(italian_stop_words)="word"
italian_stop_words$word=as.character(italian_stop_words$word)
#modifico aggiungendo una "a"
italian_stop_words=rbind(italian_stop_words,"a")
#-----






#proviamo a dividire per hashtag
unique_tweets=readRDS("./all_tweets_cleaned_stripped_unique_ita.rds")
unique_tweets_tidy=separate_rows(unique_tweets) 
dataset_clean <- unique_tweets_tidy %>%
  select(hashtags) %>%
  unnest_tokens(word, hashtags)
#put in original dataframe
unique_tweets_tidy$hashtags=dataset_clean$word






#----------------------------------------------------------------------------------------SHINY SCELTA HASHTAG funziaaaa!!! ricorda le ()
# Define UI for application that plots features of movies 
ui <- fluidPage(
  
  
  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      
      sliderInput(inputId="correlation",step = 0.1, label="Correlation", min=0.,
                  max=1.,
                  value=0.5),
      
      sliderInput(inputId="number",step = 1, label="Number of repetitions", min=1,
                  max=1000,
                  value=50),
     
      selectInput(inputId = "hash", 
                  label = "Select hashtag:",
                  choices = c("dimaio", "governolegam5s", "ilmiovotoconta", "mattarella", "salvini","iostoconmattarella","mattarelladimettiti","impeachment","impeachmentmattarella"), 
                  selected = "mattarella"),
      actionButton("mybutton","Submit")
    ),
    
    # Outputs
    mainPanel(
      
      plotOutput(outputId ="plot1"),
      # Show data table
      dataTableOutput(outputId = "tweet")
    )
  )
)



# Define server function 
server <- function(input,output){
  
  unique_tweets_filtered= reactive({
    unique_tweets_tidy%>%
    filter(hashtags==input$hash)
     
    })
  
  data_prova=reactive({unique_tweets_filtered()%>%
    select(status_id,stripped_text)
  })
  
  data_prova_words=reactive({data_prova() %>%
    unnest_tokens(word,stripped_text)
  })
  
  # remove stop words from your list of words
  word_cors  <- reactive({
    
    data_prova_words() %>%
    anti_join(italian_stop_words) %>%
      group_by(word) %>%
      filter(n() >= input$number) %>%
      pairwise_cor(word, status_id, sort = TRUE)
  })
  
  output$plot1 <- renderPlot({
       input$mybutton
       isolate(
      word_cors ()%>%
      filter(correlation > input$correlation) %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_alpha = correlation), show.legend = TRUE ) +
      geom_node_point(color = "lightblue", size = 5) +
      geom_node_text(aes(label = name), repel = TRUE,size= 5,color="black") +
      theme_minimal())

  } )
  
  # Create data table vera
  output$tweet <- DT::renderDataTable({
    input$mybutton
    isolate(

    unique_tweets_filtered()%>%
      select(screen_name,text))
  })


 


}

shinyApp(ui=ui, server=server)



