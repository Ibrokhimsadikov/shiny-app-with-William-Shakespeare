library(shiny)
library(quanteda)
library(wordcloud)
library(memoise)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

getDfm <- memoise(function(book, minterms, stem, punct, ngrams)
  
  {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")

  text <- readLines(sprintf("./data/%s.txt", book), encoding="UTF-8")
  
  # could also pass text column of dataframe instead
  myCorpus <- corpus(text)
  
  # if... else if statement depending on 
  if(ngrams == "unigram"){
    ng = 1
  }else if(ngrams == "both"){
    ng = 1:2
  }else if(ngrams == "bigram"){
    ng = 2
  }
  
  
  dfm(myCorpus, remove = stopwords('english'), 
      remove_punct = punct, stem = stem, ngrams = ng) %>%
    dfm_trim(min_termfreq = minterms, verbose = FALSE)
})

  ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Shakespeare's Plays Word Frequencies"), 
    
    sidebarLayout(
     
      sidebarPanel(position="left",
                   selectInput('books', 'Choose a book', choices=books),
                   checkboxInput('stem', 'Stem Words:', value=FALSE),
                   checkboxInput('punct', 'Remove Punctuation:', value=TRUE),
                   radioButtons('ngram', 'Choice in n-grams:', choices = c("Unigrams only" = "unigram",
                                                     "Unigrams & Bigrams" = "both",
                                                     "Bigrams only" = "bigram")),
                   sliderInput('freq', 'Minimum Frequency', min=1, max=50, value=10),
                   hr(),
                   actionButton('run', 'Rerun')
                               
                   
      
                   ),
      
      
      
     mainPanel(position="right",
        tabsetPanel(
          tabPanel("Word Cloud", plotOutput("cloud")), 
          tabPanel("Counts", plotOutput("freq", height = "600px", width="100%"))
        )
      )
    )
  )         
                   


server <- function(input, output) {
  dfm    <- reactive({
    input$run
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getDfm(input$books, input$freq, input$stem, input$punct, input$ngram)

      })
    })})

   output$cloud <- renderPlot({
    v <- dfm()
    textplot_wordcloud(v,
                       min_size=0.5,
                       max_size=6,
                       max_words=100,
                       color=brewer.pal(8, "Dark2"))
  })

    output$freq <- renderPlot({
      v <- dfm()
      dfm_freq <- textstat_frequency(v, n = 50)
      dfm_freq$feature <- with(dfm_freq, reorder(feature, frequency))
      ggplot(dfm_freq, aes(x=feature , y= frequency))+geom_point()+coord_flip()+
        theme(text = element_text(size = 18))
})

  }




shinyApp(ui = ui, server = server)














