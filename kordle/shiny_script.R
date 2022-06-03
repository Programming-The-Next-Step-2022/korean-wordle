#' @export


kordle <- function () {
  
  # load the dataset
  source('wordlist.R', encoding = 'utf-8')
  
  #library(shiny)
  #library(readr)
  
  # Define UI for application that draws a histogram
  ui <- fluidPage(
    
    # interface to be improved
    
    actionButton(inputId = 'clicks', label = '아이'),
    
    tags$h3('title', style='color:grey;'),
    
    tags$h3('안녕', style='color:grey;'),
    
    
    textInput('guess',''),
    actionButton('go', 'Go'),
    verbatimTextOutput('result',placeholder = T),
    
    verbatimTextOutput('keyboard',placeholder = T)
    
  )
  
  
  # select a target word
  target_word <- sample(wordlist$hangeul,1)
  
  
  check_word <- function(target = target_word,guess) {
    
    # check if target and guess words have the same length
    if (nchar(target) != nchar(guess)) {
      stop('the word must have 2 characters!')
    }
    
    # check if target word is in the list
    if(! guess %in% wordlist$hangeul) {
      stop("incorrect spelling or unknown word")
    }
    
    result <- character(6)
    
    # wordle
    for (i in 1:6) {
      
      # if guess word letters are equal to target word letters
      if (as.data.frame(wordlist[wordlist$hangeul == guess,i]) %in%
          as.data.frame(wordlist[wordlist$hangeul == target,i])) {
        
        result[i] <- 'correct'
        
      } else if (as.data.frame(wordlist[wordlist$hangeul == guess,i]) %in%
                 as.data.frame(wordlist[wordlist$hangeul == target,])) {
        
        result[i] <- 'close'
        
      } else {
        
        result[i] <- 'nope'
      }
      
    }
    
    return(result)
    
  } 
  
  #result1 <- check_word('단근','친구')
  
  
  output_word <- function(result,guess) {
    
    answer <- ""
    
    for (i in 1:6) {
      
      if (result[i] == 'correct') {
        
        answer <- paste0(answer,'[',
                         as.data.frame(wordlist[wordlist$hangeul == guess,i]),']')
        
      } else if (result[i] == ' close') {
        
        answer <- paste0(answer,'(',
                         as.data.frame(wordlist[wordlist$hangeul == guess,i]),')')
        
      } else {
        
        answer <- paste0(answer,' ',
                         as.data.frame(wordlist[wordlist$hangeul == guess,i]),' ')
        
      }
    }
    return(answer)
  }
  
  #output_word(result1,'친구')
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    output$result <- renderText({
      
      #paste('your guess:',input$guess)
      output_word(result,input$guess)
      
    }) %>%
      bindEvent(input$go)
    
    
    
    
    output$keyboard <- renderText({
      letters <-  c('ㅂ ㅈ ㄷ ㄱ ㅅ ㅛ ㅕ ㅑ ㅐ ㅔ')
    })
    
    wordss <- list()
    observeEvent(
      input$clicks, {
        
        wordss <- append(wordss,'아이')
        print('아이')
      }
    )
    
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
