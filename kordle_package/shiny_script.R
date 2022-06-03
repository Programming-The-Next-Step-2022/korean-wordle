library(shiny)
library(readr)
library(kordle)
library(dplyr)
library(shinythemes)

# load the dataset
source('wordlist.R', encoding = 'utf-8')

# setting locale to Korean will display the characters properly
Sys.setlocale("LC_ALL", "Korean")

ui <- fluidPage(
  theme = shinytheme("journal"),

  tags$h2('kordle', style='color:black;'),
  tags$h5('안녕! guess the word in Korean!', style='color:grey;'),

  fluidRow(

    column(5,wellPanel(

      actionButton('go', 'guess!'),

      # type the guesses
      textInput('enter', label='',value='enter your guess here'),
      verbatimTextOutput('result',placeholder = T),

      # show which letters are left
      #verbatimTextOutput('keyboard'),

      # display guessed words
      tags$h4('your guesses:', style='color:grey;'),
      verbatimTextOutput('guess1',placeholder = T),

      # display if correct / ran out of guesses
      verbatimTextOutput('correct')
    )),

    column(4,wellPanel(
      tags$h4('instructions', style='color:grey;'),
      tags$h5(
        'Enter Korean words of 2 character blocks,
        e.g. 안녕. If both a character and
        and its place are correct, it is marked with square brackets
        (e.g. [ㄱ] ).If the character is right, but
        its place is wrong, it is marked with round brackets,
        (e.g. (ㅏ) ). If there are no brackets, keep guessing
        other characters. You have 5 guesses. 화이팅!',
        style='color:grey;'
      ),
      tags$h6(
        'copy for testing: 시간, 내일, 사과, 단근',
        style='color:grey;'
      )
    ))
  )
)

# select the target word
target_word <- sample(wordlist$hangeul,1)

server <- function(input, output) {

  # track the number of guesses
  guess_count <- reactiveValues(countervalue = 0)

  # track the guesses
  all_guesses <- character()


  # update the number of guesses with each press
  guess_num <- eventReactive(input$go, {
    guess_count$countervalue <- guess_count$countervalue + 1
  })



  # show the guess
  output$result <- renderText({

    if (guess_num() < 5){

      # give translation
      translation <- translate_word(input$enter)

      paste0(input$enter,' (',translation,')')

    } else {
      paste("You'll get it next time!")
    }

  }) %>%
    bindEvent(input$go)




  # provide feedback for every guess separately + show previous guesses
  output$guess1 <- renderText({

    # for 5 guesses
    if (guess_num() < 5){

      if (input$enter == target_word){
        translation <- translate_word(input$enter)
        paste0('Correct! The word is ',input$enter,' (',translation,')!')

      } else if (input$enter != target_word) {

        all_guesses <<- c(all_guesses,input$enter)


        out_str <- vapply(all_guesses,function(guess_word) {

          # check if guess word and give feedback
          guess_feedback <- compare_word(target_word,input$enter)
          output_word(target_word,input$enter,guess_feedback)

        }, character(1))

        paste(guess_num(),') ',input$enter,' /',out_str,collapse = '\n')
        #paste(all_guesses,collapse = '\n')
      }
    } else {
      paste(all_guesses)
    }

  }) %>%
    bindEvent(input$go)


  # for correct answer
  output$correct <- renderText({
    translation <- translate_word(input$enter)

    if (input$enter == target_word){
      paste0('Correct! The word is ',input$enter,' (',translation,')!')
    }

    # if not guessed in 5 tries
    if (guess_num() == 5){
      paste0('Welp! The word was ',input$enter,' (',translation,')!')
    }

  })%>%
    bindEvent(input$go)

  # additional - add keyboard

  #output$keyboard <- renderText({
  #  letters <-  c('ㅂ ㅈ ㄷ ㄱ ㅅ ㅛ ㅕ ㅑ ㅐ ㅔ',
  #                '\n ㅁ ㄴ ㅇ ㄹ ㅎ ㅗ ㅓ ㅏ ㅣ ',
  #                '\n   ㅋ ㅌ ㅊ ㅍ ㅠ ㅜ ㅡ')
  #})

}

# Run the application
shinyApp(ui = ui, server = server)
