#' Compares the Target and Guess Word
#'
#' The function \emph{compare_word} compares a randomly chosen Korean word from
#' an exhaustive wordlist to a manually entered Korean word, character by character,
#' and provides feedback. Mainly used as reference for \emph{output_word}.
#'
#' @param target a string value of length 2 (character blocks), randomly chosen
#'               from the wordlist to be guessed
#'
#' @param guess a string value of length 2 (character blocks), manually entered
#'              by the user. Must be included in the wordlist
#'
#' @return a string of length 6 (3 characters per block), indicating how accurate
#'         is the guess word
#'
#' @details the wordlist does not include all 2 character Korean words and is
#'          limited to only 6 characters per word, however there are character
#'          blocks containing 4 characters
#'
#' @examples
#' feedback <- compare_word(
#'             target = '안녕',
#'             guess = '친구'
#')
#'
#' @export
compare_word <- function(target = target_word,guess = guess_word) {

  # check if target and guess words are the same length
  if (nchar(target) != nchar(guess)) {
    #stop('sorry, the word must have 2 character blocks!')
    result <- 'nope'
  }

  # check if target word is in the list
  if(! guess %in% wordlist$hangeul) {
    #stop("sorry, this word is not in the list!")
    result <- 'nein'
  }

  result <- character(6)

  # compare each character and give feedback
  for (i in 1:6) {
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

# load the dataset
source('wordlist.R', encoding = 'utf-8')

Sys.setlocale("LC_ALL", "Korean")

# select the target and guess words
target_word <- sample(wordlist$hangeul,1)
guess_word <- '친구'


# check how similar are the target and guess words

# get the translation of the guess word
#' @export
translate_word <- function(guess = guess_word) {

  translation <- toString(wordlist[wordlist$hangeul == guess,'trans'])

  return(translation)
}




#' Gives Feedback on Guess Word
#'
#' The function \emph{output_word}.
#'
#' @param target a string value of length 2 (character blocks), randomly chosen
#'               from the wordlist to be guessed
#'
#' @param guess a string value of length 2 (character blocks), manually entered
#'              by the user. Must be included in the wordlist
#'
#' @param result a string output from function \emph{compare_word}
#'
#' @return a string of length 6 (3 characters per block), indicating how accurate
#'         is the guess word
#'
#' @details the wordlist does not include all 2 character Korean words and is
#'          limited to only 6 characters per word, however there are character
#'          blocks containing 4 characters
#'
#' @export
output_word <- function(target = target_word,
                        guess = guess_word,
                        result = compare_word(target_word,guess_word)) {

  # find which characters match correctly, so it doesn't show 'close' elsewhere
  position <- c()
  guessed <- c()
  for (i in 1:6) {
    if (as.data.frame(wordlist[wordlist$hangeul == target,i]) ==
        as.data.frame(wordlist[wordlist$hangeul == guess,i])) {

      position <- append(position,i)
      guessed <- append(guessed,toString(wordlist[wordlist$hangeul == target,i]))
    }
  }

  pos_counter <- (0)
  answer <- c()
  for (i in 1:6) {

    # if the character is guessed correctly
    if (result[i] == 'correct') {
      answer <- paste0(answer,'[',as.data.frame(wordlist[wordlist$hangeul == guess,i]),']')
    }

    # if the character is right but the position is wrong
    else if (result[i] == 'close' & !(toString(wordlist[wordlist$hangeul == guess,i]) %in% guessed)) {

      # only show 'close' for the actual number of characters (in T(hello)/G(sheep), only mark 1st 'e')

      # how many such characters in guess word
      pos_count_guess <- which(toString(wordlist[wordlist$hangeul == guess_word,i]) %in%
                                 as.data.frame(wordlist[wordlist$hangeul == guess_word,]))
      # how many such characters in target word
      pos_count_target <- which(toString(wordlist[wordlist$hangeul == target_word,i]) %in%
                                  as.data.frame(wordlist[wordlist$hangeul == target_word,]))

      # how many times was there a 'close' character
      pos_counter <- pos_counter + 1

      # if 'close' characters match the number of such target characters (h[e]llo/bl[e]ak)
      if (pos_counter == pos_count_target) {
        answer <- paste0(answer,'(',as.data.frame(wordlist[wordlist$hangeul == guess,i]),')')
      }

      # if there are more 'close' characters than such target character (h[e]llo/sh[e][e]p)
      else if ((pos_count_target < pos_count_guess) & pos_counter == 1) {
        answer <- paste0(answer,'(',as.data.frame(wordlist[wordlist$hangeul == guess,i]),')')
      }

      # if the 'close' character is presented again but there are less such target characters
      else if ((pos_count_target < pos_count_guess) & pos_counter > 1) {
        answer <- paste0(answer,' ',as.data.frame(wordlist[wordlist$hangeul == guess,i]),' ')
      }

      else {
        answer <- paste0(answer,' ',as.data.frame(wordlist[wordlist$hangeul == guess,i]),' ')
      }
    }

    # if the character is not in target word
    else {
      answer <- paste0(answer,' ',as.data.frame(wordlist[wordlist$hangeul == guess,i]),' ')
    }
  }
  return(answer)
}
