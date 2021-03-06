% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{output_word}
\alias{output_word}
\title{Gives Feedback on Guess Word}
\usage{
output_word(
  target = target_word,
  guess = guess_word,
  result = compare_word(target_word, guess_word)
)
}
\arguments{
\item{target}{a string value of length 2 (character blocks), randomly chosen
from the wordlist to be guessed}

\item{guess}{a string value of length 2 (character blocks), manually entered
by the user. Must be included in the wordlist}

\item{result}{a string output from function \emph{compare_word}}
}
\value{
a string of length 6 (3 characters per block), indicating how accurate
is the guess word
}
\description{
The function \emph{output_word}.
}
\details{
the wordlist does not include all 2 character Korean words and is
limited to only 6 characters per word, however there are character
blocks containing 4 characters
}
