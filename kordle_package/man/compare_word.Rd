% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{compare_word}
\alias{compare_word}
\title{Compares the Target and Guess Word}
\usage{
compare_word(target = target_word, guess = guess_word)
}
\arguments{
\item{target}{a string value of length 2 (character blocks), randomly chosen
from the wordlist to be guessed}

\item{guess}{a string value of length 2 (character blocks), manually entered
by the user. Must be included in the wordlist}
}
\value{
a string of length 6 (3 characters per block), indicating how accurate
is the guess word
}
\description{
The function \emph{compare_word} compares a randomly chosen Korean word from
an exhaustive wordlist to a manually entered Korean word, character by character,
and provides feedback. Mainly used as reference for \emph{output_word}.
}
\details{
the wordlist does not include all 2 character Korean words and is
limited to only 6 characters per word, however there are character
blocks containing 4 characters
}
\examples{
feedback <- compare_word(
            target = '안녕',
            guess = '친구'
)

}
