library(kordle)

context('Core kordle functionality')


test_that('guess word is of correct length',{
  
  # load the dataset
  source('C:/Users/Emilija/Desktop/r_dir/kordle/kordle/wordlist.R',
         encoding = 'utf-8')
  
  # select the target and guess words
  target_word <- sample(wordlist$hangeul,1)
  guess_word1 <- '친구'
  guess_word2 <- '해'
  
  test1 <- compare_word(
    target = target_word, guess = guess_word1
  )
  
  test2 <- compare_word(
    target = target_word, guess = guess_word2
  )
  
  # should error if the word is incorrect length
  expect_equal(length(test1),6)
  #expect_equal(length(test2),6)
})

