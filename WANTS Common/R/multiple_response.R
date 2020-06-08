multiple_response <- function(data, question.prefix, group = NULL) {
  
  # Find the columns with the questions
  a <- grep(question.prefix, names(data))
  # Find the total number of responses
  b <- sum(data[, a] != 0)
  # Find the totals for each question
  d <- colSums(data[, a] != 0)
  # Find the number of respondents
  e <- sum(rowSums(data[,a]) !=0)
  # d + b as a vector. This is your overfall frequency 
  f <- as.numeric(c(d, b))
  data.frame(question = c(names(d), "Total"),
             freq = f,
             percent = (f/b)*100,
             percentofcases = (f/e)*100 )
  
 
}





