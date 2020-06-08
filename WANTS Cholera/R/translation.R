# Setup
# install.packages("googleLanguageR")
# install.packages("sp")
# install.packages("rgdal")

gl_auth("./R/google translate API-924fa19be79e.json")
Sys.setlocale("LC_ALL","Arabic")


# Translation function 
translate.others.arabic <- function(data, cols = c(), ignore.cols = NULL) {
  #cols <- names(data)[endsWith(names(data), "_other")] 
  cols <- dplyr::select(data, c(cols))
  cols <- colnames(cols)


  if (!is.null(ignore.cols)) {
    cols <- cols[-which(!is.na(match(cols, ignore.cols)))]
  }
  result <- data.frame(question.name = character(), uuid = character(), row = numeric(), arabic = character(), english = character(),
                       stringsAsFactors = F)
  for (i in 1:length(cols)) {
    cat(sprintf("%d/%d\n", i, length(cols)))
    indices <- which(!is.na(data[,cols[i]]) & data[,cols[i]] != "")
    if (length(indices) > 0) {
      for (j in 1:length(indices)) {
        result[nrow(result) + 1, 1] <- cols[i]
        result$row[nrow(result)] <- indices[j]
        result$uuid[nrow(result)] <- data$parent_uuid[indices[j]]
        arab <- data[indices[j], cols[i]]
        result$arabic[nrow(result)] <- arab
        print(j)
        if (is.character(arab)) {
          translation <- gl_translate(arab, target = "en")
          result$english[nrow(result)] <- translation$translatedText
        } else {
          result$english[nrow(result)] <- "ERROR: input is not text"
        }
      }
    }
  }
  return(result)
}


