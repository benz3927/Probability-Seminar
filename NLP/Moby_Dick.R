library(quanteda)
require(readtext)

# Read the text file from the URL
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/NLP/moby.txt"
dat_txtone <- readtext(url)

toks <- tokens(tolower(dat_txtone$text), remove_punct = TRUE)|>
  tokens_split(separator = "—")
ngrams <- tokens_ngrams( toks,n = 4,concatenator = "_")

# Assuming 'ngrams' is a tokens object
x <- ngrams[[1]]

prediction_text <- tokens_select(ngrams, pattern = phrase("where_is_the_*"))
y <- prediction_text[[1]]
table(y)

A <- "sea"
B <- "ocean"

prediction_text <- tokens_select(ngrams, pattern = phrase(paste0("*_", A)))
y <- prediction_text[[1]]
table(y)

for (i in 1:length(y)){
  find_phrase <- sub("}_[^_]+$}", ", y[i]")
  occurances <- tokens_select(ngrams, pattern = phrase(find_phrase))
  z <- table(occurances[[1]])
  count <- z[y[i]]
  prob <- count / length(z)
  count2 <- z[paste0(find_phrase, "_", B)]
  if (is.na(count2)){
    count2 = 0
  }
  prob2 <- count2 / length(z)
  probdif <- abs(prob - prob2)
  dif <- dif + probdif
}
