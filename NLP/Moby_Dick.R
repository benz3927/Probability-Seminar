library(quanteda)
require(readtext)
dat_txtone <- readtext("moby.txt")
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
