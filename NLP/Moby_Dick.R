# Load libraries
library(quanteda)
require(readtext)
library(data.table)

# Read the text file from the URL
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/NLP/moby.txt"
dat_txtone <- readtext(url)

# Split into tokens (words)
toks <- tokens(tolower(dat_txtone$text), remove_punct = TRUE)|>
  tokens_split(separator = "—")

# Create list of all 4-grams in the book
ngrams <- tokens_ngrams(toks, n = 4, concatenator = "_")

# Two words to compare...
A <- "sea"
B <- "ocean"

# Look for instances of any three words followed by word A
predictiontext <- tokens_select(ngrams, pattern = phrase(paste0("*_",A)))
y<-predictiontext[[1]]

# Go through each of these instances...
totalabsdiff <- 0
for(i in 1:length(y))
{
  print(i)
  
  # Find all times in the text where the first three words appear
  phrasetofind <- sub("_[^_]+$", "", y[i])
  occurrences <- tokens_select(ngrams, pattern = phrase(paste0(phrasetofind,"*")))
  
  # What fraction of the time is it followed by word A? 
  z <- table(occurrences[[1]])
  count <- z[y[i]]
  prob <- count / length(occurrences[[1]])
  
  # How about word B? (Might be NA if the answer is never)
  count2 <- z[paste0(phrasetofind,"_",B)]
  if (is.na(count2))
  {
    count2 = 0
  }
  prob2 <- count2 / length(occurrences[[1]])
  
  # Calculate and sum the absolute difference between these probabilities
  probdiff <- abs(prob-prob2)
  totalabsdiff <- totalabsdiff + probdiff
  
}

# To do... look at all instances of word B as well 
# Now do the same thing with occurrences where word B is the fourth word in the n-grams
predictiontext_B <- tokens_select(ngrams, pattern = phrase(paste0("*_", B)))
y_B <- predictiontext_B[[1]]

for(i in 1:length(y_B)) {
  print(i)
  
  # Find all times in the text where the first three words appear
  phrasetofind <- sub("_[^_]+$", "", y_B[i])
  occurrences <- tokens_select(ngrams, pattern = phrase(paste0(phrasetofind, "*")))
  
  # Probability of B following the phrase
  z <- table(occurrences[[1]])
  count_B <- z[y_B[i]]
  prob_B <- count_B / length(occurrences[[1]])
  
  # Probability of A following the phrase
  count_A <- z[paste0(phrasetofind, "_", A)]
  if (is.na(count_A)) {
    count_A = 0
  }
  prob_A <- count_A / length(occurrences[[1]])
  
  # Calculate and sum the absolute difference between these probabilities
  probdiff_B <- abs(prob_B - prob_A)
  totalabsdiff_B <- totalabsdiff_B + probdiff_B
}

# Print total absolute differences
cat("Total absolute difference for occurrences of word A:", totalabsdiff_A, "\n")
cat("Total absolute difference for occurrences of word B:", totalabsdiff_B, "\n")