# Install and load necessary libraries
library(text)
library(readtext)
library(textTinyR)

# Read Moby Dick text from URL
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/NLP/moby.txt"
dat_txtone <- readtext(url)

# Preprocess the text (optional: remove punctuation, lowercase)
preprocessed_text <- tolower(dat_txtone$text)
preprocessed_text <- gsub("[[:punct:]]", "", preprocessed_text)

# Create a text corpus from the Moby Dick text
texts <- c("ocean", "sea")  # Words to compare

# Get embeddings for 'ocean' and 'sea' using pre-trained BERT model
word_embeddings <- textEmbed(texts = texts, model = 'bert-base-uncased')

# Get embeddings for "sea" and "ocean"
embedding_sea <- word_embeddings[1, ]
embedding_ocean <- word_embeddings[2, ]

# Function to calculate cosine similarity
cosine_similarity <- function(vec1, vec2) {
  dot_product <- sum(vec1 * vec2)
  norm1 <- sqrt(sum(vec1^2))
  norm2 <- sqrt(sum(vec2^2))
  return(dot_product / (norm1 * norm2))
}

# Calculate cosine similarity between "sea" and "ocean"
similarity_score <- cosine_similarity(embedding_sea, embedding_ocean)
cat("Cosine similarity between 'sea' and 'ocean':", similarity_score, "\n")

# Optionally, you can further analyze context using n-grams or other text models
