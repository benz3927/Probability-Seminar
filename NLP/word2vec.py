# This script processes the text of "Moby Dick" by tokenizing it using both SpaCy and NLTK, 
# filtering out stopwords, and training a Word2Vec model on the filtered tokens using the 
# Continuous Bag of Words (CBOW) method. We then calculate the cosine similarity "ocean" and "sea") 
# based on their word embeddings generated by the Word2Vec model. 
# Cosine similarity measures how similar the two word vectors are, with a score closer to 1 indicating high similarity.

# Import necessary libraries
import nltk
import requests
import spacy
import numpy as np
from nltk.tokenize import regexp_tokenize
from nltk.corpus import stopwords
from gensim.models import Word2Vec

# Download necessary NLTK data
nltk.download('punkt')
nltk.download('stopwords')

# Define URL to Moby Dick text file
url = "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/NLP/moby.txt"
response = requests.get(url)
moby_text = response.text

# Load the SpaCy model
nlp = spacy.load("en_core_web_sm")

# Increase the maximum length for processing
nlp.max_length = 2000000  # You can adjust this as needed

# Tokenization using spaCy
doc = nlp(moby_text)
tokens_spacy = [token.text.lower() for token in doc if token.is_alpha]

# Tokenization using NLTK (alternative method)
tokens_nltk = regexp_tokenize(moby_text.lower(), pattern=r"\s|[\.,;'\-!\*]", gaps=True)

# Filter out stopwords
stop_words = set(stopwords.words('english'))
tokens_filtered_spacy = [word for word in tokens_spacy if word not in stop_words]
tokens_filtered_nltk = [word for word in tokens_nltk if word not in stop_words]

# Train Word2Vec model using Gensim
model = Word2Vec([tokens_filtered_spacy], vector_size=100, window=5, min_count=1, workers=4)

# Function to calculate cosine similarity
def cosine_similarity(vec1, vec2):
    dot_product = np.dot(vec1, vec2)
    norm1 = np.linalg.norm(vec1)
    norm2 = np.linalg.norm(vec2)
    return dot_product / (norm1 * norm2)

# Words to compare
word1 = "ocean"
word2 = "sea"

# Check if the words are in the model's vocabulary
if word1 in model.wv and word2 in model.wv:
    # Get the word embeddings for 'ocean' and 'sea'
    embedding_ocean = model.wv[word1]
    embedding_sea = model.wv[word2]

    # Calculate cosine similarity between 'ocean' and 'sea'
    similarity_score = cosine_similarity(embedding_ocean, embedding_sea)

    # Print the similarity score
    print(f"Cosine similarity between '{word1}' and '{word2}':", similarity_score)
else:
    print(f"One or both of the words '{word1}' and '{word2}' are not in the vocabulary.")
print(model.wv.similarity(word1, word2))

word3 = "ship"

print(model.wv.similarity(word2, word3))