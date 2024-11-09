import subprocess
import sys

# Install necessary libraries
subprocess.check_call([sys.executable, "-m", "pip", "install", "gensim", "nltk", "requests"])

# Now import the necessary libraries
import nltk
import gensim
import numpy as np
from nltk.tokenize import word_tokenize
from nltk.corpus import stopwords
from gensim.models import Word2Vec
import requests

# Download Moby Dick text
url = "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/NLP/moby.txt"
response = requests.get(url)
moby_text = response.text

# Preprocess the text: Tokenization, removing stopwords, and punctuation
nltk.download('punkt')
nltk.download('stopwords')

# Tokenizing and cleaning the text
tokens = word_tokenize(moby_text.lower())  # Lowercase and tokenize
tokens = [word for word in tokens if word.isalpha()]  # Remove punctuation
stop_words = set(stopwords.words('english'))
tokens = [word for word in tokens if word not in stop_words]  # Remove stopwords

# Create and train a Word2Vec model on the tokens
model = Word2Vec([tokens], vector_size=100, window=5, min_count=1, workers=4)

# Get the embeddings for "ocean" and "sea"
embedding_ocean = model.wv['ocean']
embedding_sea = model.wv['sea']

# Cosine similarity function
def cosine_similarity(vec1, vec2):
    dot_product = np.dot(vec1, vec2)
    norm1 = np.linalg.norm(vec1)
    norm2 = np.linalg.norm(vec2)
    return dot_product / (norm1 * norm2)

# Calculate the similarity between "sea" and "ocean"
similarity_score = cosine_similarity(embedding_ocean, embedding_sea)
print(f"Cosine similarity between 'sea' and 'ocean': {similarity_score}")
