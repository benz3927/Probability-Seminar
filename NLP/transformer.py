from transformers import BertTokenizer, BertForMaskedLM, Trainer, TrainingArguments

# Load tokenizer and model
tokenizer = BertTokenizer.from_pretrained('bert-base-uncased')
model = BertForMaskedLM.from_pretrained('bert-base-uncased')

# Example of loading data from a URL (using requests or another method)
import requests
url = "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/NLP/moby.txt"
response = requests.get(url)
data = response.text

# Tokenize the data
inputs = tokenizer(data, return_tensors="pt", truncation=True, padding=True)

# Ensure that input_ids, attention_mask, and token_type_ids are provided
print(inputs)  # Check your inputs format

# Continue with your trainer setup
training_args = TrainingArguments(
    output_dir='./results',
    evaluation_strategy="epoch",
    learning_rate=2e-5,
    per_device_train_batch_size=16,
    num_train_epochs=3,
)

trainer = Trainer(
    model=model,
    args=training_args,
    train_dataset=inputs,  # Use the tokenized dataset
    eval_dataset=inputs,   # Add evaluation dataset if needed
)

trainer.train()

from transformers import BertForMaskedLM, BertTokenizer
import torch

# Sample input
inputs = tokenizer(data, return_tensors="pt", truncation=True, padding=True)
input_ids = inputs['input_ids']
attention_mask = inputs['attention_mask']

# Forward pass
outputs = model(input_ids, attention_mask=attention_mask, labels=input_ids)

# Loss and logits
loss = outputs.loss
logits = outputs.logits

print(loss)
