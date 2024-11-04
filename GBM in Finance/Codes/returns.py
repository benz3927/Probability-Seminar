# Import necessary libraries
import pandas as pd
import matplotlib.pyplot as plt

# Read the CSV file from the URL
url = "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/Report%202/msft.csv"
df = pd.read_csv(url)

# Display the first few rows of the dataframe
print("First few rows of the dataframe:")
print(df.head())

# Display the column names
print("\nColumn names in the DataFrame:")
print(df.columns)

# Check for missing values
missing_values = df.isnull().sum()
print("\nMissing values in each column:")
print(missing_values)

df['date'] = pd.to_datetime(df['date'])

# Set the date as the index
df.set_index('date', inplace=True)

# Plot the Closing Prices using the 'value' column
plt.figure(figsize=(12, 6))
plt.plot(df['value'], label='Closing Price', color='blue')
plt.title('Microsoft Stock Closing Prices')
plt.xlabel('Date')
plt.ylabel('Price (USD)')
plt.legend()
plt.grid()
# plt.show()

# Calculate and display basic statistics
statistics = df.describe()
print("\nBasic statistics of the dataframe:")
print(statistics)

# Calculate daily returns
df['Daily Return'] = df['value'].pct_change()

# Plotting daily returns
plt.figure(figsize=(12, 6))
plt.plot(df['Daily Return'], label='Daily Returns', color='orange')
plt.title('Microsoft Stock Daily Returns')
plt.xlabel('Date')
plt.ylabel('Daily Return')
plt.legend()
plt.grid()
# plt.show()

import numpy as np
import math

# Calculate log returns
df['Log Return'] = np.log(df['value'] / df['value'].shift(1))

# Calculate mean and standard deviation (sigma) of log returns
mean_log_return = df['Log Return'].mean()
sigma_log_return = df['Log Return'].std()

# Correct the formula for mu and sigma

sigma = sigma_log_return
mu = 252*(mean_log_return+0.5*sigma**2)

# Display the results
print("\nMu (Mean of Log Returns):", mu)
print("Sigma (Standard Deviation of Log Returns):", sigma)


