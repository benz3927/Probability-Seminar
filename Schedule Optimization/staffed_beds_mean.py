import requests
from bs4 import BeautifulSoup
import pandas as pd

# Example URL - replace this with the actual URL where the data is located
url = 'https://www.ahd.com/individual_hospital_statistics_for_new_york.html'

# Send a request to fetch the webpage content
response = requests.get(url)
soup = BeautifulSoup(response.content, 'html.parser')

# Find the table rows where hospital data is located
rows = soup.find_all('tr')

# Extract the data from each row
data = []
for row in rows[1:]:  # Skipping the header row
    columns = row.find_all('td')
    if len(columns) > 0:
        # Extract 'Staffed Beds' value (index 2 in the table)
        staffed_beds = columns[2].text.strip()
        try:
            # Convert the staffed_beds value to integer
            staffed_beds = int(staffed_beds)
        except ValueError:
            staffed_beds = None  # Handle cases where the value is empty or not valid
        data.append(staffed_beds)

# Filter out None values
data = [bed for bed in data if bed is not None]

# Calculate the mean of staffed beds
mean_beds = pd.Series(data).mean()
print(f"Mean of Staffed Beds: {mean_beds}")
