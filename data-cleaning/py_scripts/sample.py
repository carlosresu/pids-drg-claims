import pandas as pd
import os

# ---------------------- Configuration ----------------------

# Base path where all yearly .dta files are stored
base_dir = '/Users/carlosresu'

# Base filename prefix (before the year)
base_filename = 'claims_'

# Number of rows to randomly sample
sample_size = 10000

# ---------------------- Loop Through Years ----------------------

# Loop through each year from 2018 to 2023
for year in range(2018, 2024):

    # Construct full path to the input .dta file for the given year
    input_file = os.path.join(base_dir, f'{base_filename}{year}_anon.dta')

    # Check if the file exists before attempting to process it
    if not os.path.isfile(input_file):
        print(f"[SKIPPED] File not found: {input_file}")
        continue

    # Construct the output CSV path by replacing the extension and adding sample size
    output_file = input_file.replace('.dta', f'_sample_{sample_size}.csv')

    print(f"[PROCESSING] Sampling {sample_size} rows from: {input_file}")

    # Read the Stata file into a DataFrame
    df = pd.read_stata(input_file)

    # Sample rows randomly
    sampled_df = df.sample(n=sample_size, random_state=20250930)

    # Save to CSV with UTF-8 encoding
    sampled_df.to_csv(output_file, index=False, encoding='utf-8')

    print(f"[DONE] Sample saved to: {output_file}")
