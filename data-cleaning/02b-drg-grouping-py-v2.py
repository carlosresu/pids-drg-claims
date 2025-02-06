#!/usr/bin/env python
# coding: utf-8

# ---
# title: "DRG Cleaning (Python) v2"
# 
# author: "Carlos Resurreccion"
# 
# date: "2024-11-19"
# 
# ---
# 

# In[ ]:


# !gsutil -m rsync -r /mnt/data-disk/data gs://phic-claims-checkpoints/temp/data/
# !gsutil -m rsync -r gs://phic-claims-checkpoints/temp/data /mnt/data-disk/data/


# In[ ]:


# Import rpy2 packages
from rpy2.robjects import r, globalenv
from rpy2.robjects.packages import importr
import os

# Source the R script
r_source = r['source']
r_source("~/drg-pipeline/data-cleaning/00a-parameters.r")

# Extract R variables into Python
thread_offset = r['thread_offset'][0]
sample_size_divisor = int(r['sample_size_divisor'][0])
to_sample = bool(r['to_sample'][0])
to_write = bool(r['to_write'][0])
to_flush = bool(r['to_flush'][0])
to_parallel = bool(r['to_parallel'][0])
to_debug = bool(r['to_debug'][0])
verbose_output = bool(r['verbose_output'][0])
to_generate_subset = bool(r['to_generate_subset'][0])
to_py_prompt = bool(r['to_py_prompt'][0])
to_python = bool(r['to_python'][0])
to_generate_py_fwrite = bool(r['to_generate_py_fwrite'][0])
to_generate_feather = bool(r['to_generate_feather'][0])
to_py_bq = bool(r['to_py_bq'][0])
to_thai_prompt = bool(r['to_thai_prompt'][0])
to_thai = bool(r['to_thai'][0])
to_thai_bq = bool(r['to_thai_bq'][0])
to_generate_thai_txt = bool(r['to_generate_thai_txt'][0])
to_thai_all_years = bool(r['to_thai_all_years'][0])
to_spc = bool(r['to_spc'][0])

# Print one of the variables to verify
print("Parallelization:", to_parallel)

# Path to the year_to_load file
year_file_path = os.path.expanduser("~/drg-pipeline/data-cleaning/cache/year_to_load.txt")

# Read the year from the file
with open(year_file_path, "r") as file:
    year_to_load = file.read().strip()  # .strip() removes any surrounding whitespace or newlines

# MANUAL OVERRIDES
# to_sample = True
# sample_size_divisor = 25

# Create a suffix based on the `to_sample` flag and sample_size_divisor
if to_sample:
    suffix = f"_sampled_{sample_size_divisor}_"
else:
    suffix = "_full_"

print(suffix)
print(year_to_load)


# In[ ]:


import pandas as pd
import os
import numpy as np
from grouper import seeker
from multiprocessing import Pool, cpu_count
import traceback
import swifter
import traceback
import sys
import io
import pyarrow
import gc


# In[ ]:


# Construct the file path for the Feather file
feather_file_path = f"~/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_7_py_input/python_input_{year_to_load}{suffix}.feather"

# Expand the `~` to the user's home directory
feather_file_path = os.path.expanduser(feather_file_path)

# Read the Feather file
pandas_df = pd.read_feather(feather_file_path)

# Print the DataFrame or process it as needed
# print(pandas_df)
print(pandas_df[(pandas_df['patage'] == 0) & (pandas_df['ageday'].notna())])

# # Sample 100,000 rows from the DataFrame
# full_df = pandas_df
# pandas_df = pandas_df.sample(n=100000, random_state=42)


# In[ ]:


# # Define the patient data as a dictionary with lists
# pat = {
#     'id_series': ['1'],
#     'patage': [0],
#     'patsex': ['F'],
#     'date_adm': ['2018-08-01 14:46:00'],
#     'date_dis': ['2018-08-04 07:00:00'],
#     'pdx': ['J189'],
#     'sdx1': [None],
#     'sdx2': [None],
#     'sdx3': [None],
#     'sdx4': [None],
#     'sdx5': [None],
#     'sdx6': [None],
#     'sdx7': [None],
#     'sdx8': [None],
#     'sdx9': [None],
#     'sdx10': [None],
#     'sdx11': [None],
#     'sdx12': [None],
#     'proc1': [None],
#     'proc2': [None],
#     'proc3': [None],
#     'proc4': [None],
#     'proc5': [None],
#     'proc6': [None],
#     'proc7': [None],
#     'proc8': [None],
#     'proc9': [None],
#     'proc10': [None],
#     'proc11': [None],
#     'proc12': [None],
#     'proc13': [None],
#     'proc14': [None],
#     'proc15': [None],
#     'proc16': [None],
#     'proc17': [None],
#     'proc18': [None],
#     'proc19': [None],
#     'proc20': [None],
#     'discharge': [1],
#     'birthweight': [2.717],
#     'ageday': [1]
# }
# pandas_df = pd.DataFrame(pat)


# In[ ]:


# Convert the column types explicitly
print("Converting data types")
pandas_df['patage'] = pd.to_numeric(pandas_df['patage'], errors='coerce')
pandas_df['ageday'] = pd.to_numeric(pandas_df['ageday'], errors='coerce')
pandas_df['birthweight'] = pd.to_numeric(pandas_df['birthweight'], errors='coerce')
pandas_df['discharge'] = pandas_df['discharge'].astype('Int64')

# Convert string columns to 'string' dtype and replace NA values with None
string_columns = ['id_series', 'patsex', 'pdx', 'sdx1', 'sdx2', 'sdx3', 'sdx4', 'sdx5', 'sdx6', 'sdx7', 'sdx8', 'sdx9', 'sdx10', 'sdx11', 'sdx12',
                  'proc1', 'proc2', 'proc3', 'proc4', 'proc5', 'proc6', 'proc7', 'proc8', 'proc9', 'proc10', 'proc11', 'proc12',
                  'proc13', 'proc14', 'proc15', 'proc16', 'proc17', 'proc18', 'proc19', 'proc20', 'date_adm', 'date_dis', 'dob']

print("Replacing with None")
# Replace missing values in place
pandas_df.replace([pd.NA, np.nan, '<NA>', 'None', 'NA', -2147483648], None, inplace=True)

print("Converting to string")
# Convert columns to string dtype after replacing the values
pandas_df[string_columns] = pandas_df[string_columns].astype('string')

print("Replacing -2147483648 with None")
# Replace -2147483648 with None again (in case it was missed)
pandas_df.replace(-2147483648, None, inplace=True)

# print("Filter discharge")
# # Filter rows where 'discharge' is not in [1, 2, 3, 4, 9]
# not_in_list_values = pandas_df.loc[~pandas_df['discharge'].isin([1, 2, 3, 4, 9]), 'discharge']

# # Get unique values and their counts
# unique_not_in_list_values = not_in_list_values.value_counts()

# # Print the unique values and their counts
# print(unique_not_in_list_values)

print("Generating info()")
pandas_df.info()
print(pandas_df)


# In[ ]:


print(pandas_df[(pandas_df['patage'] == 0) & (pandas_df['ageday'].notna())])


# In[ ]:


# # SINGLE THREADED-VERSION
# print("Initializing Libraries")
# # Initialize the necessary libraries
# libs = seeker.Libraries()

# # Define a function to instantiate a Patient object for each row
# def process_patient(row, libs):
#     try:
#         # Convert the row to a dictionary and create a Patient object
#         patient = seeker.Patient(row.to_dict(), libs)
        
#         # Extract relevant attributes from the Patient object
#         result = {
#             'pdc': patient.pdc,
#             'pccl': patient.pccl,
#             'drg': patient.drg,
#             'error_code': patient.error_code,
#             'warning_code': patient.warning_code
#         }
        
#         return pd.Series(result)
    
#     except Exception as e:
#         # Log the error and row information for debugging
#         print(f'''Error processing patient with id_series {row['id_series']}: {e}''')
        
#         # Optionally, you can log more information such as row content or traceback
#         traceback.print_exc()  # Print the full stack trace for more details
        
#         # Return None or default values for the error case
#         return pd.Series({
#             'pdc': None,
#             'pccl': None,
#             'drg': None,
#             'error_code': None,
#             'warning_code': None
#         })

# print("swifter.apply process_patient")
# # Apply the Patient class directly to each row using swifter
# pandas_df[['pdc', 'pccl', 'drg', 'error_code', 'warning_code']] = pandas_df.apply(
#     lambda row: process_patient(row, libs),
#     axis=1
# )
# print("Renaming columns")
# # Store the result in output to be retrieved by R
# output = pandas_df.rename(columns={'drg': 'py_drg'})
# print("Reordering columns")
# # Define the desired column order
# desired_columns = [
#     'id_series', 'pdc', 
#     'pccl', 'py_drg', 'error_code', 
#     'warning_code'
# ]
# print("Subsetting columns")
# # Reorder the DataFrame and drop any columns not in the desired list
# output = output[desired_columns]
# print(output)


# In[ ]:


# Define a function to instantiate a Patient object for each row
# def process_patient(row, libs):
#     try:
#         # Convert the row to a dictionary and create a Patient object
#         patient = seeker.Patient(row.to_dict(), libs)

#         # Extract relevant attributes from the Patient object
#         result = {
#             'mdc': patient.mdc,
#             'pdc': patient.pdc,
#             'pccl': patient.pccl,
#             'drg': patient.drg,
#             'error_code': patient.error_code,
#             'warning_code': patient.warning_code
#         }

#         return result

#     except Exception as e:
#         # Log the error and row information for debugging
#         print(f"Error processing patient with id_series {row['id_series']}: {e}")
#         traceback.print_exc()

#         # Return default values for error cases
#         return {
#             'mdc': None,
#             'pdc': None,
#             'pccl': None,
#             'drg': None,
#             'error_code': None,
#             'warning_code': None
#         }

# # Initialize the necessary libraries
# print("Initializing Libraries")
# libs = seeker.Libraries()

# # Split DataFrame into chunks for multiprocessing
# num_cores = cpu_count()  # Automatically detect the number of CPU cores
# chunks = np.array_split(pandas_df, num_cores)  # Split the DataFrame into chunks

# print(f"Processing using {num_cores} cores")

# # Use multiprocessing to process each chunk in parallel
# with Pool(num_cores) as pool:
#     results = pool.starmap(process_chunk, [(chunk, libs) for chunk in chunks])

# # Combine the results back into a single DataFrame
# processed_df = pd.concat(results)

# # Add the processed columns to the original DataFrame
# pandas_df[['mdc', 'pdc', 'pccl', 'drg', 'error_code', 'warning_code']] = processed_df

# # Rename and reorder columns
# pandas_df.rename(columns={'drg': 'py_drg'}, inplace=True)
# desired_columns = [
#     'id_series', 'mdc', 'pdc',
#     'pccl', 'py_drg', 'error_code',
#     'warning_code'
# ]
# # Drop columns not in the desired list
# columns_to_drop = [col for col in pandas_df.columns if col not in desired_columns]
# pandas_df.drop(columns=columns_to_drop, inplace=True)
# Initialize the necessary libraries


# In[ ]:


print(pandas_df)


# In[ ]:


# Construct the file path
file_path = f"/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_7_py_input/python_final_input_{year_to_load}{suffix}.feather"
# Save the DataFrame as a Feather file
pandas_df.to_feather(file_path)

# Initialize the necessary libraries
print("Initializing Libraries")
libs = seeker.Libraries()

# Define a function to instantiate a Patient object for each row
def process_patient(row, libs):
    try:
        # Convert the row to a dictionary and create a Patient object
        patient = seeker.Patient(row.to_dict(), libs)
        
        # Extract relevant attributes from the Patient object
        result = {
            'id_series': row['id_series'],  # Ensure `id_series` is carried forward
            'pdc': patient.pdc,
            'pccl': patient.pccl,
            'drg': patient.drg,
            'error_code': patient.error_code,
            'warning_code': patient.warning_code
        }
        
        return result

    except Exception as e:
        # Log the error and row information for debugging
        print(f'''Error processing patient with id_series {row['id_series']}: {e}''')
        
        # Optionally, you can log more information such as row content or traceback
        traceback.print_exc()  # Print the full stack trace for more details
        
        # Return default values for the error case
        return {
            'id_series': row['id_series'],  # Ensure `id_series` is carried forward
            'pdc': None,
            'pccl': None,
            'drg': None,
            'error_code': None,
            'warning_code': None
        }

# Helper function to process a chunk of the DataFrame
def process_chunk(chunk):
    return [process_patient(row, libs) for _, row in chunk.iterrows()]

# Main logic to process the DataFrame with multiprocessing
if __name__ == "__main__":
    print("Splitting DataFrame into chunks")
    # Split DataFrame into chunks
    num_cores = cpu_count()  # Use the number of available CPU cores
    chunk_size = len(pandas_df) // num_cores
    chunks = [pandas_df[i:i + chunk_size] for i in range(0, len(pandas_df), chunk_size)]

    print("Processing chunks with multiprocessing")
    # Use multiprocessing Pool to process chunks
    with Pool(num_cores) as pool:
        results = pool.map(process_chunk, chunks)

    print("Combining results")
    # Combine the results back into a DataFrame
    processed_data = pd.DataFrame([row for chunk in results for row in chunk])

    print("Renaming columns")
    # Rename columns as required
    processed_data.rename(columns={'drg': 'py_drg'}, inplace=True)

    print("Reordering columns")
    # Define the desired column order
    desired_columns = [
        'id_series', 'pdc', 
        'pccl', 'py_drg', 'error_code', 
        'warning_code'
    ]

    print("Subsetting columns")
    # Reorder the DataFrame and drop any columns not in the desired list
    pandas_df = processed_data[desired_columns]

    # Print or save the final output
    print(pandas_df)


# In[ ]:


print(pandas_df)


# In[ ]:


# !pip install matplotlib
# import matplotlib.pyplot as plt

# # Drop rows with NaN values in the 'py_drg' column (if necessary)
# valid_py_drg = pandas_df['py_drg'].dropna()

# # Convert 'py_drg' to numeric if it's not already
# valid_py_drg = pd.to_numeric(valid_py_drg, errors='coerce').dropna()

# # Plot the histogram
# plt.figure(figsize=(10, 6))
# plt.hist(valid_py_drg, bins=30, edgecolor='black', color='blue')
# plt.title('Histogram of py_drg')
# plt.xlabel('py_drg')
# plt.ylabel('Frequency')
# plt.grid(axis='y', linestyle='--', alpha=0.7)

# # Show the plot
# plt.show()


# In[ ]:


# Construct the file path
file_path = f"/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_8_py_output/python_output_{year_to_load}{suffix}.feather"
# Save the DataFrame as a Feather file
pandas_df.to_feather(file_path)

