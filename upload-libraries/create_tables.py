import os
import pandas as pd
from google.cloud import bigquery

# Initialize a BigQuery client
client = bigquery.Client()

# Load the Excel file with column names and types
excel_file = 'output.xlsx'
xlsx = pd.ExcelFile(excel_file)

# Set variables
CSV_DIRECTORY = 'C:/Users/resur/Documents/drg/Excel/'
DATASET = 'grouper_v5'

# Function to get schema from Excel file
def get_schema(sheet_name):
    df = pd.read_excel(excel_file, sheet_name=sheet_name)
    schema = []
    for _, row in df.iterrows():
        column_name = row['Column_Names']
        column_type = row['Column_Type']
        schema.append(bigquery.SchemaField(column_name, column_type))
    return schema

# List all CSV files in the directory
csv_files = [f for f in os.listdir(CSV_DIRECTORY) if f.endswith('.csv')]

for csv_file in csv_files:
    # Get the full path of the file
    file_path = os.path.join(CSV_DIRECTORY, csv_file)
    
    # Get the base name of the file without the .csv extension
    base_name = os.path.splitext(csv_file)[0]
    
    # Define the table ID
    table_id = f'{client.project}.{DATASET}.{base_name}'

    # Get the schema from the corresponding sheet in the Excel file
    schema = get_schema(base_name)
    
    # Load data into BigQuery
    job_config = bigquery.LoadJobConfig(
        source_format=bigquery.SourceFormat.CSV,
        schema=schema,
        write_disposition=bigquery.WriteDisposition.WRITE_TRUNCATE,  # Replace table data
        skip_leading_rows=1  # Skip the header row
    )
    
    with open(file_path, 'rb') as source_file:
        job = client.load_table_from_file(source_file, table_id, job_config=job_config)
    
    job.result()  # Wait for the job to complete
    print(f'Loaded {file_path} into {table_id}')

print("All data loaded successfully.")
