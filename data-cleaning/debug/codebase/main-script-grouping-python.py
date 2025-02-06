from rpy2.robjects import r, globalenv
from rpy2.robjects.packages import importr
import os
r_source = r['source']
r_source("~/drg-pipeline/data-cleaning/00a-parameters.r")
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
print("Parallelization:", to_parallel)
year_file_path = os.path.expanduser("~/drg-pipeline/data-cleaning/cache/year_to_load.txt")
with open(year_file_path, "r") as file:
    year_to_load = file.read().strip()  # .strip() removes any surrounding whitespace or newlines
if to_sample:
    suffix = f"_sampled_{sample_size_divisor}_"
else:
    suffix = "_full_"
print(suffix)
print(year_to_load)
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
feather_file_path = f"~/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_7_py_input/python_input_{year_to_load}{suffix}.feather"
feather_file_path = os.path.expanduser(feather_file_path)
pandas_df = pd.read_feather(feather_file_path)
print(pandas_df[(pandas_df['patage'] == 0) & (pandas_df['ageday'].notna())])
print("Converting data types")
pandas_df['patage'] = pd.to_numeric(pandas_df['patage'], errors='coerce')
pandas_df['ageday'] = pd.to_numeric(pandas_df['ageday'], errors='coerce')
pandas_df['birthweight'] = pd.to_numeric(pandas_df['birthweight'], errors='coerce')
pandas_df['discharge'] = pandas_df['discharge'].astype('Int64')
string_columns = ['id_series', 'patsex', 'pdx', 'sdx1', 'sdx2', 'sdx3', 'sdx4', 'sdx5', 'sdx6', 'sdx7', 'sdx8', 'sdx9', 'sdx10', 'sdx11', 'sdx12',
                  'proc1', 'proc2', 'proc3', 'proc4', 'proc5', 'proc6', 'proc7', 'proc8', 'proc9', 'proc10', 'proc11', 'proc12',
                  'proc13', 'proc14', 'proc15', 'proc16', 'proc17', 'proc18', 'proc19', 'proc20', 'date_adm', 'date_dis', 'dob']
print("Replacing with None")
pandas_df.replace([pd.NA, np.nan, '<NA>', 'None', 'NA', -2147483648], None, inplace=True)
print("Converting to string")
pandas_df[string_columns] = pandas_df[string_columns].astype('string')
print("Replacing -2147483648 with None")
pandas_df.replace(-2147483648, None, inplace=True)
print("Generating info()")
pandas_df.info()
print(pandas_df)
print(pandas_df[(pandas_df['patage'] == 0) & (pandas_df['ageday'].notna())])
        
        
    
        
        
print(pandas_df)
file_path = f"/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_7_py_input/python_final_input_{year_to_load}{suffix}.feather"
pandas_df.to_feather(file_path)
print("Initializing Libraries")
libs = seeker.Libraries()
def process_patient(row, libs):
    try:
        patient = seeker.Patient(row.to_dict(), libs)
        
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
        print(f'''Error processing patient with id_series {row['id_series']}: {e}''')
        
        traceback.print_exc()  # Print the full stack trace for more details
        
        return {
            'id_series': row['id_series'],  # Ensure `id_series` is carried forward
            'pdc': None,
            'pccl': None,
            'drg': None,
            'error_code': None,
            'warning_code': None
        }
def process_chunk(chunk):
    return [process_patient(row, libs) for _, row in chunk.iterrows()]
if __name__ == "__main__":
    print("Splitting DataFrame into chunks")
    num_cores = cpu_count()  # Use the number of available CPU cores
    chunk_size = len(pandas_df) // num_cores
    chunks = [pandas_df[i:i + chunk_size] for i in range(0, len(pandas_df), chunk_size)]
    print("Processing chunks with multiprocessing")
    with Pool(num_cores) as pool:
        results = pool.map(process_chunk, chunks)
    print("Combining results")
    processed_data = pd.DataFrame([row for chunk in results for row in chunk])
    print("Renaming columns")
    processed_data.rename(columns={'drg': 'py_drg'}, inplace=True)
    print("Reordering columns")
    desired_columns = [
        'id_series', 'pdc', 
        'pccl', 'py_drg', 'error_code', 
        'warning_code'
    ]
    print("Subsetting columns")
    pandas_df = processed_data[desired_columns]
    print(pandas_df)
print(pandas_df)
file_path = f"/home/resurreccion_cmc/drg-pipeline/data-cleaning/data/checkpoints/checkpoint_8_py_output/python_output_{year_to_load}{suffix}.feather"
pandas_df.to_feather(file_path)
