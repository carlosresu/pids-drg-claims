# import pandas as pd
# import numpy as np
# import swifter
# from grouper import seeker

# # Initialize the necessary libraries
# libs = seeker.Libraries()

# # Apply the drg_seeker function directly to each row using swifter
# pandas_df[['mdc', 'pdc', 'dc', 'pccl', 'drg', 'error_code', 'warning_code']] = pandas_df.swifter.apply(
#     lambda row: pd.Series(seeker.drg_seeker(row.to_dict(), libs)),
#     axis=1
# )

# # Store the result in output to be retrieved by R
# output = pandas_df.rename(columns={'drg': 'py_drg'})

# # Define the desired column order
# desired_columns = [
#     'id_series', 'id_pin', 'date_adm', 'time_adm', 'date_dis', 'time_dis',
#     'date_rec', 'date_ref', 'date_check', 'id_hci', 'id_hcp', 'clin_outpatient',
#     'clin_emergency', 'pat_type', 'clin_acc', 'pat_rel', 'pat_bdate', 'patage',
#     'patsex', 'birthweight', 'pat_memcat_parent', 'pat_memcat_child',
#     'discharge', 'clin_c1', 'clin_c2', 'claim_status', 'claim_payout',
#     'claim_charge', 'date_ext', 'id_year', 'clin_icd', 'icd9_list', 'pdx',
#     'pdx_code', # 'thai_drg', 'rw', 'wtlos', 'ot', 'adjrw', 'err', 'warn', 'los', 
#     'mdc', 'pdc', 'dc', 'pccl', 'py_drg', 'ageday', 'error_code', 'warning_code'
# ]

# # Reorder the DataFrame and drop any columns not in the desired list
# output = output[desired_columns]

# # Define the renaming mapping
# rename_mapping = {
#     'patage': 'pat_age',
#     'patsex': 'pat_sex',
#     'birthweight': 'pat_bwt',
#     'discharge': 'clin_discharge',
#     'icd9_list': 'clin_rvs'
# }

# # Rename the columns
# output = output.rename(columns = rename_mapping)

import pandas as pd
import numpy as np
import swifter
from grouper import seeker

# Initialize the necessary libraries
libs = seeker.Libraries()

# Define a function to instantiate a Patient object for each row
def process_patient(row, libs):
    try:
        # Convert the row to a dictionary and create a Patient object
        patient = seeker.Patient(row.to_dict(), libs)
        
        # Extract relevant attributes from the Patient object
        result = {
            'mdc': patient.mdc,
            'pdc': patient.pdc,
            'dc': patient.dc,
            'pccl': patient.pccl,
            'drg': patient.drg,
            'error_code': patient.error_code,
            'warning_code': patient.warning_code
        }
        
        return pd.Series(result)
    
    except Exception as e:
        print(f"Error processing patient with caseid {row['id_series']}: {e}")
        return pd.Series({
            'mdc': None,
            'pdc': None,
            'dc': None,
            'pccl': None,
            'drg': None,
            'error_code': None,
            'warning_code': None
        })

# Apply the Patient class directly to each row using swifter
pandas_df[['mdc', 'pdc', 'dc', 'pccl', 'drg', 'error_code', 'warning_code']] = pandas_df.swifter.apply(
    lambda row: process_patient(row, libs),
    axis=1
)

# Store the result in output to be retrieved by R
output = pandas_df.rename(columns={'drg': 'py_drg'})

# Define the desired column order
desired_columns = [
    'id_series', 'id_pin', 'date_adm', 'time_adm', 'date_dis', 'time_dis',
    'date_rec', 'date_ref', 'date_check', 'id_hci', 'id_hcp', 'clin_outpatient',
    'clin_emergency', 'pat_type', 'clin_acc', 'pat_rel', 'pat_bdate', 'patage',
    'patsex', 'birthweight', 'pat_memcat_parent', 'pat_memcat_child',
    'discharge', 'clin_c1', 'clin_c2', 'claim_status', 'claim_payout',
    'claim_charge', 'date_ext', 'id_year', 'clin_icd', 'icd9_list', 'pdx',
    'pdx_code', 'mdc', 'pdc', 'dc', 'pccl', 'py_drg', 'ageday', 'error_code', 
    'warning_code'
]

# Reorder the DataFrame and drop any columns not in the desired list
output = output[desired_columns]

# Define the renaming mapping
rename_mapping = {
    'patage': 'pat_age',
    'patsex': 'pat_sex',
    'birthweight': 'pat_bwt',
    'discharge': 'clin_discharge',
    'icd9_list': 'clin_rvs'
}

# Rename the columns
output = output.rename(columns=rename_mapping)
