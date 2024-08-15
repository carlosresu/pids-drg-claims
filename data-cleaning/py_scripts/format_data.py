import pandas as pd
import numpy as np

# Create sdx and proc columns from clin_icd and icd9_list
def split_codes(df, column, prefix, max_cols):
    split_cols = df[column].str.split(r'\|\|', expand=True).iloc[:, :max_cols]
    split_cols.columns = [f'{prefix}{i+1}' for i in range(split_cols.shape[1])]
    split_cols = split_cols.reindex(columns=[f'{prefix}{i+1}' for i in range(max_cols)], fill_value=np.nan)
    return split_cols

# Fixed columns to keep
fixed_columns = [
    'pat_age', 'pat_sex', 'date_adm', 'date_dis',
    'pdx', 'clin_discharge', 'pat_bwt',
    'pat_bdate', 'pat_bdate_orig',  # Add pat_bdate and pat_bdate_orig
    # Add the new columns below
    'id_series', 'id_pin', 'time_adm', 'time_dis',
    'date_rec', 'date_ref', 'date_check', 'id_hci',
    'id_hcp', 'clin_outpatient', 'clin_emergency',
    'pat_type', 'clin_acc', 'pat_rel', 'pat_memcat_parent',
    'pat_memcat_child', 'clin_c1', 'clin_c2', 'claim_status',
    'claim_payout', 'claim_charge', 'date_ext', 'id_year',
    'clin_icd', 'clin_rvs', 'clin_c1_orig', 'clin_c2_orig',
    'icd9_list', 'pdx_code', 'drg', 'rw', 'wtlos', 'ot',
    'adjrw', 'err', 'warn', 'los'
]

# Assuming `pandas_df` is already defined in the environment
# Create sdx and proc columns
sdx_columns = split_codes(pandas_df, 'clin_icd', 'sdx', 12)
proc_columns = split_codes(pandas_df, 'icd9_list', 'proc', 20)

# Combine all columns into the final DataFrame
subset_df = pd.concat([pandas_df[fixed_columns], sdx_columns, proc_columns], axis=1)

# Rename 'clin_discharge' to 'discharge'
subset_df.rename(columns={'clin_discharge': 'discharge',
                          'pat_bwt': 'birthweight',
                          'pat_age': 'patage',
                          'pat_sex': 'patsex'}, inplace=True)

# Ensure 'None' is a category in all Categorical columns
for col in subset_df.select_dtypes(include=['category']).columns:
    subset_df[col] = subset_df[col].cat.add_categories(['None'])

# Replace NaN and NA with 'None'
subset_df = subset_df.fillna('None')

# Reorder columns to match desired output
# Columns you want to come first
priority_columns = [
    'id_series', 'id_pin', 'date_adm', 'time_adm', 'date_dis', 'time_dis',
    'date_rec', 'date_ref', 'date_check', 'id_hci', 'id_hcp', 'clin_outpatient',
    'clin_emergency', 'pat_type', 'clin_acc', 'pat_rel', 'pat_bdate', 'pat_age',
    'pat_sex', 'pat_bwt', 'pat_memcat_parent', 'pat_memcat_child',
    'clin_discharge', 'clin_c1', 'clin_c2', 'claim_status', 'claim_payout',
    'claim_charge', 'date_ext', 'id_year', 'clin_icd', 'clin_rvs',
    'clin_c1_orig', 'clin_c2_orig', 'icd9_list', 'pdx', 'pdx_code', 'drg',
    'rw', 'wtlos', 'ot', 'adjrw', 'err', 'warn', 'los'
]

# Remaining columns to follow the priority columns
remaining_columns = [
    'patage', 'patsex', 'date_adm', 'date_dis', 'pdx',
    'sdx1', 'sdx2', 'sdx3', 'sdx4', 'sdx5', 'sdx6',
    'sdx7', 'sdx8', 'sdx9', 'sdx10', 'sdx11', 'sdx12',
    'proc1', 'proc2', 'proc3', 'proc4', 'proc5', 'proc6',
    'proc7', 'proc8', 'proc9', 'proc10', 'proc11', 'proc12',
    'proc13', 'proc14', 'proc15', 'proc16', 'proc17', 'proc18',
    'proc19', 'proc20', 'discharge', 'birthweight', 'ageday'
]

# Combine the lists, ensuring no duplicates
desired_columns = priority_columns + [col for col in remaining_columns if col not in priority_columns]

# Add missing columns with None values if they are not already in the DataFrame
for col in desired_columns:
    if col not in subset_df.columns:
        subset_df[col] = 'None'

# Store the result in output
output = subset_df.rename(columns = {'drg':'thai_drg'})