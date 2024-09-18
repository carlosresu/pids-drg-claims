import pandas as pd
import numpy as np
import swifter
from grouper import seeker
import traceback
import sys
import io

### START OF seeker.py
# import pandas as pd
# import numpy as np
# import json
# import operator
# import itertools
# import dateutil.parser

# from grouper.scripts.data import *
# from grouper.scripts.mdc import *
# from grouper.scripts.pdc import *
# from grouper.scripts.dc import *
# from grouper.scripts.drg import *

# # Class for all libraries on the backend of the TDRG grouper
# class Libraries():
# 	def __init__(self):
		
# 		#Load libraries
# 		self.ax = json.loads(open('libraries/ax.json','r', encoding='utf-8').read())
# 		self.bmdc = json.loads(open('libraries/bmdc.json','r', encoding='utf-8').read())
# 		self.ccex = json.loads(open('libraries/ccex.json','r', encoding='utf-8').read())
# 		self.cclm = json.loads(open('libraries/cclm.json','r', encoding='utf-8').read())
# 		self.da = json.loads(open('libraries/da.json','r', encoding='utf-8').read())
# 		self.dc = json.loads(open('libraries/dc.json','r', encoding='utf-8').read())
# 		self.drg = json.loads(open('libraries/drg.json','r', encoding='utf-8').read())
# 		self.i10 = json.loads(open('libraries/i10.json','r', encoding='utf-8').read())
# 		self.mdc = json.loads(open('libraries/mdc.json','r', encoding='utf-8').read())
# 		self.pcom = json.loads(open('libraries/pcom.json','r', encoding='utf-8').read())
# 		self.proc = json.loads(open('libraries/proc.json','r', encoding='utf-8').read())
# 		self.prpdc = json.loads(open('libraries/prpdc.json','r', encoding='utf-8').read())
# 		self.pdc = json.loads(open('libraries/pdc.json','r', encoding='utf-8').read())


# # Patient object with all relevant attributes from claims and DRG-related effects
# class Patient():
# 	def __init__(self, patient, libs):
# 		'''Takes in a dictionary of all a patients required 
# 		data elements and converts to a Patient object'''

# 		## DATA ENTRY
# 		patient = data_cleaner(patient, libs)
# 		fields = ['age','ageday','sex','pdx','sdx','diagnoses','procedures',
# 			'procedures_rfmt','time_adm','time_dis','los','hours','discharge',
# 			'birthweight','radio','chemo','warning_code']
# 		for field in fields:
# 			setattr(self, field, patient[field])

# 		## DETERMINE MDC, PDC, DC, AND DRG

# 		# Pre-MDC will return an instant DRG + error code if not none
# 		error_drg, self.error_code = determine_validity(self, libs)
# 		if error_drg is not None:
# 			self.mdc = None
# 			self.pdc = None
# 			self.dc = None
# 			self.drg = error_drg
# 			self.pccl = None

# 		else:
# 			## dagger asterisk swapping
# 			self = dagger_asterisk(self, libs)

# 			# MDC
# 			self.mdc = determine_mdc(self, libs)

# 			# PDC
# 			self.pdc = determine_pdc(self, libs)

# 			# DC and DRG
# 			self.drg, self.pccl, new_error = determine_drg(self, libs)
# 			if self.error_code is None and new_error is not None:
# 				self.error_code = new_error

			
# def drg_seeker(patient_dict, libs):
# 	"""Produce the pertinent classifications per inpatient case"""
# 	try:
# 		patient = Patient(patient_dict, libs)
# 		result = {
# 			'mdc': patient.mdc,
# 			'pdc': patient.pdc,
# 			'dc': patient.dc,
# 			'pccl': patient.pccl,
# 			'drg': patient.drg
# 		}
# 		return result
# 	except:
# 		# print failure message
# 		print(f"""Classification failed with patient:
# 			PDX: {patient_dict['pdx']}
# 			SDX: {', '.join([patient_dict[p] for p in list(patient_dict.keys()) if 'sdx' in p and patient_dict[p] is not None])}
# 			Procedures: {', '.join([patient_dict[p] for p in list(patient_dict.keys()) if 'proc' in p and patient_dict[p] is not None])}
# 			Age: {patient_dict['patage']}
# 			Sex: {patient_dict['patsex']}
# 			Date of admission:  {patient_dict['date_adm']}
# 			Date of discharge:  {patient_dict['date_dis']}
# 			Discharge: {patient_dict['discharge']}
# 			""")
# 		if 'series' in patient_dict:
# 			print(f"This was claims series {patient_dict['series']}")
# 		return None
### END OF seeker.py

# Initialize StringIO object to capture print statements
statements_io = io.StringIO()

# Redirect print statements to the StringIO object
sys.stdout = statements_io

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
            # 'dc': patient.dc,
            'pccl': patient.pccl,
            'drg': patient.drg,
            'error_code': patient.error_code,
            'warning_code': patient.warning_code
        }
        
        return pd.Series(result)
    
    except Exception as e:
        # Log the error and row information for debugging
        print(f"Error processing patient with id_series {row['id_series']}: {e}")
        
        # Optionally, you can log more information such as row content or traceback
        traceback.print_exc()  # Print the full stack trace for more details
        
        # Return None or default values for the error case
        return pd.Series({
            'mdc': None,
            'pdc': None,
            # 'dc': None,
            'pccl': None,
            'drg': None,
            'error_code': None,
            'warning_code': None
        })

# Apply the Patient class directly to each row using swifter
pandas_df[['mdc', 'pdc', 
        #    'dc', 
           'pccl', 'drg', 'error_code', 'warning_code']] = pandas_df.swifter.apply(
    lambda row: process_patient(row, libs),
    axis=1
)

# Store the result in output to be retrieved by R
output = pandas_df.rename(columns={'drg': 'py_drg'})

# Define the desired column order
desired_columns = [
    'id_series', 'mdc', 'pdc', 
    # 'dc', 
    'pccl', 'py_drg', 'error_code', 
    'warning_code'
]

# Reorder the DataFrame and drop any columns not in the desired list
output = output[desired_columns]

# # Define the renaming mapping
# rename_mapping = {
#     'patage': 'pat_age',
#     'patsex': 'pat_sex',
#     'birthweight': 'pat_bwt',
#     'discharge': 'clin_discharge',
#     'icd9_list': 'clin_rvs'
# }

# # Rename the columns
# output = output.rename(columns=rename_mapping)

# Capture the print statements
statements = statements_io.getvalue()

# Reset the stdout to default
sys.stdout = sys.__stdout__

# Return both the output and the captured print statements
output, statements

