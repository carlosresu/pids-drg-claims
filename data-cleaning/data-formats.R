# Define column types
character_cols <- c("PSEUDO_CLAIMSERIES", "PSEUDO_MEM_PIN", "HCI_PMCC_NO", "HCP_NO_LIST", "PRIMARY_ILLNESS", 
                    "SECONDARY_ILLNESS", paste0("ICDCODE", c(1:14, 16:170)), "ICCODED15", paste0("RVSCODE", 1:20), 
                    "DATE_ADM", "TIME_ADM", "DATE_DIS", "TIME_DIS", "DATE_REC", "DATE_REF", "CHKDT", 
                    "PAT_BDAY", "EXTRACTION_DATE")
integer_cols <- c("OUT_PATIENT", "EMERGENCY")
factor_cols <- c("PATIENT_TYPE", "ROOM_TYPE", "DEP_REL", "PATSEX", "MEMCAT_PARENT_DESC", "MEMCAT_CHILD_DESC", 
                 "MEMCAT_SUBCHILD_DESC", "DISPOSITION", "CLAIMS_STATUS")
numeric_cols <- c("PATAGE", "PAT_BWT_KG", "CLAIMS_PAID_AMT", "ACR_AMOUNT_ACTUAL")

covid_rvs <- c('C19T1', 'C19T2', 'C19T3', 'C19X1', 'C19X2', 'C19X3', 'C19FRP', 'C19IP1', 'C19IP2', 'C19IP3', 'C19IP4', 
               'C19PP1', 'C19PP2', 'C19PP3', 'C19PP4', 'MP01', 'IMP02', 'C19CI', 'C19H1', 'C19VIH', 'C19VID')