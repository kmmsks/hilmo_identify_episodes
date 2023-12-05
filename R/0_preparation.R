
# Preparing register data for the actual processing ---------------------------
#
# This script controls the process. Several detailed decisions must be made for 
# selecting only the desired medical contacts and for excluding non-valid entries.
# Go through sub-scripts carefully.
#
# In real use case, go through all of the following sub-scripts:
# 0a_preparation_settings_dgs.R
#   - Define time range of interest in years.
#   - Define variables of interest and date formats. 
#   - Set values to subset the contact types of interest.
#
# 0a_preparation_funs.R
#   - Read functions: 
#       In case you cannot rename the raw files or organize the raw data sub-folders, 
#       reading the data might be a bit laborious. These read functions represent a real-world example.
#   - Preparation functions:
#       + process_dgs_1996_2017 and process_dgs_2018_: format of diagnosis data 
#         is in slightly different formats
#       + process_before_1996: years 1975-1995 are divided into several files, 
#         which need no be handled separately, but this function contains the 
#         parts that can be pre-processed in a similar way (after varying pre-pre-processing)
#       + process_avohilmo: primary care data. Harmonize column names, exclude non-valid rows. 
#   - Convert ICD-8-9-10 diagnoses:
#         Conversion of selected ICD-8-9 codes into equivalent ICD-10.
#   - Outpatient report: statistics about secondary care outpatient appointments
#         within inpatient episodes.

# Next, the pre-processing is in the following scripts.
# Each dataset will be read and processed. The processed data will be saved into
# "longitudinal" chunks, meaning that each individual's all data will be saved
# into the same file, using the append functionality of the fwrite function.

# 0b_preparation_before_1996.R:
#   - These data contain only inpatient care and less variables than data since 1996.
#     Hence, this is much more simple than pre-processing newer data. 
#     Be aware of varying date formats and erroneous values, however. 
#
# 0b_preparation_1996_2020.R:
#   - This script first reads data and then sources the pre-processing files:
#     + 0c_preparation_source_FCRHC.R
#       Secondary care inpatient and outpatient data
#     + 0c_preparation_source_RPHC.R
#       Primary care data
#   - Finally, longitudinal chunks will be saved by appending.

source(here("scripts", "R", "0a_preparation_settings_dgs.R"), encoding = "UTF-8")

source(here("scripts", 'R', "0a_preparation_funs.R"), encoding = 'UTF-8')


# Create the directory tree --------------------------------------------------------
# See README

settings$data_folder_name <- 'data_main'
dirs <- c( dirs0, create_dirs_preprocess(data_folder_name = settings$data_folder_name))


# 0 PRE-PROCESS registers, save in longitudinal format -------------------------

# initially data are in sub-parts based on years.
# After pre-processing, each individual's all data will be put in a single file, here called longitudinal format.
# Individuals are grouped based on the first character of the ID-variable (here called `shnro`).
# In the next step, each group will be processed separately for computing performance reasons.

## Old registers, before 1996 --------------------------------------------------

settings$old_to_longitudinal <-  TRUE

### NOTE appending files 
# remove old files if rerunning from beginning
list.files(dirs$pre, full.names = T) %>% 
  grep("_poisto",., value = TRUE) #%>% file.remove() 

source(here('R', '0b_preparation_before_1996.R'), encoding = 'UTF-8')


## Hilmo and Avohilmo 1996 -> -------------------------------------------------
settings$to_longitudinal <- TRUE

settings$avo_only_psy_dgs <- TRUE  
# TRUE: Include only rows with ICD-10 F, ICPC-2 P, or self harm (ICD-10) diagnoses
# FALSE: everything

### NOTE appending files 
# remove old files if rerunning from beginning
list.files(dirs$pre, full.names = T) %>% 
  grep("_inpat|_outpat|_avo",., value = TRUE) #%>% file.remove() 

source(here('R', '0b_preparation_1996_2020.R'), encoding = 'UTF-8')
