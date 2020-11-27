#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
#
# Script purpose:
# To identify hospital admissions, discharges and discharge diagnoses from the 
# Finnish Care Register for Health Care ("Hilmo" register)
#
# In this main script, control the setting for the aggregations
#
# Version v1.0.1
# Author: Kimmo Suokas


# Load packages ---------------------------------------------------------------------------------------------------------

library(data.table)
library(fst)
library(writexl)
library(here)

# Aggregation rules -----------------------------------------------------------------------------------------------------

# Data locations --------------------------------------------------------------------------------------------------------
# may locate within or outside the project folder

dir <- list(
  data_parts_in = here('data_temp', 'parts_raw'), 
  old_registers = here() # location of data sets with years before 1996, no example currently provided.
)


# Time range of the data with current data format (between 1996 and 2018) --------------------------------------------
# Note some major changes in the data definitions (at least in PALA) in 2019

# fake data 2010 -2014

max_year <- 2014 
min_year <- 2010 # no earlier than 1996


# data structure was different before 1996. Hence, older registers have separate method below.


# Outpatient data: evaluated starting from this year 
# (not relevant for inpatient care only):

outpatient_start_year <- 2007

# THL says data is relevant after 2006, here 2007 is the beginning of any analysis


# Days between periods ------------------------------------------------------------------------------------------------
#
# Minimum of full calender days required between two hospital treatment periods:
# 0 : a new period may start the next day after the previous one
# 1 : there must be one full calender day between two treatment periods
#     If less, Hilmo entries are considered to belong to a single period (due to unit transfer etc.)

add_days <- 1


# PALA: Register entry types defining treatments types of interest ------------------------------------------------------
#
# See Hilmo manuals for details

PALA_inpatient <- c(1)

PALA_outpatient <- 'PALA==83 | PALA>90'

# Define specialties of interest (psychiatry) ---------------------------------------------------------------------------
# See erikoisala EA, in Hilmo manuals. Notice changes in the coding between years.

specialties_of_interest <-  '70|70F|70X|70Z|74|75|75X'  # psychiatry

# write_fst compression  ------------------------------------------------------------------------------------------------
  # 0 for speed, 100 for reducing file sizes, default 50. 

compression <- 100



# intermediate variables, no need to change ------------------------------------------------------------------------------

# Number of  data parts to be processed, 1996 and later 
# 1 to n parts possible, persons' all entries should be in the same part
n_parts <- length(list.files(dir$data_parts_in))

outpatient_start_date <-  as.integer(as.IDate(paste0(outpatient_start_year, '-01-01')))

# // settings ready


# INPATIENT TREATMENT EPISODES 1996--2018 --- ----------------------------------------------------------------------------


# Run aggregation

source(here('R', '110_run_inpatient_1996_2018.R'))

# Output:
  # data_processed -> 1_inpatient_episodes -> add_days_[add_days] -> data_inpatient_[years_range].fst"
    # full aggregated data
  # data_processed -> 1_inpatient_episodes -> add_days_1 -> parts_years_[years_range] -> [1 - n_parts].fst
      # aggregated data in parts (for outpatient aggregation)
  # data_temp -> prepared_parts -> add_days_[add_days]_years_[years_range] -> [1 - n_parts].fst"
      # prepared, but not aggregated data (needed for outpatient aggregation)
  ## data_processed -> 1_inpatient_episodes -> preparation_description 
      # -> preparation_description__add_days_[add_days]_[years_range].xlsx
      # description of invalid rows etc.


# INPATIENT AND OUTPATIENT EPISODES --------------------------------------------------------------------------------------

# Run aggregation

source(here('R', '130_run_in_and_outpatient.R'))

# Input: data_processed and data temp, according to settings of current session.

# Output: 
  # data_processed -> 2_in_and_outpatient_episodes -> -> add_days_[add_days] -> data_episodes_[years_range].fst
  # and description_episodes_[years_range].xlsx



# OLD POISTO- and HILMO- REGISTERS (inpatient only), 1969 - 1995 ---------------------------------------------------------


# Input:
# input from dir$old_registers

# Example data not currently provided

#source(here('R', '120_run_inpatient_aggregation_old_registers.R')) # Go through settings in this file in detail.


# Output
# Same directories as in years 1996 and after


# COMBINE INPATIENT EPISODES ----------------------------------------------------------------------
dir$d2_aggregated_all <-  here('data_processed', '1_inpatient_episodes', paste('add_days', add_days, sep = '_'))

source(here('R', '122_combine_inpatient_all_years.R'))




# GET SOME PERSON-LEVEL DATA OUT OF THE PROCESSED DATASETS ---------------------------------------

# count episodes and time spent in hospital -------------------------------------------------------

# if only overnight episodes are considered as inpatient treatments:
  # get number of psychiatric inpatient episodes by person, all years included:

dat_all_inpatient[overnight_psy == TRUE, .N, by = shnro]

# get number of psychiatric outpatient episodes by person, starting from the year outpatient_start_year

dat_episodes[psy == TRUE & overnight_psy == FALSE, .N, by = shnro]

# get days spent in hospital by person

dat_all_inpatient[, .(days_hospitalized = lahtopvm - tulopvm), by = shnro][, .(days_hospitalized = sum(days_hospitalized)), by = shnro][]

# get days in psychiatric inpatient care by person
dat_all_inpatient[psy == TRUE, .(days_hospitalized = lahtopvm - tulopvm), by = shnro][, .(days_hospitalized = sum(days_hospitalized)), by = shnro][]

# // end