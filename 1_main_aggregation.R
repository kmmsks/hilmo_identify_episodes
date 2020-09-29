#
#
# Aggregation of Hilmo entries // Hilmojen ketjutus
#

# Script purpose:
# To identify hospital admissions, discharges and discharge diagnoese from the 
# Finnish Care Register for Health Care ("Hilmo" register)
#
# In this main script, control the setting for the aggregations
#
# Author: Kimmo Suokas
#
# Version 0.1 beta 

# Load packages ---------------------------------------------------------------------------------------------------------

library(data.table)
library(fst)
library(writexl)
library(here)

# Aggregation rules -----------------------------------------------------------------------------------------------------

# Data locations -------------------------------------------------------------------------------------------------------
# may locate within or outside the project folder

dir <- list(
  data_parts_in = here('data_temp', 'parts_raw'), 
  old_registers = here(),
  avohilmo_in   = here()
)


# Time range of the data with current data (form 1996 and lateer) -------------------------------------------------------

# Inpatient data:
# fake data 2010 -2014

max_year <- 2014 
min_year <- 2010 # no earlier than 1996


# data structure was different before 1996. Hence, older registers have separate method below.


# Outpatient data: evaluated starting from this year 
# (not relevant for inpatient care only):

outpatient_start_year <- 2007
outpatient_start_date <-  as.integer(as.IDate(paste0(outpatient_start_year, '-01-01')))

# THL says data is relevant after 2006, here 2007 is the beginning of any analysis


# Days between periods ---------------------------------------------
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


# Number of  data parts to be processed, 1996 and later -----------------------------------------------------------------
  # 1 to n parts possible, persons' all entries should be in the same part

n_parts <- length(list.files(dir[["data_parts_in"]]))

# write_fst compression  ------------------------------------------------------------------------------------------------
  # 0 for speed, 100 for reducing file sizes, default 50. 

compression <- 100


# // settings ready



# INPATIENT TREATMENT EPISODES  ------------------------------------------------------------------------------------------


# Run aggregation

source(here('R', '110_run_inpatient_1996_and_after.R'))

# Output:
  # data_processed -> 1_inpatient_episodes -> add_days_[add_days] -> inpatient_[years_range].fst"
    # full aggregated data
  # data_processed -> 1_inpatient_episodes -> add_days_1 -> parts_years_1996_2017 -> [1 - n_parts].fst
      # aggregated data in parts (for outpatient aggregation)
  # data_temp -> prepared_parts -> add_days_[add_days]_years_[years_range] -> [1 - n_parts].fst"
      # prepared, but unaggregated data (for outpatient aggregation)
rm(dat_inpatient)
gc()



# INPATIENT AND OUTPATIENT EPISODES --------------------------------------------------------------------------------------

# Run aggregation

source(file.path(dir[["main"]], 'R', '130_run_outpatient_aggregation.R'))

rm(dat_episodes, episodes_description)
gc()

# Output: 
  # data_processed/2_in_and_outpatient_episodes/
  # description.xlsx

# Diagnosis fields
  # os.hoito T/F tapahtumassa osastohoitoa T/F
  # psy T/F tapahtumassa psykiatriaa
  # psy_os T/F tapahtumassa psy osastohoitoa

  #pkl_same_day T/F. T jos saman paivana yli 1 pkl-kaynti

  # dg_os: osaston viimeisin dg 
  # dg_psy_os: psy osastojakson viimeisin psykiatrialla annettu dg tai 
        #muun alan sairaalahoidon aikana psy-pkl dg

  # dg_os_psy_pkl_psy jos psy osastohoidon aikana psy pkl-kaynteja, niiden dg:t tassa 
    # yleensa jatetaan huomiotta
    # muuden alojen pkl-dg:t ovat dg_pkl

  # dg_pkl polikliiniset diagnoosit jaksolla
    # myos psykiatrian pkl-tapahtumien dg:t ovat dg.pkl:ssa, jos tapahtumassa ei ole osastohoitoa
    # osastohoidon aikana olleet pkl-kaynnit ovat tassa mukana
      # kun halutaan vain kotiutusdiagnoosit, nama jatetaan os_hoito == TRUE -riveilta huomiotta


# OLD POISTO- and HILMO- REGISTERS (inpatient only), 1996 - 1993 ---------------------------------------------------------

# Input: 
# input from dir[['old_registers']]


source(file.path(dir[["main"]], 'R', '120_run_inpatient_aggregation_old_registers.R'))

# Output
# Same directories as in years 1996 and after


# // end