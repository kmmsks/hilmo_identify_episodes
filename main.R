
# Information -----------------------------------------------------------------
# This script contains methods for preparing the Finnish Care Register for Health Care
# and the Register of Primary Health Care visits for analysis by identifying partly
# overlapping episodes from the registers.
#
# This script comes with synthetic data for experimenting the functioning of the scripts.
#
# This script supplements this paper: 
#
# For general introduction, see https://github.com/kmmsks/hilmo_identify_episodes
#
# This process has the following steps:
#
# 0. preprocessing: 
#     Real data: Preparation of the real data for the actual processing,
#       including: 
#           reading the data, controlling for changes over time in variable names,
#           variable classifications, diagnostic classifications, excluding non-valid
#           entries, selecting treatment types andmedical specialties of interest etc. 
#       At this stage, the data will be split into "longitudinal" chunks, 
#           meaning that each individuals' all data is saved into a singe file 
#           (instead of having data saved in annual files).
#     Synthetic data: Alternatively, create synthetic fake data to experiment with.
#     
# 1. Processing: Identification of the partly overlapping treatment episodes and
#     aggregation of the ovarlaping register entries into single rows with desired
#     behaviours. Each chunk will be processed separately, and processed chunks 
#     will be saved.
#
# 2. Post-processing: supplementary functionality to recognize each individuals'
#     first dates with certain diagnoses from the processed data and include 
#     an optional minimum age for each diagnostic category of interest.
#
# 3. Look at the data. At this stage, the the processed chunks will be red and 
#     combined for inspection.
#

## Libraries -------------------------------------------------------------------
library(data.table)
library(fst)
library(writexl)
library(bit64)
library(stringr)
library(here)
library(haven)
library(magrittr)

## General functions -----------------------------------------------------------
# This file contains functions for creating directory tree for the processed data,
# to synthesize the example dataset and helper functions.

source(here("R", "_general_funs.R"), encoding = "UTF-8")

# A. Longitudinal analysis ----------------------------------------------------

# 0. Preprocessing of the registers -------------------------------------------

# when using real data, go through the file R/0a_preparation_settings_dgs.R
# After controlling for setting values, source the following:

#source(here("R", '0_preparation.R'), encoding = 'UTF-8')


## Synthetic data --------------------------------------------------------------

# To demonstrate the actual process of identifying treatment episodes form the
# partly overlapping data, synthetic data are created. 

# See readme for optional arguments

start_year <- 2015
end_year <- 2020

dat_synthetic <- synthetize_data(start_year = start_year, end_year = end_year)


# 1. Processing ----------------------------------------------------------------
# IDENTIFY episodes in longitudinal or annual preprocessed data

# process_data function controls the actual identification of treatment episodses.

# add_days controls the number of full calendar days that need to be spend
#   outside the hospital before a new treatment period may start.
#   The most common values are  0 and 1, and they are used in the this example.

# See the readme file for details and optional arguments.

source(here("R", "1_processing_fun.R"), encoding = "UTF-8")

process_data(add_days = 0, start_year = start_year, end_year = end_year)
process_data(add_days = 1, start_year = start_year, end_year = end_year)


# 2. first dates ---------------------------------------------------------------

# For each individual in the data, the incident date of each diagnosis of interest is collected.

# This process is done separately for each datasets, ie. for different values of the add_days

# The script saves the results and returns two datasets:
# - first_dates_on_f: first dates of different diagnoses when overnight stay is not required for an inpatent episode
# - first_dates_on_t: first dates of different diagnoses when overnight stay is required for an inpatent episode

# Define diagnoses of interest and minimum age for each diagnosis
source(here("R", '2a_first_dates_set_diagnoses.R'), encoding = 'UTF-8')

# Birthdays are needed for age calculation. 
# Here, birthdays are synthesized in the @r_data() -function

birthdays <- data.table(shnro = dat_synthetic[,unique(shnro)], 
                        birthday = sample(seq(as.IDate("1950/01/01"), as.IDate("2005/01/01"), by="day"), dat_synthetic[,uniqueN(shnro)]))


source(here("R", '2_first_dates_fun.R'), encoding = 'UTF-8')

get_first_dates(add_days = 0, start_year = start_year, end_year = end_year)
get_first_dates(add_days = 1, start_year = start_year, end_year = end_year)


## non-processed data ----------------------------------------------------------
#Here the first dates are collected from data where treatment episodes are not identified. 
# This is the comparison dataset.

source(here("R", '2c_first_dates_nonprocessed.R'), encoding = 'UTF-8')

get_first_dates_nonprocessed_data(start_year = start_year, end_year = end_year)


# 3. Look at the data ----------------------------------------------------------

# locations of the datasets:
l <- TRUE
dirs <-  list(pre = create_dirs_preprocess(start_year = start_year, end_year = end_year, longitudinal = l),
       ad0 = c(create_dirs_postprocess(add_days = 0, start_year = start_year, end_year = end_year, longitudinal = l),
               create_dirs_first_dates(add_days = 0, start_year = start_year, end_year = end_year, longitudinal = l)),
       ad1 = c(create_dirs_postprocess(add_days = 1, start_year = start_year, end_year = end_year, longitudinal = l),
               create_dirs_first_dates(add_days = 1, start_year = start_year, end_year = end_year, longitudinal = l))
)

# the starting point:
dat_synthetic

# processed data:

dat_processed <- list(
  ad0 = read_files_from(dirs$ad0$post),
  ad1 = read_files_from(dirs$ad1$post)
)

# first dates
dat_first_dates <- list(
  ad0_on_f = fread(file.path(dirs$ad0$first_dates_mh, "1_full_data_overnight_false.csv")),
  ad0_on_t = fread(file.path(dirs$ad0$first_dates_mh, "1_full_data_overnight_true.csv")),
  ad1_on_f = fread(file.path(dirs$ad0$first_dates_mh, "1_full_data_overnight_false.csv")),
  ad1_on_t = fread(file.path(dirs$ad0$first_dates_mh, "1_full_data_overnight_true.csv"))
)

# 4. Subsetting ----------------------------------------------------------------
## Relevant psychiatric diagnoses:
dat_processed$ad1[, dg_psy_all_relevet := paste(na.omit(dg_psy), dg_avo, sep = "_") %>% str_remove_all("_NA") ]

# For example, using model 4, secondary outpatient and primary care appointments
# each individuals' each visit: date and diagnoses:

dat_processed$ad1[overnight_psy == FALSE & (psy == TRUE | primary_care == TRUE), .(shnro, date = tulopvm %>% as.IDate(), dg_psy_all_relevet)]

# Same but the latest appointments only
dat_processed$ad1[overnight_psy == FALSE & (psy == TRUE | primary_care == TRUE)][, date_max := max(lahtopvm), by = shnro][lahtopvm == date_max][
  , .(shnro, date = tulopvm %>% as.IDate(), dg_psy_all_relevet)]

# B. Annual analysis -----------------------------------------------------------

source(here("R", "_general_funs.R"), encoding = "UTF-8")

start_year <- 2015
end_year <- 2020

source(here("R", "_general_funs.R"), encoding = "UTF-8")
source(here("R", "1_processing_fun.R"), encoding = "UTF-8")
source(here("R", "1_processing_fun.R"), encoding = "UTF-8")
source(here("R", '2a_first_dates_set_diagnoses.R'), encoding = 'UTF-8')
source(here("R", '2_first_dates_fun.R'), encoding = 'UTF-8')
source(here("R", '2c_first_dates_nonprocessed.R'), encoding = 'UTF-8')

birthdays <- data.table(shnro = dat_synthetic[,unique(shnro)], 
                        birthday = sample(seq(as.IDate("1950/01/01"), as.IDate("2005/01/01"), by="day"), dat_synthetic[,uniqueN(shnro)]))

dat_synthetic <- synthetize_data(start_year = start_year, end_year = end_year, longitudinal = FALSE)

process_data(add_days = 0, start_year = start_year, end_year = end_year, longitudinal = FALSE)
process_data(add_days = 1, start_year = start_year, end_year = end_year, longitudinal = FALSE)

get_first_dates(add_days = 0, start_year = start_year, end_year = end_year, longitudinal = FALSE)
get_first_dates(add_days = 1, start_year = start_year, end_year = end_year, longitudinal = FALSE)

# Look
l <- FALSE
dirs <-  list(pre = create_dirs_preprocess(start_year = start_year, end_year = end_year, longitudinal = l),
              ad0 = c(create_dirs_postprocess(add_days = 0, start_year = start_year, end_year = end_year, longitudinal = l),
                      create_dirs_first_dates(add_days = 0, start_year = start_year, end_year = end_year, longitudinal = l)),
              ad1 = c(create_dirs_postprocess(add_days = 1, start_year = start_year, end_year = end_year, longitudinal = l),
                      create_dirs_first_dates(add_days = 1, start_year = start_year, end_year = end_year, longitudinal = l))
)

dat_processed <- list(
  ad0 = read_files_from(dirs$ad0$post, longitudinal = FALSE),
  ad1 = read_files_from(dirs$ad1$post, longitudinal = FALSE)
)

years <- seq(start_year, end_year)
names(years) <- paste0("year_", years)


dat_first_dates <- lapply(years, function(y) list(
  ad0_on_f = fread(file.path(dirs$ad0$first_dates_mh, y, "1_full_data_overnight_false.csv")),
  ad0_on_t = fread(file.path(dirs$ad0$first_dates_mh, y, "1_full_data_overnight_true.csv")),
  ad1_on_f = fread(file.path(dirs$ad0$first_dates_mh, y, "1_full_data_overnight_false.csv")),
  ad1_on_t = fread(file.path(dirs$ad0$first_dates_mh, y, "1_full_data_overnight_true.csv"))
))


