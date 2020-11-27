#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Data Preparations
#
# Version v1.0.1
# Author: Kimmo Suokas
#
library(data.table)
library(here)
library(fst)

# This step depends on the form of data you have.

# At least check column names and data types, turn dates to integers.

# In this example, the data is divided to five sub parts, as the whole data can usually not be processed at once with normal PC.

# SET NUMBER OF SUB PARTS TO SPLIT THE DATA -----------------------------------------------------------------------------

n_parts <- 5


# SET LOCATION OF THE RAW DATA  ------------------------------------------------------------------------------------------

raw_data_location <- here('data_raw_fake')


# Read the data ----------------------------------------------------------------------------------------------------------

files_list <- list.files(raw_data_location, full.names = TRUE)

lst <- lapply(files_list, fread)
dat <- rbindlist(lst)

rm(files_list,lst)
gc()


# modify variables -------------------------------------------------------------------------------------------------------

# SET DATE TYPES

dat[, `:=`(TUPVA = as.integer(as.IDate(TUPVA, '%d-%b-%y')),
           LPVM  = as.integer(as.IDate(LPVM, '%d-%b-%y'))
)]

# SET DIAGNOSIS COLUMUN NAMES HERE
dat[, dg := paste(DG1, DG2, sep = '_')]
dat[, `:=`(DG1 = NULL,
           DG2 = NULL)]

# POSSIBLE OTHER TRANSFORMATIONS HERE:


# Divide data in parts by person (instead of year) -----------------------------------------------------------------------

dat[, idn := .GRP, by = shnro]

dat[, part := cut(
  idn,
  breaks = quantile(idn, probs = 0:n_parts / n_parts),
  labels = 1:n_parts,
  right = FALSE,
  include.lowest = TRUE
)]

# save data parts --------------------------------------------------------------------------------------------------------

# folder for data parts
dir.create(here('data_temp'))
dir.create(here('data_temp', 'parts_raw'))

dat[, write_fst(.SD, here('data_temp', 'parts_raw', paste0(part, '.fst')), compress = 100), by = part]

rm(dat, n_parts)
#gc()

# end
