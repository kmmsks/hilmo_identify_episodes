#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#
#
library(data.table)
library(here)
library(fst)

# This step depends on the form of data you have.

# At least check column names and data types.
# Turn dates to integers.

# In this example, the data is divided to five sub parts.
# Save the parts.

# Read the data ----------------------------------------------------------------------------------------------------------

files_list <- list.files(here('data_raw_fake'))

lst <- lapply(here('data_raw_fake', files_list), fread)
dat <- rbindlist(lst)

rm(files_list,lst)
gc()


# modify variables -------------------------------------------------------------------------------------------------------

dat[, `:=`(TUPVA = as.integer(as.IDate(TUPVA, '%d-%b-%y')),
           LPVM  = as.integer(as.IDate(LPVM, '%d-%b-%y'))
)]

dat[, dg := paste(DG1, DG2, sep = '_')]
dat[, `:=`(DG1 = NULL,
           DG2 = NULL)]


# Divide data in parts by person (instead of year) -----------------------------------------------------------------------

dat[, idn := .GRP, by = shnro]

n_parts <- 5


dat[, part := cut(
  idn,
  breaks = quantile(idn, probs = 0:n_parts / n_parts),
  labels = 1:n_parts,
  right = FALSE,
  include.lowest = TRUE
)]

# save data parts --------------------------------------------------------------------------------------------------------

dir.create(here('data_temp'))
dir.create(here('data_temp', 'parts_raw'))

dat[, write_fst(.SD, here('data_temp', 'parts_raw', paste0(part, '.fst')), compress = 100), by = part]

rm(dat, n_parts)
#gc()

# end
