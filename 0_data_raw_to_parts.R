
library(data.table)
library(here)
library(fst)

# This step depends on the form of data you have.

# What you need to achieve, is the have minimum of the following columns in your data:

# 'shnro': character. person id. The name shnro is used by the Statistic Finland, it refers to a person, 
#                     not ID number, which may change (in relative rare occasions).
# vuosi: numeric. Year of the entry

# The following variables, details in the Hilmo manual. Note possible changes in the classifications between years.
# Desired data types:

# 'ILAJI' : numeric
# 'PALTU' : numeric
# 'PALA'  : numeric
# 'EA'    : character
# 'TUPVA' : as.integer(as.IDate(TUPVA, format = [FORMAT]))
# 'LPVM'  : as.integer(as.IDate(LPVM, format = [FORMAT]))

# Diagnoses: 
  # One register entry may contain multiple diagnoses. Names of the diagnosis varaibles vary between yaers.
  
# Combine diagnoses to one character variable called 'dg'


# If your data is in one file of reasonable size, all you need to do is to check you have above mentioned columns 
# in the data and name it '1.csv'

# If your data is in form THL calls the database form, you need to combine the data from different files based on entry id.
# Variables are in diferent files and each year is in its own file.
# Use library(bit64) for long id numbers. 
#(Example not shwn currently)


# If you have the full register, you may need to process the data in smaller parts (chunks), due to performance reasons.
# Each part must contain all entries of a single person to have the chaining of treatments correctly.
# To determine the optimal part size is out of scope here. See what is enough on your machine.

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

dat[, part := cut(idn,
                  breaks = quantile(idn, probs = 0: n_parts/n_parts),
                  labels = 1:n_parts, right = FALSE,
                  include.lowest = TRUE
                  )]

# save data parts --------------------------------------------------------------------------------------------------------

dir.create(here('data_temp'))
dir.create(here('data_temp', 'parts_raw'))

dat[, write_fst(.SD, here('data_temp', 'parts_raw', paste0(part, '.fst')), compress = 100), by = part]

# clear environment
rm(list=ls())
gc()

# end
