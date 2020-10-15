#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#
#
# Aggregation of inpatient periods  ---------------------------------------------

# Source files -----------------------------------------------------------------
#
source(here('R', '011_functions_prepare_parts.R'))
source(here('R', '012_functions_aggregate_inpatient_periods.R'))
source(here('R', '013_functions_create_dirs.R'))


# Aggregate raw Hilmo data to inpatient treatment episode based on aggregation rules in 1_main_aggregation.R
#
# param part: data table part to process
# returns: list containing data prepared for the aggregation, the aggregated data part and 
#           description of preparation process (number of incorrecet entries in the raw data part etc) 
#
# details:
#   * 1. prepare_descripe_parts(d0): remove and report incorrect entries, select relevant entries, define psychiatry
#        Returns list: [1] data, [2]-[4] reports
#   * 2. source the actual aggregation,
#        input: d0, returns: d2, the aggregated data 
#
run_aggregate_inpatient <- function(part) {
  setwd(dir$data_parts_in)
  d0 <- read_fst(paste0(part, '.fst'), as.data.table = TRUE)  #
  
  prepared_part <- prepare_describe_parts(d0)
  # the data:
  d0 <- prepared_part[['d0']]
  # descriptions:
  part_n_vals          <- prepared_part[['n_vals']]
  part_n_of_valid_rows <- prepared_part[['n_of_valid_rows']]
  part_n_pala          <- prepared_part[['n_pala']]
  
  rm(prepared_part)
  
  # select inpatient data
  d1 <- d0[PALA %in% PALA_inpatient] ###
  
  
  # treatments that continue after the end of study period, have LPVM == NA.
  # For this aggregation script, set na to high value and back after aggregation (tech reason)
  
  # set LPVM to high, that's necessary for aggregation.
  d1[ILAJI == 2, LPVM := as.integer(as.IDate(paste(max(vuosi) + 1000, '12-31', sep = '-')))]
  
  
  # THE ACTUAL AGGREGATION
  d2 <- aggregate_inpatient_periods(d1)
  
  # lahtopvm max value is max year + 1000, representing treatments that continued after the end of
  # the study period. Hence, real value is NA.
  d2[lahtopvm == max(lahtopvm), lahtopvm := NA]
  
  # process
  
  # Create variable year (again), based on lahtopvm ---------------------------------
  d2[, vuosi := as.integer(format(as.IDate(lahtopvm), '%Y'))]
  
  # if treatment continues at the end of the data, lahtopvm is NA, year is max year
  d2[, episode_continues := FALSE]
  d2[is.na(lahtopvm), `:=`(vuosi = as.integer(max_year), episode_continues = TRUE)]
  

  # mark episodes that contain any inpatient period lasting over a night 
  d2[, overnight_all := FALSE]
  d2[lahtopvm > tulopvm, overnight_all := TRUE]
  
  # mark episodes that contain psychiatric inpatient period lasting over a night 
  d2[, overnight_psy := FALSE]
  d2[lahtopvm_psy_inpat > tulopvm_psy_inpat, overnight_psy := TRUE]
  
  
  # return
  list(
    prepared_part                    = d0,
    aggregated_part                  = d2,
    part_description_n_vals          = part_n_vals,
    part_description_n_of_valid_rows = part_n_of_valid_rows,
    part_description_n_pala          = part_n_pala
  )
}
 
# call
parts_out <- lapply(seq(1, n_parts), run_aggregate_inpatient)

# processed inpatient data
dat_inpatient <- rbindlist(sapply(parts_out, '[','aggregated_part'))



# create directories for prepared and fully processed data---------------------------------------------------------------

dir <- c(dir, create_dirs_inpatient()) # see 013_create_dirs.R


# save aggregated inpatient data ----------------------------------------------------------------------------------------
setwd(dir$d2_aggregated_all)
write_fst(dat_inpatient, paste0('data_inpatient_', min_year, '_', max_year, '.fst'), compress = compression)

# save parts
setwd(dir$d2_aggregated_parts)
for(i in seq_along(sapply(parts_out, '[', 'aggregated_part'))) {
  write_fst(
    sapply(parts_out, '[', 'aggregated_part')[[i]],
    path = paste(i, 'fst', sep = '.'),
    compress = compression
  )
}

# save prepared parts ---------------------------------------------------------------------------------------------------
setwd(dir$d0_prepared_parts)

for(i in seq_along(sapply(parts_out, '[', 'prepared_part'))) {
  write_fst(
    sapply(parts_out, '[', 'prepared_part')[[i]],
    path = paste(i, 'fst', sep = '.'),
    compress = compression
  )
}




# Descriptions of the preparation process -------------------------------------------------------------------------------

# Number of rows in raw data and step-wise preparation of the dataset containing valid rows only 
a <- sapply(parts_out, '[','part_description_n_vals')

# Number of special case among valid rows
b <- sapply(parts_out, '[','part_description_n_of_valid_rows')

# PALA distribution
c <- sapply(parts_out, '[','part_description_n_pala')

# process list to data.tables
description_inpatient <-
  list(
    descr_valid_rows = data.table(
      value = sapply(seq_along(a[[1]]),
                     function (i)
                       names(sapply(a, '[', i))[1]),
      n = sapply(seq_along(a[[1]]),
                 function (i)
                   Reduce('+', (sapply(
                     a, '[', i
                   ))))
      )[, delta := .(n - shift(n))],
    descr_cases_among_valid = data.table(
      value = sapply(seq_along(b[[1]]),
                     function (i)
                       names(sapply(b, '[', i))[1]),
      n = sapply(seq_along(b[[1]]),
                 function (i)
                   Reduce('+', (sapply(
                     b, '[', i
                   ))))
      ),
    descr_pala_all = rbindlist(sapply(c, '[', 1))[, lapply(.SD, sum, na.rm = TRUE), by = PALA],
    descr_pala_event_start = rbindlist(sapply(c, '[', 2))[, lapply(.SD, sum, na.rm = TRUE), by = PALA],
    rules = data.table(
      title = c('outpatient_start_year'),
      value = c(outpatient_start_year)
      ),
    read_me = data.table(
      sheet = c(
        'descr_valid_rows',
        'descr_cases_among_valid',
        'descr_pala_all',
        'descr_pala_event_start',
        'rules'
        ),
      info = c(
        'number of not valid rows excluded, starting from raw data',
        'number of specific cases among valid rows',
        'distribution of PALA values among all valid cases',
        'distribution of PALA values among all valid cases, starting from the event star year (relevant for outpatient data)',
        'rules used'
      )
    )
  )

# write descriptions
setwd(dir[["preparation_description"]])
write_xlsx(description_inpatient,
  path = paste0('preparation_description_', '_add_days_', add_days, '_', min_year, '_', max_year,'.xlsx')
  )

setwd(here())
rm(a,b,c, parts_out)
## end

