#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#
## Function definition

# create directory tree for processed data-----------------------------------------------------

create_dirs_inpatient <- function(){
  dir.create(here('data_processed'))
  dir.create(here('data_processed', '1_inpatient_episodes'))
  dir.create(here('data_processed', '1_inpatient_episodes', paste('add_days', add_days, sep = '_')))

  dir.create(here('data_processed', '1_inpatient_episodes', paste('add_days', add_days, sep = '_'),
                  paste('parts_years', min_year, max_year, sep = '_')))
  
  dir.create(here('data_temp', 'prepared_parts'))
  dir.create(here('data_temp', 'prepared_parts', paste('add_days', add_days, 'years', min_year, max_year, sep = '_')))
  
  dir.create(here('data_processed', '1_inpatient_episodes', 'preparation_description'))

  new_dir <- list()
  new_dir[['d2_aggregated_all']] <- here('data_processed', '1_inpatient_episodes', paste('add_days', add_days, sep = '_'))
  new_dir[['d2_aggregated_parts']] <- here('data_processed', '1_inpatient_episodes', paste('add_days', add_days, sep = '_'),
                                       paste('parts_years', min_year, max_year, sep = '_'))
  new_dir[['d0_prepared_parts']] <- here('data_temp', 'prepared_parts', paste('add_days', add_days, 'years', min_year, max_year, sep = '_'))
  
  new_dir[['preparation_description']] <- here('data_processed', '1_inpatient_episodes', 'preparation_description')
  
  new_dir
  }

create_dirs_in_and_outpatient <- function() {
  dir.create(here('data_processed', '2_in_and_outpatient_episodes'))
  dir.create(here('data_processed', '2_in_and_outpatient_episodes', paste('add_days', add_days, sep = '_')))
  
  new_dir <- list()
  new_dir[['d3_episodes']] <- here('data_processed', '2_in_and_outpatient_episodes', paste('add_days', add_days, sep = '_'))
  
  new_dir
}
