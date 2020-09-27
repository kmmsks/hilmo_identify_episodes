
# create directory tree for processed data

create_dirs_periods <- function(){
  dir.create(here('data_processed'))
  dir.create(here('data_processed', '1_inpatient_periods'))
  dir.create(here('data_processed', '1_inpatient_periods', paste('add_days', add_days, sep = '_')))

  dir.create(here('data_processed', '1_inpatient_periods', paste('add_days', add_days, sep = '_'),
                  paste('parts_years', min_year, max_year, sep = '_')))
  
  dir.create(here('data_temp', 'prepared_parts'))
  dir.create(here('data_temp', 'prepared_parts', paste('add_days', add_days, 'years', min_year, max_year, sep = '_')))
  
  dir.create(here('data_processed', '1_inpatient_periods', 'preparation_description'))

  dir[['d2_aggregated_all']] <- here('data_processed', '1_inpatient_periods', paste('add_days', add_days, sep = '_'))
  dir[['d2_aggregated_parts']] <- here('data_processed', '1_inpatient_periods', paste('add_days', add_days, sep = '_'),
                                       paste('parts_years', min_year, max_year, sep = '_'))
  dir[['d0_prepared_parts']] <- here('data_temp', 'prepared_parts', paste('add_days', add_days, 'years', min_year, max_year, sep = '_'))
  
  dir[['preparation_description']] <- here('data_processed', '1_inpatient_periods', 'preparation_description')
  
  dir
  }


