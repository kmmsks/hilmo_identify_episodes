#' Find first date of selected diagnoses - non-processed data
#'
#' Very similar to 2b function, but here pre-processed data (without the actual processing of identifying treatment episodes) is used instead.
#'
#' This function finds the first date of selected diagnoses for each individual in
#' the data. Optionally, minimum age for each diagnosis can be specified in 2a_first_dates_set_diagnoses.R
#' 
#' This function controls a set of sub-functions from 2b_first_dates_subfuns.R
#' - These functions do the job.
#'
#' This function does the following:
#' 1. Creates dirs to refer to the right data and to create the location for the results
#' 2. Reads chunk identifiers. The pre-processed data is in chunks. 
#' 3. Loops through chunks and does the following:
#'   3.1 Inpatient and outpatient data
#'   3.2 Primary care appointments, reads, finds first dates
#'   3.3 Binds results
#'   3.4 Saves results, each treatment type separately
#'   3.5 Combines treatment types to get the first dates in any setting
#'   3.6 Saves final results
#'  
#'  
#' @param start_year Numeric, start year of the data to be processed. May be used for subsetting longitudinal data.
#' @param end_year Numeric, end year of the data to be processed. May be used for subsetting longitudinal data.
#' @param dg_age_in A data.table such as dg_groups_w_min_ages from 2a_first_dates_set_diagnoses.R or similar.
#'
#' @return Nothing. Data are processed in chunks and each chunk is saved as CSV file in the folder defined in 
#'          the create_dirs_first_dates function in file R/_general_functions.R. Location of the data is printed as a message
#' @export
#'
#' @examples

get_first_dates_nonprocessed_data <- function(start_year, end_year, separate_files_for_old_registers = FALSE, dg_age_in = dg_groups_w_min_ages) {
  
  # dirs create ----
  dirs <- c(create_dirs_preprocess(start_year = start_year, end_year = 2020), 
            create_dirs_first_dates(add_days = NA, start_year = start_year, end_year = 2020))
  
  ## RAW diagnoses ---------------------------------------------------------------
  inpat <- list()
  outpat <- list()
  primary_care <- list()
  
  chunks_in <- dirs$pre %>% list.files() %>% substr(start = 1, stop = 1) %>% unique()
  
  for( i in chunks_in){
    if(separate_files_for_old_registers == TRUE){
      inpat_0 <- rbindlist(list(
        fread(file.path(dirs$pre, paste0(i, '_inpatient.csv'))),
        fread(file.path(dirs$pre, paste0(i, '_before_1996.csv')))
      ), fill = TRUE)
    } else if(separate_files_for_old_registers == FALSE){
      inpat_0 <- fread(file.path(dirs$pre, paste0(i, '_inpatient.csv')))[, shnro := as.character(shnro)]
    } else {
      stop("separate_files_for_old_registers must be TRUE or FALSE. Old registers refer to data before 1996 which usually are provided in 
             separate files.")
    }
    
    outpat_0 <- fread(file.path(dirs$pre, paste0(i, '_outpatient.csv')))[, shnro := as.character(shnro)]
    
    prim_care_0 <- fread(file.path(dirs$pre, paste0(i, '_primary_care.csv')))[, shnro := as.character(shnro)]
    
    inpat_0[birthdays, on =  'shnro', c('birthday'):= .(i.birthday)]
    outpat_0[birthdays, on =  'shnro', c('birthday'):= .(i.birthday)]
    prim_care_0[birthdays, on =  'shnro', c('birthday'):= .(i.birthday)]
    
    # variable dg contains all diagnoses, combined in pre-processing
    inpat[[i]]  <- inpat_0[psy == T] %>%  find_first_date(dg_field = 'dg', tulopvm_field = 'tupva', lahtopvm_field = 'lpvm', dg_age = dg_age_in)
    
    outpat[[i]] <- outpat_0[psy == T] %>% find_first_date(dg_field = 'dg', tulopvm_field = 'tupva', lahtopvm_field = 'lpvm', dg_age = dg_age_in)
    
    primary_care[[i]] <- prim_care_0 %>%  find_first_date(dg_field = 'dg_avo', tulopvm_field = 'tupva', lahtopvm_field = 'lpvm', dg_age = dg_age_in)
    
  }
  
  
  lst <- list(
    inpatient  = inpat   %>% rbindlist(),
    outpatient = outpat  %>% rbindlist(),
    primary_care = primary_care  %>% rbindlist()
  )
  # save first dates by treatment type
  lapply(lst %>% names(), 
         function(i) lst[[i]] %>% combine_first_dates(one_treatment_type_only = T, dg_age = dg_age_in) %>% fwrite(file = file.path(dirs$first_dates_nonprocessed, paste0(i,'.csv'))))
  
  # combine treatment types to get the first dates in any setting
  
  d1 <- rbindlist(list(
    avo = lst$avo, 
    inpatient = lst$inpatient, 
    outpatient = lst$outpatient
  ), idcol = 'source', fill = TRUE)
  
  first_dates_raw <- combine_first_dates(dat_in = d1, dg_age = dg_age_in)
  
  first_dates_raw %>% fwrite(file.path(dirs$first_dates_nonprocessed, '1_full_data.csv'))
  
  # settings
  list(
    dg_info = dg_groups_w_min_ages
  ) %>% write_xlsx(file.path(dirs$first_dates_nonprocessed, paste0('settings_', Sys.time() %>% as.character() %>% gsub("\\:","-",.), '.xlsx')))

  message(paste("Message: Done. Processed data saved at", dirs$first_dates_nonprocessed))
}
