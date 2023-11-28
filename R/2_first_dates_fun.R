

#' Find first date of seleceted diagnoses
#'
#' This function finds the first date of selected diagnoses for each individual in
#' the data. Optionally, minimum age for each diagnosis can be specified in 2a_first_dates_set_diagnoses.R
#' 
#' This function controls a set of sub-functions from 2b_first_dates_subfuns.R
#' - These functions do the job.
#'
#' This function does the following:
#' 1. create dirs to refer to the right data and to create location for the results
#' 2. Read chunk identifiers. The processed data with identified episodes are in chunks. 
#' 3. Loop throug chunks and do the following:
#'   3.1 inpatient and outpatient data
#'   3.2 Primary care appointments, read, find first dates
#'  3.3 bind results
#'  3.4  save results, each treatment type separately
#'  3.5 combine treatment types to get the first dates in any setting
#'  3.6 save final results
#'  
#'  
#' @param add_days  Numeric, defines how many full calendar days are required between inpatient episodes. Commonly 0 or 1.
#' @param start_year Numeric, start year of the data to be processed. May be used for subsetting longitudinal data.
#' @param end_year Numeric, end year of the data to be processed. May be used for subsetting longitudinal data.
#'
#' @return Nothing. Data are processed in chunks and each chunk is saved as CSV file in the folder defined in 
#'          the create_dirs_first_dates function in file R/_general_functions.R. Location of the data is printed as a message
#' @export
#'
#' @examples
get_first_dates <- function(add_days, start_year, end_year) {
  # The sub-functions needed in this processt:
  source(here("scripts", "R", '2b_first_dates_subfuns.R'), encoding = 'UTF-8')
  
  # 1. create dirs ----
  dirs <- c(create_dirs_preprocess(start_year = start_year, end_year = end_year, longitudinal = T),
            create_dirs_postprocess(add_days = add_days, start_year = start_year, end_year = end_year, longitudinal = T),
            create_dirs_first_dates(add_days = add_days, start_year = start_year, end_year = end_year, longitudinal = T))
  
  # 2. Read chunk identifiers ----
  # - These are the pre-processed files that contains all identified treatment episodes that 
  #   are used here. Take the first character of the file name that serves as identifier for chunks.
  chunks_in <- dirs$post %>% list.files() %>% substr(start = 1, stop = 1) %>% unique()
  
  # 3. Loop through chunks ---- 
  # - These list are appended when the cunks are processed
  # - on refers to OverNight, whether an inpatient episode is defined to last atleast over one night or not.
  
  inpatient_on_true <- list()
  inpatient_on_false <- list()
  outpatient_on_true <- list()
  outpatient_on_false <- list()
  primary_care <- list()
  
  for( i in chunks_in){
    # 3.1 inpatient and outpatient data ----
    
    # read
    hilmo_1 <- fread(file.path(dirs$post, paste0(i, '_inpatient_outpatient.csv')))[, shnro := as.character(shnro)]
    hilmo_1[birthdays, on =  'shnro', c('birthday'):= .(i.birthday)]
    
    ## find the first dates ----
    inpatient_on_true[[i]]  <- hilmo_1[overnight_psy == T] %>%  find_first_date(dg_field = 'dg_psy')
    inpatient_on_false[[i]] <- hilmo_1[inpat_psy == T] %>% find_first_date(dg_field = 'dg_psy')
    
    # multiple outpatient appointments may take place on a single day. All appointments are included
    outpatient_on_true[[i]] <- hilmo_1[overnight_psy == F & psy == T, 
                                       .(tulopvm = tulopvm %>% min(), 
                                        lahtopvm = lahtopvm %>% min(), 
                                        birthday = birthday %>% min(),
                                        dg_psy = dg_psy %>% paste(collapse = "_")), by = .(shnro, event)] %>% 
      find_first_date(dg_field = 'dg_psy', tulopvm_field = 'tulopvm')
    
    outpatient_on_false[[i]] <- hilmo_1[inpat_psy == F & psy == T, 
                                        .(tulopvm = tulopvm %>% min(), 
                                          lahtopvm = lahtopvm %>% min(), 
                                          birthday = birthday %>% min(),
                                          dg_psy = dg_psy %>% paste(collapse = "_")), by = .(shnro, event)] %>%
      find_first_date(dg_field = 'dg_psy', tulopvm_field = 'tulopvm')
    
    # 3.2 Primary care appointments, read, find first dates ----
    avo_1 <- fread(file.path(dirs$post, paste0(i, '_primary_care.csv')))[, shnro := as.character(shnro)]
    avo_1[birthdays, on =  'shnro', c('birthday'):= .(i.birthday)]
    
    primary_care[[i]] <- avo_1 %>%  find_first_date(dg_field = 'dg_avo', tulopvm_field = 'tulopvm')
  }  
  
  # 3.3 bind results ----
  # - All previously created result list and combine them into a singe list
  lst <- list(
    inpatient_overnight_true   = inpatient_on_true   %>% rbindlist(),
    inpatient_overnight_false  = inpatient_on_false  %>% rbindlist(),
    outpatient_overnight_true  = outpatient_on_true  %>% rbindlist(),
    outpatient_overnight_false = outpatient_on_false %>% rbindlist(), 
    primary_care = primary_care  %>% rbindlist()
  )
  
  # 3.4  save results, each treatment type separately ----
  #   - save first dates by treatment type and by overnight T/F
  lapply(lst %>% names(), 
         function(i) lst[[i]] %>% combine_first_dates(one_treatment_type_only = T) %>% 
           fwrite(file = file.path(dirs$first_dates_mh, paste0(i,'.csv'))))
  
  # 3.5 combine treatment types to get the first dates in any setting ----
  # overnight T/F separately. avo is not affected by the overnight variable
  
  # overnight TRUE:
  d1 <- rbindlist(list(
    avo = lst$primary_care, 
    inpatient = lst$inpatient_overnight_true, 
    outpatient = lst$outpatient_overnight_true
  ), idcol = 'source', fill = TRUE)
  
  first_dates_on_t <- combine_first_dates(dat_in = d1)[]
  
  # 3.6 save final results ----
  first_dates_on_t %>% fwrite(file.path(dirs$first_dates_mh, '1_full_data_overnight_true.csv'))
  
  
  # overnight FALSE:
  d1 <- rbindlist(list(
    primary_care = lst$primary_care, 
    inpatient = lst$inpatient_overnight_false, 
    outpatient = lst$outpatient_overnight_false
  ), idcol = 'source', fill = TRUE)
  
  first_dates_on_f <- combine_first_dates(dat_in = d1)[]
  
  # save final results
  first_dates_on_f %>% fwrite(file.path(dirs$first_dates_mh, '1_full_data_overnight_false.csv'))
  
  # settings. Here the diagnoses and min ages are written into a xlsx.
  list(
    dg_info = dg_groups_w_min_ages
  ) %>% write_xlsx(file.path(dirs$first_dates_mh, paste0('settings_', Sys.time() %>% as.character() %>% gsub("\\:","-",.), '.xlsx')))
  
  rm(avo_1, d1, hilmo_1, inpatient_on_false, inpatient_on_true, outpatient_on_false, outpatient_on_true, primary_care, lst, chunks_in, i)
  gc()
  
  message(paste("Message: Done. Processed data saved at", dirs$first_dates_mh))
}
