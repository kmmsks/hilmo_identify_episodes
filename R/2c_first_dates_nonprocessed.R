
get_first_dates_nonprocessed_data <- function(start_year, end_year, separate_files_for_old_registers = FALSE) {
  
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
    
    # variable dg contains all diagnoses, combined in pre processing
    inpat[[i]]  <- inpat_0[psy == T] %>%  find_first_date(dg_field = 'dg', tulopvm_field = 'tupva', lahtopvm_field = 'lpvm')
    
    outpat[[i]] <- outpat_0[psy == T] %>% find_first_date(dg_field = 'dg', tulopvm_field = 'tupva', lahtopvm_field = 'lpvm')
    
    primary_care[[i]] <- prim_care_0 %>%  find_first_date(dg_field = 'dg_avo', tulopvm_field = 'tupva', lahtopvm_field = 'lpvm')
    
  }
  
  
  lst <- list(
    inpatient  = inpat   %>% rbindlist(),
    outpatient = outpat  %>% rbindlist(),
    primary_care = primary_care  %>% rbindlist()
  )
  # save first dates by treatment type
  lapply(lst %>% names(), 
         function(i) lst[[i]] %>% combine_first_dates(one_treatment_type_only = T) %>% fwrite(file = file.path(dirs$first_dates_nonprocessed, paste0(i,'.csv'))))
  
  # combine treatment types to get the first dates in any setting
  
  d1 <- rbindlist(list(
    avo = lst$avo, 
    inpatient = lst$inpatient, 
    outpatient = lst$outpatient
  ), idcol = 'source', fill = TRUE)
  
  first_dates_raw <- combine_first_dates(dat_in = d1)
  
  first_dates_raw %>% fwrite(file.path(dirs$first_dates_nonprocessed, '1_full_data.csv'))
  
  
  # settings
  list(
    dg_info = dg_groups_w_min_ages
  ) %>% write_xlsx(file.path(dirs$first_dates_nonprocessed, paste0('settings_', Sys.time() %>% as.character() %>% gsub("\\:","-",.), '.xlsx')))

}
