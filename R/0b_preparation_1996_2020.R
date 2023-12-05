
# This script pre-processes Hilmo secondary care registers after 1996. 
# 1996-2017 is slightly different than 2018-2020.
# Raw data are in annual datasets. 

# This script loops through desired years, reads, pre-processes, and saves data in
# longitudinal format, meaning that each individual's 
# all data are saved into a singe file (instead of having data saved in annual files).
# Progress is reported for each year into report_-datasets, which are saved in
# xlsx, so the number of erroneous rows in every year will be recorded.

# The process goes as follows:
## read secondary care data 
## source pre-processing
## save reports
## source reading and pre-processing primary care data
## save reports
## Save to longitudinal
## Or save to annual format


# building these files to report the pre-processing process:
report_flow <- list()
report_variable <- list()
report_avo <- list()
# report_outpatient will be formed along the way


# The loop ---------------------------------------------------------------------

for (y in seq(settings$hilmo_start_year , settings$end_year)){
  ## read secondary care data ----
  if(y >=2018){
    iso <- read_hilmo_2018_(y= y, cols_in = settings$hilmo_iso_cols)
  } else if ( y %between% c(1996,2017)){
    iso <- read_hilmo_1996_2017_iso(y=y, cols_in = settings$hilmo_iso_cols_1996_2017)
  } else {
    stop('Year not implemented. Note, data structure change 2018, outpatient recognition 2019.')
  }
  ## Source pre-processing ----
  source(here('R', '0c_preparation_source_FCRHC.R'), encoding = 'UTF-8')
  
  ### Save reports ----
  report_flow %>% as.data.table() %>% .[, vuosi := y] %>%
    write_xlsx(file.path(dirs$report_pre, paste0('flow_',y, '.xlsx')))
  report_variable %>% 
    write_xlsx(file.path(dirs$report_pre, paste0('variables_',y, '.xlsx')))
  
  ## source reading and pre-processing primary care data ----
  if(y >=2011){ 
    source(here('R', '0c_preparation_source_RPHC.R'), encoding = 'UTF-8', local = TRUE)
    ### Save report ----
    report_avo %>% as.data.table() %>%
      write_xlsx(file.path(dirs$report_pre, paste0('avo_',y, '.xlsx')))
  }
  
  # Save longitudinal ----
  #create longitudinal data (data grouped by person, each individual's all years in a single file)
  if (settings$to_longitudinal == TRUE){
    inpat_0[, id_group := shnro %>% substr(start = 1, stop = 1)][
      , fwrite(.SD, file.path(dirs$pre, paste0(id_group, '_inpat.csv')), append = TRUE), 
      by = id_group]
    
    outpat_0[, id_group := shnro %>% substr(start = 1, stop = 1)][
      , fwrite(.SD, file.path(dirs$pre, paste0(id_group, '_outpat.csv')), append = TRUE), 
      by = id_group]
    
    if(y >=2011){
      avo_dg[, id_group := shnro %>% substr(start = 1, stop = 1)][
        , fwrite(.SD, file.path(dirs$pre, paste0(id_group, '_avo.csv')), append = TRUE), 
        by = id_group]
    }
  } else {
    # Or save annual ----
    inpat_0  %>% fwrite(file.path(dirs$pre, paste0(y, '_inpat.csv')))
    outpat_0 %>% fwrite(file.path(dirs$pre, paste0(y, '_outpat.csv')))
    avo_dg   %>% fwrite(file.path(dirs$pre, paste0(y, '_avo.csv')))
  }
}

rm(avo_dg, inpat_0, outpat_0)
gc()
