
#' Identify treatment episodes from partly overlapping data
#' 
#' This function does the following:
#' 1. creates directory tree if not present already, based on the create_dirs_preprocess-function in file R/_general_functions.R
#' 2. identify chunk files in the folder with pre-processed data. This pre-processed data may be real data after running
#'    0_preparation.R or synthetic data produced with syntetize_data()-function. The chunks may be longitudinal or annual.
#' 3. Loops through the chunks and does the actual processing
#'    - Defines admission and discharge dates for each inpatient episode with possible overlapping register entires.
#'      Recognizes dischgarge diagnoses at the end of the inpatien episodes and if treamtent in psychiatric unit ends before
#'      the end of the episode, recognizes psychiatric discharge diagnoses. Preliminary diagnoses durign the hospital stay are
#'      collected. PSychiatric diagnoses from other medical specialities during psychiatric inpatient treatment are ignored.
#'    - Identifies records of outpatient appointments that take place during inpatient care. Psychiatric outpatient diagnoses 
#'      during psychiatric inpatient care before discharge date are considered preliminary.
#'    - Identifies records of primary care (general practice) appointments that take place during inpatient care. Again, psychiatric
#'      diagnoses during psychiatric inpatient care before discharge date are considered preliminary.
#'  4. Save processed chunks into a folder defined in the create_dirs_postprocess-function in file R/_general_functions.R
#'
#' @param add_days Numeric, defines how many full calendar days are required between inpatient episodes. Commonly 0 or 1.
#' @param start_year Numeric, start year of the data to be processed. May be used for subsetting longitudinal data.
#' @param end_year Numeric, end year of the data to be processed. May be used for subsetting longitudinal data.
#' @param longitudinal Logistic, TRUE if each individual's all data are in single dataset. FALSE if annual data are in separate datasets.
#' @param process_secondary_outpatient Logistic, TRUE if secondary outpatient care is processed, this is default behaviour. 
#'                                      FALSE if no outpatient data exists, ie. data end before 1998.
#' @param process_primary_care Logistic, TRUE if primary care data is included, FALSE if no.
#' @param separate_files_for_old_registers Logistic, TRUE if prepared data before the year 1996 are in separate files. 
#'
#' @return Nothing. Data are processed in chunks and each chunk is saved as CSV file in the folder defined in 
#'          the create_dirs_postprocess-function in file R/_general_functions.R. Location of the data is printed as a message
#' @export
#'
#' @examples
process_data <- function(add_days, start_year, end_year, longitudinal = TRUE, 
                         process_secondary_outpatient = TRUE, process_primary_care = TRUE, separate_files_for_old_registers = FALSE){
  
  # Here are the actual processing functions:
  source(here("scripts", 'R', '1a_processing_subfuns.R'), encoding = 'UTF-8')
  
  # location of the pre-processed data and create directory tree for processed data
  dirs <- c(create_dirs_preprocess(start_year = start_year, end_year = end_year, longitudinal = longitudinal),
            create_dirs_postprocess(add_days = add_days, start_year = start_year, end_year = end_year, longitudinal = longitudinal))
  
  # file chunks to be processed
  chunks_in <- dirs$pre %>% list.files() %>% word(sep = "_") %>% unique()
  
  # Loop: read pre-processed chunk of inpatient data
  for( i in chunks_in){
    if(separate_files_for_old_registers == TRUE){
      inpat_0 <- rbindlist(list(
        fread(file.path(dirs$pre, paste0(i, '_inpatient.csv'))),
        fread(file.path(dirs$pre, paste0(i, '_before_1996.csv')))
      ), fill = TRUE)
    } else if(separate_files_for_old_registers == FALSE){
      inpat_0 <- fread(file.path(dirs$pre, paste0(i, '_inpatient.csv')))
    } else {
      stop("separate_files_for_old_registers must be TRUE or FALSE. Old registers refer to data before 1996 which usually are provided in 
           separate files.")
    }
    
    # Actual processing of the inpatient data
    inpat_1 <- identify_inpatient_episodes(inpat_0 = inpat_0, add_days = add_days, 
                                           start_year = start_year, end_year = end_year, include_new_variables = F)
    
    # Processing of the outpatient data
    if(process_secondary_outpatient == TRUE){
      # read pre-processed chunk of outpatient data
      outpat_0 <- fread(file.path(dirs$pre, paste0(i, '_outpatient.csv')))
      
      # Actual processing of the outpatient data
      inpat_outpat_1 <- process_outpatient_data(outpat_0 = outpat_0, inpat_1 = inpat_1, start_year = start_year, end_year = end_year)
      
      # Save processed data
      inpat_outpat_1 %>% fwrite(file.path(dirs$post, paste0(i, '_inpatient_outpatient.csv')))
    }
    # Processing of the primary care data
    if(process_primary_care == TRUE) {
      if(process_secondary_outpatient == FALSE){
        stop("process_secondary_outpatient must be TRUE for primary care processing")
      }
      # read pre-processed chunk of outpatient data
      prim_care_0 <- fread(file.path(dirs$pre, paste0(i, '_primary_care.csv')))
      
      # Actual processing
      prim_care_1 <- process_primary_care(prim_care_0 = prim_care_0, inpat_outpat_1 = inpat_outpat_1,
                                                       start_year = start_year, end_year = end_year)
      # save
      prim_care_1 %>% fwrite(file.path(dirs$post, paste0(i, '_primary_care.csv')))
    } 
    
    # if only period before any secondary care data (before 1998) is processed, save inpatient data only
    if(process_secondary_outpatient == FALSE & process_primary_care == FALSE) {
      inpat_1 %>% fwrite(file.path(dirs$post, paste0(i, '_inpatient_only.csv')))
    } 
    
  }
  message(paste("Message: Done. Processed data saved on", dirs$post))
}


