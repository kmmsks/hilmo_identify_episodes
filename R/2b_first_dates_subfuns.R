#' Find first date
#'
#' @param dat_in A data.table to be processed. Needs the variables defined by _field, and birthday
#' @param dg_field The name of the variable containing the diagnoses to be considered.
#' @param tulopvm_field The name of the variable containing the admission dates to be considered.  This is the incident date of the diagnoses present in an inpatient episode. The date must be set to integer format. 
#' @param lahtopvm_field The name of the variable containing the discharge dates to be considered. This date is considered when substing the diagnoses that meet the requirement for minimun age. The date must be set to integer format.
#' @param id_field The name of the ID variable, "shnro" throughout.
#' @param dg_age A data.table such as dg_groups_w_min_ages from 2a_first_dates_set_diagnoses.R or similar.
#'
#' @return A data.table with results
#' @export
#'
#' @examples
find_first_date <- function(dat_in, dg_field, tulopvm_field = "tulopvm_inpat_psy", 
                            lahtopvm_field = "lahtopvm", id_field = "shnro", dg_age){
  
  # same false date entries may have persisted even at this point. Hence, discard episodes that start before birth.
  d0 <- dat_in[get(tulopvm_field)> birthday]
  
  # first_any and first_any_w_f_dg goes without minimum age.
  # They are the first possible dates.
  
  # first start day of any treatment in the data
  first_any <- 
    d0[, .(date_min = min(get(tulopvm_field)), birthday = min(birthday)), by = eval(id_field)
    ][, `:=`(dg = 'any_psy')]
  # first start day of any treatment with a diagnosis of a mental disorder in the data
  first_any_w_f_dg <- 
    d0[grepl('f', get(dg_field), ignore.case = TRUE), .(date_min = min(get(tulopvm_field)), birthday = min(birthday)), by = eval(id_field)
    ][, `:=`(dg = 'any_psy_w_f_dg')]
  
  # First dates by diagnoses with mimimum age considered:
  
  # first date by diagnosis with the earliest possible age at onset set in
  # 2a_first_dates_set_diagnoses.R
  lst <- lapply(dg_age$dg %>% as.character(), function(i){
    pttrn <- dg_age[dg == i, dg_code]
    min_age <- dg_age[dg == i, age]
    
    d0[get(lahtopvm_field) >= birthday + min_age * 365.25 & grepl(pttrn, get(dg_field), ignore.case = TRUE), 
       if(.N > 0){.(date_min = min(get(tulopvm_field)), birthday = min(birthday))}, 
       by = eval(id_field)][, `:=`(dg = i)]
  } )
  
  first_dates <- rbindlist(c(lst, list(first_any), list(first_any_w_f_dg)), fill = TRUE)
  first_dates
}

# to combine results from the chunks and to get the first date of any disorder with
# the earliest possible age included
#' Title
#'
#' @param dat_in A data.table containing one ore more outputs of the find_first_date function.
#' @param one_treatment_type_only Logical. TRUE: prcesses a single type of data at once. FALSE: dat_in may consist of
#'    inpatient, outpatient, and primary care data that are bind by rbindlist with idcol="source".
#' @param dg_age A data.table such as dg_groups_w_min_ages from 2a_first_dates_set_diagnoses.R or similar.
#' @param id_field The name of the ID variable, "shnro" throughout.
#'
#' @return
#' @export
#'
#' @examples
combine_first_dates <- function(dat_in,one_treatment_type_only = FALSE, dg_age, id_field = "shnro"){
  
  dg_maingroups <- paste0("f", seq(0,9))
  # get earliest dates by person and by diagnoses ------------------------
  d2 <-  dat_in[dat_in[, .I[which.min(date_min)], by = .(get(id_field), dg)]$V1]
  
  setnames(d2, 'date_min', 'date')
  
  for (i in (dg_age$dg %>% as.character())){
    pttrn <- dg_age[dg == i, dg_code]
    min_age <- dg_age[dg == i, age]
    
    d2[dg == pttrn & date < birthday + min_age * 365.25, date := birthday + floor(min_age * 365.25)]
  }
  
  # if dat_in contains one treatment type only, ie. primary care or inpatient or outpatient, source variable is not needed
  if (one_treatment_type_only == FALSE){
    d3 <- rbindlist(list(
      d2,
      d2[dg %in% dg_maingroups, .(date = min(date, na.rm = T), birthday = min(birthday), 
                                  dg = "first", source = source[which.min(date)]), by = eval(id_field)]),
      fill = TRUE)
    out <- dcast(d3, get(id_field) + birthday ~ dg, value.var = c('date', 'source'))
  }  else {
    d3 <- rbindlist(list(
      d2,
      d2[dg %in% dg_maingroups, .(date = min(date, na.rm = T), birthday = min(birthday), 
                                  dg = "first"), by = eval(id_field)]),
      use.names = T)
    
    out <- dcast(d3, get(id_field) + birthday ~ dg, value.var = c('date'))
    setnames(out, old = names(out) %>% tail(-2), new = paste0('date_', names(out) %>% tail(-2)))
  }
  
  
  # dates: numeric to dates
  
  cols <- names(out)[startsWith(names(out), 'date')| startsWith(names(out), 'birthday')]
  out[, (cols) := lapply(.SD, as.IDate), .SDcols =cols]
  
  out[]
}

