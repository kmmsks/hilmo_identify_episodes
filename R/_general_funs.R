
create_dirs_preprocess <- function(start_year, end_year, data_folder_name = "data_main", longitudinal = TRUE){
  
  if (!dir.exists(here(data_folder_name))) {dir.create(here(data_folder_name))}
  
  if (longitudinal == TRUE){
    pre <- here(data_folder_name, 
                paste0('0_preprocessed_longitudinal_', start_year, "_",  end_year))
    if (!dir.exists(pre)) {dir.create(pre)}
  } else {
    pre <- here(data_folder_name, 
                paste0('0_preprocessed_annual'))
    if (!dir.exists(pre)) {dir.create(pre)}
  }
  
  if (!dir.exists(here(data_folder_name, 'raports'))) {dir.create(here(data_folder_name, 'raports'))}
  raport_pre <- here(data_folder_name, 'raports', '0_preprocessing')

  if (!dir.exists(raport_pre)) {dir.create(raport_pre)}

  # named list:
  tibble::lst(pre, raport_pre)
}


create_dirs_postprocess <- function(add_days, start_year, end_year, data_folder_name = "data_main", longitudinal = TRUE){

  p <- here(data_folder_name, "1_identified_episodes")
  if (!dir.exists(p)) {dir.create(p)}

  p <- here(data_folder_name, "1_identified_episodes", paste0('add_days_', add_days))
  
  if (!dir.exists(p)) {dir.create(p)}
  
  post <- here(data_folder_name, "1_identified_episodes", paste0('add_days_', add_days),
                 ifelse(longitudinal == TRUE, paste0("longitudinal_",start_year, "_", end_year), "annual"))

  if (!dir.exists(post)) {dir.create(post)}
  
#  raport_post <- here(data_folder_name, 'raports', '1_raport_identification')
#  dir.create(raport_post)
  
    # named list:
  tibble::lst(post)#, raport_post)
}

create_dirs_first_dates <- function(add_days, start_year, end_year, data_folder_name = "data_main", longitudinal = TRUE){
  
  first_dates <- here(data_folder_name, '2_first_dates')
  if (!dir.exists(first_dates)) {dir.create(first_dates)}
  
  if(longitudinal == TRUE){
    l <- paste0('longitudinal_', start_year, "_", end_year)
  } else {
    l <- "annual"
  }

  first_dates_long <- here(data_folder_name, '2_first_dates', l)
  
  if (!dir.exists(first_dates_long)) {dir.create(first_dates_long)}

  if(is.na(add_days)){
    p <- here(data_folder_name, '2_first_dates', l, "non_processed")
    if (!dir.exists(p)) {dir.create(p)}
        
    first_dates_nonprocessed <- file.path(p, "mental_health_services") 
    if (!dir.exists(first_dates_nonprocessed)) {dir.create(first_dates_nonprocessed)}
        
    # named list:
    return(tibble::lst(first_dates, first_dates_long, first_dates_nonprocessed))
    } else {
      p <- here(data_folder_name, '2_first_dates', l, paste0("add_days_", add_days))
      if (!dir.exists(p)) {dir.create(p)}
        
      first_dates_mh <- file.path(p, paste0('mental_health_services')) 
      if (!dir.exists(first_dates_mh)) {dir.create(first_dates_mh)}
        
      # named list:
      tibble::lst(first_dates, first_dates_long, first_dates_mh)
    }
  }

synthetize_data <- function(n_rows = 20000, n_individuals = 1000, 
                           start_year = 2015, end_year = 2020, seed = 1,
                           outpatient_proportion = .35, primary_care_proportion = .4, ilaji2_proportion = .05, 
                          save_data = TRUE, data_folder_name = 'data_main', longitudinal = TRUE){
  
  if(n_rows < n_individuals){stop("n_rows must be >= n_individuals")}
  if(start_year > end_year){stop("start_year must <= end_year")}
  if(!(outpatient_proportion %between% c(0, 1)) | !(ilaji2_proportion %between% c(0, 1))) {stop("proportion must be between 0 and 1")}
  if(!(longitudinal %in% c("TRUE", "FALSE", 0, 1)) | !(save_data %in% c("TRUE", "FALSE", 0, 1))){stop("non-logical in logical argument")}
  
  # formart dates
  start_date <- as.IDate(paste0(start_year, "-01-01"))
  end_date <-   as.IDate(paste0(end_year, "-12-31"))
  
  # first create discharge dates that are within the range of start_date and end_date
  set.seed(seed)
  lpvm <- sample(start_date:end_date, size = n_rows, replace = TRUE) %>% as.IDate()
  
  # IDs: n_individuals entiries repated n_rows times:
  a  <- paste0(sample(c(letters[1:5], seq(1,10)), size = n_rows, replace = TRUE), sample(sprintf(paste0("%0", nchar(n_rows), "d"), 1:n_rows)))
  b  <- sample(a, size =n_individuals)
  id <- sample(b, size = n_rows, replace = T)
  
  # admission date approximately based on 2020 LOS distribution
  ranges <- list(1:6, 7:13, 14:29, 30:59, 60:89, 90:364, 365: 256*5)
  lens <- lengths(ranges)
  probs <- c( 0.3, 0.2, 0.2, 0.15, .05, .05, .01)
  los <- sample(unlist(ranges), size = n_rows, replace = TRUE, prob = rep(probs / lens, times = lens))
  
  # create data
  dat <- data.table(
    shnro = id %>% as.character(),
    ilaji = sample(c(1,2), size = n_rows, replace = TRUE, prob = c(1 - ilaji2_proportion, ilaji2_proportion)),
    tupva = as.integer(lpvm - los), 
    lpvm  = as.integer(lpvm),
    paltu = sample(5000:5100, size = n_rows, replace = TRUE),
    psy   = sample(c(FALSE,TRUE), size = n_rows, replace = TRUE, prob = c(.70, .30)),
    inpat = TRUE
  )
  
  # create vuosi (year) based on admission
  dat[, vuosi := year(lpvm)]
  
  # create specialty codes, psychiatry is 70, 75, 76, 10:25 are internal medicine, surgery etc.
  dat[psy == 1, ea := sample(c(70,75,76), size = dat[psy == 1, .N], replace = T)][psy == 0, ea := sample(c(10:25), size = dat[psy == 0, .N], replace = TRUE)]
  
  # create diagnoses, 1 to 3 per row
  a <- paste0(sample(LETTERS[1:6], size = n_rows, replace = TRUE), sample(10:999, size = n_rows, replace = TRUE))
  b <- paste0(sample(LETTERS[1:6], size = n_rows, replace = TRUE), sample(10:999, size = n_rows, replace = TRUE))
  c <- paste0(sample(LETTERS[1:6], size = n_rows, replace = TRUE), sample(10:999, size = n_rows, replace = TRUE))
  
  b_na <- sample(c(FALSE,TRUE), size = n_rows, replace = T, prob = c(.9, .1))
  c_na <- sample(c(FALSE,TRUE), size = n_rows, replace = T, prob = c(.6, .4))
  
  b[b_na] <- NA
  c[c_na] <- NA
  
  dat[,dg := paste0(a, "_", b, "_", c)]
  
  # change some rows to outpatient and primart care visits based on the outpatient_proportion and priamry_care_proportion
  
  dat[, type := sample(c("inpatient","outpatient", "primary_care"), size = n_rows, replace = T, 
                       prob = c(1 - outpatient_proportion -primary_care_proportion, outpatient_proportion, primary_care_proportion ))]
  
  dat[type != "inpatient", inpat := FALSE]
  
  # outpatient rows: ----
  # outpatient, ilaji 1 always
  dat[, outpat := 0]
  dat[, outpat := sample(c(1,2,3), size = n_rows, replace = T)]
  dat[inpat == 1, outpat := NA]
  
  dat[inpat == 0,`:=`(tupva = lpvm, ilaji = 1)]
  
  

  # ilaji 2, lpvm is NA by definition. In the preprocessing, it has been set to the last day of the year
  dat[ilaji == 2, lpvm := last_day_of(vuosi)]
  
  #primary care rows: ----
  primary_care_cols <- c("dg_avo")
  
  dat[, (primary_care_cols) := NA_character_]
  dat[type == "primary_care", `:=`(dg_avo = dg)]

  dat[type == "primary_care", `:=`(dg = NA_character_)]

  d <- dat[type == "primary_care" & shnro == shnro %>% head(1)]
  d[, `:=`(tupva = tupva[1], lpvm = lpvm[1])]

  dat <- rbindlist(list(
    dat[type %in% c("inpatient","outpatient") | (type ==  "primary_care" & shnro != d[,shnro %>% head(1)])],
    d
  ),fill = T)  

  # group_id based on the first character of personal id (shnro)
  dat[, id_group := shnro %>% substr(start = 1, stop = 1)]
  
  if(save_data == TRUE){
    dirs <- create_dirs_preprocess(data_folder_name = data_folder_name, longitudinal = longitudinal, 
                                   start_year = start_year, end_year = end_year)
    
    if(longitudinal == TRUE){
      cols <- setdiff(names(dat), c("type", primary_care_cols))
      dat[type == "inpatient", ..cols][, fwrite(.SD, file.path(dirs$pre, paste0(id_group, '_inpatient.csv'))), by = id_group]
      dat[type == "outpatient", ..cols][, fwrite(.SD, file.path(dirs$pre, paste0(id_group, '_outpatient.csv'))), by = id_group]
      
      cols <- c("shnro", "vuosi", "tupva", "lpvm", primary_care_cols, "id_group")
      dat[type == "primary_care", ..cols][, fwrite(.SD, file.path(dirs$pre, paste0(id_group, '_primary_care.csv'))), by = id_group]
    } else {
      cols <- setdiff(names(dat), c("type", primary_care_cols))
      dat[inpat == 1, ..cols][, fwrite(.SD, file.path(dirs$pre, paste0(vuosi, '_inpatient.csv'))), by = vuosi]
      dat[inpat == 0, ..cols][, fwrite(.SD, file.path(dirs$pre, paste0(vuosi, '_outpatient.csv'))), by = vuosi]
      
      cols <- c("shnro", "vuosi", "tupva", "lpvm", primary_care_cols, "id_group")
      dat[type == "primary_care", ..cols][, fwrite(.SD, file.path(dirs$pre, paste0(vuosi, '_primary_care.csv'))), by = vuosi]
    }
    message(paste0('Message: Fake data created and saved on folder ', dirs$pre, "."))
  } else {
    message(paste0("Message: Fake data created. Not saved"))
  }
  # all done
  dat[]
}

last_day_of <- function(y_in){
  as.IDate(paste0(y_in, '-12-31')) %>% as.integer()
}

read_files_from <- function(location, longitudinal = TRUE){
  # read inpatient and outpatient data
  chunks_in <- location %>% list.files() %>%  word(sep = "_") %>% unique()
  
  f <- function(i){  
    secondary <- fread(file.path(location, paste0(i, '_inpatient_outpatient.csv')))[, shnro := as.character(shnro)]
    # read primary care data
    primary <- fread(file.path(location, paste0(i, '_primary_care.csv')))[, shnro := as.character(shnro)]
    
    rbindlist(list(secondary, primary), fill = TRUE)
  }
  if(longitudinal == TRUE){
    out <- lapply(chunks_in, f) %>% rbindlist()
  } else{
    names(chunks_in) <- paste0("year_", chunks_in)
    out <- lapply(chunks_in, f)
  }
  out
}
