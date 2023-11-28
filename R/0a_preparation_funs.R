
# These are helper functions for holding code. The idea is to make the preparation scripts
# more readable by having part of the stuff here.

# This file contains:
#
# READ functions
# - Reading the registers can be tricky if you cannot rename the files or folder structure.
#    Hence, several functions are needed.
# PREPARATION functions
# - The preparation function contain simple but highly important steps.
# CONVERT ICD
# - Convert ICD: the conversion rules are defined 0a_preparation_settings_dgs.R
#   Here is only the engine.
# Outpatient report
# - Reporting the pre-processingsteps of the secondary care outpatient data is in this
#   function.

# READ functions ---------------------------------------------------------------

# Carefully set the subdirectory of each data

read_hilmo_2018_ <- function(y, file_in = '', max_in = Inf, cols_in = NULL){
  
  f <- ifelse(file_in=='', 
              paste0('hilmo_iso_', y, '.sas7bdat'),
              paste0('hilmo_iso_', y, '_', file_in, '.sas7bdat')
  )
  
  p <- file.path(dirs$hilmo_root, paste0('Hilmo_', y), f)
  
  if(is.null(cols_in)){
    d <- read_sas(p, n_max = max_in)
  } else {
    d <- read_sas(p, n_max = max_in, col_select = cols_in)
  }
  setDT(d)
  d
}


read_hilmo_1996_2017_iso <- function(y, max_in = Inf, cols_in = NULL){
  
  p <- file.path(dirs$hilmo_root, "HILMO_kokonais", "thl_2044_hilmo_iso_1996_2017", "terveydenhuolto_iso",
                 paste0("terveydenhuolto_iso_", y, "_shnro.sas7bdat")
  )
  
  if(is.null(cols_in)){
    d <- read_sas(p, n_max = max_in)
  } else {
    d <- read_sas(p, n_max = max_in, col_select = cols_in)
  }
  setDT(d)
  d
}
read_hilmo_1996_2017_dg <- function(y, cols_in = NULL){
  
  p1 <- file.path(dirs$hilmo_root, "thuolto_iso_dg",
                  paste0("thuolto_iso_dg_", y, ".csv"))
  p2 <- file.path(dirs$hilmo_root, "HILMO_kokonais", "thl_2044_hilmo_iso_1996_2017",
                  "terveydenhuolto_iso_taptyyp", 
                  paste0("thuolto_iso_taptyyp_", y, ".sas7bdat"))
  #p2: lahinna Y, yksittaisia muita
  p3 <- file.path(dirs$hilmo_root, "HILMO_kokonais", "thl_2044_hilmo_iso_1996_2017",
                  "terveydenhuolto_iso_ulksyy", 
                  paste0("thuolto_iso_ulksyy_", y, ".sas7bdat"))
  # p3: X, W, Y, V
  
  if(is.null(cols_in)){
    lst <- list(
      d1 = fread(p1) %>% setnames(., tolower(names(.))),
      if(y >= 1999){d2 = read_sas(p2) %>% setDT() %>% 
        setnames(., tolower(names(.))) %>% 
        .[,isoid := as.integer64(isoid)]},
      d3 = read_sas(p3) %>% setDT() %>% 
        setnames(., tolower(names(.))) %>% 
        .[,isoid := as.integer64(isoid)]
    )
  } else {
    lst <- list(
      d1 = fread(p1, select = cols_in) %>% setnames(., tolower(names(.))),
      if(y >= 1999){d2 <- read_sas(p2, select = cols_in) %>% setDT() %>% 
        setnames(., tolower(names(.))) %>% 
        .[,isoid := as.integer64(isoid)]},
      d3 = read_sas(p3, select = cols_in) %>% setDT() %>% 
        setnames(., tolower(names(.))) %>% 
        .[,isoid := as.integer64(isoid)]
    )
  }
  rbindlist(lst,fill = TRUE)
}

read_avohilmo <- function(y, max_in = Inf, cols_in = NULL){
  
  f <- if(y %between% c(2011, 2017)){
    paste0('ah_palvelutapahtuma_', y, '_shnro.sas7bdat')
  } else if(y %between% c(2018, 2020)){
    paste0('avohilmo_', y, '_palvelutapahtuma.sas7bdat')
  } else {
    stop (paste('Year', y, 'not implemented. AvoHIlmo read.'))
  }
  
  p <- if(y %between% c(2011, 2017)){
    file.path(dirs$hilmo_root, "AvoHilmo", "avohilmo_palvelutapahtuma", f)
  } else if(y %between% c(2018, 2020)){
    file.path(dirs$hilmo_root, paste0("AvoHilmo_", y), f)
  } else {
    stop (paste('Year', y, 'not implemented. AvoHIlmo read.'))
  }
  
  if(is.null(cols_in)){
    d <- read_sas(p, n_max = max_in)
  } else {
    d <- read_sas(p, n_max = max_in, col_select = cols_in)
  }
  setDT(d)
  d
}

read_avohilmo_dg <- function(y, dg_type = 'kayntisyy_icd10', max_in = Inf, cols_in = NULL){
  
  f <- if(y %between% c(2011, 2017)){
    paste0('avohilmo_', dg_type, '_', y, '.sas7bdat')
  } else if(y %between% c(2018, 2020)){
    paste0('avohilmo_', y, '_', dg_type, '.sas7bdat')
  } else {
    stop (paste('Year', y, 'not implemented. AvoHIlmo read.'))
  }
  
  p <- if(y %between% c(2011, 2017)){
    file.path(dirs$hilmo_root, "AvoHilmo", paste0("avohilmo_", dg_type), f)
  } else if(y %between% c(2018, 2020)){
    file.path(dirs$hilmo_root, paste0("AvoHilmo_", y), f)
  } else {
    stop (paste('Year', y, 'not implemented. AvoHIlmo read.'))
  }
  
  if(dg_type == "palvelutapahtuma" & y %between% c(2011, 2017)){
    p <- gsub(".sas7bdat", "_shnro.sas7bdat", p)
    p <- gsub("avohilmo_palvelutapahtuma_", "ah_palvelutapahtuma_", p)
    
    if(is.null(cols_in)){
      d <- read_sas(p, n_max = max_in)
    } else {
      d <- read_sas(p, n_max = max_in, col_select = c("tapahtuma_tunnus", cols_in))
    }
    setDT(d)
    d
  } else {
    
    if(is.null(cols_in)){
      d <- read_sas(p, n_max = max_in)
    } else {
      d <- read_sas(p, n_max = max_in, col_select = c("TAPAHTUMA_TUNNUS", cols_in))
    }
    setDT(d)
    d
  }
  d %>% setnames(., tolower(names(.)))
  d
}

# PREPARATION functions --------------------------------------------------------

process_dgs_2018_ <- function(){
  
  dg_0 %>% setnames(., tolower(names(.)))
  
  # Set dates and isoid to bit64
  
  dg_0[, `:=`(isoid = as.integer64(isoid))]
  
  # monenlaista tassa taulussa
  #dg_0[,.N,kentta]
  
  # valitaan diagnoosien kentat jos halutaan
  #  dg <-  dg_0[kentta %in% settings$dg_fields, .(dg = paste(koodi, collapse = '_')), by = isoid]
  
  # mutta otetaan kaikki kentat
  dg <-  dg_0[, .(dg = paste(koodi, collapse = '_')), by = isoid]
  
  dg
}

process_dgs_1996_2017 <- function(){
  
  
  dg_0[koodi1 %>% is.na(), koodi1 := ""]
  dg_0[koodi2 %>% is.na(), koodi2 := ""]
  dg_0[ulksyy %>% is.na(), ulksyy := ""]
  
  if (y >=1999){
    dg_0[taptyyp %>% is.na(), taptyyp := ""]
    dg <- dg_0[, .(dg = paste0(koodi1, koodi2, taptyyp, ulksyy, collapse = '_')), by = isoid ]
  } else{
    dg <- dg_0[, .(dg = paste0(koodi1, koodi2, ulksyy, collapse = '_')), by = isoid ]
  }
  dg
}

process_before_1996 <- function(d0){
  d0 <- d0[vuosi %between% c(old_start_year, old_end_year)]
  # exclude entries with no identification to any person
  d0 <- d0[!shnro=='']
  
  # exclude entries with admission date missing
  d0 <- d0[!is.na(tupva)]
  
  # discharge date missing 
  d0 <- d0[( !is.na(lpvm))]
  
  # exclude if discharge before admission
  d0 <- d0[lpvm >= tupva]
  
  # exclude dates that are after the last register yaer
  
  d0 <- d0[tupva <= as.IDate(paste(old_end_year, '12', '31', sep = '-')) & lpvm <= as.IDate(paste(old_end_year, '12', '31', sep = '-'))]  
  
  d0
}

# Primary care data processing
process_avohilmo <- function(dat, report = TRUE, set_tupva_to_lpvm_if_duration_over_1 = TRUE,
                             yhtestapa_select = settings$yhteystapa_avo # everything: '.'
){
  dat %>% setnames(., tolower(names(.)))
  
  # yhtestapa: subset of interest 
  if(report == TRUE){
    report_avo$raw_nrow <<- dat[, .N]
    report_avo$raw_n_indiv <<- dat[, uniqueN(shnro)]
  }
  
  dat <- dat[grepl(yhtestapa_select %>% paste(collapse = "|"), kaynti_yhteystapa, ignore.case = T)]
  
  
  if(report == TRUE){
    report_avo$raw_nrow_yhtestapa_avo <<- dat[, .N]
    report_avo$raw_n_indiv_yhtestapa_avo <<- dat[, uniqueN(shnro)]
  }
  gc()
  
  ## palvelumuoto ----------------
  dat[, palvelumuoto_mttyo := 0]
  dat[kaynti_palvelumuoto == 'T71', palvelumuoto_mttyo := 1]
  dat[, kaynti_palvelumuoto := NULL]
  
  # missing vals and errors -----------------------------------------------------
  
  
  ## exclude entries with no identification to any person ----
  dat <- dat[!shnro=='']
  
  if(report == TRUE){
    report_avo$notna_shnro <<- dat[, .N]
    report_avo$notna_shnro_n_indiv <<- dat[, uniqueN(shnro)]
  }
  ## Set dates ----
  
  dat[, `:=`(tupva = kaynti_alkoi %>% as.IDate(format = settings$date_format) %>% as.integer(), 
             lpvm  = kaynti_loppui  %>% as.IDate(format = settings$date_format) %>% as.integer())]
  
  dat[, `:=`(kaynti_alkoi= NULL, kaynti_loppui = NULL)]
  
  
  ## tupva, lpvm ----
  
  # exclude entries with admission date missing
  dat <- dat[!is.na(tupva)]
  if(report == TRUE){
    report_avo$notna_tupva <<- dat[, .N]
  }
  ## lpvm
  
  dat <- dat[!is.na(lpvm)]
  
  if(report == TRUE){
    report_avo$notna_lpvm <<- dat[,.N]
  }
  ## lpvm before tupva
  
  dat <- dat[tupva <= lpvm]
  if(report == TRUE){
    report_avo$tupva_lesser_lpvm <<- dat[,.N]
    report_avo$valid_nrow <<- dat[,.N]
    report_avo$valid_n_indiv <<- dat[,uniqueN(shnro)]
  }
  ## overnight
  if(report == TRUE){
    report_avo$n_duration_overnight <<- dat[tupva < lpvm, .N]
  }
  if(set_tupva_to_lpvm_if_duration_over_1 == TRUE){
    dat[tupva < lpvm -1 , tupva := lpvm]
  }
  dat
}

# CONVERT ICD diagnoses  -------------------------------------------------------
# (ICPC-2 diagnoses are converted into ICD-10 within 0c_preparation_source_RPHC.R)

convert_to_icd_10 <- function(dat, from='icd-8', raw_hilmo = FALSE){
  # raw_hilmo TRUE: talloin uudelleenkoodataan raaka-hilmoa, jote ei ole ketjutettu
  
  if(from == 'icd-8'){
    conversion_key <-  icd_conversions$icd8_icd10
  } else if (from == 'icd-9'){
    conversion_key <-  icd_conversions$icd9_icd10
  } else {
    stop('set conversion key in 01_dgs.R')
  }
  
  if (raw_hilmo == FALSE){
    dat[, dg := '']
    for(i in conversion_key %>% names()) {
      dat[grepl(conversion_key[i], dg_old, ignore.case = TRUE), 
          dg := paste(dg, i, sep = '_')]
    }
  } else {
    
    dat[, dg_all_old := paste0('_', dg_all)]
    dat[, dg_inpat_old := paste0('_', dg_inpat)]
    dat[, dg_inpat_psy_old := paste0('_', dg_inpat_psy)]
    
    dat[, `:=`(dg_all = NA_character_, dg_inpat = NA_character_, dg_inpat_psy = NA_character_)]
    
    for(i in conversion_key %>% names()) {
      dat[grepl(conversion_key[i], dg_all_old, ignore.case = TRUE), 
          dg_all := paste(dg_all, i, sep = '_')]
      
      dat[grepl(conversion_key[i], dg_inpat_old, ignore.case = TRUE), 
          dg_inpat := paste(dg_inpat, i, sep = '_')]
      
      dat[grepl(conversion_key[i], dg_inpat_psy_old, ignore.case = TRUE), 
          dg_inpat_psy := paste(dg_inpat_psy,i, sep = '_')]
    }
    
    dat[, dg_all       := gsub("NA_", "", dg_all)]
    dat[, dg_inpat     := gsub("NA_", "", dg_inpat)]
    dat[, dg_inpat_psy := gsub("NA_", "", dg_inpat_psy)]
  }
  dat
}

# outpatient report ------------------------------------------------------------

get_outpat_report <- function(dat_in){
  
  dat <- copy(dat_in)
  
  setorderv(dat, c('shnro', 'tulopvm', 'lahtopvm', 'episode'))
  
  nums <- list(
    ## episode alkaa pkl-kaynnilla
    na = nrow(dat[shnro == shift(shnro) & episode > 0 & shift(episode) == 0 & tulopvm == shift(tulopvm)]),
    # psy osastoepisode alkaa psy pkl-kaynnilla
    naP = nrow(dat[shnro == shift(shnro) & episode > 0 & 
                     shift(episode) == 0 & tulopvm == shift(tulopvm) & shift(psy) == T & psy == T]),
    #pkl-kaynti admissiota edeltavana paivana
    nb = nrow(dat[shnro == shift(shnro) & episode > 0 & shift(episode) == 0 & tulopvm == shift(tulopvm) + 1]),
    #psy pkl-kaynti psy admissiota edeltavana paivana
    nbP = nrow(dat[shnro == shift(shnro) & episode > 0 & shift(episode) == 0 & tulopvm == shift(tulopvm) + 1 & 
                     shift(psy) == T & psy == T]),
    # mika tahansa pkl-kaynti psy osastoepisoden edella
    ncP = nrow(dat[psy == T & shnro == shift(shnro) & episode > 0 & shift(episode) == 0 & tulopvm == shift(tulopvm) + 1]),
    #episoden sisalla pkl-kaynti
    nd = nrow(dat[shnro == shift(shnro, type = 'lead') & episode > 0 & shift(episode, type = 'lead') == 0 & 
                    lahtopvm > shift(tulopvm, type = 'lead')]),
    # psy episoden sisalla psy pkl kaynti
    ndP = nrow(dat[shnro == shift(shnro, type = 'lead') & episode > 0 & shift(episode, type = 'lead') == 0 & 
                     lahtopvm > shift(tulopvm, type = 'lead')  & psy == T & shift(psy) == T]),
    #episoden paatospaivana pkl-kanyti
    ne = nrow(dat[shnro == shift(shnro, type = 'lead') & episode > 0 & shift(episode, type = 'lead') == 0 & 
                    shift(tulopvm, type = 'lead') == lahtopvm]),
    #psy episoden paatospaivana psy pkl-kanyti
    neP = nrow(dat[shnro == shift(shnro, type = 'lead') & episode > 0 & shift(episode, type = 'lead') == 0 & 
                     shift(tulopvm, type = 'lead') == lahtopvm & psy == T & shift(psy) == T]),
    nf = dat[inpat!=1, .N],
    nfP = dat[inpat!=1 & psy == T, .N]
  )
  
  labs <- list(
    na = 'inpatient period starts with outpatient appointment', 
    nb = 'outpatient appointment the previous day of the start of an inpatient period',
    nd = 'outpatient appointment within an inpatient period', 
    ne = 'outpatient appointment on the last day of an inpatient period',
    naP = 'psychiatric inpatient period starts with psychiatric outpatient appointment', 
    nbP = 'psychiatric outpatient appointment the previous day of the start of a psychiatric inpatient period', 
    ncP = 'whatever outpatient appointment the previous day of the start of a psychiatric inpatient period',   
    ndP = 'psychiatric outpatient appointment within a psychiatric inpatient period', 
    neP = 'psychiatric opoutpatient appointment on the last day of a psychiatric inpatient period',
    nf  = 'N all outpatient rows before aggregation',
    nfP = 'N psychiatric outpatient rows before aggregation')
  
  
  a <- labs %>% stack() %>% setDT()
  b <- nums %>% stack() %>% setDT()
  
  out <- a[b, on = 'ind']
  out[, psy := ifelse(ind %like% 'P', 1,0) ]
  out[order(psy), .(lab = values, psy, value = i.values)]
}


