#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#
# Function definition


# prepare data part for aggregation and give description of the preparation:
# - remove incorrect entries,
# - define psychiatric entries
# - ILAJI: choose value 1 (discharge), and for yaer_max also 2 (patient count each year on Dec 31)
#
# param d0: data table input for preparation
# return: list: prepared data and description of the processing
#
prepare_describe_parts <- function(d0){
  nrow1_raw_data <- nrow(d0)
  
  # exclude entries with no identification to any person
  d0 <- d0[!shnro=='']
  nrow2_notna_shnro <- nrow(d0)
  
  # exclude entries with admission date missing
  d0 <- d0[!is.na(TUPVA)]
  nrow3_notna_tupva <- nrow(d0)

  # ILAJI
  # only discharges (ILAJI == 1) and treatments that continue (ILAJI == 2) 
  # after the last day included in the dataset
  
  d0 <- d0[(vuosi < max_year & ILAJI == 1) | (vuosi == max_year)]
  nrow4_included_ilaji <- nrow(d0)
  
  # 
  d0 <- d0[(ILAJI == 1 & !is.na(LPVM)) | ILAJI == 2]
  nrow5_notna_lpvm_ilaji1 <- nrow(d0)
  
  d0 <- d0[(ILAJI == 1 & LPVM >= TUPVA) | ILAJI == 2]
  nrow6_lpvm_grthan_tupva <- nrow(d0)
  
  # this is also the number of valid rows
  nrow7_valid_rows <- nrow6_lpvm_grthan_tupva
  
  # 
  # when ILAJI == 2, LPVM should be NA
  # N:o of not NA entries
  nrowA_ilaji2_notna_lpvm <- nrow(d0[ILAJI==2 & !is.na(LPVM)])
  
  # set those to NA
  d0[ILAJI == 2, LPVM := NA]
  
  
  # n of treatments that continue after the end of the dataset 
  nrowB_max_year_ilaji2 <-nrow(d0[vuosi == max(vuosi) & ILAJI == 2]) 
  
  ###
  ## Define register entries related to psychiatry
  #
  d0[, psy := FALSE]
  d0[, psy := grepl(specialties_of_interest, EA)]
  
  nrowC_psy <- nrow(d0[psy==TRUE])
  

  ###
  ## PALA: service type, 1: inpatient, 83 day hospital, >90 outpatient
  #
  # 1 sairaala, 2 paivakir, 3 virhe?, 5 paihdehuolto, 6 kuntoutuslaitos, 83 paivasairaala, 
  #91 paivystyskaynti, 92 ajanvaraus ensikaynti, 93 uusinta,kaynti, 94 konsultaatiokaynti
  
  n_pala_all <- d0[,.N,keyby=PALA]
  
  if (exists('events_start_year')){
    n_pala_events_start_year <- d0[vuosi > events_start_year, .N, keyby = PALA]
  } else {
    n_pala_events_start_year <- data.table(PALA=NA)
  }

  # descriptions out: 
  n_vals <- list(
    nrow_raw_data = nrow1_raw_data,
    n_notna_shnro = nrow2_notna_shnro,
    n_notna_TUPVA = nrow3_notna_tupva,
    n_included_ilaji = nrow4_included_ilaji,
    n_notna_lpvm_when_ilaji1 = nrow5_notna_lpvm_ilaji1,
    n_lpvm_grthan_tupva_when_ilaji1 = nrow6_lpvm_grthan_tupva,
    n_Valid_rows = nrow7_valid_rows
  )
  
  n_of_valid_rows <-
    list(
      n_ilaji2_notna_lpvm = nrowA_ilaji2_notna_lpvm,
      n_max_year_ilaji2 = nrowB_max_year_ilaji2,
      n_psy = nrowC_psy
    )
  
  n_pala <- list(PALA_all = n_pala_all,
                 PALA_events_start_yaer = n_pala_events_start_year)
  
  list(d0 = d0, n_vals = n_vals, n_of_valid_rows = n_of_valid_rows, n_pala = n_pala)
}

# // end

