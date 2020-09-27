
#d0 virheellisten siivous ja psy luonti


# kuin edella, mutta myos descriptio ulos
#f.describe.d0 <- function(d0){
prepare_describe_parts <- function(d0){
  # prepare data part for aggregation and give description of the preparation: 
  #   * remove incorrect entries,
  #   * define psychiatric entries
  #   * ILAJI: choose 1: discharge (2: patient count each year on Dec 31)
  #
  #
  # @param d0: data part 
  # @return: list: processed data part and descriptions
  #
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
  
  d0 <- d0[(vuosi < max(vuosi) & ILAJI == 1) | (vuosi == max(vuosi))]
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
  ## psy alat
  #
  d0[,psy := FALSE]
  pattern<-'70|70F|70X|70Z|74|75|75X'
  d0[, psy := grepl(pattern, EA)]
  
  nrowC_psy <- nrow(d0[psy==TRUE])
  

  ###
  ## PALA
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
  n_vals <- list(nrow_raw_data = nrow1_raw_data,
                    n_notna_shnro = nrow2_notna_shnro, 
                    n_notna_TUPVA = nrow3_notna_tupva, 
                    n_included_ilaji = nrow4_included_ilaji,
                    n_notna_lpvm_when_ilaji1 = nrow5_notna_lpvm_ilaji1,
                    n_lpvm_grthan_tupva_when_ilaji1 = nrow6_lpvm_grthan_tupva, 
                    n_Valid_rows = nrow7_valid_rows
                 )
  
  n_of_valid_rows <- list(n_ilaji2_notna_lpvm = nrowA_ilaji2_notna_lpvm,
                    n_max_year_ilaji2 = nrowB_max_year_ilaji2,
                    n_psy = nrowC_psy
                    )
  
  n_pala <- list(PALA_all = n_pala_all, 
                 PALA_events_start_yaer = n_pala_events_start_year
                 )
  
  list(d0 = d0, n_vals = n_vals, n_of_valid_rows = n_of_valid_rows, n_pala = n_pala)
}

