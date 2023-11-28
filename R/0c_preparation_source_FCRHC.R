
# Pre-processing secondary care data
# Data are red already, this script can be run for years 1996-2020 (not tested for 2021 ->)

# The process goes as follows:
# Set col names to lower
#   Set date format and isoid to bit64
# missing vals and errors
#    process variable ILAJI
#    admission and discharge dates tupva, lpvm
# ea: define psychiatry or other specialty of interest
# Recognize treatment type
#   Inpatient recognition
#   Outpatient recognition
#   outpatient episodes with length over 1 day
# Daignoses
#   read
#   join dgs

# the preprocessed data are saved in the upper level script that sourced this script. 

# Note. Diagnoses may be split into several sub-files which all need to be red and combined.


# Set col names to lower ----
setnames(iso, tolower(names(iso)))

## Set date format and isoid to bit64 ----

iso[, `:=`(tupva = tupva %>% as.IDate(format = settings$date_format) %>% as.integer(), 
           lpvm  = lpvm  %>% as.IDate(format = settings$date_format) %>% as.integer(),
           isoid = as.integer64(isoid))]

## missing vals and errors -----------------------------------------------------

report_flow$raw_nrow <- iso[, .N]
report_flow$raw_n_indiv <- iso[, uniqueN(shnro)]

### exclude entries with no identification to any person ----
iso <- iso[!shnro=='']

report_flow$notna_shnro <- iso[, .N]
report_flow$notna_shnro_n_indiv <- iso[, uniqueN(shnro)]

### exclude entries with admission date missing ----
iso <- iso[!is.na(tupva)]

report_flow$notna_tupva <- iso[, .N]

## process variable ILAJI ----

# 2019 iljai 1 tai 2
report_variable$ilaji <-  iso[,.N, ilaji]

# there are some errors in ilaji2 rows
report_variable$ilaji_2_lpvm_na <- iso[ilaji == 2, .N, is.na(lpvm)]

### set lpvm in ilaji 2 to the last day of  the year ----

# because the entry in the next year shoud have tpvm before or at the last day 
# of the year

iso[ilaji == 2, lpvm := last_day_of(y)]

## admission and discharge dates tupva, lpvm ----

report_variable$tupva_missing <- iso[,.N, tupva %>% is.na()]

iso <- iso[!is.na(tupva)]

### lpvm
report_variable$lpvm_missing <- iso[,.N, lpvm %>% is.na()]

iso <- iso[!is.na(lpvm)]

report_flow$notna_lpvm <- iso[,.N]

### lpvm before tupva, ie. admission after discharge

report_variable$tupva_greater_lpvm <-  iso[, .N, tupva > lpvm]

iso <- iso[tupva <= lpvm]

report_flow$tupva_lesser_lpvm <- iso[,.N]

report_flow$valid_nrow <- iso[,.N]
report_flow$valid_n_indiv <- iso[,uniqueN(shnro)]

# ea: define psychiatry or other specialty of interest -------------------------
iso[,psy := FALSE]

pttrn <- paste(settings$ea_psy, collapse = '|')
iso[, psy := grepl(pttrn, ea)]

report_variable$psy_entires <- iso[,.N, psy==TRUE]

# Recognize treatment type ---------------------------------------------------------------

# Old PALA
#
# 1 sairaala, 2 paivakir, 3 virhe?, 5 paihdehuolto, 6 kuntoutuslaitos, 83 paivasairaala, 
# 91 paivystyskaynti, 92 ajanvaraus ensikaynti, 93 uusinta,kaynti, 94 konsultaatiokaynti

# New: Yhteystapa

# R10 asiakkaan käynti vo:lla
# R20 kotikäynti
# R30 tyopaikkakaynti
# R40 sairaalakaynti (?) mainittu koodistopalvelimella, ei Hilmo-opaassa
# R41 käytni muualla, esim paivakodissa, palvelutalossa jne
# R52 reaaliaikainen etaasiointi
# R56 etaasiointi ilman reaalikaista kontaktia
# R60 ammattilaisten valinen konsultaatio, R71 neuvottelu, R72 asioiden hoito
# R80 vuodeosastohoito
# R90 muu

report_variable$yhteystapa <- iso[,.N, keyby = yhteystapa]

# psychiatric inpatient care: only a few errors
report_variable$psy_yt_r80_pala <- iso[psy == T & yhteystapa == "R80",.N, keyby = pala]

report_variable$psy_pala_1_yt <- iso[psy == T & pala == 1, .N, keyby = yhteystapa]

# more in other specialities

report_variable$yt_r80_pala <- iso[(yhteystapa == "R80"),.N, keyby = pala]

### inpatient recognition ------------------------------------------------------
iso[, inpat := 0]
iso[(pala ==1 & yhteystapa =='R80') | 
      (pala == 1 & yhteystapa == "") | 
      (is.na(pala) & yhteystapa == "R80"), inpat := 1]

inpat_0 <- iso[inpat== 1]

### outpatient recognition -----------------------------------------------------
# 1 fyysinen kontakti jossain (pkl, koti, muu)
# 2 puheli
# 3 etakontakti reaaliaikainen
# 4 etakontakti ei reaaliaikainen 
# 5 muita asioiden hoitoja, kuntoutuslaitokset joita ei lasketa inpat yms. tms.

iso[, outpat := 0]

if(y >= 2019){
  iso[inpat == 0 & yhteystapa == '', outpat := ifelse(pala %in% c(2, 83, 91, 92, 93, 94), 1,5) ]
  iso[inpat == 0 & yhteystapa != '', outpat := 5]
  iso[inpat == 0 & yhteystapa == settings$yhteystapa_4, outpat := 4]
  iso[inpat == 0 & yhteystapa == settings$yhteystapa_3, outpat := 3]
  iso[inpat == 0 & yhteystapa == settings$yhteystapa_2, outpat := 2]
  iso[inpat == 0 & yhteystapa %in% settings$yhteystapa_1, outpat := 1]
} else if ( y %between% c(1996,2018)){
  iso[inpat == 0, outpat := ifelse(pala %in% c(2, 83, 91, 92, 93, 94), 1,5) ]
} else { stop(paste('Year', y, 'not implemented. Outpatient recognition.'))}


##  outpatient episodes with length over 1 day ----
report_variable$outpat_lpvm_minu_tupva <- iso[outpat>0,.N, keyby = .(lpvm_minus_tupva = lpvm-tupva)]

# over night outpaient allowed, longer considered as inpatient care
iso[outpat>0 & tupva<lpvm-1, `:=`(inpat = 1, outpat = 0)]

report_variable$outpat <- iso[,.N, outpat]

# outpatient rows to separate dataset
outpat_0 <- iso[outpat %in% settings$yhteystapa_outpat]

rm(iso)
gc()


# Diagnoses --------------------------------------------------------------------
## read ----
if(y >=2018){
  dg_0a <- read_hilmo_2018_(y= y, file_in = 'icd10')
  dg_0b <- read_hilmo_2018_(y= y, file_in = 'ulksyy')
  
  dg_0a %>% setnames(., tolower(names(.)))
  dg_0b %>% setnames(., tolower(names(.)))
  
  setnames(dg_0b, "ulksyy", "koodi")
  dg_0 <- rbindlist(list(dg_0a, dg_0b), fill = T)

  dg <- process_dgs_2018_()
  
} else if ( y %between% c(1996,2017)){
  dg_0 <- read_hilmo_1996_2017_dg(y= y)
  dg <- process_dgs_1996_2017()
} else { stop(paste('Year', y, 'not implemented. DG'))}

rm(dg_0)

## join dgs --------------------------------------------------------------------

## inpat
inpat_0[dg, on = 'isoid', dg := i.dg]
gc()

report_variable$inpat_dg_100 <-  inpat_0[,.N,dg][order(-N)][1:100]
report_variable$inpat_psy_dg_100 <-  inpat_0[psy==T,.N,dg][order(-N)][1:100]

## outpat
outpat_0[dg, on = 'isoid', dg := i.dg]
gc()

report_variable$outpat_dg_100 <-  outpat_0[,.N,dg][order(-N)][1:100]
report_variable$outpat_psy_dg_100 <-  outpat_0[psy==T,.N,dg][order(-N)][1:100]

rm(dg)
gc()

