
# Pre-processing primary care data
# This script can be run for years 2011-2020 (not tested for 2021 ->)

# 2011-2017 is slightly different than 2018-2020.

# Filtering diagnoses:
# settings$avo_only_psy_dgs
#     TRUE: Include only rows with ICD-10 F, ICPC-2 P, or self harm (ICD-10) diagnoses
#     FALSE: everything will be included


# The process goes as follows:
# Read data
# Source process function: treatment types, errors etc. are controlled within this function
# Read diagnoses
# Join diagnoses and filter
# Excluded if no ID (variable shnro missing)

# Note, primary care data comes with numerous variables. Here we 

# Read data --------------------------------------------------------------------
if(y %between% c(2011, 2017)){
  cols <-settings$hilmo_avo_cols
  } else if (y %between% c(2018, 2020)){
     cols <-settings$hilmo_avo_cols_2018_2020
   } else {
       stop("Year not implemented")
     }

avo <- read_avohilmo(y = y, cols_in = cols)

# Source process function ------------------------------------------------------
avo <- process_avohilmo(avo, set_tupva_to_lpvm_if_duration_over_1 = TRUE)

# Read diagnoses ---------------------------------------------------------------

## ICPC-2 ----

icpc2_0 <- read_avohilmo_dg(y = y, dg_type = 'kayntisyy_icpc2', cols_in = c('TAPAHTUMA_TUNNUS', 'ICPC2'))
icpc2_0 %>% setnames(., tolower(names(.)))

if(settings$avo_only_psy_dgs == TRUE){
  icpc2_0 <- icpc2_0[grepl('P', icpc2, ignore.case = TRUE)]
}

icpc2 <- icpc2_0[, .(icpc2 = paste(icpc2, collapse = '_')), keyby = tapahtuma_tunnus]

icpc2[, icd10 := '']

for(i in c(paste0('f', c(0:9, 'x')), 'x84')) {
  icpc2[grepl(icd_conversions$icpc2_icd10[i], icpc2, ignore.case = TRUE), 
        icd10 := paste(na.omit(icd10), i, sep = '_')]
}

icpc2[, icd10 := sub("^_","", icd10)]

## ICD-10 ----

icd10_a <- read_avohilmo_dg(y = y, dg_type = 'kayntisyy_icd10', cols_in = c('ICD10'))
icd10_b <- read_avohilmo_dg(y = y, dg_type = 'palvelutapahtuma',  cols_in = c('ULKOINEN_SYY'))

icd10_a %>% setnames(., tolower(names(.)))
icd10_b %>% setnames(., tolower(names(.)))

setnames(icd10_b, "ulkoinen_syy", "icd10")

icd10_0 <- rbindlist(list(icd10_a, icd10_b), fill = T)



if(settings$avo_only_psy_dgs == TRUE){
icd10_0 <- icd10_0[grepl(paste('F', dg_groups_of_interest['imtm'], sep = '|'), icd10, ignore.case = TRUE)]
}
icd10 <- icd10_0[, .(icd10 = paste(icd10, collapse = '_')), keyby = tapahtuma_tunnus]

icd10[, icd10 := gsub("\\.","",icd10)]

rm(icpc2_0, icd10_0)
gc()

# join diagnoses & filter ------------------------------------------------------

## combine dgs ----
dgs <- merge(
  icd10,
  icpc2[, .(tapahtuma_tunnus, converted = icd10)], 
  on = 'tapahtuma_tunnus', 
  all = TRUE
)
rm(icd10, icpc2)

dgs[icd10 %>% is.na(), icd10 := ""]
dgs[ converted %>% is.na(), converted := ""]

dgs[, dg_avo := paste0(icd10, converted)]

## join dg to avo, filter only rows w relevant dg ----

avo_dg <- merge(
  avo,
  dgs[,.(tapahtuma_tunnus, dg_avo)],
  by = "tapahtuma_tunnus",
  all.y = TRUE
)
rm(avo,dgs)
gc()

report_avo$relevant_dgs_shnro_na <- avo_dg[shnro %>% is.na(),.N]

# Excluded if no ID -----------------------------------------------------------

avo_dg <- avo_dg[!is.na(shnro)]

report_avo$relevant_dgs_nrow <- avo_dg[,.N]
report_avo$relevant_dgs_n_indiv <- avo_dg[,uniqueN(shnro)]

