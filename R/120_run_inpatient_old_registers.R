#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#

# Registers 1969-1993 have different data forms than 1996 and after. 
# Datasets need harmonization

# In this file, go through settings for each time period with different data forms.
# After that, gun 121_describe_aggregate_save.R 

# Output in the same folder as years 1996 and after

# Source files -----------------------------------------------------------------
#
source(here('R', '012_functions_aggregate_inpatient_periods.R'))

# diagnosis conversions -------------------------------------------------------
# 1975-1986
icd8_icd10 <- list(
  f0 = c('_290|_292|_293|_294|_309'),
  f1 = c('_291|_303|_304'),
  f2 = c('_295|_297|_2981|_2982|_2989|_299'),
  f3 = c('_296|_2980|_3004|_2033'),
  f4 = c('_3000|_3001|_3002|_3003|_3005|_3006|_3007|_3008|_3009|_305|_2068|_307|_3084'),
  f5 = c('_3064|_3065'),
  f6 = c('_3010|_3012|_3013|_3014|_3015|_3016|_3017|_3019|_3019|_302'),
  f7 = c('_310|_311|_312|_313|_314|_315'),
  f8 = c('_3060|_3061|_3063|_3080'),
  f9 = c('_3062|_3066|_3067|_3069|_3081|_3082|_3083')
)

# 1987-1995
icd9_icd10 <- list(
  f0 = c('_290||_293|_294|_310'),
  f1 = c('_291|_292|_303|_304|_305'),
  f2 = c('_295|_297|_298|'),
  f3 = c('_296|_311'),
  f4 = c('_300|_3078|_309'),
  f5 = c('_3027|_3071|_3074|_3075|_316'),
  f6 = c('_301|_302|_312'),
  f7 = c('_317|_318|_319'),
  f8 = c('_299|_315'),
  f9 = c('_3080|_3072|_3073|_3076|_3077|_3079|_313|_314')
)




# years 1975 -1986 -------------------------------------------------------------
old_start_year <- 1975
old_end_year   <- 1986

setwd(dir[["old_registers"]])
d0 <- fread("data_in_7086.csv")

# harmonize column names
setnames(d0, c('REKVV','EALA', 'tulopvm', 'lahtopvm'), c('vuosi', 'EA', 'TUPVA', 'LPVM'))

# convert dates
d0[, TUPVA  := as.IDate(TUPVA, format = '%d/%m/%y')]
d0[, LPVM := as.IDate(LPVM, format = '%d/%m/%y')]

#define psychiatric treatments
d0[,psy := FALSE]
d0[ST == 'mp' | (ST == 'ys' & vuosi %between% c(1976, 1979) & EA %in% c('70', '71', '72', '73', '74', '42'))
   | (ST == 'ys' & vuosi %between% c(1980, 1985) & EA %in% c('70', '71', '72', '73', '74'))
   | (vuosi >= 1986 & EA %in% c('70', '74', '75') )
   , psy := TRUE
   ]
d0[, EA := paste(ST, EA, sep = '.')]


# run the aggregation
source(here('R', '121_describe_aggregate.R'))

# convert diagnoses ------------------------------------------------------------------

setnames(d2, c('dg_inpat_psy', 'dg_inpat'), c('dg_inpat_psy_icd8', 'dg_inpat_icd8'))

d2[, dg_inpat_psy_icd8 := paste0('_', dg_inpat_psy_icd8)]
d2[, dg_inpat_icd8 := paste0('_', dg_inpat_icd8)]

d2[, dg_inpat_psy := '']
d2[, dg_inpat := '']

for (i in 0:9){
  d2[grepl(icd8_icd10[paste0('f', i)], dg_inpat_psy_icd8, ignore.case = TRUE),
     dg_inpat_psy := paste(na.omit(dg_os_psy), paste0('F', i), sep = '_')]
  
  d2[grepl(icd8_icd10[paste0('f', i)], dg_inpat_icd8, ignore.case = TRUE),
     dg_inpat := paste(na.omit(dg_inpat), paste0('F', i), sep = '_')]
  
}


# save processed data -----------------------------------------------------------------

setwd(file.path(here(
  'data_processed',
  '1_inpatient_episodes',
  paste('add_days', add_days, sep = '_')
)))

write_fst(d2, paste0('data_inpatient_', old_start_year, '_', old_end_year, '.fst'), compress = compression)

rm(d2)
gc()


# years 1987 -1993 -------------------------------------------------------------------
old_start_year <- 1987
old_end_year   <- 1993

setwd(dir[["old_registers"]])
d0 <- fread("data_in_8793.csv")

# harmonize column names
setnames(d0, c('VUOSI', 'tulopv', 'lahtopv', 'PDG', 'SDG1', 'SDG2', 'SDG3'), 
         c('vuosi', 'TUPVA', 'LPVM', 'DG1', 'DG2', 'DG3', 'DG4'))

# convert dates
d0[, TUPVA  := as.IDate(TUPVA, format = '%d/%m/%Y')]
d0[, LPVM   := as.IDate(LPVM, format = '%d/%m/%Y')]

#define psychiatric treatments
d0[,psy := FALSE]
d0[EA %in% c(70, 74, 75), psy := TRUE]

# run the aggregation
source(here('R', '121_describe_aggregate.R'))


# convert diagnoses ------------------------------------------------------------------

setnames(d2, c('dg_inpat_psy', 'dg_inpat'), c('dg_inpat_psy_icd9', 'dg_inpat_icd9'))

d2[, dg_inpat_psy_icd9 := paste0('_', dg_inpat_psy_icd9)]
d2[, dg_inpat_icd9 := paste0('_', dg_inpat_icd9)]

d2[, dg_inpat_psy := '']
d2[, dg_inpat := '']

for (i in 0:9){
  d2[grepl(icd9_icd10[paste0('f', i)], dg_inpat_psy_icd9, ignore.case = TRUE),
     dg_inpat_psy := paste(na.omit(dg_os_psy), paste0('F', i), sep = '_')]

  d2[grepl(icd9_icd10[paste0('f', i)], dg_inpat_icd9, ignore.case = TRUE),
     dg_inpat := paste(na.omit(dg_inpat), paste0('F', i), sep = '_')]
  
}


# save processed data -----------------------------------------------------------------

setwd(file.path(here(
  'data_processed',
  '1_inpatient_episodes',
  paste('add_days', add_days, sep = '_')
)))


write_fst(d2, paste0('data_inpatient_', old_start_year, '_', old_end_year, '.fst'), compress = compression)

rm(d2)
gc()

# years 1994 -1995 --------------------------------------------------------------
old_start_year <- 1994
old_end_year   <- 1995

setwd(dir[["old_registers"]])
d0 <- fread("data_in_9495.csv")

# harmonize column names
setnames(d0, c('tulopvm', 'lahtopvm', 'PDG', 'SDG1', 'SDG2'), 
         c('TUPVA', 'LPVM', 'DG1', 'DG2', 'DG3'))

# vuosi and DG4 missing, add
d0[, `:=`(vuosi = 1994, DG4 = '')]

d0[, TUPVA  := as.IDate(TUPVA, format = '%d/%m/%y')]
d0[, LPVM   := as.IDate(LPVM, format = '%d/%m/%y')]

#define psychiatric treatments
d0[,psy := FALSE]
d0[ EA %in% c('70', '70F', '70X', '70Z', '75', '75X'), psy := TRUE]

# run the aggregation
source(here('R', '121_describe_aggregate.R'))


# convert diagnoses ------------------------------------------------------------------

setnames(d2, c('dg_inpat_psy', 'dg_inpat'), c('dg_inpat_psy_icd9', 'dg_inpat_icd9'))

d2[, dg_inpat_psy_icd9 := paste0('_', dg_inpat_psy_icd9)]
d2[, dg_inpat_icd9 := paste0('_', dg_inpat_icd9)]

d2[, dg_inpat_psy := '']
d2[, dg_inpat := '']

for (i in 0:9){
  d2[grepl(icd9_icd10[paste0('f', i)], dg_inpat_psy_icd9, ignore.case = TRUE),
     dg_inpat_psy := paste(na.omit(dg_os_psy), paste0('F', i), sep = '_')]
  
  d2[grepl(icd9_icd10[paste0('f', i)], dg_inpat_icd9, ignore.case = TRUE),
     dg_inpat := paste(na.omit(dg_inpat), paste0('F', i), sep = '_')]
}


# save processed data -----------------------------------------------------------------

setwd(file.path(here(
  'data_processed',
  '1_inpatient_episodes',
  paste('add_days', add_days, sep = '_')
)))


write_fst(d2, paste0('data_inpatient_', old_start_year, '_', old_end_year, '.fst'), compress = compression)

rm(d2)
gc()

rm(old_start_year, old_end_year)

# // end

