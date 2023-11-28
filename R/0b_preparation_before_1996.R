
# This script pre-processes Hilmo registers before 1996. The data are in three
# different formats based on period. Periods are pre-processed separately and 
# then combined and saved in longitudinal format. Meaning that each individuals' 
# all data is saved into a singe file (instead of having data saved in annual files).

# The process goes as follows:
## set year range 
## read data. define variables of interest
## harmonize column names
## convert dates 
## define psychiatric treatments
## process diagnosis variables
## set ilaji 
## convert diagnoses 

# Data harmonization and aggrataion
convert_cols_old <- function(){
  setnames(inpat_1, old=c("tulopvm_psy", "lahtopvm_psy", "dg_os_psy"), 
           new = c("tulopvm_psy_os", "lahtopvm_psy_os", "dg_psy"))
  inpat_1[, `:=`(psy_os = psy, os_hoito = TRUE)]
}


# years 1975 -1986 -------------------------------------------------------------

## set year range ----
old_start_year <- 1975
old_end_year   <- 1986

## read data ----
cols <- c('shnro', 'REKVV', 'ST', 'tulopvm', 'lahtopvm', 'EALA', 'DG1', 'DG2', 'DG3', 'DG4')

p <- file.path(dirs$hilmo_root, 'Hilmo', 'poisto_triad_7086.sas7bdat')

d0 <- read_sas(p, col_select =cols)
d0 %>% setDT()

## harmonize column names ----
setnames(d0, c('REKVV','EALA'), c('vuosi', 'ea'))
d0 %>% setnames(., tolower(names(.)))

## convert dates ----
frmt <- '%y/%m/%d'
d0[, tupva  := as.IDate(tulopvm, format = frmt) %>% as.integer()]
d0[, lpvm := as.IDate(lahtopvm,  format = frmt) %>% as.integer()]

d0[, `:=`(tulopvm = NULL, lahtopvm = NULL)]

d0 <- process_before_1996(d0)

## define psychiatric treatments ----
d0[,psy := FALSE]
d0[st == 'mp' | (st == 'ys' & vuosi %between% c(1976, 1979) & ea %in% c('70', '71', '72', '73', '74', '42'))
   | (st == 'ys' & vuosi %between% c(1980, 1985) & ea %in% c('70', '71', '72', '73', '74'))
   | (vuosi >= 1986 & ea %in% c('70', '74', '75') )
   , psy := TRUE
]

## Process diagnosis variables ----
d0[, ea := paste(st, ea, sep = '.')]

d0[, dg_old := paste(dg1, dg2, dg3, dg4, sep = '_') %>% gsub("__", "", .) %>% gsub("___", "", .) %>% gsub("_$", "", .)]

# all codes start with "_"
d0[, dg_old := paste0("_", dg_old)]

## set ilaji ----
# is introduced later to the registers. These old data is equivalent to ilaji 1

d0[, ilaji := 1]

## convert diagnoses ----
convert_to_icd_10(d0, from='icd-9')
  


# years 1987 -1993 -------------------------------------------------------------
old_start_year <- 1987
old_end_year   <- 1993

cols <- c('shnro', 'VUOSI', 'tulopv', 'lahtopv', 'EA', 'PDG', 'SDG1', 'SDG2', 'SDG3')
p <- file.path(dirs$hilmo_root, 'Hilmo_char', 'poisto_triad_8793_char.sas7bdat')

d0 <- read_sas(p, col_select = cols)
d0 %>% setDT()

d0 %>% setnames(., tolower(names(.)))


# harmonize column names
setnames(d0, c('pdg', 'sdg1', 'sdg2', 'sdg3'), c('dg1', 'dg2', 'dg3', 'dg4'))


# convert dates
frmt <- '%d/%m/%Y'
d0[, tupva  := as.IDate(tulopv, format = frmt) %>% as.integer()]
d0[, lpvm := as.IDate(lahtopv,  format = frmt) %>% as.integer()]

d0 <- process_before_1996(d0)


#define psychiatric treatments
d0[,psy := FALSE]
d0[ea %in% c(70, 74, 75), psy := TRUE]

d0[, dg_old := paste(dg1, dg2, dg3, dg4, sep = '_') %>% gsub("__", "", .) %>% gsub("___", "", .) %>% gsub("_$", "", .)]

# aloitetaan _-merkilla dg-kentta, jotta jokainen yksittainen koodi alkaa _:lla
d0[, dg_old := paste0("_", dg_old)]


d0[, ilaji := 1]

convert_to_icd_10(d0, from='icd-9')
  
assign(paste0('hilmo_', old_start_year), d0)
  


# years 1994 -1995 -------------------------------------------------------------
old_start_year <- 1994
old_end_year   <- 1995

cols <- c('shnro', 'PALTU', 'KOKU', 'PALA', 'EA', 'PDG', 'SDG1', 'SDG2', 'PITK', 'tulopvm', 'lahtopvm')
p <- file.path(dirs$hilmo_root, 'Hilmo_char', 'hilmo_triad_9495_char.sas7bdat')

d0 <- read_sas(p, col_select = cols)
d0 %>% setDT()

d0 %>% setnames(., tolower(names(.)))


# harmonize column names
setnames(d0, c('tulopvm', 'lahtopvm', 'pdg', 'sdg1', 'sdg2'), 
         c('tupva', 'lpvm', 'dg1', 'dg2', 'dg3'))

# convert dates
frmt <- '%d/%m/%y'
d0[, tupva  := as.IDate(tupva, format = frmt) %>% as.integer()]
d0[, lpvm :=   as.IDate(lpvm,  format = frmt) %>% as.integer()]

# vuosi and DG4 missing, add
d0[, `:=`(vuosi = lpvm %>% as.IDate() %>% year(), dg4 = '')]


#define psychiatric treatments
d0[,psy := FALSE]
d0[ ea %in% c('70', '70F', '70X', '70Z', '75', '75X'), psy := TRUE]

d0 <- process_before_1996(d0)

d0[, dg_old := paste(dg1, dg2, dg3, dg4, sep = '_') %>% gsub("__", "", .) %>% gsub("___", "", .) %>% gsub("_$", "", .)]

# aloitetaan _-merkilla dg-kentta, jotta jokainen yksittainen koodi alkaa _:lla
d0[, dg_old := paste0("_", dg_old)]

d0[, ilaji := 1]

convert_to_icd_10(d0, from='icd-9')
  
assign(paste0('hilmo_', old_start_year), d0)
  
gc()

# save longitudinal ------------------------------------------------------------

if (settings$old_to_longitudinal == TRUE) {
  
  dat <- rbindlist(list(hilmo_1975, hilmo_1987, hilmo_1994), fill = TRUE)
  
  dat[, id_group := shnro %>% substr(start = 1, stop = 1)][
    , fwrite(.SD, file.path(dirs$pre, paste0(id_group, '_before_1996.csv')), append = TRUE), by = id_group]
  
}


gc()
#rm(hilmo_1975, hilmo_1987,hilmo_1994, d0, dat)
