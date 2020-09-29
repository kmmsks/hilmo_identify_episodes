#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#

# Dataset need some harmonization

# source aggrgate-function
source(here('R', '012_functions_aggregate_inpatient_periods.R'))


# years 1975 -1986
old_start_year <- 1975
old_end_year   <- 1986

setwd(dir[["old_registers"]])
d0 <- fread("poisto_triad_7086.csv")

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


# run the aggregation (includes saving)
source(here('R', '121_describe_aggregate_save.R'))


# years 1987 -1993
old_start_year <- 1987
old_end_year   <- 1993

setwd(dir[["old_registers"]])
d0 <- fread("poisto_triad_8793.csv")

# harmonize column names
setnames(d0, c('VUOSI', 'tulopv', 'lahtopv', 'PDG', 'SDG1', 'SDG2', 'SDG3'), 
         c('vuosi', 'TUPVA', 'LPVM', 'DG1', 'DG2', 'DG3', 'DG4'))

# convert dates
d0[, TUPVA  := as.IDate(TUPVA, format = '%d/%m/%Y')]
d0[, LPVM   := as.IDate(LPVM, format = '%d/%m/%Y')]

#define psychiatric treatments
d0[,psy := FALSE]
d0[EA %in% c(70, 74, 75), psy := TRUE]

# run the aggregation (includes saving)
source(here('R', '121_describe_aggregate_save.R'))


# years 1994 -1995
old_start_year <- 1994
old_end_year   <- 1995

setwd(dir[["old_registers"]])
d0 <- fread("hilmo_triad_9495.csv")

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

# run the aggregation (includes saving)
source(here('R', '121_describe_aggregate_save.R'))

rm(old_start_year, old_end_year)
