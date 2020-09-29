#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#
#
# remove invalid entries and report their amounts -----------------------------

# year limits
d0 <- d0[vuosi %between% c(old_start_year, old_end_year)]

n_vals <- data.table(nrow_raw_data = nrow(d0))


# exclude entries with no identification to any person
d0 <- d0[!shnro=='']
n_vals[, nrow_notna_shnro := nrow(d0)]

# exclude entries with admission date missing
d0 <- d0[!is.na(TUPVA)]
n_vals[, nrow_notna_tupva := nrow(d0)]

# discharge date missing 
d0 <- d0[( !is.na(LPVM))]
n_vals[, nrow_notna_lpvm := nrow(d0)]
  
# exclude if discharge before admission
d0 <- d0[LPVM >= TUPVA]
n_vals[, nrow_lpvm_grthan_tupva := nrow(d0)]

# exclude dates that are after the last register yaer

d0 <- d0[TUPVA <= as.IDate(paste(old_end_year, '12', '31', sep = '-')) & LPVM <= as.IDate(paste(old_end_year, '12', '31', sep = '-'))]  
n_vals[, nrow_lpvm_tupva_too_big := nrow(d0)]
# this is also the number of valid rows
n_vals[, nrow_valid_rows := nrow(d0)]

n_vals[, nrow_psy := nrow(d0[psy==TRUE])]


# Preaparatiens for the aggregation --------------------------------------
# combine diagnoses to one columns
d0[, dg := paste(DG1, DG2, DG3, DG4, sep = '_')]
# remove extra _'s
d0[, dg := gsub("___", "", dg)]
d0[, dg := gsub("__", "", dg)]


# dates to integers for aggregation
d0[, `:=`(TUPVA = as.integer(TUPVA),
          LPVM  = as.integer(LPVM))]

# the aggregation function ------------------------------------------------------------

d2 <- aggregate_inpatient_periods(d0, years_1996_andafter = FALSE)


# save processed data -----------------------------------------------------------------

setwd(file.path(dir[["main"]], 
                               'data_processed', 
                               '1_inpatient_periods', 
                               paste('add_days', add_days, sep = '_')
                               )
      )

write_fst(d2, paste0('data_inpatient_', old_start_year, '_', old_end_year, '.fst'), compress = compression)




# descriptions out: 
n_vals[, `:=`(start_year = old_start_year,
              end_year = old_end_year,
              n_aggregated_periods = nrow(d2)
              )]

setwd(file.path(dir[['main']], 'data_processed', '1_inpatient_periods',  'preparation_description'))

write_xlsx(transpose(n_vals, keep.names = 'value')[, .(value, n = V1)],
           path = paste0('preparation_description_', '_add_days_', add_days, old_start_year, '_', old_end_year, '.xlsx')
)

rm(d0, d2, n_vals)
gc(
  
)
