
setwd(dir$d2_aggregated_all)

dir.create(here('data_processed', '1b_inpatient_all_combined'))

lst <- list.files(pattern = '.fst')
d0 <- rbindlist(lapply(lst, read_fst, as.data.table = TRUE), fill = TRUE)


# if lahtopvm is NA, treatment continues at the end of the data.
# set lahtopvm to high, that's necessary for aggregation.

d0[is.na(lahtopvm), lahtopvm := as.integer(as.IDate(paste(max(vuosi) + 1000, '12-31', sep = '-')))]

# order for aggregation

setorder(d0, shnro, tulopvm, lahtopvm)

# highest lahtopvm so far
d0[, highest_so_far := shift(cummax(lahtopvm), fill = lahtopvm[1]), by = shnro]


# the actual chaining: check if row admission date is greater than the highest discharge date so far (+ add_days)  

d0[, `:=`(subgroup = .GRP), 
   by = .(shnro, g = cumsum(tulopvm > highest_so_far + add_days)) ] ####


# running count of persons episodes
d0[order(subgroup), episode_inpatient := cumsum(!duplicated(subgroup)), by = shnro]


###
## d1: aggregate rows based on the chaining
#

d1 <- d0[,.(tulopvm = min(tulopvm), 
            lahtopvm = max(lahtopvm), 
            vuosi = max(vuosi), 
            psy = any(psy == TRUE), 
            n_rows_inpat = sum(n_rows_inpat),
            ea_list = paste0(na.omit(unique(ea_list)), collapse = '_')
            ), 
         by = .(shnro, episode_inpatient)]

# episode's start and end in psychiatry
d1 <- merge(d1, 
            d0[psy == TRUE, .(
              tulopvm_psy_inpat = min(tulopvm_psy_inpat), 
              lahtopvm_psy_inpat = max(lahtopvm_psy_inpat)
              ), 
              by = .(shnro, episode_inpatient)], 
            on = .(shnro, episode_inpatient), 
            all.x = TRUE)

# episode's last diagnoses
d1 <- merge(d1, 
            d0[d0[,.I[lahtopvm == max(lahtopvm)], by=.(shnro, episode_inpatient)]$V1, dg_inpat,  
               by = .(shnro, episode_inpatient)
               ][, .(dg_inpat = paste0(na.omit(unique(dg_inpat)), collapse = '_')), by = .(shnro, episode_inpatient)], 
            on = .(shnro, episode_inpatient), 
            all.x = TRUE)

# episode's last diagnoses in psychiatry
d1 <- merge(d1, 
            d0[d0[psy == TRUE, .I[lahtopvm == max(lahtopvm)], by = .(shnro, episode_inpatient)]$V1, dg_inpat_psy,  
               by = .(shnro, episode_inpatient)
               ][, .(dg_inpat_psy = paste0(na.omit(unique(dg_inpat_psy)), collapse = '_')), by = .(shnro, episode_inpatient)], 
            on = .(shnro, episode_inpatient), 
            all.x=TRUE)

# last paltu of the episode: service provider from where discharged
d1 <- merge(d1, 
            d0[d0[, .I[lahtopvm == max(lahtopvm)], by = .(shnro, episode_inpatient)]$V1, paltu,  
               by = .(shnro, episode_inpatient)
               ][, .(paltu = paste0(na.omit(unique(paltu)), collapse = '_')), by = .(shnro, episode_inpatient)], 
            on=.(shnro, episode_inpatient), 
            all.x=TRUE)

# last paltu of the episode's psychiatry: psychiatric service provider from where discharged
d1 <- merge(d1, 
            d0[d0[psy == TRUE, .I[lahtopvm == max(lahtopvm)], by = .(shnro, episode_inpatient)]$V1, paltu_psy,  
               by = .(shnro, episode_inpatient)
               ][, .(paltu_psy = paste0(na.omit(unique(paltu_psy)), collapse = '_')), by = .(shnro, episode_inpatient)], 
            on = .(shnro, episode_inpatient), 
            all.x = TRUE)

# process ------------------------------------------------------------------------

# mark episodes that contain any inpatient period lasting over a night 
d1[, overnight_all := FALSE]
d1[lahtopvm > tulopvm, overnight_all := TRUE]

# mark episodes that contain psychiatric inpatient period lasting over a night 
d1[, overnight_psy := FALSE]
d1[lahtopvm_psy_inpat > tulopvm_psy_inpat, overnight_psy := TRUE]

# lahtopvm max is last day + 1000 years, if treatment continues at the end of the data. 
# In this case, set lahtopvm NA
d1[, episode_continues := FALSE]
d1[lahtopvm == max(lahtopvm) & lahtopvm > as.integer(as.IDate('2050-01-01')), `:=`(lahtopvm = NA,
                                                                                   episode_continues = TRUE)]
rm(d0)
gc()




# save --------------------------------------------------------------------------------------------------------

write_fst(d1, here('data_processed', '1b_inpatient_all_combined', 'data_inpatient_all_years.fst'), compress = compression)

dat_all_inpatient <- d1

rm(d1)
gc()
setwd(here())
