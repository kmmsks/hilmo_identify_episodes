#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#
# Function definition

# Aggregataion on pre-precossed data parts
#
# param d1 data table input
# param years_1996_andafter TRUE if working with data on years 1996 or after
# return processed data
#
aggregate_inpatient_periods <- function(d1, years_1996_andafter = TRUE) {
  setorder(d1, shnro, TUPVA, LPVM)
  
  # threshold for the highest LPVM for episode_inpatient
  d1[, highest_so_far := shift(cummax(LPVM), fill = LPVM[1]), by = shnro]
  
  # the actual chaining: check if row admission date is greater than the highest discharge date so far (+ add_days)  
  
  d1[, `:=`(j_tulopvm = TUPVA[1], j_lahtopvm = LPVM[.N], subgroup = .GRP), 
     by = .(shnro, g = cumsum(TUPVA > highest_so_far + add_days)) ] ####
  
  # running count of persons episodes
  d1[order(subgroup), episode_inpatient := cumsum(!duplicated(subgroup)), by = shnro]
  
  
  ###
  ## d2: aggregate rows based on the chaining
  #
  
  d2 <- d1[,.(tulopvm = unique(j_tulopvm), lahtopvm = unique(j_lahtopvm), psy = any(psy > 0), n_rows_inpat = .N), 
           by = .(shnro, episode_inpatient)]
  
  # episode's start and end in psychiatry
  d2 <- merge(d2, 
              d1[psy == TRUE, .(tulopvm_psy_inpat = min(TUPVA), lahtopvm_psy_inpat = max(LPVM)), by = .(shnro, episode_inpatient)], 
              on = .(shnro, episode_inpatient), 
              all.x = TRUE)
  
  # episodes last diagnoses
  d2 <- merge(d2, 
              d1[d1[,.I[LPVM == max(LPVM)], by=.(shnro, episode_inpatient)]$V1, dg,  by = .(shnro, episode_inpatient)
                 ][, .(dg_inpat = paste0(na.omit(unique(dg)), collapse = '_')), by = .(shnro, episode_inpatient)], 
              on = .(shnro, episode_inpatient), 
              all.x = TRUE)
  
  # episodes last diagnoses in psychiatry
  d2 <- merge(d2, 
              d1[d1[psy == T,.I[LPVM == max(LPVM)], by = .(shnro, episode_inpatient)]$V1, dg,  by = .(shnro, episode_inpatient)
                 ][, .(dg_inpat_psy = paste0(na.omit(unique(dg)), collapse = '_')), by = .(shnro, episode_inpatient)], 
              on = .(shnro, episode_inpatient), 
              all.x=TRUE)
  
  # ea_list, all specialities during the episode
  d2 <- merge(d2, 
              d1[, .(ea_list = paste0(na.omit(unique(EA)), collapse = '_')), by = .(shnro, episode_inpatient)], 
              on = .(shnro, episode_inpatient), 
              all.x = TRUE)

  if (years_1996_andafter == TRUE){
    # last paltu of the episode: service provider from where discharged
    d2 <- merge(d2, 
                d1[d1[, .I[LPVM == max(LPVM)], by = .(shnro, episode_inpatient)]$V1, PALTU,  by = .(shnro, episode_inpatient)
                   ][, .(paltu = paste0(na.omit(unique(PALTU)), collapse = '_')), by = .(shnro, episode_inpatient)], 
                on=.(shnro, episode_inpatient), 
                all.x=TRUE)
    
    # last paltu of the episode's psychiatry: psychiatric service provider from where discharged
    d2 <- merge(d2, 
                d1[d1[psy == T, .I[LPVM == max(LPVM)], by = .(shnro, episode_inpatient)]$V1, PALTU,  by = .(shnro, episode_inpatient)
                   ][, .(paltu_psy = paste0(na.omit(unique(PALTU)), collapse = '_')), by = .(shnro, episode_inpatient)], 
                on = .(shnro, episode_inpatient), 
                all.x = TRUE)
  }
  
  d2
}

# // end