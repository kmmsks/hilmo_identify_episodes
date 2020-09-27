
aggregate_inpatient_periods <- function(d1, years_1996_andafter = TRUE) {
  
  setorder(d1, shnro, TUPVA, LPVM)
  
  # treshold for highest LPVM for jakso
  #str(d1)
  d1[, highest_so_far := shift(cummax(LPVM), fill = LPVM[1]), by = shnro]
  
  #ketjutus ## add_days
  
  d1[, `:=`(j_tulopvm = TUPVA[1], j_lahtopvm = LPVM[.N], subgroup = .GRP), 
     by = .(shnro, g = cumsum(TUPVA > highest_so_far + add_days)) ] ####
  
  #jakson numero
  d1[order(subgroup), jakso := cumsum(!duplicated(subgroup)), by = shnro]
  
  
  
  ###
  ## d2: kaikki jaksot aggregoituna
  #
  
  d2 <- d1[,.(tulopvm = unique(j_tulopvm), lahtopvm = unique(j_lahtopvm), psy = any(psy > 0), n_rivi = .N), 
           by = .(shnro, jakso)]
  
  #jakson alku ja loppu psykiatrialla
  d2 <- merge(d2, 
              d1[psy == TRUE, .(tulopvm_psy = min(TUPVA), lahtopvm_psy = max(LPVM)), by = .(shnro, jakso)], 
              on = .(shnro, jakso), 
              all.x = TRUE)
  
  #jakson viimeisin dg
  d2 <- merge(d2, 
              d1[d1[,.I[LPVM == max(LPVM)], by=.(shnro, jakso)]$V1, dg,  by = .(shnro, jakso)
                 ][, .(dg_os = paste0(na.omit(unique(dg)), collapse = '_')), by = .(shnro, jakso)], 
              on = .(shnro, jakso), 
              all.x = TRUE)
  
  #jakson viimeisin dg psykiatrialta
  d2 <- merge(d2, 
              d1[d1[psy == T,.I[LPVM == max(LPVM)], by = .(shnro, jakso)]$V1, dg,  by = .(shnro, jakso)
                 ][, .(dg_os_psy = paste0(na.omit(unique(dg)), collapse = '_')), by = .(shnro, jakso)], 
              on = .(shnro, jakso), 
              all.x=TRUE)
  
  # ea_list, kaikki jakson EA:t
  d2 <- merge(d2, 
              d1[, .(ea_list = paste0(na.omit(unique(EA)), collapse = '_')), by = .(shnro, jakso)], 
              on = .(shnro, jakso), 
              all.x = TRUE)

  if (years_1996_andafter == TRUE){
    # jakson viimeisin paltu
    d2 <- merge(d2, 
                d1[d1[, .I[LPVM == max(LPVM)], by = .(shnro, jakso)]$V1, PALTU,  by = .(shnro, jakso)
                   ][, .(paltu = paste0(na.omit(unique(PALTU)), collapse = '_')), by = .(shnro, jakso)], 
                on=.(shnro, jakso), 
                all.x=TRUE)
    
    # jakson viimeisin PALTU psy
    d2 <- merge(d2, 
                d1[d1[psy == T, .I[LPVM == max(LPVM)], by = .(shnro, jakso)]$V1, PALTU,  by = .(shnro, jakso)
                   ][, .(paltu_psy = paste0(na.omit(unique(PALTU)), collapse = '_')), by = .(shnro, jakso)], 
                on = .(shnro, jakso), 
                all.x = TRUE)
  }
  
  d2
}

# // end