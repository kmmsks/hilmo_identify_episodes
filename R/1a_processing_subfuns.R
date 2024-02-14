
# These functions contain the actual processing: identifying inpatient episodes
# and handling outpatient appointments that take place during inpatient care

identify_inpatient_episodes <- function(inpat_0, add_days, start_year, end_year, include_new_variables = TRUE){
  
  # Episode identification 
  
  # Possible subsetting by year, relevant for longitudinal data only.
  inpat_0 <- inpat_0[vuosi %between% c(start_year, end_year)]

  # Order by start date and end date of the register entry
    setkey(inpat_0, shnro, tupva, lpvm)
  
  # Threshold for the highest lpvm (= end date)
  inpat_0[, highest_so_far := shift(cummax(lpvm), fill = lpvm[1]), by = shnro]
  
  # Chaining of the overlapping dates. 
  # If tupva (start date) is higher than the highest end date + the add_days value before the current row,
  # a new episode starts. Otherwise, the row is recognized to be a part of the episode that is already
  # ongoing in the previous row(s).
  
  inpat_0[, `:=`(subgroup = .GRP), 
          by = .(shnro, g = cumsum(tupva > highest_so_far + add_days)) ]
  
  # Running number of episode
  inpat_0[order(subgroup), episode := cumsum(!duplicated(subgroup)), by = shnro]
  
  # Define if the inpatient treatment continues at the last day covered by the current data
  
  inpat_0[, episode_continues := 0]
  inpat_0[, episode_psy_continues := 0]
  inpat_0[lpvm == last_day_of(end_year) & ilaji == 2,
          episode_continues := 1]
  inpat_0[psy == TRUE & lpvm == last_day_of(end_year) & ilaji == 2,
          episode_psy_continues := 1]
  
  
  # episode aggregation ----------------------------------------------------------
  
  # Now we know which rows belong to some episodes. Aggregate each episode into 
  # one row.
  
  # The Finnish variable names names tulopvm admission date and lahtopvm discharge date are used.
  
  inpat_1 <- inpat_0[,.(tulopvm = min(tupva), lahtopvm = max(lpvm), psy = any(psy > 0),
                        episode_continues = any(episode_continues == 1),
                        episode_psy_continues = any(episode_psy_continues == 1),
                        ilaji_2_n = sum(ilaji ==2), n_rows = .N, 
                        dg_all = paste(dg, collapse = "_")), 
                     by = .(shnro, episode)]
  
  # Start and end date of psychiatric inpatient care 
  # (may or may not be the same dates as the start and end of the whole episode)
  
  inpat_1[inpat_0[psy == T, .(tupva = min(tupva), lpvm = max(lpvm)), .(shnro, episode)],
          on =.(shnro, episode), c("tulopvm_psy", "lahtopvm_psy") := .(tupva, lpvm) ]
  
  
  # Diagnoses at the last day of the inpatient episodes == discharge diagnoses
  
  dat <- inpat_0[inpat_0[,.I[lpvm == max(lpvm)], by=.(shnro, episode)]$V1, dg,  by = .(shnro, episode)
  ][, .(dg_inpat = paste0(na.omit(unique(dg)), collapse = "_")), by = .(shnro, episode)]
  
  inpat_1[dat, on =.(shnro, episode), c("dg_inpat"):= .(dg_inpat)]
  
  # Diagnoses at the last day of the inpatient care at psychiatry == psychiatric discharge diagnoses

    dat <- inpat_0[inpat_0[psy == T,.I[lpvm == max(lpvm)], by = .(shnro, episode)]$V1, dg,  by = .(shnro, episode)
  ][, .(dg_inpat_psy = paste0(na.omit(unique(dg)), collapse = "_")), by = .(shnro, episode)]
  
  inpat_1[dat, on =.(shnro, episode), c("dg_inpat_psy"):= .(dg_inpat_psy)]
  
  
  # List of all medical specialties included in the episode (refer to register manuals for coding)
  dat <- inpat_0[, .(ea_list = paste0(na.omit(unique(ea)), collapse = "_")), by = .(shnro, episode)]
  
  inpat_1[dat, on =.(shnro, episode), c("ea_list"):= .(ea_list)]
  
  ### if data are ONLY before 1996, some of the variables are not present ------
  #   and the following cannot be performed
    if (include_new_variables == TRUE){
    
    # The last value of the variable paltu (this is a kind of service provider ID)
    dat <- inpat_0[inpat_0[, .I[lpvm == max(lpvm)], by = .(shnro, episode)]$V1, paltu,  by = .(shnro, episode)
    ][, .(paltu = paste0(na.omit(unique(paltu)), collapse = "_")), by = .(shnro, episode)]
    
    inpat_1[dat, on =.(shnro, episode), c("paltu"):= .(paltu)]
    
    
    # The last value of the variable paltu in psychiatry (this is a kind of service provider ID)
    
    dat <- inpat_0[inpat_0[psy == T, .I[lpvm == max(lpvm)], by = .(shnro, episode)]$V1, paltu,  by = .(shnro, episode)
    ][, .(paltu_psy = paste0(na.omit(unique(paltu)), collapse = "_")), by = .(shnro, episode)]
    
    inpat_1[dat, on =.(shnro, episode), c("paltu_psy"):= .(paltu_psy)]
    
    
    # home municipality of the patient at the start of the episode
    
    dat <- inpat_0[inpat_0[, .I[tupva == min(tupva)], by = .(shnro, episode)]$V1, koku,  by = .(shnro, episode)
    ][, .(koku = paste0(na.omit(unique(koku)), collapse = "_")), by = .(shnro, episode)]
    
    inpat_1[dat, on =.(shnro, episode), c("koku"):= .(koku)]
    
    
    # urgency of the episode
    
    inpat_0[kiireellisyys == "", kiireellisyys := NA_character_]
    dat <- inpat_0[inpat_0[, .I[lpvm == min(lpvm)], by = .(shnro, episode)]$V1, kiireellisyys,  by = .(shnro, episode)
    ][, .(kiireellisyys = paste0(na.omit(unique(kiireellisyys)), collapse = "_")), by = .(shnro, episode)]
    
    inpat_1[dat, on =.(shnro, episode), c("kiireellisyys"):= .(kiireellisyys)]
  }
  
  # Recognize overnight episodes
  
  inpat_1[, overnight := ifelse(tulopvm<lahtopvm, TRUE, FALSE)]
  inpat_1[, overnight_psy := ifelse(tulopvm_psy<lahtopvm_psy, TRUE, FALSE)]
  inpat_1[overnight_psy %>% is.na(), overnight_psy := FALSE]
  
  ## mark inpatient rows, all rows in this dataset
  inpat_1[, `:=`(inpat = 1, outpat = 0, primary_care = 0)]
  
  inpat_1[]
}



process_outpatient_data <- function(outpat_0, inpat_1, start_year, end_year) {
  
  # Possilbe subsetting by year, relevant for longitudinal data only.
  outpat_0 <- outpat_0[vuosi %between% c(start_year, end_year)]
  
  # Harmonize variable names
  setnames(outpat_0, old = c("dg", "tupva", "lpvm"), new = c("dg_outpat", "tulopvm", "lahtopvm"))
  
  # Mark outpatient rows
  outpat_0[, `:=`(episode = 0)]
  
  
  # combine inpatient and outpatient data
  d1 <- rbindlist(list(inpat_1, outpat_0), fill = T)
  
  gc()
  
  # Identification of appointments inside inpatient episodes -------------------

  # Same logic as in inpatient care. No add_days here  
  setkey(d1, shnro, tulopvm, lahtopvm)

  d1[, highest_so_far := shift(cummax(lahtopvm), fill = lahtopvm[1]), by = shnro]
  
  # Chaining of entries (rows) together
  d1[, `:=`(subgroup=.GRP), by = .(shnro, g = cumsum(tulopvm > highest_so_far))]
  
  # Event number. Event refers to inpatient episodes with possible outpatient 
  # appointments within, or to outpatient appointments outside inpatient episodes.
  
  d1[order(subgroup), event := cumsum(!duplicated(subgroup)), by = shnro]
  
  
  # aggregations: combine overlapping events -----------------------------------
  
  d1[outpat > 0, n_rows := 1]
  
  setorder(d1, shnro, tulopvm, -lahtopvm)
  
  # Mark number of rows in each event,  number of inpatient episodes, and number of 
  # psychiatric rows 
  d1[, `:=`(sum_n_rivi = sum(n_rows), sum_episode = sum(episode), sum_psy = sum(psy) ), .(shnro, event)]
  
  
  # Events with different compositions are aggregated separately and the aggregate events are
  # collected to list called lst:
  
  lst <- list()
  
  # a: Event consists of one outpatient appointment. Do nothing except select the variables of interest:
  
  # select event with one row and no inpatient episode:
  lst$a <- d1[
    sum_n_rivi == 1 & sum_episode == 0, 
    .(shnro, 
      event,
      tulopvm, 
      lahtopvm,
      psy,
      n_rows, 
      paltu, 
      #  koku, kiireellisyys, yhteystapa, pala, # if you need these
      ea_list = ea,
      dg_outpat,
      dg_all = dg_outpat
    )]
  
  # mark these events as follows:
  lst$a[,`:=`(inpat_psy = FALSE, inpat = FALSE, outpat = TRUE)]
  
  
  # b: Event consists of more than one outpatient appointment. Do nothing except mark these events and select the variables of interest.

    lst$b <- d1[sum_n_rivi > 1 & sum_episode == 0,
              .(shnro, event,tulopvm, lahtopvm , n_rows, psy, paltu,
                #tulopvm_psy = NA, lahtopvm_psy = NA,
                # koku, kiireellisyys, yhteystapa, pala, # if needed
                ea_list = ea,
                #dg_inpat = NA, dg_inpat_psy = NA,
                dg_outpat, dg_all = dg_outpat)]
  
    # mark these events as follows:
    lst$b[,`:=`(inpat_psy = FALSE, inpat = FALSE, outpat = TRUE, outpat_same_day = TRUE)]
  

  # c: Inpatient care, event consists of only one row. Do nothing
    
    lst$c <- d1[sum_n_rivi == 1 & sum_episode != 0,
              .(shnro, event,tulopvm, lahtopvm , tulopvm_psy, lahtopvm_psy, episode_continues,
                episode_psy_continues, ilaji_2_n, n_rows, psy,  paltu, 
                #paltu_psy, koku, kiireellisyys, yhteystapa, pala, # if needed 
                ea_list = ea,
                dg_inpat, dg_inpat_psy, dg_outpat, dg_all)]
    
    # mark these events as follows:
    lst$c[,`:=`(inpat_psy = psy, inpat = TRUE, outpat = FALSE)]
  
  # d: Event consists of inpatient care and outpatient appointments, no psychiatry included.
    
    # All diagnoses are collected
    lst$d <- d1[sum_n_rivi > 1 & sum_episode != 0 & sum_psy == 0,
              .(tulopvm = min(tulopvm), 
                lahtopvm = max(lahtopvm), 
                #psy = sum(psy),
                episode_continues, episode_psy_continues,
                ilaji_2_n = sum(ilaji_2_n, na.rm = T) >= 1,
                n_rows = sum(n_rows), 
                tulopvm_psy = min(tulopvm_psy), 
                lahtopvm_psy = max(lahtopvm_psy), 
                paltu = paste0(na.omit(unique(paltu)), collapse = "_"), 
                #   paltu_psy = paste0(na.omit(unique(paltu_psy)), collapse = "_"), 
                #    koku = paste0(na.omit(unique(koku)), collapse = "_"), # if needed
                #    kiireellisyys = paste0(na.omit(unique(kiireellisyys)), collapse = "_"), # if needed 
                #    yhteystapa = paste0(na.omit(unique(yhteystapa)), collapse = "_"), # if needed
                #   pala = paste0(na.omit(unique(pala)), collapse = "_"), # if needed
                ea_list = paste0(na.omit(unique(ea_list)), na.omit(unique(ea)), collapse = "_"), 
                dg_inpat = paste0(na.omit(unique(dg_inpat)), collapse = "_"), 
                #    dg_inpat_psy = NA, 
                dg_outpat = paste0(na.omit(unique(dg_outpat)), collapse = "_"),
                dg_all = paste0(na.omit(unique(dg_all)), na.omit(unique(dg_outpat)), collapse = "_")
              ), 
              .(shnro, event)]
    
    # mark these events as follows:
    lst$d[,`:=`(inpat_psy = FALSE, inpat = TRUE, outpat = TRUE, psy = FALSE)]
  
  
    # e: Event consists of inpatient care and outpatient appointments, PSYCHIATRY included.
    
    # First inpatient data:
    lst$e_inpat <- d1[sum_n_rivi > 1 & sum_episode != 0 & sum_psy > 0 & episode > 0,
                    .(shnro, event,tulopvm, lahtopvm , psy, tulopvm_psy, lahtopvm_psy,
                      episode_continues, episode_psy_continues, ilaji_2_n, n_rows,  
                      paltu, 
                      #paltu_psy, koku, kiireellisyys, yhteystapa, pala, # if needed
                      ea_list = ea, inpat,
                      inpat_psy = psy, dg_inpat, dg_inpat_psy, dg_outpat, dg_all)]
  
    # Outpatient data:
    # If the medical specialty is not psychiatry, psychiatric diagnoses are excluded:
    e_outpat_no_psy <- d1[sum_n_rivi > 1 & sum_episode != 0 & sum_psy > 0 & episode == 0 & psy == FALSE,
                        .(dg_outpat_no_psy = paste0(na.omit(unique(dg_outpat)), collapse = "_") %>% gsub("F.*?_", "", .) %>% gsub("F.*", "",.) ,
                          ea_list = paste0(na.omit(unique(ea_list)), na.omit(unique(ea)), collapse = "_")), 
                        .(shnro, event)]
  
    # outpatient visits in psychiatry
    # All diagnoses included
    e_outpat_psy <- d1[sum_n_rivi > 1 & sum_episode != 0 & sum_psy > 0 & episode == 0 & psy == TRUE,
                     .(lahtopvm = max(lahtopvm),
                       dg_outpat_psy = paste0(na.omit(unique(dg_outpat)), collapse = "_"),
                       ea_list = paste0(na.omit(unique(ea_list)), na.omit(unique(ea)), collapse = "_")
                     ), 
                     .(shnro, event)]
  
  
    # Recognize psychiatric outpatient diagnoses that are set before the date of discharge from psychiatry
    
    # update join: psychiatric discharge date to psychiatric outpatient data
    e_outpat_psy[lst$e_inpat, on = c("shnro", "event"), c("lahtopvm_psy") := .(i.lahtopvm_psy)]
  
    # collect outpatient diagnoses that are not from psychiatry (psychiatric diagnoses excluded) and
    # psychiatric outpatient diagnoses at or after discharge from psychiatry

    e_outpat <- merge(
      e_outpat_no_psy,
      e_outpat_psy[is.na(lahtopvm_psy) | (!is.na(lahtopvm_psy) & lahtopvm >= lahtopvm_psy) , .(shnro, event, dg_outpat_psy, ea_list)],
      by= c("shnro", "event"),
      all = T
    )
  
    # list medical specialties
    e_outpat[, `:=`(ea_in = paste0(ea_list.x, "_", ea_list.y))]
  
    # all outpatient dgs
    e_outpat_all <- merge(
      e_outpat_no_psy[,.(shnro, event, dg_outpat_no_psy)],
      e_outpat_psy[,.(shnro, event, dg_outpat_psy)],
      by= c("shnro", "event"),
      all = T
    )
  
    e_outpat_all[,dg_outpat_all := paste(dg_outpat_no_psy, dg_outpat_psy, sep = "_")]
  
    ### join the processed outpatient diagnoses to the underlying inpatient events: ----
    lst$e_inpat[e_outpat, on = c("shnro", "event"), 
                c("dg_outpat_psy", "dg_outpat_no_psy", "ea_in") := .(i.dg_outpat_psy, i.dg_outpat_no_psy, i.ea_in)]
    
    lst$e_inpat[e_outpat_all, on = c("shnro", "event"), 
                c("dg_outpat_all") := .(i.dg_outpat_all)]
    
    lst$e_inpat[, dg_all := paste(dg_all, dg_outpat_all, sep = "_")]
    lst$e_inpat[, dg_outpat_all := NULL]
    
    lst$e_inpat[, ea_list := paste(ea_list, ea_in, sep = "_")]
    lst$e_inpat[,ea_in := NULL]
    
    lst$e_inpat[dg_outpat_psy =="", dg_outpat_psy := NA_character_]
    
    lst$e_inpat[inpat_psy==F,.N,psy]
    lst$e_inpat[, `:=`(psy =TRUE, outpat = TRUE)]
    
    # bind aggregated events ---------------------------------------------------
    
    dat <- rbindlist(rev(lst), fill = TRUE)
    
    rm(lst, e_outpat, e_outpat_all, e_outpat_no_psy, e_outpat_psy)
    gc()
    
    
    # fix variables ----------------------------------------------------------
    dat[episode_continues %>% is.na(), episode_continues := FALSE]
    dat[episode_psy_continues %>% is.na(), episode_psy_continues := FALSE]
    
    
    dat[ilaji_2_n %>% is.na(), ilaji_2_n := 0]
    
    setnames(dat, old=c("tulopvm_psy", "lahtopvm_psy"), 
             new = c("tulopvm_inpat_psy", "lahtopvm_inpat_psy"))
    
    dat[,primary_care := FALSE]
    
    ## vuosi (year) ----
    
    dat[, vuosi := lahtopvm %>% as.IDate() %>% year()]
    
    
    ## Overnight variable ----
    
    dat[, overnight := FALSE]
    dat[, overnight_psy := FALSE]
    
    dat[tulopvm < lahtopvm, overnight := TRUE]
    dat[tulopvm_inpat_psy < lahtopvm_inpat_psy, overnight_psy := TRUE]
    

    # Psychiatric diagnoses ----------------------------------------------------
    
    # Collect psychiatric diagnoses into single variable:

    # Inpatient care:
    # Discharge diagnoses from psychiatric inpatient care and possible psychiatric outpatient
    # diagnoses after the discharge from psychiatry if inpatient care continues in different
    # medical specialty
    dat[inpat_psy == T, dg_psy := paste0(dg_inpat_psy, "_", dg_outpat_psy)]

    # Psychiatric outpatient diagnoses during inpatient care in another specialty
    # This may include general hospital psychiatry or contacts from psychiatric outpatient clinics 
    dat[inpat_psy == F & psy == T & inpat == T, dg_psy := dg_outpat_psy]

    # Psychiatric outpatient diagnoses outside inpatient care
    dat[inpat_psy == F & psy == T & inpat == F, dg_psy := dg_outpat]
    
    # helper variables removed
    dat[, `:=`(dg_outpat_psy = NULL, dg_outpat_no_psy = NULL)]
    
    # info ---------------
    
    # dg_inpat:     Discharge diagnoses
    # dg_inpat_psy  Psychiatric discharge diagnoses
    # dg_psy        All relevant psychiatric diagnoses
    
    # dg_outpat     Outpatient diagnoses and outpatient diagnoses during inpatient care. 
    #               No psychiatric diagnoses from other specialties during psychiatric inpatient care
    # dg_outpat_psy Outpatient diagnoses during inpatient care only from psychiatric outpatient services
    
    # outpat_same_day T if more than one outpatient appointments on the same day
    
    dat[]
}

process_primary_care <- function(prim_care_0, inpat_outpat_1, start_year, end_year){
  
  # we consider only starting days, there are very few overnight appointments and it is hard to tell
  # what they really are.
  
  prim_care <- prim_care_0[vuosi %between% c(start_year, end_year), 
                           .(tulopvm = min(tupva), 
                             dg_avo = paste(na.omit(dg_avo), collapse = "_"),
                             n_appointments = .N), 
                           by = .(shnro, lahtopvm = lpvm)]
  
  # mark avohilmo (primary care register) rows
  prim_care[, `:=` (primary_care = 1, inpat = FALSE, outpat = FALSE, inpat_psy = FALSE, psy = FALSE, overnight = FALSE,
                    overnight_psy = FALSE)]
  
  # create vuosi (year)
  prim_care[, vuosi := lahtopvm %>% as.IDate() %>% year()]
  
  
  # combine psy inpatient periods, psy outpatient (1 per day) ------------------
  # and primary care appointments
  
  d1 <- rbindlist(list(
    inpat_outpat_1[inpat_psy == TRUE, .(shnro, tulopvm, lahtopvm, psy, inpat_psy)], 
    inpat_outpat_1[psy == TRUE & inpat_psy == FALSE, head(.SD,1), keyby = .(shnro, tulopvm), .SDcols = c("lahtopvm", "psy", "inpat_psy", "vuosi")],
    prim_care), fill = TRUE )
  
  d1[, `:=`(inpat_psy = inpat_psy %>% as.integer(), psy = psy %>% as.integer())]
  
  setnafill(d1, fill = 0, cols = c("psy", "inpat_psy", "primary_care"))
  
  gc()
  
  # exclude primary care appointments during psychiatric inpatient periods or
  # on the day of secondary outpatient 
  
  setorder(d1, shnro, lahtopvm, -inpat_psy, -psy, -primary_care)
  
  
  # find the latest discharge day and compare rows to this maximum value:
  
  d1[, highest_so_far := shift(cummax(lahtopvm), fill = NA), by = shnro] 
  # now, fill is NA instead lahtopvm[1], to treat individuals' first rows correctly
  
  # mark if primary care appointment within an inpatient period or same day with outpatient event 
  d1[lahtopvm <= highest_so_far, remove_row := TRUE]
  
  # keep rows not marked
  d2 <- d1[is.na(remove_row)]
  d2[, remove_row := NULL]
  
  d2[primary_care == 1]
}

report_inpatient_processing <- function(){
  lst <- list()
  inpat_0[, overnight := ifelse(tupva < lpvm, TRUE, FALSE)]
  inpat_0[, overnight_psy := ifelse(tupva < lpvm & psy == T, TRUE, FALSE)]
  
  lst$flow$overnight_FALSE_inpat_nrow_pre_ketjutus <- inpat_0[,.N]
  lst$flow$overnight_FALSE_inpat_nrow_pre_ketjutus_psy <- inpat_0[psy==TRUE,.N]
  lst$flow$overnight_FALSE_inpat_nrow_post_ketjutus <- inpat_1[,.N]
  lst$flow$overnight_FALSE_inpat_nrow_post_ketjutus_psy <- inpat_1[psy==TRUE,.N]
  #
  lst$flow$overnight_FALSE_inpat_n_indiv_pre_ketjutus <- inpat_0[, uniqueN(shnro)]
  lst$flow$overnight_FALSE_inpat_n_indiv_pre_ketjutus_psy <- inpat_0[psy==TRUE, uniqueN(shnro)]
  lst$flow$overnight_FALSE_inpat_n_indiv_post_ketjutus <- inpat_1[, uniqueN(shnro)]
  lst$flow$overnight_FALSE_inpat_n_indiv_post_ketjutus_psy <- inpat_1[psy==TRUE, uniqueN(shnro)]
  # overinight TRUE
  lst$flow$overnight_TRUE_inpat_nrow_pre_ketjutus <- inpat_0[overnight == TRUE,.N]
  lst$flow$overnight_TRUE_inpat_nrow_pre_ketjutus_psy <- inpat_0[overnight_psy == TRUE,.N]
  lst$flow$overnight_TRUE_inpat_nrow_post_ketjutus <- inpat_1[overnight == TRUE,.N]
  lst$flow$overnight_TRUE_inpat_nrow_post_ketjutus_psy <- inpat_1[overnight_psy==TRUE,.N]
  #
  lst$flow$overnight_TRUE_inpat_n_indiv_pre_ketjutus <- inpat_0[overnight == TRUE, uniqueN(shnro)]
  lst$flow$overnight_TRUE_inpat_n_indiv_pre_ketjutus_psy <- inpat_0[overnight_psy==TRUE, uniqueN(shnro)]
  lst$flow$overnight_TRUE_inpat_n_indiv_post_ketjutus <- inpat_1[overnight == TRUE, uniqueN(shnro)]
  lst$flow$overnight_TRUE_inpat_n_indiv_post_ketjutus_psy <- inpat_1[overnight_psy==TRUE, uniqueN(shnro)]
  
  
  # ilaji2
  
  lst$variable$inpat_ilaji_2_post_ketjutus <- inpat_1[, .N, ilaji_2_n]
  
  lst
}


get_outpat_report <- function(dat_in){
  
  dat <- copy(dat_in)
  
  setorderv(dat, c("shnro", "tulopvm", "lahtopvm", "episode"))
  
  nums <- list(
    ## episode starts with secondary care psychiatric outpatient contact
    na = nrow(dat[shnro == shift(shnro) & episode > 0 & shift(episode) == 0 & tulopvm == shift(tulopvm)]),
    # psychiatric inpatient episode starts with secondary care psychiatric outpatient contact
    naP = nrow(dat[shnro == shift(shnro) & episode > 0 & 
                     shift(episode) == 0 & tulopvm == shift(tulopvm) & shift(psy) == T & psy == T]),
    # secondary care outpatient contact on the day before admission
    nb = nrow(dat[shnro == shift(shnro) & episode > 0 & shift(episode) == 0 & tulopvm == shift(tulopvm) + 1]),
    # psychiatric secondary care outpatient contact on the day before psychiatric admission
    nbP = nrow(dat[shnro == shift(shnro) & episode > 0 & shift(episode) == 0 & tulopvm == shift(tulopvm) + 1 & 
                     shift(psy) == T & psy == T]),
    # any secondary care outpatient contact preceding psychiatric inpatient episode
    ncP = nrow(dat[psy == T & shnro == shift(shnro) & episode > 0 & shift(episode) == 0 & tulopvm == shift(tulopvm) + 1]),
    # secondary outpatient contact within inpatient episode
    nd = nrow(dat[shnro == shift(shnro, type = "lead") & episode > 0 & shift(episode, type = "lead") == 0 & 
                    lahtopvm > shift(tulopvm, type = "lead")]),
    # psychiatric secondary care outpatient contact within psychiatric inpatient episode
    ndP = nrow(dat[shnro == shift(shnro, type = "lead") & episode > 0 & shift(episode, type = "lead") == 0 & 
                     lahtopvm > shift(tulopvm, type = "lead")  & psy == T & shift(psy) == T]),
    # secondary care outpatient contact on the last day of episode
    ne = nrow(dat[shnro == shift(shnro, type = "lead") & episode > 0 & shift(episode, type = "lead") == 0 & 
                    shift(tulopvm, type = "lead") == lahtopvm]),
    # psychiatric care outatient contact on the last day of psychiatric inpatient episode
    neP = nrow(dat[shnro == shift(shnro, type = "lead") & episode > 0 & shift(episode, type = "lead") == 0 & 
                     shift(tulopvm, type = "lead") == lahtopvm & psy == T & shift(psy) == T]),
    nf = dat[inpat!=1, .N],
    nfP = dat[inpat!=1 & psy == T, .N]
  )
  
  labs <- list(
    na = "inpatient period starts with outpatient appointment", 
    nb = "outpatient appointment the day before the start of an inpatient period",
    nd = "outpatient appointment within an inpatient period", 
    ne = "outpatient appointment on the last day of an inpatient period",
    naP = "psychiatric inpatient period starts with psychiatric outpatient appointment", 
    nbP = "psychiatric outpatient appointment the day before the start of a psychiatric inpatient period", 
    ncP = "any outpatient appointment the previous day of the start of a psychiatric inpatient period",   
    ndP = "psychiatric outpatient appointment within a psychiatric inpatient period", 
    neP = "psychiatric opoutpatient appointment on the last day of a psychiatric inpatient period",
    nf  = "N all outpatient rows before aggregation",
    nfP = "N psychiatric outpatient rows before aggregation")
  
  
  a <- labs %>% stack() %>% setDT()
  b <- nums %>% stack() %>% setDT()
  
  out <- a[b, on = "ind"]
  out[, psy := ifelse(ind %like% "P", 1,0) ]
  out[order(psy), .(lab = values, psy, value = i.values)]
}




