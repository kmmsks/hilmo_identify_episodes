#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#
# Inpatient and outpatient episodes, combine.
#
# Source files -----------------------------------------------------------------
#
source(here('R', '013_functions_create_dirs.R'))


# input locations, folders created in script 110 --------------------------------

dir[['d0_prepared_parts']] <-
  file.path(here(
    'data_temp',
    'prepared_parts',
    paste('add_days', add_days, 'years', min_year, max_year, sep = '_')
  ))

dir[['d2_aggregated_data']] <-
  file.path(here(
    'data_processed',
    '1_inpatient_episodes',
    paste('add_days', add_days, sep = '_'),
    paste('parts_years', min_year, max_year, sep = '_')
  ))


aggregate_specialized_care_episodes <- function(part) {
  
  
  setwd(dir[['d0_prepared_parts']])
  d0 <- read_fst(paste0(part, '.fst'), as.data.table = TRUE)
  
  
  setwd(dir[['d2_aggregated_data']])
  d2 <- read_fst(paste0(part, '.fst'), as.data.table = TRUE)
  
  

  # collect outpatient entries staring one year earlier
  d0 <- d0[vuosi >= (outpatient_start_year - 1) & eval(parse(text = PALA_outpatient))]

  # outpatient visits in 66 rows last overnight. Can it be true?
  # Here only start date is concidered.
  
  d0[, `:=`(tulopvm = TUPVA, lahtopvm = TUPVA)]
  d0[,`:=`(TUPVA = NULL, LPVM = NULL)]

  # harmonize naems  
  setnames(d0, c('EA', 'dg', 'PALTU'), c('ea_list', 'dg_outpat', 'paltu'))
  
  #names(d0)
  d0 <- d0[, .(shnro, paltu, ea_list, dg_outpat, psy, tulopvm, lahtopvm, PALA)]
  
  d0[, episode_inpatient := 0]
  
  
  # collect inpatient episodes
  outpatient_start_date <-  as.integer(as.IDate(paste0(outpatient_start_year, '-01-01'))) # esim. 2007-01-01
  
  d2 <- d2[tulopvm >= outpatient_start_date]
  
  # number of inpatient periods
  d2_nrow <- nrow(d2)
  d2_psyT_nrow <- nrow(d2[psy == T])
  
  # treatments that continue after the end of study period, have LPVM == NA.
  # For this aggregation script, set na to high value and back after aggregation (tech reason)
  
  # set LPVM to high, that's necassery for aggregation. 
  d2[is.na(lahtopvm), lahtopvm := as.integer(as.IDate(paste(max(vuosi)+1000, '12-31', sep = '-')))]
  
  
  ## d3 contains aggregated inpatient episodes and all outpatient entries
  d3 <- rbindlist(list(d0, d2), fill = T)
  rm(d0, d2)

  
  
    
  # describe episodes ---------------------------------------------------------------------------
  setorderv(d3, c('shnro', 'tulopvm', 'lahtopvm', 'episode_inpatient'))
  
  ## inpatient period starts with outpatient visit
  na <-
    nrow(d3[shnro == shift(shnro) &
              episode_inpatient > 0 & shift(episode_inpatient) == 0 & tulopvm == shift(tulopvm)])
  ## psychiatric inpatient period starts with psychiatric outpatient visit

  naP <- nrow(d3[shnro == shift(shnro) & episode_inpatient > 0 &
                   shift(episode_inpatient) == 0 &
                   tulopvm == shift(tulopvm) & shift(psy) == T & psy == T])
  
  #outpatient visit the day before inpatient admission
  nb <- nrow(d3[shnro == shift(shnro) & episode_inpatient > 0 & shift(episode_inpatient) == 0 & tulopvm == shift(tulopvm) + 1])
  # psychiatric outpatient visit the day before psychiatric inpatient admission

  nbP <-
    nrow(d3[shnro == shift(shnro) &
              episode_inpatient > 0 & shift(episode_inpatient) == 0 & tulopvm == shift(tulopvm) + 1 &
              shift(psy) == T & psy == T])
  
  # what ever outpatient visit the day before psychiatric inpatient admission
  ncP <-
    nrow(d3[psy == T &
              shnro == shift(shnro) &
              episode_inpatient > 0 & shift(episode_inpatient) == 0 & tulopvm == shift(tulopvm) + 1])
  
  # outpaitent visit within an inpatient period
  nd <-
    nrow(d3[shnro == shift(shnro, type = 'lead') &
              episode_inpatient > 0 & shift(episode_inpatient, type = 'lead') == 0 &
              shift(tulopvm, type = 'lead') < lahtopvm])
  # psychiatric outpatient visit within a psychiatric inpatient period
  ndP <-
    nrow(d3[shnro == shift(shnro, type = 'lead') &
              episode_inpatient > 0 & shift(episode_inpatient, type = 'lead') == 0 &
              shift(tulopvm, type = 'lead') < lahtopvm &
              psy == T & shift(psy) == T])
  
  # outpatient visit the day ef discharge
  ne <-
    nrow(d3[shnro == shift(shnro, type = 'lead') &
              episode_inpatient > 0 & shift(episode_inpatient, type = 'lead') == 0 &
              shift(tulopvm, type = 'lead') == lahtopvm])
  # psychiatric outpatient visit the day of psychiatric discharge

  neP <-
    nrow(d3[shnro == shift(shnro, type = 'lead') &
              episode_inpatient > 0 & shift(episode_inpatient, type = 'lead') == 0 &
              shift(tulopvm, type = 'lead') == lahtopvm &
              psy == T & shift(psy) == T])
  
  
  descr_all_episodes <-
    data.table(
      episode = c(
        'inpatient period starts with outpatient appointment',
        'outpatient appointment the previous day of the start of an inpatient period',
        'NA',
        'outpatient appointment within an inpatient period',
        'outpatient appointment on the last day of an inpatient period',
        'number of hospital treatment periods (d2_nrow)'
      ),
      n = list(na,
               nb,
               NA,
               nd,
               ne,
               d2_nrow)
    )[, sample := 'all']
  
  descr_psy_episodes <-
    data.table(
      episode = c(
        'psychiatric inpatient period starts with psychiatric outpatient appointment',
        'psychiatric outpatient appointment the previous day of the start of a psychiatric inpatient period',
        'what ever outpatient appointment the previous day of the start of a psychiatric inpatient period',
        'psychiatric outpatient appointment within a psychiatric inpatient period',
        'psychiatric opoutpatient appointment on the last day of a psychiatric inpatient period',
        'number of psychiatric hospital treatment periods (d2_psyT_nrow)'
      ),
      n = list(naP,
               nbP,
               ncP,
               ndP,
               neP,
               d2_psyT_nrow)
    )[, sample := 'psy']
  
  descr_episodes <- rbindlist(list(descr_all_episodes, descr_psy_episodes))
  
  descr_episodes[, `:=`(n = as.numeric(n))]
  
  
  # aggregation of outpatient appointments with inpatient periods ---------------------------------
  # 
  # a combination on inpatient and outpatient register entries is here called an episode
  
  setorderv(d3, c('shnro', 'tulopvm', 'lahtopvm'))
  
  d3[, highest_so_far := shift(cummax(lahtopvm), fill = lahtopvm[1]), by = shnro]
  
  # chaining of entries (rows) together
  d3[, `:=`(subgroup=.GRP), by = .(shnro, g = cumsum(tulopvm > highest_so_far))]
  #episode number
  d3[order(subgroup), episode := cumsum(!duplicated(subgroup)), by = shnro]
  
  #episodes are now regocnized. 
  
  # Next, combine episodes:

  # n_rows_inpat is na if only one row in the episode. replace with value 1.
  d3[is.na(n_rows_inpat), n_rows_inpat := 1]
  
  setorder(d3, shnro, tulopvm, -lahtopvm)
  
  # one outpatient visit on one day, do nothing
  a <-
    d3[, if (.N == 1 & sum(episode_inpatient) == 0)
      .(
        tulopvm            = min(tulopvm),
        lahtopvm           = max(lahtopvm),
        n_rows_episode     = sum(n_rows_inpat),
        psy                = sum(psy, na.rm = T) > 0,
        tulopvm_psy_inpat  = NA,
        lahtopvm_psy_inpat = NA,
        dg_inpat           = NA,
        dg_inpat_psy       = NA,
        dg_outpat          = paste0(na.omit(unique(dg_outpat)), collapse = '_'),
        paltu              = paste0(na.omit(unique(paltu)), collapse = '_'),
        paltu_psy          = NA,
        ea_list            = paste0(na.omit(unique(ea_list)), collapse = '_')
      ), by = .(shnro, episode)]
  
  # # more than one outpatient visit on one day, do nothing but mark these rows, so these rows can be combined if needed later
  b <- 
    d3[, if(.N > 1 & sum(episode_inpatient) == 0) 
      .(tulopvm, lahtopvm, psy, tulopvm_psy_inpat, lahtopvm_psy_inpat, 
        dg_inpat, dg_inpat_psy, dg_outpat, paltu, paltu_psy, ea_list), by = .(shnro, episode)]
  
  a[,`:=`(inpatient_psy = FALSE, inpatient = FALSE, dg_inpat_psy_outpat_psy = NA, outpat_same_day = FALSE)]  # type pkl
  b[,`:=`(inpatient_psy = FALSE, inpatient = FALSE, dg_inpat_psy_outpat_psy = NA, n_rows_episode = 1, outpat_same_day = TRUE)]
  
  # inpatient care, no psychiatry, one row in the episode, do nothing
  c <-
    d3[, if (.N == 1 & sum(episode_inpatient) > 0 & sum(psy) == 0)
      .(
        tulopvm            = min(tulopvm),
        lahtopvm           = max(lahtopvm),
        n_rows_episode     = sum(n_rows_inpat),
        tulopvm_psy_inpat  = NA,
        lahtopvm_psy_inpat = NA,
        dg_inpat           = paste0(na.omit(unique(dg_inpat)), collapse = '_'),
        dg_inpat_psy       = NA,
        dg_outpat          = NA,
        paltu              = paste0(na.omit(unique(paltu)), collapse = '_'),
        paltu_psy          = NA,
        ea_list            = paste0(na.omit(unique(ea_list)), collapse = '_')
      ), by = .(shnro, episode)]

    c[,`:=`(inpatient_psy = FALSE, inpatient = TRUE, psy = FALSE, dg_inpat_psy_outpat_psy = NA, outpat_same_day = FALSE)]
  
  #  psychiatric inpatient care, one row in the episode, do nothing
    cc <-
      d3[, if (.N == 1 &  sum(episode_inpatient) > 0 & sum(psy) > 0)
        .(
          tulopvm            = min(tulopvm),
          lahtopvm           = max(lahtopvm),
          n_rows_episode     = sum(n_rows_inpat),
          tulopvm_psy_inpat  = min(tulopvm_psy_inpat),
          lahtopvm_psy_inpat = max(lahtopvm_psy_inpat),
          dg_inpat           = na.omit(unique(dg_inpat)),
          dg_inpat_psy       = na.omit(unique(dg_inpat_psy)),
          dg_outpat          = NA,
          paltu              = na.omit(unique(paltu)),
          paltu_psy          = na.omit(unique(paltu_psy)) ,
          ea_list            = paste0(na.omit(unique(ea_list)), collapse = '_')
        ), by = .(shnro, episode)]
    
  cc[,`:=`(inpatient_psy = TRUE, inpatient = TRUE, psy = TRUE, dg_inpat_psy_outpat_psy = NA,outpat_same_day = FALSE)]
  
  
  
  # inpatient care and outpatient visits durning it, no psychiatry, collapse all data
  d <-
    d3[, if (.N > 1 & sum(episode_inpatient) > 0 & sum(psy) == 0)
      .(
        tulopvm            = min(tulopvm),
        lahtopvm           = max(lahtopvm),
        n_rows_episode     = sum(n_rows_inpat),
        tulopvm_psy_inpat  = NA,
        lahtopvm_psy_inpat = NA,
        dg_inpat           = paste0(na.omit(unique(dg_inpat)), collapse = '_'),
        dg_inpat_psy       = NA,
        dg_outpat          = paste0(na.omit(unique(dg_outpat)), collapse = '_'),
        paltu              = paste0(na.omit(unique(paltu)), collapse = '_'),
        paltu_psy          = NA,
        ea_list            = paste0(na.omit(unique(ea_list)), collapse = '_')
      ), by = .(shnro, episode)]

  d[,`:=`(inpatient_psy = FALSE, inpatient = TRUE, psy = FALSE, dg_inpat_psy_outpat_psy = NA, outpat_same_day = FALSE)]
  
  # psychiatric inpatient care and outpatient visits during it. Outpatient diagnoses are collected to separate variables
  #
  e1 <- d3[, if(.N > 1 & sum(episode_inpatient) > 0 & sum(psy) > 0 & any(episode_inpatient > 0 & psy == T)) .SD, by = .(shnro, episode)]
  
  e  <-
    e1[, .(
      tulopvm            = min(tulopvm),
      lahtopvm           = max(lahtopvm),
      n_rows_episode     = sum(n_rows_inpat, na.rm = T),
      tulopvm_psy_inpat  = min(tulopvm_psy_inpat, na.rm = T),
      lahtopvm_psy_inpat = max(lahtopvm_psy_inpat, na.rm = T),
      dg_inpat           = paste0(na.omit(unique(dg_inpat)), collapse = '_'),
      dg_inpat_psy       = paste0(na.omit(unique(dg_inpat_psy)), collapse = '_'),
      paltu              = paste0(na.omit(unique(paltu)), collapse = '_'),
      paltu_psy          = paste0(na.omit(unique(paltu_psy)), collapse = '_'),
      ea_list            = paste0(na.omit(unique(ea_list)), collapse = '_')
    ),
    by = .(shnro, episode)]
  
  e <- merge(e,
             e1[psy == T, .(dg_inpat_psy_outpat_psy = paste0(na.omit(unique(dg_outpat)), collapse = '_')), by = .(shnro, episode)],
             on = .(shnro, episode),
             all.x = TRUE)
  
  e <- merge(e,
             e1[psy == F, .(dg_outpat = paste0(na.omit(unique(dg_outpat)), collapse = '_')), by = .(shnro, episode)],
             on = .(shnro, episode),
             all.x = TRUE)
  
  e[,`:=`(inpatient_psy = TRUE, inpatient = TRUE, psy = TRUE, outpat_same_day = FALSE)]
  
  # inpatient care, but only outpatient visits in psychiatry. 
  f1 <- d3[,if(.N > 1 & sum(episode_inpatient) > 0 & sum(psy) > 0 & !any(episode_inpatient > 0 & psy == T)) .SD, by = .(shnro, episode)]
  f  <-
    f1[, .(
      tulopvm           = min(tulopvm),
      lahtopvm          = max(lahtopvm),
      n_rows_episode    = sum(n_rows_inpat, na.rm = T),
      tulopvm_psy_inpat = NA,
      lahtopvm_psy_inpat= NA,
      dg_inpat          = paste0(na.omit(unique(dg_inpat)), collapse = '_'),
      paltu             = paste0(na.omit(unique(paltu)), collapse = '_'),
      paltu_psy         = NA,
      ea_list           = paste0(na.omit(unique(ea_list)), collapse = '_')
    ),
    by = .(shnro, episode)]
  
  # psychiatric diagnoses:
  f <- merge(f, 
             f1[psy == T, .(dg_inpat_psy=paste0(na.omit(unique(dg_outpat)), collapse = ('_'))), by=.(shnro, episode)],  
             on = .(shnro, episode), 
             all.x = TRUE)
  # other outpatient diagnoses
  f <- merge(f, 
             f1[psy == F, .(dg_outpat=paste0(na.omit(unique(dg_outpat)), collapse = ('_'))), by=.(shnro, episode)],  
             on = .(shnro, episode), 
             all.x = TRUE)
  
  f[,`:=`(inpatient_psy = FALSE, inpatient = TRUE, psy = TRUE, dg_inpat_psy_outpat_psy = NA, outpat_same_day = FALSE)] # type: osasto muu; pkl psyk
  
  # combine everything
  dat <- rbindlist(list(a, b, c, cc, d, e, f), use.names = TRUE, fill = TRUE)
  
  # lahtopvm max value is max year + 1000, representing treatments that continued after the end of
  # the study period. Hence, real value is NA.
  dat[lahtopvm == max(lahtopvm), lahtopvm := NA]
  

  # harmonize names
  setnames(dat, old=c("paltu_psy"), 
           new = c("paltu_psy_inpat"))
  
  # psychiatric outpatient diagnoses are in dg_outpat, or in dg:inpat_psy if psychiatric outpatient visit during other inpatient episode
  
  
  # processed episodes data
  
  # keep episodes starting from the start_date
  dat <- dat[tulopvm >= outpatient_start_date]
  
  # Create variable year (again), based on lahtopvm ---------------------------------
  dat[, vuosi := as.integer(format(as.IDate(lahtopvm), '%Y'))]
  
  # if treatment continues at the end of the data, lahtopvm is NA, year is max year
  dat[is.na(lahtopvm), vuosi := as.integer(max_year)]
  
  
  # mark episodes that contain any inpatient period lasting over a night 
  dat[, overnight_all := FALSE]
  dat[inpatient == TRUE & lahtopvm > tulopvm, overnight_all := TRUE]
  
  # mark episodes that contain psychiatric inpatient period lasting over a night 
  dat[, overnight_psy := FALSE]
  dat[inpatient_psy == TRUE & lahtopvm_psy_inpat > tulopvm_psy_inpat, overnight_psy := TRUE]
  
  # dg: only relevant diagnoses included (not diagnoses within periods)
  dat[inpatient== TRUE & inpatient_psy == FALSE, dg := dg_inpat]
  dat[inpatient== TRUE & inpatient_psy == TRUE, dg := dg_inpat_psy]
  dat[inpatient== FALSE, dg := dg_outpat]
  
  
  list(episodes = dat, description = descr_episodes )

}

# function call ----------------------------------------------------------------


parts_out <- lapply(seq(1, n_parts), aggregate_specialized_care_episodes)


# all episodes
dat_episodes <- rbindlist(sapply(parts_out, '[', 'episodes'))

setorder(dat_episodes, 'shnro', 'tulopvm')

# descriptions
description_episodes <-  rbindlist(sapply(parts_out, '[','description'))[, .(n = sum(n)), by = .(episode, sample)]

rm(parts_out)
gc()


# save aggregated periods data ----------------------------------------------------

dir <- c(dir, create_dirs_in_and_outpatient()) # see 013_create_dirs.R


setwd(dir[['d3_episodes']])

write_fst(dat_episodes, paste0('data_episodes_', outpatient_start_year, '_', max_year, '.fst'), compress = compression)



# Descriptions of the preparation process ------------------------------------------

description_episodes[sample == 'all', percent := n / tail(n, 1) * 100]
description_episodes[sample == 'psy', percent := n / tail(n, 1) * 100]
description_episodes <- rbindlist(list(description_episodes, data.table(episode= paste0('add_days ', add_days), sample = NA, n=NA, percent = NA)))

# write descriptions
write_xlsx(
  list(description_episodes = description_episodes),
  path = paste0('description_episodes_', outpatient_start_year, '_', max_year, '.xlsx')
  )

setwd(here())

# // end