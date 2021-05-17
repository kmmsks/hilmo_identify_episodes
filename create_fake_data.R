#
#
# Identify Hilmo episodes // Hilmojen ketjutus
#
# Author: Kimmo Suokas
#
#
# Create fake data to see the scripts in action.
#The fake data does not necassarey follow the real world patterns in any way!

library(data.table)
library(here)

fake_size = 20000
fake_start <- '2010-01-01'
fake_end <- '2014-12-31'

set.seed(2)
d <- data.table(shnro = paste0(sample(1000:(1000 + fake_size / 5), fake_size, replace = TRUE), LETTERS[5]) ,
         vuosi =  NA,
         ILAJI = 1,
         PALTU = sample(5000:5100, size = fake_size, replace = TRUE),
         PALA  = c(sample(c(1,91, 92, 93, 94), size = fake_size * 4 / 5, replace = TRUE), 
                   sample(c(1, 2, 5, 6, 83), size = fake_size / 5, replace = TRUE)),
         EA    = sample(c(10:25, 60:80), size = fake_size, replace = TRUE),
         TUPVA = NA,
         LPVM  = sample(as.integer(as.IDate(fake_start)):as.integer(as.IDate(fake_end)), size = fake_size, replace = TRUE),
         DG1   = paste0(sample(LETTERS[1:6], size = fake_size, replace = TRUE), sample(10:999, size = fake_size, replace = TRUE)),
         DG2   = paste0(sample(LETTERS[1:6], size = fake_size, replace = TRUE), sample(10:999, size = fake_size, replace = TRUE))
         )[, `:=`(vuosi = format(as.IDate(LPVM), '%Y'),
                  TUPVA = LPVM - sample(1:1000, size = fake_size, replace = TRUE)
                  )]

# create overnight_psy == FALSE -case
d[shnro == '1023E' & EA == 74, LPVM := TUPVA]
d[shnro == '1023E' & EA == 23, LPVM := TUPVA]

d[PALA > 90, TUPVA := LPVM] # outpatient events have the same start_date and end_date

# create case with two outpaitent appointments on the same day
d <- rbindlist(list(d, d[shnro == '4924E' & PALA>90][, EA := 11]))


d[EA %in% c(70, 74,75), DG1 := paste0('F', sample(10:999, size = .N, replace = TRUE))]

ilaji2 <- tail(d[PALTU == 1], fake_size / 10)[, `:=`(ILAJI = 2,
                                                     LPVM  = NA)]

fake_data <- rbindlist(list(d, ilaji2))

setorder(fake_data, shnro, LPVM)

fake_data[, `:=`(TUPVA = format(as.IDate(TUPVA), '%d-%b-%y'),
                 LPVM  = format(as.IDate(LPVM) , '%d-%b-%y')
)]

#
add_person <- data.table(
  shnro = rep('1000A', 8),
  vuosi = c(rep(2014,8)),
  ILAJI = c(rep(1,7), 2),
  PALTU = sample(5000:5100, size = 8, replace = TRUE),
  PALA  = c(91, 94, 1, 92, 1, 1, 92, 1 ),
  EA    = c(70, 98, 70, 70, 77, 70, 70, 10),
  TUPVA = c('01-helmi-14', '02-helmi-14', '02-helmi-14', '10-helmi-14', '17-helmi-14', '21-helmi-14', '05-huhti-14', format(as.IDate(fake_end)-2, '%d-%b-%y')),
  LPVM  = c('01-helmi-14', '02-helmi-14', '16-helmi-14', '10-helmi-14', '20-helmi-14', '21-helmi-14', '05-huhti-14', format(as.IDate(fake_end)+9, '%d-%b-%y')),
  DG1   = c('F29', 'F20', 'F31', 'F25', 'G45', 'F31', 'F43', 'F00'),
  DG2   = c('F41', '', 'F40', 'F25', 'F20', 'G45', 'F41', 'I10' )
)

fake_data <- rbindlist(list(
  fake_data, add_person
))
setorder(fake_data, shnro)

# save 
dir.create(here('data_raw_fake'))

fake_data[, vuosi2 := vuosi] # just to include vuosi in the .SD

fake_data[, fwrite(.SD, here('data_raw_fake', paste0(vuosi2, '.csv'))), by = vuosi2]

rm(d, fake_end, fake_size, fake_start, ilaji2)
gc()
