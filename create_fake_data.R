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
#         SUKUP = NA,
         ILAJI = 1,
         PALTU = sample(5000:5100, size = fake_size, replace = T),
         PALA  = c(sample(c(1,91, 92, 93, 94), size = fake_size * 4 / 5, replace = T), 
                   sample(c(1, 2, 5, 6, 83), size = fake_size / 5, replace = T)),
         EA    = sample(c(10:25, 60:80), size = fake_size, replace = T),
         TUPVA = NA,
         LPVM  = sample(as.integer(as.IDate(fake_start)):as.integer(as.IDate(fake_end)), size = fake_size, replace = TRUE),
         DG1   = paste0(sample(LETTERS[1:6], size = fake_size, replace = TRUE), sample(10:999, size = fake_size, replace = T)),
         DG2   = paste0(sample(LETTERS[1:6], size = fake_size, replace = TRUE), sample(10:999, size = fake_size, replace = T))
         )[, `:=`(vuosi = format(as.IDate(LPVM), '%Y'),
                  TUPVA = LPVM - sample(1:1000, 1)
                  )]

d[PALA > 90, TUPVA := LPVM] # outpatient events have the same start_date and end_date

d[EA %like% c(70, 74,75), DG1 := paste0('F', sample(10:999, size = .N, replace = T))]

ilaji2 <- tail(d[PALTU == 1], fake_size / 10)[, `:=`(ILAJI = 2,
                                                     LPVM  = NA)]

fake_data <- rbindlist(list(d, ilaji2))


fake_data[, `:=`(TUPVA = format(as.IDate(TUPVA), '%d-%b-%y'),
                 LPVM  = format(as.IDate(LPVM) , '%d-%b-%y')
)]


setorder(fake_data, shnro, LPVM)



# save 
dir.create(here('data_raw_fake'))

fake_data[, vuosi2 := vuosi] # just to include vuosi in the .SD

fake_data[, fwrite(.SD, here('data_raw_fake', paste0(vuosi2, '.csv'))), by = vuosi2]

rm(d, fake_end, fake_size, fake_start, ilaji2)
gc()
