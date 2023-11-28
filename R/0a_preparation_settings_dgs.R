

# Here are some important settings and definitions. Go everything through carefully.

# settings  are collected into a list, see document outline.
settings <- list()

# General ----------------------------------------------------------------------
## Date formats 
settings$date_format <- "%d.%m.%Y"

# Compression on fst files
settings$compress <- 100

# Years of interest ------------------------------------------------------------

# End of the study
# (There might be entries that end after the expected end of the data)
settings$end_year           <-2020

# Start year for the current data type, 1996 at the earliest. Before that, data
# are in different format
settings$hilmo_start_year   <- 1996

# Start year for secondary outpatient data. 
## Collected since 1998,  comparability across time and service providers achieved from 2006.

settings$outpat_start_year  <- 1998

# Start year for primary care data. Collected since 2011
settings$avo_start_year     <- 2011

# Start years for the dataset before 1996 
settings$poisto_start_years <- c(1975, 1987, 1994)


# Data locations ---------------------------------------------------------------

# working directory
dirs0 <- list(main =  here())

# The directory where register data are located
# NOTE! Go through all read_-functions in the file 0a_preparation_funs.R and set 
# the correct sub-directory to each dataset. 

dirs0$hilmo_root <-file.path('location', 'of', 'the', 'register', 'root')


# Columns of interest to be red from raw data ----------------------------------

# here list the names of the columns you want to read from different data on different
# periods. Later, all column names will be set to lowercase.

# Years 2018 and later
settings$hilmo_iso_cols <- c('shnro', 'ISOID', 'VUOSI', 'ILAJI', 'TUPVA', 'LPVM', 'KOKU', 'PALTU', 'PALA', 'EA',  
                             'YHTEYSTAPA', 'KIIREELLISYYS')
# Years 1996-2017
settings$hilmo_iso_cols_1996_2017 <- c('shnro', 'isoid', 'vuosi', 'ILAJI', 'TUPVA', 'LPVM', 'KOKU', 'PALTU', 'PALA', 'EA', 
                                       'YHTEYSTAPA', 'KIIREELLISYYS')
# Primary care, 2018 an later
settings$hilmo_avo_cols_2018_2020 <-c("shnro", "TAPAHTUMA_TUNNUS", "KAYNTI_PALVELUMUOTO", "KAYNTI_YHTEYSTAPA", "KAYNTI_ALKOI", "KAYNTI_LOPPUI")

# Primary care, 2011-2017
settings$hilmo_avo_cols <-c("shnro", "tapahtuma_tunnus", "KAYNTI_PALVELUMUOTO", "KAYNTI_YHTEYSTAPA", "KAYNTI_ALKOI", "KAYNTI_LOPPUI")


# Contacts ---------------------------------------------------------------------
#                
#           Consult Hilmo register manuals for details
#           __________________________________________

## Medical specialty ----
# variable EA (specialty) is used for recognizing psychiatry

# For EA values, see Hilmo manuals 

settings$ea_psy <-c('70', '70F', '70X', '70Z', '74', '75', '75X')


## Type of contact ----

# 1996-2017:

# variable PALA ("service type"), classification:
# 1 sairaala (inpatient), 
# 2 paivakirurgia (day surgery), 
# 3 should not exist, 
# 5 substance use services, 
# 6 rehabilitaion institution, 
# 83 day hospital, 
# 91 outpatient emergence visit, 
# 92 outpatient first scheduled appointment, 
# 93 outpatient subsequent scheduled appointment,
# 94 outpatient consultation

# New classification 2018-, variable YHTEYSTAPA (type of contact)

# R10 asiakkaan käynti vastaanotolla (patient visits the service provider)
# R20 kotikäynti (home visit)
# R30 tyopaikkakaynti (work visit)
# R40 sairaalakaynti (vist at an institution)
# R41 käytni muualla, esim paivakodissa, palvelutalossa jne (visit somewhere else)
# R52 reaaliaikainen etaasiointi (remote contact, real-time)
# R56 etaasiointi ilman reaalikaista kontaktia (remote contact, not real-time)
# R60 ammattilaisten valinen konsultaatio (consultation between professional), 
# R71 neuvottelu (meeting), 
# R72 asioiden hoito (handling patient related matter)
# R80 vuodeosastohoito (inpatient care)
# R90 muu (other)

## Outpatient contacts, helper ----

# 1 Physical appointments somewhere (at clinic, at home, or elsewhere)
# 2 phone contact
# 3 online appointment, real-time
# 4 online appointment, not real-time 
# 5 the rest

settings$yhteystapa_1 <- c("R10", "R20", "R30", "R40", "R41")
settings$yhteystapa_2 <- c("R50")
settings$yhteystapa_3 <- c("R52")
settings$yhteystapa_4 <- c("R56")

# Which of the above mentioned helper-groups are considered as outpatient appointemts
# Here, physical contacts and real-time onlie are selected
settings$yhteystapa_outpat <- c(1, 3)
settings$yhteystapa_avo <- c(settings$yhteystapa_1, settings$yhteystapa_3)



# Diagnoses --------------------------------------------------------------------

## conversions -----------------------------------------------------------------
icd_conversions <- list()
###  ICPC-2 -> ICD-10 ----
icd_conversions$icpc2_icd10 <- list(
  f0 = c('P70|P71'),
  f1 = c('P16|P15|P19|P17'),
  f2 = c('P72'),
  f3 = c('P73'),
  f4 = c('P79|P74|P02|P82|P75|P78'),
  f5 = c('P86|P06|P07|P08'),
  f6 = c('P80|P09'),
  f7 = c('P85'),
  f8 = c('P24'),
  f9 = c('P81|P22|P23|P10|P12|P13|P11'),
  fx = c('P18|P98|P76|P99|P29'),
  x84 =c('P77')
)

### ICD-8 -> ICD-10, 1975-1986 ---- 
icd_conversions$icd8_icd10 <- list(
  f0 = '_290|_292|_293|_2941|_2942|_2943|_2948|_2949|_309",
  f1 = "_291|_2943|_303|_304",
  f2 = "_295|_297|_2981|_2982|_2983|_2989|_299",
  f4 = "_3000|_3001|_3002|_3003|_3005|_3006|_3007|_3008|_3009|_305|_3068|_307|_3084",
  f5 = "_3064|_3065",
  f6 = "_3010|_3012|_3013|_3014|_3015|_3016|_3017|_3018|_3019|_302",
  f3 = "_296|_2980|_3004|_3011",
  f7 = "_310|_311|_312|_313|_314|_315",
  f8 = "_3060|_3061|_3063|_3080",
  f9 = "_3062|_3066|_3067|_3069|_3081|_3082|_3083'
)

### ICD-9 -> ICD-19, 1987-1993 and 1994-1995  ----
icd_conversions$icd9_icd10 <- list(
  f0 = c('_290|_293|_294|_310'),
  f1 = c('_291|_292|_303|_304|_305'), 
  f2 = c('_295|_297|_298'),
  f3 = c('_296|_311'),
  f4 = c('_300|_3078|_309'), 
  f6 = c('_301|_302|_312'),
  f5 = c('_3027|_3071|_3074|_3075|_316'),
  f7 = c('_317|_318|_319'),
  f8 = c('_299|_315'), 
  f9 = c('_3080|_3072|_3073|_3076|_3077|_3079|_313|_314')#,
  #  f30_f31 = c('_2962|_2964|_2963E'),
  #  f20 = c('_2951|_2952|_2953|_2954A|_2956|_2959')
)

## ICD-10 subchapter categoreis ----

dg_maingroups <-  c('f0', 'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9') %>% as.list()
names(dg_maingroups) <- dg_maingroups
