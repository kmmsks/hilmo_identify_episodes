# hilmo_identify_episodes v 2.0.0-beta

An R script to identify hospital admissions, discharges, discharge diagnoses, and outpatient visits outside inpatient episodes from the Finnish [Care Register for Health Care](https://thl.fi/en/web/thlfi-en/statistics/information-on-statistics/register-descriptions/care-register-for-health-care) (the Hilmo register) between since 1975. 

This script identifies all inpatient episodes and episodes related to psychiatric care specifically. It can be generalized to other specializes as well. This script handles secondary care inpatient and outpatient data and primary care data.

This scripts supplement this paper: 

Suokas, Gutvilig, Pirkola, Lumme, Hakulinen. Enhancing the accuracy of register-based metrics: Comparing methods for handling overlapping psychiatric register entries in Finnish healthcare registries. Submittied.

2023-XX-YY 

[![DOI](https://zenodo.org/badge/299097747.svg)](https://zenodo.org/badge/latestdoi/299097747)


Author: Kimmo Suokas, kimmo.suokas@helsinki.fi

Aqknowledgements: 

## Background

In order to identify actual hospital admissions, discharges, and discharge diagnose from the Finnish Care register for health care, multiple register entries may need be combined, as one hospitalization may consist of multiple register entries. 

This is because during a single hospitalization, a new register entry must be supplied every time a hospital transfer, or transfer form one specialty to another within the hospital occurs. A register entry is also supplied from outpatient and emergency visit, which may take place at the beginning or during the hospitalization.

Based on the previous efforts to handle these issues, four models can be formulated in order to combine overlapping register entries:

Model   |  Description
:-------|:-------------
Model 1 | A new hospitalization may begin on the day following a previous one, with no specific minimum length required for a hospitalization. **This represents the most liberal approach.**
Model 2 | A new hospitalization may begin on the day following a previous one. Valid hospitalizations are those that extend over a minimum of two consecutive days, incorporating at least one overnight stay. If both admission and discharge take place on the same day, the visit is classified as an outpatient visit. **This model was used in the CEPHOS-LINK project.**
Model 3 | "A new hospitalization is allowed after a full day has been spent outside the hospital following the previous one. There is no specific minimum duration required for a hospitalization. **This model was used in the REDD project.**
Model 4 | A new hospitalization is allowed after a full day has been spent outside the hospital following the previous one. Valid hospitalizations are those that extend over a minimum of two consecutive days, incorporating at least one overnight stay. If both admission and discharge take place on the same day, the visit is classified as an outpatient visit. **This represents the most conservative model.**

Models 1 and 3 find admissions, as some of the admissions do not necessarily result in hospitalization. Models 2 and 4 differentiate overnight inpatient episodes from other visits to hospitals. If the focus is on inpatient discharge diagnoses, model 4 may result with little less preliminary diagnoses included, comparing to model 2.

### The Registers

The Disharge Reister was launched in 1969. The method presented here is suitable starting from the year 1975. Before that, recognizing psychiatric treatments is not univocal, and person identifications have more errors. For details, see the paper.

Years  |  Diagnoses | Description
:------|:-----------|:-----------
**1969&ndash;1974** | ICD-8 | Not covered in this method.
**1975&ndash;1995** |  | The older data is usually provided in three datasets, with slightly different formatting in each of them. Notice changes in variable coding within the datasets.
&nbsp;&nbsp;&nbsp;1975&ndash;1986 | ICD-8 | 
&nbsp;&nbsp;&nbsp;1987&ndash;1993 | ICD-9 |  
&nbsp;&nbsp;&nbsp;1994&ndash;1995 | ICD-9 | 
**1996&ndash;2018** | ICD-10 | Data is convergent enough to be processed together. Refer to Hilmo manuals concerning the minor changes in the data between years. 
&nbsp;&nbsp;&nbsp;1998 -> | ICD-10 | First data on secondary care outpatient visits are included.
&nbsp;&nbsp;&nbsp;2006 -> | ICD-10 | Secondary care outpatient data are considered consistently comparable across time and service providers achieved.
&nbsp;&nbsp;&nbsp;2011 -> | ICD-10 and ICPC-2 | Primary care outpatient appointments from public healthcare services are included in the registers.
**2019 ->** | ICD-10 and ICPC-2 | Major changes in the structure of the data.

### Purposes of this script: 
1. To propose a method for aggregating Hilmo entries in order to identify admissions, discharges, discharge diagnoses, and outpatient visits outside inpatient episodes from the partly overlapping register entries. This is necessary in order to find out: 
   + dates of admission and discharge, i.e. the period actually spent in the hospital,
   + dates of admission to and discharge from psychiatric care, if a single hospitalization included care in more than one specialty,
   + actual discharge diagnoses at the end of the hospitalization, or the last diagnosis from certain specialty, in this case psychiatry, and the service provider from where the discharge took place.
<br><br>
2. To tell apart register entries related to outpatient episodes that took place during an inpatient care. There may be a need to consider these outpatient entries as a part of the inpatient care, not as separate episodes.

3. To provide this script accessible for critical evaluation and further utilization (despite the actual register data is not openly available). The aim is to let future researchers to focus more on their actual research, and less on technicalities like this. 

## Step-by-stpe processing

### 0. Data preprocessing (real data):

Go through the scripts the **0_preparation.R**, **0a_preparation_settings_dgs.R**, and **0a_preparation_funs.R** in the folder **R**, and define necessary settings and paths to data.

These scripts harmonize variable names and types over time, recognize treatment types, medical specialties, and diagnoses over time time with varying classifications.

If working with the full registers, you may need to process the data in chunks, due to performance reasons.

The data are usually provided in so called annual format, meaning that data on each year are in separate files. In this step, the preprocessed data are saved longitudinally, meaning that each individual's all data is written into a single file, using `append = TRUE` in `fwrite` function from `library(data.table)`.

Here, the data are divided into chunks based on the first character of the ID variable (called `shnro`).


### Testing of the scripts with synthetic data

Synthetic data may be used for testing these scripts. Access to the actual registers is limited.

To create data, use function syntetize_data.

#### Usage

`syntetize_data(n_rows = 20000, n_individuals = 1000, 
                           start_year = 2015, end_year = 2020, seed = 1,
                           outpatient_proportion = .35, primary_care_proportion = .4, ilaji2_proportion = .05, 
                          save_data = TRUE, data_folder_name = 'data_main', longitudinal = TRUE)`

n_rows = 20000, 
n_individuals = 1000, 
start_year = 2015, 
end_year = 2020, 
seed = 1,
outpatient_proportion = .35, 
primary_care_proportion = .4, 
ilaji2_proportion = .05, 
save_data = TRUE, 
data_folder_name = 'data_main', 
longitudinal = TRUE




## Prepare Data for the Aggregation:

Preparations are defined in **0_data_raw_to_parts.R**

If your data is in form THL calls the database form, you need to combine the data from different files based on entry id. Variables are in different files and each year is in its own file. Use library(bit64) for long id numbers. Example not currently shown.

What needs to be achieved, is to have minimum of the following columns in your data:

Variable | Data type | Description
:--------|:----------|:------------
```shnro``` | character | person id. The name shnro is used by the Statistic Finland, it refers to a person, not ID number, which may change (in relative rare occasions).
```vuosi``` | integer   | Year of the entry
```ILAJI``` | integer   | See Hilmo manuals for details. Note possible changes in the classifications between years. 
```PALTU``` | integer   | (as above)
```PALA```  | integer   | (as above)
```EA```    | character | (as above)
```TUPVA``` | integer   | set: ```as.integer(as.IDate(TUPVA, format = [FORMAT]))```
```LPVM```  | integer   | set: ```as.integer(as.IDate(LPVM, format = [FORMAT]))```
```dg```    | character | Diagnoses, see below

Diagnoses: 
- One register entry may contain multiple diagnoses, each in its own column. 
- Names of the diagnosis variables may vary between years and datasets.
- Create variable ```dg```, where all diagnoses related to one register entry are pasted. 
   + f. ex. ```dat[, dg := paste(DG1, DG2, sep = '_')]```
   
Make all neacassery preparations in 0_data_raw_to_parts.R.

### Data Size, Operating with Chunks

If your data is in one file of reasonable size, all you need to do is to check you have above mentioned columns in the data and name the data as '1.csv'

If you have full register, you may need to process the data in smaller parts (chunks), due to performance reasons. In this case, this method requires all entries of a single person to locate in one part (instead of having the data split in parts by year and variables).

To determine the optimal part size is out of scope here. See what is small enough on your machine. In this example, five parts are used to demonstrate the action.

Set the desired number of parts, ```n_parts```, in  0_data_raw_to_parts.R.


## 3. Processing of the Prepared Data

### Years 1996&ndash;2018 Inpatient and Outpatient Data
Control the following setting for desired episode identification rules in **1_main_aggregation.R**:

Variable | Description
:--------|:----------
```dir```                  | Input file locations for preprocessed data. In this example, data_temp/parts_raw.
```max_year```             | Time range of the data, the latest year included. **Currently, maximum is 2018**. 
```min_year```             | The first year included. **Earliest 1996**. Previous years have their own methods.
```outpatient_start_year```| According to THL, outpatient data is **relevant starting from 2006**.
```add_days```             |Days between periods, see below
```PALA_inpatient``` <br> ```PALA_outpatient``` | Register entry types defining treatments types of interest, see Hilmo manuals for details
```specialties_of_interest``` |  Define speciality (psychiatry in this case). See erikoisala (EA) in Hilmo manuals. Notice changes in the coding between years.

#### Days between periods ```add_days``` 

Minimum of full calender days required between two hospital treatment periods:

- 0 : a new period may start the next day after the previous one (models 1 and 2).
- 1 : there must be one full calender day between two treatment periods (models 3 and 4). Otherwise, Hilmo entries are combined into a single episode.

### Years 1975&ndash;1995, Inpatient Data Only

The method presented here is suitable starting from the year 1975. Years 1969&ndash;1986, 1987&ndash;1993 and 1994&ndash;1995 are processed separately first, and after that, all inpatient data is combined.

Conversions of diagnoses with mental disorders to ICD-10 inclued. ICD-8 was used until 1986, ICD-9 1987&ndash;1995 and ICD-10 thereafter. Conversions are preliminary, please check before use.

Set column names, date formats, define desired diagnostic cathegories for conversion to ICD-10, and define specialities of interest in 120_run_inpatient_old_registers.R before sourcing.

**Notice, no example datasets currently provided for years 1975&ndash;1995.**

### Source Desired Scripts

- ```source(here('R', '110_run_inpatient_1996_2018.R'))```, this script identifies inpatient episodes 1996&ndash;2018.
- ```source(here('R', '130_run_in_and_outpatient.R'))```, this script identifies inpatient and outpatient episodes 2006&ndash;2018.
- ```source(here('R', '120_run_inpatient_old_registers.R'))```, this identifies inpatient episodes 1975&ndash;1995. Go through settings in this file in detail.
- ```source(here('R', '122_combine_inpatient_all_years.R'))```, combine all inpatient episodes into one file. Identifys episodes that continue between datasets.

## Data Output

Processed data is saved in the following subfolders of the folder **data_processed**:

First level subfolders       | Second level           | Data sets
:----------------------------|:-----------------------|:--------
1_inpatient_episodes         |add_days_[add_days]     | Inpatient datasets, datsets before the year 1996 in separate files.
1_inpatient_episodes         |preparation_description | Descriptions of incorrect entries, PALA distribution, etc.
1b_inpatient_all_combined    | -                      | All inaptient episodes combined into one file.
2_in_and_outpatient_episodes |add_days_[add_days]     | Data set with inpatient and outpatient episodes combined. Description of included episodes.


This test script creates the following data.tables:

- **dat_episodes**: processed data with inpatient and outpatient episodes included
- **dat_inpatient**: inaptient episodes only
- **dat_all_inpatient**, all inpatient episodes combined. Practically identical with dat_inpatient with the example fake data.
- **description_episode** and **description_inpatient**: see read_me sheets in xlsx files in folder data_processe.


## New Variables in the Processed Data

### Inpatient Data:

Variable | Data type | Description
:--------|:----------|:--------
```episode_inpatient```| integer | Running number of person's inpatient periods.
```tulopvm```          | date    | Date of admission to inpatient care or date of outpatient visit.
```lahtopvm```         | date    | Date of discharge from inpatient care, if multiple transfers between specialties, the last one. For outpatient visits, the date of the visit (same as tulopvm).
```vuosi```             | integer| Year if discharge, ie. year of ```lahtopvm``` (note, year of lahtopvm_psy may be different than ```vuosi```)
```psy```               | logical| Episode contains psychiatric care.
```n_rows_inpat```    | integer  | Number of rows aggregated to this inpatient episode.
```ea_list```         | character| List of specialties included in the episode.
```tulopvm_psy_inpat```    | date| Date of admission to psychiatric inpatient care.
```lahtopvm_psy_inpat```   | date| Date of discharge from psychiatric inpatient care, if multiple transfers between specialties, the last one.
```dg_inpat```        | character| All diagnoses registered on the date of discharge.
```dg_inpat_psy```    | character| All diagnoses registered from psychiatry on the date of discharge from psychiatric unit.
```paltu```           | character| The service provider's code of the unit of the discharge.
```paltu_psy```       | character| The service provider's code of the psychiatric unit of the last psychiatric discharge.
```overnight_all```     | logical| Episode starts and ends on different calender days.
```overnight_psy```     | logical| Episode's psychiatric treatment starts and ends on different calender days.
```episode_continues``` | logical| True if inpatient episodes continues after the last day coverd in the data. In this case, ```lahtopvm``` is ```NA```.

### All Episodes, in Addition:

Variable | Data type | Description
:--------|:----------|:--------
```episode```           |Integer | Running number of person's episodes.
```inpatient```         |Logical| Any inpatient care included in the episode.
```inpatient_psy```     |Logical| Psychiatric inpatient care included in the episode.
```n_rows_episode```    |Integer | Number of rows aggregated to this inpatient episode.
```outpat_same_day```   | Logical| More than one outpatient visit on the same day.           
```dg_outpat```         | charcteg| Outpatient diagnoses during the episode. See below.
```dg_inpat_psy_outpat_psy```| character| Psychiatric outpatient diagnoses during psychiatric inpatient episode. Usually to be excluded.
```dg_inpat_psy```      | character| If inpatient period contains psychiatric outpatient care, but not psychiatric inpatient care, psychiatric outpatient diagnoses are here. Contains also psychiatric discharge diagnoses from psychiatric inpatien episodes.
```dg```                | character| Combination of relevant discharge diagnoses, and diagnoses of outpatient appointments. Discharge diagnoses from psychiatric inpatient care preferred. See below.

#### Outpatient Diagnoses

Outpatient diagnoses situate in different variables in different cases:

- Diagnoses of all sinlge outpatient appointments are in ```dg_outpat```.

- Diagnoses of all non psychiatric outpatient appointments during non-psychiatric inpatient episodes are in ```dg_outpat```.

- If non psychiatric inpatient episode contains psychiatric outpatient appointments, the non psychiatric outpatient diagnoses are in ```dg_outpat```. The psychiatric outpaitent diagnoses are in ```dg_inpat_psy```.

- If psychiatric inpatient episode contains non psychiatric outpatient appointments, they are in ```dg_outpat```. Psychiatric outpatient diagnoses during psychiatric inpatient episodes are in ```dg_inpat_psy_outpat_psy``` (usually excluded). 

The ```dg_outpat``` variable should be excluded when only discharge diagnoses are collected.

<br>

#### Variable ```dg```

Combination of discharge diagnoses from inpatient episodes only, and outpatient diagoses . From psychiatric inpatient episodes, only diagnoses from psychiatry:

- when ```inpatient== TRUE & inpatient_psy == FALSE```, variable ```dg_inpat``` included,
- when ```inpatient== TRUE & inpatient_psy == TRUE```, variable ```dg_inpat_psy``` included,
- when ```inpatient== FALSE```, variable ```dg_outpat``` included.


## 4. Subset Processed Data & Count Episodes and Time Spent in Hospital by Person

### Only Overnight Episodes Considers as Inpatient episodes

If only overnight episodes are considered as inpatient treatments (models 2 and 4):

- Psychiatric inpatient episodes, all years included:
   + ```dat_all_inpatient[overnight_psy == TRUE]```
- Psychiatric outpatient episodes, starting from the year ```outpatient_start_year```
   + including psychiatric outpatient visits during non psychiatric inpatient care ```dat_episodes[psy == TRUE & overnight_psy == FALSE]```
   + only psychiatric outpatient visits when not in inpatient care ```dat_episodes[psy == TRUE & overnight_all == FALSE]```

### All Episodes with Inpatient Entries

If all episodes with any register entry from inpatient care (ie. also shorter than overnight episodes) are considered inpatient care (models 1 and 3):

- Psychiatric inpatient episodes, all years included:
   + ```dat_all_inpatient[psy == TRUE]```
- Psychiatric outpatient episodes, starting from the year ```outpatient_start_year```
   + including psychiatric outpatient visits during non psychiatric inpatient care ```dat_episodes[psy == TRUE & inpatient_psy == FALSE]```
   + only psychiatric outpatient visits when not in inpatient care ```dat_episodes[psy == TRUE & inpatient == FALSE]```
   
### Number of Episodes by Person

To get number of episodes by person, get .N by shnro, f. ex. number of psychiatric inpatient episodes by person (models 2 and 4), all years included:
   + ```dat_all_inpatient[overnight_psy == TRUE, .N, by = shnro]```

### Number of Days Hospitalized by Person

To get the total number of days hospitalized:

- ```dat_all_inpatient[, .(days_hospitalized = lahtopvm - tulopvm), by = shnro][, .(days_hospitalized = sum(days_hospitalized)), by = shnro][]```

### Number of Days wiht Hospitalized in Psychiatric Care by Person

To get the number of days hospitalized in psychiatric care:

- ```dat_all_inpatient[psy == TRUE, .(days_hospitalized_psy = lahtopvm_psy_inpat - tulopvm_psy_inpat), by = shnro][, .(days_hospitalized = sum(days_hospitalized_psy)), by = shnro][]```
   + Note: if patient is transferred from psychiatric inpatient care to other speciality and then back, also the days spent in other speciality are covered. Days spent in other specialties after the last discharge (or before the first admission to psychiatry) are not coverd. 

## 5. Primary Care

The primary care register (called Avohilmo) has more complex structure than the secondary care registers. Four follow-up points are included in Avohilmo: the first contact, assessment of treatment needs, scheduling of the appointment, the appointment.

To recognize actual appointments that took place  in person, subset the following values in variable ```yhteystapa```:
- ``` yhteystapa %in% c('R10', 'R20', 'R30', 'R41')```

If you want to subset doctors appointments only, subset the following values in variable ```ammattioikeus```:
-```ammattioikeus %in% c("001", "002", "031", "032", "034", "701", "702", "717", "718", "720", "722", "723", "724", "810", "811", "900", "901")```

There are few entries, which do not start and end on the same day. Here, only the start date is considered.

After desired subsettings, bind the Avohilmo data with inpatient data, f.ex. ```dat_all_inpatient[overnight_psy == TRUE]``` and exclude primary care appointments during psychiatric inpatient episodes. This needs to be done to recognize discharge diagnoses only.

Example will be provided in the future.

### ICPC-2 conversion

Some primary care providers use ICPC-2 diagnostic classification.

The following schema was used.

```
# NOT RUN

icpc2_icd10 <-  list(
  f0= 'P70|P71',
  f1= 'P15|P16|P17|P19',
  f2= 'P72',
  f3= 'P73',
  f4= 'P79|P74|P02|P82|P75|P78',
  f5= 'P86|P06|P07|P08',
  f6= 'P09|P80',
  f7= 'P85',
  f8= 'P24',
  f9= 'P81|P22,P23|P10|P11|P12|P13',
  fx= 'P18|P98|P76|P99|P29'
)
  
dat[, dg_converted := '']

for( i in c(0:9, 'x')){
  dat[grepl(icpc2_icd10[paste0('f', i)], dg_icpc2, ignore.case = TRUE),
      dg_converted := paste(na.omit(dg_converted), paste0('F', i), sep = '_')]
}
```

Conversion codes:
- Kvist M, Savolainen T, Suomen Kuntaliitto. [ICPC-2 : perusterveydenhuollon kansainvälinen luokitus](https://www.kuntaliitto.fi/julkaisut/2010/1344-icpc-2-perusterveydenhuollon-kansainvalinen-luokitus). Suomen kuntaliitto, Helsinki 2010. p. 194, Finnish version.
- [ICPC-2-R: International Classification of Primary Care](https://www.who.int/standards/classifications/other-classifications/international-classification-of-primary-care) see p. 147

## Version History

- 2.0.0 (DATE): New functional format for the scripts. The coding of treatment types in the Hilmo rgisters changed in 2019 and is now included in this method. The current behaviour is tested with data up to the year 2020. ICD-8 and ICD-9 conversion are included in more datail. This version supplements this paper: LINK TO PAPER.

- 1.1.1 (2022-05-23): Info on primary care included.Typos.

- 1.1.0-beta (2021-05-18): Inference of discharge dates fixed. Fake data supplemented. Typos.

- 1.0.2 (2020-11-29): First stable version. In this version, however, discharge date (```lahtopvm```) was the date of discharge in the register entry with the latest admission. This may give too early discharge dates. Admission dates and numbers of episodes were correctly calculated. 

## Citation

Suokas, K (2021). hilmo_identify_episodes (v1.1.0) [Source code]. https://github.com/kmmsks/hilmo_identify_episodes. doi: 10.5281/zenodo.5381082.

<br><br><br>
