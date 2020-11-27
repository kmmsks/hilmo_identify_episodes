# hilmo_identify_episodes

R script to identify hospital admissions, discharges and discharge diagnoses from the Finnish [Care Register for Health Care](https://thl.fi/en/web/thlfi-en/statistics/information-on-statistics/register-descriptions/care-register-for-health-care) ("Hilmo" register) between years 1975&ndash;2018. 

This script identifies all episodes and episodes related to psychiatric care. It can be generalized to other specializes as well.

2020-11-27

Author: Kimmo Suokas, kimmo.suokas@tuni.fi

[![DOI](https://zenodo.org/badge/299097747.svg)](https://zenodo.org/badge/latestdoi/299097747)

## Background

In order to identify actual hospital admissions, discharges, and discharge diagnose from the Finnish Care register for health care, multiple register entries may need be combined, as one hospitalization may consist of multiple register entries. 

This is because during a single hospitalization, a new register entry must be supplied every time a hospital transfer, or trasfer form one specialty to another within the hospital occurs. A register entry is also supplied from outpatient and emergency visit, which may take place at the beginning or during the hospitalization.

In previous research, up to 25 % of the psychiatric inpatient care related register entries have been related to transfers during an actual hospitalizations ([CEPHOS-LINK)](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186) project, p. 35).

Hospitalization may start from emergency clinic, and during inpatient care, transfers within and between hospitals and outpatient appointments may take place. Hilmo entries of these appointments are registered with possibly preliminary dignoses of that time. Hence, it is important to first identify the actual discharges and then identify the discharge diagnoses.

In previous papers using the Hilmo register, it is usually mentioned that hospital periods, admissions or discharges were identified. However, usually no criteria, let alone scripts, for this procedure is provided. 

As far as I know, there is no generally know or accepted methods available for identifying treatment episodes. Different sets of criteria have previusly been used:

- Others require a hospital treatment to start and end on different calender days (f. ex. the [CEPHOS-LINK](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186) project), ie. an inpatient episode should last overnight. Others do not require tihis.
- Others state that a new treatment period may start the next day after previous one (f. ex. [CEPHOS-LINK](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186)), others require a full calender day outside of hospital in between two treatment periods (f. ex. [REDD project](http://urn.fi/URN:NBN:fi-fe201204193720)). This criteria is used to stronger differentiate transfers between hospitals and 'real' rehospitalizations.


Combinations of these criteria form four models:

Model   |  Description
:-------|:-------------
Model 1 | A new hospitalization may start the next day after a previous one. No minimun length for a hospitalization. **The most liberal model.**
Model 2 | A new hospitalization may start the next day after a previous one. Minimun length for a hospitalization is overnight, ie. admission and discharge must take place on different days, otherwise the visit is considered as an outpatient visit. **F. ex. CEPHOS-LINK**
Model 3 | A new hospitalization may start after one whole day outside the hopsital after the previus one. No minimun length for a hospitalization. **F. ex REDD.**
Model 4 | A new hospitalization may start after one whole day outside the hopsital after the previus one.  Minimun length for a hospitalization is overnight, ie. admission and discharge must take place on different days, otherwise the visit is considered as an outpatient visit. **The most coservative model.**

Models 1 and 3 find admissions, as some of the admissions do not necasserily result in hospitalization. Models 2 and 4 differentiate actual overnight inpatient episodes from other visits. If the focus is on inpatient diachagre diagnoses, model 4 may result with little less preliminary diagnoses included, comparing to model 2.

### Exampeles of Papers Adopting These Principles

Model 3 was used in:

- Suokas K, Koivisto A, Hakulinen C, et al. Association of Income With the Incidence Rates of First Psychiatric Hospital Admissions in Finland, 1996-2014. JAMA Psychiatry. 2020;77(3):274–284. doi:10.1001/jamapsychiatry.2019.3647

## Script Purpose: 
1. To propose a method for aggregating Hilmo entries in order to identify actual admissions, discharges, and discharge diagnoses from the register with different criteria. This is necessary in order to find out: 
   + dates of admission and discharge, i.e. the period actually spent in the hospital,
   + dates of admission to and discharge from psychiatric care, if a single hospitalization included care in more than one specialty,
   + actual discharge diagnoses at the end of the hospitalization, or the last diagnosis from certain specialty, in this case psychiatry, and the service provider from where the discharge took place.
<br><br>
2. To tell apart register entries related to outpatient episodes that took place during an inpatient care. There may be a need to consider these outpatient entries as a part of the inpatient care, not as separate episodes.

3. To provide this script accessible for critical evaluation and further utilization (despite the actual register data is not openly available). The aim is to let future (at least clinically oriented) researchers to focus more on their actual research, and less on technicalities like this. 

## Covered Register Years

The Disharge Reister was launched in 1969. The method presented here is suitable starting from the year 1975. Before that, recognizing psychiatric treatments is not univocal, and person identifications have more mistakes.

Years  |  Diagnoses | Description
:------|:-----------|:-----------
**1969&ndash;1974** | ICD-8 | Not covered in this method. 
**1975&ndash;1995** | ICD-8 1975&ndash;1986, <br> ICD-9 1987&ndash;1995 | The older data is usually provided in three datasetes, years 1969&ndash;1986, 1987&ndash;1993 and 1994-1995, with slightly different formating in each of them. Notice changes in variable codings within datasets.
**1996&ndash;2018** | ICD-10 | Data is convergent enough to be processed together. Refer to Hilmo manuals concerning the minor changes in the data between years. 
**2019 ->** | ICD-10 | some major changes in the variables. Not covered in this script yet.




## 1. Data Input:

Set the location of the raw data in **0_data_raw_to_parts.R**. The data folder must contain only the data files.

csv format for data input is used.

This project comes with fake data to see the scripts in action, starting 1996. Notice, the fake data does not necessary follow the real world patterns in any way!

- First, run **create_fake_data.R** to generate data for testing these scripts.

No example datasets currently provided for the years 1975&ndash;1995.

## 2. Prepare Data for the Aggregation:

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
```TUPVA``` | integer   | set: as.integer(as.IDate(TUPVA, format = [FORMAT]))
```LPVM```  | integer   | set: as.integer(as.IDate(LPVM, format = [FORMAT]))
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

Set ```n_parts``` in  0_data_raw_to_parts.R.


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
- ```source(here('R', '120_run_inpatient_aggregation_old_registers.R'))```, this identifies inpatient episodes 1975&ndash;1995. Go through settings in this file in detail.
- ```source(here('R', '122_combine_inpatient_all_years.R'))```, combine all inpatient episodes into one file. Identifys episodes that continue between datasets.

## 4. Data Output

Full aggregated data is saved in the folders:

- data_processed -> 1_inpatient_episodes 
   + -> add_days_[add_days]: inpatient data in with datasets covering years before 1996 in separate files.
   + -> preparation_description: description of incorrect entries, PALA distribution, etc.
- data_processed -> 1b_inpatient_all_combined: all inaptient episodes combined into one file.
- data_processed -> 2_in_and_outpatient_episodes -> add_days_[add_days]: the data and description of included episodes

This test script creates the following data object:

- **dat_episodes**: processed data with inpatient and outpatient episodes included
- **dat_inpatient**: inaptient episodes only
- **dat_all_inpatient**, all inpatient episodes combined. Practically identical with dat_inpatient with the example fake data.
- **description_episode** and **description_inpatient**: see read_me sheets in xlsx files in folder data_processe.


## New Variables in the Processed Data

### Inpatient Data:

Variable | Data type | Description
:--------|:----------|:--------
```episode_inpatient```| integer | Running number of person's inpatient periods.
```psy```               | logical| Episode contains psychiatric care.
```overnight_all```     | logical| Episode starts and ends on different calender days.
```overnight_psy```     | logical| Episode's psychiatric treatment starts and ends on different calender days.
```episode_continues``` | logical| True if inpatient episodes continues after the last day coverd in the data. In this case, ```lahtopvm``` is ```NA```.
```vuosi```             | integer| Year if discharge, ie. year of ```lahtopvm``` (note, year of lahtopvm_psy may be different than ```vuosi```)
```tulopvm_psy_inpat```    | date| Date of admission to psychiatric inpatient care.
```lahtopvm_psy_inpat```   | date| Date of discharge from psychiatric inpatient care, if multiple transfers between specialties, the last one.
```ea_list```         | character| List of specialties included in the episode.
```paltu```           | character| The service provider's code of the unit of the discharge.
```paltu_psy```       | character| The service provider's code of the psychiatric unit of the last psychiatric discharge.
```n_rows_inpat```    | integer  | Number of rows aggregated to this inpatient episode.
```dg_inpat```        | character| All diagnoses registered on the date of discharge.
```dg_inpat_psy```    | character| All diagnoses registered from psychiatry on the date of discharge from psychiatric unit.

### All Episodes, in Addition:

Variable | Data type | Description
:--------|:----------|:--------
```episode```           |Integer | Running number of person's episodes.
```inpatient```         |Logical| Any inpatient care included in the episode.
```inpatient_psy```     |Logical| Psychiatric inpatient care included in the episode.
```n_rows_episode```    |Integer | Number of rows aggregated to this inpatient episode.
```outpat_same_day```   | Logical| More than one outpatient visit on the same day.           
```dg_outpat```         | charcteg| Outpatient diagnoses during the episode.See below
```dg_inpat_psy_outpat_psy```| character| Psychiatric outpatient diagnoses during psychiatric inpatient episode. Usually to be excluded.
```dg_psy_inpat```      | character| If inpatient period contains psychiatric outpatient care, but not inpatient care, psychiatric outpatient diagnoses are here
```dg```                | character| Combination of relevant discharge diagnoses, discharge diagnoses from psychiatric inpatient care preferred. See below.

#### Variable ```dg_outpat```

Includes psychiatric outpatient diagnoses, if the episode contains no psychiatric inpatient care.

Otherwise, outpatient diagnoses during inpatient care are included. 

This diagnoses should be excluded when only discharge diagnoses are collected.

<br>

#### Variable ```dg```

Combination of relevant discharge diagnoses, discharge diagnoses from psychiatric inpatient care preferred:

- when ```inpatient== TRUE & inpatient_psy == FALSE```, variable ```dg_inpat``` included,
- when ```inpatient== TRUE & inpatient_psy == TRUE```, variable ```dg_inpat_psy``` included,
- when ```inpatient== FALSE```, variable ```dg_outpat``` included.

## Subset Processed Data & Count Episodes and Time Spent in Hospital by Person

If only overnight episodes are considered as inpatient treatments (models 2 and 4):

- Psychiatric inpatient episodes, all years included:
   + ```dat_all_inpatient[overnight_psy == TRUE]```
- Psychiatric outpatient episodes, starting from the year ```outpatient_start_year```
   + including psychiatric outpatient visits during non psychiatric inpatient care ```dat_episodes[psy == TRUE & overnight_psy == FALSE]```
   + only psychiatric outpatient visits when not in inpatient care ```dat_episodes[psy == TRUE & overnight_all == FALSE]```
<br><br>

If all episodes with any register entry from inpatient care (ie. also shorter than overnight episodes) are considered inpatient care (models 1 and 3):

- Psychiatric inpatient episodes, all years included:
   + ```dat_all_inpatient[psy == TRUE]```
- Psychiatric outpatient episodes, starting from the year ```outpatient_start_year```
   + including psychiatric outpatient visits during non psychiatric inpatient care ```dat_episodes[psy == TRUE & inpatient_psy == FALSE]```
   + only psychiatric outpatient visits when not in inpatient care ```dat_episodes[psy == TRUE & inpatient == FALSE]```
   
<br><br>
To get number of episodes by person, get .N by shnro, f. ex. number of psychiatric inpatient episodes by person (models 2 and 4), all years included:
   + ```dat_all_inpatient[overnight_psy == TRUE, .N, by = shnro]```

<br><br>
To get the total number of days hospitalized:

- ```dat_all_inpatient[, .(days_hospitalized = lahtopvm - tulopvm), by = shnro][, .(days_hospitalized = sum(days_hospitalized)), by = shnro][]```

To get the number of days hospitalized in psychiatric care:

- ```dat_all_inpatient[psy == TRUE, .(days_hospitalized_psy = lahtopvm_psy_inpat - tulopvm_psy_inpat), by = shnro][, .(days_hospitalized = sum(days_hospitalized_psy)), by = shnro][]```
   + Note: if patient is transferred from psychiatric inpatient care to other speciality and then back, also the days spent in other speciality are covered. Days spent in other specialties after the last discharge (or before the first admission to psychiatry) are not coverd. 
   

## Citation

Suokas, K (2020). hilmo_identify_episodes (v1.0.0) [Source code]. https://github.com/kmmsks/hilmo_identify_episodes. doi: 10.5281/zenodo.4095154.

<br><br><br>
