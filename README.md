# hilmo_identify_episodes

R script to identify hospital admissions, discharges and discharge diagnoses from the Finnish [Care Register for Health Care](https://thl.fi/en/web/thlfi-en/statistics/information-on-statistics/register-descriptions/care-register-for-health-care) ("Hilmo" register). 
This script identifies all episodes, and episodes related to psychiatric care. IT can be generalized to other specializes as well.

Version 0.1 (2020-09-28)

Author: Kimmo Suokas, kimmo.suokas@tuni.fi

## Background
In order to identify actual hospital admissions, discharges, and discharge diagnose from the Finnish Care register for health care, multiple register entries may need be combined, as one hospitalization may consist of multiple register entries. 

This is because during a single hospitalization, a new register entry must be supplied every time a hospital transfer, or trasfer form one specialty to another within the hospital occurs. A register entry is also supplied from outpatient and emergency visit, which may take place at the beginning or during the hospitalization.

In previous research, up to 25 % of the psychiatric inpatient care related register entries have been related to transfers during an actual hospitalizations ([CEPHOS-LINK)](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186) project, p. 35).

Hospitalization may start from emergency clinic, and during inpatient care, transfers within and between hospitals and outpatient appointments may take place. Hilmo entries of these appointments are registered with possibly preliminary dignoses of that time. Hence, it is important to first identify the actual discharges and then identify the discharge diagnoses.

In previous papers using the Hilmo register, it is usually mentioned that hospital periods, admissions or discharges were identified. However, usually no criteria, let alone scripts, for this procedure is provided. As far as I know, there is no generally know or accepted methods for identifying treatment episodes available.

Recognizing real readmissions from transfer from unit to another is not straightforward. Different sets of criteria have previusly been used:
- Others require a hospital treatment to start and end on different calender days (f. ex. the [CEPHOS-LINK](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186) project )), others do not.
- Others state that a new treatment period may start the next day after previous one (f. ex. [CEPHOS-LINK](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186)), others require a full calender day outside of hospital in between two treatment periods (f. ex. [REDD project](http://urn.fi/URN:NBN:fi-fe201204193720)). This criteria is used to stronger differentiate transfers between hospitals and real rehospitalizations.


## Script Purpose: 
1. To propose a method for aggregating Hilmo entries in order to identify actual admissions, discharges, and discharge diagnoses from the register with different criteria. This is necessary in order to find out: 
   + dates of admission and discharge, i.e. the period actually spent in the hospital,
   + dates of admission to and discharge from psychiatric care, if a single hospitalization includes care in more than one specialty,
   + actual discharge diagnoses at the end of the hospitalization, or the last diagnosis from certain specialty, in this case psychiatry, and the service provider from where the discharge takes place.

2. To tell apart register entries related to outpatient episodes that take place during an inpatient care. There may be a need to consider these outpatient entries as a part of the inpatient care, not as separate episodes.

3. To provide this script accessible for critical evaluation and further utilization (despite the actual register data is not openly available). The aim is to let future (at least clinically oriented) researchers to focus on their actual research, not technicalities. 


## 1. Data Input:
Set the location of the raw data in **0_data_raw_to_parts.R**. The data folder must contain only the data files.

csv format for data input is currently used.

This project comes with fake data to see the scripts in action. Notice, the fake data does not necessary follow the real world patterns in any way!
- **Run create_fake_data.R first to generate fake test data**

## 2. Prepare Data for the Aggregation:
**Preparations are done in 0_data_raw_to_parts.R**

If your data is in form THL calls the database form, you need to combine the data from different files based on entry id. Variables are in different files and each year is in its own file. Use library(bit64) for long id numbers. 
- (Example not currently shown here)

What needs to achieved, is to have minimum of the following columns in your data:

Variable | Data type | Description
:--------|:----------|:------------
```shnro``` | character | person id. The name shnro is used by the Statistic Finland, it refers to a person, not ID number, which may change (in relative rare occasions).
```vuosi``` | integer   | Year of the entry
```ILAJI``` | integer | See Hilmo manuals for details. Note possible changes in the classifications between years. 
```PALTU``` | integer |
```PALA```  | integer |
```EA```    | character |
```TUPVA``` | integer | set: as.integer(as.IDate(TUPVA, format = [FORMAT]))
```LPVM```  | integer | set: as.integer(as.IDate(LPVM, format = [FORMAT]))
```dg```    | character | see beelow

Diagnoses: 
- One register entry may contain multiple diagnoses. 
- Names of the diagnosis variables may vary between years and datasets.
- Create variable ```dg```, where all diagnoses related to one entry are pasted. 
   + f. ex. ```dat[, dg := paste(DG1, DG2, sep = '_')]```

### Data Size, Operating with Chunks
If your data is in one file of reasonable size, all you need to do is to check you have above mentioned columns in the data and name the data as '1.csv'

If you have full register, you may need to process the data in smaller parts (chunks), due to performance. In this case, this method require all entries of a single person to locate in one part (instead of having the data split in parts by year and variables).

To determine the optimal part size is out of scope here. See what is small enough on your machine. In this example, five parts are used to demonstrate the action.


## 3. Processing of the Data
**Control setting for desired episode identification rules in 1_main_aggregation.R**

Variable | Description
:--------|:----------
```dir```| Input file locations for preprocessed data. In this example, data_temp/parts_raw
```max_year```| Time range of the data, the latest year included 
```min_year``` | The first year included. Earliest 1996. Previous years have their own methods.
```outpatient_start_year```| According to THL, outpatient data is relevant starting from 2006
```add_days``` |Days between periods, see below
```PALA_inpatient``` <br> ```PALA_outpatient``` | Register entry types defining treatments types of interest, see Hilmo manuals for details

### Days between periods ```add_days``` 
Minimum of full calender days required between two hospital treatment periods:

- 0 : a new period may start the next day after the previous one
- 1 : there must be one full calender day between two treatment periods. If less, Hilmo entries are considered to belong to a single episode (due to unit transfer etc.)

## Data Output
Full aggregated data is saved in the folders:

- data_processed -> 1_inpatient_episodes 
   + -> add_days_[add_days]: the data
   + -> preparation_description: description of incorrect entries, PALA distribution, etc.
- data_processed -> 2_in_and_outpatient_episodes -> add_days_[add_days]: the data and description of included episodes

This test script creates the following object:

- **dat_episodes**: processed data with inpatient and outpatient episodes included
- **dat_inpatient**: inaptient episodes only
- **description_episode** and **description_inpatient**: see the formatted xlsx files in folder data_processed


## New variables in the Processed Data
### Inpatient data:

Variable | Data type | Description
:--------|:----------|:--------
```jakso```        | integer | Running number of person's inpatient periods.
```psy```          | logical| Episode contains psychiatric care.
```overnight_all```| logical| Episode starts and ends on different calender days.
```overnight_psy```| logical| Episode's psychiatric treatment starts and ends on different calender days.
```tulopvm_psy```  | date| Date of admission to psychiatric inpatient care.
```lahtopvm_psy``` | date| Date of discharge from psychiatric inpatient care, if multiple transfers between specialties, the last one.
```ea_list```      | character| List of specialties included in the episode.
```paltu```        | character| The service provider's code of the unit of the discharge.
```paltu_psy```    | character| The service provider's code of the psychiatric unit of the last psychiatric discharge.
```n_rows_inpat``` | integer  | Number of rows aggregated to this inpatient episode.
```dg_inpat```     | character| All diagnoses registered on the date of discharge.
```dg_inpat_psy``` | character| All diagnoses registered from psychiatry on the date of discharge from psychiatric unit.

### All episodes, in addition:

Variable | Data type | Description
:--------|:----------|:--------
```n_episode```         |Integer | Running number of person's episodes.
```tulopvm_psy_inpat``` |Date | As tulopvm_psy       
```lahtopvm_psy_inpat```|Date | As lahtopvm_psy       
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

<br><br><br>
