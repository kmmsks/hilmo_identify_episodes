# hilmo_identify_episodes

R script to identify hospital admissions, discharges and discharge diagnoses from the Finnish [Care Register for Health Care](https://thl.fi/en/web/thlfi-en/statistics/information-on-statistics/register-descriptions/care-register-for-health-care) ("Hilmo" register). 
This script identifies all episodes, and episodes related to psychiatric care, but can easily be generalized to other specializes

Version 0.1 (2020-09-28)

Author Kimmo Suokas, kimmo.suokas@tuni.fi

## Background
In order to identify actual hospital admissions, discharges and discharge diagnose from the Finnish Care register for health care, multiple register entries may need be combined, as one hospitalization may consist of multiple entries. 

This is because during a single hospitalization, a new register entry must be supplied every time a hospital transfer or change in form one specialty to another within the hospital occurs. A register entry is also supplied from outpatient and emergency visit, which may take place at the beginning or during the hospitalization.

In previous research, up to 25 % of the psychiatric inpatient care related register entries have been related to transfers during an actual hospitalizations ([CEPHOS-LINK)](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186), project, p. 35).

Hospitalization may start from emergency clinic, and during inpatient care, transfers within and between hospitals and outpatient appointments may take place. Hilmo entries of these appointments are registered with possibly preliminary diagnoses. Hence, it is important to identify the actual discharges, in order to identify the discharge diagnoses.

In papers using the Hilmo register, it is usual to mention that hospital periods, admissions or discharges were identified. However, usually no criteria, let alone scripts, for this procedure is presented. As far as I know, there is no generally know or accepted methods for treatment period recognition available.

However, recognizing real readmissions from transfer from unit to another is not straightforward.
Different sets of criteria have been used:
- Others require a hospital treatment to start and end on different calender days (f. ex. the [CEPHOS-LINK](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186) project )), others don't
- others state that a new treatment period may start the next day after previous one([CEPHOS-LINK](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186)), others require a full calender day outside of hospital in between two treatment periods (f. ex. [REDD project](http://urn.fi/URN:NBN:fi-fe201204193720)). This criteria is used to stronger differentiate transfers between hospitals and real rehospitalizations.


## Script Purpose: 
1. To propose a method for aggregating Hilmo entries in order to identify actual admissions, discharges and discharge diagnoses from the register with different criteria. This is necessary in order to find out 
   + dates of admission and discharge, i.e. the period actually spent in the hospital,
   + dates of admission to and discharge from psychiatric care, if a single hospitalization includes care in more than one specialty,
   + actual discharge diagnoses at the end of the hospitalization, or the last diagnosis from certain specialty, in this case psychiatry, and the service provider from where the discharge takes place.

2. To tell apart register entries related to outpatient episodes that take place during an inpatient care. There may be need to consider these outpatient entries as a part of the inpatient care, not as separate episodes.

3. To provide this script accessible for critical evaluation and further open use (despite the actual register data is not openly available). The aim is to let future (at least clinically oriented) researchers to focus on their actual research, not technicalities. 


## Data Input:
- csv format. set location of the data in 1_main_aggregation.R
- This project comes with fake data to see the scripts in action. The fake data does not necessary follow the real world patterns in any way! 

## Prepare Data for the Aggregation:
- This is done in 0_data_raw_to_parts.R
- What you need to achieve, is to have minimum of the following columns in your data:
   + 'shnro': character. person id. The name shnro is used by the Statistic Finland, it refers to a person, not ID number, which may change (in relative rare occasions).
   + 'vuosi': numeric. Year of the entry
   
   + The following variables, details in the Hilmo manual. Note possible changes in the classifications between years. Desired data types:
      + 'ILAJI' : numeric
      + 'PALTU' : numeric
      + 'PALA'  : numeric
      + 'EA'    : character
      + 'TUPVA' : as.integer(as.IDate(TUPVA, format = [FORMAT]))
      + 'LPVM'  : as.integer(as.IDate(LPVM, format = [FORMAT]))
      
   + Diagnoses: 
      + One register entry may contain multiple diagnoses. Names of the diagnosis variables may vary between years and datasets.
      + Combine diagnoses to one character variable called 'dg'


- If your data is in one file of reasonable size, all you need to do is to check you have above mentioned columns in the data and name it '1.csv'

- If your data is in form THL calls the database form, you need to combine the data from different files based on entry id. Variables are in different files and each year is in its own file. Use library(bit64) for long id numbers. 
   + (Example not currently shown here)


- If you have the full register, you may need to process the data in smaller parts (chunks), due to performance reasons. In this case, this method require all entries of a single person to locate in one part (instead of having the data split by year).
- To determine the optimal part size is out of scope here. See what is small enough on your machine. This example uses five parts.


## Processing of the Data
- Control setting for desired episode identification rules in 1_main_aggregation.R

## Data Output
- Full aggregated data is saved in the folders 
   + data_processed -> 1_inpatient_episodes 
      + -> add_days_[add_days]: the data
      + -> preparation_description: description of incorrect entries, PALA distribution, etc.
   + data_processed -> 2_in_and_outpatient_episodes -> add_days_[add_days]: the data and description of included episodes


# New variables in the Processed Data
- Inpatient data:
   + jakso: int. Running number of person's inpatient periods.
   + psy: logical. Episode contains psychiatric care.
   + overnight_all: logical. Episode starts and ends on different calender days.
   + overnight_psy: logical. Episode's psychiatric treatment starts and ends on different calender days.
   + tulopvm_psy: date. Date of admission to psychiatric inpatient care.
   + lahtopvm_psy: date. Date of discharge from psychiatric inpatient care, if multiple transfers between specialties, the last one.
   + ea_list: character. List of specialties included in the episode.
   + paltu: character. The service provider's code of the unit of the discharge.
   + paltu_psy: character. The service provider's code of the psychiatric unit of the last psychiatric discharge.
   + n_rows_inpat: int. Number of rows aggregated to this inpatient episode.

   + dg_inpat: character. All diagnoses registered on the date of discharge.
   + dg_inpat_psy: character. All diagnoses registered from psychiatry on the date of discharge from psychiatric unit.

- All episodes, in addition:
   + n_episode: int. Running number of person's episodes.
   + tulopvm_psy_inpat. As tulopvm_psy       
   + lahtopvm_psy_inpat. As lahtopvm_psy       
   + inpatient. Logical. Any inpatient care included in the episode.
   + inpatient_psy. Logical. Psychiatric inpatient care included in the episode.
   + n_rows_episode: int. Number of rows aggregated to this inpatient episode.
   + outpat_same_day: Logical. More than one outpatient visit on the same day.           

   + dg_outpat: char. Outpatient diagnoses during the episode.
      + includes psychiatric outpatient diagnoses, if the episode contains no psychiatric inpatient care.
      + otherwise, outpatient diagnoses during inpatient care are included. 
      + this diagnoses should be excluded when only discharge diagnoses are collected.
   + dg_inpat_psy_outpat_psy: character. Psychiatric outpatient diagnoses during psychiatric inpatient episode.
      + Usually to be excluded.
   + dg_psy_inpat: char. If inpatient period contains psychiatric outpatient care, but not inpatient care, psychiatric outpatient diagnoses are here
   + dg. char. Combination of relevant discharge diagnoses, discharge diagnoses from psychiatric inpatient care preferred:
      + when inpatient== TRUE & inpatient_psy == FALSE, dg_inpat column included,
      + when inpatient== TRUE & inpatient_psy == TRUE, dg_inpat_psy column included,
      + when inpatient== FALSE, dg_outpat included.


