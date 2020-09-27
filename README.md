# hilmo_identify_episodes
Identify hospital admissions, discharges and discharge diagnoese from the Finnish [Care Register for Health Care](https://thl.fi/en/web/thlfi-en/statistics/information-on-statistics/register-descriptions/care-register-for-health-care) ("Hilmo" register). This script identifies all episodes, and episodes related to pychiatric care.


## Background
In order to identify actual hospital admissions, discharges and discharge diagnoese from the Finnish Care register for health care, multiple register entries may need be combined, as one hospitalization may consist of multiple entries. 

This is because during a single hospitalization, a new register entry must be supplied every time a hospital transfer or change in form one spiciality to another within the hospital occures. A register entry is also supplied from outpatient and emergency visit, which may take place at the beginning or during the hopsitalization.

In previous research, up to 25 % of the psychiatric inpatient care related register entries have been related to transfers during an actual hospitalizations ([CEPHOS-LINK project, p. 35](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186))

Hospitalization may start from emergency clinic, and during inpatient care, transfers within and between hospitals and outpatient appintments may take place. Hilmo enries of these appointments are registered with possibly preliminary diagnoses. Hence, it is important to identify the actual discharges, in order to identify the discharge diagnoses.

In papers using the Hilmo register, it is usual to mention that hospital periods, admissions or discharges were identified. However, usually no criteria, let alone scripts, for this procedure is presented. As far as I know, there is no generally know or accpeted methods for treatment period recognizition available.

However, recognizing real readmissions from transfer from unit to another is not streightforward.
Different sets of criteria have been used:
- Others require a hospital treatment to start and end on different calender days (the [CEPHOS-LINK](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186) project f. ex.)), others don't
- others state that a new treatment period may start the next day after previous one([CEPHOS-LINK](https://thl.fi/documents/189940/2732416/CEPHOS-LINK+final+scientific+report+2017-03-31+export.pdf/6f206810-5919-415c-82a1-884795732186)), others require a full calender day outside of hospital in between two treatment periods ([REDD project](http://urn.fi/URN:NBN:fi-fe201204193720) f. ex.). This criteria is used to stronger differentiate transfers between hospitals ande real rehospitalizations.


 Script purpose: 
1. To propose a method for aggregating Hilmo entries in order to identify actual admissions, discharges and dischrge diagnoses from the register with different criteria. This is necassery in order to find out 
- dates of admission and discharge, i.e. the period actually spent in the hospital,
- dates of admission to and discharge from psychiatric care, if a single hospitalization includes care in more than one speciality,
- actual discharge diagnoses at the end of the hospitalization, or the last diagnosis from certain speciality, in this case psychiatry, and the service provider from where the discharge takes place.

2. To tell apart register entries related to outpatient episodes that take place during an inpatient care. There may be need to consider these outpatient entries as a part of the inpatient care, not as separate episodes.

3. Provide this script accessible for critical evaluation and further use, despite the actual register data is not openly availabel. Hopefully, let futrure (at least clinically oriented) researchers to focus on their actual research, not technicalities. 


## Data input:
   * csv format
   * Hilmo starting 1996:
     * Data input is in parts, where all Hilmo entries from a single person must locate in the same part.
     * contrary to the original data form, which is devided according to year 
     * run Hilmo_csv_to_parts.R before this script
     * If smaller sample, all entries after 1995 may be bind to one file.
   * Older Hilmos (1969-1994) unprocessed csv files.

## Output:
   * data_inpatient_[start_year]_[end_year].fst: aggregated data and same in parts in folder 
     data_processed / add_days_[add_days]
   * preaparation_description.xlsx: description of excluded rows, PALA distibution, etc. in a subfolder

 Author: Kimmo Suokas