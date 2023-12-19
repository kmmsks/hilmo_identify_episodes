
# hilmo_identify_episodes v 2.0.0

An R script to identify hospital admissions, discharges, discharge
diagnoses, and outpatient visits during inpatient episodes in the
Finnish [Care Register for Health
Care](https://thl.fi/en/web/thlfi-en/statistics-and-data/data-and-services/register-descriptions/care-register-for-health-care)
(FCRHC, commonly known as Hilmo register) and [The Register of Primary
Health Care
visits](https://thl.fi/en/web/thlfi-en/statistics-and-data/data-and-services/register-descriptions/register-of-primary-health-care-visits)
(RPHC, commonly known as AvoHilmo register) between 1975 and 2020 (and
possibly later but this has not been tested).

This script identifies all inpatient episodes from all medical
specialties and episodes related to psychiatric care specifically. The script can
handle secondary care inpatient and outpatient data and primary care
data. These behaviors can be generalized to specialties other than
psychiatry as well.

This scripts supplement this paper:

Suokas K, Gutvilig M, Pirkola S, Lumme S, Hakulinen C. Enhancing the
accuracy of register-based metrics: Comparing methods for handling
overlapping psychiatric register entries in Finnish healthcare
registries. Submitted. [medRxiv](https://www.medrxiv.org/content/10.1101/2023.12.07.23299655v1)

2023-12-05 [![DOI](https://zenodo.org/badge/299097747.svg)](https://zenodo.org/badge/latestdoi/299097747)

Author: Kimmo Suokas, firstname.lastname@helsinki.fi

Acknowledgments: I would like to express my gratitude to Mai Gutvilig for proofreading the materials.

## Background

In order to identify actual hospital admissions, discharges, and
discharge diagnoses from the Finnish healthcare registers, multiple
register entries may need be combined, as one hospitalization may
consist of multiple register entries.

This is because during a single hospitalization, a new register entry
must be supplied every time a hospital transfer or an intra-hospital 
transfer between specialties occurs. A register entry is
also supplied from each outpatient and emergency visit, which may take
place at the beginning or during the hospitalization.

Based on the previous efforts to handle these issues, four models can be
formulated in order to combine overlapping register entries:

| Model | Description                                                                                                                                                                                                                                                                                                                                                                                                    |
|:------|:---------------------------------------------------------------------|
| 1     | A new hospitalization may begin on the day after a previous hospitalization, with no specific minimum length required for a hospitalization. **This represents the most liberal approach.**                                                                                                                                                                                                                            |
| 2     | A new hospitalization may begin on the day after a previous hospitalization. Valid hospitalizations are those that extend over a minimum of two consecutive days, including at least one overnight stay. If both admission and discharge take place on the same day, the visit is classified as an outpatient visit. **This model was used in the CEPHOS-LINK project.**                                           |
| 3     | "A new hospitalization is allowed after a full day has been spent outside the hospital following the previous hospitalization. There is no specific minimum duration required for a hospitalization. **This model was used in the REDD project.**                                                                                                                                                                          |
| 4     | A new hospitalization is allowed after a full day has been spent outside the hospital following the previous hospitalization. Valid hospitalizations are those that extend over a minimum of two consecutive days, including at least one overnight stay. If both admission and discharge take place on the same day, the visit is classified as an outpatient visit. **This represents the most conservative model.** |

Note:

-   Models 1 and 3 find admissions, as some of the admissions do not
    necessarily result in hospitalization.
-   Models 2 and 4 differentiate overnight inpatient episodes from other
    visits to hospitals.
-   If the focus is on inpatient discharge diagnoses, model 4 may result
    with little less preliminary diagnoses included, compared to model
    2.
-   The secondary care contacts that are not considered inpatient episodes and are not within any inpatient episode are considered outpatient           events. Hence, the choice of model for identification of inpatient episodes affects the number of outpatient events as well.

See the paper for further information.

### Purposes of this script

1.  To propose a method for aggregating Hilmo entries in order to
    identify admissions, discharges, discharge diagnoses, and outpatient
    visits outside inpatient episodes from the partly overlapping
    register entries. This is necessary in order to find out:

    -   dates of admission and discharge, i.e., the period actually spent
        in the hospital,
    -   dates of admission to and discharge from psychiatric care, if a
        single hospitalization included care in more than one specialty,
    -   actual discharge diagnoses at the end of the hospitalization, or
        the last diagnosis from certain specialty, in this case
        psychiatry, and the service provider from whom the discharge
        took place. <br><br>

2.  To tell apart register entries related to outpatient episodes that
    took place during inpatient care. There may be a need to consider
    these outpatient entries as part of inpatient care, not as
    separate episodes.

3.  To provide fully reproducible scripts accessible for critical
    evaluation and further utilization (despite the real register data
    not being openly available).

<br>

## The Registers

The Discharge Register was launched in 1969. The method presented here
is suitable starting from the year 1975. Before that, recognizing
psychiatric treatments was not unequivocal and person identifications had
more errors. For details, see the paper.

| Years          | Diagnoses         | Description                                                                                                                                                      |
|:-------------|:-----------|:-------------------------------------------------|
| **1969--1974** | ICD-8             | Not covered in this method.                                                                                                                                      |
| **1975--1995** |                   | The older data is usually provided in three datasets, with slightly different formatting in each of them. Notice changes in variable coding within the datasets. |
|    1975--1986  | ICD-8             |                                                                                                                                                                  |
|    1987--1993  | ICD-9             |                                                                                                                                                                  |
|    1994--1995  | ICD-9             |                                                                                                                                                                  |
| **1996--2018** | ICD-10            | Data is convergent enough to be processed together. Refer to Hilmo manuals concerning the minor changes in the data between years.                               |
|    1998 -\>    | ICD-10            | First data on secondary care outpatient visits are included.                                                                                                     |
|    2006 -\>    | ICD-10            | Secondary care outpatient data are considered consistently comparable across time and service providers.                                                |
|    2011 -\>    | ICD-10 and ICPC-2 | Primary care outpatient appointments from public healthcare services are included in the registers.                                                              |
| **2019 -\>**   | ICD-10 and ICPC-2 | Major changes in the structure of the data.                                                                                                                      |

<br>

## Step-by-step processing

The numbered headings refer to the numbered sections in the `main.R`, which is the main script file that controls the whole behavior. All subfiles are located in the directory `R` and the first number in the name of the file also refers to these numbered sections.

### General considerations

####  ***Differences in the four models: `add_days` and overnight stays***

The two characteristics that separate the four models are:

##### `add_days`: the mimimun number of days between separate inpatient episodes

The minimum of full calender days required between two hospital
treatment periods:

-   0 : a new period may start the day after the previous hospitalization
    (models 1 and 2).
-   1 : there must be one full calendar day between two treatment
    periods (models 3 and 4). Otherwise, Hilmo entries are combined into
    a single episode.

##### Overnight: whether or not an inpatient episode extends over a minimum of two consecutive days

In models 2 and 4, valid hospitalizations are those that extend over a minimum of two consecutive days, incorporating at least one overnight stay. This can be determined after processing of the data by subsetting, see section 4. subsetting below.

#### Operating with chunks, longitudinal or annual

If working with the full registers, you may need to process the data in
chunks, due to computing performance issues.

The data are usually provided in so called annual format, meaning that
data on each year are in separate files.

Here, the data will be divided into chunks based on the first character
of the ID variable (called `shnro`) so that each individual's all data
is written into a single file. This will be referred to as the
**longitudinal** format.

If data from each year are handled separately, this is referred to as
**annual** format. Notice, that in this case episodes are cut at the last
day of each year. This behavior will be signaled, though.

#### Directory tree

This scripts create the following directory tree:

-   [data_folder_name]
    -   0_preprocessed_longitudinal\_[start_year]\_[end_year]
    -   0_preprocessed_annual
    -   1_identified_episodes
        -   add_days\_[add_days]
            -   longitudinal\_[start_year]\_[end_year]
    -   2_first_dates
        -   add_days\_[add_days]
            -   mental_health_services_2015_2020
            -   non_processed
                -   mental_health_services_2015_2020

`data_folder_name` can be specified in the functions. `data_main` as default.

`start_year`and `end_year` specify the the period of data within the longitudinal chunks.

#### Conversion of diagnostic classifications

ICPC-2, ICD-8 and ICD-9 codes are converted into ICD-10 codes, the conversion
key is set in `0a_preparation_settings_dgs.R`

Conversion codes from ICPC-2 to ICD-10 can be found from: 

- Kvist M, Savolainen T, Suomen Kuntaliitto. [ICPC-2 :perusterveydenhuollon
kansainvälinen luokitus](https://www.kuntaliitto.fi/julkaisut/2010/1344-icpc-2-perusterveydenhuollon-kansainvalinen-luokitus).
Suomen kuntaliitto, Helsinki 2010. p. 194, Finnish version. 
- [ICPC-2-R:International Classification of Primary
Care](https://www.who.int/standards/classifications/other-classifications/international-classification-of-primary-care),
p. 147

When an ICPC-2 code does not convert into ICD-10 but represents some kind
of mental disorder, group fx is used in these scripts.

<br>

### 0. Data preprocessing

#### Real data

Go through the scripts `0_preparation.R`,
`0a_preparation_settings_dgs.R`, and `0a_preparation_funs.R` in the
folder `R`, and define necessary settings and data paths.

These scripts harmonize variable names and types over time, recognize
treatment types, medical specialties, and diagnoses over time time with
varying classifications.

In this step, the pre-processed data will be saved longitudinally,
meaning that each individual's all data is written into a single file,
using `append = TRUE` in the `fwrite` function from `library(data.table)`.

#### `synthetize_data()`: Synthetic data for demonstration

Synthetic data may be used for testing these scripts. Access to the real
registers is limited. This function creates data for testing purposes.

##### Usage

`synthetize_data(n_rows = 20000, n_individuals = 1000,
  start_year = 2015, end_year = 2020, seed = 1,
  outpatient_proportion = .35, primary_care_proportion = .4, ilaji2_proportion = .05,
  save_data = TRUE, data_folder_name = 'data_main', longitudinal = TRUE)`

##### Arguments

|                         |           |                                                                                                                                                                              |
|:-------------|:------|:------------------------------------------------------|
| n_rows                  | Numeric   | number of rows to generate                                                                                                                                                    |
| n_individuals           | Numeric   | number of individuals to generate, n_individuals must be \<= n_rows                                                                                                          |
| start_year              | Numeric   | Start year of the data                                                                                                                                                       |
| end_year                | Numeric   | End year of the data                                                                                                                                                         |
| seed                    | Numeric   | Seed for the random generator                                                                                                                                                |
| outpatient_proportion   | Double    | The proportion of rows to be labeled as secondary outpatient care, must be between 0 and 1                                                                                   |
| primary_care_proportion | Double    | The proportion of rows to be labeled as primary care, must be between 0 and 1                                                                                                |
| ilaji2_proportion       | Double    | The proportion of rows to be labeled as patient count on the last day of each calendar year, must be between 0 and 1                                                         |
| save_data               | Logical   | Whether to save the generated data. If FALSE, the data.table will be returned, not saved                                                                                      |
| data_folder_name        | Character | The name of the file where data will be saved. See below                                                                                                                     |
| longitudinal            | Logical   | If TRUE, the data will be saved in longitudinal format, meaning that each individual's all data is written into a single file. if FALSE, data will be saved in annual format |

<br>

### 1. Processing

These are the main operations:

First, the following lines are used for identifying inpatient episodes
with possible overlapping dates:

```
# Order by  id, admission date, and discharge date
    setkey(inpat_0, shnro, tupva, lpvm)
  
  # Threshold for the highest lpvm (= end date) so far
  inpat_0[, highest_so_far := shift(cummax(lpvm), fill = lpvm[1]), by = shnro]
  
  #Chaining of the overlapping dates. 
  #If tupva (start date) is higher than the highest end date  so far + the add_days value before the current row,
  # a new episode starts. Otherwise, the row is recognized to be a part of the episode that is already
  # ongoing in the previous row(s).
  
  inpat_0[, `:=`(subgroup = .GRP), by = .(shnro, g = cumsum(tupva > highest_so_far + add_days)) ]
  
  # Running number of episode
  inpat_0[order(subgroup), episode := cumsum(!duplicated(subgroup)), by = shnro]
```

Where `inpat_0` is pre-processed inpatient data, `shnro` is the ID variable, `tupva` is an admission date in integer format, and `lpvm` is a discharge date in integer format. 

After this step, the episodes can be aggregated by id `shnro`and
`episode`. The desired aggregation functions need to be defined for each
variable of interest.

Second, outpatient appointments during the inpatient episodes were
identified in a similar way. The aggregation of desired information is more
complex at this stage if diagnoses of mental disorders and other
diagnoses from different medical specialties at the different stages of
the episode need to be specified. 

Third, primary care appointments during inpatient episodes or at the same day with secondary care psychiatric outpatient visits are ignored.

See `1a_processing_subfuns.R` for details regarding these behaviors.

#### `process_data()`: function to control the main processing

##### Usage

`process_data(add_days, start_year, end_year, longitudinal = TRUE,  process_secondary_outpatient = TRUE, process_primary_care = TRUE, separate_files_for_old_registers = FALSE)`

##### Arguments

|                                           |         |                                                                                                                                                        |
|:-----------------|:-----------------|:-----------------------------------|
| add_days                                  | Numeric | The minimum number of full calender days required between two hospital treatment periods. See general considerations above. |
| start_year                                | Numeric | Start year of the data                                                                                                                                 |
| end_year                                  | Numeric | End Year of the data                                                                                                                                   |
| longitudinal                              | Logical | If each individual's all data is written into a single file set TRUE, if the data is in the annual format, set FALSE, see general considerations above |
| process_secondary_outpatient              | Logical | If only inpatient care data is processed, set to FALSE, otherwise TRUE                                                                                 |
| process_primary_care                      | Logical | If only secondary care data is processed, set to FALSE, otherwise TRUE                                                                                   |
| separate_files_for_old_registers          | Logical | If years before 1996 are saved to separate files in pre-processing, set TRUE, if working with synthetic data, must be set to FALSE                  |

##### Data output location

Processed data are saved into the location [data_folder_name] -\>
1_identified_episodes -\> add_days\_[add_days] -\>
longitudinal\_[start_year]\_[end_year]

Files are named as follows:

`[chunk]_1_inpatient_outpatient.csv`

`[chunk]_primary_care.csv`

##### Variables in the  processed data

| Variable                | Variable type | Description                                                                                                                                            |
|:------------------|:------------------|:----------------------------------|
| `shnro`                 | Character     | ID                                                                                                                                                     |
| `event`                 | integer       | Running number of person's inpatient or outpatient contacts in the FCRHC (secondary care register) after the processing.                               |
| `vuosi`                 | Integer       | Year of discharge                                                                                                                                      |
| `tulopvm`               | date          | Date of admission to inpatient care or date of outpatient visit.                                                                                       |
| `lahtopvm`              | date          | Date of discharge from inpatient care, if multiple transfers between specialties, the last one. For outpatient visits, the end date of the visit.      |
| `tulopvm_psy_inpat`     | date          | Date of admission to psychiatric inpatient care.                                                                                                       |
| `lahtopvm_psy_inpat`    | date          | Date of discharge from psychiatric inpatient care, if multiple transfers between specialties, the last one.                                            |
| `episode_continues`     | logical       | True if inpatient episode continues after the last day covered in the data. In this case, `lahtopvm` is the last day covered in the data. Note<sup>a</sup>             |
| `episode_psy_continues` | logical       | True if inpatient episode continues after the last day covered in the data. In this case, `lahtopvm_inpat_psy` is is the last day covered in the data.  Note<sup>a</sup> |
| `psy`                   | Logical       | TRUE if event contains psychiatry. Note<sup>a</sup>                                                                                                                               |
| `inpat`                 | Logical       | TRUE if the event includes inpatient care.  Note<sup>a</sup>                                                                                                               |
| `inpat_psy`             | Logical       | TRUE if the event includes psychiatric inpatient care. Note<sup>a</sup>                                                                                                  |
| `overnight`             | Logical       | TRUE if inpatient episode lasts overnight. Note<sup>a</sup>                                                                                                               |
| `overnight_psy`         | Logical       | TRUE if inpatient episode in psychiatric unit lasts overnight. Note<sup>a</sup>                                                                                           |
| `outpat`                | Logical       | Event contains secondary outpatient care. Note<sup>a</sup>                                                                                                                |
| `primary_care`          | Logical       | TRUE if the row presents primary care appointment.                                                                                                      |
| `dg_inpat`              | Character     | All diagnoses registered on the date of discharge.                                                                                                     |
| `dg_inpat_psy`          | Character     | All diagnoses registered from psychiatry on the date of the last discharge from psychiatric unit during the inpatient episode.                         |
| `dg_outpat`             | Character     | Outpatient diagnoses. Contains outpatient diagnoses during inpatient care, also. No psychiatric diagnoses from other specialties during psychiatric inpatient care |
| `dg_outpat_psy`         | Character     | Outpatient diagnoses during inpatient care only from psychiatric outpatient services                                                                   |
| `dg_all`                | Character     | All diagnoses during the event (secondary care only)                                                                                                   |
| `dg_psy`                | Character     | All relevant psychiatric diagnoses from the FCRHC (secondary care registers). Preliminary diagnoses excluded.                                           |
| `dg_avo`                | Character     | Primary care diagnoses                                                                                                                                 |
| `outpat_same_day`       | Logical       | Two or more secondary outpatient appointments on a single day                                                                                         |
| `n_appointments`        | Integer       | Number of primary care appointments on a single day                                                                                                    |
| `paltu`                 | character     | The service provider's code of the unit of discharge.                                                                                              |
| `ilaji_2_n`             | Integer       | The number of ILAJI == 2 rows within the inpatient episode.                                                                                            |
| `n_rows`                | Intger        | The total number of rows within the event episode                                                                                                    |
| `ea_list`               | Character     | All medical specialties within the event                                                                                                               |
| `highest_so_far`        | Integer       | Technical variable for quality control only                                                                                                           |
**Notes:**

- a: Always FALSE in primary care.

<br>

### *Postprocessing*

### 2. First dates

Now the identification of inpatient episodes and outpatient and primary care appointments is done. Next, the data are ready for analysis of post-processing procedures. Here, each individual's first occurrence of diagnoses of interest is determined. This is useful for incidence studies.

#### `get_first_dates()`: function to control the evaluation of the date of incidence

##### Usage

`get_first_dates(add_days, start_year, end_year, dg_age = dg_groups_w_min_ages)`

##### Arguments

|                |         |                                           |
|:---------|:-----------------|:---------------------------------------------|
| add_days    | Numeric     | The value of add_days in the data to be processed  |
| start_year  | Numeric     | Start year of the data to be processed |
| end_year    | Numeric     | End Year of the data to be processed  |
| dg_age      | data.table  | A data.table such as `dg_groups_w_min_ages` from `2a_first_dates_set_diagnoses.R` or similar that contains diagnoses of interest, and minimum age for every diagnostic category. See below.|

`dg_age` must have structure such as this:

`r data.table::data.table(dg = c("f2", "schizophrenia", "bipolar_disorders", "psychoses"), dg_code = c("f2", "f20", "f30|f31", "f20|f22|f23|f24|f25|f28|f29|f301|f302|f308|f309|f311|f312|f315|f316|f323|f333|f1.5|f1.7"), min_age = c(5,10,5,5) )`

- `dg` is the name of the category
- `dg_code` is the ICD-10 codes in the category, separated by `|`, regex allowed, and case insensitive.
- `min_age` is the minimum age when the diagnosis is considered valid for this process.

##### Details

The default behavior for inpatient episodes is that discharge must be at or after the age of `min_age`, and the date of incidence is the admission date of the episode. Hence, the incident date may be before the age of `min_age`. Incident dates before `min_age` can be set to `min_age`
afterwards.

##### Returns 

A data.table with the following columns

|                |         |                                           |
|:---------|:----------|:------------------------------------------------------|
| shnro               | Character   | Personal ID |
| birthday            | iDate       |  Person's birthday |
| date_any_psy        | iDate       | The date of the incident psychiatric medical contact, no age limits. |
| date_any_psy_w_f_dg | iDate       | The date of the incident psychiatric medical contact with a diagnosis of any mental disorder, no age limits. |
| date_[diagnosis]    | iDate       | The date of the incident diagnosis of interest, with minimum age considered. The diagnoses of interest are defined in the `dg_age` argument. |
| source variable     | Character   | The source of each date of incidence  ("inpatient", "outpatient", or "primary_care").  |

<br>

### 3. Look at the data

The processed data are saved in chunks. In this section, all data are read to memory and organized into a list for inspection.
`read_files_from()`is a helper to read and combine the chunks.

First dates are in single files. The four models are read into a list.

<br>

### 4. Subsetting

Identifying inpatient episodes that last overnight (models 2 and 4) can be done simply by selecting those episodes with `tulopvm < lahtopvm` (i.e. admission date < discharge date) and considering inpatient episodes with `tulopvm == lahtopvm` (i.e. admission date == discharge date) as outpatient events.

Here are some examples of subsetting the processed data, and counting episodes and time spent in hospital by person.

Below, `dat` refers to the processed data, for example, `dat_processed$ad0` or `dat_processed$ad1`.

#### Only overnight episodes considered inpatient episodes

If only overnight episodes are considered inpatient treatments
(models 2 and 4):

-   Psychiatric inpatient episodes
    -   `dat[overnight_psy == TRUE]`
-   Psychiatric outpatient episodes
    -   including psychiatric outpatient visits during non-psychiatric
        inpatient care
          - `dat[psy == TRUE & overnight_psy == FALSE]`
    -   only psychiatric outpatient visits when not in inpatient care
          - `dat[psy == TRUE & overnight == FALSE]`

#### All episodes with inpatient entries

If all episodes with any register entry from inpatient care (i.e., also
shorter than overnight episodes) are considered inpatient care (models 1
and 3):

-   Psychiatric inpatient episodes:
    -   `dat[psy == TRUE]`
-   Psychiatric outpatient episodes
    -   including psychiatric outpatient visits during non-psychiatric
        inpatient care
          - `dat[psy == TRUE & inpat_psy == FALSE]`
    -   only psychiatric outpatient visits when not in inpatient care
          - `dat[psy == TRUE & inpat == FALSE]`

#### Number of episodes by person

To get the number of episodes by person, get .N by shnro, e.g., the number of
psychiatric inpatient episodes by person (models 2 and 4) 

- `dat[overnight_psy == TRUE, .N, by = shnro]`

#### Number of days hospitalized by person

To get the total number of days hospitalized in all medical specialties throughout the data:

- `dat[inpat == TRUE, .(days_hospitalized = lahtopvm - tulopvm), by = shnro][
  , .(days_hospitalized = sum(days_hospitalized)), by = shnro][]`

#### Number of days hospitalized in psychiatric care by person

To get the total number of days hospitalized in psychiatric care:

- `dat[inpat_psy ==T, .(days_hospitalized = lahtopvm - tulopvm), by = shnro][, .(days_hospitalized = sum(days_hospitalized)), by = shnro][]`

Note: if patient is transferred from psychiatric inpatient care to another
specialty and then back, the days spent in the other specialty are also
covered. Days spent in other specialties after the last discharge (or
before the first admission to psychiatry) are not covered.

#### Diagnoses

Relevant psychiatric diagnoses, including those set at

- the discharge from psychiatric inpatient care,
- psychiatric secondary outpatient care but not during inpatient care, or 
- primary care but not during inpatient care or on the same day with psychiatric secondary outpatient care

- `dat[, dg_psy_all_relevet := paste(na.omit(dg_psy), dg_avo, sep = "_") %>% str_remove_all("_NA")]`

<br>

## Version history

-   2.0.0 (2023-12-07): New functional format for the scripts. The coding of
    treatment types in the Hilmo rgisters changed in 2019 and is now
    included in this method. The current behaviour is tested with data
    up to the year 2020. ICD-8 and ICD-9 conversion are included in more
    datail. This version supplements this paper: LINK TO PAPER.

-   [1.1.1](https://github.com/kmmsks/hilmo_identify_episodes/tree/1.1.1-old_version) (2022-05-23): Info on primary care included.Typos.

-   1.1.0-beta (2021-05-18): Inference of discharge dates fixed. Fake
    data supplemented. Typos.

-   1.0.2 (2020-11-29): First stable version. In this version, however,
    discharge date (`lahtopvm`) was the date of discharge in the
    register entry with the latest admission. This may give too early
    discharge dates. Admission dates and numbers of episodes were
    correctly calculated.

<br>

## Citation

To the paper

<br><br><br>
