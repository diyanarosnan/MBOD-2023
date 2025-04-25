## PUSAT INFORMASI KESIHATAN 2023
This is a walk through tutorial for the data sorting using dataset from PIK.

- [ ] [Load library](#load-the-necessary-libraries)
- [ ] Calculate age for rows with missing values
- [ ] Filter for your desired disease
- [ ] Remove missing age, sex and those who died
- [ ] Summarise the data based on age group & sex
- [ ] Upload mid-year population 2023

### LOAD THE NECESSARY LIBRARIES
```r
library(dplyr)
library(tidyr)
```

### LOAD THE R DATA FILE FROM PIK
```r
load("/directory/path/to/file.xlsx")
```
>[!NOTE]
>There is more than one way to load your file!
>
> If your file is in CSV format, remember to change file extension from `.xlsx` to `.csv`

### CALCULATE AGE FOR ROWS WITH -1 VALUES IN THE `patient_age_single` COLUMN
Identify the rownames in the `Inp2023_burden` where `patient_age_single` is equal to -1.
```r
target_indices <- which(Inp2023_burden$patient_age_single == "-1")
```

Create a separate dataframe (`df`). Extract the rows with unknown age from `Inp2023_burden`.
```r
df <- Inp2023_burden[target_indices, ]
```

Generate a new column (`dob_clean`) and insert the date of birth.
```r
df$dob_clean <- as.Date(sub(" .*", "", df$dob), format = "%Y/%m/%d")
```
```
as.date()           : converts character to date
sub()               : substitute
(" .*", "", df$dob) : (string after first space (" .*"), replace with nothing (""), extract from (df$dob)
                    : it will replace anything after the first space with nothing.
                    : BEFORE df$dob      = 2021/04/21 00:00:00
                    : AFTER df$dob_clean = 2021/04/21

as.date( 2021/04/21 , format = "%Y/%m/%d")
as.date ()          : convert character/string to date
format              : tells R how the date is structured
%Y/%m/%d            : year/month/date
```

Extract the year from column `dob_clean` and place it in a new column (`year`).
```r
df$year <- as.numeric(format(df$dob_clean, "%Y"))
```

Calculate the age and upate the values in the `patient_age_single` column.
```r
df$patient_age_single <- 2023 - df$year
```

Extract age > 0 and substitute the -1 to the newly calculated age.
```r
df_clean <- df[df$patient_age_single >= 0 & !is.na(df$patient_age_single), ]
target_indices <- as.numeric(rownames(df_clean))
Inp2023_burden_clean <- Inp2023_burden
Inp2023_burden_clean$patient_age_single[target_indices] <- df_clean$patient_age_single
```
#### GENERATE A CSV FILE
This command will save your data `Inp2023_burden_clean` as a CSV file named `"PIK 2023.csv"` in your current working directory.
```r
write.csv(Inp2023_burden_clean, "PIK 2023.csv")
```

#### THE `patient_gender` COLUMN HAS FIVE UNIQUE VALUES
```r
unique(Inp2023_burden_clean$patient_gender)
> unique(Inp2023_burden_clean$patient_gender)
"LELAKI"  "PEREMPUAN"  "Tiada Maklumat"  "NOT AVAILABLE"  "RAGU"
```

#### FILTER FOR DESIRED DISEASE CODE (IN THIS TUTORIAL, I WANT TO OBSERVE COPD)
```r
disease_data <- Inp2023_burden_clean %>% 
  filter(icd10_3d_code %in% c("J41", "J42", "J43", "J44"))
```
#### CHANGE THE DISEASE CODE USING THE ```filter()``` FUNCTION.
```r
DIARRHOEAL DISEASE: A00, A02-A04, A06-A09
disease_data <- Inp2023_burden_clean %>% 
  filter(icd10_3d_code %in% c("A00", "A02", "A03", "A04", "A06", "A07", "A08", "A09")) 
```
```r
FILTER FOR: RHEUMATIC HEART DISEASE: I01-I09
disease_data <- Inp2023_burden_clean %>% 
  filter(icd10_3d_code %in% c("I01", "I02", "I03", "I04", "I05", "I06", "I07", "I08", "I09"))
```

#### FILTER OUT MISSING DATA (FOR YOUR REFERENCE)
```r
missing_summary <- disease_data %>%
  filter(patient_gender %in% c("RAGU", "NOT AVAILABLE", "Tiada Maklumat") |  patient_age_single == "-1")
```

#### REMOVE DATA WITH MISSING GENDER, MISSING AGE AND THOSE WHO DIED.
```r
disease_data <- disease_data %>%
  filter(patient_gender %in% c("PEREMPUAN", "LELAKI")) %>%
  filter(patient_age_single >= 0, !disch_type_desc == "MATI")

```


>[!IMPORTANT]
> Make sure to put **double quotation mark("")** and **comma (,)** between your code.

>[!CAUTION]
> Entering values like **"I01-I09"** will not work in R .
> You have to write down one by one, **"I01"** , **"I02"** , **"I03"**

#### GENERATE A SUMMARY TABLE
```r
copd_summary <- disease_data %>%
  filter(patient_gender %in% c("PEREMPUAN", "LELAKI")) %>%
  mutate(age_group = case_when(
    patient_age_single >= 0 & patient_age_single <= 4 ~ "0-5",
    patient_age_single >= 5 & patient_age_single <= 14 ~ "5-14",
    patient_age_single >= 15 & patient_age_single <= 29 ~ "15-29",
    patient_age_single >= 30 & patient_age_single <= 44 ~ "30-44",
    patient_age_single >= 45 & patient_age_single <= 59 ~ "45-59",
    patient_age_single >= 60 & patient_age_single <= 69 ~ "60-69",
    patient_age_single >= 70 & patient_age_single <= 79 ~ "70-79",
    patient_age_single >= 80 ~ "80+")) %>%
  group_by(age_group, patient_gender) %>%
  summarise(count = n(), .groups = 'drop')
```
```r
> copd_summary
## A tibble: 16 × 3
##   age_group patient_gender count
##   <chr>     <chr>          <int>
## 1 0-5       LELAKI           478
##  2 0-5       PEREMPUAN        271
##  3 15-29     LELAKI            84
##  4 15-29     PEREMPUAN         56
##  5 30-44     LELAKI           791
##  6 30-44     PEREMPUAN        135
##  7 45-59     LELAKI          4904
##  8 45-59     PEREMPUAN        613
##  9 5-14      LELAKI           148
## 10 5-14      PEREMPUAN        117
## 11 60-69     LELAKI          9247
## 12 60-69     PEREMPUAN       1384
## 13 70-79     LELAKI          8127
## 14 70-79     PEREMPUAN       1866
## 15 80+       LELAKI          2798
## 16 80+       PEREMPUAN       1118
```
#### CHANGE THE STRUCTURE OF THE TABLE TO BECOME WIDER
```r
copd_summary <- copd_summary %>%
  pivot_wider(names_from = patient_gender, values_from = count, values_fill = 0) %>%
  arrange(age_group)
```

```r
> copd_summary
##  A tibble: 8 × 3
##  age_group LELAKI PEREMPUAN
##   <chr>      <int>     <int>
## 1 0-5          478       271
## 2 15-29         84        56
## 3 30-44        791       135
## 4 45-59       4904       613
## 5 5-14         148       117
## 6 60-69       9247      1384
## 7 70-79       8127      1866
## 8 80+         2798      1118
```

#### UPLOAD MID-YEAR POPULATION 2023
```r
population_2023 <- read.xlsx("/directory/to/file.xlsx")
```

############# DATA / MID-YEAR POPULATION 2023 = PREVALENCE 2023
############# PREVALENCE 2023 X DW ETC ETC
