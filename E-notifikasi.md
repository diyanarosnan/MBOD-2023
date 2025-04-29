Filter out data with no age, and MATI 

#### UPLOAD DATA

```r
HFMD <- read.xlsx("directory/to/file/file_name.xlsx", sheet =  "HFMD")
```
#### FILTER OUT THE DATA WHERE THE AGE IS NA OR DEATH DATA 
```r
HFMD.mia <- HFMD %>% filter(is.na(`Umur.(Tahun)`) & is.na(`Umur.(Bulan)`) & is.na(`Umur.(Hari)`)| Status.Pesakit == "Mati")
```
Returns rows in `HFMD` that do not have matching rows in `HFMD.mia`
```r
HFMD <- (anti_join(HFMD, HFMD.mia))
```

#### GENERATE A SUMMARY TABLE

`mutate()` will generate a new column called `age_group`. We will assign each age into a specific age group as shown below.

Data was grouped based on `age_group` and `Jantina` followed by `summarise()` function.

`summarise()` will tell you the count for each age group based on their gender.
```r
summary_HFMD <- HFMD  %>%
  mutate(age_group = case_when(
  `Umur.(Hari)` >= 0 & `Umur.(Hari)` <= 31 ~ "0-4",
  `Umur.(Bulan)` >= 0 & `Umur.(Bulan)` <= 12 ~ "0-4",
  `Umur.(Tahun)` >= 0 & `Umur.(Tahun)` <= 4 ~ "0-4",
  `Umur.(Tahun)` >= 5 & `Umur.(Tahun)` <= 14 ~ "5-14",
  `Umur.(Tahun)` >= 15 & `Umur.(Tahun)` <= 29 ~ "15-29",
  `Umur.(Tahun)` >= 30 & `Umur.(Tahun)` <= 44 ~ "30-44",
  `Umur.(Tahun)` >= 45 & `Umur.(Tahun)` <= 59 ~ "45-59",
  `Umur.(Tahun)` >= 60 & `Umur.(Tahun)` <= 69 ~ "60-69",
  `Umur.(Tahun)` >= 70 & `Umur.(Tahun)` <= 79 ~ "70-79",
  `Umur.(Tahun)` >= 80 ~ "80+")) %>%
  group_by(age_group, Jantina) %>%
  summarise(count = n(), .groups = 'drop')
```
```
> summary_HFMD
##   A tibble: 15 × 3
##   age_group Jantina   count
##   <chr>     <chr>     <int>
## 1  0-4       Lelaki    50919
## 2  0-4       Perempuan 39925
## 3  15-29     Lelaki      735
## 4  15-29     Perempuan   942
## 5  30-44     Lelaki      625
## 6  30-44     Perempuan   650
## 7  45-59     Lelaki       63
## 8  45-59     Perempuan    52
## 9  5-14      Lelaki    26111
## 10 5-14      Perempuan 20245
## 11 60-69     Lelaki        5
## 12 60-69     Perempuan    11
## 13 70-79     Lelaki        1
## 14 70-79     Perempuan     1
## 15 80+       Lelaki        3
```

To make it easier to view the data, we will use `pivot_wider()`

`pivot_wider()` will increase the number of columns and decrease the number of rows, resulting in a wider dataframe

```r
summary_HFMD <- summary_HFMD %>%
                pivot_wider(names_from = Jantina, values_from = count, values_fill = 0) %>%
                arrange(age_group)
```
```
> summary_HFMD
##   A tibble: 8 × 3
##   age_group Lelaki Perempuan
##   <chr>      <int>     <int>
## 1 0-4        50919     39925
## 2 15-29        735       942
## 3 30-44        625       650
## 4 45-59         63        52
## 5 5-14       26111     20245
## 6 60-69          5        11
## 7 70-79          1         1
## 8 80+            3         0
```

#### WE CAN COMBINE ALL THE FUNCTION USING %>% AND STILL GET THE SAME RESULT
```r
summary_HFMD <- HFMD  %>%
  mutate(age_group = case_when(
  `Umur.(Hari)` >= 0 & `Umur.(Hari)` <= 31 ~ "0-4",
  `Umur.(Bulan)` >= 0 & `Umur.(Bulan)` <= 12 ~ "0-4",
  `Umur.(Tahun)` >= 0 & `Umur.(Tahun)` <= 4 ~ "0-4",
  `Umur.(Tahun)` >= 5 & `Umur.(Tahun)` <= 14 ~ "5-14",
  `Umur.(Tahun)` >= 15 & `Umur.(Tahun)` <= 29 ~ "15-29",
  `Umur.(Tahun)` >= 30 & `Umur.(Tahun)` <= 44 ~ "30-44",
  `Umur.(Tahun)` >= 45 & `Umur.(Tahun)` <= 59 ~ "45-59",
  `Umur.(Tahun)` >= 60 & `Umur.(Tahun)` <= 69 ~ "60-69",
  `Umur.(Tahun)` >= 70 & `Umur.(Tahun)` <= 79 ~ "70-79",
  `Umur.(Tahun)` >= 80 ~ "80+")) %>%
  group_by(age_group, Jantina) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Jantina, values_from = count, values_fill = 0) %>%
  arrange(age_group)
```

```r
custom_order <- c("0-4", "5-14", "15-29", "30-44", "45-59", "60-69",  
                  "70-79", "80+")
summary_HFMD <- summary_HFMD[match(custom_order, summary_HFMD$age_group), ]
```
```
> summary_HFMD[match(custom_order, summary_HFMD$age_group), ]
##  A tibble: 8 × 3
##  age_group Lelaki Perempuan
##  <chr>      <int>     <int>
## 1 0-4        50919     39925
## 2 5-14       26111     20245
## 3 15-29        735       942
## 4 30-44        625       650
## 5 45-59         63        52
## 6 60-69          5        11
## 7 70-79          1         1
## 8 80+            3         0
```
