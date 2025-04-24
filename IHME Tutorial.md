#### UPLOAD THE NECESSARY LIBRARIES
```
library(readxl)
library(dplyr)
library(openxlsx)
library(tidyr)
```

```
library(readxl)   : allow us to import from Excel spreadsheet into R workspace.

library(dplyr)    : allow us to manipulate and transform data frames efficiently.
                    function includes filter(), select()

library(openxlsx) : allows us to export data frames to Excel

library(tidyr)    : allow us to reshape data
                    function includes pivot_wider(), pivot_longer()
```

#### INSERT THE PREVALENCE/INCIDENCE FROM IHME
```
MEASURE  : PREVALENCE / INCIDENCE 
METRICS  : NUMBER
CAUSE    : RESPECTIVE DISEASES
LOCATION : MALAYSIA
AGE      : < 5 YEARS, 5-9 YEARS, 10-14 YEARS, 15-19 YEARS, 20-24 YEARS, 25-29 YEARS, 30-34 YEARS,
           35-39 YEARS, 40-44 YEARS, 45-49 YEARS, 50-54 YEARS, 55-59 YEARS, 60-64 YEARS, 65-69 YEARS,
           70-74 YEARS, 75-79 YEARS, 80+ YEARS
SEX      : FEMALE &  MALE
YEAR     : 2013 - 2021
```

### INSERT POPULATION FROM IHME
```
LOCATION : MALAYSIA
AGE      : < 5 YEARS, 5-9 YEARS, 10-14 YEARS, 15-19 YEARS, 20-24 YEARS, 25-29 YEARS, 30-34 YEARS,
           35-39 YEARS, 40-44 YEARS, 45-49 YEARS, 50-54 YEARS, 55-59 YEARS, 60-64 YEARS, 65-69 YEARS,
           70-74 YEARS, 75-79 YEARS, 80+ YEARS
SEX      : FEMALE &  MALE
YEAR     : 2013 - 2021
```

> [!NOTE] 
> We will generate the data for female first.

### ESTABLISH THE YEAR YOU WANT TO INCLUDE
```
## Remove the year you don't want to include
years <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
```

### INSERT THE NUMBER FROM IHME
```
## Change the directory to your own directory
number.female <- read.csv("\DIRECTORY REDACTED\", header = TRUE) %>%
  filter(sex_name == "Female", year %in% years) %>% 
  select(age_name, year, val) %>%
  pivot_wider(names_from = year, values_from = val) %>%
  add_row(age_name = c("5-14 years", "15-29 years", "30-44 years",
                       "45-59 years", "60-69 years", "70-79 years"))
```
```
> number.female
## A tibble: 28 × 10
##   age_name    `2013` `2014` `2015` `2016` `2017` `2018` `2019` `2020` `2021`
##   <chr>        <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
## 1 35-39 years     0      0      0      0      0      0      0      0      0 
## 2 40-44 years   181.   183.   185.   186.   188.   190.   194.   194.   203.
## 3 45-49 years  1075.  1083.  1090.  1096.  1102.  1110.  1121.  1112.  1139.
## 4 50-54 years  2548.  2591.  2626.  2655.  2681.  2704.  2721.  2705.  2716.
```

### ADD UP THE YEARS TO FORM 8 GROUPS
```
for (year in years){
  count1 <- number.female[number.female$age_name == "5-9 years", year]
  count2 <- number.female[number.female$age_name == "10-14 years", year]
  result <- count1+count2
  number.female[number.female$age_name == "5-14 years", year] <- result
  
  count1 <- number.female[number.female$age_name == "15-19 years", year]
  count2 <- number.female[number.female$age_name == "20-24 years", year]
  count3 <- number.female[number.female$age_name == "25-29 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  number.female[number.female$age_name == "15-29 years", year] <- result
  
  count1 <- number.female[number.female$age_name == "30-34 years", year]
  count2 <- number.female[number.female$age_name == "35-39 years", year]
  count3 <- number.female[number.female$age_name == "40-44 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  number.female[number.female$age_name == "30-44 years", year] <- result
  
  count1 <- number.female[number.female$age_name == "45-49 years", year]
  count2 <- number.female[number.female$age_name == "50-54 years", year]
  count3 <- number.female[number.female$age_name == "55-59 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  number.female[number.female$age_name == "45-59 years", year] <- result
  
  count1 <- number.female[number.female$age_name == "60-64 years", year]
  count2 <- number.female[number.female$age_name == "65-69 years", year]
  result <- count1+count2
  number.female[number.female$age_name == "60-69 years", year] <- result
  
  count1 <- number.female[number.female$age_name == "70-74 years", year]
  count2 <- number.female[number.female$age_name == "75-79 years", year]
  result <- count1+count2
  number.female[number.female$age_name == "70-79 years", year] <- result
  }
```
```
> number.female
## A tibble: 28 × 10
##   age_name     `2013` `2014` `2015` `2016` `2017` `2018` `2019` `2020` `2021`
##   <chr>         <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
## 1 80-84 years  11114. 11536. 11985. 13314. 14157. 14865. 15400. 15813. 15282.
## 2 2-4 years        0      0      0      0      0      0      0      0      0 
## 3 85+ years    10669. 11251. 11945. 12201. 12816. 13553. 14407. 15289. 16206.
## 4 12-23 months     0      0      0      0      0      0      0      0      0 
## 5 <5 years         0      0      0      0      0      0      0      0      0 
```

### MATCH AND CUSTOM ORDER OF THE YEAR
```
custom_order <- c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years",  
                  "70-79 years", "80+ years")
number.female <- number.female[match(custom_order, number.female$age_name), ]
```
```
> number.female
## A tibble: 8 × 10
##  age_name    `2013` `2014` `2015` `2016` `2017` `2018` `2019` `2020` `2021`
##  <chr>        <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
## 1 <5 years        0      0      0      0      0      0      0      0      0 
## 2 5-14 years      0      0      0      0      0      0      0      0      0 
## 3 15-29 years     0      0      0      0      0      0      0      0      0 
## 4 30-44 years   181.   183.   185.   186.   188.   190.   194.   194.   203.
## 5 45-59 years  8002.  8197.  8385.  8556.  8706.  8834.  8945.  8981.  8937.
## 6 60-69 years 15047. 15766. 16422. 16907. 17798. 18580. 19195. 19788. 19205.
## 7 70-79 years 21326. 22356. 23479. 24520. 24955. 25776. 27289. 29333. 30307.
## 8 80+ years   21783. 22787. 23930. 25516. 26973. 28418. 29806. 31102. 31488.
```

### UPLOAD POPULATION DATA
```
population.female <- read.csv("/DIRECTORY REDACTED/", header = TRUE)%>% 
  filter(sex_name == "Female", year %in% years) %>%
  select(age_name, year, val) %>%
  pivot_wider(names_from = year, values_from = val)  %>% 
  add_row(age_name = c("5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years", "70-79 years"))
```

### ADD UP THE YEARS TO FORM 8 GROUPS
```
for (year in years){
  count1 <- population.female[population.female$age_name == "5-9 years", year]
  count2 <- population.female[population.female$age_name == "10-14 years", year]
  result <- count1+count2
  population.female[population.female$age_name == "5-14 years", year] <- result
  
  count1 <- population.female[population.female$age_name == "15-19 years", year]
  count2 <- population.female[population.female$age_name == "20-24 years", year]
  count3 <- population.female[population.female$age_name == "25-29 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  population.female[population.female$age_name == "15-29 years", year] <- result
  
  count1 <- population.female[population.female$age_name == "30-34 years", year]
  count2 <- population.female[population.female$age_name == "35-39 years", year]
  count3 <- population.female[population.female$age_name == "40-44 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  population.female[population.female$age_name == "30-44 years", year] <- result
  
  count1 <- population.female[population.female$age_name == "45-49 years", year]
  count2 <- population.female[population.female$age_name == "50-54 years", year]
  count3 <- population.female[population.female$age_name == "55-59 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  population.female[population.female$age_name == "45-59 years", year] <- result
  
  count1 <- population.female[population.female$age_name == "60-64 years", year]
  count2 <- population.female[population.female$age_name == "65-69 years", year]
  result <- count1+count2
  population.female[population.female$age_name == "60-69 years", year] <- result
  
  count1 <- population.female[population.female$age_name == "70-74 years", year]
  count2 <- population.female[population.female$age_name == "75-79 years", year]
  result <- count1+count2
  population.female[population.female$age_name == "70-79 years", year] <- result
}
```
### CUSTOM ORDER THE YEAR
```
custom_order <- c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years",  
                  "70-79 years", "80+ years")
population.female <- population.female[match(custom_order, population.female$age_name), ]
```

### MEASURE THE PREVALENCE RATE FROM 2013 - 2019
```
rate.female <- data.frame(age_name = custom_order, "2013" = NA, "2014" = NA, "2015" = NA,
                          "2016" = NA, "2017" = NA, "2018" = NA, "2019" = NA, "2020" = NA, "2021" = NA)
```

```
> rate.female
##      age_name X2013 X2014 X2015 X2016 X2017 X2018 X2019 X2020 X2021
## 1    <5 years    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 2  5-14 years    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 3 15-29 years    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 4 30-44 years    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 5 45-59 years    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 6 60-69 years    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 7 70-79 years    NA    NA    NA    NA    NA    NA    NA    NA    NA
## 8   80+ years    NA    NA    NA    NA    NA    NA    NA    NA    NA
```
```
## Establish the correct column name for rate.female
colnames(rate.female) <- c("age_name", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")
```
```
> rate.female
##      age_name 2013 2014 2015 2016 2017 2018 2019 2020 2021
## 1    <5 years   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 2  5-14 years   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 3 15-29 years   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 4 30-44 years   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 5 45-59 years   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 6 60-69 years   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 7 70-79 years   NA   NA   NA   NA   NA   NA   NA   NA   NA
## 8   80+ years   NA   NA   NA   NA   NA   NA   NA   NA   NA
```
### ESTABLISH THE AGE GROUPS
```
age_groups <- c(number.female$age_name)
```
```
> age_groups
[1] "<5 years"    "5-14 years"  "15-29 years" "30-44 years" "45-59 years" "60-69 years" "70-79 years"
[8] "80+ years" 
```
### CALCULATE THE PREVALENCE RATE FOR FEMALE
```
for (age_group in age_groups){
  for(year in years){
    count1 <- number.female[number.female$age_name == age_group, year]
    count2 <- population.female[population.female$age_name == age_group, year]
    result <- (count1/count2)*100000
    rate.female[rate.female$age_name == age_group, year] <- result
  }
}
```
```
> rate.female
##      age_name         2013         2014         2015         2016         2017         2018           2019         2020         2021
## 1    <5 years     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000       0.000000     0.000000     0.000000
## 2  5-14 years     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000       0.000000     0.000000     0.000000
## 3 15-29 years     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000       0.000000     0.000000     0.000000
## 4 30-44 years     6.121613     6.065475     5.999001     5.918077     5.838169     5.776106       5.742918     5.637876     5.780343
## 5 45-59 years   391.019288   393.670466   396.302353   398.566020   400.088187   400.739847     400.771706   397.677669   391.540352
## 6 60-69 years  1903.038738  1907.481102  1911.909267  1909.778052  1914.232428  1914.387301    1911.314377  1908.616471  1799.437631
## 7 70-79 years  6061.590508  6056.639709  6005.719329  5876.874161  5819.750284  5782.305049    5770.588460  5796.074214  5605.579556
## 8   80+ years 17353.994304 17334.634083 17371.763505 17274.299496 17277.983849 17313.979393    17387.286447 17447.947080 17276.171843
```
### GENERATE REGRESSION FOR 2023 ESTIMATION
```
x <- pivot_longer(rate.female, cols = 2:10)
```
```
> x
## A tibble: 72 × 3
##    age_name   name  value
##    <chr>      <chr> <dbl>
##  1 <5 years   2013      0
##  2 <5 years   2014      0
##  3 <5 years   2015      0
##  4 <5 years   2016      0
##  5 <5 years   2017      0
##  6 <5 years   2018      0
##  7 <5 years   2019      0
##  8 <5 years   2020      0
##  9 <5 years   2021      0
## 10 5-14 years 2013      0
```
```
new_data <- data.frame(year = 2023)

female.new.df <- data.frame(age_name = age_groups)
```
```
> female.new.df
##      age_name
## 1    <5 years
## 2  5-14 years
## 3 15-29 years
## 4 30-44 years
## 5 45-59 years
## 6 60-69 years
## 7 70-79 years
## 8   80+ years
```

```
years <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

for (a in age_groups){
  for (i in years){
    #result <- data.alzheimer[data.alzheimer$age == a, i]
    temp <- x %>% filter(age_name == a) %>% select(name, value) %>% filter(name %in% years)
    colnames(temp) <- c("year", "val")
    model <- lm(val ~ (as.integer(year)), data = temp)
    predicted_value <- predict(model, new_data)
    female.new.df[female.new.df$age_name == a, 2:3] <- coef(model)
    female.new.df[female.new.df$age_name == a, "2023"] <- predicted_value
  }
}
```
```
> female.new.df
##      age_name          V2           V3         2023
## 1    <5 years      0.0000   0.00000000     0.000000
## 2  5-14 years      0.0000   0.00000000     0.000000
## 3 15-29 years      0.0000   0.00000000     0.000000
## 4 30-44 years    140.9702  -0.06698766     5.454138
## 5 45-59 years  -2999.7850   1.68506611   409.103729
## 6 60-69 years  -1038.9924   1.46294557  1920.546514
## 7 70-79 years 121660.9000 -57.41587532  5508.584230
## 8   80+ years  19865.8708  -1.25759314 17321.759859
```
```
colnames(female.new.df) <- c("age", "constant", "coefficient", "2023")
```
```
> female.new.df
##           age    constant  coefficient         2023
## 1    <5 years      0.0000   0.00000000     0.000000
## 2  5-14 years      0.0000   0.00000000     0.000000
## 3 15-29 years      0.0000   0.00000000     0.000000
## 4 30-44 years    140.9702  -0.06698766     5.454138
## 5 45-59 years  -2999.7850   1.68506611   409.103729
## 6 60-69 years  -1038.9924   1.46294557  1920.546514
## 7 70-79 years 121660.9000 -57.41587532  5508.584230
## 8   80+ years  19865.8708  -1.25759314 17321.759859
```

```
female.alzheimer.regression <- cbind(rate.female[, 1:10], "", female.new.df[-1])
```
```
> female.alzheimer.regression
##      age_name         2013         2014         2015         2016         2017         2018         2019         2020         2021 ""    constant   coefficient         2023
## 1    <5 years     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000         0.0000   0.00000000     0.000000
## 2  5-14 years     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000         0.0000   0.00000000     0.000000
## 3 15-29 years     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000     0.000000         0.0000   0.00000000     0.000000
## 4 30-44 years     6.121613     6.065475     5.999001     5.918077     5.838169     5.776106     5.742918     5.637876     5.780343       116.8782  -0.05503355     5.545307
## 5 45-59 years   391.019288   393.670466   396.302353   398.566020   400.088187   400.739847   400.771706   397.677669   391.540352      -451.0501   0.42030668   399.230272
## 6 60-69 years  1903.038738  1907.481102  1911.909267  1909.778052  1914.232428  1914.387301  1911.314377  1908.616471  1799.437631     15599.2419  -6.79298089  1857.041600
## 7 70-79 years  6061.590508  6056.639709  6005.719329  5876.874161  5819.750284  5782.305049  5770.588460  5796.074214  5605.579556    112447.9356 -52.84285236  5546.845249
## 8   80+ years 17353.994304 17334.634083 17371.763505 17274.299496 17277.983849 17313.979393 17387.286447 17447.947080 17276.171843     13996.9085   1.65624876 17347.499715
```

#### YOU CAN CHANGE THE FILE NAME TO ANYTHING
```
write.csv(female.alzheimer.regression, "8 groups female alzheimer 2013-2021 regression.csv")
```
>[!WARNING]
> Make sure to put ".csv" when you want to create a csv file.

> [!NOTE]
> SIMILARLY, RUN THE SAME THING FOR MALE
> 
> CODE FOR MALE IS SLIGHTLY DIFFERENT SO PLEASE REFER YOUR R SCRIPT

### ESTIMATE PREVALENCE FOR 2023 USING DISMOD
```
IN DISMOD
SELECT RATE: EVERY 100,000
```

### INSERT DATA FROM DISMOD
#### SELECT OUTPUT IN NUMBER
```
dataa <- read.csv(file = "C:/Users/DELL/Downloads/DISMOD_ALZHEIMER_8.csv", 
                  skip = 50, nrows = 9, header = TRUE)
```
>[!WARNING]
> ADJUST THE SKIP ACCORDING TO HOW MANY ROWS YOU WANT TO SKIP IN YOUR RESPECTIVE FILE 
```
> dataa
##     Age Prevalence Remission  Mortality  Incidence Prevalence.1 Remission.1 Case.fatality Duration Mortality.1 RR.mortality Age.of.onset  X
## 1        (numbers)   (rates)  (numbers)  (numbers)    (numbers)   (numbers)     (numbers)  (years)   (numbers)     (number)      (years) NA
## 2   0-4          0    0.0000          0          0            0           0             0   0.0000           0       0.0000       0.0000 NA
## 3  5-14          0    0.0000          0          0            0           0             0   0.0000           0       0.0000       0.0000 NA
## 4 15-29          0    0.0000          0          3            5           0             0  46.9466           0       1.0010      28.5000 NA
## 5 30-44        208    0.0000          0         48          281           0             0  36.7796           0       1.0015      40.0930 NA
## 6 45-59      10255    0.0000          0       1455         9516           0             0  26.3174           0       1.0005      53.8047 NA
## 7 60-69      22269    0.0000          0       2111        21670           0            35  19.8314          35       1.0774      64.5862 NA
## 8 70-79      32352    0.0000         59       4319        29834           0            66  14.8969          66       1.0624      76.6734 NA
## 9   80+      31110    0.0000         54        679        29580           0            50  12.4519          50       1.0233      83.1062 NA
```
### INSERT POPULATION FOR 2023
```
population_2023 <- read.xlsx("C:/Users/DELL/Downloads/POPULATION 2023 8.xlsx")
```
```
> population_2023
##   age_name    MALE  FEMALE
## 1       <5 1215700 1150800
## 2     5-14 2684000 2518600
## 3    15-29 4729600 4011800
## 4    30-44 4396500 3815200
## 5    45-59 2596700 2506800
## 6    60-69 1146300 1159500
## 7    70-79  538600  587300
## 8      80+  165000  179600
```
### GENERATE A DATAFRAME FOR PREVALENCE
```
age_levels <- dataa$Age[-1]
prevalence.df <- data.frame(age = age_levels, prevalence = dataa$Prevalence.1[-1])
```
```
> prevalence.df
##     age prevalence
## 1   0-4          0
## 2  5-14          0
## 3 15-29          5
## 4 30-44        281
## 5 45-59       9516
## 6 60-69      21670
## 7 70-79      29834
## 8   80+      29580
```

### INSERT YOUR DW
```
DW <- data.frame("age_name" = c(age_levels) , dw = c((rep(0.137, 6)), 0.161, 0.194))
```
```
> DW
##   age_name    dw
## 1      0-4 0.137
## 2     5-14 0.137
## 3    15-29 0.137
## 4    30-44 0.137
## 5    45-59 0.137
## 6    60-69 0.137
## 7    70-79 0.161
## 8      80+ 0.194
```

### GENERATE YLD DATAFRAME
```
YLD <- data.frame("age_name" = age_levels, population = c(population_2023$FEMALE), prevalence = prevalence.df[-1], dw = c((rep(0.137, 6)), 0.161, 0.194), YLD = NA)
```
```
> YLD
##   age_name population prevalence    dw YLD
## 1      0-4    1215700          0 0.137  NA
## 2     5-14    2684000          0 0.137  NA
## 3    15-29    4729600          5 0.137  NA
## 4    30-44    4396500        281 0.137  NA
## 5    45-59    2596700       9516 0.137  NA
## 6    60-69    1146300      21670 0.137  NA
## 7    70-79     538600      29834 0.161  NA
## 8      80+     165000      29580 0.194  NA
```
```
for(umur in age_levels){
  count1 <- as.numeric(prevalence.df[prevalence.df$age == umur, 2])
  count2 <- as.numeric(DW[DW$age_name == umur, 2])
  result <- count1 * count2
  YLD[YLD$age_name == umur, 5] <- result
}
```
```
> YLD
##   age_name population prevalence    dw      YLD
## 1      0-4    1215700          0 0.137    0.000
## 2     5-14    2684000          0 0.137    0.000
## 3    15-29    4729600          5 0.137    0.685
## 4    30-44    4396500        281 0.137   38.497
## 5    45-59    2596700       9516 0.137 1303.692
## 6    60-69    1146300      21670 0.137 2968.790
## 7    70-79     538600      29834 0.161 4803.274
## 8      80+     165000      29580 0.194 5738.520
```
```
write.csv(YLD, "YLD_8_female.CSV")
```
