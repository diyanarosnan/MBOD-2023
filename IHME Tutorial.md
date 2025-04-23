library(readxl)
library(dplyr)
library(openxlsx)
library(tidyr)

# INSERT THE IHME
# MEASURE  : PREVALENCE / INCIDENCE 
# METRICS  : NUMBER
# CAUSE    : RESPECTIVE DISEASES
# LOCATION : MALAYSIA
# AGE      : < 1 YEAR, 12 - 23 MONTHS, 2-4 YEARS, 5-9 YEARS, 10-14 YEARS, 15-19 YEARS, 
#            20-24 YEARS, 25-29 YEARS, 30-34 YEARS, 35-39 YEARS, 40-44 YEARS, 45-49 YEARS,
#            50-54 YEARS, 55-59 YEARS, 60-64 YEARS, 65-69 YEARS, 70-74 YEARS, 75-79 YEARS,
#            80-84 YEARS, 85+ YEARS
# SEX      : FEMALE &  MALE
# YEAR     : 2013 - 2021

# INSERT POPULATION FROM IHME - 2013 UNTIL 2021
# LOCATION : MALAYSIA
# AGE      : < 1 YEAR, 12 - 23 MONTHS, 2-4 YEARS, 5-9 YEARS, 10-14 YEARS, 15-19 YEARS, 
#            20-24 YEARS, 25-29 YEARS, 30-34 YEARS, 35-39 YEARS, 40-44 YEARS, 45-49 YEARS,
#            50-54 YEARS, 55-59 YEARS, 60-64 YEARS, 65-69 YEARS, 70-74 YEARS, 75-79 YEARS,
#            80-84 YEARS, 80+ YEARS
# SEX      : FEMALE &  MALE
# YEAR     : 2013 - 2021

# FILTER BY SEX

# ESTABLISH THE YEAR YOU WANT TO INCLUDE
years <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

# INSERT THE NUMBER FROM IHME
number.female <- read.csv("C:/Users/DELL/Downloads/IHME-GBD_2021_ALZHEIMER_FM_DATA_NUMBER/IHME-GBD_2021_DATA-9f622dd7-1.csv", header = TRUE) %>% 
  filter(sex_name == "Female", year %in% years) %>%
  select(age_name, year, val) %>%
  pivot_wider(names_from = year, values_from = val) %>%
  add_row(age_name = c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years", "70-79 years", "80+ years"))

# ADD UP TO FORM 8 GROUPS
for (year in years){
  count1 <- number.female[number.female$age_name == "<1 year", year]
  count2 <- number.female[number.female$age_name == "2-4 years", year]
  count3 <- number.female[number.female$age_name == "13-23 months", year]
  result <- sum(unlist(c(count1, count2, count3)))
  number.female[number.female$age_name == "<5 years", year] <- result
  
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

  count1 <- number.female[number.female$age_name == "80-84 years", year]
  count2 <- number.female[number.female$age_name == "85+ years", year]
  result <- count1+count2
  number.female[number.female$age_name == "80+ years", year] <- result
  }

# CUSTOM THE YEAR ORDER
custom_order <- c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years",  
                  "70-79 years", "80+ years")

number.female <- number.female[match(custom_order, number.female$age_name), ]

# DO THE SAME FOR POPULATION
population.female <- read.csv("C:/Users/DELL/Downloads/IHME-GBD_2021_POP_DATA_MF_80+/IHME-GBD_2021_DATA-4cfd2114-1.csv", header = TRUE)%>% 
  filter(sex_name == "Female", year %in% years) %>%
  select(age_name, year, val) %>%
  pivot_wider(names_from = year, values_from = val)  %>% 
  add_row(age_name = c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years", "70-79 years"))

for (year in years){
  count1 <- population.female[population.female$age_name == "<1 year", year]
  count2 <- population.female[population.female$age_name == "2-4 years", year]
  count3 <- population.female[population.female$age_name == "13-23 months", year]
  result <- sum(unlist(c(count1, count2, count3)))
  population.female[population.female$age_name == "<5 years", year] <- result
  
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

custom_order <- c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years",  
                  "70-79 years", "80+ years")

population.female <- population.female[match(custom_order, population.female$age_name), ]

# MEASURE THE RATE FROM 2013 - 2019
rate.female <- data.frame(age_name = custom_order, "2013" = NA, "2014" = NA, 
                          "2015" = NA, "2016" = NA, "2017" = NA, "2018" = NA,
                          "2019" = NA)

colnames(rate.female) <- c("age_name", "2013", "2014", "2015", "2016", "2017",
                           "2018", "2019")

age_groups <- c(number.female$age_name)

for (age_group in age_groups){
  for(year in years){
    count1 <- number.female[number.female$age_name == age_group, year]
    count2 <- population.female[population.female$age_name == age_group, year]
    result <- (count1/count2)*100000
    rate.female[rate.female$age_name == age_group, year] <- result
  }
}

# GENERATE REGRESSION FOR 2023 ESTIMATION
x <- pivot_longer(rate.female, cols = 2:8)
new_data <- data.frame(year = 2023)

female.new.df <- data.frame(age_name = age_groups)

years <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019")

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

colnames(female.new.df) <- c("age", "constant", "coefficient", "2023")

female.alzheimer.regression <- cbind(rate.female[, 1:8], "", female.new.df[-1])
write.csv(female.alzheimer.regression, "2204 8 groups female alzheimer 2013-2019 regression.csv")

############# MALE

number.male <- read.csv("C:/Users/DELL/Downloads/IHME-GBD_2021_ALZHEIMER_FM_DATA_NUMBER/IHME-GBD_2021_DATA-9f622dd7-1.csv", header = TRUE) %>% 
  filter(sex_name == "Male") %>%
  select(age_name, year, val) %>%
  pivot_wider(names_from = year, values_from = val) %>%
  add_row(age_name = c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years", "70-79 years", "80+ years"))

years <- c(colnames(number.male[-1]))

for (year in years){
  count1 <- number.male[number.male$age_name == "<1 year", year]
  count2 <- number.male[number.male$age_name == "2-4 years", year]
  count3 <- number.male[number.male$age_name == "13-23 months", year]
  result <- sum(unlist(c(count1, count2, count3)))
  number.male[number.male$age_name == "<5 years", year] <- result
  
  count1 <- number.male[number.male$age_name == "5-9 years", year]
  count2 <- number.male[number.male$age_name == "10-14 years", year]
  result <- count1+count2
  number.male[number.male$age_name == "5-14 years", year] <- result
  
  count1 <- number.male[number.male$age_name == "15-19 years", year]
  count2 <- number.male[number.male$age_name == "20-24 years", year]
  count3 <- number.male[number.male$age_name == "25-29 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  number.male[number.male$age_name == "15-29 years", year] <- result
  
  count1 <- number.male[number.male$age_name == "30-34 years", year]
  count2 <- number.male[number.male$age_name == "35-39 years", year]
  count3 <- number.male[number.male$age_name == "40-44 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  number.male[number.male$age_name == "30-44 years", year] <- result
  
  count1 <- number.male[number.male$age_name == "45-49 years", year]
  count2 <- number.male[number.male$age_name == "50-54 years", year]
  count3 <- number.male[number.male$age_name == "55-59 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  number.male[number.male$age_name == "45-59 years", year] <- result
  
  count1 <- number.male[number.male$age_name == "60-64 years", year]
  count2 <- number.male[number.male$age_name == "65-69 years", year]
  result <- count1+count2
  number.male[number.male$age_name == "60-69 years", year] <- result
  
  count1 <- number.male[number.male$age_name == "70-74 years", year]
  count2 <- number.male[number.male$age_name == "75-79 years", year]
  result <- count1+count2
  number.male[number.male$age_name == "70-79 years", year] <- result
  
  count1 <- number.male[number.male$age_name == "80-84 years", year]
  count2 <- number.male[number.male$age_name == "85+ years", year]
  result <- count1+count2
  number.male[number.male$age_name == "80+ years", year] <- result
}

custom_order <- c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years",  
                  "70-79 years", "80+ years")

number.male <- number.male[match(custom_order, number.male$age_name), ]

# DO THE SAME FOR POPULATION
population.male <- read.csv("C:/Users/DELL/Downloads/IHME-GBD_2021_POP_DATA_MF_80+/IHME-GBD_2021_DATA-4cfd2114-1.csv", header = TRUE)%>% 
  filter(sex_name == "Female") %>%
  select(age_name, year, val) %>%
  pivot_wider(names_from = year, values_from = val)  %>% 
  add_row(age_name = c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years", "70-79 years"))

for (year in years){
  count1 <- population.male[population.male$age_name == "<1 year", year]
  count2 <- population.male[population.male$age_name == "2-4 years", year]
  count3 <- population.male[population.male$age_name == "13-23 months", year]
  result <- sum(unlist(c(count1, count2, count3)))
  population.male[population.male$age_name == "<5 years", year] <- result
  
  count1 <- population.male[population.male$age_name == "5-9 years", year]
  count2 <- population.male[population.male$age_name == "10-14 years", year]
  result <- count1+count2
  population.male[population.male$age_name == "5-14 years", year] <- result
  
  count1 <- population.male[population.male$age_name == "15-19 years", year]
  count2 <- population.male[population.male$age_name == "20-24 years", year]
  count3 <- population.male[population.male$age_name == "25-29 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  population.male[population.male$age_name == "15-29 years", year] <- result
  
  count1 <- population.male[population.male$age_name == "30-34 years", year]
  count2 <- population.male[population.male$age_name == "35-39 years", year]
  count3 <- population.male[population.male$age_name == "40-44 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  population.male[population.male$age_name == "30-44 years", year] <- result
  
  count1 <- population.male[population.male$age_name == "45-49 years", year]
  count2 <- population.male[population.male$age_name == "50-54 years", year]
  count3 <- population.male[population.male$age_name == "55-59 years", year]
  result <- sum(unlist(c(count1, count2, count3)))
  population.male[population.male$age_name == "45-59 years", year] <- result
  
  count1 <- population.male[population.male$age_name == "60-64 years", year]
  count2 <- population.male[population.male$age_name == "65-69 years", year]
  result <- count1+count2
  population.male[population.male$age_name == "60-69 years", year] <- result
  
  count1 <- population.male[population.male$age_name == "70-74 years", year]
  count2 <- population.male[population.male$age_name == "75-79 years", year]
  result <- count1+count2
  population.male[population.male$age_name == "70-79 years", year] <- result
}

custom_order <- c("<5 years", "5-14 years", "15-29 years", "30-44 years", "45-59 years", "60-69 years",  
                  "70-79 years", "80+ years")

population.male <- population.male[match(custom_order, population.male$age_name), ]

# CALCULATE RATE

rate.male <- data.frame(age_name = custom_order, "2013" = NA, "2014" = NA, 
                          "2015" = NA, "2016" = NA, "2017" = NA, "2018" = NA,
                          "2019" = NA, "2020" = NA, "2021" = NA)

colnames(rate.male) <- c("age_name", "2013", "2014", "2015", "2016", "2017",
                           "2018", "2019", "2020", "2021")

age_groups <- c(number.male$age_name)

for (age_group in age_groups){
  for(year in years){
    count1 <- number.male[number.male$age_name == age_group, year]
    count2 <- population.male[population.male$age_name == age_group, year]
    result <- (count1/count2)*100000
    rate.male[rate.male$age_name == age_group, year] <- result
  }
}

# REGRESSION FOR 2023 ESTIMATION
x <- pivot_longer(rate.male, cols = 2:8)
new_data <- data.frame(year = 2023)

male.new.df <- data.frame(age_name = age_groups)

years <- c("2013", "2014", "2015", "2016", "2017", "2018", "2019")

for (a in age_groups){
  for (i in years){
    #result <- data.alzheimer[data.alzheimer$age == a, i]
    temp <- x %>% filter(age_name == a) %>% select(name, value) %>% filter(name %in% years)
    colnames(temp) <- c("year", "val")
    model <- lm(val ~ (as.integer(year)), data = temp)
    predicted_value <- predict(model, new_data)
    male.new.df[male.new.df$age_name == a, 2:3] <- coef(model)
    male.new.df[male.new.df$age_name == a, "2023"] <- predicted_value
  }
}

colnames(male.new.df) <- c("age", "constant", "coefficient", "2023")

male.alzheimer.regression <- cbind(rate.male[, 1:8], "", male.new.df[-1])
write.csv(male.alzheimer.regression, "2204 8 groups male alzheimer 2013-2019 regression.csv")

# INSERT DATA FROM DISMOD
# SELECT NUMBER

dataa <- read.csv(file = "C:/Users/DELL/Downloads/alzheimer_dismod_8_age_groups.csv", 
                  skip = 5, nrows = 9, header = TRUE)

dataa <- read.csv(file = "C:/Users/DELL/Downloads/DISMOD_ALZHEIMER_8.csv", 
                  skip = 20, nrows = 9, header = TRUE)

# INSERT POPULATION FOR 2023
population_2023 <- read.xlsx("C:/Users/DELL/Downloads/POPULATION 2023 8.xlsx")

# PREVALENCE
age_levels <- dataa$Age[-1]

prevalence.df <- data.frame(age = age_levels, prevalence = dataa$Prevalence.1[-1])

# INSERT YOUR DW
DW <- data.frame("age_name" = c(age_levels) , dw = c((rep(0.137, 6)), 0.161, 0.194))

YLD <- data.frame("age_name" = age_levels, population = c(population_2023$MALE), prevalence = prevalence.df[-1], dw = c((rep(0.137, 6)), 0.161, 0.194), YLD = NA)

# CALCULATE YLD
for(umur in age_levels){
  count1 <- as.numeric(prevalence.df[prevalence.df$age == umur, 2])
  count2 <- as.numeric(DW[DW$age_name == umur, 2])
  result <- count1 * count2
  YLD[YLD$age_name == umur, 5] <- result
}

YLD_8_male <- YLD
write.csv(YLD_8_male, "2204 YLD_8_male.CSV")


### FEMALE

dataa <- read.csv(file = "C:/Users/DELL/Downloads/DISMOD_ALZHEIMER_8.csv", 
                  skip = 50, nrows = 9, header = TRUE)

# PREVALENCE
age_levels <- dataa$Age[-1]

prevalence.df <- data.frame(age = age_levels, prevalence = dataa$Prevalence.1[-1])

# INSERT YOUR DW
DW <- data.frame("age_name" = c(age_levels) , dw = c((rep(0.137, 6)), 0.161, 0.194))

YLD <- data.frame("age_name" = age_levels, population = c(population_2023$FEMALE), prevalence = prevalence.df[-1], dw = c((rep(0.137, 6)), 0.161, 0.194), YLD = NA)

for(umur in age_levels){
  count1 <- as.numeric(prevalence.df[prevalence.df$age == umur, 2])
  count2 <- as.numeric(DW[DW$age_name == umur, 2])
  result <- count1 * count2
  YLD[YLD$age_name == umur, 5] <- result
}

YLD_8_female <- YLD
write.csv(YLD_8_female, "2204 YLD_8_female.CSV")
