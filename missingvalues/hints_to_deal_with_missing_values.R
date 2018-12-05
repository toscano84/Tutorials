# Hints to deal with missing values

# load libraries
library(Amelia)
library(tidyr)
library(readxl)
library(tidyimpute)
library(mice)
library(VIM)
library(dplyr)
library(here)


# open dataframe
nfl <- read_excel(here("nfl_salaries.xlsx"))
glimpse(nfl)


# check for missing values
any(is.na(nfl))

sum(is.na(nfl))

any(is.na(nfl)) 

which(is.na(nfl))

colSums(is.na(nfl))




# plot missing values
missmap(nfl)

plot <- aggr(nfl, col = c("green", "red"), 
             numbers = TRUE, 

#------------manage missing values---------------
# delete missing values
nfl_complete <- na.omit(nfl) 

# delete missing values of a particular column
nfl %>%
  drop_na("Special Teamer")

# fill missing values
nfl %>%
  fill(Quarterback, .direction = "down")

nfl %>%
  fill(Quarterback, .direction = "up")



# replace missing values with the mean
nfl %>% 
  replace_na(replace = list(Quarterback = mean(.$Quarterback, na.rm =TRUE),
                            `Special Teamer` = mean(.$`Special Teamer`, na.rm =TRUE)))

# with the median
nfl %>% 
  replace_na(replace = list(Quarterback = median(.$Quarterback, na.rm =TRUE),
                            `Special Teamer` = median(.$`Special Teamer`, na.rm =TRUE)))


# replace missing values with the function impute_mean
nfl %>%
  impute_mean(Quarterback, `Special Teamer`)

# with impute_median
nfl %>%
  impute_median(Quarterback, `Special Teamer`)


