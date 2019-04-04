
# load libraries
library(lubridate) # dealing with dates and times
library(readxl) # open excel files
library(xts) # dealing with time series
library(tidyverse) # wrangling, data iteration and visualization
library(here) # create a file directory
library(kableExtra) # visualize html tables
library(scales) # create different scale values(percent, commas, etc...)
library(zoo) # deal with time-series data
library(ggthemes) # add themes to ggplot2


# load file
euro_vs_dollar <- read_excel(here::here("euro-dollar-exchange-rate-historical-chart.xlsx"), skip = 13) # skip the first 13 empty lines of the csv file


# check the first 20 rows
euro_vs_dollar %>%
head(20) %>% # only the first 20 rows
kable() 


# check the last 20 rows
euro_vs_dollar %>%
tail(20) %>% # only the first 20 rows
kable() 

# explore data
glimpse(euro_vs_dollar)

# create new data frame
euro_dollar_parse <- euro_vs_dollar %>%
  
  # change to character variable to work afterwards with lubridate functions
  mutate(date = as.character(date), 
         
         # change the format of some values in the date variable
         date = case_when(date == "1999-01-04" ~ "January 4, 1999",
                          date == "2018-12-31" ~ "31st December 2018",
                          date == "2017-07-12" ~ "2017, 12th July",
                          TRUE ~ date))

# explore data
glimpse(euro_dollar_parse)

# use parse_date_time
euro_vs_dollar_tbl <- euro_dollar_parse %>%
  # parse the four different formats of the date variable 
  mutate(date = parse_date_time(date, c("mdy", "ymd", "dmy", "ydm")))
 

# use ymd() function
euro_vs_dollar_tbl <- euro_vs_dollar_tbl %>%
  mutate(date = ymd(date))

# create year, month, day of the month, week day, day of the yea, and quarter
euro_vs_dollar_tbl <- euro_vs_dollar_tbl %>%
  mutate(# variable year
    year = year(date), 
    
    # variable month with the label
    month = month(date, label = TRUE), 
    
    # variable day of the month
    day = day(date), 
    
    # variable day of the week with abbreviated label
    week_day = wday(date, label = TRUE, abbr = TRUE), 
    
    # variable day of the year
    day_year = yday(date),
    
    # variable quarter
    qter = quarter(date)) 

# check table of the data frame - first 25 cases
euro_vs_dollar_tbl %>%
  head(25) %>%
  kable()

# example of make_date() function to create a date type variable
euro_vs_dollar_tbl %>%
  mutate(new_year_var = make_date(
    year = year, 
    month = month, 
    day = day)) %>%
  select(new_year_var, everything())


# use first and last functions of the xts package
euro_vs_dollar_tbl <- euro_vs_dollar_tbl %>%
  mutate(first_date = xts::first(date),


# use interval function to check the number of days that the EURO currency has
euro_vs_dollar_tbl <- euro_vs_dollar_tbl %>%
  # create euro_life variable
  mutate(euro_life = interval(first_date, last_date) / ddays(1))

euro_vs_dollar_tbl$euro_life[1]


# difference in the exchange rate of the EURO vs the DOLLAR from the beginning until today(2019-04-04)
euro_vs_dollar_tbl <- euro_vs_dollar_tbl %>%
  mutate(first_value = xts::first(value),
         last_value = xts::last(value),
         diff_value = last_value - first_value)

euro_vs_dollar_tbl$diff_value[1]

# the percent difference - use if scales::percent()
euro_vs_dollar_tbl <- euro_vs_dollar_tbl %>%
  mutate(
    # create difference in value
    dif = diff_value / first_value,
    # create difference in value in terms of percentage
    perc_dif = scales::percent(dif))

euro_vs_dollar_tbl$perc_dif[1]

# check floor_date, ceiling_date, and round_date functions
euro_vs_dollar_tbl %>%
  mutate(
    floor_d = floor_date(date, "months"),
    round_d = round_date(date, "months"),
    ceiling_d = ceiling_date(date, "months")
  ) %>% 
  select(
    date, floor_d, round_d, ceiling_d) %>%
  head(20) %>%
  kable()


# check lag function from dplyr
euro_vs_dollar_tbl %>%
  mutate(lag_value =lag(value, n =1)) %>%
  mutate(lag_value = case_when(is.na(lag_value) ~ value,
                               TRUE ~ lag_value),
         diff_lag = value - lag_value) %>%
  select(date, value, lag_value, diff_lag) %>%
  head(10) %>%
  kable()

# check rollmean to see how averages move through time
euro_vs_dollar_tbl %>%
  mutate(sum_five_d = zoo::rollmean(value, 
                                    k = 5, na.pad = TRUE, 
                                    align = "right")) %>%
  replace_na(., list(sum_five_d = 0)) %>% 
  select(date, value, sum_five_d) %>%
  head(10) %>% 
  kable()

# check the %m+% function
euro_vs_dollar_tbl %>%
  filter(date == ymd("2007-07-20") %m+% months(1)) %>%

 
# plot euro vs dollar 
euro_vs_dollar_tbl %>%
  group_by(date) %>%
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2008-09-15"), size = 2, lty = "dashed", 
             color = "red") +
  geom_curve(aes(x = as.Date("2006-01-01"), y = 1.51, 
                 xend = as.Date("2008-09-01"),
                 yend = 1.64), size = 1.5, color = "red",
             curvature = 0.3, angle = 165,
             arrow = arrow(length = unit(0.45,"cm"))) +
  annotate("text", x = as.Date("2004-06-01"), color = "red", y = 1.55, size = 4.55,label = "Lehman Brothers\nBankruptcy") +
  ggthemes::theme_economist() +
  labs(title = "Euro versus Dollar", y = "Exchange Value", x = 
         "Year") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_text(size = 14))
