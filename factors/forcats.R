# load libraries
library(tidyverse) # wrangling and data visualization - includes the forcats package
library(readxl) # open excel files
library(here) # sets a path to your file
library(kableExtra) # visualize html tables
library(scales) # creates different types of scales(e.g. commas, dollar sign, etc...)
library(viridis) # colour palette
library(ggdark) # themes for plots
library(hrbrthemes) # add fonts in this case
options(scipen = 999) # remove scientific notation


# open dataset
suicides <- read_excel(here::here("suicides.xlsx"))
glimpse(suicides)

# explore dataset
suicides %>% 
  head()

# lets work with the age variable. Let's turn age into a factor variable
suicides_tbl <- suicides %>% 
  mutate(age = as.factor(age))


# check levels of the variable age
levels(suicides_tbl$age)



# forcats functions
# 1: fct_relevel()
suicides_tbl <- suicides_tbl %>% 
  mutate(age = fct_relevel(age,
                           "5-14 years",
                           "15-24 years",
                           "25-34 years",
                           "35-54 years",
                           "55-74 years",
                           "75+ years"
  )) 

suicides_tbl %>% 
  
  pull(age) %>%
  
  levels()


# fct_relevel alternative
suicides_tbl %>% 
  mutate(age = fct_relevel(age,
                           "5-14 years", after = 0
  )) %>% 
  pull(age) %>% 

# make a plot without fct_reorder
suicides_tbl %>%
  filter(year== 2015,
         country == "United Kingdom") %>%
  group_by(age) %>% 
  summarise(suicides_total = sum(suicides_no)) %>% 
  mutate(prop = suicides_total / sum(suicides_total))%>% 
  ggplot(aes(age, suicides_total, fill = suicides_total,
             color = NULL)) +
  geom_col() + 
  scale_fill_viridis(option = "inferno", direction = -1) +
  labs(title = "Suicides in the UK", subtitle = "Year 2015", y = "Total Number of Suicides",
       x = "Age", fill = "Number of Suicides") +
  scale_y_continuous(labels = comma,
                     expand = c(0,0)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(family = "Agency FB", face = "bold",
                                  size = 25, hjust = .5),
        plot.subtitle = element_text(family = "Agency FB", face = "bold",
                                     size = 15),
        axis.title = element_text(family = "Agency FB", size = 15),
        axis.text = element_text(family = "Agency FB", size = 15),
        legend.text = element_text(family = "Agency FB", size = 15),
        legend.title = element_text(family = "Agency FB", size = 15))

# forcats
# 2: fct_reorder() 
suicides_tbl %>%
  filter(year== 2015,
         country == "United Kingdom") %>%
  group_by(age) %>% 
  summarise(suicides_total = sum(suicides_no)) %>% 
  mutate(prop = suicides_total / sum(suicides_total))%>% 
  ggplot(aes(fct_reorder(age, suicides_total), suicides_total, fill = suicides_total,
             color = NULL)) +
  geom_col() + 
  scale_fill_viridis(option = "inferno", direction = -1) +
  labs(title = "Suicides in the UK", subtitle = "Year 2015", y = "Total Number of Suicides",x = "Age", fill = "Number of Suicides") +
  scale_y_continuous(labels = comma,
                     expand = c(0,0)) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(family = "Agency FB", face = "bold",
                                  size = 25, hjust = .5),
        plot.subtitle = element_text(family = "Agency FB", face = "bold",
                                     size = 15),
        axis.title = element_text(family = "Agency FB", size = 15),
        axis.text = element_text(family = "Agency FB", size = 15),
        legend.text = element_text(family = "Agency FB", size = 15),
        legend.title = element_text(family = "Agency FB", size = 15))


# ### fct_explicit_na
# Let's work now with another variable of the dataset. We will create 5 levels of Human Development Index based on the variable `HDI for year`. We will turn this variable into a factor and relevel it again with `fct_relevel` and check if it has missing values.

suicides_tbl <- suicides_tbl %>% 
  mutate(hdi_cat = case_when(`HDI for year` >= 0.80 ~ "Very High Development",
                             `HDI for year` >= 0.70 ~ "High Development",
                             `HDI for year` >= 0.55 ~ "Medium Development",
                             `HDI for year` >= 0.35 ~ "Low Development",
                             `HDI for year` < 0.35 ~ "Very Low Development"
  ) %>% as.factor) %>%
    mutate(hdi_cat = fct_relevel(hdi_cat, 
                "High Development", after = 2)) 


# check missing values
any(is.na(suicides_tbl$hdi_cat))


#Given that we have missing values, we can use the function `fct_explicit_na` to give a name to the missing values.
# forcats
# 3: fct_explicit_na() 
suicides_tbl <- suicides_tbl %>% 
  mutate(hdi_cat = fct_explicit_na(hdi_cat, na_level = "Missing"))


# forcats
# 4: fct_lump to lump levels
# n argument
suicides_tbl %>%
  na.omit() %>%
  mutate(hdi_lumped = fct_lump(hdi_cat, n = 2, other_level = "Average or Below Category")) %>%
  count(hdi_lumped) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(n))

# prop argument
suicides_tbl %>%
  na.omit() %>%
  mutate(hdi_relevel = fct_lump(hdi_cat, prop =0.20, other_level = "Below Average")) %>%
  count(hdi_relevel) %>%
  mutate(prop = n / sum(n))


# forcats
# 5: fct_infreq
suicides_tbl %>%
  na.omit() %>%
  add_count(hdi_cat) %>%
  ggplot(aes(fct_infreq(hdi_cat))) +
  geom_bar(stat = "count") +
  labs(x = "HDI Level", y = "Count") +
  theme_minimal() 


# forcats
# 6: fct_rev
suicides_tbl %>%
  na.omit() %>%
  add_count(hdi_cat) %>%
  ggplot(aes(fct_rev(fct_infreq(hdi_cat)))) +
  geom_bar(stat = "count") +
  labs(x = "HDI Level", y = "Count") +
  theme_minimal() 


# forcats
# 7: fct_count() 
fct_count(suicides_tbl$hdi_cat)


# forcats
# 8: fct_unique()
fct_unique(suicides_tbl$hdi_cat) 


# forcats
# 9: fct_collapse()
suicides_tbl %>%
  mutate(generation = as.factor(generation)) %>% 
  mutate(generations = fct_collapse(generation,
    "Older Generations" = c("Silent", "G.I. Generation", "Boomers"),
    "Younger Generations" = c("Generation X", "Generation Z", "Millenials")
  )) %>%
  pull(generations) %>% 
  levels()

```


# forcats
# 10: fct_other()
suicides_tbl %>%
  mutate(silent_against_other = fct_other(generation, keep = "Silent")) %>% 
  pull(silent_against_other) %>% 
  levels()


# forcats
# 11: fct_recode()
suicides_tbl %>% 
  mutate(age_levels = fct_recode(age,
                                 "Child" = "5-14 years",
                                 "Adolescent/Young Adult"= "15-24 years",
                                 "Adult" = "25-34 years",
                                 "Middle-Aged Adult"= "35-54 years",
                                 "Older Adult" = "55-74 years",
                                 "Senior" = "75+ years")) %>% 
  pull(age_levels) %>% 
  levels()


# forcats
# 12: fct_reorder2()
suicides_tbl%>%
  filter(year > 1989, year < 2000, country == "United States") %>% 
  group_by(year, age) %>% 
  summarise(suicides_total = sum(suicides_no)) %>% 
  ggplot(aes(year, suicides_total, colour = fct_reorder2(age, year, suicides_total))) +
  geom_line(size = 2.5) + 
  scale_x_continuous(expand = c(0,0),
                     breaks = c(seq(1990, 1998, 2)),
                     labels = c(1990, 1992, 1994, 1996, 1998),
                     limits = c(1990, 1999))  +
  scale_colour_viridis_d(option = "inferno") +
  labs(title = "Suicides in the USA", subtitle = "90's decade",
       y = "Total Number of Suicides",
       x = "Year", colour = "Age") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Agency FB", face = "bold",
                                  size = 25, hjust = .5),
        plot.subtitle = element_text(family = "Agency FB", face = "bold",
                                  size = 15, hjust = 0.5),
        axis.title = element_text(family = "Agency FB", size = 15),
        axis.text = element_text(family = "Agency FB", size = 15),
        legend.text = element_text(family = "Agency FB", size = 15),
        legend.title = element_text(family = "Agency FB", size = 15))


# forcats
# 13: fct_relabel
suicides_tbl$age %>% fct_relabel(~ str_replace_all(.x, "years", " ")) %>% head()


# forcats
# 14: fct_anon
suicides_tbl %>%
  mutate(generation = as.factor(generation) %>% fct_anon()) %>%
  group_by(generation) %>% 
  count()

# fct_shift
# When you have a factor with levels in order that you don't appreciate or is not the correct one you can use the `fct_shift` function and add positive (shift to the left) or negative (shift to right) numbers to the `n` argument. We can check it in this example without  `fct_shift`:
suicides_tbl %>% 
  mutate(generation = as.factor(hdi_cat)) %>% 
  pull(generation) %>% 
  levels()

# forcats
# 15: fct_shift ()
suicides_tbl %>% 
  mutate(generation = as.factor(hdi_cat) %>% fct_shift(n = -1)) %>% 
  pull(generation) %>% 
  levels()

# forcats
# 16: fct_shuffle()
suicides_tbl %>% 
  mutate(generation = as.factor(generation) %>% fct_shuffle()) %>%
  pull(generation) %>%
  levels()





