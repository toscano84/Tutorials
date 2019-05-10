library(tidyverse) # wrangling and data visualization
library(kableExtra) # visualize html tables
library(rebus) # string maniipulation
library(data.table) # in this case open dataframe
library(splitstackshape) # split columns
library(lubridate) # dealing with dates and times

# open file
pet_names <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")


# explore it
glimpse(pet_names)


# cats dataframe
cats_names <- pet_names %>%
  filter(species == "Cat")


# dogs dataframe
dogs_names <- pet_names %>% 
  filter(species == "Dog")


# cats new dataframe
cats_names_tbl <- cats_names %>% 
  mutate(
    animals_name = 
      animals_name %>%
      str_remove_all(pattern = "[:punct:]") %>% # remove punctuation
      str_squish()) # remove all excess white space

# explore the data
cats_names_tbl %>%
  head(15) %>% 
  kable()

# dogs new dataframe
dogs_names_tbl <- dogs_names %>% 
  mutate(
    animals_name = 
      animals_name %>%
      str_remove_all(pattern = "[:punct:]") %>% # remove punctuation
      str_squish()) # remove all excess white space

dogs_names_tbl %>%
  head(15) %>% 
  kable()


# count the number of words 
# for cats
cats_names_tbl %>% 
  mutate(count_words = str_count(animals_name, pattern = "\\w+")) %>%
  arrange(desc(count_words)) %>%
  select(animals_name, count_words)

# for dogs
dogs_names_tbl %>% 
  mutate(count_words = str_count(animals_name, pattern = "\\w+")) %>%
  arrange(desc(count_words)) %>%
  select(animals_name, count_words)


# find the number of characters
# for cats
cats_names_tbl %>% 
  mutate(animals_name_rem = animals_name %>% str_remove_all(pattern = " "),
         number_char = animals_name_rem %>% str_length()) %>%
  select(animals_name, number_char) %>% 
  arrange(desc(number_char))

# for dogs
dogs_names_tbl %>% 
  mutate(animals_name_rem = animals_name %>% str_remove_all(pattern = " "),
         number_char = animals_name_rem %>% str_length()) %>%
  select(animals_name, number_char) %>% 
  arrange(desc(number_char))



# to upper case
# for cats
cats_names_tbl %>% 
  mutate(animals_name = animals_name %>% str_to_upper())

# for dogs
dogs_names_tbl %>% 
  mutate(animals_name = animals_name %>% str_to_upper())


# to lower case
# for cats
cats_names_tbl <- cats_names_tbl %>% 
  mutate(animals_name = animals_name %>% str_to_lower())

# for dogs
dogs_names_tbl <- dogs_names_tbl %>% 
  mutate(animals_name = animals_name %>% str_to_lower())


# separate columns with str_c
# for cats
cats_names_tbl %>% 
  separate(animals_name,
           into = str_c("animals_name", 1:7), # 7 because that's the maximum number of words
           sep = " ",
           remove = FALSE,
           extra = "drop")

# separate columns with str_c
# for dogs
dogs_names_tbl %>% 
  separate(animals_name,
           into = str_c("animals_name", 1:8), # 8 because that's the maximum number of words
           sep = " ",
           remove = FALSE,
           extra = "drop")

# separate columns with str_split
# for cats
splitting1 <- as.data.frame(str_split(cats_names_tbl$animals_name, fixed(" "), n = 7, simplify = TRUE)) # 7 because that's the maximum number of words

cats_after_split <- bind_cols(cats_names_tbl, splitting1)

# separate columns with str_split
# for dogs
splitting2 <- as.data.frame(str_split(dogs_names_tbl$animals_name, fixed(" "), n = 8, simplify = TRUE)) # 8 because that's the maximum number of words

cats_after_split <- bind_cols(dogs_names_tbl, splitting2)


# with the cSplit function
# for cats
cats_names_tbl <- cats_names_tbl %>%
  cSplit("animals_name", sep = " ", drop = FALSE) # separate animals_name column, but keep it

cats_names_tbl %>%
  head(15) %>% 
  kable()


# with the cSplit function
# for dogs
dogs_names_tbl <- dogs_names_tbl %>%
  cSplit("animals_name", sep = " ", drop = FALSE) # separate animals_name column, but keep it


dogs_names_tbl %>%
  head(15) %>% 
  kable()

# first letter
# for cats
cats_names_tbl %>% 
  mutate(first_letter = animals_name_1 %>% str_sub(1,1)) %>%
  count(first_letter) %>%
  arrange(desc(n)) 

# for dogs
dogs_names_tbl %>% 
  mutate(first_letter = animals_name_1 %>% str_sub(1,1)) %>%
  count(first_letter) %>%
  arrange(desc(n))

# last letter
# for cats
cats_names_tbl %>% 
  mutate(last_letter = animals_name_1 %>% str_sub(-1,-1)) %>%
  count(last_letter) %>%
  arrange(desc(n)) 

# for dogs
dogs_names_tbl %>% 
  mutate(last_letter = animals_name_1 %>% str_sub(-1,-1)) %>%
  count(last_letter) %>%
  arrange(desc(n))

# most common name
# for cats
cats_names_tbl %>%
  filter(!is.na(animals_name_1)) %>% 
  count(animals_name_1) %>%
  arrange(desc(n))

# for dogs
dogs_names_tbl %>% 
  filter(!is.na(animals_name_1)) %>% 
  count(animals_name_1) %>%
  arrange(desc(n))


# find unique first names with the pattern "su"
# for cats
unique(str_subset(cats_names_tbl$animals_name_1, pattern = "su"))

# for dogs
unique(str_subset(dogs_names_tbl$animals_name_1, pattern = "su"))


# find a pattern row location
# for cats
str_which(cats_names_tbl$animals_name_1, pattern = "car")

# for dogs
str_which(cats_names_tbl$animals_name_1, pattern = "car")
```



## Rebus
# In the previous section,   some important functions within the `stringr` ecosystem were shown.
# Now let's work again with `stringr`, this time in association with the `rebus` package. This package facilitates our work when dealing with regular expressions.  As aquick example,let's imagine that we wanted to count all dogs and cats names starting with a specific letter.  Wejust have to write `START %R%` and subsequently the required pattern:
# names that start with L
# for cats
cats_names_tbl %>% 
  mutate(L_char = str_count(animals_name_1, pattern = START %R% "l")) %>%
  summarize(cats_words_start_with_L = sum(L_char, na.rm = TRUE))

# dogs
dogs_names_tbl %>% 
  mutate(L_char = str_count(animals_name_1, pattern = START %R% "l")) %>%
  summarize(dogs_words_start_with_L = sum(L_char, na.rm = TRUE))


# find names that end with the pattern "cy"
# for cats
sort(
  table(
    str_subset(
      cats_names_tbl$animals_name_1, pattern = "cy" %R% END
    )
  )
  , decreasing = TRUE)

# for dogs
sort(
  table(
    str_subset(
      dogs_names_tbl$animals_name_1, pattern = "cy" %R% END
    )
  )
  , decreasing = TRUE)


# first names where the third letter is a "z"
# for cats
sort(
  table(
    str_subset(
      cats_names_tbl$animals_name_1, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% "z"
    )
  )
  , decreasing = TRUE)


# for dogs
sort(
  table(
    str_subset(
      dogs_names_tbl$animals_name_1, pattern = START %R% ANY_CHAR %R% ANY_CHAR %R% "z"
    )
  )
  , decreasing = TRUE)


# match the pattern "cc"
# for cats
sort(
  table(
  str_subset(
    cats_names_tbl$animals_name_1, pattern = "cc" %R% ANY_CHAR
    )
)
, decreasing = TRUE)


# match the pattern "cc"
# for dogs
sort(
  table(
  str_subset(
    dogs_names_tbl$animals_name_1, pattern = "cc" %R% ANY_CHAR
    )
)
, decreasing = TRUE)


# match multiple patterns with "or"
# for cats
sort(
  table(
  str_subset(
    cats_names_tbl$animals_name_1, pattern = START %R% or("lucy", "lucie", "luci") 
    %R% END)
)
, decreasing = TRUE)



# match multiple patterns with "or"
# for dogs
sort(
  table(
  str_subset(
    dogs_names_tbl$animals_name_1, pattern = START %R% or("lucy", "lucie", "luci") 
    %R% END)
)
, decreasing = TRUE)


# find first names that correspond only to vowels
# for cats
vow <- char_class("aeiou")
sort(
  table(
  str_subset(
    cats_names_tbl$animals_name_1, pattern = START %R% one_or_more(vow) %R% END)
)
, decreasing = TRUE)


# find first names that correspond only to vowels
# for dogs
vow <- char_class("aeiou")
sort(
  table(
  str_subset(
    dogs_names_tbl$animals_name_1, pattern = START %R% one_or_more(vow) %R% END)
)
, decreasing = TRUE)


# find names that do not have vowels
# for cats
not_vow <- negated_char_class("aeiou")

sort(
  table(
  str_subset(
    cats_names_tbl$animals_name_1, pattern = START %R% one_or_more(not_vow) %R% END)
)
, decreasing = TRUE)


# find names that do not have vowels
# for dogs
not_vow <- negated_char_class("aeiou")

sort(
  table(
  str_subset(
    dogs_names_tbl$animals_name_1, pattern = START %R% one_or_more(not_vow) %R% END)
)
, decreasing = TRUE)


# find names only with digits
# for cats
sort(
  table(
  str_subset(
    cats_names_tbl$animals_name_1, pattern = START %R% one_or_more(DIGIT) %R% END)
)
, decreasing = TRUE)


# find names only with digits
# for cats
sort(
  table(
  str_subset(
    dogs_names_tbl$animals_name_1, pattern = START %R% one_or_more(DIGIT) %R% END)
)
, decreasing = TRUE)


# capture first name that have words and digits
# for cats
sort(
  table(
  str_subset(
    cats_names_tbl$animals_name_1, pattern = capture(WRD) %R% one_or_more(DIGIT) %R% END)
)
, decreasing = TRUE)


# capture first name that have words and digits
# for cats
sort(
  table(
    str_subset(
      dogs_names_tbl$animals_name_1, pattern = capture(WRD) %R% one_or_more(DIGIT) %R% END)
  )
  , decreasing = TRUE)


# find first names with three repeated letters
# for cats
sort(
  table(
    str_subset(
      cats_names_tbl$animals_name_1, pattern = capture(LOWER) %R% REF1 %R% REF1)
  )
  , decreasing = TRUE)


# find first names with three repeated letters
# for cats
sort(
  table(
    str_subset(
      dogs_names_tbl$animals_name_1, pattern = capture(LOWER) %R% REF1 %R% REF1)
  )
  , decreasing = TRUE)


# detect a pattern with exactly 
# for dogs
sort(
  table(
    str_subset(
      dogs_names_tbl$animals_name_1, pattern = exactly("lucy"))
  )
  , decreasing = TRUE)

  
  
  
  