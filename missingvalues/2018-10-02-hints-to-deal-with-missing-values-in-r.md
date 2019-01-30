---
title: "Hints to deal with Missing Values in R"
author: Hugo Toscano
date: '2018-10-02'
slug: hints-to-deal-with-missing-values-in-r
categories: [Tutorials]
tags: [missing values]
---
In R missing values are usually, but not always, represented by letters `NA`. How to deal with missing values is very important in the data analytics world.  Missing data can be sometimes  tricky while  analyzing a data frame, since it should be handled correctly for our statistical analysis. Before diving into  more complex details about missing data, the first question that should be asked  in any exploratory data analysis is: **Do I have missing values in my database?**

First, we need to load the libraries needed for our analysis.

```{r message=FALSE, warning=FALSE}
library(Amelia)
library(tidyr)
library(readxl)
library(tidyimpute)
library(mice)
library(VIM)
library(dplyr)
library(here)
```

Next, we will load the data frame related to the NFL Positional Salaries from 2011 to 2018.

```{r}
nfl <- read_excel(here("nfl_salaries.xlsx"))
glimpse(nfl)
```

Now that the data is loaded, let's use some nice functions from Base R to answer the question mentioned above. The first function we could use is `is.na()`. This function tells us for each observation if  the existence of missing values is TRUE or FALSE. However, when we have a multitude of cases it is not very clear if our question was answered. Instead, we should use `any(is.na(name of dataframe))` or `sum(is.na(name of the dataframe))`.

```{r}
any(is.na(nfl))

sum(is.na(nfl))
```

any(is.na(nfl)) says TRUE, meaning we have missing data. Besides, sum(is.na(nfl)) tells us that we have in total 56 missing values.

Even though our question was answered, we still do not know where the missing values are present. More specifically, **in which variables do we have missing values?** That's our second question.

For that, there are some functions available. We can use the `which(is.na())` function.

```{r}
which(is.na(nfl))
```

However, this gives only the location in terms of number of case and nothing about the variable where the missing data is present. We can use for this the `colSums()` function.

```{r}
colSums(is.na(nfl))
```

We can now see that we have 55 missing values in the Quarterback position and 1 missing value in the Special Teamer position. Nonetheless, we can visually check where in our variables we have missing data. The `missmap()` function from the `Amelia` package is fit for this purpose.

```{r message=FALSE, warning=FALSE}
missmap(nfl)
```

Visually we can check that we have only missing values in the Quarterback and Special Teamer columns. Another alternative is to check where exactly are the missing values and their correspondent proportion in total. Now, we should use `aggr()` function from the `VIM` package.

```{r paged.print=TRUE}
plot <- aggr(nfl, col = c("green", "red"), 
             numbers = TRUE, 
             sortVars = TRUE)
```

The values in the table show that 6.875% of the cases in the Quarterback variable correspond to missing values, while only 0.125% correspond to missing values in the Special Teamer variable. The plot does not show all the variable names because of some issue that I could not solve. Nonetheless, from the table it is clear the proportion of missing values in each variable.


Now the last step anwers the  third question: **What should I do with the missing values?**
The simplest and more direct way to deal with missing data is to omit it from our data frame. In this case, we can use the `na.omit()` function.

```{r}
nfl_complete <- na.omit(nfl) 
any(is.na(nfl_complete))
```

The downside being the deletion of too many observations of our data which could have been useful to our analysis. In this case, we could delete only missing values of a specific variable. To do that, go ahead and use the `drop_na()` function form the `tidyr()` package.

```{r}
nfl %>%
  drop_na("Special Teamer")

```

In the example, we have deleted all missing values corresponding to the Special Teamer column. 

Two other neat functions from the `tidyr()` package can also be of great help. The `fill()` and `replace_na()` can impute values where before were missing values. The `fill()` function allows to replace the missing values with the most recent non-missing value present before or after the missing value.  There is an argument in this function called .direction which enables you to choose if it is the nearest value being displayed before or after the missing value.

If we write the code as follows:

```{r}
nfl %>%
  fill(Quarterback, .direction = "down")
```

With .direction = "down", it will show (?) the value  appearing before the missing value which will fill the NA values, but if we write with .direction = "up" the reserve will occur. Meaning, the value being displayed after the NA value will fill the space where the missing value once was.

```{r}
nfl %>%
  fill(Quarterback, .direction = "up")
```


Rather, if we want to replace the missing value with the mean or median of the respective variable we could use `replace_na`.

```{r}
# with the mean
nfl %>% 
  replace_na(replace = list(Quarterback = mean(.$Quarterback, na.rm =TRUE),
             `Special Teamer` = mean(.$`Special Teamer`, na.rm =TRUE)))

# with the median
nfl %>% 
  replace_na(replace = list(Quarterback = median(.$Quarterback, na.rm =TRUE),
             `Special Teamer` = median(.$`Special Teamer`, na.rm =TRUE)))
```

A more elegant approach - I believe - to substitute missing values with the mean or median is to use the functions available in the package `tidyimpute`. As the code below shows, it`s very simple to impute the mean or median to our missing values.

```{r}
# with mean
nfl %>%
  impute_mean(Quarterback, `Special Teamer`)

# with the median
nfl %>%
  impute_median(Quarterback, `Special Teamer`)
```


And that's it for today's post! I hope this mini tutorial helps you in dealing with missing values in R. Thanks for reading us and keep coding in R.
