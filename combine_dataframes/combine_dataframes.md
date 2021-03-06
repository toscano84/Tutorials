---
title: "Combine Data frames in R"
author: Hugo Toscano
date: '2018-09-13'
slug: combine-data-frames-in-r
categories: [Tutorials]
tags: [dplyr, joins, combine]
---

Sometimes, before we start to explore our data, we need to put them together. For instance, we might have them stored in different data frames and we have to join variables from two or more data frames in one. This post will talk about the different functions  we can use to achieve that goal. We will be using the `dplyr` package to combine different data frames.

Firstly, we will show examples related to what is called **mutating joins**. These joins combine two data frames by matching observations in common variables. 

## Mutating Joins
For this tutorial I have created small datasets just for the sole purpose of writing this blog. Therefore, the data was created by myself and if there is any relationship between the variables, it is not real.

Here you can see a Venn diagram of the different types of mutating joins (source: http://r4ds.had.co.nz/relational-data.html):

![](/post/2018-10-23-predicting-airfares-prices-on-new-routes-a-supervised-learning-approach-with-multiple-linear-regression_files/joins.PNG){width=80%}

First, we will load the packages and data frames needed for this tutorial.

```{r message=FALSE, warning=FALSE}
library(here)
library(tibble)
library(tidyverse) # load packages related to data cleaning(in this tutorial dplyr for joins and set operators,  and purrr to use the reduce() function)
library(readxl) # load excel files
# dataframes
df1 <- read_excel(here("df1.xlsx"))
df2 <- read_excel(here("df2.xlsx"))
df3 <- read_excel(here("df3.xlsx"))
df4 <- read_excel(here("df4.xlsx"))


```


Let us start with the data frames df1 and df2.

df1:
```{r}
df1 
```

df2:
```{r}
df2
```

If our goal is to maintain all elements of df1 and the elements of df2 in common with df1, we should use a `left_join()`. We can see that we have 3 variables in common. 
Our code for this is the following:


```{r}
df_left <- df1 %>% 
  left_join(df2, by = c("Id", "Face", "Sex"))# use of by argument where we write the common variables in both datasets

df_left
```

As you can see the output maintains all the columns, but we have 2 missing values in the Nationality and SES columns because the first and third elements of df2 on the Face and Sex columns were different from df1. As said before, a `left_join()` maintains all elemens of df1 and the common elements between df2 and df1.

Sometimes, we might just want to do the reverse, maintain all elements of df2 and elements of df1 in common with df2. For that you should use a `right_join()` as the one below:

```{r}
df_right <- df2 %>% 
  right_join(df1, by = c("Id", "Face", "Sex"))

df_right
```


In other situations we might want to maintain only the elements in common between df1 and df2. For that you use an `inner_join()`:


```{r}
df_inner <- df1 %>% 
  inner_join(df2, by = c("Id", "Face", "Sex"))

df_inner
```

As you can see, elements in the first and third columns disappear from our database as they are not common between df1 and df2.
In those cases where we want to maintain all observations from df1 and df2, we should go with a `full_join()`:


```{r}
df_full <- df1 %>% 
  full_join(df2, by = c("Id", "Face", "Sex"))

df_full
```

We have two additional rows. Why? Because with a full join the non-common elements in rows 1 and 3 are added to the joined database.

### Very Important **NOTES** to consider in two situations:

*1st situation*
You may have common variables in two different datasets with different names as in df1 and df4

df1:
```{r}
df1
```

And df4:
```{r}
df4 
```

The variables Face and face_stimuli are the same, but have different names. In that case you should do the following (in this example, we are using an `inner_join()`, but we could use another type of join):

```{r}
df_sit1 <- df1 %>% 
  inner_join(df4, by = c("Id", "Sex", 
                         "Face" = "face_stimuli"))# you make explicit that face equals item

df_sit1
```

*2nd situation*
We have variables  with the same name, but are not exactly the same. In these two data frames I have created, df5 and df6, we have the counting of white blood cells before and after treatment, respectively.

df5:
```{r}
df5 <- tibble(Id = 1:10,
             WhiteBloodCell = c(900, 910, 1000, 250, 300, 600, 300, 800, 200, 100),
             SES = c("Middle Class", "Middle Class", "Middle Class", "Upper Middle Class",
                     "Middle Class", "Middle Class", "Middle Class", "Upper Middle Class",
                     "Lower Middle Class", "Lower Middle Class"))

df5 
```


And df6:
```{r}
df6 <- tibble(Id = 1:10,
             WhiteBloodCell = c(1000, 980, 1200, 500, 
                                500, 700, 300, 1000, 400, 300)) 
df6 
```

In these cases, we should follow the function described below: 


```{r}
df_sit2 <- df5 %>% 
  left_join(df6, by = c("Id"), suffix = c("Before Treatment", "After Treatment"))#we have in common the variables Id and whitebloodcell, but the latter corresponds to two different elements, before and after treatment.

df_sit2
```

## Filtering Joins

Let's go now to another type of joins. The **FILTERING** joins. They have this name because they filter observations in two different datasets.
There are two types of filtering joins: `semi_join()` and `anti_join()`.

The `semi_join()` maintains all observations in a data frame (x) which are not matched in another data frame (y):

```{r}
df_semi <- df1 %>% 
  semi_join(df2, by = c("Id", "Face", "Sex"))

df_semi
```

Rows 1 and 3 disappear and the variables Nationality and SES from df2 are no longer showing in this new dataset. Therefore, this join maintains all variables of j1 and the ones in common with df2. In regard to the observations/cases, it maintains only the matching ones.

The `anti_join()` maintains only the non-common observations between data frame (x) and data frame (y):

```{r}
df_anti <- df1 %>% 
  anti_join(df2, by = c("Id", "Face", "Sex"))

df_anti
```

In this case, only  rows 1 and 3 are maintained.


## Joining more than 2 data frames

Unfortunately, the mutating and filtering joins only enable the merger of two data frames. However, there are situations where we have 3 or more datasets. 
Let's imagine we wanted to join df1, df2, and df3. In  this scenario, we would need the `reduce()` function from the `purrr` package.

First step is to create a list with the 3 data frames:

```{r}
a <- list(df1, df2, df3)#create a list with the 3 datasets
```

Afterwards  we write the function below:
```{r}
df_reduce <- a %>% 
  reduce(left_join, by = c("Id", "Face", "Sex"))#use reduce() function. The first argument is the list created. The second argument is the join that you intend to use.

df_reduce
```

## Set Operations
Sometimes we do not need to use joins when we have two datasets  we want to unite. This happens when two datasets have the same variables. For instance, df1 and df3:

See df1:
```{r}
df1
```


And df3:
```{r}
df3
```

To combine these datasets we should use what is called set operators. R has 3 of such: union, setdiff, and intersect.

`union()` is used when we want all the unique observations of the two data frames


```{r}
df_u <- df1 %>% 
  union(df3)

df_u 
```

Now, we have only 12 rows, because 6 of the rows in df3 were the same as in df1. Remember, the union function returns only the unique rows. In this case, we have 12 observations that are unique within both data frames.

In other cases, we can use the `setdiff()` if we want only the observation in df1 not in common with df3.


```{r}
df_dif <- df1 %>% 
  setdiff(df3)

df_dif 
```

Returns only the rows with Id 1 to 3. The rows not in common with df3.

Finally, we can use `intersect()` if we want only the observations in common between df1, df3.


```{r}
df_intersect <- df1 %>% 
  intersect(df3)

df_intersect
```


## Adding Rows and Columns
Occasionally, it may happen that you want to combine cases from two datasets with the same variables. In that scenario you should use the function `bind_rows()` from the `dplyr` package.

We can use the function bind_rows with df1 and df3.

```{r}
df_bind <- df1 %>% 
  bind_rows(df3)

df_bind
```
Now we have 18 rows.
One nice feature of `bind_rows()` is the possibility to add an extra column , allowing you to identify of which data frame came each observation.

```{r}
bind_df2 <- bind_rows(Dataset1 = df1, 
                   Dataset2 = df3, 
                   .id = "Dataset")

bind_df2 
```


As a result we have a new variable called Dataset with two levels, Dataset1 and Dataset2. This allow us to identify from which dataset the observations came.

Besides rows you can also bind columns. Again, with the package `dplyr` you have the option of doing it through  the `bind_cols()` function. 

Let us use a data frame from another post and break in two:

```{r}
us_tuition <- read_excel(here("us_avg_tuition.xlsx"))

head(us_tuition)
# break the data frame in two
us_1 <- us_tuition[1:6] 
head(us_1)
#
us_2 <- us_tuition[7:13]
head(us_2)

```

Now let us use the `bind_cols()` function to unite them.
```{r}
us_reunited <- us_1 %>% 
  bind_cols(us_2)

head(us_reunited)
```

I hope you liked this tutorial about how to put together different data frames. Thank you and keep coding!

