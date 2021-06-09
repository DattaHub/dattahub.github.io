## ---- Getting the package -------------------------------------------------------

install.packages("dplyr")


## ---- message = FALSE----------------------------------------------------

library("dplyr")

## -----Use the cats data ----------------------------------------------

cats <- read.csv("data/herding-cats-small.csv")


##----Subsetting data : 'select'--------------------------------------------------------------------

cats[, "coat"]  ## usual way


## ----select function --------------------------------------------------------------------
## No need to enclose our column names in quotes

select(cats, coat)

select(cats, sex)

## different outputs: data.frames instead of vectors 

## ------select multiple columns ------------------------------------------------------------------

select(cats, coat, cat_id, fixed) #maintains order


## ---- 'Filtering the data -------------------------------------------------------------------
## `filter`function 

## `select` allows us to select _columns_, 
## `filter` operates on _rows_. 

## Say we want to see the all the cats with black coats;

cats$coat == "black"

cats[cats$coat == "black", ] ## usual way


## ----Use filter --------------------------------------------------------------------

filter(cats, coat == "black")

# we don't have to use the `$` operator to tell `filter` 
# where the `coat` column is; it's smart enough to figure out


## ----Sorting by value --------------------------------------------------------------------

# Maybe you have a set of observations in your data
# that you want to organize by their value.

arrange(cats, coat)

# you can include additional columns to help sort the data
arrange(cats, coat, sex)


## -----Cleaning data-------------------------------------------------------------------

# updating/cleaning some of the values in columns. 
# `mutate` allows us to do this relatively easily.
# Let's say I don't want a lot of decimal places in one of variable

mutate(cats, weight = round(weight, 2))


## ------Transforming------------------------------------------------------------------
# generating a new column based on values that are already in the dataset
# let's say you want to add two variables together

mutate(cats, new_variable = age + weight)

# you can include as many new variables as you want, separated by a comma

mutate(cats, new_var_1 = age + weight, new_var_2 = age * weight)


## -----Multiple functions -------------------------------------------------------------------
# Need to use multiple functions in a row to organize some data
# Can lead to ugly codes

# for example
sort(round(sqrt(cats$age * 2), 3))

# I have multiple steps to get my result, 
# but you have to read what's going on from the inside out. 
# Really cumbersome ! 

## --------Using Pipes----------------------------------------------------------------
#`dplyr` includes a special operator designed to make code *flow* 
# and appear more readable.

## It's written as `%>%`, and you can call it the "pipe" operator.

(cats$age * 2) %>% 
  sqrt() %>%
  round(3) %>%
  sort()


## think of the `%>%` as short-hand for "then"


## ----Another example --------------------------------------------------------------------
round(1.23456789, 3)


## ----Using pipe --------------------------------------------------------------------
1.23456789 %>% round(3)

## Pipe makes sure the output of the expression on the left-hand-side 
## is inserted as the first argument of the expression on the right-hand-side.

## --Period as a placeholder ----------------------------------------------------------------------

3 %>% round(1.23456789, .)

## Real examples 
# The pipes really become useful when chaining 
# together multiple operations in sequence, 
# as we'll do with our `dplyr` functions.

## -----Combining Select & Filter-------------------------------------------------------------------

# Remember how we used `filter` to select only the black cats?
# What if we only want to see the ID's of those cats, 
# rather than all the info about them?


# First the standard way 
# select the `cat_id` column from our `filter`ed dataset like so
# reading from the inside out

select(filter(cats, coat == "black"), cat_id)

# more layers make this annoying to read from inside out 

## ------------------------------------------------------------------------
# reading from left to right
#  We can use the pipe operator to clean that up.

filter(cats, coat == "black") %>% select(cat_id)


## ------------------------------------------------------------------------
# We could even add another pipe to feed `cats` into `filter`; 
# it isn't necessary, but it makes it even easier to see 

cats %>%
  filter(coat == "black") %>%
  select(cat_id)


## ----Summarize --------------------------------------------------------------------
# While `mutate` creates new columns, 
# it's often useful to summarize multiple rows into a single value. 
# Say we want to find the mean weight of all these cats; 
# enter `summarize`! Like `mutate`

cats %>% summarize(mean_weight = mean(weight))

# Notice how we have only a single value returned, 
# but it's still in a `data.frame` format.Makes chaining easy 

## -----Grouping by -------------------------------------------------------------------
### `group_by`

# A very common data analysis task is to do operations like we did above, 
# but to do them on a group-by-group basis.
# Let's look at the mean weights of our cats, grouping up by coat.

cats %>%
  group_by(coat) %>%
  summarize(mean_weight = mean(weight))


## --------Mutate and group_by----------------------------------------------------------------
# We can also use `mutate` on a per-group basis. 
# Let's make a new column which centers our weights around zero; 
# subtracting the group's mean weight from each cat's weight:

cats %>%
  group_by(coat) %>%
  mutate(centered_weight = weight - mean(weight))


gapminder %>%
  group_by(continent) %>%
  summarize(mean_exp = mean(lifeExp))

gapminder %>%
  filter(year %in% c(1952,2007)) %>% 
  group_by(continent) %>%
  summarize_at(vars(lifeExp, gdpPercap), list(mean,median))



