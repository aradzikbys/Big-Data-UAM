###################################################
### DPLYR - 01
###################################################

# Calculate:
#     - average weight
#     - average fuel consumption in liters per 100 kilometers
# for cars with at least 6 cylinders.

library(dplyr)
?mtcars  

data01 = mtcars %>%
  tibble::rownames_to_column('car_model') %>%
  tibble::as_tibble()

# 1 mile = 1.609344 km
# 1 gallon = 3.78541178 l

# 1 mpg = 1.609344 km / g = 0.4251437 km / l
# (1 / 0.4251437) * 100  = l / km
# 235.2146 l / 100 km

1.609344 / 3.78541178
(1 / 0.4251437) * 100

data01 %>%
  filter(cyl >= 6) %>%
  mutate(lp100 = 235.2146 / mpg ) %>%
  summarise(
    mean_weight = mean(wt),
    mean_lp100 = mean(lp100), 
  )


###################################################
### DPLYR - 02
###################################################

# Add flag columns (TRUE/FALSE) to the iris dataset:
#   - petal length of the individual is greater than the mean + std dev. for the species
#   - petal width of the individual is greater than the mean + std dev. for the species
#   - sepal length of the individual is greater than the mean + std dev. for the species
#   - the sepal width of the individual is greater than the mean + std dev. for the species
#   - the total number of deviations of a given individual (i.e. how many of the above are TRUE)
# Sort result descending by. total number of deviations.


library(dplyr)
?iris

data_iris = as_tibble(iris)
data_iris

         
data_iris %>%
  group_by(Species) %>%
  
  mutate(big_sl = case_when(
          Sepal.Length > mean(Sepal.Length) + sd(Sepal.Length) ~ TRUE,
          Sepal.Length <= mean(Sepal.Length) + sd(Sepal.Length) ~ FALSE
          ),
         
         big_sw = case_when(
          Sepal.Width > mean(Sepal.Width) + sd(Sepal.Width) ~ TRUE,
          Sepal.Width <= mean(Sepal.Width) + sd(Sepal.Width) ~ FALSE
          ),
         
         big_pl = case_when(
           Petal.Length > mean(Petal.Length) + sd(Petal.Length) ~ TRUE,
           Petal.Length <= mean(Petal.Length) + sd(Petal.Length) ~ FALSE
          ),
         
         big_pw = case_when(
           Petal.Width > mean(Petal.Width) + sd(Petal.Width) ~ TRUE,
           Petal.Width <= mean(Petal.Width) + sd(Petal.Width) ~ FALSE
          )
        ) %>%
  
  rowwise() %>%
  
  mutate(big_cnt = sum(c(big_sl,big_sw,big_pl, big_pw))) %>%
  
  arrange(desc(big_cnt))

