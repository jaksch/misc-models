library("jsonlite")
library("dplyr")

ls("package:jsonlite")
data_path <- "C:/R/github/misc-models/yelp-json-data/data/"

##############
## load data #
##############
yelp_data <- fromJSON(paste0(data_path, "yelp_academic_dataset_business.json"))
## we get an error because this is NDJSON

yelp_data <- stream_in(file(paste0(data_path, "yelp_academic_dataset_business.json")))
head(yelp_data)

str(yelp_data)
glimpse(yelp_data)
## we have variables that are data frames or lists

yelp_data_flat <- flatten(yelp_data)
glimpse(yelp_data_flat)
## it does flatten the data more but we still have a couple of variables that are lists

## put the data into a tibble
yelp_tbl <- as_data_frame(yelp_data_flat)
yelp_tbl

###################
## transform data #
###################
## we have a lot of variables, let's get rid of all hours.* and attributes.* variables

yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attributes"))
## we now have 13 variables (compared with 98 before)

## now I want to see all variables on the screen
options(dplyr.width = Inf)

## look at first 10 categories (lists)
yelp_tbl$categories[1:10]

## lets find all rows with "Restaurants" in the categories variable
yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attributes")) %>% 
  filter(stringr::str_detect(categories, "Restaurants"))
## now we have just 26,719 observations (compared to 85,891 before)
## we have one row per restaurant (or business_id if you will)

## we now wnat to unnest the "categories" variable
yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attributes")) %>% 
  filter(stringr::str_detect(categories, "Restaurants")) %>% 
  tidyr::unnest(categories)
## we now have 81,220 rows because the unnest function creates one row per restaurant X category so
## if a restaurant have three categories it will get three rows
## also notice that the other list variable "neighborhoods" now is gone, so pay attention when using
## the unnest function

## NOTE 
## we can keep the other list variable "neighborhoods" if we add '.drop=FALSE' to the unnest function
yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attributes")) %>% 
  filter(stringr::str_detect(categories, "Restaurants")) %>% 
  tidyr::unnest(categories, .drop = FALSE)

## under the assumption that one restaurant only can have a given category one time we can now count
## which categories that have gotten most reviews
yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attributes")) %>% 
  filter(stringr::str_detect(categories, "Restaurants")) %>% 
  tidyr::unnest(categories) %>% 
  count(categories) %>% 
  arrange(-n)

## lets look at categories per state
yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attributes")) %>% 
  filter(stringr::str_detect(categories, "Restaurants")) %>% 
  tidyr::unnest(categories) %>% 
  filter(categories != "Restaurants") %>% 
  count(categories, state) %>% 
  arrange(-n)

#########
## plot #
#########
library("ggplot2")
## cool lets find the 20 most used categories and plot them per state
most_used_categories <- yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attributes")) %>% 
  filter(stringr::str_detect(categories, "Restaurants")) %>% 
  tidyr::unnest(categories) %>% 
  filter(categories != "Restaurants") %>% 
  count(categories) %>% 
  arrange(-n) %>% 
  top_n(20, n)

yelp_tbl %>% 
  select(-starts_with("hours"), -starts_with("attributes")) %>% 
  filter(stringr::str_detect(categories, "Restaurants")) %>% 
  tidyr::unnest(categories) %>% 
  filter(categories %in% most_used_categories$categories) %>% 
  mutate(categories = factor(categories, levels = most_used_categories$categories, ordered = TRUE)) %>% 
  count(categories, state) %>% 
  arrange(-n) %>% 
  ggplot(aes(x = categories, y = n, fill = state)) +
  geom_bar(stat = "identity")

## end


