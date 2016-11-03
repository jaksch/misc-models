# devtools::install_github("exploratory-io/exploratory_func")
library('exploratory')
library('tidyverse')

##############################################
## association rules market basket analysis ##
##############################################
## from: https://blog.exploratory.io/introduction-to-association-rules-market-basket-analysis-in-r-7a0dd900a3e0#.5xyea2w5d

## data
data <- read_csv("C:\\R\\github\\misc-models\\association-rules-market-basket-analysis\\groceries.csv", 
                 col_names = FALSE)
head(data)
## one row is a transaction

## add unique id
data2 <- data %>% 
  mutate(transaction_id = row_number())
data2

## make the data tidy
tidy_data <- gather(data2, key, value = product, X1:X4, na.rm = TRUE) %>% 
  select(-key) %>% 
  arrange(transaction_id)
tidy_data

## data are ready to be analyzed ##

model_results <- exploratory::do_apriori(tidy_data, product, transaction_id, min_support = 0.0001)
head(model_results)
nrow(model_results)
## 16186 different rules!!

## Support
"The support of an item or item set is the fraction of transactions in our data set that contain 
that item or item set. In general, it is nice to identify rules that have a high support, as these 
will be applicable to a large number of transactions. For super market retailers, this is likely to 
involve basic products that are popular across an entire user base (e.g. bread, milk)"

## Confidence
"The confidence of a rule is the likelihood that it is true for a new transaction that contains the 
items on the LHS of the rule. (I.e. it is the probability that the transaction also contains the 
item(s) on the RHS.) "

## Lift
"The lift of a rule is the ratio of the support of the items on the LHS of the rule co-occuring 
with items on the RHS divided by probability that the LHS and RHS co-occur if the two are 
independent.
If lift is greater than 1, it suggests that the precense of the items on the LHS has increased the 
probability that the items on the right hand side will occur on this transaction. If the lift is 
below 1, it suggests that the presence of the items on the LHS make the probability that the items 
on the RHS will be part of the transaction lower. If the lift is 1, it suggests that the presence 
of items on the LHS and RHS really are independent: knowing that the items on the LHS are present 
makes no difference to the probability that items will occur on the RHS."

## Note on what we are looking for
"When we perform market basket analysis, then, we are looking for rules with a lift of more than 
one. Rules with higher confidence are ones where the probability of an item appearing on the RHS is 
high given the presence of the items on the LHS. It is also preferable (higher value) to action 
rules that have a high support - as these will be applicable to a larger number of transactions."

model_results2 <- model_results %>% 
  filter(support > 0.0004)
head(model_results2)
nrow(model_results2)
## now only 66 different rules

## find the best rules for each RHS with regard to lift
model_results3 <- model_results2 %>% 
  group_by(rhs) %>% 
  top_n(3, lift)
model_results3

## present the results
model_results3 %>% 
  arrange(rhs, desc(lift)) %>% 
  select(rhs, lhs, support:lift) %>% 
  as.data.frame()

## end