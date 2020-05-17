install.packages("dplyr")

library(dplyr)

# read in all csv files
aisles <- read.csv('data/aisles.csv')
departments <- read.csv('data/departments.csv')
orders <- read.csv('data/orders.csv')
products <- read.csv('data/products.csv')
op_prior <- read.csv('data/order_products__prior.csv')
op_train <- read.csv('data/order_products__train.csv')

# remove 'test' orders
orders <- orders %>%
  filter(eval_set != 'test')

# merge the train and prior order_products datasets
order_products <- rbind(op_prior, op_train)

# join order_products with product info datatsets
order_products <- order_products %>%
  left_join(products, by = 'product_id') %>%
  left_join(departments, by = 'department_id') %>%
  left_join(aisles, by = 'aisle_id')

# join orders with order_products for the full comprehensive dataset
full_data <- left_join(orders, order_products, by = 'order_id')

# price imputation for aisle wise products
getAislePrice <- function(data) {
  data$price = NA
  for (i in 1:nrow(data)) {
    data$price[i] = case_when(data$purch_freq[i] <= quantile(data$purch_freq, probs = seq(0, 1, by= 0.1))[[2]] ~ round(rnorm(1, data$max, data$max_sd), digits = 2),
                              data$purch_freq[i] > quantile(data$purch_freq, probs = seq(0, 1, by= 0.1))[[2]] & data$purch_freq[i] <= quantile(data$purch_freq, probs = seq(0, 1, by= 0.1))[[10]] ~ round(rnorm(1, data$mid, data$mid_sd), digits = 2),
                              data$purch_freq[i] > quantile(data$purch_freq, probs = seq(0, 1, by= 0.1))[[10]] & data$purch_freq[i] <= quantile(data$purch_freq, probs = seq(0, 1, by= 0.1))[[11]] ~ round(rnorm(1, data$min, data$min_sd), digits = 2))
  }            
  return(data)
}

alist <- list()

# price ranges for aisles 1 - 134 basked on research on Instacart and general market
alist[[1]] <- prod_freq %>% 
  filter(aisle_id == 1) %>%
  mutate(max = 7) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

alist[[2]] <- prod_freq %>% 
  filter(aisle_id == 2) %>%
  mutate(max = 20) %>%
  mutate(mid = 14) %>%
  mutate(min = 7) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 1)

alist[[3]] <- prod_freq %>% 
  filter(aisle_id == 3) %>%
  mutate(max = 7) %>%
  mutate(mid = 5.5) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.25) %>%
  mutate(min_sd = 0.1)

alist[[4]] <- prod_freq %>% 
  filter(aisle_id == 4) %>%
  mutate(max = 10) %>%
  mutate(mid = 8) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.5) 

alist[[5]] <- prod_freq %>% 
  filter(aisle_id == 5) %>%
  mutate(max = 7) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.5)

alist[[6]] <- prod_freq %>% 
  filter(aisle_id == 6) %>%
  mutate(max = 12) %>%
  mutate(mid = 9) %>%
  mutate(min = 7) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.5)

alist[[7]] <- prod_freq %>% 
  filter(aisle_id == 7) %>%
  mutate(max = 16) %>%
  mutate(mid = 12) %>%
  mutate(min = 8) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 1)

alist[[8]] <- prod_freq %>% 
  filter(aisle_id == 8) %>%
  mutate(max = 8) %>%
  mutate(mid = 7) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 1)

alist[[9]] <- prod_freq %>% 
  filter(aisle_id == 9) %>%
  mutate(max = 7.5) %>%
  mutate(mid = 5.5) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

alist[[10]] <- prod_freq %>% 
  filter(aisle_id == 10) %>%
  mutate(max = 22) %>%
  mutate(mid = 14) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 2) %>%
  mutate(min_sd = 2)

alist[[11]] <- prod_freq %>% 
  filter(aisle_id == 11) %>%
  mutate(max = 14.5) %>%
  mutate(mid = 8.5) %>%
  mutate(min = 3.5) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 1)

alist[[12]] <- prod_freq %>% 
  filter(aisle_id == 12) %>%
  mutate(max = 13) %>%
  mutate(mid = 9) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[13]] <- prod_freq %>% 
  filter(aisle_id == 13) %>%
  mutate(max = 8) %>%
  mutate(mid = 6) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.5)

alist[[14]] <- prod_freq %>% 
  filter(aisle_id == 14) %>%
  mutate(max = 8) %>%
  mutate(mid = 4) %>%
  mutate(min = 1.5) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.25)

alist[[15]] <- prod_freq %>% 
  filter(aisle_id == 15) %>%
  mutate(max = 21) %>%
  mutate(mid = 14 ) %>%
  mutate(min = 9) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 1)

alist[[16]] <- prod_freq %>% 
  filter(aisle_id == 16) %>%
  mutate(max = 3.5) %>%
  mutate(mid = 2.5) %>%
  mutate(min = 1.5) %>%
  mutate(max_sd = 0.25) %>%
  mutate(mid_sd = 0.25) %>%
  mutate(min_sd = 0.25)

alist[[17]] <- prod_freq %>% 
  filter(aisle_id == 17) %>%
  mutate(max = 14) %>%
  mutate(mid = 10) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 1.5)

alist[[18]] <- prod_freq %>% 
  filter(aisle_id == 18) %>%
  mutate(max = 7) %>%
  mutate(mid = 5.5) %>%
  mutate(min = 4.5) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.25) %>%
  mutate(min_sd = 0.25)

alist[[19]] <- prod_freq %>% 
  filter(aisle_id == 19) %>%
  mutate(max = 14) %>%
  mutate(mid = 11) %>%
  mutate(min = 8) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[20]] <- prod_freq %>% 
  filter(aisle_id == 20) %>%
  mutate(max = 11.5) %>%
  mutate(mid = 8.5) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 0.75) %>%
  mutate(mid_sd = 0.75) %>%
  mutate(min_sd = 0.5)

alist[[21]] <- prod_freq %>% 
  filter(aisle_id == 21) %>%
  mutate(max = 9) %>%
  mutate(mid = 7) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.5)

alist[[22]] <- prod_freq %>% 
  filter(aisle_id == 22) %>%
  mutate(max = 16) %>%
  mutate(mid = 12) %>%
  mutate(min = 8) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 1)

alist[[23]] <- prod_freq %>% 
  filter(aisle_id == 23) %>%
  mutate(max = 9.5) %>%
  mutate(mid = 7) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.75) %>%
  mutate(min_sd = 0.5)

alist[[24]] <- prod_freq %>% 
  filter(aisle_id == 24) %>%
  mutate(max = 7) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.5)

alist[[25]] <- prod_freq %>% 
  filter(aisle_id == 25) %>%
  mutate(max = 13) %>%
  mutate(mid = 8) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.5)

alist[[26]] <- prod_freq %>% 
  filter(aisle_id == 26) %>%
  mutate(max = 10.5) %>%
  mutate(mid = 7) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 0.75) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[27]] <- prod_freq %>% 
  filter(aisle_id == 27) %>%
  mutate(max = 7) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.5)

alist[[28]] <- prod_freq %>% 
  filter(aisle_id == 28) %>%
  mutate(max = 150) %>%
  mutate(mid = 70) %>%
  mutate(min = 25) %>%
  mutate(max_sd = 20) %>%
  mutate(mid_sd = 10) %>%
  mutate(min_sd = 5)

alist[[29]] <- prod_freq %>% 
  filter(aisle_id == 29) %>%
  mutate(max = 10) %>%
  mutate(mid = 6) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[30]] <- prod_freq %>% 
  filter(aisle_id == 30) %>%
  mutate(max = 7) %>%
  mutate(mid = 5) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

alist[[31]] <- prod_freq %>% 
  filter(aisle_id == 31) %>%
  mutate(max = 6) %>%
  mutate(mid = 4) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1) 

alist[[32]] <- prod_freq %>% 
  filter(aisle_id == 32) %>%
  mutate(max = 7) %>%
  mutate(mid = 5) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.3) %>%
  mutate(min_sd = 0.2)

alist[[33]] <- prod_freq %>% 
  filter(aisle_id == 33) %>%
  mutate(max = 15) %>%
  mutate(mid = 12) %>%
  mutate(min = 8) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[34]] <- prod_freq %>% 
  filter(aisle_id == 34) %>%
  mutate(max = 18) %>%
  mutate(mid = 12) %>%
  mutate(min = 8) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[35]] <- prod_freq %>% 
  filter(aisle_id == 35) %>%
  mutate(max = 12) %>%
  mutate(mid = 8) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.2)

alist[[36]] <- prod_freq %>% 
  filter(aisle_id == 36) %>%
  mutate(max = 3) %>%
  mutate(mid = 2) %>%
  mutate(min = 1) %>%
  mutate(max_sd = 0.1) %>%
  mutate(mid_sd = 0.1) %>%
  mutate(min_sd = 0.1)

alist[[37]] <- prod_freq %>% 
  filter(aisle_id == 37) %>%
  mutate(max = 12) %>%
  mutate(mid = 5) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

alist[[38]] <- prod_freq %>% 
  filter(aisle_id == 38) %>%
  mutate(max = 10) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[39]] <- prod_freq %>% 
  filter(aisle_id == 39) %>%
  mutate(max = 50) %>%
  mutate(mid = 25) %>%
  mutate(min = 15) %>%
  mutate(max_sd = 5) %>%
  mutate(mid_sd = 2.5) %>%
  mutate(min_sd = 1)

alist[[40]] <- prod_freq %>% 
  filter(aisle_id == 40) %>%
  mutate(max = 45) %>%
  mutate(mid = 30) %>%
  mutate(min = 15) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 1)

alist[[41]] <- prod_freq %>% 
  filter(aisle_id == 41) %>%
  mutate(max = 45) %>%
  mutate(mid = 30) %>%
  mutate(min = 15) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 1)

alist[[42]] <- prod_freq %>% 
  filter(aisle_id == 42) %>%
  mutate(max = 15) %>%
  mutate(mid = 10) %>%
  mutate(min = 7) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.3) %>%
  mutate(min_sd = 0.1)

alist[[43]] <- prod_freq %>% 
  filter(aisle_id == 43) %>%
  mutate(max = 6) %>%
  mutate(mid = 4) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.3) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[44]] <- prod_freq %>% 
  filter(aisle_id == 44) %>%
  mutate(max = 10) %>%
  mutate(mid = 5) %>%
  mutate(min = 1) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[45]] <- prod_freq %>% 
  filter(aisle_id == 45) %>%
  mutate(max = 8) %>%
  mutate(mid = 4) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.3)

alist[[46]] <- prod_freq %>% 
  filter(aisle_id == 46) %>%
  mutate(max = 3) %>%
  mutate(mid = 2) %>%
  mutate(min = 1) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[47]] <- prod_freq %>% 
  filter(aisle_id == 47) %>%
  mutate(max = 20) %>%
  mutate(mid = 15) %>%
  mutate(min = 8) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[48]] <- prod_freq %>% 
  filter(aisle_id == 48) %>%
  mutate(max = 4) %>%
  mutate(mid = 3) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[49]] <- prod_freq %>% 
  filter(aisle_id == 49) %>%
  mutate(max = 20) %>%
  mutate(mid = 14) %>%
  mutate(min = 8) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 1)

alist[[50]] <- prod_freq %>% 
  filter(aisle_id == 50) %>%
  mutate(max = 5) %>%
  mutate(mid = 3) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[51]] <- prod_freq %>% 
  filter(aisle_id == 51) %>%
  mutate(max = 8) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

alist[[52]] <- prod_freq %>% 
  filter(aisle_id == 52) %>%
  mutate(max = 12) %>%
  mutate(mid = 8) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[53]] <- prod_freq %>% 
  filter(aisle_id == 53) %>%
  mutate(max = 5) %>%
  mutate(mid = 4) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[54]] <- prod_freq %>% 
  filter(aisle_id == 54) %>%
  mutate(max = 8) %>%
  mutate(mid = 6) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.2)

alist[[55]] <- prod_freq %>% 
  filter(aisle_id == 55) %>%
  mutate(max = 40) %>%
  mutate(mid = 18) %>%
  mutate(min = 0.10) %>%
  mutate(max_sd = 3.75) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[56]] <- prod_freq %>% 
  filter(aisle_id == 56) %>%
  mutate(max = 50) %>%
  mutate(mid = 20) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 3.78) %>%
  mutate(mid_sd = 1.25) %>%
  mutate(min_sd = 0.6)

alist[[57]] <- prod_freq %>% 
  filter(aisle_id == 57) %>%
  mutate(max = 40) %>%
  mutate(mid = 14) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 4) %>%
  mutate(mid_sd = 1.75) %>%
  mutate(min_sd = 0.54)

alist[[58]] <- prod_freq %>% 
  filter(aisle_id == 58) %>%
  mutate(max = 10) %>%
  mutate(mid = 4) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.4) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1) 

alist[[59]] <- prod_freq %>% 
  filter(aisle_id == 59) %>%
  mutate(max = 61) %>%
  mutate(mid = 40) %>%
  mutate(min = 10) %>%
  mutate(max_sd = 3.4) %>%
  mutate(mid_sd = 2) %>%
  mutate(min_sd = 0.75)

alist[[60]] <- prod_freq %>% 
  filter(aisle_id == 60) %>%
  mutate(max = 54) %>%
  mutate(mid = 16) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 2.2) %>%
  mutate(mid_sd = 0.75) %>%
  mutate(min_sd = 0.5)

alist[[61]] <- prod_freq %>% 
  filter(aisle_id == 61) %>%
  mutate(max = 78) %>%
  mutate(mid = 30) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 10) %>%
  mutate(mid_sd = 2) %>%
  mutate(min_sd = 0.5)

alist[[62]] <- prod_freq %>% 
  filter(aisle_id == 62) %>%
  mutate(max = 400) %>%
  mutate(mid = 100) %>%
  mutate(min = 30) %>%
  mutate(max_sd = 20) %>%
  mutate(mid_sd = 10) %>%
  mutate(min_sd = 5)

alist[[63]] <- prod_freq %>% 
  filter(aisle_id == 63) %>%
  mutate(max = 203) %>%
  mutate(mid = 100) %>%
  mutate(min = 55) %>%
  mutate(max_sd = 30) %>%
  mutate(mid_sd = 10) %>%
  mutate(min_sd = 5)

alist[[64]] <- prod_freq %>% 
  filter(aisle_id == 64) %>%
  mutate(max = 50) %>%
  mutate(mid = 15) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 2.5) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.7)

alist[[65]] <- prod_freq %>% 
  filter(aisle_id == 65) %>%
  mutate(max = 10) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[66]] <- prod_freq %>% 
  filter(aisle_id == 66) %>%
  mutate(max = 70) %>%
  mutate(mid = 32) %>%
  mutate(min = 15) %>%
  mutate(max_sd = 5) %>%
  mutate(mid_sd = 2.5) %>%
  mutate(min_sd = 1)

alist[[67]] <- prod_freq %>% 
  filter(aisle_id == 67) %>%
  mutate(max = 45) %>%
  mutate(mid = 30) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 2.6) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.7)

alist[[68]] <- prod_freq %>% 
  filter(aisle_id == 68) %>%
  mutate(max = 10) %>%
  mutate(mid = 4) %>%
  mutate(min = 1.5) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = .5) %>%
  mutate(min_sd = .08)

alist[[69]] <- prod_freq %>% 
  filter(aisle_id == 69) %>%
  mutate(max = 25) %>%
  mutate(mid = 12) %>%
  mutate(min = 7) %>%
  mutate(max_sd = 1.2) %>%
  mutate(mid_sd = 0.9) %>%
  mutate(min_sd = 0.1)

alist[[70]] <- prod_freq %>% 
  filter(aisle_id == 70) %>%
  mutate(max = 10) %>%
  mutate(mid = 4.75) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.04)

alist[[71]] <- prod_freq %>% 
  filter(aisle_id == 71) %>%
  mutate(max = 11) %>%
  mutate(mid = 7) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.3)

alist[[72]] <- prod_freq %>% 
  filter(aisle_id == 72) %>%
  mutate(max = 13) %>%
  mutate(mid = 5) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

alist[[73]] <- prod_freq %>% 
  filter(aisle_id == 73) %>%
  mutate(max = 36) %>%
  mutate(mid = 13) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[74]] <- prod_freq %>% 
  filter(aisle_id == 74) %>%
  mutate(max = 23) %>%
  mutate(mid = 9.9) %>%
  mutate(min = 1.3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.06)

alist[[75]] <- prod_freq %>% 
  filter(aisle_id == 75) %>%
  mutate(max = 27) %>%
  mutate(mid = 13) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 2.5) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.8)

alist[[76]] <- prod_freq %>% 
  filter(aisle_id == 76) %>%
  mutate(max = 5) %>%
  mutate(mid = 3) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.04)

alist[[77]] <- prod_freq %>% 
  filter(aisle_id == 77) %>%
  mutate(max = 8) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

alist[[78]] <- prod_freq %>% 
  filter(aisle_id == 78) %>%
  mutate(max = 5) %>%
  mutate(mid = 3) %>%
  mutate(min = 0.90) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.25) %>%
  mutate(min_sd = 0.04)

alist[[79]] <- prod_freq %>% 
  filter(aisle_id == 79) %>%
  mutate(max = 20) %>%
  mutate(mid = 13) %>%
  mutate(min = 4.75) %>%
  mutate(max_sd = 1.25) %>%
  mutate(mid_sd = 0.6) %>%
  mutate(min_sd = 0.2)

alist[[80]] <- prod_freq %>% 
  filter(aisle_id == 80) %>%
  mutate(max = 15) %>%
  mutate(mid = 6) %>%
  mutate(min = 4.24) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.2)

alist[[81]] <- prod_freq %>% 
  filter(aisle_id == 81) %>%
  mutate(max = 20) %>%
  mutate(mid = 10) %>%
  mutate(min = 2.50) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.2)

# baby accessories
alist[[82]] <- prod_freq %>% 
  filter(aisle_id == 82) %>%
  mutate(max = 100) %>%
  mutate(mid = 20) %>%
  mutate(min = 12) %>%
  mutate(max_sd = 20) %>%
  mutate(mid_sd = 10) %>%
  mutate(min_sd = 0.5)

# fresh vegetables
alist[[83]] <- prod_freq %>% 
  filter(aisle_id == 83) %>%
  mutate(max = 13) %>%
  mutate(mid = 7) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.3) %>%
  mutate(min_sd = 0.1)

# milk
alist[[84]] <- prod_freq %>% 
  filter(aisle_id == 84) %>%
  mutate(max = 10) %>%
  mutate(mid = 6) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

# food storage
alist[[85]] <- prod_freq %>% 
  filter(aisle_id == 85) %>%
  mutate(max = 10) %>%
  mutate(mid = 6) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1) 

# eggs
alist[[86]] <- prod_freq %>% 
  filter(aisle_id == 86) %>%
  mutate(max = 7) %>%
  mutate(mid = 5) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.3) %>%
  mutate(min_sd = 0.1)

# more household
alist[[87]] <- prod_freq %>% 
  filter(aisle_id == 87) %>%
  mutate(max = 45) %>%
  mutate(mid = 26) %>%
  mutate(min = 7) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.5)

# spreads
alist[[88]] <- prod_freq %>% 
  filter(aisle_id == 88) %>%
  mutate(max = 8) %>%
  mutate(mid = 6) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

# salad dressing toppings
alist[[89]] <- prod_freq %>% 
  filter(aisle_id == 89) %>%
  mutate(max = 12) %>%
  mutate(mid = 9) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.2)

# cocoa drink mixes
alist[[90]] <- prod_freq %>% 
  filter(aisle_id == 90) %>%
  mutate(max = 20) %>%
  mutate(mid = 13) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 3) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

# soy lactosefree
alist[[91]] <- prod_freq %>% 
  filter(aisle_id == 91) %>%
  mutate(max = 60) %>%
  mutate(mid = 40) %>%
  mutate(min = 20) %>%
  mutate(max_sd = 3) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.1)

# baby food formula
alist[[92]] <- prod_freq %>% 
  filter(aisle_id == 92) %>%
  mutate(max = 80) %>%
  mutate(mid = 50) %>%
  mutate(min = 20) %>%
  mutate(max_sd = 3) %>%
  mutate(mid_sd = 2) %>%
  mutate(min_sd = 0.5)

# breakfast bakery
alist[[93]] <- prod_freq %>% 
  filter(aisle_id == 93) %>%
  mutate(max = 5) %>%
  mutate(mid = 4) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

# tea
alist[[94]] <- prod_freq %>% 
  filter(aisle_id == 94) %>%
  mutate(max = 7) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

# canned meat seafood
alist[[95]] <- prod_freq %>% 
  filter(aisle_id == 95) %>%
  mutate(max = 12) %>%
  mutate(mid = 8) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.3)

# lunch meat
alist[[96]] <- prod_freq %>% 
  filter(aisle_id == 96) %>%
  mutate(max = 5) %>%
  mutate(mid = 4) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.3) %>%
  mutate(min_sd = 0.1)

# baking supplies decor
alist[[97]] <- prod_freq %>% 
  filter(aisle_id == 97) %>%
  mutate(max = 18) %>%
  mutate(mid = 12) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 3) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.5)

# juice nectars
alist[[98]] <- prod_freq %>% 
  filter(aisle_id == 98) %>%
  mutate(max = 15) %>%
  mutate(mid = 8) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

# canned friut applesauce
alist[[99]] <- prod_freq %>% 
  filter(aisle_id == 99) %>%
  mutate(max = 6) %>%
  mutate(mid = 4) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

# missing
alist[[100]] <- prod_freq %>%
  filter(aisle_id == 100) %>%
  mutate(max = 50) %>%
  mutate(mid = 50) %>%
  mutate(min = 50) %>%
  mutate(max_sd = 10) %>%
  mutate(mid_sd = 10) %>%
  mutate(min_sd = 10)

# air fresheners candles
alist[[101]] <- prod_freq %>% 
  filter(aisle_id == 101) %>%
  mutate(max = 15) %>%
  mutate(mid = 11) %>%
  mutate(min = 8) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.3)

# baby bath bady care
alist[[102]] <- prod_freq %>% 
  filter(aisle_id == 102) %>%
  mutate(max = 24) %>%
  mutate(mid = 15) %>%
  mutate(min = 6) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.5)

# ice cream toppings
alist[[103]] <- prod_freq %>% 
  filter(aisle_id == 103) %>%
  mutate(max = 4) %>%
  mutate(mid = 3) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

# spices seasonings
alist[[104]] <- prod_freq %>% 
  filter(aisle_id == 104) %>%
  mutate(max = 5) %>%
  mutate(mid = 3) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1)

# doughs gelatins bake mixes
alist[[105]] <- prod_freq %>% 
  filter(aisle_id == 105) %>%
  mutate(max = 8) %>%
  mutate(mid = 5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.1)

# hot dogs bacon sausage
alist[[106]] <- prod_freq %>% 
  filter(aisle_id == 106) %>%
  mutate(max = 12) %>%
  mutate(mid = 8) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

# chips pretzels
alist[[107]] <- prod_freq %>% 
  filter(aisle_id == 107) %>%
  mutate(max = 8) %>%
  mutate(mid = 6) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

# other creams cheeses
alist[[108]] <- prod_freq %>% 
  filter(aisle_id == 108) %>%
  mutate(max = 8) %>%
  mutate(mid = 6) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[109]] <- prod_freq %>% 
  filter(aisle_id == 109) %>%
  mutate(max = 30) %>%
  mutate(mid = 10) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 6) %>%
  mutate(mid_sd = 2) %>%
  mutate(min_sd = 0.8)

alist[[110]] <- prod_freq %>% 
  filter(aisle_id == 110) %>%
  mutate(max = 7) %>%
  mutate(mid = 4) %>%
  mutate(min = 1.5) %>%
  mutate(max_sd = 1.2) %>%
  mutate(mid_sd = 0.6) %>%
  mutate(min_sd = 0.1)

alist[[111]] <- prod_freq %>% 
  filter(aisle_id == 111) %>%
  mutate(max = 6) %>%
  mutate(mid = 4) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.8) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.2)

alist[[112]] <- prod_freq %>% 
  filter(aisle_id == 112) %>%
  mutate(max = 3.5) %>%
  mutate(mid = 2.2) %>%
  mutate(min = 1) %>%
  mutate(max_sd = 0.4) %>%
  mutate(mid_sd = 0.2) %>%
  mutate(min_sd = 0.1) 

alist[[113]] <- prod_freq %>% 
  filter(aisle_id == 113) %>%
  mutate(max = 8) %>%
  mutate(mid = 4) %>%
  mutate(min = 1.5) %>%
  mutate(max_sd = 1.2) %>%
  mutate(mid_sd = 0.6) %>%
  mutate(min_sd = 0.2)

alist[[114]] <- prod_freq %>% 
  filter(aisle_id == 114) %>%
  mutate(max = 20) %>%
  mutate(mid = 10) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 2.5) %>%
  mutate(mid_sd = 1.2) %>%
  mutate(min_sd = 0.6)

alist[[115]] <- prod_freq %>% 
  filter(aisle_id == 115) %>%
  mutate(max = 30) %>%
  mutate(mid = 10) %>%
  mutate(min = 2.5) %>%
  mutate(max_sd = 4) %>%
  mutate(mid_sd = 1.2) %>%
  mutate(min_sd = 0.3)

alist[[116]] <- prod_freq %>% 
  filter(aisle_id == 116) %>%
  mutate(max = 8) %>%
  mutate(mid = 5.5) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.8) %>%
  mutate(min_sd = 0.3)

alist[[117]] <- prod_freq %>% 
  filter(aisle_id == 117) %>%
  mutate(max = 8) %>%
  mutate(mid = 5) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.4) %>%
  mutate(min_sd = 0.3)

alist[[118]] <- prod_freq %>% 
  filter(aisle_id == 118) %>%
  mutate(max = 24) %>%
  mutate(mid = 8) %>%
  mutate(min = 2.5) %>%
  mutate(max_sd = 4) %>%
  mutate(mid_sd = 1.2) %>%
  mutate(min_sd = 0.1)

alist[[119]] <- prod_freq %>% 
  filter(aisle_id == 119) %>%
  mutate(max = 12) %>%
  mutate(mid = 6) %>%
  mutate(min = 2.5) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.3)

alist[[120]] <- prod_freq %>% 
  filter(aisle_id == 120) %>%
  mutate(max = 12) %>%
  mutate(mid = 6) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.3) %>%
  mutate(min_sd = 0.1)

alist[[121]] <- prod_freq %>% 
  filter(aisle_id == 121) %>%
  mutate(max = 10) %>%
  mutate(mid = 7) %>%
  mutate(min = 4) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.7) %>%
  mutate(min_sd = 0.4)

alist[[122]] <- prod_freq %>% 
  filter(aisle_id == 122) %>%
  mutate(max = 12) %>%
  mutate(mid = 8) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 1.5) %>%
  mutate(mid_sd = 0.8) %>%
  mutate(min_sd = 0.3)

alist[[123]] <- prod_freq %>% 
  filter(aisle_id == 123) %>%
  mutate(max = 8) %>%
  mutate(mid = 5) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.6) %>%
  mutate(min_sd = 0.2)

alist[[124]] <- prod_freq %>% 
  filter(aisle_id == 124) %>%
  mutate(max = 120) %>%
  mutate(mid = 60) %>%
  mutate(min = 20) %>%
  mutate(max_sd = 12) %>%
  mutate(mid_sd = 6) %>%
  mutate(min_sd = 3)

alist[[125]] <- prod_freq %>% 
  filter(aisle_id == 125) %>%
  mutate(max = 10) %>%
  mutate(mid = 6) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.6) %>%
  mutate(min_sd = 0.3)

alist[[126]] <- prod_freq %>% 
  filter(aisle_id == 126) %>%
  mutate(max = 15) %>%
  mutate(mid = 8) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 2) %>%
  mutate(mid_sd = 1) %>%
  mutate(min_sd = 0.5)

alist[[127]] <- prod_freq %>% 
  filter(aisle_id == 127) %>%
  mutate(max = 20) %>%
  mutate(mid = 10) %>%
  mutate(min = 3.5) %>%
  mutate(max_sd = 3) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.5)

alist[[128]] <- prod_freq %>% 
  filter(aisle_id == 128) %>%
  mutate(max = 8) %>%
  mutate(mid = 5) %>%
  mutate(min = 2.5) %>%
  mutate(max_sd = 0.8) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.3)

alist[[129]] <- prod_freq %>% 
  filter(aisle_id == 129) %>%
  mutate(max = 10) %>%
  mutate(mid = 5) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 1) %>%
  mutate(mid_sd = 0.5) %>%
  mutate(min_sd = 0.2)

alist[[130]] <- prod_freq %>% 
  filter(aisle_id == 130) %>%
  mutate(max = 7) %>%
  mutate(mid = 4) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.5) %>%
  mutate(mid_sd = 0.3) %>%
  mutate(min_sd = 0.2)

alist[[131]] <- prod_freq %>% 
  filter(aisle_id == 131) %>%
  mutate(max = 4) %>%
  mutate(mid = 3) %>%
  mutate(min = 2) %>%
  mutate(max_sd = 0.4) %>%
  mutate(mid_sd = 0.3) %>%
  mutate(min_sd = 0.2)

alist[[132]] <- prod_freq %>% 
  filter(aisle_id == 132) %>%
  mutate(max = 25) %>%
  mutate(mid = 15) %>%
  mutate(min = 5) %>%
  mutate(max_sd = 2.5) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.5)

alist[[133]] <- prod_freq %>% 
  filter(aisle_id == 133) %>%
  mutate(max = 25) %>%
  mutate(mid = 8) %>%
  mutate(min = 3) %>%
  mutate(max_sd = 4) %>%
  mutate(mid_sd = 1.5) %>%
  mutate(min_sd = 0.5)

alist[[134]] <- prod_freq %>% 
  filter(aisle_id == 134) %>%
  mutate(max = 70) %>%
  mutate(mid = 40) %>%
  mutate(min = 25) %>%
  mutate(max_sd = 7) %>%
  mutate(mid_sd = 5) %>%
  mutate(min_sd = 3)

# running the custom defined price imputation function on the aisles
for (i in 1:134) {
  alist[[i]] <- getAislePrice(alist[[i]])
}

price_full <- bind_rows(alist, .id = "id")

prodID_price <- price_full %>% 
  ungroup() %>% 
  select(product_id, price)

# join with full(orders + products data)
full <- left_join(full, prodID_price, by = 'product_id')

# write out data
write.csv(full, file = 'full_data.csv')

