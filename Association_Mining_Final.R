install.packages("data.table")
install.packages("dplyr")
install.packages("arulesViz")
install.packages("arules")
install.packages("IRdisplay")

library(data.table)
library(dplyr)
library(arules)
library(arulesViz)
library(IRdisplay)

# reading the complete dataset after joining of orders and products data
full_data = fread("data/full_data.csv")

# reading customer segments data
segments = fread('rfm_new.csv')

# premium customer group with M (EFM) = 4 & 5
premium = c(314,
         235,
         215,
         115,
         224,
         234,
         324,
         125,
         325,
         114,
         124,
         214,
         134,
         335,
         315,
         334,
         225,
         135)

premium_cust = segments %>%
  filter(CS1 %in% premium)

# non premium customer group
nonpremium_cust = segments %>%
  filter(!CS1 %in% premium)

# join premium and non premium customer groups with full (orders + products) data
baskets_prem = left_join(premium_cust, full_data, by='user_id')
baskets_nonprem = left_join(nonpremium_cust, full_data, by='user_id')

##### Premium Customer Group #####
# creating Orders Tranasction Data
baskets_premium_order = group_by(baskets, order_id)
baskets_premium_order = summarise(baskets_premium_order,items=as.vector(list(product_name)))

transactions_premium_order = as(baskets_premium_order$items, 'transactions')

#Apriori - By orders
apriori_premium_orders = apriori(transactions_premium_order, list(support=0.001, confidence=0.1, minlen= 2))

apriori_rules_premium_orders = as(apriori_premium_orders,"data.frame")

f = function(x, fun) unlist(as(fun(x), "list"))
dt = data.table(lhs=f(apriori_premium_orders, lhs), rhs=f(apriori_premium_orders, rhs))

topRules = head(apriori_premium_orders, n = 100, by = "lift")
plot = plot(topRules, method = "graph",  engine = "htmlwidget")
htmlwidgets::saveWidget(plot, "plot.html")
display_html('<iframe src="plot.html" width=100% height=450></iframe>')
plot

# creating Customers based Transaction Data
baskets_premium_cust = group_by(baskets, user_id)
baskets_premium_cust = summarise(baskets_premium_cust,items=as.vector(list(product_name)))

transactions_premium_cust = as(baskets_premium_cust$items, 'transactions')

#Apriori - By customers
apriori_premium_cust = apriori(transactions_premium_cust, list(support=0.001, confidence=0.1, minlen= 2))

apriori_rules_premium_cust = as(apriori_premium_cust,"data.frame")

topRules2 = head(apriori_premium_cust, n = 100, by = "lift")
plot2 = plot(topRules2, method = "graph",  engine = "htmlwidget")
htmlwidgets::saveWidget(plot2, "plot2.html")
display_html('<iframe src="plot2.html" width=100% height=450></iframe>')
plot2

##### Non Premium Customer Group #####
# creating Orders Tranasction Data
baskets_nonprem_order = group_by(baskets_nonprem, order_id)
baskets_nonprem_order = summarise(baskets_nonprem_order,items=as.vector(list(product_name)))

transactions_nonprem_order = as(baskets_nonprem_order$items, 'transactions')

#Apriori - By orders
apriori_nonprem_orders = apriori(transactions_nonprem_order, list(support=0.001, confidence=0.1, minlen= 2))

apriori_rules_nonprem_orders = as(apriori_nonprem_orders,"data.frame")

topRules3 = head(apriori_nonprem_orders, n = 100, by = "lift")
plot3 = plot(topRules3, method = "graph",  engine = "htmlwidget")
htmlwidgets::saveWidget(plot3, "plot3.html")
display_html('<iframe src="plot3.html" width=100% height=450></iframe>')
plot3

# creating Customers based Transaction Data
baskets_nonprem_cust = group_by(baskets_nonprem, user_id)
baskets_nonprem_cust = summarise(baskets_nonprem_cust,items=as.vector(list(product_name)))

transactions_nonprem_cust = as(baskets_nonprem_cust$items, 'transactions')

#Apriori - By customers
apriori_nonprem_cust = apriori(transactions_nonprem_cust, list(support=0.001, confidence=0.1, minlen= 2))

apriori_rules_nonprem_cust = as(apriori_nonprem_cust,"data.frame")

topRules4 = head(apriori_nonprem_cust, n = 100, by = "lift")
plot4 = plot(topRules4, method = "graph",  engine = "htmlwidget")
htmlwidgets::saveWidget(plot4, "plot4.html")
display_html('<iframe src="plot4.html" width=100% height=450></iframe>')
plot4

##### Entire Customer Base #####
# creating Customers based Transaction Data and Orders based Transaction Data
baskets_cust = group_by(full_data, user_id) 
baskets_cust = summarise(baskets_cust,items=as.vector(list(product_name)))

baskets_order = group_by(full_data, order_id)
baskets_order = summarise(baskets_order,items=as.vector(list(product_name)))

transactions_cust = as(baskets_cust$items, 'transactions')
transactions_order = as(baskets_order$items, 'transactions')

# Apriori on customers basket
apriori_cust = apriori(transactions_cust, list(support=0.01, confidence=0.4, minlen= 2))

apriori_rules_cust = as(apriori_cust,"data.frame")

topRules5 = head(apriori_cust, n = 100, by = "lift")
plot5 = plot(topRules5, method = "graph",  engine = "htmlwidget")
htmlwidgets::saveWidget(plot5, "plot5.html")
display_html('<iframe src="plot5.html" width=100% height=450></iframe>')
plot5

apriori_cust1 = apriori(transactions_cust, list(support=0.01, confidence=0.4, minlen= 2), appearance = list(lhs="Shredded Mozzarella",default="rhs"))
apriori_cust2 = apriori(transactions_cust, list(support=0.01, confidence=0.4, minlen= 2), appearance = list(lhs="Yellow Onions",default="rhs"))
apriori_rules_cust1_mozzarella = as(apriori_cust1, "data.frame")
apriori_rules_cust2_onion = as(apriori_cust2, "data.frame")

# Apriori on orders basket
apriori_orders = apriori(transactions_order, list(support=0.001, confidence=0.1, minlen= 2))

apriori_rules_orders = as(apriori_orders,"data.frame")

topRules6 = head(apriori_orders, n = 100, by = "lift")
plot6 = plot(topRules6, method = "graph",  engine = "htmlwidget")
htmlwidgets::saveWidget(plot6, "plot6.html")
display_html('<iframe src="plot6.html" width=100% height=450></iframe>')
plot6

apriori_orders1 = apriori(transactions_order, list(support=0.001, confidence=0.1, minlen= 2), appearance = list(lhs="Shredded Mozzarella",default="rhs"))
apriori_orders2 = apriori(transactions_order, list(support=0.001, confidence=0.1, minlen= 2), appearance = list(lhs="Yellow Onions",default="rhs"))
apriori_rules_orders1_mozzarella = as(apriori_orders, "data.frame")
apriori_rules_orders2_onion = as(apriori_orders, "data.frame")         