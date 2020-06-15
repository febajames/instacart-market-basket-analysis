# instacart-market-basket-analysis

Instacart(E-commerce company that concentrates in same-day grocery delivery and pick-up service in the U.S. and Canada)
Dataset was obtained directly from instacart.com (https://www.instacart.com/datasets/grocery-shopping-2017), and contains more than 
3 million transactions generated from over 200,000 customer accounts within a year. The dataset is made up of 6 CSV files, including 
orders, products, aisles, departments, order_products__prior, and order_products__train.<br/>
Followed a methodical approach in analyzing buying behaviour and purchase patterns by first performing a value-based customer
segmentation using EFM (Engagement, Frequency, Monetary), a variation of the conventional RFM framework. Since no timestamps were 
provided in the dataset, the level of engagement for each customer was measured as the average number of days between orders. Next,
market basket analyis was performed to study prchase patterns - association rule mining done on 2 types of transaction data:<br/>
i. Transactions data that was generated based on orders<br/>
ii.	Transactions data that was generated based on each customer’s purchase history<br/>
Association mining based on individual customers helped to capture any associations between items that didn’t exist while looking at
each order of a customer as a separate transaction. Rules were mined on the entire customer base, as well as separately, 
on premium and non-premium customer groups. Beyond MBA, recommender system was also built using collaborative filtering.
