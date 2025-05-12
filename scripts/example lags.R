# https://datasciencetut.com/how-to-calculate-lag-by-group-in-r/

library(tidyverse)

df <- data.frame(store=c('Store1', 'Store2', 'Store1', 'Store2', 'Store1', 'Store2', 'Store1', 'Store2'), 
                 sales=c(1057, 1212, 1560, 459, 1259, 4511, 28718, 789523))

lag1 <- df %>%
  group_by(store) %>%
  mutate(lag1_sales = lag(sales, n=1, order_by=store))

lag2 <- df %>%
  group_by(store) %>%
  mutate(lag2_sales = lag(sales, n=2, order_by=store))

lags <- df %>%
  group_by(store) %>%
  mutate(lag1_sales = lag(sales, n=1, order_by= store)) %>%
  mutate(lag2_sales = lag(sales, n=2, order_by = store))