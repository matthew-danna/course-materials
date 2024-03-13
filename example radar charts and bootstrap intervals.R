# https://datageeek.com/2024/02/28/homicide-rates-from-gender-perspective-understanding-with-radar-chart-and-bootstrap-intervals/

library(tidyverse)
install.packages('WDI')
library(WDI)
install.packages('crosstable')
library(crosstable)

#Intentional homicides, female (per 100,000 female)
df_vi_fe <- 
  WDI(indicator = "VC.IHR.PSRC.FE.P5", 
      extra = TRUE) %>% 
  as_tibble() %>% 
  mutate(gender = "female") %>% 
  rename(rate = VC.IHR.PSRC.FE.P5)

#Intentional homicides, male (per 100,000 male)
df_vi_ma <- 
  WDI(indicator = "VC.IHR.PSRC.MA.P5", 
      extra = TRUE) %>% 
  as_tibble() %>% 
  mutate(gender = "male") %>% 
  rename(rate = VC.IHR.PSRC.MA.P5)

#Combining all the datasets
df_merged <- 
  df_vi_fe %>% 
  rbind(df_vi_ma) %>% 
  #removing labels attribute for fitting process
  crosstable::remove_labels() %>% 
  drop_na()