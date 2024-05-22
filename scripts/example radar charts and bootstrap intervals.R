# https://datageeek.com/2024/02/28/homicide-rates-from-gender-perspective-understanding-with-radar-chart-and-bootstrap-intervals/

install.packages('fmsb')
install.packages('WDI')
install.packages('crosstable')
install.packages('rsample')
install.packages('ggtext')
library(tidyverse)
library(WDI)
library(crosstable)
library(ggtext)

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

#The data frame of the international homicide rate by gender, 2020
df_2020 <- 
  df_merged %>% 
  filter(year == 2020,
         region != "Aggregates") %>% 
  select(region, gender, income, rate)

#Radar/spider chart
library(fmsb)

df_radar <- 
  df_2020 %>% 
  group_by(region, gender) %>% 
  summarise(mean = mean(rate)) %>% 
  pivot_wider(names_from = "region", 
              values_from = "mean") %>% 
  column_to_rownames("gender")

#Adding the max and min of each variable to use the fmsb package
df_radar <- rbind(rep(32,7),
                  rep(0,7),
                  df_radar)

#Plotting the average homicide rates(per 100.000 people) 
#by gender in the Regions, 2020
radarchart(df_radar,
           pcol = c("orange","steelblue"))

#Setting font family
par(family = "Bricolage Grotesque") 

#Plot title
title("Average Homicide Rates by Gender in the Regions, 2020", 
      sub = "(per 100.000 people)",
      font = 2)

#Legend
legend(x= 0.7, 
       y= 1.2,
       legend = c("Female", "Male"), 
       bty = "n", 
       pch=20 , 
       col=c("orange","steelblue"), 
       text.col = "black", 
       cex=0.9, 
       pt.cex=1.6)

#Bootstrap intervals
library(rsample)

set.seed(12345)
without_gender <- 
  reg_intervals(rate ~ region + income, 
                data = df_2020, 
                times = 500)

set.seed(12345)
with_gender <- 
  reg_intervals(rate ~ region + income + gender, 
                data = df_2020, 
                times = 500)

#Bootstrap confidence intervals plot

#Legend colors for the title
legend_cols <- RColorBrewer::brewer.pal(3, "Dark2")

bind_rows(
  without_gender %>% mutate(gender = "without"), 
  with_gender %>% mutate(gender = "with")
) %>%
  mutate(term = str_remove_all(term, "gender|income|region")) %>%
  mutate(term = str_to_title(term)) %>% 
  ggplot(aes(.estimate, 
             term %>% reorder(.estimate), 
             color = gender)) +
  geom_vline(xintercept = 0, 
             linewidth = 1.5, 
             lty = 2, 
             color = "gray50") +
  geom_errorbar(size = 1.4, 
                alpha = 0.7,
                aes(xmin = .lower, 
                    xmax = .upper)) +
  geom_point(size = 3) +
  scale_x_continuous() +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Higher indicates more important",
       y = "",
       title = glue::glue("Bootstrap Intervals <span style='color:{legend_cols[1]}'>with</span> or <span style='color:{legend_cols[2]}'>without</span> Gender")) +
  theme_minimal(base_family = "Bricolage Grotesque",
                base_size = 15) +
  theme(legend.position="none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = "#eaf7fa"),
        axis.title.x = element_text(size = 12),
        plot.title = ggtext::element_markdown(hjust = 0.5, face = "bold"))


