# https://albert-rapp.de/posts/ggplot2-tips/28_maps/28_maps.html

library(tidyverse)
install.packages('giscoR')
library(giscoR)
install.packages('janitor')
library(janitor)
install.packages('ggiraph')
library(ggiraph)
library(sf)

germany_districts <- gisco_get_nuts(
  year = "2021", 
  nuts_level = 3,
  epsg = 3035,
  country = 'Germany'
) %>% 
  # Nicer output
  as_tibble() %>% 
  janitor::clean_names()

germany_districts %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf()

germany_states <- gisco_get_nuts(
  year = "2021", 
  nuts_level = 1,
  epsg = 3035,
  country = 'Germany'
) %>% 
  as_tibble() %>% 
  janitor::clean_names()

germany_districts %>%
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    data = germany_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf()

germany_districts %>%
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    data = germany_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf(
    fill = NA,
    color = 'black',
    linewidth = 0.1
  )

gg_plt <- germany_districts %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    data = germany_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf_interactive(
    fill = NA, 
    aes(
      data_id = nuts_id,
      tooltip = glue::glue('{nuts_name}')
    ),
    linewidth = 0.1
  )

girafe(ggobj = gg_plt)

gg_plt <- germany_districts %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    data = germany_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf_interactive(
    fill = NA, 
    aes(
      data_id = nuts_id,
      tooltip = glue::glue('{nuts_name}')
    ),
    linewidth = 0.1
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  )

girafe(ggobj = gg_plt)

state_nmbrs <- map_dbl(
  germany_districts$geometry,
  \(x) {
    map_lgl(
      germany_states$geometry,
      \(y) st_within(x, y) %>% 
        as.logical()
    ) %>% which()
  }
)

germany_districts_w_state <- germany_districts %>% 
  mutate(
    state = germany_states$nuts_name[state_nmbrs]
  )

gg_plt <- germany_districts_w_state %>% 
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    data = germany_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf_interactive(
    fill = NA, 
    aes(
      data_id = nuts_id,
      tooltip = glue::glue('{nuts_name}<br>{state}')
    ),
    linewidth = 0.1
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  )

girafe(ggobj = gg_plt)

make_nice_label <- function(nuts_name, state) {
  nuts_name_label <- htmltools::span(
    nuts_name,
    style = htmltools::css(
      fontweight = 600,
      font_family = 'Source Sans Pro',
      font_size = '32px'
    )
  )
  state_label <- htmltools::span(
    state,
    style = htmltools::css(
      font_family = 'Source Sans Pro',
      font_size = '20px'
    )
  )
  glue::glue('{nuts_name_label}<br>{state_label}')
}

germany_districts_w_state_and_labels <- germany_districts_w_state  %>% 
  mutate(
    nice_label = map2_chr(
      nuts_name,
      state,
      make_nice_label
    )
  )

ggplt <- germany_districts_w_state_and_labels  |> 
  ggplot(aes(geometry = geometry)) +
  geom_sf(
    data = germany_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf_interactive(
    fill = NA, 
    aes(
      data_id = nuts_id,
      tooltip = nice_label
    ),
    linewidth = 0.1
  ) +
  geom_sf(
    data = germany_states,
    aes(fill = nuts_name),
    color = 'black',
    linewidth = 0.5
  ) +
  geom_sf_interactive(
    fill = NA, 
    aes(
      data_id = nuts_id,
      tooltip = nice_label
    ),
    linewidth = 0.1
  ) +
  theme_void() +
  theme(
    legend.position = 'none'
  ) +
  scale_fill_manual(
    values = c("#A0CBE8FF", "#F28E2BFF", "#FFBE7DFF", "#59A14FFF", "#8CD17DFF", "#B6992DFF", 
               "#F1CE63FF", "#499894FF", "#86BCB6FF", "#E15759FF", "#FF9D9AFF", "#79706EFF", 
               "#BAB0ACFF", "#D37295FF", "#FABFD2FF", "#B07AA1FF", "#D4A6C8FF", "#9D7660FF", "#D7B5A6FF")
  )

girafe(ggobj = ggplt)

girafe(
  ggobj = ggplt,
  options = list(
    opts_hover(
      css = girafe_css(
        css = '',
        area = 'stroke: black; fill: black;'
      )
    )
  )
)
