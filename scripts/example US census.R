##### INTRO
# DATA: https://www.census.gov/programs-surveys/acs/data.html
# More data: https://data.census.gov/table
# PACKAGE: https://walker-data.com/tidycensus/

##### SET-UP
library(tidyverse)
library(tidycensus)

# get an API key: https://api.census.gov/data/key_signup.html
census_api_key("YOUR API KEY GOES HERE", install = TRUE, overwrite = TRUE)

# find Census variables
variables <- load_variables(2023, "acs5", cache = TRUE)
# just the totals
variables.totals <- subset(variables, variables$label == 'Estimate!!Total:')

##### SAMPLE QUERIES
# 2023 total population by state:
pop23 <- get_acs(geography = "state",
                 variables = "B01003_001", 
                 year = 2024)

# 2023 total population by metropolitan statistical area:
pop.msa.23 <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                      variables = "B01003_001",
                      year = 2023)

# 2023 population by race by MSA:
msa.race <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                    variables = c("B02008_001", "B02009_001", "B02010_001", "B02011_001"),
                    year = 2023)

# 2022 ‘component’ datasets by state:
# The variables included in the components of change product consist of both estimates of counts and rates. 
# Rates are preceded by an R in the variable name and are calculated per 1000 residents.
state_components <- get_estimates(geography = "state", product = "components", year = 2022)
# 2022 ‘component’ datasets by MSA:
msa_components <- get_estimates(geography = "metropolitan statistical area/micropolitan statistical area", 
                                product = "components")
# 2022 county comparisons of sex, age, and hispanic populations:
sex.age.hisp <- get_estimates(geography = "county", product = "characteristics", 
                              breakdown = c("SEX", "AGEGROUP", "HISP"),  
                              breakdown_labels = TRUE)
# 2022 county comparisons of sex:
sex.county <- get_estimates(geography = "county", product = "characteristics", breakdown = c("SEX"),  
                            breakdown_labels = TRUE)
# 2022 for age:
county.age <- get_estimates(geography = "county", product = "characteristics", breakdown = c("AGEGROUP"),  
                            breakdown_labels = TRUE)
# 2022 migration population flows to and from an MSA:
dc_flows <- get_flows(geography = "metropolitan statistical area",
                      msa = 47900, # can adjust this to different areas
                      year = 2022,
                      geometry = TRUE)


##### ELECTION DATA: 2024
install.packages('R.utils')
library(data.table)
election.2024 <- fread("https://int.nyt.com/newsgraphics/elections/map-data/2024/national/precincts-with-results.csv.gz")

##### ELECTION DATA: 2020
library(sf)
install.packages('geojsonsf')
library(geojsonsf)
url.2020 <- 'https://int.nyt.com/newsgraphics/elections/map-data/2020/national/precincts-with-results.geojson.gz'
file.2020 <- gzcon(url(url.2020, 'rb'))
election.2020 <- geojson_sf(file.2020)

##### CENSUS MASTER CODES
library(tidycensus)
data("fips_codes")

##### SAMPLE CENSUS MAP
library(tidyverse)
fips.subset <- c(19097, 17155, 50009, 27055, 39143, 55113, 44003, 55011, 19105, 46109, 19179, 55099, 51057)

maps::county.fips %>%
  as.tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") ->
  fips.clean

map_data("county") %>% 
  left_join(fips.clean) ->
  fips.all

fips.all %>% 
  mutate(is_example = fips %in% fips.subset) %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=is_example), color="gray70") +
  coord_map() +
  scale_fill_manual(values=c("TRUE"="red", "FALSE"="gray90"))