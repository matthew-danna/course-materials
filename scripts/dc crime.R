# Step 0
library(tidyverse) # run this every session

# Step 1: Data
url <- "https://opendata.dc.gov/datasets/DCGIS::crime-incidents-in-"
years <- c(2008:2025)
full.urls <- paste0(url, years, ".csv")
dc.data <- data.frame()

for(file in full.urls) {
  tmp.data <- read.csv(file, stringsAsFactors = FALSE)
  dc.data <- rbind(tmp.data, dc.data)
}

rm(tmp.data)

dc.data <- separate(dc.data, REPORT_DAT, into = c("DATE", "TIME"), sep = " ")
dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")

dc.data$YEAR <- substr(dc.data$DATE, 0, 4)
dc.data$MONTH <- month(dc.data$DATE)
dc.data$MONTHS <- months(dc.data$DATE)
dc.data$DAY <- day(dc.data$DATE)
dc.data$DOW <- weekdays(dc.data$DATE)
dc.data$HOUR <- substr(dc.data$TIME, 0, 2)
dc.data$WEEK <- format(as.Date(dc.data$DATE), "%U")
dc.data$WEEK <- as.numeric(dc.data$WEEK)

dc.data$TYPE <- case_when(
  dc.data$OFFENSE %in%
    c(
      "ARSON",
      "BURGLARY",
      "MOTOR VEHICLE THEFT",
      "THEFT F/AUTO",
      "THEFT/OTHER") ~ "Property",
  dc.data$OFFENSE %in%
    c("ASSAULT W/DANGEROUS WEAPON", "HOMICIDE", "ROBBERY", 
      "SEX ABUSE")
  ~ "Person")

# Step 2: Graph prep
### Dashed line for 2025-08-11
deployment_week <- as.numeric(format(as.Date("2025-08-11"), "%U"))

### Get weekly counts by Type
crime.weekly <- dc.data %>%
  filter(YEAR >= 2023 & YEAR <= 2025) %>%  # Keep only recent years
  group_by(YEAR, WEEK, TYPE) %>%
  summarise(COUNT = n(), .groups = "drop")

### GET WEEKLY COUNTS BY WARD/DISTRICT

# Step 3: Graphs
### Property vs Person
ggplot(crime.weekly, aes(x = WEEK, y = COUNT, color = as.factor(YEAR), group = YEAR)) +
  geom_line(size = 1) +
  geom_vline(aes(xintercept = deployment_week, linetype = "National Guard"), 
             color = "black", size = 1) +
  scale_linetype_manual("", values = c("National Guard" = "dashed")) +
  facet_wrap(~ TYPE, scales = "free_y") +
  labs(
    title = "Weekly Crime Counts in DC",
    subtitle = "By Crime Category (Property vs Person)",
    x = "Week of Year",
    y = "Number of Incidents",
    color = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 12)
  )

# Step 4: Before vs After by District

# Define deployment date
deployment_date <- as.Date("2025-08-11")

# Step 1: If not already aggregated, count incidents per date/district/crime type
crime_daily <- dc.data %>%
  group_by(DATE, WARD, TYPE) %>%
  summarise(COUNT = n(), .groups = "drop")

# Step 2: Determine window length (balanced before/after)
max_date <- max(crime_daily$DATE, na.rm = TRUE)
days_after <- as.numeric(max_date - deployment_date)
start_date <- deployment_date - days_after
end_date <- max_date

# Step 3: Filter for balanced window and classify as before/after
crime_window <- crime_daily %>%
  filter(DATE >= start_date & DATE <= end_date) %>%
  mutate(PERIOD = ifelse(DATE < deployment_date, "Before", "After"))

# Step 4: Count number of days in each period (should be the same, but let’s compute just in case)
days_per_period <- crime_window %>%
  group_by(PERIOD) %>%
  summarise(DAYS = n_distinct(DATE), .groups = "drop")

# Step 5: Aggregate and normalize per day
crime_summary <- crime_window %>%
  group_by(PERIOD, WARD, TYPE) %>%
  summarise(TOTAL.CRIME = sum(COUNT, na.rm = TRUE), .groups = "drop") %>%
  left_join(days_per_period, by = "PERIOD") %>%
  mutate(avg_daily_crimes = TOTAL.CRIME / DAYS)

# Step 6: Visualize (ordered bars + all districts visible)
crime_summary$PERIOD <- factor(crime_summary$PERIOD, levels = c("Before", "After"))

# Get all possible district numbers (assuming they're labeled like 1, 2, 3, ... 7)
all_wards <- sort(unique(crime_window$WARD))

# Create dynamic subtitle text
subtitle_text <- paste0(
  "Before/After the National Guard: ",
  format(start_date, "%b %d, %Y"), " to ", format(end_date, "%b %d, %Y"), ")"
)

ggplot(crime_summary, aes(x = as.factor(WARD), 
                          y = avg_daily_crimes, 
                          fill = PERIOD)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = 0.8)) +
  facet_wrap(~TYPE, scales = "free_y") +
  scale_x_discrete(limits = all_wards) +
  scale_fill_manual(values = c("Before" = "#1f77b4", 
                               "After" = "#ff7f0e")) +
  labs(
    title = "Average Daily Person vs Property Crime by Ward",
    subtitle = subtitle_text,
    x = "Ward",
    y = "Average Daily Crimes",
    fill = "Time Period"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12)
  )

##### maps!