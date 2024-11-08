# https://medium.com/number-around-us/table-it-like-a-pro-print-ready-tables-in-r-ff1856611008

#####
#####
#####

library(knitr)

# Basic table in Markdown
kable(head(mtcars), format = "markdown")

#####
#####
#####

---
  title: "My PDF Report"
output: pdf_document
---
  
  ```{r}
library(knitr)
kable(head(mtcars), format = "latex")
```

#####
#####
#####

library(officer)
library(flextable)

doc <- read_docx()

# Add a title and table
doc <- body_add_par(doc, "Table of Cars", style = "heading 1")
doc <- body_add_flextable(doc, flextable(head(mtcars)))

print(doc, target = "my_report.docx")

#####
#####
#####

ibrary(openxlsx)
library(ggplot2)

# Taking a sample of the diamonds dataset for demo
diamonds_sample <- diamonds[sample(nrow(diamonds), 100), ]

# Writing the data to an Excel file
write.xlsx(diamonds_sample, "diamonds_sample.xlsx", sheetName = "Diamonds Sample")

#####
#####
#####

# Create a new workbook
wb <- createWorkbook()

# Add two worksheets
addWorksheet(wb, "Diamonds Data")
addWorksheet(wb, "Summary")

# Write the diamonds sample data to the first sheet
writeData(wb, sheet = "Diamonds Data", diamonds_sample)

# Create a summary of the diamonds dataset
summary_data <- data.frame(
  Mean_Price = mean(diamonds_sample$price),
  Median_Carat = median(diamonds_sample$carat),
  Max_Price = max(diamonds_sample$price)
)

# Write the summary to the second sheet
writeData(wb, sheet = "Summary", summary_data)

# Apply styling to the header of the Diamonds Data sheet
headerStyle <- createStyle(textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#4F81BD")
addStyle(wb, sheet = "Diamonds Data", style = headerStyle, rows = 1, cols = 1:ncol(diamonds_sample), gridExpand = TRUE)

# Save the workbook
saveWorkbook(wb, "styled_diamonds_report.xlsx", overwrite = TRUE)

#####
#####
#####

library(gt)
library(ggplot2)

# Take a small sample of the diamonds dataset for our table
diamonds_sample <- diamonds[sample(nrow(diamonds), 5), ]

# Create a basic gt table
gt_table <- gt(diamonds_sample) %>%
  tab_header(
    title = "Diamonds Data Sample",
    subtitle = "A snapshot of key attributes"
  ) %>%
  fmt_number(
    columns = c(carat, price),
    decimals = 2
  ) %>%
  cols_label(
    carat = "Carat Weight",
    cut = "Cut Quality",
    color = "Diamond Color",
    clarity = "Clarity Rating",
    price = "Price (USD)"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_footnote(
    footnote = "Data from ggplot2's diamonds dataset.",
    locations = cells_title(groups = "title")
  )

# Print the table
gt_table

#####
#####
#####

# Save the table as an HTML file
gtsave(gt_table, "diamonds_table.html")

# Or save it as a PNG image
gtsave(gt_table, "diamonds_table.png")e

#####
#####
#####


