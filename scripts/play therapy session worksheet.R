# Play Therapy Session Tracking Worksheet (Letter, narrow margins)
# Produces: Play_Therapy_Session_Tracking_Worksheet.pdf
# Run this script in R (no extra packages required).

pdf_file <- "/Users/matthewdanna/Downloads/Play_Therapy_Session_Tracking_Worksheet.pdf"
pdf(pdf_file, width = 8.5, height = 11)

# constants (in inches)
page_w <- 8.5
page_h <- 11
margin <- 0.5          # narrow margins (0.5 inch)
left_x <- margin
right_x <- page_w - margin
usable_w <- right_x - left_x

# vertical layout parameters (in inches)
y_top <- page_h - margin       # start near top
line_h <- 0.28                 # single-line vertical spacing
small_gap <- 0.12
rule_inset <- 1.6              # how much offset from left for rules (so label sits left of rule)

# helper functions using base grid
library(grid)

# draw a left-justified label and a ruled line to the right
draw_field <- function(label, y, rule_length = usable_w - rule_inset, rule_height = 0.01, label_cex = 0.9) {
  # text label
  grid.text(label,
            x = unit(left_x, "inches"),
            y = unit(y, "inches"),
            just = "left",
            gp = gpar(fontfamily = "Helvetica", fontsize = 11 * label_cex))
  # horizontal rule (thin line)
  x_start <- left_x + rule_inset
  x_end <- x_start + rule_length
  grid.lines(x = unit(c(x_start, x_end), "inches"),
             y = unit(c(y - rule_height, y - rule_height), "inches"),
             gp = gpar(lwd = 1))
}

# draw a block of N ruled lines (for narrative)
draw_block <- function(n_lines, y_top_line, line_spacing = 0.22, rule_length = usable_w - rule_inset) {
  ys <- seq(y_top_line, by = -line_spacing, length.out = n_lines)
  for (y in ys) {
    x_start <- left_x + rule_inset
    x_end <- x_start + rule_length
    grid.lines(x = unit(c(x_start, x_end), "inches"),
               y = unit(c(y, y), "inches"),
               gp = gpar(lwd = 1))
  }
}

# draw a small checkbox at given (x,y)
draw_checkbox <- function(x, y, size = 0.18) {
  grid.rect(x = unit(x, "inches"),
            y = unit(y, "inches"),
            width = unit(size, "inches"),
            height = unit(size, "inches"),
            gp = gpar(lwd = 1),
            just = "center")
}

# new page and start drawing
grid.newpage()

# Title
title_y <- y_top - 0.15
grid.text("Play Therapy Session Tracking Worksheet",
          x = unit(page_w/2, "inches"),
          y = unit(title_y, "inches"),
          gp = gpar(fontface = "bold", fontsize = 16, fontfamily = "Helvetica"))

cursor_y <- title_y - 0.5

# Section function to draw headers
draw_section_header <- function(text, y) {
  grid.text(text,
            x = unit(left_x, "inches"),
            y = unit(y, "inches"),
            just = "left",
            gp = gpar(fontface = "bold", fontsize = 12, fontfamily = "Helvetica"))
}

# Client Information
draw_section_header("Client Information", cursor_y)
cursor_y <- cursor_y - 0.25

fields <- c("Client Name:", "Date:", "Therapist:", "Diagnosis:", "Treatment Goal(s):")
for (f in fields) {
  draw_field(paste0(f), cursor_y)
  cursor_y <- cursor_y - line_h
}
cursor_y <- cursor_y - small_gap

# Session Overview
draw_section_header("Session Overview", cursor_y)
cursor_y <- cursor_y - 0.25
overview_fields <- c("Session Number:", "Length of Session:", "Type of Play Therapy:", "Setting/Materials Used:")
for (f in overview_fields) {
  draw_field(f, cursor_y)
  cursor_y <- cursor_y - line_h
}
cursor_y <- cursor_y - small_gap

# Observations of Play (narrative blocks)
draw_section_header("Observations of Play", cursor_y)
cursor_y <- cursor_y - 0.25

obs_items <- list(
  "Play Themes:" = 3,
  "Play Patterns:" = 3,
  "Role Enactments:" = 3,
  "Use of Space & Materials:" = 2,
  "Interaction with Therapist:" = 3
)

for (name in names(obs_items)) {
  nlines <- obs_items[[name]]
  # label
  grid.text(name,
            x = unit(left_x, "inches"),
            y = unit(cursor_y, "inches"),
            just = "left",
            gp = gpar(fontfamily = "Helvetica", fontsize = 11))
  # block of rules starting a bit below the label
  block_top_y <- cursor_y - 0.18
  draw_block(nlines, block_top_y, line_spacing = 0.22)
  # update cursor: move down by number of lines * spacing + small gap
  cursor_y <- block_top_y - (nlines - 1) * 0.22 - 0.28
}
cursor_y <- cursor_y - small_gap

# Emotional Content
draw_section_header("Emotional Content", cursor_y)
cursor_y <- cursor_y - 0.25

# We'll put emotion indicators as labeled short lines
emotion_labels <- c(
  "Emotion Indicators (verbal, behavioral, symbolic) / Intensity (1–5):",
  "Joy / Excitement:",
  "Anger / Frustration:",
  "Fear / Anxiety:",
  "Sadness / Grief:",
  "Nurturance / Care:",
  "Other:"
)

# first a descriptive line (no rule)
grid.text(emotion_labels[1],
          x = unit(left_x, "inches"),
          y = unit(cursor_y, "inches"),
          just = "left",
          gp = gpar(fontfamily = "Helvetica", fontsize = 10))
cursor_y <- cursor_y - 0.22

for (lbl in emotion_labels[-1]) {
  draw_field(lbl, cursor_y)
  cursor_y <- cursor_y - line_h
}
cursor_y <- cursor_y - small_gap

# Therapeutic Themes & Interpretation (narrative)
draw_section_header("Therapeutic Themes & Interpretation", cursor_y)
cursor_y <- cursor_y - 0.25

grid.text("Emerging or Recurrent Themes:",
          x = unit(left_x, "inches"),
          y = unit(cursor_y, "inches"),
          just = "left",
          gp = gpar(fontfamily = "Helvetica", fontsize = 11))
block_top_y <- cursor_y - 0.18
draw_block(3, block_top_y, line_spacing = 0.22)
cursor_y <- block_top_y - (3 - 1) * 0.22 - 0.28

grid.text("Possible Meanings / Client Needs:",
          x = unit(left_x, "inches"),
          y = unit(cursor_y, "inches"),
          just = "left",
          gp = gpar(fontfamily = "Helvetica", fontsize = 11))
block_top_y <- cursor_y - 0.18
draw_block(3, block_top_y, line_spacing = 0.22)
cursor_y <- block_top_y - (3 - 1) * 0.22 - 0.28

cursor_y <- cursor_y - small_gap

# Interventions
draw_section_header("Therapist Interventions & Responses", cursor_y)
cursor_y <- cursor_y - 0.25

grid.text("Techniques Used:",
          x = unit(left_x, "inches"),
          y = unit(cursor_y, "inches"),
          just = "left",
          gp = gpar(fontfamily = "Helvetica", fontsize = 11))
block_top_y <- cursor_y - 0.18
draw_block(3, block_top_y, line_spacing = 0.22)
cursor_y <- block_top_y - (3 - 1) * 0.22 - 0.28

grid.text("Client's Response:",
          x = unit(left_x, "inches"),
          y = unit(cursor_y, "inches"),
          just = "left",
          gp = gpar(fontfamily = "Helvetica", fontsize = 11))
block_top_y <- cursor_y - 0.18
draw_block(3, block_top_y, line_spacing = 0.22)
cursor_y <- block_top_y - (3 - 1) * 0.22 - 0.28

cursor_y <- cursor_y - small_gap

# Progress toward goals with checkboxes (draw real boxes)
draw_section_header("Progress Toward Treatment Goals", cursor_y)
cursor_y <- cursor_y - 0.25

# Define checkboxes with labels horizontally
cb_labels <- c("Progressing", "No Change", "Regression", "New Goal Identified")
cb_spacing <- 1.7  # inches between checkbox groups
start_x <- left_x + 0.05
for (i in seq_along(cb_labels)) {
  x_pos <- start_x + (i - 1) * cb_spacing
  draw_checkbox(x_pos, cursor_y, size = 0.18)
  grid.text(cb_labels[i],
            x = unit(x_pos + 0.26, "inches"),
            y = unit(cursor_y, "inches"),
            just = "left",
            gp = gpar(fontfamily = "Helvetica", fontsize = 10))
}
cursor_y <- cursor_y - 0.5

grid.text("Notes on Progress / Next Steps:",
          x = unit(left_x, "inches"),
          y = unit(cursor_y, "inches"),
          just = "left",
          gp = gpar(fontfamily = "Helvetica", fontsize = 11))
block_top_y <- cursor_y - 0.18
draw_block(4, block_top_y, line_spacing = 0.22)
cursor_y <- block_top_y - (4 - 1) * 0.22 - 0.4

# Footer signature (Therapist only) near bottom margin
#sig_y <- margin + 0.8   # 0.8 inches above bottom margin
# signature line
#sig_line_left <- left_x
#sig_line_right <- left_x + 3.2
#grid.lines(x = unit(c(sig_line_left, sig_line_right), "inches"),
#           y = unit(c(sig_y, sig_y), "inches"),
#           gp = gpar(lwd = 1))
#grid.text("Therapist Signature:",
#          x = unit(sig_line_left - 0.02, "inches"),
#          y = unit(sig_y + 0.12, "inches"),
#          just = "right",
#          gp = gpar(fontfamily = "Helvetica", fontsize = 10))

# date line to the right of signature
#date_line_left <- sig_line_right + 1.2
#date_line_right <- date_line_left + 1.6
#grid.lines(x = unit(c(date_line_left, date_line_right), "inches"),
#           y = unit(c(sig_y, sig_y), "inches"),
#           gp = gpar(lwd = 1))
#grid.text("Date:",
#          x = unit(date_line_left - 0.06, "inches"),
#          y = unit(sig_y + 0.12, "inches"),
#          just = "right",
#          gp = gpar(fontfamily = "Helvetica", fontsize = 10))

# small footer note (page info)
#grid.text("Play Therapy Session Tracking Worksheet — printable",
#          x = unit(page_w/2, "inches"),
#          y = unit(margin - 0.15, "inches"),
#          gp = gpar(fontsize = 8, fontfamily = "Helvetica"))

dev.off()

cat("✅ PDF created:", pdf_file, "\n")