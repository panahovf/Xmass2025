# date: Dec 2025
# author: Farhad Panahov
# project: 12 Charts of Christmass - 2025





#########################################################################
# library
if(!require("dplyr")){
  install.packages("dplyr")
  library(dplyr)
}

if(!require("readxl")){
  install.packages("readxl")
  library(readxl)
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("ggthemes")) {
  install.packages("ggthemes")
  library(ggthemes)
}

if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}





#########################################################################
# load data
df_mariage <- read_excel("1 - input/mariage.xlsx", sheet = "chart")





#########################################################################
# set variables for later and transform data

# for correcting order in X axis
age_levels <- c("Early 20s","Late 20s","Early 30s","Late 30s","Early 40s")

# for setting order in legend
cohort_order <- c(
  "Older Millennials (1981–85)",
  "Later Millennials (1986–90)",
  "Youngest Millennials (1991–95)",
  "Early Gen Z (1996–00)"
)

# pivot longer
df_long <- df_mariage %>%
  pivot_longer(
    cols = -`Born/Age`,
    names_to = "age_group",
    values_to = "pct"
  ) %>%
  mutate(
    age_group = stringr::str_squish(age_group),
    age_group = factor(age_group, levels = age_levels),

    # robust percent -> share
    pct = readr::parse_number(as.character(pct)),
    pct = dplyr::if_else(pct > 1, pct/100, pct)
  )





#########################################################################
# chart

# colors
born_cols <- c(
  "Older Millennials (1981–85)" = "#0B3C5D",
  "Later Millennials (1986–90)" = "#00b4d8",
  "Youngest Millennials (1991–95)" = "#6C757D",
  "Early Gen Z (1996–00)" = "#E9C46A"
)



# plot
ggplot(df_long, aes(x = age_group, y = pct, color = `Born/Age`, group = `Born/Age`)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  geom_point(size = 2.2, na.rm = TRUE) +
  scale_color_manual(values = born_cols) +
 
  # aligns 0 line with X axis
  scale_y_continuous(
    limits = c(0, 0.8),
    # no gap at bottom; a little extra space at the top
    expand = expansion(mult = c(0, 0.06)),
    labels = function(x) paste0(x*100, "%")
  ) +
  
  labs(
    title = "Younger generations of Canadians\nare delaying long-term partnerships",
    subtitle = "Share of Canadians married or in common-law relationship\nby generation and age stage",
    x = NULL, 
    y = NULL, 
  ) +
  
  # legend text and order
  scale_color_manual(
    name = "Generation:",
    values = born_cols,
    breaks = cohort_order
  ) +

  theme_hc() +
  
  theme(
    # space around the whole plot (top, right, bottom, left)
    plot.margin = margin(t = 18, r = 18, b = 18, l = 18),

    # space between title and the plot panel
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5,
      color = "#154c79",
      margin = margin(t = 8, b = 14)   # <- b controls gap to panel
    ),

    plot.subtitle = element_text(
      size   = 12,
      hjust  = 0.5,
      margin = margin( b = 8),
      color = "#154c79"
    ),

    # Legend inside plot (right side)
    legend.position = c(1, 0.3),
    legend.justification = c(1, 0.5),
    legend.text = element_text(size = 12),
    legend.key = element_rect(fill = NA, color = NA),
    legend.title = element_text(size = 12, color = "#154c79", face = "bold"),
    legend.box.background = element_rect(
      fill = NA,
      color = "#154c79",
      linewidth = 0.8
    ),

    axis.text = element_text(colour = "#154c79"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    axis.line.x        = element_line(color = "grey90", linewidth = 0.2)
  )



# save
ggsave("marriage by age.png", width = 7, height = 5, dpi = 1000, units = "in")

