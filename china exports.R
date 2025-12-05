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

if (!require("ggrepel")) {
  install.packages("ggrepel")
  library(ggrepel)
}

if (!require("zoo")) {
  install.packages("zoo")
  library(zoo)
}


#########################################################################
# load data
df_exports <- read_excel("1 - input/china exports.xlsx", sheet = "master")

# make sure it is date
df_exports$date <- as.Date(df_exports$date)

# add 12 month sum
df_exports$annualized <- rollsum(df_exports$exports, k = 12, align = "right", fill = NA)
df_exports$annualized <- df_exports$annualized/10^6 # numbers are in million, convert to trillion





#########################################################################
# chart

# plot df
df_plot <- df_exports %>%
  filter(date >= as.Date("2018-01-01"))


# define 2025 range
start_2025 <- as.Date("2025-01-01")
end_2025   <- as.Date("2025-12-31")


# plot
ggplot(df_plot, aes(x = date, y = annualized)) +
  
  # shade 2025
  annotate("rect",
           xmin = start_2025, xmax = end_2025,
           ymin = -Inf, ymax = Inf,
           fill = "orange",
           alpha = 0.1) +
  
  scale_x_date(
    date_breaks = "1 year",      # tick every year
    date_labels = "%Y"           # show just the year
  ) +
  
  geom_line(color = "#40b3d2") +
  
  labs(
    title = "China shrugged off U.S. tariffs, for now",
    subtitle = "China's global exports in trillion US$, 12-month moving sum",
    y = NULL,
    x = NULL
  ) +

  theme_hc() + 
  
  theme(
    plot.title = element_text(
      face   = "bold",
      size   = 16,
      hjust  = 0.5,
      margin = margin(t = 15, b = 5),
      color = "#154c79"
    ),
    plot.subtitle = element_text(
      size   = 8,
      hjust  = 0.5,
      margin = margin( b = 8),
      color = "#154c79"
    ),
    axis.text = element_text(colour = "#154c79"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    axis.line.x        = element_line(color = "grey90", linewidth = 0.2),
  )


# save
ggsave("china exports.png", width = 7, height = 3.5, dpi = 1000, units = "in")

