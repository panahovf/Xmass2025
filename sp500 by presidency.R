# date: Dec 2025
# author: Farhad Panahov
# project: 12 Charts of Christmass - 2025





#########################################################################
# library
if(!require("dplyr")){
  install.packages("dplyr")
  library(dplyr)
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



#########################################################################
# load data
df_spx <- read.csv("1 - input/S&P 500 Historical Data.csv")

df_spx <- (df_spx %>% 
  mutate(
  date = as.Date(as.character(Date), format = "%m/%d/%Y"),
  price = as.numeric(gsub(",", "", Price))
 ) %>%
  select(date, price) %>%  
  filter(date >= "1981/01/18") %>%
  arrange(date)
)



# https://www.presidentsusa.net/presvplist.html
df_presidents <- data.frame( term = c("Reagan 1", "Reagan 2","G. Bush","Clinton 1", "Clinton 2",
    "GW. Bush 1", "GW. Bush 2", "Obama 1", "Obama 2", "Trump 1", "Biden", "Trump 2"),
  start_year = c(1981, 1985, 1989, 1993, 1997, 2001, 2005, 2009, 2013, 2017, 2021, 2025)
)

df_presidents <- df_presidents %>% arrange(start_year)





#########################################################################
# merging datasets

# add actual staring date Jan 20th
df_presidents <- (df_presidents %>% 
  arrange(start_year) %>% 
  mutate(
    target_date = as.Date(paste0(start_year, "-01-20"))
  )
)

# add empty column for start date
df_presidents$start_date <- as.Date(NA)

# find best match date for between weekly data and Jan 20th
for(i in 1:nrow(df_presidents)) {
  target <- df_presidents$target_date[i] # get date
  
  tmp <- df_spx # temp database
  tmp$diff_days <- abs(as.numeric(tmp$date - target)) # get difference
  
  closest_index <- which.min(tmp$diff_days) # find closest date index
  closest_date  <- tmp$date[closest_index] # get the closest date based on index
  
  df_presidents$start_date[i] <- closest_date # add to the dataframe
}

rm(i, target, closest_date, closest_index, tmp)

# get index of the dates between the start dates
idx <- findInterval(df_spx$date, df_presidents$start_date)

# get the presidents based on the interval
df_spx$term <- df_presidents$term[idx]
rm(idx)





#########################################################################
# series by presidents

# group by president and index to 100
df_spx_indexed <- (df_spx %>%
  arrange(term, date) %>%         # sort by president, then date
  group_by(term) %>%
  mutate(
    week  = row_number(),              # 1, 2, 3,... within each term
    index = price / first(price) * 100 # start week = 100
  ) %>%
  ungroup()
)


# ranking
df_ranking_ytd <- df_spx_indexed %>% filter(week == 46)





#########################################################################
# plot
df_plot <- df_spx_indexed


# specify terms --- adds predidency and term type (1,2) for plotting solid vs dashed lines
df_plot <- df_plot %>%
  mutate(
    president = case_when(
      grepl("Trump", term)      ~ "Trump",
      grepl("Obama", term)      ~ "Obama",
      grepl("Biden", term)      ~ "Biden",
      grepl("GW. Bush", term)   ~ "GW. Bush",
      grepl("G. Bush", term)    ~ "G. Bush",
      grepl("Clinton", term)    ~ "Clinton",
      grepl("Reagan", term)     ~ "Reagan",
    ),
    term_type = ifelse(grepl(" 2$", term), "Second", "First")
  )


# Colours by president
pres_colors <- c(
  "Trump"   = "#8B1A1A",
  "Obama"   = "#08306b",  # dark blue
  "Biden"   = "#4292c6",  # light blue
  "GW. Bush"= "#238b45",  # green
  "G. Bush" = "#ff7f00",  # orange
  "Clinton" = "#6a3d9a",  # purple
  "Reagan"  = "grey40"
)


# set labels
labels_df <- df_plot %>%
  group_by(term) %>%
  filter(week == max(week)) %>%
  ungroup()


# plot
ggplot(df_plot,
       aes(x = week, y = index,
           colour = president,
           linetype = term_type,
           group = term)) +
  
  geom_hline(yintercept = 100, linetype = "dashed", colour = "black") +
  
  # all terms: very thin lines
  geom_line(size = 0.1) +
  
  # highlight Trump 2: thicker on top
  geom_line(
    data = subset(df_plot, term == "Trump 2"),
    aes(x = week, y = index),
    size = 1
  ) +
  
  # labels for all terms, repelled so they don't overlap
  ggrepel::geom_text_repel(
    data = labels_df,
    aes(label = term),
    hjust = 0,
    nudge_x = 5,
    direction = "y",
    segment.color = NA,
    show.legend = FALSE,
    size = ifelse(labels_df$term == "Trump 2", 4, 3),
    fontface = ifelse(labels_df$term == "Trump 2", "bold", "plain"),
    max.overlaps = Inf
  ) +
  
  # colours: no legend (labels + colours are enough)
  scale_colour_manual(values = pres_colors, guide = "none") +
  
  # show legend only for line type: first vs second term
  scale_linetype_manual(
  values = c("First" = "solid", "Second" = "dashed"),  # simpler, more visible
  name   = "Term:",
  guide  = guide_legend(override.aes = list(size = 5, linewidth = 0.5))  # thin strokes
  )  +
  
  coord_cartesian(xlim = c(1, max(labels_df$week) + 25)) +
  
  # axis labels and title
  labs(
    x = "Week of presidency",
    y = "Indexed, start of term = 100",
    title = "S&P 500 Performance by Presidency"
  ) +
  
  # theme & item configurations
  theme_hc() +
  
  theme(
    plot.title = element_text(
      face   = "bold",
      size   = 16,
      hjust  = 0.5,
      margin = margin(b = 8)
    ),
    axis.title.x = element_text(
      size   = 8,
      margin = margin(t = 8)
    ),
    axis.title.y = element_text(
      size   = 8,
      margin = margin(r = 8)
    ),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    axis.line.x        = element_line(color = "grey90", linewidth = 0.2),
    legend.position    = c(0.1, 0.8),
    legend.title = element_text(size = 8),  # title font size
  legend.text  = element_text(size = 6)    # item font size
  )


# save
ggsave("spx_presidents.png", width = 7, height = 3.5, dpi = 1000, units = "in")





