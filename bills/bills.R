library(readxl)
library(tidyverse)
library(ggbump)


# Read the first sheet by default
data <- read_excel("~/Desktop/envs/code/Legislation.xlsx")
data
yrs <- data%>%select(date)%>%mutate(year = year(date))%>%
  group_by(year)%>%
  summarize(peryear = n())

ggplot(data=yrs, aes(x=year, y=peryear)) +
  geom_col()+
  theme_minimal()

# BUMPPP
library(stringr)

df <- tribble(
  ~rank, ~`1980`, ~`1985`, ~`1990`, ~`1995`, ~`2000`, ~`2005`, ~`2010`, ~`2015`, ~`2020`, ~`2025`,
  1, "toxic", "control", "title vi", "oil", "control", "control", "water", "prevent", "reauthor", "control",
  2, "control", "air", "control", "toxic", "act", "water", "federal", "control", "nutrient", "tribe",
  3, "disease", "repeal", "revolve", "air", "amendments", "safe", "act", "protect", "clean", "engineer",
  4, "pollutant", "clean", "share", "control", "zone", "drink", "control", "oil", "safe", "indian",
  5, "concentration", "stat", "reclamation", "fresh", NA, "clean", "stance", "safe", "control", "state",
  6, "concentration of", "rodenticid", "pollutant", "contamination", NA, "quality", "toxic", "drink", "fungicide", "vehicle",
  7, "abatement", NA, "sources", "possible", NA, "act", "lake", "response", "rodenticide", "water",
  8, NA, NA, "strategies", "diver", NA, NA, "waste", "cited", "restore", "standard",
  9, NA, NA, "implementing", "threaten", NA, NA, "section", NA, NA, NA,
  10, NA, NA, "monitoring", "purposes", NA, NA, NA, NA, NA, NA
)

plot_df <- df %>%
  pivot_longer(-rank, names_to = "year", values_to = "term") %>%
  filter(!is.na(term), term != "") %>%
  mutate(
    year = as.integer(year),
    term = str_to_title(term)
  )

ggplot(plot_df, aes(x = year, y = rank, group = term, color = term)) +
  geom_bump(linewidth = 1.2, smooth = 8) +
  geom_point(size = 3) +
  geom_text(
    data = plot_df %>% filter(year == min(year)),
    aes(label = term),
    hjust = 1.1,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = plot_df %>% filter(year == max(year)),
    aes(label = term),
    hjust = -0.1,
    size = 3,
    show.legend = FALSE
  ) +
  scale_y_reverse(breaks = 1:10) +
  scale_x_continuous(
    breaks = sort(unique(plot_df$year)),
    expand = expansion(mult = c(0.18, 0.22))
  ) +
  labs(
    title = "Bump Chart of Pollution/Pollutant Terms Over Time",
    x = NULL,
    y = "Rank"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )





# ANOTHERERRRR

df <- data.frame(
  rank = 1:10,
  `1970` = c(
    "energyor", "materialsto", "byproduct", "especi", "componentor",
    "valuabl", "fossil", "impedi", "marketingof", "ore"
  ),
  `1985` = c(
    "ian", "spent", "fuelto", "fuelat", "titleto",
    "fuelin", "storageof", "reprocessingof", "power", "nuclea"
  ),
  `1990` = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  `1995` = c(
    "termin", "storag", "comm", "understand", "disposalof",
    "assistin", "expans", "elect", "havea", "dispos"
  ),
  check.names = FALSE
)

library(ggplot2)
library(ggbump)
library(dplyr)
library(tidyr)
library(stringr)

plot_df <- df %>%
  pivot_longer(-rank, names_to = "year", values_to = "term") %>%
  filter(!is.na(term), term != "") %>%
  mutate(
    year = as.integer(year),
    term = str_to_title(term)
  )

ggplot(plot_df, aes(x = year, y = rank, group = term, color = term)) +
  geom_bump(linewidth = 1.2, smooth = 8) +
  geom_point(size = 3) +
  geom_text(
    data = plot_df %>% filter(year == min(year)),
    aes(label = term),
    hjust = 1.1,
    size = 3,
    show.legend = FALSE
  ) +
  geom_text(
    data = plot_df %>% filter(year == max(year)),
    aes(label = term),
    hjust = -0.1,
    size = 3,
    show.legend = FALSE
  ) +
  scale_y_reverse(breaks = 1:10) +
  scale_x_continuous(
    breaks = sort(unique(plot_df$year)),
    expand = expansion(mult = c(0.2, 0.2))
  ) +
  labs(
    title = "Bump Chart of Nuclear Terms Over Time",
    x = NULL,
    y = "Rank"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )
