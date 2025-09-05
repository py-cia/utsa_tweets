library(tidyverse)

# Color Game from Sickos Committee

# Reddit List -------------------------------------------------------------
# Gonna do Go5: Boise State, UTSA, Bowling Green, Sam Houston 
# Boise State: D64309, UTSA:F15A22, Bowling Green:FD5000, Sam Houston: F56423

orange_df <- data.frame(school = c("Boise State", "UTSA", "Bowling Green", "Sam Houston"),
           color = c("#D64309", "#F15A22", "#FD5000", "#F56423"))

colors <- orange_df$color
labels <- c("A", "B", "C", "D")

df <- data.frame(
  x = seq_along(colors),
  label = labels,
  color = colors
)

ggplot(df, aes(x = x, y = 1, fill = color, label = label)) +
  geom_tile(width = 1, height = 0.5) +
  geom_text(color = "white", size = 6, fontface = "bold") +
  scale_fill_identity() +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))

# Answer: A: Boise State, B: UTSA, C: Bowling Green, D: Sam Houston






