# create logo

library(ggplot2)
library(dplyr)
library(hexSticker)

values <- c(
  0.30, 0.15, 0.45, 0.55, 0.35, 0.50, 0.32, 0.58, 0.10,
  -0.25, -0.30, -0.45, -0.90, -1.35, -1.55, -1.10, -0.75, -0.40, -0.25,
  0.28
)

threshold <- -0.8

df <- data.frame(x = seq_along(values), value = values) |>
  mutate(sign = case_when(
    value >= 0       ~ "pos",
    value < threshold ~ "extreme",
    TRUE              ~ "neg"
  ))

p <- ggplot(df, aes(x = x, y = value, fill = sign)) +
  geom_col(width = 0.6, color = NA) +
  geom_hline(yintercept = 0, color = "#2b2520", linewidth = 0.4) +
  geom_hline(yintercept = threshold, linetype = "dashed", color = "#2b2520", linewidth = 0.4) +
  scale_fill_manual(values = c(pos = "#3f6b54", neg = "#b35a2e", extreme = "#8c3d1c")) +
  theme_void() +
  theme(legend.position = "none")

sticker(
  subplot = p,
  s_x = 1, s_y = 0.75, s_width = 1.7, s_height = 1.0,
  package = "droughtevents",
  p_size = 13, p_color = "#2b2520", p_y = 1.45,
  h_size = .9, h_fill = "#f7f3ea", h_color = "#2b2520",
  filename  = "man/figures/logo.png"
)



