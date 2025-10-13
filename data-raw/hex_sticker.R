library(cropcircles)
# library(magick)
library(tibble)
library(ggplot2)
library(ggpath)
# library(ggtext)
library(showtext)
# library(glue)

# choose a font from Google Fonts
font_add_google("Single Day", "singleday")
showtext_auto()

# Scatterplot
set.seed(1)
n <- 10
df <- tibble(
  x = c(runif(n, 0, 1), 0.2),
  y = c(x[seq(n)] + 3 * rnorm(n, 0, 0.04), 1.1)
)

df |>
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 10, col = "white") +
  labs(x = "", y = "") +
  xlim(0, 1.2) +
  ylim(-0.3, 1.5) +
  theme_void() +
  coord_fixed()

hex_scatter <- tempfile(fileext = ".png")
ggsave(hex_scatter, height = 6, width = 6)

img_cropped <- hex_crop(
  images = hex_scatter,
  bg_fill = "#236d7a",
  border_colour = "#44d6ec",
  border_size = 72
)

ggplot() +
  geom_from_path(aes(0.5, 0.5, path = img_cropped)) +
  annotate(
    "text",
    x = 0.48,
    y = 0.08,
    label = "weird",
    family = "singleday",
    size = 96,
    colour = "white",
    angle = 30,
    hjust = 0,
    fontface = "bold"
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  coord_fixed()

ggsave("./man/figures/weird-hex.png", height = 6, width = 6)
