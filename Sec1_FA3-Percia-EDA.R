## Loading the dataset

install.packages("ggplot2")
library(ggplot2)

data(diamonds)      
head(diamonds)    

#### NUM 1

p <- ggplot() + geom_histogram(aes(x = carat), data = diamonds)
p$layers

ggplot() + 
  layer(
    geom = "bar",  # histograms in ggplot2 use the "bar" geom with stat_bin
    stat = "bin",  # stat_bin is used for histograms
    position = "identity",
    data = diamonds,
    mapping = aes(x = carat),
    params = list(
      na.rm = FALSE,
      bins = 30,       # default bin count
      color = "black", # default outline color
      fill = "gray35"  # default fill color
    )
  ) +
  scale_x_continuous() +  # default x-axis scale
  scale_y_continuous()    # default y-axis scale


#### NUM 2

#Frequency Polygon
ggplot() +
  layer(
    geom = "line",      # Changed from "bar" to "line"
    stat = "bin",       # Still using stat_bin for binning
    position = "identity",
    data = diamonds,
    mapping = aes(x = carat, y = after_stat(count)),  # Explicitly map y to count
    params = list(
      na.rm = FALSE,
      bins = 30,
      color = "darkred",  # Line color
      linewidth = 1       # Line thickness
    )
  ) +
  labs(title = "Diamond Carat Distribution (Line)") +
  scale_x_continuous() +
  scale_y_continuous()

#Binned Scatterplot
ggplot() +
  layer(
    geom = "point",     # Changed from "bar" to "point"
    stat = "bin",       # Still binning
    position = "identity",
    data = diamonds,
    mapping = aes(x = carat, y = after_stat(count)),  # y = count per bin
    params = list(
      na.rm = FALSE,
      bins = 30,
      color = "blue",    # Point color
      size = 2           # Point size
    )
  ) +
  labs(title = "Diamond Carat Distribution (Points)") +
  scale_x_continuous() +
  scale_y_continuous()

# Line version (frequency polygon)
ggplot(diamonds, aes(x = carat)) + 
  geom_freqpoly(bins = 30, color = "darkred")

# Point version (binned scatter)
ggplot(diamonds, aes(x = carat)) + 
  stat_bin(aes(y = after_stat(count)), geom = "point", bins = 30, color = "blue")


#### NUM 3

# Stackbars
ggplot() +
  layer(
    geom = "bar",
    stat = "bin",
    position = "stack",  # Default for fill grouping
    data = diamonds,
    mapping = aes(
      x = carat,         # Binned by carat
      fill = clarity     # Fill color by clarity
    ),
    params = list(
      na.rm = FALSE,
      bins = 30,
      color = "black"    # Outline color of bars
    )
  ) +
  labs(
    title = "Diamond Carat Distribution by Clarity",
    x = "Carat",
    y = "Count"
  ) +
  scale_fill_brewer(palette = "Set2")  # Optional: Use a color palette

#Outline color
ggplot() +
  layer(
    geom = "bar",
    stat = "bin",
    position = "identity",  # Overlapping bars (use alpha for visibility)
    data = diamonds,
    mapping = aes(
      x = carat,
      color = cut          # Outline color by cut
    ),
    params = list(
      na.rm = FALSE,
      bins = 30,
      fill = "white",      # Fill bars with white
      alpha = 0.3          # Semi-transparent to see overlaps
    )
  ) +
  labs(
    title = "Diamond Carat Distribution by Cut",
    x = "Carat",
    y = "Count"
  ) +
  scale_color_brewer(palette = "Dark2")  # Outline color palette

# Stacked bars (fill = clarity)
ggplot(diamonds, aes(x = carat, fill = clarity)) +
  geom_histogram(bins = 30, color = "black")

# Outlined bars (color = cut)
ggplot(diamonds, aes(x = carat, color = cut)) +
  geom_histogram(bins = 30, fill = "white", alpha = 0.3)


#### NUM 4

# Histogram with `position = "dodge"`
ggplot() +
  layer(
    geom = "bar",
    stat = "bin",
    position = "dodge",  # Changed from "stack" to "dodge"
    data = diamonds,
    mapping = aes(
      x = carat,
      fill = clarity      # Fill by clarity (categorical)
    ),
    params = list(
      na.rm = FALSE,
      bins = 30,
      color = "black",    # Outline color
      width = 0.8        # Adjust bar width for dodging
    )
  ) +
  labs(
    title = "Diamond Carat Distribution by Clarity (Dodged Bars)",
    x = "Carat",
    y = "Count"
  ) +
  scale_fill_brewer(palette = "Set2")

ggplot(diamonds, aes(x = carat, fill = clarity)) +
  geom_histogram(
    bins = 30,
    position = "dodge",  # Side-by-side bars
    color = "black",
    width = 0.8
  )
