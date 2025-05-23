library(tidyr)
library(dplyr)

#### NUMBER 1

cytof_data <- read.csv("C:/Users/john/Downloads/cytof_one_experiment.csv")

long_cytof_data <- cytof_data %>%
  pivot_longer(
    cols = everything(),           # Pivot all columns
    names_to = "Protein",          # New column for protein names
    values_to = "Amount"           # New column for protein amounts
  )

dim(long_cytof_data)


#### NUMBER 2

library(dplyr)

protein_summary <- long_cytof_data %>%
  group_by(Protein) %>%
  summarise(
    Median = median(Amount, na.rm = TRUE),
    MAD = mad(Amount, na.rm = TRUE)
  )

print(protein_summary)


#### NUMBER 3

library(ggplot2)

ggplot(protein_summary, aes(x = MAD, y = Median, label = Protein)) +
  geom_point(color = "steelblue", size = 3) +  # Optional: adds labels to points
  theme_minimal() +
  labs(
    title = "Spread-Location (s-l) Plot of Protein Expression",
    x = "Median Absolute Deviation (MAD)",
    y = "Median Expression Level"
  )


#### NEXT

install.packages("remotes")
remotes::install_github("dcl-docs/dcldata")

library(dcldata)

data(example_gymnastics_2)


#### NUMBER 4

library(tidyverse)

gymnastics_long <- example_gymnastics_2 %>%
  pivot_longer(
    cols = -country,                # Pivot all columns except 'country'
    names_to = "event_year",        # New column to hold event and year info
    values_to = "score"             # Values go into 'score' column
  ) %>%
  separate(
    col = event_year,
    into = c("event", "year"),      # Split into 'event' and 'year'
    sep = "_"                       # Use underscore as the separator
  ) %>%
  mutate(year = as.integer(year))   # Convert 'year' from character to integer

head(gymnastics_long)
