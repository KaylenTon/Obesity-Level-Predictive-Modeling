library("dplyr")
library(ggplot2)
library(tidyverse)

o_df <- read.csv("ObesityData.csv")
head(o_df)
str(o_df)

o_df.num <- select_if(o_df, is.numeric)
o_df.chr <- select_if(o_df, is.character)

summary(o_df.num)
sapply(o_df.chr, table)

o_df.eda <- o_df.chr %>%
  pivot_longer(everything(), names_to = "col", values_to = "value") %>%
  group_by(col, value) %>%
  summarise(
    total = n(),
    .groups = "drop_last"
  ) %>%
  mutate(proportion = total/sum(total)) %>%
  ungroup()
