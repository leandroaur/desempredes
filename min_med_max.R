library(tidyverse)

nperf4g <- read.csv("data/nperfres4g.csv")

nperf4g %>% as_tibble() %>%
  select(SPEED_DOWNLOAD_AVG, NET_NAME, Period) %>%
  group_by(NET_NAME, Period) %>%
  summarise(
    minimo = min(SPEED_DOWNLOAD_AVG),
    media = mean(SPEED_DOWNLOAD_AVG, na.rm = TRUE),
    maximo = max(SPEED_DOWNLOAD_AVG),
   desvio = sd(SPEED_DOWNLOAD_AVG, na.rm = TRUE),
)
  

