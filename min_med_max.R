library(tidyverse)

# carrega os dados
nperf4g <- read.csv("data/nperfres4g.csv")

# Tabela com mínimo, média, máximo e desvio padrão
nperf4g %>%
  select(SPEED_DOWNLOAD_AVG, NET_NAME, Period) %>% # seleciona as variáveis de interesse
  group_by(NET_NAME, Period) %>%  # informa por quais variáveis agrupar para o comando seguinte
  summarise(
    minimo = min(SPEED_DOWNLOAD_AVG), # o valor mínimo das observações
    media = mean(SPEED_DOWNLOAD_AVG, na.rm = TRUE), # o valor médio das observações
    maximo = max(SPEED_DOWNLOAD_AVG), # o valor máximo das observações
   desvio = sd(SPEED_DOWNLOAD_AVG, na.rm = TRUE)) # o desvio padrão


# Gráfico de pontos
nperf4g %>%
  select(SPEED_DOWNLOAD_AVG, NET_NAME, Period, WEATHER) %>%
  mutate(Period = fct_relevel(Period, "Morning", "Afternoon", "Evening")) %>%
  ggplot(mapping = aes(x = Period, y = SPEED_DOWNLOAD_AVG)) + # gráfico com o Period no eixo x, e velocidade no y
  geom_point( # plota pontos para cada observação
    aes(color = NET_NAME), # cada operadora com uma cor diferente
    position = "jitter") # espalha os pontos para não serem plotados um em cima do outro


# Gráfico boxplot
nperf4g %>%
  select(SPEED_DOWNLOAD_AVG, NET_NAME, Period, WEATHER) %>%
  mutate(Period = fct_relevel(Period, "Morning", "Afternoon", "Evening")) %>%
  ggplot(mapping = aes(x = Period, y = SPEED_DOWNLOAD_AVG)) + # gráfico com o Period no eixo x, e velocidade no y
  geom_boxplot(aes(color = NET_NAME))


# Gráfico boxplot com facetas
nperf4g %>%
  select(SPEED_DOWNLOAD_AVG, NET_NAME, Period, WEATHER) %>%
  mutate(Period = fct_relevel(Period, "Morning", "Afternoon", "Evening")) %>%
  ggplot(mapping = aes(x = Period, y = SPEED_DOWNLOAD_AVG)) + # gráfico com o Period no eixo x, e velocidade no y
  geom_boxplot(aes(color = NET_NAME)) +
  facet_wrap(~WEATHER)
