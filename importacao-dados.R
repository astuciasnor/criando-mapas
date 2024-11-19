library(tidyverse)
library(readxl)

aquicultura <- read_excel("dados/tabela_geral_estados.xlsx", sheet = 2) |> 
  filter(Ano == 2023) # Filtra pelo ano desejado

aquicultura |> 
  ggplot(aes(x = Estado, y = `Producao(t)`, fill = Estado)) +
  geom_col(show.legend = F)
