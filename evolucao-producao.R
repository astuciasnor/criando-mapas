# 1) Preparação e carregamento de limites poligonais dos municipios -------------------------
library(geobr)# Esse pacote acessa os dados dos limites dos estado sem a necessidade de ir no site IBGE
library(sf)     # sf: simple feature


# acessa os limites terrotoriais de qualquer municipio brasileiro ou estado
lim.estados = read_state(code_state = 'all') # Acessa os limites de estados
class(lim.estados) # Esses dados, al?m de ter os limites, tem caracter?sicas, como popula??o, etc


# 3) Acessar dados da Revista PeixeBr 2023 ---------------------------
# Filtrar os dados pelo ano de 2023 de aqui_bra_23
library(readxl)
library(dplyr)
# Fonte dos dados: https://www.peixebr.com.br/


aqui_bra <- read_excel("dados/tabela_geral_estados.xlsx", sheet = 2) 
head(aqui_bra)
tail(aqui_bra)


# 4) Vamos juntar dados (merge) -------------------------------------------
# Precisamos ter uma coluna com mesmo conteúdo nos 2 conjuntos de dados
aqui_bra <- aqui_bra %>% rename(abbrev_state = Estado) # Renomeia a coluna 

# Juntando as informações também com o pacote dplyr
juntos <- inner_join(lim.estados, aqui_bra, by = "abbrev_state")
# Junta só as linhas onde há corrrespondencia exat entre as duas tabelas para a coluna

juntos %>%
  filter(name_region == "Norte") %>%
  ggplot(aes(x = Ano, y = `Producao(t)`, group = abbrev_state, colour = abbrev_state )) +
  geom_point() +
  geom_line() +
  labs(colour =  "Estado", x = NULL) 

juntos %>%
  filter(name_region == "Sul") %>%
  ggplot(aes(x = Ano, y = `Producao(t)`, group = abbrev_state, colour = abbrev_state )) +
  geom_point() +
  geom_line() +
  labs(colour =  "Estado", x = NULL) 




