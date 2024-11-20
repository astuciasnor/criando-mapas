#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxXXXXXXXXXXXXXXXXX
#
#                          CRIAÇÃO DE MAPAS COM O PACOTE GEOBR- aqui_bra_2319
#
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Criação de mapa do Brasil com produção da aquiculutra

# 1) Preparação e carregamento de limites poligonais dos municipios -------------------------
  library(geobr) # Esse pacote acessa os dados dos limites dos estado sem a necessidade de ir no site IBGE
  library(sf)     # sf: simple feature

# acessa os limites terrotoriais de qualquer municipio brasileiro ou estado
  lim.estados = read_state(code_state = 'all') # Acessa os limites de estados
  class(lim.estados) # Esses dados, al?m de ter os limites, tem caracter?sicas, como popula??o, etc
  lim_PA <- read_state(code_state = "PA")

# 2) Criando o primeiro mapa simples --------------------------
  library(ggplot2)
  ggplot(lim.estados) + # informa qual conjunto de dados o ggplot usará (dados geoespaciais)
    geom_sf()   # informa o tipo de função gráfica sobre os tipos de dados (sf: spatial features) 
  
  ggplot(lim_PA) + 
    geom_sf()
  
# Vamos colorir de acordo com o codigos dos estados
  ggplot(lim.estados) + 
    geom_sf(aes(fill=code_state))
  

# 3) Acessar dados da Revista PeixeBr 2023 ---------------------------
# Filtrar os dados pelo ano de 2023 de aqui_bra_23
  library(readxl)
  library(dplyr)
# Fonte dos dados: link da peixe-br
 
  
  aqui_bra_23 <- read_excel("dados/tabela_geral_estados.xlsx", sheet = 2) |> 
    filter(Ano == 2023) # Filtra pelo ano desejado
  str(aqui_bra_23)
  head(aqui_bra_23)
  tail(aqui_bra_23)
  

# 4) Vamos juntar dados (merge) -------------------------------------------
# Precisamos ter uma coluna com mesmo conteúdo nos 2 conjuntos de dados
  aqui_bra_23 <- aqui_bra_23 %>% rename(abbrev_state = Estado) # Renomeia a coluna 

# Juntando as informações também com o pacote dplyr
  juntos <- inner_join(lim.estados, aqui_bra_23, by = "abbrev_state")
  # Junta só as linhas onde há corrrespondencia exat entre as duas tabelas para a coluna "abbrev-state"
  
# 5) Fazendo gráfico (mapa) com a informações de aqui_bra_23 -----------------------------
  ggplot(juntos) + geom_sf(aes(fill = abbrev_state))   # Usa dados categóricos
  ggplot(juntos) + geom_sf(aes(fill = `Producao(t)`)) # Usa os dados numéricos

# 6) Criando intervalo de classe dos CASOS ACUMULADOS -----------------------------
  
  # Categorizando os dados:
  categoria <- cut(juntos$`Producao(t)`,  # Dados originais
                  breaks = c(0, 5000, 15000, 30000, 70000, Inf), # Defindo os breakpoints
                  labels = c("1 - 5000", 
                             "5001 - 15.000", 
                             "15.001 - 30.000", 
                             "30.001 - 70.000", 
                             "70.001 - Inf" ),
                  include.lowest = TRUE,
                  right = FALSE)
 
  
  # Colocando as faixas de categorias no conjuntos de dados chamados juntos
  juntos$categoria = categoria
  head(juntos)
  
  # 7) Plotando agora pela coluna categoria -----------------------------------
  ggplot(juntos) + geom_sf(aes(fill = categoria))

# Modificando com uma paleta de cores manualmente
  library(RColorBrewer) # procure no google a faixa desejadaRColorBrewer
  # display.brewer.all()
  display.brewer.pal(n=9, name = "Reds")
  cor = brewer.pal(9, 'Reds')
  
  gg = ggplot(juntos) + geom_sf(aes(fill = categoria)) + 
    scale_fill_manual(values = c("#FFFFE5", "#FFF7BC", "#FEE391",
                                 "#FE9929", "#CC4C02", "#662506")) 
    # scale_fill_manual(values = c("#FEE0D2","#FCBBA1", "#FC9272", "#EF3B2C",
    #                              "#A50F15", "#67000D"))
  
  gg
# 8) Colocando a escala e a orientação do mapa ------------------------------
  library(ggspatial)
  # colocando a escala
  gg + annotation_scale(location = 'br', height = unit(0.2, "cm"))    # br: bottompright
  
  # adicionando a orientação do mapa
  gg + annotation_scale(location = 'br', height = unit(0.2, "cm")) +  # tr: toppright
    annotation_north_arrow(location = 'tr')
  
  # Mudando o estilo da seta de orientação:
  gg + annotation_scale(location = 'br', height = unit(0.2, "cm")) +  
    annotation_north_arrow(location = 'tr', style = north_arrow_nautical())
  
  gg <-  gg + annotation_scale(location = 'br', height = unit(0.2, "cm")) +    
    annotation_north_arrow(location = 'tr', style = north_arrow_nautical(), 
                           width = unit(1.5, 'cm'), height = unit(1.5, 'cm'))
  gg
  
# 9) Colocando uma imagem no mapa -----------------------------------
  library(ggimage)
  gg + geom_image(aes(-35,-27), image = "images/coronavirus.png", size = 0.2)
                 
    
# 10) Coloando título e mudando tema ----------------------------------------
  meu.plot <- gg + geom_image(aes(-35,-27), image = "images/coronavirus.png", size = 0.2) +
                   labs(title = "Produção Aquicola do Brasil",
                        subtitle = "Ano de 2023",
                        fill = "Produção (ton)", #Muda titulo da legenda
                        x = NULL, y = NULL) +
                   theme_bw()+
              theme(legend.position = c(0.18, 0.2),
                    legend.key.height = unit(5, "mm"))
  meu.plot

  # 11) Salvando -----------------------------------------
  # Salvando o que é exibido na janela gráfica:
  ggsave(meu.plot, filename = "mapa_aqui_bra_23.png")
  
  # Especificando dimensões  e a qualidade da imagem:
  ggsave(meu.plot, 
         filename = "mapa_aqui_bra_23.png",   # Nome do arquivo a ser salvo
         width = 7,                    # Largura em polegadas
         height = 6,                    # Altura em polegadas
         dpi = 300)                     # Resolução em DPI (300 é recomendado para impressões)
  