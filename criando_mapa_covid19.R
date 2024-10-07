#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxXXXXXXXXXXXXXXXXX
#
#                          CRIAÇÃO DE MAPAS COM O PACOTE GEOBR- COVID19
#
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# 1) Preparação e carregamento de limites poligonais dos municipios -------------------------
  library(geobr) # Esse pacote acessa os dados dos limites dos estado sem a necessidade de ir no site IBGE
  library(sf)     #sf: simple feature

# acessa os limites terrotoriais de qualquer municipio brasileiro ou estado
  lim.estados = read_state(code_state = 'all') # Acessa os limites de estados
  class(lim.estados) # Esses dados, al?m de ter os limites, tem caracter?sicas, como popula??o, etc


# 2) Criando o primeiro mapa simples --------------------------
  library(ggplot2)
  ggplot(lim.estados) + # informa qual conjunto de dados o ggplot usará (dados geoespaciais)
    geom_sf()   # informa o tipo de função gráfica sobre os tipos de dados (sf: spatial features) 

# Vamos colorir de acordo com o codigos dos estados
  ggplot(lim.estados) + 
    geom_sf(aes(fill=code_state))

# 3) Acessar dados do atlas 2013 e filtrar ---------------------------
# Filtrar os dados pela último dia dos dados acumulados de covid
  library(readxl)
  library(dplyr)
  # list.files()
  # Fonte dos dados: https://covid.saude.gov.br/
  # Após digitar as primeiras letras do arquivo, tecla + tab
  
  covid <- read_excel('HIST_PAINEL_COVIDBR_24jul2020.xlsx', sheet = 2) 
  str(covid)
  head(covid)
  tail(covid)
  
  # Selecionando colunas para limpar
  covid2 <- covid %>% select(regiao, estado, coduf, populacaoTCU2019, casosAcumulado,
                            obitosAcumulado)
  
  
# 4) Vamos juntar dados (merge) -------------------------------------------
# Precisamos ter uma coluna com mesmo conteúdo nos 2 conjuntos de dados
  covid2 <- covid2 %>% rename(abbrev_state = estado) # Renomeia a coluna 

# Juntando as informações também com o pacote dplyr
  juntos <- inner_join(lim.estados, covid2, by = "abbrev_state")
  # Junta só as linhas onde há corrrespondencia exat entre as duas tabelas para a coluna "abbrev-state"
  
# 5) Fazendo gráfico (mapa) com a informações de covid -----------------------------
  ggplot(juntos) + geom_sf(aes(fill = abbrev_state))   # Usa dados categóricos
  ggplot(juntos) + geom_sf(aes(fill = casosAcumulado)) # Usa os dados numéricos

# 6) Criando intervalo de classe dos CASOS ACUMULADOS -----------------------------
  
  # Criando dados para criarr uma categorização de valores 
  x = c(0, 200, 2000, 4000, 6000, 10000)  # Valores mais consistentes a serem categorizados
  
  # Definindo intervalos coerentes e rótulos adequados
  y = cut(x, 
          breaks = c(0, 200, 2000, 4000, 6000, Inf),  # Intervalos ajustados
          labels = c("1 a 200", "201 a 2000", "2001 a 4000", "4001 a 6000", "Mais de 6000"),  # Rótulos coerentes
          right = TRUE)  # Intervalos fechados à direita (inclusivos no limite superior)
  
  # Criando um data frame para visualizar os resultados
  resultado <- data.frame(Valores = x, Categoria = y)
  resultado
  
 
  # agora vamos fazer com dados com os casos acumulados de covid-19
  categoria = cut(juntos$casosAcumulado, 
  breaks = c(0, 30000, 60000, 100000, 150000, 200000, Inf) , 
  labels = c("1 a 30000", "30001 a 60000", "60001 a 100000", 
             "100001 a 150000", "150001 a 200000", "Mais de 200000"))
  
  # Vamos verificar se os dados foram atribuidos a categorias corretas
  data.frame(juntos$casosAcumulado, categoria)
  
  # Colocando as faixas no conjuntos de dados chamados juntos
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
  
  gg
# 8) Colocando a escala ~de orientaço do mapa ------------------------------
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
  gg + geom_image(aes(-35,-27), image = "coronavirus.png", size = 0.2)
                 
    
# 10) Coloando título e mudando tema ----------------------------------------
  meu.plot <- gg + geom_image(aes(-35,-27), image = "coronavirus.png", size = 0.2) +
                   labs(title = "Casos de Covid-19",
                        subtitle = "Confirmados em 24.07.2020",
                        fill = "Casos \nConfirmados", #Muda titulo da legenda
                        x = NULL, y = NULL) +
                   theme_bw()+
              theme(legend.position = c(0.18, 0.2),
                    legend.key.height = unit(5, "mm"))
  meu.plot
# 11) Salvando -----------------------------------------
  # Salvando o que é exibido na janela gráfica:
  ggsave(meu.plot, filename = "mapa_covid.png")
  
  # Especificando dimensões  e a qualidade da imagem:
  ggsave(meu.plot, 
         filename = "mapa_covid.png",   # Nome do arquivo a ser salvo
         width = 7,                    # Largura em polegadas
         height = 6,                    # Altura em polegadas
         dpi = 300)                     # Resolução em DPI (300 é recomendado para impressões)
  