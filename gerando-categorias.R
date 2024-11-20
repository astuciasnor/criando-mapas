# Carregando o pacote necessário
library(dplyr)

# Gerando 20 valores aleatórios entre 0 e 10000
set.seed(123)
x <- sample(0:10000, 20)

# Definindo intervalos e rótulos
breaks <- c(0, 200, 2000, 4000, 6000, Inf)
labels <- c("1 a 200", "201 a 2000", "2001 a 4000", "4001 a 6000", "Mais de 6000")

# Criando um data frame e categorizando os valores
resultado <- data.frame(Valores = x) %>%
  mutate(Categoria = cut(Valores, breaks = breaks, labels = labels, right = TRUE))

# Visualizando o resultado
print(resultado)
