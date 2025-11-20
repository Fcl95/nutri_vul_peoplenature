library(tidyverse)
library(readxl)
library(janitor)
#----------------


cons_food_month <- read_excel("fish_species/cons_food_month.xlsx",
                              sheet = "Planilha1") |> 
  clean_names() |> 
  select(3, 13:21)

cons_muni <- cons_food_month |> 
  group_by(municipality) |> 
  summarise_all(mean)

cons_muni_lg <- cons_muni |> 
  pivot_longer(cols = -municipality, names_to = "fish_sp", values_to = "value")


ggplot(cons_muni_lg, aes(x = municipality, y = fish_sp, fill= value)) + 
  geom_tile() +
  scale_fill_gradient(low="#eff3ff", high="#08519c") +
  scale_x_discrete(
    labels = c(
      "Baía Formosa",
      "Ipojuca", 
      "Rio do Fogo",
      "S.J da Coroa Grande",
      "Tamandaré",
      "Touros"),
    expand = c(0.001, 0.01)) +
  scale_y_discrete(labels = rev( c(
    "Thunnus.sp", "Spa.axi", "Sco.bra", "Sca.tri",
    "Ocy.chr", "Myc.bon",  "Lut.syn", "Lut.ana", "Caranx.sp"
  )),
                   expand = c(0.001, 0.01)) +
  labs(x = " ", y = " ", 
       fill = "Intake\n(meals/month)") +
  theme_minimal(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.text.x= 
          element_text(angle=22, vjust=0.95, 
                       hjust=.8, colour = "black"),
        axis.text.y= 
          element_text(face = "italic", colour = "black"),
        axis.ticks = element_line(colour = "gray50"),
        legend.title.align = 0.5,
        legend.box.just = "center",
        legend.title = element_text(size = 12)) 

#----------Permanova----------------------------

# Instalar e carregar o pacote vegan (caso ainda não tenha instalado)
# if (!require("vegan")) install.packages("vegan")
library(vegan)
# 
# # Exemplo de dados simulados (substitua pelos seus dados reais)
# set.seed(123)
# dados <- data.frame(
#   Municipio = rep(c("Mun1", "Mun2", "Mun3", "Mun4", "Mun5", "Mun6"), each = 10),
#   Especie1 = rpois(60, lambda = rep(5:10, each = 10)),
#   Especie2 = rpois(60, lambda = rep(3:8, each = 10)),
#   Especie3 = rpois(60, lambda = rep(2:7, each = 10))
# )
# 
# # Verifique a estrutura dos dados
# head(dados)

# Prepare os dados
# 1. Extraia as variáveis de espécies (composição)
composicao <- cons_food_month[, -1]

# 2. Crie a matriz de distâncias (usando a distância de Bray-Curtis)
distancia <- vegdist(composicao, method = "bray")

# Realize a PERMANOVA
resultado_permanova <- adonis2(distancia ~ municipality, data = cons_food_month, permutations = 999)

# Exibir resultados
print(resultado_permanova)

# Visualização (opcional)
# Gráfico de NMDS para explorar visualmente a separação entre municípios
# nmds <- metaMDS(composicao, distance = "bray", k = 2, trymax = 100)
# plot(nmds, type = "t", main = "NMDS - Composição de Espécies por Município")
# ordihull(nmds, groups = cons_food_month$municipality, draw = "polygon", col = 1:6, label = TRUE)

source("C:/Users/fca95/OneDrive/Documentos/Doutorado/Nutri_vuln/pairwise.adonis.R")

# Matriz de distâncias (usando Bray-Curtis, como no exemplo anterior)
distancia <- vegdist(composicao, method = "bray")

# Comparações par a par entre os municípios
comparacoes_par <- pairwise.adonis(distancia, 
                                   factors = cons_food_month$municipality, 
                                   p.adjust.m = "bonferroni")

# Exibir os resultados
print(comparacoes_par)


# Converter resultados em um dataframe
tabela_resultados <- as.data.frame(comparacoes_par)

# Renomear as colunas (ajuste conforme necessário)
colnames(tabela_resultados) <- c("Municipios", "df", "Sumofsquares", 
                                 "F", "R2", "p-valor", "p.adjusted")

# Arredondar valores numéricos
tabela_resultados$F <- round(tabela_resultados$F, 3)
tabela_resultados$Sumofsquares  <- round(tabela_resultados$Sumofsquares, 3)
tabela_resultados$R2 <- round(tabela_resultados$R2, 3)
tabela_resultados$`p-valor` <- signif(tabela_resultados$`p-valor`, 3)
tabela_resultados$p.adjusted <- signif(tabela_resultados$p.adjusted, 3)
tabela_resultados <- tabela_resultados[, -8]


# Salvar como CSV
openxlsx::write.xlsx(tabela_resultados, 
                     file = "resultados_comparacoes.xlsx")
