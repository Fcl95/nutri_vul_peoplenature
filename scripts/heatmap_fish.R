#====================================================================
# Analysis of Monthly Fish Consumption and PERMANOVA by Municipality
#--------------------------------------------------------------------
# This script:
# 1) Loads and cleans consumption data
# 2) Generates a heatmap of average monthly intake of fish species
# 3) Runs PERMANOVA (adonis2) to test differences between municipalities
# 4) Performs pairwise PERMANOVA using custom function pairwise.adonis
#====================================================================


#--------------------------- Load packages ---------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(vegan)

#--------------------------- Load dataset ----------------------------

# Import dataset and clean column names
cons_food_month <- read_excel("fish_species/cons_food_month.xlsx",
                              sheet = "Planilha1") |> 
  clean_names() |> 
  select(3, 13:21)

#---------------------- Mean consumption per municipality ------------

# Compute mean values of all fish species per municipality
cons_muni <- cons_food_month |> 
  group_by(municipality) |> 
  summarise_all(mean)

# Convert to long format for heatmap plotting
cons_muni_lg <- cons_muni |> 
  pivot_longer(cols = -municipality, names_to = "fish_sp", values_to = "value")

#----------------------------- Heatmap -------------------------------
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

#=====================================================================
#                           PERMANOVA
#=====================================================================

# Purpose:
# Test whether the composition of consumed fish species differs
# among municipalities.
#---------------------------------------------------------------------

# 1. Extract compositional matrix (all species columns)
composicao <- cons_food_month[, -1]

# 2. Crie a matriz de distâncias (usando a distância de Bray-Curtis)
distancia <- vegdist(composicao, method = "bray")

# Realize a PERMANOVA
resultado_permanova <- adonis2(distancia ~ municipality, data = cons_food_month, permutations = 999)

# Exibir resultados
print(resultado_permanova)

# Optional: NMDS visualization
# Gráfico de NMDS para explorar visualmente a separação entre municípios
# nmds <- metaMDS(composicao, distance = "bray", k = 2, trymax = 100)
# plot(nmds, type = "t", main = "NMDS - Composição de Espécies por Município")
# ordihull(nmds, groups = cons_food_month$municipality, draw = "polygon", col = 1:6, label = TRUE)

#=====================================================================
#                     Pairwise PERMANOVA comparisons
#=====================================================================

# Import custom function for pairwise adonis
source("scripts_ok/pairwise.adonis.R")

# Matriz de distâncias (usando Bray-Curtis, como no exemplo anterior)
distancia <- vegdist(composicao, method = "bray")

# Comparações par a par entre os municípios
comparacoes_par <- pairwise.adonis(distancia, 
                                   factors = cons_food_month$municipality, 
                                   p.adjust.m = "bonferroni")

# Exibir os resultados
print(comparacoes_par)


#---------------------- Format pairwise results ----------------------
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
# openxlsx::write.xlsx(tabela_resultados, 
#                      file = "resultados_comparacoes.xlsx")
