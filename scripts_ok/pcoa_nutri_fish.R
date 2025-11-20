# Pacotes necessários
library(cluster)
library(FD)
library(ape)
library(geometry)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readxl)

# Dados de exemplo para replicar os cálculos
nutris <- read_excel("fish_species/dt_fish_ok.xlsx")
#colnames(nutris) <- c("trait1", "trait2", "trait3", "trait4", "trait5")

# Calcular a distância e a PCoA
nutris_stand <- decostand(nutris[,-1], method = "hellinger")
distance <- daisy(nutris_stand, metric = "euclidean")
cwm_pcoa <- pcoa(distance, correction="cailliez")
 

100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[1] #48.22% de explicação do eixo 1  
100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[2] #> [1] 25.46% eixo 2 
#100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[3]

pcoa_eixos <- as.data.frame(cwm_pcoa$vectors[, 1:2])  

pcoa_eixos <- data.frame(species = nutris$species, pcoa_eixos)

correlations <- cor(nutris[,2:6], pcoa_eixos[,2:3])

# Plotar as duas primeiras dimensões

pca_load <- 
  as_tibble(correlations, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'calcium_mg_100g' = 'Calcium',
                                  'iron_mg_100g' = 'Iron',
                                  'total_omega_3_pufa_g_100g' = 'Omega-3',
                                  'selenium_mg_100g' = 'Selenium',
                                  'zinc_mg_100g' = 'Zinc'))

pca_hull <- 
  pcoa_eixos %>%
  slice(chull(Axis.1, Axis.2))


ggplot(pcoa_eixos, aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, alpha = 0.5) +
  geom_polygon(data = pca_hull,
               fill = "gray90",
               colour = "gray",
               alpha = 0.3,
               show.legend = FALSE) +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = Axis.1*0.3,
                   yend = Axis.2*0.1),
               size = 0.8,
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$Axis.1*0.35), y = (pca_load$Axis.2*0.15),
           label = pca_load$variable,
           size = 4) +
  # scale_color_manual(labels = c(
  #   "Baía Formosa",
  #   "Ipojuca", 
  #   "Rio do Fogo",
  #   "S.J da Coroa Grande",
  #   "Tamandaré",
  #   "Touros"),
  #   values = c(
  #     "#2E7253", "#F9EA29", "#045a8d", 
  #     "#F0442A", "#FF9F24", "#74a9cf")) +
  labs(x = "PC1 (94.9%)", y = "PC2 (2.1%)",
       colour = "Municipality") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))

