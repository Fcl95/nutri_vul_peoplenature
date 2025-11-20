library(FactoMineR)
library(factoextra)
library(ggpubr)
library(FD)
library(tidyverse)
library(readxl)
library(patchwork)
library(sp)

#-----------data-----------------------

food_source_nutri <- read_excel("fish_species/food_source_nutri.xlsx") |> 
  column_to_rownames(var = "food_resource")

#food_source_nutri <- food_source_nutri[c(-35, -42),]
#----------------------------------

food_source_nutri <- decostand(food_source_nutri, method = "standardize")

food_dis <- vegdist(food_source_nutri, "euclidian")


#rownames(pcoa_eixos)

cwm_pcoa <- pcoa(D = food_dis, correction = "cailliez")  


100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[1]#46.00% de explicação do eixo 1  
100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[2] #> [1] 21.51% eixo 2 
#100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[3]

pcoa_eixos <- as.data.frame(cwm_pcoa$vectors[, 1:2])  

name_ax <- data.frame(axi = c("Goat","Tilapia","Crustaceans","Ultraproc_meat","Egg","Pork","Chicken","Beef", rep("Fish", 36)))

pcoa_eixos <- data.frame(muni = name_ax$axi, pcoa_eixos)

correlations <- cor(food_source_nutri, pcoa_eixos[,2:3])

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
                   xend = Axis.1*4,
                   yend = Axis.2*4),
               size = 0.8,
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$Axis.1*5), y = (pca_load$Axis.2*4.2),
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
  labs(x = "PC1 (61.60%)", y = "PC2 (38.10%)",
       colour = "Municipality") +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))

#-------------------------------
pca_hull_fish <- 
  pcoa_eixos %>% 
  filter(muni == "Fish") |> 
  slice(chull(Axis.1, Axis.2))


ggplot(filter(pcoa_eixos, muni == "Fish"), 
       aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, colour = "#74a9cf", alpha = 0.6) +
  geom_point(data = filter(pcoa_eixos, muni != "Fish"),
             aes(x = Axis.1, y = Axis.2), 
             size = 3, colour = "black", alpha = 0.6) +
  geom_polygon(data = pca_hull,
               fill = "gray90",
               colour = "gray",
               alpha = 0.3,
               show.legend = FALSE)+
  geom_polygon(data = pca_hull_fish,
               fill =  "#74a9cf",
               colour = "#74a9cf",
               alpha = 0.3,
               show.legend = FALSE) +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = Axis.1*3.5,
                   yend = Axis.2*3.5),
               size = 0.8,
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$Axis.1*4), y = (pca_load$Axis.2*3.7),
           label = pca_load$variable,
           size = 4) +
  annotate('text', x = 4, y = -2.5,
           label = "45%",
           size = 8, colour = "#74a9cf") +
  labs(x = "PCoA1 (40.10%)", y = "PCoA2 (34.87%)") +
  annotate("point", x = c(4:4), y = c(3:4), size = 4, 
           colour = c("Black", "#74a9cf")) + 
  annotate("text", x = 4.5:4.5, y = 3:4, 
           label = c("Other animal-based\nfoods", "Marine fishes"))+
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank(),
        #plot.title = element_text(hjust = 0.95, vjust = -10, size = 8),
        #axis.text.x = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))

#---------------------------------

polygon2 <- Polygon(pca_hull_fish[,2:3])
area2 <- polygon2@area
print(area2)

10.63/23.41

