# ============================================================
#   Nutrient Composition and PCoA Analysis of Food Sources
#   Author: Fabricio Albuquerque
#   Description:
#   This script computes community‐weighted means (CWM) of
#   nutrient traits based on monthly food consumption data,
#   performs PCoA on Euclidean distances among municipalities,
#   and visualizes functional space and nutrient loadings.
# ============================================================

# ------------------ Load Required Packages ------------------

library(FactoMineR)
library(factoextra)
library(ggpubr)
library(FD)
library(tidyverse)
library(readxl)
library(patchwork)
library(sp)

# ----------------------- Input Data --------------------------

# Matrix of nutritional traits for each food resource.
# Rows = food items, columns = nutrients (Ca, Fe, Zn, Se, Omega-3)

food_source_nutri <- read_excel("fish_species/food_source_nutri.xlsx",
                                sheet = "Planilha1") |> 
  column_to_rownames(var = "food_resource")

# Monthly food consumption per municipality.
# Rows = municipalities, columns = food items.
cons_food_month <- read_excel("fish_species/cons_food_month.xlsx",
                              sheet = "Planilha1") |> 
  select(id_annm, 5:21) |> 
  column_to_rownames(var = "id_annm")

cons_food_mon2 <- read_excel("fish_species/cons_food_month.xlsx")                      
cons_food_binary <- ifelse(cons_food_month > 0, 1, 0)

# ------------------ Nutrient Composition -------------------

# Compute Community‐Weighted Means (CWM) of nutrients for each municipality.
# This summarises the nutrient content of the local diet.
cwm_fren <- functcomp(food_source_nutri, as.matrix(cons_food_binary))  
head(cwm_fren)
cwm_dis <- vegdist(cwm_fren, "euclidean")




#result <- dbFD(food_source_nutri, cons_food_month, w.abun = TRUE)
#print(result)

# Extrair as coordenadas das espécies
#cwm_fren <- as.data.frame(result$CWM)

#cwm_dis <- vegdist(cwm_fren, "euclidean") 

# ---------------- PCA / PCoA Ordination ----------------------

# Perform Principal Coordinates Analysis (PCoA) using Cailliez correction.

cwm_pcoa <- pcoa(D = cwm_dis, correction = "cailliez")  

#Percentage of variation explained by the first two axes.
100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[1]#65.10% de explicação do eixo 1  
100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[2] #> [1] 34.62% eixo 2 
#100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[3]

pcoa_eixos <- as.data.frame(cwm_pcoa$vectors[, 1:2])  

pcoa_eixos <- data.frame(muni = cons_food_mon2$municipality, pcoa_eixos)

correlations <- cor(cwm_fren, pcoa_eixos[,2:3])

# ---------------- Loadings (Nutrient Vectors) ----------------

pca_load <- 
  as_tibble(correlations, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'calcium_mg_100g' = 'Calcium',
                                  'iron_mg_100g' = 'Iron',
                                  'total_omega_3_pufa_g_100g' = 'Omega-3',
                                  'selenium_mg_100g' = 'Selenium',
                                  'zinc_mg_100g' = 'Zinc'))

# Compute convex hull of all municipalities in PCoA space.
pca_hull <- 
  pcoa_eixos %>%
  slice(chull(Axis.1, Axis.2))

# ------------------ Global PCoA Plot --------------------------

# Plot functional dietary space and nutrient loading vectors.
poca_muni <- ggplot(pcoa_eixos, aes(x = Axis.1, y = Axis.2)) +
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

# ---------------- Plots for Each Municipality -----------------
# The following blocks create one plot per municipality, showing its
# convex hull (functional dietary space) in relation to the full space.

# Example for Touros (same workflow repeated below)

pca_hull_tr <- 
  pcoa_eixos %>% 
  filter(muni == "Touros") |> 
  slice(chull(Axis.1, Axis.2))

poca_tr <- ggplot(filter(pcoa_eixos, muni == "Touros"), 
                  aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, colour = "#74a9cf", alpha = 0.6) +
  geom_polygon(data = pca_hull,
               fill = "gray90",
               colour = "gray",
               alpha = 0.3,
               show.legend = FALSE)+
  geom_polygon(data = pca_hull_tr,
               fill =  "#74a9cf",
               colour = "#74a9cf",
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
  labs(x = " ", y = "PC2",
       title = "Touros") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #plot.title = element_text(hjust = 0.95, vjust = -10, size = 8),
        axis.text.x = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))

# (Remaining municipalities follow the same structure)
# Rio do Fogo (RF), Baía Formosa(BF), Ipojuca (IP), Tamandaré (TM), S.J. da Coroa Grande (SJ)
# Each generates a polygon of the municipality-specific functional space

#------------------RF-------------------------

pca_hull_rf <- 
  pcoa_eixos %>% 
  filter(muni == "Rio do Fogo") |> 
  slice(chull(Axis.1, Axis.2))

poca_rf <- ggplot(filter(pcoa_eixos, muni == "Rio do Fogo"),
                  aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, colour = "#045a8d", alpha = 0.6) +
  geom_polygon(data = pca_hull,
               fill = "gray90",
               colour = "gray",
               alpha = 0.3,
               show.legend = FALSE)+
  geom_polygon(data = pca_hull_rf,
               fill =  "#045a8d",
               colour = "#045a8d",
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
  labs(x = " ", y = " ",
       title = "Rio do Fogo") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #plot.title = element_text(hjust = 0.95, vjust = -10, size = 8),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))

#------------------BF-------------------------

pca_hull_bf <- 
  pcoa_eixos %>% 
  filter(muni == "Baia Formosa") |> 
  slice(chull(Axis.1, Axis.2))

poca_bf <- ggplot(filter(pcoa_eixos, muni == "Baia Formosa"),
                  aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, colour = "#2E7253", alpha = 0.6) +
  geom_polygon(data = pca_hull,
               fill = "gray90",
               colour = "gray",
               alpha = 0.3,
               show.legend = FALSE)+
  geom_polygon(data = pca_hull_bf,
               fill =  "#2E7253",
               colour = "#2E7253",
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
           size = 4)+
  labs(x = " ", y = " ",
       title = "Baía Formosa") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #plot.title = element_text(hjust = 0.95, vjust = -10, size = 8),
        axis.text = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))


#------------------IP-------------------------

pca_hull_ip <- 
  pcoa_eixos %>% 
  filter(muni == "Ipojuca") |> 
  slice(chull(Axis.1, Axis.2))

poca_ip <- ggplot(filter(pcoa_eixos, muni == "Ipojuca"),
                  aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, colour = "#F9EA29", alpha = 0.6) +
  geom_polygon(data = pca_hull,
               fill = "gray90",
               colour = "gray",
               alpha = 0.3,
               show.legend = FALSE)+
  geom_polygon(data = pca_hull_ip,
               fill =  "#F9EA29",
               colour = "#F9EA29",
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
  labs(x = "PC1", y = "PC2",
       title = "Ipojuca") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        # = element_text(hjust = 0.95, vjust = -10, size = 8),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))

#------------------TM-------------------------

pca_hull_tm <- 
  pcoa_eixos %>% 
  filter(muni == "Tamandare") |> 
  slice(chull(Axis.1, Axis.2))

poca_tm <- ggplot(filter(pcoa_eixos, muni == "Tamandare"),
                  aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, colour = "#FF9F24", alpha = 0.6) +
  geom_polygon(data = pca_hull,
               fill = "gray90",
               colour = "gray",
               alpha = 0.3,
               show.legend = FALSE)+
  geom_polygon(data = pca_hull_tm,
               fill =  "#FF9F24",
               colour = "#FF9F24",
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
  labs(x = "PC1", y = " ",
       title = "Tamandaré") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #plot.title = element_text(hjust = 0.95, vjust = -10, size = 8),
        axis.text.y = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))

#------------------SJ-------------------------

pca_hull_sj <- 
  pcoa_eixos %>% 
  filter(muni == "Sao Jose da Coroa") |> 
  slice(chull(Axis.1, Axis.2))

poca_sj <- ggplot(filter(pcoa_eixos, muni == "Sao Jose da Coroa"),
                  aes(x = Axis.1, y = Axis.2)) +
  geom_point(size = 3, colour = "#F0442A", alpha = 0.6) +
  geom_polygon(data = pca_hull,
               fill = "gray90",
               colour = "gray",
               alpha = 0.3,
               show.legend = FALSE)+
  geom_polygon(data = pca_hull_sj,
               fill =  "#F0442A",
               colour = "#F0442A",
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
  labs(x = "PC1", y = " ",
       title = "S.J. da Coroa Grande") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #plot.title = element_text(hjust = 0.95, vjust = -10, size = 8),
        axis.text.y = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))


# ---------------- Combine All Municipality Plots --------------
pcoa_muni_sep <- (poca_tr + poca_rf + poca_bf)/ (poca_ip + poca_tm + poca_sj)
#plot_annotation(tag_levels = c('A'), tag_suffix = ')')

poca_muni/pcoa_muni_sep 


pcoa_muni_sep 
#--------------------------------------

# ----------------- Convex Hull Area Calculation ---------------

# Example: compute functional space area for Touros

polygon2 <- Polygon(pca_hull_tr[,2:3])
area2 <- polygon2@area
print(area2)

35.5/75.3

# Repeat for each municipality

# --------------- Explained Variation Plot ----------------------

# Extract proportion of explained variance for each PCoA axis
explained_variance <-cwm_pcoa$values$Relative_eig

barplot(explained_variance[1:5], 
        main = "Variation Explained per PCoA Axis",
        xlab = "PCoA axis",
        ylab = "Explained Proportion of Variation",
        ylim = c(0,0.8),
        las = 2,
        names.arg = paste0("Eixo ", 1:5),
        col = "lightblue"
)

#-----------------------------------------------------
