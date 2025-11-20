library(FactoMineR)
library(factoextra)
library(ggpubr)
library(FD)
library(tidyverse)
library(readxl)
library(patchwork)
library(sp)

#-----------data-----------------------

food_source_nutri <- read_excel("fish_species/food_source_nutri.xlsx",
                                sheet = "Planilha1") |> 
  column_to_rownames(var = "food_resource")

cons_food_month <- read_excel("fish_species/cons_food_month.xlsx",
                              sheet = "Planilha1") |> 
  select(id_annm, 5:21) |> 
  column_to_rownames(var = "id_annm")

cons_food_mon2 <- read_excel("fish_species/cons_food_month.xlsx")                      
cons_food_binary <- ifelse(cons_food_month > 0, 1, 0)
#-----------func div-----------------------

cwm_fren <- functcomp(food_source_nutri, as.matrix(cons_food_binary))  
head(cwm_fren)
cwm_dis <- vegdist(cwm_fren, "euclidean")




#result <- dbFD(food_source_nutri, cons_food_month, w.abun = TRUE)
#print(result)

# Extrair as coordenadas das espécies
#cwm_fren <- as.data.frame(result$CWM)

#cwm_dis <- vegdist(cwm_fren, "euclidean") 

cwm_pcoa <- pcoa(D = cwm_dis, correction = "cailliez")  

100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[1]#65.10% de explicação do eixo 1  
100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[2] #> [1] 34.62% eixo 2 
#100 * (cwm_pcoa$values[, 1]/cwm_pcoa$trace)[3]

pcoa_eixos <- as.data.frame(cwm_pcoa$vectors[, 1:2])  

pcoa_eixos <- data.frame(muni = cons_food_mon2$municipality, pcoa_eixos)

correlations <- cor(cwm_fren, pcoa_eixos[,2:3])

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

#------------------Touros-------------------------

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

#---juntando os plots-----------------
pcoa_muni_sep <- (poca_tr + poca_rf + poca_bf)/ (poca_ip + poca_tm + poca_sj)
#plot_annotation(tag_levels = c('A'), tag_suffix = ')')

poca_muni/pcoa_muni_sep 


pcoa_muni_sep 
#--------------------------------------

polygon2 <- Polygon(pca_hull_tr[,2:3])
area2 <- polygon2@area
print(area2)

35.5/75.3



#---------explained variation-----------
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
ssp_pca_var <- consumo_pesc[, 7:15]
ssp_pca_var <-as.data.frame(ssp_pca_var)

row.names(ssp_pca_var) <- consumo_pesc$id_annm 

ssp_pca_var <- decostand(x = ssp_pca_var, method = "hellinger")

pca.p <- PCA(X = ssp_pca_var, 
             #scale.unit = TRUE, 
             graph = FALSE)

print(row.names(ssp_pca_var))
pca.p$eig

p1 <- fviz_pca_biplot(X = pca.p,
                      #repel = TRUE,
                      geom.ind = "point", 
                      fill.ind = consumo_pesc$municipality, 
                      col.ind = "black",
                      alpha.ind = 0.7,
                      pointshape = 21, 
                      pointsize = 4,
                      palette = c("#3182bd", "#a50f15", 
                                  "#fecc5c", "#fb6a4a",
                                  "purple", "orange3"),
                      addEllipses = TRUE,
                      col.var = "#636363",
                      arrowsize = 0.9,
                      labelsize = 5,
                      invisible = "quali",
                      title = NULL,
                      legend.title = "Municipality") +
  labs(title = "A) PCA plot axis 1 and 2",
       x = "PC1 (24.1%)", y = "PC2 (15.5%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.13, 0.8),
        legend.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

res.desc <- dimdesc(pca.p, axes = c(1,2), proba = 0.05)
