library(tidyverse)
library(readxl)
library(ggpmisc)
library(janitor)
library(vegan)
library(patchwork)
library(scales)
#-------------------------

consumo_pesc <- read_excel("C:/Users/fca95/OneDrive/Documentos/Doutorado/Nutri_vuln/fish_species/Quest_consumo_pesc.xlsx", 
                            sheet = "food_intake") |> 
  clean_names() %>% 
  replace(is.na(.), 0)

head(consumo_pesc)
str(consumo_pesc)

# turn data frame into matrix so it functions in the vegan package

consumo_pesc_community <- consumo_pesc[,7:15]

consumo_pesc_matrix <- as.matrix(consumo_pesc_community)

consumo_pesc$riq <- specnumber(consumo_pesc[,7:15])

# fazendo o nMDS usando a matriz de distancia de bray-curtis

## Composição de espécies padronizar com método de Hellinger 
cons.hel <- decostand(x = consumo_pesc[,7:15], method = "hellinger") 

## Matriz de distância com método Bray-Curtis 
cons.dis <- vegdist(x = cons.hel, method = "bray") #tentar outros method


# check model assumptions
# check for multivariate homogeneity of group variances

# use betadisper test to check for multivariate homogeneity of group variances
inv.dispersion <- betadisper(cons.dis, group=consumo_pesc$municipality)
permutest(inv.dispersion)


#nMDS
cons1 <- metaMDS(cons.dis, trymax = 999, distance = "bray")  
inv.NMDS <- metaMDS(cons.dis, previous.best = cons1, distance = "bray")  

##
# inv.NMDS <- metaMDS(consumo_pesc_matrix, distance = "bray", 
#                     #k = 3, autotransform = TRUE, 
#                     trymax=500)

inv.NMDS$stress
# 0.17, ideal ser menor que 0.2


#-------PERMANOVA para testar diferenças entre os grupos------

inv_permanova <- adonis2(as.matrix(consumo_pesc[,7:15]) ~ municipality,
                         consumo_pesc, permutations = 999, 
                         method = "bray"
                         #method = "jaccard"
)


inv.NMDS$points

#pairwise test
source("C:/Users/fca95/OneDrive/Documentos/Doutorado/Nutri_vuln/pairwise.adonis.R")

pairwise_result <- pairwise.adonis(x = consumo_pesc[,7:15],
                                   factors = consumo_pesc$municipality,
                                   sim.method = "bray",
                                   perm = 999)


# Visualizar os resultados
print(pairwise_result)

pairws_resl <- as.data.frame(pairwise_result)

#openxlsx::write.xlsx(pairws_resl, "pairwise_tab.xlsx")
#------------------Fazendo o grafico do nMDS----------------------------------

dat.graf <- data.frame(inv.NMDS$points, 
                       muni = consumo_pesc$municipality)

head(dat.graf)

## Definir os grupos ("HULL") para serem categorizados no gráfico 
grp.bf <- dat.graf[dat.graf$muni == "Baia Formosa", ] [chull(dat.graf[dat.graf$muni == "Baia Formosa", c("MDS1", "MDS2")]), ]


grp.tr <- dat.graf[dat.graf$muni == "Touros", ] [chull(dat.graf[dat.graf$muni == "Touros", c("MDS1", "MDS2")]), ]

grp.rf <- dat.graf[dat.graf$muni == "Rio do Fogo", ] [chull(dat.graf[dat.graf$muni == "Rio do Fogo", c("MDS1", "MDS2")]), ]

grp.ip <- dat.graf[dat.graf$muni == "Ipojuca", ] [chull(dat.graf[dat.graf$muni == "Ipojuca", c("MDS1", "MDS2")]), ]

grp.tm <- dat.graf[dat.graf$muni == "Tamandare", ] [chull(dat.graf[dat.graf$muni == "Tamandare", c("MDS1", "MDS2")]), ]

grp.sj <- dat.graf[dat.graf$muni == "Sao Jose da Coroa", ] [chull(dat.graf[dat.graf$muni == "Sao Jose da Coroa", c("MDS1", "MDS2")]), ]

## Combinar dados dos grupos para cada Convex Hull
hull.data <- rbind(grp.tr, grp.rf, grp.bf, grp.ip, grp.tm, grp.sj) 
head(hull.data)

## Plot
#nmds_d1 <-
  
  
nmds_plot <-   ggplot() +
  geom_polygon(data = hull.data, 
               aes(x = MDS1, y = MDS2, fill = muni, group = muni),
               alpha = 0.3) +
  geom_point(data = dat.graf, aes(x = MDS1, y = MDS2, 
                                  color = muni, 
                                  shape = muni), 
             size = 4, alpha = 0.7) +
  scale_color_manual(labels = c(
    "Baía Formosa",
    "Ipojuca", 
    "Rio do Fogo",
    "S.J da Coroa Grande",
    "Tamandaré",
    "Touros"),
    values = c(
      "#2E7253", "#F9EA29", "#045a8d", 
      "#F0442A", "#FF9F24", "#74a9cf")
    ) +
  scale_fill_manual(labels = c(
    "Baía Formosa",
    "Ipojuca", 
    "Rio do Fogo",
    "S.J da Coroa Grande",
    "Tamandaré",
    "Touros"),
    values = c(
      "#2E7253", "#F9EA29", "#045a8d", 
      "#F0442A", "#FF9F24", "#74a9cf")
    ) +
  scale_shape_manual(labels = c(
    "Baía Formosa",
    "Ipojuca", 
    "Rio do Fogo",
    "S.J da Coroa Grande",
    "Tamandaré",
    "Touros"),
    values = c(16, 15, 17, 18, 19, 20)) +
  labs(x = "NMDS1", y = "NMDS2", 
       shape = "Municipality", 
       colour = "Municipality", fill = "Municipality") +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.92, 0.1), 
        legend.justification = c(0.7, 0.3),
        legend.background =  element_rect( 
          fill = "transparent"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        panel.grid = element_blank()
        ) +
  annotate("text", x = 0.5, y = 0.5,
           #label = "italic(R) ^ 2 == 0.75"), colocar r2
           label = "atop(italic(stress) == 0.2)",
           parse = TRUE,
           size = 6)

  # +
  #   geom_point(data = cnt_bf, aes(x = MDS1, y = MDS2), 
  #              colour = "yellow", size = 3) 

# cnt_bf <- grp.bf %>%
#   summarise_at(c("MDS1", "MDS2"), mean, na.rm = TRUE)

#---------------------------
cons_food_month <- read_excel("fish_species/cons_food_month.xlsx",
                              sheet = "Planilha1") |> 
    clean_names()

cons_food_month <- cons_food_month |> 
  select(id_annm, municipality, 4:12) |> 
  group_by(municipality) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)|> 
  pivot_longer(cols = -municipality, names_to = "food_tp", values_to = "value") |> 
  pivot_wider(names_from = municipality, values_from = value) |> 
  clean_names()


#cons_food_month <- cons_food_month[-1,]
  
(cons_f <- cons_food_month %>% 
    gather(sample, value, baia_formosa:touros) %>%
    ggplot(aes(sample, food_tp, size = value, 
               colour = sample, fill = sample)) + 
    geom_point(alpha=0.7) +
    scale_size(range = c(1, 12), name="Food intake") +
    scale_x_discrete(
      labels = c(
        "Baía Formosa",
        "Ipojuca", 
        "Rio do Fogo",
        "S.J da Coroa Grande",
        "Tamandaré",
        "Touros")) +
    scale_y_discrete(
      labels =c(
        "Beef", "Chicken", "Crustaceans",
        "Egg", "Goat", "Marine fish", "Pork",
        "Tilapia", "ultra-processed meat" 
      )
    ) +
    scale_fill_manual(
      values = c("#2E7253", "#F9EA29", "#045a8d", 
                 "#F0442A", "#FF9F24", "#74a9cf"),
      guide = "none") +
    scale_color_manual(
      values = c("#2E7253", "#F9EA29", "#045a8d",
                 "#F0442A", "#FF9F24", "#74a9cf"),
      guide = "none") +
    labs(x = "", y = "") +
    theme_bw() +
  theme(axis.text.x=element_text(angle=30, vjust=0.95, hjust=.8)) 
)

nmds_plot + cons_f

#----------------------------

cons_bar <- cons_food_month %>% 
  gather(sample, value, baia_formosa:touros) %>%
  ggplot(aes(x = sample, y = value, fill = food_tp)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(labels = c(
    "Beef","Chicken","Crustaceans",
    "Egg","Goat","Marine fish",
    "Pork", "Tilapia", "Ultra-processed meat"),
    values = c("#d53e4f","#f46d43", "#fdae61", 
    "#ffeda0", "#fec44f", "#3288bd", "#67a9cf",
    "#66c2a5", "#02818a"),
    guide = "none") +
  scale_y_continuous(labels = label_percent(),
                     expand = c(0.001, 0.01)) +
  scale_x_discrete(
    labels = c(
      "Baía Formosa",
      "Ipojuca", 
      "Rio do Fogo",
      "S.J da Coroa Grande",
      "Tamandaré",
      "Touros")) +
  labs(x = " ", y = " ", 
       fill = "Food source") +
  theme_classic(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.text.x= 
          element_text(angle=30, vjust=0.95, 
                       hjust=.8, colour = "black")) 

cons_bar + nmds_plot


#-----------------------------------------------

pca_values <- 
  prcomp(consumo_pesc_community, center = TRUE, scale = TRUE)


summary(pca_values)

pca_points <- 
  # first convert the pca results to a tibble
  as_tibble(pca_values$x) %>% 
  # now we'll add the penguins data
  bind_cols(municipality = consumo_pesc$municipality)


basic_plot <- 
  ggplot(pca_points, aes(x = PC1, y = PC2)) +
  geom_point(aes(colour = municipality), size = 3) +
  theme_light()

basic_plot


# first create a dataframe to extract the convex hull points
pca_hull <- 
  pca_points %>% 
  group_by(municipality) %>% 
  slice(chull(PC1, PC2))

# now, we'll just continue to build on our ggplot object
chull_plot <- 
  basic_plot +
  geom_polygon(data = pca_hull,
               aes(fill = municipality,
                   colour = municipality),
               alpha = 0.3,
               show.legend = FALSE)

chull_plot

pca_load <- 
  as_tibble(pca_values$rotation, rownames = 'variable') %>% 
  # we can rename the variables so they look nicer on the figure
  mutate(variable = dplyr::recode(variable,
                                  'marine_fish_intk' = 'Marine fish',
                                  'tilapia_intk_year' = 'Tilapia',
                                  'chicken_intk' = 'Chicken',
                                  'beef_intk' = 'Beef',
                                  'pork_intk' = 'Pork',
                                  'egg_intk' = 'Eggs',
                        'ultrprcd_meat_intk_month' = 'Ultra-proc. meat',
                                  'goat_intk_year' = 'Goat',
                        'crustaceans_intk_month' = 'Crustaceans'))

pca_food <- chull_plot +
  geom_segment(data = pca_load, 
               aes(x = 0, y = 0, 
                   xend = PC1*5,
                   yend = PC2*5),
               arrow = arrow(length = unit(1/2, 'picas'))) +
  annotate('text', x = (pca_load$PC1*6), y = (pca_load$PC2*5.2),
           label = pca_load$variable,
           size = 3.5) +
  scale_color_manual(labels = c(
    "Baía Formosa",
    "Ipojuca", 
    "Rio do Fogo",
    "S.J da Coroa Grande",
    "Tamandaré",
    "Touros"),
    values = c(
      "#2E7253", "#F9EA29", "#045a8d", 
      "#F0442A", "#FF9F24", "#74a9cf")
  ) +
  scale_fill_manual(labels = c(
    "Baía Formosa",
    "Ipojuca", 
    "Rio do Fogo",
    "S.J da Coroa Grande",
    "Tamandaré",
    "Touros"),
    values = c(
      "#2E7253", "#F9EA29", "#045a8d", 
      "#F0442A", "#FF9F24", "#74a9cf")
  ) +
  scale_shape_manual(labels = c(
    "Baía Formosa",
    "Ipojuca", 
    "Rio do Fogo",
    "S.J da Coroa Grande",
    "Tamandaré",
    "Touros"),
    values = c(16, 15, 17, 18, 19, 20)) +
  labs(x = "PC1 (19%)", y = "PC2 (16%)", 
       shape = "Municipalities", 
       colour = "Municipalities", fill = "Municipalities") +
  theme_bw(base_size = 14) +
  theme(legend.position = c(0.15, 0.1), 
        legend.justification = c(0.7, 0.3),
        legend.background =  element_rect( 
          fill = "transparent"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        panel.grid = element_blank()
  )


cons_bar + pca_food
