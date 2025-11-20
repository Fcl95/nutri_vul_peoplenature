# ============================================================
# Data processing and visualization of food intake and PCA
# Author: Fabricio Albuquerque
# Description: This script processes monthly food intake data,
#              generates stacked bar plots of food sources,
#              and performs PCA to visualize dietary patterns
#              across municipalities.
# ============================================================

# ---------------------- Load packages ------------------------

library(tidyverse)
library(readxl)
library(ggpmisc)
library(janitor)
library(vegan)
library(patchwork)
library(scales)
#-------------------------

# --------------- Import food intake questionnaire ---------

# Read household-level food intake data and replace NAs with zero
consumo_pesc <- read_excel("fish_species/Quest_consumo_pesc.xlsx", 
                           sheet = "food_intake") |> 
  clean_names() %>% 
  replace(is.na(.), 0)

head(consumo_pesc)
str(consumo_pesc)

# ------------ Monthly food consumption by municipality ------------

# Read monthly consumption dataset
cons_food_month <- read_excel("fish_species/cons_food_month.xlsx",
                              sheet = "Planilha1") |> 
  clean_names()

# Select relevant columns, aggregate numeric variables by municipality,
# then reshape to long format and back to wide to prepare for plotting

cons_food_month <- cons_food_month |> 
  select(id_annm, municipality, 4:12) |> 
  group_by(municipality) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)|> 
  pivot_longer(cols = -municipality, names_to = "food_tp", values_to = "value") |> 
  pivot_wider(names_from = municipality, values_from = value) |> 
  clean_names()


# ---------------------- Stacked barplot ----------------------

# Prepare stacked proportional barplot of food sources by municipality

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

cons_bar


# -------------- PCA: Food intake composition -------------

# Extract only food intake numeric columns for PCA

consumo_pesc_community <- consumo_pesc[,7:15]


# Run PCA with centering and scaling
pca_values <- 
  prcomp(consumo_pesc_community, center = TRUE, scale = TRUE)


summary(pca_values)

# Create a tibble of PCA scores and add municipality info
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


# ---------------------- Convex hull polygons ----------------------

# Extract convex hull for each municipality to outline PCA clusters
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

# -------------- PCA loadings (variable vectors) ----------------

# Convert PCA loadings to tibble

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

# Add loading vectors to PCA plot
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

# ---------------------- Combine plots ----------------------
cons_bar + pca_food
