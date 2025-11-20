############################################################
# Data preparation pipeline for nutritional and trait data
# Author: Fabricio Albuquerque
# Description:
#   - Loads fisheries catch data
#   - Merges predicted nutrient levels
#   - Loads and cleans trait datasets (Quimbayo et al.)
#   - Standardizes numeric fields
#   - Merges traits + nutrients
#   - Computes PCA on nutritional variables
#   - Computes nutrient density index (% of RDI)
############################################################

# ----------------------------------------------------------
# Load required packages
# ----------------------------------------------------------

library(tidyverse)
library(readxl)
library(ggpmisc)
library(janitor)
require(here)
require(openxlsx)
require(dplyr)
library(readr)

# custom functions
source ("functions.R")

# ----------------------------------------------------------
# Load catch data (species names)
# ----------------------------------------------------------
catch_fish <- read_excel("fish_species/catch_fish.xlsx") |> 
  clean_names() %>% 
  replace(is.na(.), 0) |> 
  select(species)

# ----------------------------------------------------------
# Load predicted nutrient data for Brazilian species
# ----------------------------------------------------------

NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_BRAZIL <- read_csv("fish_species/NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_BRAZIL.csv") |> 
  clean_names() |> 
  select(1, 6:26)

# Merge catch list with predicted nutrient data
data <- left_join(catch_fish, NUTRIENT_PREDICTED_DATA_OF_SPECIES_IN_BRAZIL,
                  by=join_by(species==scientific_name)) |> 
  drop_na()

# -------------------------------------------------------------

# create directory to host the results
# dir.create("output")
# 
# # load fisheries data (Freire et al. 2021)
# fisheries <- read.xlsx (here ("data", "FINAL_RECONSTRUCTED_Brazil_1950_2015_CommercialEtapaII_04072021_IP_Freire.xlsx"),
#                         sheet = 2)
# 
# # adjust name
# fisheries$Sector [which(fisheries$Sector == "industrial (LS, C)")] <- "Industrial (LS, C)"
# 
# 
# 
# 
# # load data of Pinheiro et al. 2018 (BR reef fish)
# reef_fish <- read.csv (here ("data","brazilian-reef-fish-table-04-mar-18-website.xlsx - Database.csv"))
# reef_fish<-reef_fish[which(reef_fish$Relation == "RES"),] # REef fish  (RESident fish)
# 
# # mistake on the database
# reef_fish$Genus [grep ("Ocyurus chrysurus", reef_fish$Species)] <- "Ocyurus"



# ----------------------------------------------------------
# Load trait dataset (Quimbayo et al.)
# ----------------------------------------------------------

traits<- read_excel("fish_species/data/Atributos_especies_Atlantico_&_Pacifico_Oriental_2020_04_28.xlsx")



traits$Genus <- firstup(traits$Genus)  



# est_spp <- read.csv(file = "Valor_Est_All.csv", 
#                     h = T, sep = ";") |> 
#   filter(type.picture == "bony.fish") |>
#   select(Answer, ratio) |> 
#   rename(Name = Answer)
# 
# est_spp$Name <- gsub("_", " ", est_spp$Name)
# 
# est_spp$Name <- gsub("Chilomycterus spinosus", "Chilomycterus spinosus spinosus", est_spp$Name)
# 
# est_spp$Name <- gsub("Mycteroperca marginata", "Epinephelus marginatus", est_spp$Name)
# 
# est_spp$Name <- gsub("Centropyge aurantanotus", "Centropyge aurantonotus", est_spp$Name)

# ajusts  
traits$Body_size <- as.numeric(gsub (",",".",traits$Body_size))
traits$Trophic_level <- as.numeric(gsub (",",".",traits$Trophic_level))
traits$Depth_range <- as.numeric(gsub (",",".",traits$Depth_range))
traits$Depth_max<-as.numeric(gsub (",",".",traits$Depth_max))
traits$Depth_min<-as.numeric(gsub (",",".",traits$Depth_min))

# average depth
traits$Depth_mean<- apply (cbind (traits$Depth_max,  
                                  traits$Depth_min),1,mean,na.rm=T)

traits <- traits |> 
  select (Name, Body_size, Trophic_level, Diet,
          # Depth_min,Depth_max,Depth_range,
          # Depth_mean,
          # Diel_activity, Size_group, 
          # Level_water, Home_range
          )

# ----------------------------------------------------------
# Extract species names from nutrient dataset
# ----------------------------------------------------------

dt_nms <- data |> 
  select(species)

fish_traits <- inner_join(traits, dt_nms, 
                         by = join_by(Name == species))



# write.xlsx(x = NUTRIENT_SPECIES_IN_BRAZIL, file = "fish_species/NUTRIENT_SPECIES_IN_BRAZIL.xlsx")


NUTRIENT_SPECIES_IN_BRAZIL <- read_csv("fish_species/NUTRIENT_SPECIES_IN_BRAZIL.csv") |> 
  clean_names() |> 
  select(1, 6:26) |> 
  rename(species = scientific_name)

fish_nutris <- read_excel("fish_species/fish_nutris.xlsx", 
                          col_types = c("text", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))

data_fish <- bind_rows(fish_nutris, NUTRIENT_SPECIES_IN_BRAZIL)


dt_nms <- data_fish |> 
  select(species)

fish_traits <- inner_join(traits, dt_nms, 
                          by = join_by(Name == species))

#--------------------------------

# dtf <- left_join(data_fish, fish_traits,
#                  by = join_by(species==Name)) |> 
#   select(1, 23:25)

#write.xlsx(x = dtf, file = "fish_species/dft.xlsx")

dft <- read_excel("fish_species/dft_ok.xlsx")


all_fishes_fb <- read_csv("all_fishes_fb.csv")

all_fishes_fb <- all_fishes_fb |> 
  select(spp, Vulnerability)

all_fishes_fb$spp <- gsub("_", " ", all_fishes_fb$spp)

#dados$especie <- paste(dados$genero, dados$epiteto)

fishes_fb <- left_join(dft, all_fishes_fb,
                       by = join_by(species==spp))
#----------------------------------
#peixes mais nutritivos
library(FactoMineR)
library(factoextra)

dt_fish <- data_fish |> 
  select(species, contains("100g")) |> 
  select(-protein_g_100g, -vitamin_a_mg_100g)

dt_fish <- read_excel("fish_species/dt_fish_ok.xlsx")

dt_fish_ntr <- dt_fish[, 2:6]
dt_fish_ntr <- as.data.frame(dt_fish_ntr)
rownames(dt_fish_ntr) <- dt_fish$species

dt_fish_stnd <- decostand(dt_fish_ntr, method = "standardize")


pca.p <- PCA(X = dt_fish_stnd, 
             #scale.unit = TRUE, 
             graph = FALSE)

print(row.names(dt_fish_stnd))
pca.p$eig

fishes_fb$pca_axione <- round(pca.p$ind$contrib[,1], 4)

fishes_fb$top_nutri <- round(rowSums(dt_fish_stnd), 3)

unique(fishes_fb$Diet)

#----------Nutrient density-------------

# Ingestão Diária Recomendada para mulheres adultas (19–50 anos)
idr <- list(
  Calcium_mg = 1000,
  Iron_mg = 7.05,
  Selenium_ug = 45,
  Zinc_mg = 8.1,
  omega_3_g = 1.35
)

nutrients_percent <- dt_fish %>%
  mutate(
    Calcium = 100 * calcium_mg_100g / idr$Calcium_mg,
    Iron = 100 * iron_mg_100g / idr$Iron_mg,
    Zinc = 100 * zinc_mg_100g / idr$Zinc_mg,
    Selenium = 100 *selenium_mg_100g / idr$Selenium_ug,
    Omega3 = 100* total_omega_3_pufa_g_100g / idr$omega_3_g
  ) %>%
  select(species, Calcium, Iron, Zinc, Selenium, Omega3) %>%   # aqui já ajusta a ordem
  pivot_longer(cols = -species, names_to = "Nutrient", values_to = "Percent_IDR") %>%
  mutate(
    Percent_pad = ifelse(Percent_IDR > 100, 100, Percent_IDR),
    Nutrient = factor(Nutrient, levels = c("Calcium", "Iron", "Zinc", "Selenium", "Omega3")) # ordem fixa
  )

densidade_capped <- nutrients_percent %>%
  group_by(species) %>%
  summarise(
    densidade_mean = mean(Percent_pad, na.rm = TRUE),
    densidade_sd   = sd(Percent_pad, na.rm = TRUE),
    n_nutrientes   = sum(!is.na(Percent_pad)),
    .groups = "drop"
  ) %>% select(species, densidade_mean)


fishes_fba <- left_join(fishes_fb, densidade_capped)
#----------------------------------------
#openxlsx::write.xlsx(fishes_fba, "fish_species/fishes_fb.xlsx")
