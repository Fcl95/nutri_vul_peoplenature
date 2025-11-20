############################################################
# Fish Species Erosion Simulation
# Author: Fabricio Albuquerque
# Description:
#   This script calculates the loss of nutritional richness (NRic)
#   under different species-loss scenarios (from small to large
#   species, and vice-versa), and compares them against a null
#   model generated through randomization.
#   Includes PCoA computation, convex hull volumes, null model,
#   normalization and visualization.
############################################################

# ----------------------------------------------------------
# Load required packages
# ----------------------------------------------------------
library(cluster)
library(FD)
library(ape)
library(geometry)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(readxl)

# ----------------------------------------------------------
# Load datasets
# Replace the file paths if needed
# traits: species trait dataset (must include Body_size)
# nutris: matrix of nutritional variables used for distance
# ----------------------------------------------------------
traits <- read_excel("fish_species/fishes_fb.xlsx") 
nutris <- read_excel("fish_species/dt_fish_ok.xlsx")
#colnames(nutris) <- c("trait1", "trait2", "trait3", "trait4", "trait5")

# ----------------------------------------------------------
# Compute trait distance matrix and perform PCoA
# We remove the first column assuming it is species ID
# ----------------------------------------------------------

distance <- daisy(nutris[,-1], metric = "euclidean")
PCOA <- pcoa(distance, correction="none")

# Number of PCoA axes to retain
NoD <- 4
Axes <- PCOA$vectors[,1:NoD]


# ----------------------------------------------------------
# Prepare data for species loss simulations
# ----------------------------------------------------------

RI <- traits
RI<-RI["Body_size"] # Retain only Body_size trait
ERS <- cbind(Axes, RI)  # Combine PCoA axes + Body_siz

# Number of species
NoS <- 116

# ----------------------------------------------------------
# Scenario 1: Remove species from smallest to largest body size
# ----------------------------------------------------------
Simulat_rare <- ERS[order(ERS[,"Body_size"]),] # Increasing order
tr_rare <- Simulat_rare[,1:NoD]
FRic_Simulat_rare <- numeric(NoS)

for (i in 1:NoS) {
  tr_rare <- tr_rare[-1,] # Remove one species at a time
  FRic_Simulat_rare[i] <- convhulln(tr_rare,"FA")$vol
}

# ----------------------------------------------------------
# Scenario 2: Remove species from largest to smallest body size
# ----------------------------------------------------------
Simulat_comm <- ERS[order(ERS[,"Body_size"], decreasing = TRUE),]
tr_comm <- Simulat_comm[,1:NoD]
FRic_Simulat_comm <- numeric(NoS)
for (i in 1:NoS) {
  tr_comm <- tr_comm[-1,]
  FRic_Simulat_comm[i] <- convhulln(tr_comm,"FA")$vol
}

# ----------------------------------------------------------
# Null model: Randomize body size 1000 times
# ----------------------------------------------------------
randFRic <- matrix(NA, nrow = NoS, ncol = 1000)
for (j in 1:1000) {
  dataperm <- ERS
  dataperm[,"Body_size"] <- sample(dataperm[,"Body_size"])
  Null_Simulat <- dataperm[order(dataperm[,"Body_size"]),]
  Null_tr <- Null_Simulat[,1:NoD]
  for (i in 1:NoS) {
    Null_tr <- Null_tr[-1,]
    randFRic[i, j] <- convhulln(Null_tr, "FA")$vol
  }			
}
# Summarize null model (median + 95% CI)
Null_FRic <- data.frame(
  median = apply(randFRic, 1, median),
  q025 = apply(randFRic, 1, function(x) quantile(x, probs = 0.025)),
  q975 = apply(randFRic, 1, function(x) quantile(x, probs = 0.975))
)

# ----------------------------------------------------------
# Normalize FRic curves (range 0–1)
# ----------------------------------------------------------
Spp_erosion <- 1:NoS
df <- data.frame(
  Spp_erosion = Spp_erosion,
  Null_median = Null_FRic$median / max(Null_FRic$median),
  Null_q025 = Null_FRic$q025 / max(Null_FRic$q025),
  Null_q975 = Null_FRic$q975 / max(Null_FRic$q975),
  FRic_Simulat_rare = FRic_Simulat_rare / max(FRic_Simulat_rare),
  FRic_Simulat_comm = FRic_Simulat_comm / max(FRic_Simulat_comm)
)

# ----------------------------------------------------------
# Convert to long format for ggplot2
# ----------------------------------------------------------
df_long <- df %>%
  pivot_longer(cols = c(FRic_Simulat_rare, FRic_Simulat_comm), names_to = "scenario", values_to = "FRic")

# Adicionar dados do modelo nulo
df_long <- df_long %>%
  bind_rows(data.frame(Spp_erosion = Spp_erosion, scenario = "Null_median", FRic = df$Null_median))

# Plotar o gráfico
(maxlen_plot <- ggplot(df_long, aes(x = Spp_erosion, y = FRic, color = scenario, linetype = scenario)) +
  # Faixa de confiança do modelo nulo
  geom_ribbon(data = df, aes(x = Spp_erosion, ymin = Null_q025, ymax = Null_q975), 
              fill = "#ffeda0", alpha = 0.7, inherit.aes = FALSE) +
  
  # Linhas para os três cenários e o modelo nulo
  geom_line(size = 1.2) +
  
  # Definir cores e estilos de linha
  scale_color_manual(values = c("FRic_Simulat_rare" = "black", 
                                "FRic_Simulat_comm" = "#bd0026", 
                                "Null_median" = "#fed976"),
                     labels = c(
                       "Higher to lower",
                       "Lower to higher",
                       "Radomly")) +
  scale_linetype_manual(values = c("FRic_Simulat_rare" = "dashed", 
                                   "FRic_Simulat_comm" = "solid", 
                                   "Null_median" = "solid"),
                        labels = c(
                          "Higher to lower",
                          "Lower to higher", 
                          "Radomly")) +
  
  # Personalizar rótulos e tema
  labs(x = " ", y = "NRic", color = "Scenario", linetype = "Scenario", 
       title = "Body lenght") +
  
  # Ajustar escala e rótulos do eixo x
  scale_x_continuous(breaks = c(0, NoS/4, NoS/2, NoS*3/4, NoS), 
                     labels = c(0, 25, 50, 75, 100)) +
  
  # Tema e posição da legenda
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = c(0.85, 0.85),  # Posição da legenda dentro do gráfico
    #legend.background = element_rect(fill = "white", color = "black", size = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.text.x = element_blank(),
  )
)


#The same steps are used in other species extinction scenarios in the scripts: ext_nutrit_dens, ext_troph, ext_vuln