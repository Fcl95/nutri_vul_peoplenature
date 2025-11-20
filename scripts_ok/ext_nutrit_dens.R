# Dados de exemplo para replicar os cálculos
traits <- read_excel("fish_species/fishes_fb.xlsx")  # Substitua com seus dados reais
nutris <- read_excel("fish_species/dt_fish_ok.xlsx")
#colnames(nutris) <- c("trait1", "trait2", "trait3", "trait4", "trait5")

#traits$f_ori <- oriS

# Calcular a distância e a PCoA
distance <- daisy(nutris[,-1], metric = "euclidean")
PCOA <- pcoa(distance, correction="none")
NoD <- 4
Axes <- PCOA$vectors[,1:NoD]


# Simulação para perda de espécies
RI <- traits
RI<-RI["densidade_mean"] #0ou trocar por f_ori
ERS <- cbind(Axes, RI)
NoS <- 116

# Cenário de perda das espécies mais raras
Simulat_rare <- ERS[order(ERS[,"densidade_mean"]),]
tr_rare <- Simulat_rare[,1:NoD]
FRic_Simulat_rare <- numeric(NoS)
for (i in 1:NoS) {
  tr_rare <- tr_rare[-1,]
  FRic_Simulat_rare[i] <- convhulln(tr_rare,"FA")$vol
}

# Cenário de perda das espécies mais comuns
Simulat_comm <- ERS[order(ERS[,"densidade_mean"], decreasing = TRUE),]
tr_comm <- Simulat_comm[,1:NoD]
FRic_Simulat_comm <- numeric(NoS)
for (i in 1:NoS) {
  tr_comm <- tr_comm[-1,]
  FRic_Simulat_comm[i] <- convhulln(tr_comm,"FA")$vol
}

# Modelo nulo
randFRic <- matrix(NA, nrow = NoS, ncol = 1000)
for (j in 1:1000) {
  dataperm <- ERS
  dataperm[,"densidade_mean"] <- sample(dataperm[,"densidade_mean"])
  Null_Simulat <- dataperm[order(dataperm[,"densidade_mean"]),]
  Null_tr <- Null_Simulat[,1:NoD]
  for (i in 1:NoS) {
    Null_tr <- Null_tr[-1,]
    randFRic[i, j] <- convhulln(Null_tr, "FA")$vol
  }			
}
Null_FRic <- data.frame(
  median = apply(randFRic, 1, median),
  q025 = apply(randFRic, 1, function(x) quantile(x, probs = 0.025)),
  q975 = apply(randFRic, 1, function(x) quantile(x, probs = 0.975))
)

# Normalizar dados
Spp_erosion <- 1:NoS
df <- data.frame(
  Spp_erosion = Spp_erosion,
  Null_median = Null_FRic$median / max(Null_FRic$median),
  Null_q025 = Null_FRic$q025 / max(Null_FRic$q025),
  Null_q975 = Null_FRic$q975 / max(Null_FRic$q975),
  FRic_Simulat_rare = FRic_Simulat_rare / max(FRic_Simulat_rare),
  FRic_Simulat_comm = FRic_Simulat_comm / max(FRic_Simulat_comm)
)

# Transformar em formato longo para ggplot2
df_long <- df %>%
  pivot_longer(cols = c(FRic_Simulat_rare, FRic_Simulat_comm), names_to = "scenario", values_to = "FRic")

# Adicionar dados do modelo nulo
df_long <- df_long %>%
  bind_rows(data.frame(Spp_erosion = Spp_erosion, scenario = "Null_median", FRic = df$Null_median))

# Plotar o gráfico
(tnutri_plot <- ggplot(df_long, aes(x = Spp_erosion, y = FRic, color = scenario, linetype = scenario)) +
    # Faixa de confiança do modelo nulo
    geom_ribbon(data = df, aes(x = Spp_erosion, ymin = Null_q025, ymax = Null_q975), 
                fill = "#ffeda0", alpha = 0.7, inherit.aes = FALSE) +
    
    # Linhas para os três cenários e o modelo nulo
    geom_line(size = 1.2) +
    
    # Definir cores e estilos de linha
    scale_color_manual(values = c("FRic_Simulat_comm" = "#bd0026", 
                                  "FRic_Simulat_rare" = "black", 
                                  "Null_median" = "#fed976"),
                       labels = c(
                         "Higher to lower", 
                         "Lower to higher",
                         "Radomly")) +
    scale_linetype_manual(values = c("FRic_Simulat_comm" = "solid", 
                                     "FRic_Simulat_rare" = "dashed", 
                                     "Null_median" = "solid"),
                          labels = c(
                            "Higher to lower", 
                            "Lower to higher",
                            "Radomly")) +
    
    # Personalizar rótulos e tema
    labs(x = "Species erosion (%)", y = " ", color = "Scenario", 
         linetype = "Scenario", 
         title = "Nutrient density") +
    
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
      axis.text.y = element_blank()
    )
)


#------------------------------

library(patchwork)


(maxlen_plot + troph_plot) / (vuln_plot + tnutri_plot) +
  #plot_layout(axis_titles = "collect") +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')
#-----------------------------------


pcoa_eixos <- as.data.frame(PCOA$vectors[, 1:2])  


pcoa_eixos <- data.frame(spp = nutris$species, pcoa_eixos)


pca_values <- 
  prcomp(nutris[, c(2:6)], center = TRUE, scale = TRUE)


pcoa_eixos <- data.frame(spp = nutris$species,pca_values$x[, 1:2])

colnames(pcoa_eixos) <- c("spp", "Axis.1" ,"Axis.2" )

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
  geom_text(aes(label = spp)) +
  theme_classic() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.9, 0.85),
        legend.title = element_text(size = 10))
