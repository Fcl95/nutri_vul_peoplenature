cons1 <- metaMDS(cons.dis, k = 2, trymax = 999, distance = "bray")  
inv.NMDS <- metaMDS(cons.dis, k = 2, previous.best = cons1, distance = "bray")  

##
# inv.NMDS <- metaMDS(consumo_pesc_matrix, distance = "bray", 
#                     #k = 3, autotransform = TRUE, 
#                     trymax=500)

inv.NMDS$stress


env <- decostand(consumo_pesc[,7:15], method = "hellinger")

colnames(env) <- c("Marine fish", "Tilapia", "Chicken", 
                   "Beef", "Pork", "Egg",
                   "Ultra-proc meat", "Goat", "Crustacenas")

en <- envfit(inv.NMDS, env, permutations = 999, na.rm = TRUE)

plot(inv.NMDS)
plot(en)

en_coord_cont <- as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)


ggplot() +
  geom_polygon(data = hull.data, 
               aes(x = MDS1, y = MDS2, fill = muni, group = muni),
               alpha = 0.3) +
  geom_point(data = dat.graf, aes(x = MDS1, y = MDS2, 
                                  color = muni, 
                                  shape = muni), 
             size = 4, alpha = 0.7) +
  geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2), 
               data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30")+
  geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30", 
            fontface = "bold", label = row.names(en_coord_cont)) + 
  scale_color_manual(labels = c(
    "Baía Formosa",
    "Ipojuca", 
    "Rio do Fogo",
    "S.J da Coroa Grande",
    "Tamandaré",
    "Touros"),
    values = c(
      "#b35806", "#f1a340", "#de2d26", "#2166ac", "#006d2c", "#542788")
  ) +
  scale_fill_manual(labels = c(
    "Baía Formosa",
    "Ipojuca", 
    "Rio do Fogo",
    "S.J da Coroa Grande",
    "Tamandaré",
    "Touros"),
    values = c(
      "#b35806", "#f1a340", "#de2d26", "#2166ac", "#006d2c", "#542788")
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
  theme_bw() +
  theme(legend.position = c(0.9, 0.1), 
        legend.justification = c(0.7, 0.3),
        legend.background =  element_rect( 
          fill = "transparent"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.7, "cm"),
        panel.grid = element_blank()
  ) +
  annotate("text", x = 0.6, y = 0.5,
           #label = "italic(R) ^ 2 == 0.75"), colocar r2
           label = "atop(italic(stress) == 0.21, italic(P) == 0.001)",
           parse = TRUE)
