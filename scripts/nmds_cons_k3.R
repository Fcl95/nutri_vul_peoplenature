library(tidyverse)
library(readxl)
library(ggpmisc)
library(janitor)
library(patchwork)
library(vegan)
#-------------------------
setwd("~/Doutorado/Nutri_vuln")
source("pairwise.adonis.R")




consumo_pesc <- read_excel("C:/Users/fca95/OneDrive/Documentos/Doutorado/Nutri_vuln/fish_species/Quest_consumo_pesc.xlsx", 
                           sheet = "food_intake") |> 
  clean_names() %>% 
  replace(is.na(.), 0)

head(consumo_pesc)
str(consumo_pesc)

#openxlsx::write.xlsx(consumo_pesc, "cons_pesc.xlsx")
# turn data frame into matrix so it functions in the vegan package

consumo_pesc_community <- consumo_pesc[,7:15]

consumo_pesc_matrix <- as.matrix(consumo_pesc_community)

#consumo_pesc$riq <- specnumber(consumo_pesc[,7:15])

# fazendo o nMDS usando a matriz de distancia de bray-curtis

## Composição de espécies padronizar com método de Hellinger 
cons.hel <- decostand(x = consumo_pesc[,7:15], method = "hellinger") 

#cons.hel <- decostand(x = consumo_pesc[,8:14], method = "hellinger") 

## Matriz de distância com método Bray-Curtis 
cons.dis <- vegdist(x = cons.hel, method = "bray") #tentar outros method


# check model assumptions
# check for multivariate homogeneity of group variances

# use betadisper test to check for multivariate homogeneity of group variances
inv.dispersion <- betadisper(cons.dis, group=consumo_pesc$municipality)
permutest(inv.dispersion)
# p = 0.393, ok



#nMDS
cons1 <- metaMDS(cons.dis, k = 3, trymax = 999, distance = "bray")  
inv.NMDS <- metaMDS(cons.dis, k = 3, previous.best = cons1, distance = "bray")  

##
# inv.NMDS <- metaMDS(consumo_pesc_matrix, distance = "bray", 
#                     #k = 3, autotransform = TRUE, 
#                     trymax=500)

inv.NMDS$stress
# 0.14, ideal ser menor que 0.2
#sem peixe e crustaceo 0.19, k=2


morisita_horn_dist <- vegdist(cons.dis, method = "horn")

#-------PERMANOVA para testar diferenças entre os grupos------

inv_permanova <- adonis2(as.matrix(consumo_pesc[,7:15]) ~ municipality,
                         consumo_pesc, permutations = 999, 
                         #strata = consumo_pesc$municipality,
                         method = "bray"
                         #method = "jaccard"
)


inv.NMDS$points


pairwise_result <- pairwise.adonis(x = consumo_pesc[,7:15],
                factors = consumo_pesc$municipality,
                sim.method = "bray",
                perm = 999)


# Visualizar os resultados
print(pairwise_result)

# pairwise_df <- as.data.frame(pairwise_result)
# openxlsx::write.xlsx(pairwise_df, "pairwise_df.xlsx")

#------------------Fazendo o grafico do nMDS----------------------------------

dat.graf <- data.frame(inv.NMDS$points, 
                       muni = consumo_pesc$municipality)

head(dat.graf)

#----------------mds1 and mds2-----------------------------

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


mds12 <- ggplot() +
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
  labs(x = "", y = "NMDS2", 
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
        panel.grid.minor = element_blank()
  ) +
annotate("text", x = 0.6, y = 0.5,
          #label = "italic(R) ^ 2 == 0.75"), colocar r2
         label = "atop(italic(stress) == 0.14, italic(P) == 0.001)",
         parse = TRUE)

# +
#   geom_point(data = cnt_bf, aes(x = MDS1, y = MDS2), 
#              colour = "yellow", size = 3) 

# cnt_bf <- grp.bf %>%
#   summarise_at(c("MDS1", "MDS2"), mean, na.rm = TRUE)

#---------------mds1 and mds3----------------

## Definir os grupos ("HULL") para serem categorizados no gráfico 
grp.bf <- dat.graf[dat.graf$muni == "Baia Formosa", ] [chull(dat.graf[dat.graf$muni == "Baia Formosa", c("MDS1", "MDS3")]), ]


grp.tr <- dat.graf[dat.graf$muni == "Touros", ] [chull(dat.graf[dat.graf$muni == "Touros", c("MDS1", "MDS3")]), ]

grp.rf <- dat.graf[dat.graf$muni == "Rio do Fogo", ] [chull(dat.graf[dat.graf$muni == "Rio do Fogo", c("MDS1", "MDS3")]), ]

grp.ip <- dat.graf[dat.graf$muni == "Ipojuca", ] [chull(dat.graf[dat.graf$muni == "Ipojuca", c("MDS1", "MDS3")]), ]

grp.tm <- dat.graf[dat.graf$muni == "Tamandare", ] [chull(dat.graf[dat.graf$muni == "Tamandare", c("MDS1", "MDS3")]), ]

grp.sj <- dat.graf[dat.graf$muni == "Sao Jose da Coroa", ] [chull(dat.graf[dat.graf$muni == "Sao Jose da Coroa", c("MDS1", "MDS3")]), ]

## Combinar dados dos grupos para cada Convex Hull
hull.data <- rbind(grp.tr, grp.rf, grp.bf, grp.ip, grp.tm, grp.sj) 
head(hull.data)

## Plot
#nmds_d1 <-


mds13 <- ggplot() +
  geom_polygon(data = hull.data, 
               aes(x = MDS1, y = MDS3, fill = muni, group = muni),
               alpha = 0.3) +
  geom_point(data = dat.graf, aes(x = MDS1, y = MDS3, 
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
  labs(x = "NMDS1", y = "NMDS3", 
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
        panel.grid.minor = element_blank()
  )

#---------------mds2 and mds3----------------

## Definir os grupos ("HULL") para serem categorizados no gráfico 
grp.bf <- dat.graf[dat.graf$muni == "Baia Formosa", ] [chull(dat.graf[dat.graf$muni == "Baia Formosa", c("MDS2", "MDS3")]), ]


grp.tr <- dat.graf[dat.graf$muni == "Touros", ] [chull(dat.graf[dat.graf$muni == "Touros", c("MDS2", "MDS3")]), ]

grp.rf <- dat.graf[dat.graf$muni == "Rio do Fogo", ] [chull(dat.graf[dat.graf$muni == "Rio do Fogo", c("MDS2", "MDS3")]), ]

grp.ip <- dat.graf[dat.graf$muni == "Ipojuca", ] [chull(dat.graf[dat.graf$muni == "Ipojuca", c("MDS2", "MDS3")]), ]

grp.tm <- dat.graf[dat.graf$muni == "Tamandare", ] [chull(dat.graf[dat.graf$muni == "Tamandare", c("MDS2", "MDS3")]), ]

grp.sj <- dat.graf[dat.graf$muni == "Sao Jose da Coroa", ] [chull(dat.graf[dat.graf$muni == "Sao Jose da Coroa", c("MDS2", "MDS3")]), ]

## Combinar dados dos grupos para cada Convex Hull
hull.data <- rbind(grp.tr, grp.rf, grp.bf, grp.ip, grp.tm, grp.sj) 
head(hull.data)

## Plot
#nmds_d1 <-


mds23 <- ggplot() +
  geom_polygon(data = hull.data, 
               aes(x = MDS2, y = MDS3, fill = muni, group = muni),
               alpha = 0.3) +
  geom_point(data = dat.graf, aes(x = MDS2, y = MDS3, 
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
  labs(x = "NMDS2", y = "NMDS3", 
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
        panel.grid.minor = element_blank()
  )


mds12 + mds23 + mds13 + guide_area() +
  plot_layout(guides = 'collect')

#---------Rarefacao----------------

library(iNEXT)

pesc_rare <- consumo_pesc |> 
  select(id_annm, municipality, 7:15) |> 
  group_by(municipality) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
  
# need rownanems to work with lists so quick workaround to get rownames in data frame
pesc_rare1 <- as.data.frame(pesc_rare)
rownames(pesc_rare1) <- pesc_rare1$municipality
pesc_seqs_nt <- pesc_rare1[-1]

# turn into lists
pesc_seqs_list = as.data.frame(t(pesc_seqs_nt))
#pesc_seqs_list = as.data.frame(pesc_seqs_nt)
str(pesc_seqs_list)
pesc_list2 <- as.data.frame(pesc_seqs_list)

# pesc_rare |> 
#   pivot_wider(names_from = municipality, values_from = riq) |> 
#   select(2:7) %>% 
#   replace(is.na(.), 0)

# split dataframe into lists, with each species/population as an element
#pesc_list_split <- split(pesc_list2, rownames(pesc_list2))

# transpose each element
#pesc_list_t <- lapply(pesc_list_split, t)

####2. Data analysis####
# compute rarefaction curves by species/populations using iNEXT package


# rarefactioncoi <- iNEXT(pesc_list_split, q=0, datatype="abundance", size=NULL, endpoint=max(colSums(pesc_list2)), knots=50, se=TRUE, conf=0.95, nboot=100)
# 
# 
# 
# ggiNEXT(rarefactioncoi, type = 1) +  
#   geom_vline(xintercept = 47, lty = 2) +  
#   scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +  
#   scale_colour_manual(values = c("darkorange", "darkorchid",  
#                                  "cyan4", "royalblue", "red2", "yellow3")) +  
#   scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4",
#                                "royalblue", "red2", "yellow3")) +  
#   labs(x = "Número de refeições", y = " Riqueza alimentar") +  
#   theme_bw()
# #------------------------------------------------------
#rownames(pesc_rare) <- consumo_pesc$id_annm

#openxlsx::write.xlsx(x = pesc_rare, file = "fish_species/pesc_rare.xlsx")

# colSums(pesc_rare[,2:10], na.rm = TRUE)
# 
# pesc_rare <- as.matrix(pesc_rare)

pesc_seqs_list2 <- pesc_seqs_list %>%
  summarise_if(is.numeric, round)

colSums(pesc_seqs_list2, na.rm = TRUE)

rownames(pesc_seqs_list2) <- rownames(pesc_list2)

pesc_list3 <- as.matrix(pesc_seqs_list2)

#openxlsx::write.xlsx(pesc_list2, "pesclist2.xlsx")

pesc_list2 <- read_excel("pesclist2.xlsx")



pesc_list3 <- pesc_list2 %>%
  summarise_if(is.numeric, round)

rownames(pesc_list3) <- pesc_list2$food_tp

pesc_list3 <- as.matrix(pesc_list3)

head(pesc_list3)

#base em amostras
rslt_rare <- iNEXT(pesc_list3, q = 0,   
                             datatype = "incidence_freq", 
                   endpoint = 300)


ggiNEXT(rslt_rare, type = 1) +  
  geom_vline(xintercept = 51, lty = 2) +  
  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +  
  scale_colour_manual(values = c("darkorange", "darkorchid",  
                                 "cyan4", "royalblue", "red2", "yellow3")) +  
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4",
                               "royalblue", "red2", "yellow3")) +  
  scale_x_continuous(limits = c(0, 300),
                     breaks = seq(0, 400, by = 100)) +
  scale_y_continuous(limits = c(0, 10),
                     breaks = seq(0, 10, by = 2),
                     expand = c(0.01, 0)) +
  labs(x = "Número de refeições", y = " Riqueza alimentar") +  
  theme_classic() +
  theme(legend.position = c(0.9, 0.3),
        axis.ticks = element_blank())


# #---------------------------
# pesc_rare <- read_excel("fish_species/pesc_rare.xlsx")
# 
# pesc_rare_nv <- pesc_rare |> 
#   select(2:7)
# 
# rownames(pesc_rare_nv) <- pesc_rare$...1
# 
# bf_b <- pesc_rare_nv |> 
#   clean_names() |> 
#   filter(baia_formosa > 0)
# bf <- bf_b[,1]
# rownames(bf) <- row.names(bf_b)
# 
# #----------------
# rf_b <- pesc_rare_nv |> 
#   clean_names() |> 
#   filter(rio_do_fogo > 0)
# rf <- rf_b[,2]
# rownames(rf) <- row.names(rf_b)
# 
# #----------------
# tr_b <- pesc_rare_nv |> 
#   clean_names() |> 
#   filter(touros > 0)
# tr <- tr_b[,3]
# rownames(tr) <- row.names(tr_b)
# 
# #-----------------
# sj_b <- pesc_rare_nv |> 
#   clean_names() |> 
#   filter(sao_jose_da_coroa > 0)
# sj <- sj_b[,4]
# rownames(sj) <- row.names(sj_b)
# 
# #-------------
# tm_b <- pesc_rare_nv |> 
#   clean_names() |> 
#   filter(tamandare > 0)
# tm <- tm_b[,5]
# rownames(tm) <- row.names(tm_b)
# 
# #----------------
# ij_b <- pesc_rare_nv |> 
#   clean_names() |> 
#   filter(ipojuca > 0)
# ij <- ij_b[,6]
# rownames(ij) <- row.names(ij_b)
# 
# #-------------------
# 
# lista_rarefacao <- list(baia_formosa = as.integer(bf$baia_formosa),
#                         rio_do_fogo = as.integer(rf$rio_do_fogo),
#                         touros = as.integer(tr$touros),
#                         ipojuca = as.integer(ij$ipojuca),
#                         tamandere = as.integer(tm$tamandare),
#                         sao_jose_da_coroa = as.integer(sj$sao_jose_da_coroa))
# 
# ## Análise
# res_rarefacao_amostras <- iNEXT(lista_rarefacao, q = 0, 
#                                 datatype = "incidence_freq")
# 
# 
# ## Gráfico
# ggiNEXT(res_rarefacao_amostras , type = 1) + 
#   geom_vline(xintercept = 8, lty = 2) +
#   scale_linetype_discrete(name = "Método", labels = c("Interpolado", "Extrapolado")) +
#   scale_colour_manual(values = c("darkorange", "darkorchid",  
#                                  "cyan4", "royalblue", "red2", "yellow3")) +  
#   scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4",
#                                "royalblue", "red2", "yellow3")) +
#   labs(x = "Número de amostras", y = " Riqueza de espécies") +
#   theme_bw()
