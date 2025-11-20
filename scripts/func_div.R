library(FactoMineR)
library(factoextra)
library(ggpubr)
library(FD)
library(tidyverse)
library(readxl)
library(patchwork)
library(sp)
library(funspace)

#-----------data-----------------------

food_source_nutri <- read_excel("fish_species/food_source_nutri.xlsx") |> 
  column_to_rownames(var = "food_resource")

colnames(food_source_nutri) <- c("Calcium", "Iron", "Omega-3", 
                                 "Selenium", "Zinc")
#food_source_nutri <- food_source_nutri[c(-35, -42),]
#----------------------------------


name_ax <- data.frame(axi = c(rep("OABF", 8), rep("Fish", 36)))

food_source_nutri_b <- data.frame(food_t = name_ax$axi, food_source_nutri)


pca_food <- princomp(scale(food_source_nutri))




fun_food <- funspace(x = pca_food, PCs = c(1, 2), threshold = 0.99, 
                     n_divisions = 200)

par(bty = "o", col.axis = "black", lty = 1, mgp = c(3, 0.3, 0))
plot(fun_food, type = "global", quant.plot = TRUE, 
     quant.lwd = 2, pnt = TRUE, pnt.cex = 0.5,
     pnt.col = rgb(0.1, 0.8, 0.2, alpha = 0.2), 
     arrows = TRUE, arrows.col = "black", arrows.length = 0.7, 
     #axis.title.x = "PCoA 1 (26,0% )", axis.title.y = "PCoA 2 (19,6%)",
     #xlim = c(-2, 2), ylim = c(-1, 1)
)

# text(eixos_north[, 1]*2, eixos_north[, 2]*2,
#      labels = rownames(eixos_north), cex = 0.8, col = "black")

# mds <- prov_north_pcoa[["CA"]][["u"]]
# mds <- mds[, c(1, 2]
#-----------------------------------

fun_food_grp <- funspace(x=pca_food, PCs=c(1,2), threshold = 0.99,
                         group.vec=food_source_nutri_b$food_t, 
                         n_divisions=300)


par(bty = "o", col.axis = "black", lty = 1, mgp = c(3, 0.2, 0))
plot(fun_food_grp, type = "groups", globalContour=T, 
     quant.plot = TRUE, quant.lwd = 2, quant.lty = 6, 
     pnt = TRUE, pnt.cex = 0.8,
     pnt.col = rgb(0.1, 0.8, 0.2, alpha = 0.3), 
     arrows = TRUE, axis.title.line= 1.4, 
     globalContour.lwd = 2, globalContour.lty = 1,
     #axis.title.x = "PCo 1 (25,8% )", axis.title.y = "PCo 2 (22,1%)",
     #xlim = c(-2, 2), ylim = c(-1, 1)
)

summary(fun_food_grp)
