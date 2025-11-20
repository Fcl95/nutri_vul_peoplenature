#===============================================================
# Functional Space Analysis of Nutritional Traits in Food Sources
#---------------------------------------------------------------
# This script:
# 1) Imports nutritional data for different food sources
# 2) Performs PCA on standardized nutrient values
# 3) Computes global functional space using the 'funspace' package
# 4) Computes functional space separated by groups (fish vs OABF)
# 5) Produces visualizations of functional diversity
#===============================================================


#----------------------- Load packages --------------------------

library(FD)
library(tidyverse)
library(readxl)
library(patchwork)
library(sp)
library(funspace)

#--------------------- Import and prepare data ------------------

# Load nutrient dataset (rows = food sources, columns = nutrients)

food_source_nutri <- read_excel("fish_species/food_source_nutri.xlsx") |> 
  column_to_rownames(var = "food_resource")

# Rename columns for cleaner interpretation
colnames(food_source_nutri) <- c("Calcium", "Iron", "Omega-3", 
                                 "Selenium", "Zinc")
#food_source_nutri <- food_source_nutri[c(-35, -42),]
#----------------------------------

# Create a grouping vector identifying the origin of each food source.
# "OABF" = non-fish food sources; "Fish" = fish species
# (Modify according to your metadata)
name_ax <- data.frame(axi = c(rep("OABF", 8), rep("Fish", 36)))

# Bind the group names with the nutrient matrix
food_source_nutri_b <- data.frame(food_t = name_ax$axi, food_source_nutri)

#---------------------- Principal Component Analysis ------------

# Perform PCA on scaled nutritional variables
# PCA will reduce multicollinearity and allow mapping into 2D space
pca_food <- princomp(scale(food_source_nutri))


#---------------------- Global Functional Space -----------------

# Estimate functional space using first 2 PCA axes
# threshold = 0.99 keeps axes that explain 99% of variability
# n_divisions = grid resolution for functional volume estimation

fun_food <- funspace(x = pca_food, PCs = c(1, 2), threshold = 0.99, 
                     n_divisions = 200)

# Plot global functional space occupied by all food sources
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
#----------------- Group-based Functional Space -----------------

# Compute functional space for groups (Fish vs OABF)
fun_food_grp <- funspace(x=pca_food, PCs=c(1,2), threshold = 0.99,
                         group.vec=food_source_nutri_b$food_t, 
                         n_divisions=300)


# Plot group-level functional space
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

# Summary of functional space metrics (overlap, volume, centroid positions)
summary(fun_food_grp)
