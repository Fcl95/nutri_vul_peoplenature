library(tidyverse)
library(readxl)
library(janitor)
library(patchwork)
#-----------------

traits <- read_excel("fish_species/fishes_fb.xlsx")

traits <- traits |> 
  mutate(bd_s = log(Body_size))
#---------------------

# reg_nutri <- lm(top_nutri ~ Vulnerability, data = traits)
# 
# summary(reg_nutri)

cor.test(traits$top_nutri, traits$Vulnerability, method = "pearson")


(p1 <- ggplot(traits, aes(x = Body_size, y = top_nutri)) + 
    geom_point(size = 3, alpha = 0.6) +
    geom_smooth(method = "lm", se = F, 
                linetype = "dashed", linewidth = 1.2) +
    labs(x = "Body length", y = "Nutritional value") +
    theme_classic(base_size = 14) +
    annotate("text", x = 220, y = 15, size = 5,
             label = "italic(p) == 0.1",
             parse = T) +
    annotate("text", x = 220, y = 13,size = 5, 
             label = "italic(R) == -0.14",
             parse = T)
)



(p2 <- ggplot(traits, aes(x = Trophic_level, y = top_nutri)) + 
    geom_point(size = 3, alpha = 0.6) +
    geom_smooth(method = "lm", se = F, 
                linetype = "dashed", linewidth = 1.2) +
    labs(x = "Trophic level", y = " ") +
    theme_classic(base_size = 14) +
    annotate("text", x = 4.2, y = 15, size = 5,
             label = "italic(p) == 0.002",
             parse = T) +
    annotate("text", x = 4.2, y = 13,size = 5, 
             label = "italic(R) == -0.27",
             parse = T)
)


(p3 <- ggplot(traits, aes(x = Vulnerability, y = top_nutri)) + 
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = F, 
              linetype = "dashed", linewidth = 1.2) +
  labs(y = " ") +
  theme_classic(base_size = 14) +
  annotate("text", x = 75, y = 15, size = 5,
           label = "italic(p) == 0.001\nitalic(R) == 0.3",
           parse = T) +
  annotate("text", x = 75, y = 13,size = 5, 
           label = "italic(R) == -0.3",
           parse = T)
)


p1 + p2 +p3
