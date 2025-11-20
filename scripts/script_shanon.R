# Load necessary libraries
library(vegan)  # for diversity calculations

# Load the data
#data <- read.csv('path_to_file.csv')

data <- consumo_pesc
# Function to calculate Shannon Index (H') for each municipality
calculate_shannon <- function(data) {
  
  # Aggregating by municipality, summing food consumption per food item
  food_data <- aggregate(. ~ municipality, data[, c('municipality', 'marine_fish_intk', 'tilapia_intk_year',
                                                    'chicken_intk', 'beef_intk', 'pork_intk', 'egg_intk',
                                                    'ultr_prcd_meat_intk', 'goat_intk_year', 'crustaceans_intk_month')], sum)
  
  # Calculating Shannon Index for each municipality
  food_data$shannon_index <- apply(food_data[, -1], 1, function(x) diversity(x, index = 'shannon'))
  
  return(food_data[, c('municipality', 'shannon_index')])
}

# Load your data and calculate the Shannon index
#data <- read.csv('path_to_file.csv')  # Replace with the path to your CSV file
shannon_results <- calculate_shannon(data)

# View the results
print(shannon_results)

# Write the results to a new CSV file
#write.csv(shannon_results, 'shannon_index_results.csv', row.names = FALSE)

var_muni <- read_excel("Doutorado/Nutri_vuln/fish_species/var_muni.xlsx")

shannon_results$municipality <- c("Baía Formosa", "Ipojuca", "Rio do Fogo",
                                  "São José da Coroa Grande", 
                                  "Tamandaré", "Touros")

var_muni_indx2 <- left_join(var_muni, shannon_results) |> 
  filter(municipality != "Ipojuca")
#------------------------------------

ggplot(var_muni_indx, aes(x = pib_per_cap, y = shannon_index)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_smooth(size = 1.2, linetype = "dashed", 
              method = "lm", se = F) +
  scale_x_continuous(limits = c(0, 160000)) +
  scale_y_continuous(limits = c(1.8,2.2)) +
  theme_classic() +
  labs(x = "PIB per capita", y = "Shannon index") +  
  theme_classic() +
  theme(axis.ticks = element_blank()) +
  annotate("text", x = 15000, y = 2.15,
           #label = "italic(R) ^ 2 == 0.75"), colocar r2
           label = "atop(italic(R) ^2 == 0.002, italic(p) == 0.92)",
           parse = TRUE)


summary(lm(var_muni_indx$shannon_index ~ var_muni_indx$pib_per_cap))          
