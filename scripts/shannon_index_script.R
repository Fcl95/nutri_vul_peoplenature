
# Load necessary libraries
library(vegan)  # for diversity calculations

# Load the data
data <- read.csv('path_to_file.csv')

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
data <- read.csv('path_to_file.csv')  # Replace with the path to your CSV file
shannon_results <- calculate_shannon(data)

# View the results
print(shannon_results)

# Write the results to a new CSV file
write.csv(shannon_results, 'shannon_index_results.csv', row.names = FALSE)
