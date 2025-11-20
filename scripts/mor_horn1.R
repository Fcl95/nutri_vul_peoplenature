
install.packages("boot")

library(boot)

# Aggregate data by municipality
aggregated_data <- consumo_pesc %>%
  group_by(municipality) %>%
  summarise(across(contains("intk"), sum))

vegdist(aggregated_data[1:2, 2:10], method = "horn")

# Function to calculate the Morisita-Horn index for two municipalities
calculate_morisita_horn <- function(data, index1, index2) {
  pair_data <- data[c(index1, index2), -1]  # Select rows for the two municipalities
  morisita_index <- vegdist(pair_data, method = "horn")
  as.numeric(morisita_index)
}

# Bootstrapping function to calculate confidence interval for Morisita-Horn index
bootstrap_morisita <- function(data, indices, index1, index2) {
  resampled_data <- data[indices, ]
  calculate_morisita_horn(resampled_data, index1, index2)
}

# Prepare an empty data frame to store results
results <- data.frame(Municipality1 = character(),
                      Municipality2 = character(),
                      Morisita_Horn = numeric(),
                      LCI = numeric(),
                      UCI = numeric(),
                      stringsAsFactors = FALSE)

# Get the number of municipalities
num_municipalities <- nrow(aggregated_data)

# Loop through each pair of municipalities
for (i in 1:(num_municipalities - 1)) {
  for (j in (i + 1):num_municipalities) {
    
    # Get names of the municipalities for labeling
    mun1 <- aggregated_data$municipality[i]
    mun2 <- aggregated_data$municipality[j]
    
    # Calculate Morisita-Horn index for the pair
    mh_index <- calculate_morisita_horn(aggregated_data, i, j)
    
    # Perform bootstrapping to calculate confidence intervals
    set.seed(123)  # For reproducibility
    bootstrap_results <- boot(data = aggregated_data[,-1], 
                              statistic = bootstrap_morisita, 
                              R = 1000, 
                              index1 = i, 
                              index2 = j)
    
    # Calculate 95% confidence intervals
    ci <- boot.ci(bootstrap_results, type = "perc")$percent[4:5]
    
    # Append results
    results <- rbind(results, data.frame(Municipality1 = mun1,
                                         Municipality2 = mun2,
                                         Morisita_Horn = mh_index,
                                         LCI = ci[1],
                                         UCI = ci[2]))
  }
}

# Print results
print(results)
