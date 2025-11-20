
morisita_horn <- vegdist(cons.hel, method = "horn")

library(boot)

# Define a function to calculate the Morisita-Horn index
morisita_horn_fun <- function(data, indices) {
  d <- data[indices, ]
  return(as.numeric(vegdist(d, method = "horn")))
}

# Perform bootstrapping
results <- boot(data, morisita_horn_fun, R = 1000)

# Calculate 95% confidence intervals
ci <- boot.ci(results, type = "perc")

lci <- ci$percent[4]
uci <- ci$percent[5]

print(paste("Morisita-Horn Index:", mean(results$t)))
print(paste("95% Confidence Interval: [", lci, ", ", uci, "]", sep = ""))
