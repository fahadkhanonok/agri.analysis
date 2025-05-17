# Load necessary libraries
library(tidyverse)
library(agricolae)

# Read the data
data <- read.csv("peh_data.csv")

# View the structure of the dataset
head(data)

# Reshape data to long format for ANOVA
long_data <- data %>%
  select(Isolate.ID, R1, R2, R3) %>%
  pivot_longer(cols = c(R1, R2, R3), names_to = "Replicate", values_to = "Value")

# Convert Isolate.ID to a factor
long_data$Isolate.ID <- as.factor(long_data$Isolate.ID)

# ANOVA
anova_model <- aov(Value ~ Isolate.ID, data = long_data)
anova_summary <- summary(anova_model)

# Save ANOVA summary to text
capture.output(anova_summary, file = "anova_summary.txt")

# LSD Test
lsd_result <- LSD.test(anova_model, "Isolate.ID", p.adj = "none")  # No adjustment
lsd_result_output <- as.data.frame(lsd_result$groups)

# Save LSD results to CSV
write.csv(lsd_result_output, "lsd_results.csv", row.names = TRUE)

# Also save long format data for checking
write.csv(long_data, "long_format_data.csv", row.names = FALSE)

# Print basic outputs
print(anova_summary)
print(lsd_result_output)
