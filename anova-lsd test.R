# Load necessary libraries
install.packages(c("agricolae", "dplyr", "tidyr", "readr"), dependencies = TRUE)
library(agricolae)
library(dplyr)
library(tidyr)
library(readr)

# Read your data (change the path if needed)
data <- read.csv("disease_data.csv")  # Make sure this CSV has Isolate.ID, R1, R2, R3 columns

# Convert to long format for ANOVA
long_data <- data %>%
  pivot_longer(cols = starts_with("R"), names_to = "Replication", values_to = "Severity")

# Run ANOVA
anova_model <- aov(Severity ~ Isolate.ID, data = long_data)
anova_summary <- summary(anova_model)
print(anova_summary)

# Save ANOVA summary as text file
capture.output(anova_summary, file = "anova_summary.txt")

# LSD Test
lsd_result <- LSD.test(anova_model, "Isolate.ID", p.adj = "none")
print(lsd_result)

# Save LSD means and group letters
write_csv(as.data.frame(lsd_result$groups), "lsd_group_letters.csv")

# Save means and comparison matrix (optional)
write_csv(as.data.frame(lsd_result$means), "lsd_means.csv")
write_csv(as.data.frame(lsd_result$comparison), "lsd_pairwise_comparisons.csv")

# Optional: Save barplot (with group letters)
png("lsd_group_plot.png", width = 1000, height = 600)
bar.group(lsd_result$means, 
          ylim = c(0,10),
          density = 8,
          col = "lightblue",
          border = "blue",
          las = 2,
          ylab = "Disease Severity")
dev.off()
