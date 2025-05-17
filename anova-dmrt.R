# Load required libraries
library(agricolae)
library(dplyr)

# Read the dataset
data <- read.csv("banana.cal.csv")

# Convert necessary columns to factors
data$district <- as.factor(data$district)
data$variety <- as.factor(data$variety)
data$genome <- as.factor(data$genome)

# ---- ANOVA for d.i ----
model_di <- aov(d.i ~ district * variety * genome, data = data)
anova_di_summary <- summary(model_di)

# Save ANOVA result for d.i
sink("anova_d.i_results.txt")
cat("----- ANOVA for d.i -----\n")
print(anova_di_summary)
sink()

# Perform DMRT for variety (example, if it's significant; you can check summary to modify)
dmrt_di_variety <- with(data, 
                        DMRT(d.i, variety, 
                             DFerror = df.residual(model_di), 
                             MSerror = deviance(model_di) / df.residual(model_di)))

# Save DMRT result for d.i
sink("dmrt_d.i_variety.txt")
cat("----- DMRT for d.i by variety -----\n")
print(dmrt_di_variety)
sink()

# ---- ANOVA for d.s ----
model_ds <- aov(d.s ~ district * variety * genome, data = data)
anova_ds_summary <- summary(model_ds)

# Save ANOVA result for d.s
sink("anova_d.s_results.txt")
cat("----- ANOVA for d.s -----\n")
print(anova_ds_summary)
sink()

# Perform DMRT for district (example, if it's significant; adjust as needed)
dmrt_ds_district <- with(data, 
                         DMRT(d.s, district, 
                              DFerror = df.residual(model_ds), 
                              MSerror = deviance(model_ds) / df.residual(model_ds)))

# Save DMRT result for d.s
sink("dmrt_d.s_district.txt")
cat("----- DMRT for d.s by district -----\n")
print(dmrt_ds_district)
sink()
