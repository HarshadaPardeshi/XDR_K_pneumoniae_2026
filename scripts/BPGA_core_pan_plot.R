# Load required libraries
library(ggplot2)
library(readr)
library(dplyr)

# Load data
pan <- read_table("C:/Users/HARSHADA/Downloads/pan_default.txt", col_names = c("Genomes", "Genes"))
core <- read_table("C:/Users/HARSHADA/Downloads/core_default.txt", col_names = c("Genomes", "Genes"))

# Fit the models
pan_fit <- nls(Genes ~ a * Genomes^b, data = pan, start = list(a = 2253.51, b = 0.552366))
core_fit <- nls(Genes ~ c * exp(d * Genomes), data = core, start = list(c = 4679.81, d = -0.0109067))

# Generate prediction data
x_vals <- seq(min(pan$Genomes), max(pan$Genomes), length.out = 200)
pan_pred <- data.frame(Genomes = x_vals,
                       Genes = predict(pan_fit, newdata = data.frame(Genomes = x_vals)))
core_pred <- data.frame(Genomes = x_vals,
                        Genes = predict(core_fit, newdata = data.frame(Genomes = x_vals)))

# Plot
ggplot() +
  geom_point(data = pan, aes(x = Genomes, y = Genes), color = "#f5b041", alpha = 0.6, size = 2) +
  geom_line(data = pan_pred, aes(x = Genomes, y = Genes), color = "#f39c12", size = 1) +
  geom_point(data = core, aes(x = Genomes, y = Genes), color = "#9b59b6", alpha = 0.6, size = 2) +
  geom_line(data = core_pred, aes(x = Genomes, y = Genes), color = "#8e44ad", size = 1) +
  #scale_color_manual(values = c("Pan genome" = "red", "Core genome" = "blue")) +
  labs(title = "Core-Pan Genome Plot",
       x = "Number of Genomes",
       y = "Number of Gene Families",
       caption = "Orange: Pan Genome (f(x) = a * x^b), Purple: Core Genome (f(x) = c * e^(d * x))") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
