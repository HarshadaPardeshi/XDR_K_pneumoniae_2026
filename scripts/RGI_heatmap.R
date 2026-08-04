install.packages("reshape2")
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)

# Adjust the path to your Excel file
df <- read_excel("C:/Users/HARSHADA/Desktop/Phd/Klebsiella/RGI/aLL_MERGED.xlsx", sheet = "Compiled_genes")

# Set Drug_Class as row names
df_fixed<- as.data.frame(df)
rownames(df_fixed)<- df_fixed$Best_Hit_ARO

# Drop the non-numeric 'Drug_Class' column
df_fixed$Best_Hit_ARO <- NULL

# Check structure to ensure everything is numeric
str(df_fixed)

mat <- df_fixed %>%
  select(S82, S90, S106, S128, S138, S146) %>%
  as.matrix()
print(mat)

# Basic heatmap with row and column dendrograms
heatmap(mat, 
        main = "Comparative analysis of AMR genes", 
        Rowv = NA, 
        Colv = TRUE, 
        col = heat.colors(256), 
        scale = "column", 
        margins = c(8, 10), 
        xlab = "Genomes", 
        ylab = "Drug Classes")

install.packages("pheatmap")
library(pheatmap)
cols <- colorRampPalette(c("blue", "white"))(100)  
pheatmap(mat,
         scale = "column",
         #color = rev(heat.colors(256)),
         color = cols,
         border_color = NA,
         cluster_rows = FALSE,
         cluster_cols = TRUE,
         fontsize_row = 8,
         fontsize_col = 10,
         main = "Comparative Resistance Gene Analysis")



##Plot of resistant genes vs drug classes using ggplot2


# Load required packages
install.packages("ggsci")
installed.packages("scico")
install.packages("Polychrome")
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(Polychrome)

# Read Excel data
data <- read_excel("C:/Users/HARSHADA/Desktop/Phd/Klebsiella/RGI/aLL_MERGED.xlsx", sheet = "Compiled_genes")
data_matrix <- data.matrix(data)
colnames(data)


# Melt the data from wide to long format
long_data <- melt(data,
                  id.vars = c("Best_Hit_ARO", "Drug_class"),
                  variable.name = "Strain",
                  value.name = "Presence")

colors_36 <- palette36.colors(36)  # Get 36 distinct colors
names(colors_36) <- unique(long_data$Drug_class)  # Map to your classes

# Plot heatmap
 p <- ggplot(long_data, aes(x = Best_Hit_ARO, y = Strain,  fill = Drug_class, alpha = Presence)) +
  geom_tile(color = "gray50") +
  scale_alpha_identity(guide = "none") +
  scale_fill_manual(values = colors_36) +
  coord_equal(expand = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 5),  # Increase X-axis font
        axis.text.y = element_text(size = 6),                         # Increase Y-axis font
        axis.title.x = element_text(size = 8),                        # Increase X-axis title font
        axis.title.y = element_text(size = 8),
        legend.key.size = unit(0.2, "cm"),        # Reduce size of legend keys
        legend.key.height = unit(0.1, "cm"),
        legend.key.width = unit(0.2, "cm"),
        legend.box.margin = margin(2, 2, 2, 2),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10)
        ) + 
  labs(y = "Strain", x = "Gene", fill = "Drug Class",
       title = "Analysis of AMR Gene Presence and Absence Across Six Genomes")
  
  ggsave("resistance_heatmap_new_2.png", plot = p, width = 12, height = 10, units = "in", dpi = 300)

  getwd()
  
  