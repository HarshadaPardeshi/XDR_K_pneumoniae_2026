library(readxl)
library(ComplexHeatmap)
library(circlize)  # for color gradients

# Read data from Excel
df <- read_excel("C:/Users/HARSHADA/Downloads/Kleb_Virulence_compiled.xlsx", sheet = "Final")

# Convert to data frame and process
df_clean <- as.data.frame(df)

# Set row names to gene names
rownames(df_clean) <- df_clean[[1]]  # assuming first column is gene names
df_clean <- df_clean[, -1]           # remove the gene name column

# Remove white spaces and convert to numeric
df_clean[] <- lapply(df_clean, function(x) as.numeric(trimws(x)))

# Convert to matrix
mat <- as.matrix(df_clean)

# Define color function
col_fun <- colorRamp2(c(0, 95, 100), c("#f9e79f","#f4d03f", "#d35400"))
#col_fun <- colorRamp2(c(0, 95, 100), c("#E0B0FF", "violet", "blueviolet"))

# Generate heatmap
Heatmap(mat,
        name = "Identity (%)",
        col = col_fun,
        na_col = "grey90", 
        cluster_rows = TRUE,
        cluster_columns = TRUE,
        clustering_distance_rows = "euclidean",
        clustering_method_rows = "complete",
        clustering_distance_columns = "euclidean",
        clustering_method_columns = "complete",
        column_dend_height = unit(10, "mm"),
        height = unit(250, "mm"),
        width = unit(120, "mm"),
        show_row_names = TRUE,
        show_column_names = TRUE,
        show_heatmap_legend = TRUE,
        column_names_gp = gpar(fontsize = 10, rot = 45),
        column_title_gp = gpar(fontsize = 16, fontface = "bold"),
        row_names_gp = gpar (fontsize = 9),
        column_title = "Virulence Gene Identity Across Strains",
        #rect_gp = gpar(col = "#616a6b", lwd = 0.5),  # ⬅️ Border settings
        row_title = "Virulence Genes")

