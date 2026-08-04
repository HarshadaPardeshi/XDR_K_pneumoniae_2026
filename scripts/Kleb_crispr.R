library(ggplot2)

crispr_csv <- "/mnt/analysis/harshada/fastq_pass/Kleb_redo/pipeline_new/S82/14_crispr/S82_crispr_prediction.csv"
df<-read_csv(crispr_csv,show_col_types=FALSE)
crispr_data <- data.frame(file_path)
# Create the linear genome diagram using ggplot2

ggplot(df, aes(x = cassette_id, y = 1, fill = predicted_label)) +
  geom_tile(height = 0.3, color = "white") +  # Adjust height as needed
  labs(title = "Predicted CRISPR-Cas Cassettes in S82", fill = "MGE_category") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
  geom_text(aes(label = predicted_label), vjust = 0.5, size = 4) +
  scale_x_continuous(breaks = crispr_data$cassette_id, labels = crispr_data$cassette_id) +
  scale_y_continuous(breaks = NULL) + # Remove y-axis labels and ticks
  labs(x = "Cassette ID", y = "", fill = "Cas System Type") +
  #scale_fill_npg() +
  scale_fill_brewer(palette = "Paired") + 
  scale_fill_manual(values = c(
  #"CAS-VI-B" = "#00BFC4",  
  "CAS-II-C"  = "#66A61E",  
  #"CAS-V-A"  = "#E15759",   
  "CAS-VI-C" = "#90EE90",
  "CAS-I-E" = "#8A9A5B",
  "CAS-V-B" = "#b2babb",
  "CAS-V-A" = "#CCCCFF",
  "CAS-VI-B" = "#96DED1",
  "CAS-IV-A" = "#5D3FD3"
  )) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    legend.position = "bottom",
    plot.margin = margin(2, 2, 2, 2)  # Reduce outer margins
  ) +
  coord_cartesian(ylim = c(0.2, 2.0))  # Shrink vertical space

```

