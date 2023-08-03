# Install and load the required libraries if not already installed
library(ggplot2)
library(readr)
# Read the data file with handling of possible missing values
data <- read_delim("/home/mvrccri/Downloads/KMC/TASK2&3/Homo_sapiens.gene_info.gz")

# Filter out rows with missing chromosome values
data <- data[complete.cases(data), ]

# Remove rows where the chromosome value contains '|'
data <- data[!grepl("\\|", data$chromosome), ]

# Count the number of genes per chromosome (including NA and Un)
gene_counts <- table(data$chromosome)

# Convert gene_counts to a data frame and order chromosomes consecutively
chromosomes_ordered <- c(1:22, "X", "Y", "MT", "Un")
gene_counts_df <- data.frame(Chromosome = names(gene_counts), Count = as.numeric(gene_counts))

# Convert gene_counts to a data frame and order chromosomes consecutively
chromosomes_ordered <- c(1:22, "X", "Y", "MT", "Un")
gene_counts_df <- data.frame(Chromosome = names(gene_counts), Count = as.numeric(gene_counts))
gene_counts_df$Chromosome <- factor(gene_counts_df$Chromosome, levels = chromosomes_ordered)

# Remove rows with 'NA' in the Chromosome column
gene_counts_df <- gene_counts_df[complete.cases(gene_counts_df), ]


# Create the plot with all bars in grey color
plot <- ggplot(gene_counts_df, aes(x = Chromosome, y = Count)) +
  geom_bar(stat = "identity", fill = "#404040") +
  theme_minimal() +
  labs(title = "Number of genes in each chromosome",
       x = "Chromosomes",  # Empty x-axis label to avoid duplication
       y = "Gene count") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.8, vjust = 0),
        axis.title.x = element_text(margin = margin(t = 10)),  # Adjust x-axis label position
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.background = element_blank(),  # Remove background color
        panel.border = element_blank(),      # Remove panel border
        axis.line = element_line(color = "black", size = 0.5),  # Add axis line
        axis.ticks = element_line(size = 0.5),  # Add ticks to x and y axes
        axis.ticks.length = unit(0.1, "cm"),  # Set the length of ticks
        plot.title = element_text(size = 14,hjust = 0.5))

# Print the plot to see it in the R session (optional)
print(plot)

# Save the plot to a PDF file (Replace 'output_plot.pdf' with the desired file name)
ggsave("GeneCount_PerChromosomes_plot.pdf", plot, width = 10, height = 6, units = "in")