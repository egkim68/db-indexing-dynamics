a####################################
# BIBLIOMETRIC ANALYSIS OF DUAL-DATABASE INDEXING PATTERNS
# R Code for: "Dual-Database Indexing Patterns: A Longitudinal Study of Journal
# Overlap, Retention, and Status Changes Between KCI and SJR"
# Author: Eungi Kim
# Date: February 2025
# Software: R version 4.x
# Analysis Purpose: Examine journal indexing dynamics across Korean Citation 
# Index (KCI) and Scopus/SJR databases
####################################

# INPUT FILES:
# - KCI Data (2008-2023): `k:/data/kci/`
#   - Format: Tab-delimited (.txt)
#   - Encoding: CP949
#   - File names: `2008.txt`, `2009.txt`, ..., `2023.txt`
#   - Columns Expected: 
#     * Journal Title
#     * ISSN
#     * Other metadata columns
#
# - SJR Data (2008-2023): `k:/data/sjr3/cleaned/`
#   - Format: Tab-delimited (.txt)
#   - Encoding: UTF-8
#   - File names: `sjr2008.txt`, `sjr2009.txt`, ..., `sjr2023.txt`
#   - Columns Expected:
#     * Journal Title
#     * ISSN
#     * SJR Score
#     * Other bibliometric indicators

# OUTPUT FILES:
# Visualization Outputs:
# - jaccard_analysis1.png: Jaccard similarity and journal counts
# - combined_journal_dynamics.png: Delisting and net change rates
# - Heatmap visualizations of database similarities
#
# Data Outputs:
# - journal_delisting_table.csv: Detailed delisting and net change statistics
# - Potential R data objects for further analysis

# REQUIRED LIBRARIES:
# Data Manipulation
library(readr)    # Reading tab-delimited files
library(dplyr)    # Data manipulation
library(tidyr)    # Data reshaping
library(reshape2) # Converting matrices to long format

# Visualization
library(ggplot2)  # Core visualization
library(scales)   # Formatting ggplot axes
library(patchwork) # Combining multiple ggplots
library(knitr)    # Generating tables

# KEY METRICS COMPUTED:
# 1. Jaccard Similarity Index (JSI)
# 2. Within-Database Jaccard Similarity (WJS)
# 3. Dual-Database Indexing Change Rate (DDICR)
# 4. Delisting Rates
# 5. Dual-Database Longevity Index (DDLI)

# METHODOLOGY WORKFLOW:
# 1. Data Cleaning and Preprocessing
# 2. Compute Similarity Metrics
# 3. Analyze Temporal Trends
# 4. Visualize Database Dynamics
# 5. Generate Comparative Analyses

####################################
# FIGURE 1: JOURNAL INDEXING DYNAMICS BETWEEN KCI AND SJR
# Metrics: Jaccard Similarity Index (JSI), Journal Counts, Shared Journals
####################################

# Required Libraries
library(readr)    # Reading tab-delimited files
library(dplyr)    # Data manipulation
library(tidyr)    # Data reshaping
library(ggplot2)  # Visualization
library(patchwork) # Combining plots

# Data Cleaning Function for Standardizing Text
clean_text <- function(text) {
    if (is.character(text)) {
        # Normalize character encoding
        text <- iconv(text, "CP949", "UTF-8", sub="")
        return(text)
    }
    return(text)
}

# Function to Read and Process SJR Database Journals
read_sjr_data <- function(year) {
    file_path <- paste0("k:/data/sjr3/cleaned/sjr", year, ".txt")
    
    # Validate file existence
    if (!file.exists(file_path)) {
        warning(paste("SJR data file not found for year:", year))
        return(NULL)
    }
    
    # Read file with error handling
    s <- tryCatch({
        read_delim(file_path, delim = "\t", locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
    }, error = function(e) {
        warning(paste("Error reading SJR file for", year, ":", e$message))
        return(NULL)
    })
    
    # Dynamic column detection
    issn_col <- grep("Issn", colnames(s), value = TRUE, ignore.case = TRUE)
    title_col <- grep("Title", colnames(s), value = TRUE, ignore.case = TRUE)
    
    if (length(issn_col) == 0 || length(title_col) == 0) {
        warning(paste("Missing ISSN or Title column in SJR data for", year))
        return(NULL)
    }
    
    # Clean and standardize journal data
    s_cleaned <- s %>%
        rename(Issn = all_of(issn_col), Title = all_of(title_col)) %>%
        mutate(
            Issn = sapply(strsplit(as.character(Issn), ", "), `[`, 1),
            Title = tolower(trimws(Title))
        ) %>%
        filter(!is.na(Issn), nchar(Issn) == 8) %>%
        distinct(Title, .keep_all = TRUE)
    
    return(s_cleaned)
}

# Function to Read and Process KCI Database Journals
read_kci_data <- function(year) {
    file_path <- paste0("k:/data/kci/", year, ".txt")
    
    # Validate file existence
    if (!file.exists(file_path)) {
        warning(paste("KCI data file not found for year:", year))
        return(NULL)
    }
    
    # Read file with error handling
    kk <- tryCatch({
        read_delim(file_path, delim = "\t", locale = locale(encoding = "CP949"), show_col_types = FALSE)
    }, error = function(e) {
        warning(paste("Error reading KCI file for", year, ":", e$message))
        return(NULL)
    })
    
    # Ensure first column is title
    colnames(kk)[1] <- "Title"
    
    # Clean and standardize journal data
    kk <- kk %>%
        mutate(
            Issn = gsub("-", "", ISSN),
            Title = tolower(trimws(Title))
        ) %>%
        filter(!is.na(Issn), nchar(Issn) == 8)
    
    return(kk)
}

# Compute Jaccard Trends: Core Computation of Bibliometric Metrics
years <- 2008:2023
jaccard_trends <- data.frame(
    Year = integer(),
    Jaccard_Index = numeric(),
    KCI_Journals = integer(),
    SJR_Journals = integer(),
    Shared_Journals = integer()
)

# Longitudinal Analysis of Journal Indexing
for (year in years) {
    # Read data for current year
    sjr_data <- read_sjr_data(year)
    kci_data <- read_kci_data(year)
    
    if (!is.null(sjr_data) && !is.null(kci_data)) {
        # Compute journal set metrics
        sjr_issns <- unique(gsub("-", "", sjr_data$Issn))
        kci_issns <- unique(gsub("-", "", kci_data$Issn))
        
        # Compute shared journals
        total_shared <- length(unique(c(
            intersect(kci_issns, sjr_issns),
            intersect(tolower(kci_data$Title), tolower(sjr_data$Title))
        )))
        
        union_issns <- length(unique(c(kci_issns, sjr_issns)))
        
        # Calculate Jaccard Similarity Index (JSI)
        jaccard_index <- ifelse(union_issns > 0, total_shared / union_issns, 0)
        
        # Store results
        jaccard_trends <- bind_rows(
            jaccard_trends,
            data.frame(
                Year = year,
                Jaccard_Index = jaccard_index,
                KCI_Journals = length(kci_issns),
                SJR_Journals = length(sjr_issns),
                Shared_Journals = total_shared
            )
        )
    }
}

# Visualization of Jaccard Trends
if (nrow(jaccard_trends) > 0) {
    # Plot A: Journal Counts and Shared Journals
    plot_A <- ggplot(jaccard_trends, aes(x = Year)) +
        geom_bar(aes(y = KCI_Journals, fill = "KCI Journals"), stat = "identity", position = "identity", alpha = 0.8) +
        geom_bar(aes(y = SJR_Journals, fill = "SJR Journals"), stat = "identity", position = "identity", alpha = 0.8) +
        geom_line(aes(y = Shared_Journals * 50, group = 1), color = "black", size = 1) +
        geom_point(aes(y = Shared_Journals * 50), color = "black", size = 3) +
        geom_text(aes(y = Shared_Journals * 50 + 2000, label = Shared_Journals), size = 3) +
        geom_text(aes(y = KCI_Journals - 100, label = KCI_Journals), size = 3) +
        geom_text(aes(y = SJR_Journals + 500, label = SJR_Journals), size = 3) +
        scale_y_continuous(
            name = "Number of Journals",
            limits = c(0, 31000),
            sec.axis = sec_axis(~ . / 50, name = "Shared Journals", breaks = seq(0, 600, 100))
        ) +
        # Use expand parameter to control spacing instead of limits
        scale_x_continuous(breaks = years, expand = c(0.01, 0.01)) +
        scale_fill_manual(values = c("KCI Journals" = "#9370DB", "SJR Journals" = "#FFA07A")) +
        labs(
            title = "KCI and SJR Indexed Journals Over Time",
            x = "Year"
        ) +
        theme_minimal() +
        # Use plot.margin to adjust spacing directly
        theme(
            legend.position = "top",
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
            axis.text.y = element_text(color = "black", size = 10),
            axis.title = element_text(color = "black"),
            panel.grid.minor = element_blank(),
            plot.margin = margin(5, 5, 5, 5)
        ) +
        guides(fill = guide_legend(title = NULL))
    
    # Plot B: Jaccard Similarity Index with rounded values to 3 decimal places
    plot_B <- ggplot(jaccard_trends, aes(x = Year, y = Jaccard_Index)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
        geom_text(aes(label = sprintf("%.3f", round(Jaccard_Index, 3)), y = Jaccard_Index + 0.0005), size = 3.5) +
        # Use same expand parameter
        scale_x_continuous(breaks = years, expand = c(0.01, 0.01)) +
        scale_y_continuous(limits = c(0, 0.015)) +
        labs(
            title = "Jaccard Index Over Time",
            x = "Year", 
            y = "Jaccard Index"
        ) +
        theme_minimal() +
        # Use same plot.margin
        theme(
            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10, color = "black"),
            axis.text.y = element_text(color = "black", size = 10),
            axis.title = element_text(color = "black"),
            panel.grid.minor = element_blank(),
            plot.margin = margin(5, 5, 5, 5)
        )
        
    # Combine Plots
    final_plot <- plot_A / plot_B + 
        plot_layout(heights = c(3, 2))
    
    # Display and Save
    print(final_plot)
    ggsave("journal_indexing_dynamics.png", final_plot, width = 12, height = 10, dpi = 300)
}
#
####################################
# FIGURE 2: WITHIN-DATABASE JACCARD SIMILARITY ANALYSIS
####################################

# Reuse functions from Figure 1 for data reading
# Ensure these functions are sourced or copied from Figure 1 script

# Compute Jaccard Similarities
years <- 2008:2023

# Initialize Jaccard Similarity Matrices
kci_jaccard <- matrix(NA, nrow = length(years), ncol = length(years), 
                      dimnames = list(years, years))
sjr_jaccard <- matrix(NA, nrow = length(years), ncol = length(years), 
                      dimnames = list(years, years))

# Compute Jaccard similarities
for(i in 1:length(years)) {
    for(j in 1:length(years)) {
        year_i <- as.character(years[i])
        year_j <- as.character(years[j])
        
        if (i != j) {
            # KCI comparison
            kci_jaccard[year_i, year_j] <- 
                compute_jaccard(kci_sets[[year_i]], kci_sets[[year_j]])
            
            # SJR comparison
            sjr_jaccard[year_i, year_j] <- 
                compute_jaccard(sjr_sets[[year_i]], sjr_sets[[year_j]])
        }
    }
}

# Prepare Year-to-Year Comparison Data
year_to_year <- data.frame(
    Year = years[-length(years)],
    KCI_Next_Year = sapply(1:(length(years)-1), function(i) 
        kci_jaccard[as.character(years[i]), as.character(years[i+1])]),
    SJR_Next_Year = sapply(1:(length(years)-1), function(i) 
        sjr_jaccard[as.character(years[i]), as.character(years[i+1])])
)

# Visualization
plot <- ggplot(year_to_year, aes(x = Year)) +
    geom_line(aes(y = KCI_Next_Year, color = "KCI"), size = 1) +
    geom_line(aes(y = SJR_Next_Year, color = "SJR"), size = 1) +
    geom_point(aes(y = KCI_Next_Year, color = "KCI"), size = 3) +
    geom_point(aes(y = SJR_Next_Year, color = "SJR"), size = 3) +
    labs(
        title = "Year-to-Year Jaccard Similarity Within Databases",
        x = "Year",
        y = "Jaccard Similarity",
        color = "Database"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    scale_color_manual(values = c("KCI" = "blue", "SJR" = "red")) +
    theme(
        text = element_text(color = "black"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 14),
        plot.title = element_text(color = "black", size = 16),
        legend.text = element_text(color = "black", size = 13),
        legend.title = element_text(color = "black", size = 14)
    ) +
    scale_x_continuous(breaks = seq(min(years), max(years), by = 1))

# Display plot
print(plot)

# Save plot
ggsave("within_database_jaccard_similarity.png", plot, width = 10, height = 6, dpi = 300)

####################################
# FIGURE 3: JACCARD SIMILARITY HEATMAPS WITHIN KCI AND SJR
####################################

# Required Libraries
library(ggplot2)
library(reshape2)
library(patchwork)

# Function to Filter Half Matrix
filter_half_matrix <- function(data) {
    data[data$Year1 <= data$Year2, ]  # Keep only upper triangle
}

# Function to Plot Heatmap with Enhanced Styling
plot_half_heatmap <- function(data, title) {
    ggplot(data, aes(x = Year1, y = Year2, fill = Similarity)) +
        geom_tile(color = "white", size = 0.5) +
        geom_text(
            aes(label = sprintf("%.3f", Similarity)), 
            color = "black", 
            size = 3.5
        ) +
        scale_fill_gradient(
            low = "#FFE4B5", 
            high = "#FF6347", 
            name = "Jaccard Index"
        ) +
        labs(
            title = title,
            x = "Year", 
            y = "Year"
        ) +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            axis.text = element_text(color = "black", size = 10),
            axis.title = element_text(color = "black", size = 12, face = "bold"),
            plot.title = element_text(color = "black", size = 14, face = "bold"),
            legend.title = element_text(color = "black", size = 10, face = "bold"),
            legend.text = element_text(color = "black", size = 9)
        ) +
        scale_x_continuous(breaks = unique(data$Year1)) +
        scale_y_continuous(breaks = unique(data$Year2))
}

# Convert Jaccard Matrices to Long Format
kci_long <- melt(kci_jaccard, varnames = c("Year1", "Year2"), value.name = "Similarity")
sjr_long <- melt(sjr_jaccard, varnames = c("Year1", "Year2"), value.name = "Similarity")

# Filter Matrices
kci_long <- filter_half_matrix(kci_long)
sjr_long <- filter_half_matrix(sjr_long)

# Generate Heatmaps
plot_kci <- plot_half_heatmap(kci_long, "Jaccard Similarity Within KCI")
plot_sjr <- plot_half_heatmap(sjr_long, "Jaccard Similarity Within SJR")

# Combine Plots
final_plot <- plot_kci / plot_sjr +
    plot_layout(guides = "collect") &
    theme(legend.position = "right")

# Display Plot
print(final_plot)

# Save Plot
ggsave(
    "jaccard_similarity_heatmaps.png", 
    final_plot, 
    width = 10, 
    height = 12, 
    dpi = 300
)

####################################
# FIGURE 4: DUAL-DATABASE INDEXING CHANGES BETWEEN KCI AND SJR
####################################

# Required Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Prepare Dual-Database Indexing Status Change Data
prepare_status_change_data <- function(jaccard_trends) {
    # Validate input data
    required_cols <- c("Year", "KCI_Journals", "SJR_Journals")
    if (!all(required_cols %in% colnames(jaccard_trends))) {
        stop("Missing required columns in jaccard_trends dataframe")
    }
    
    # Compute status change rates
    status_change_data <- jaccard_trends %>%
        arrange(Year) %>%
        mutate(
            # Changes in journal counts
            KCI_to_SJR_1Y_Rate = (lead(KCI_Journals, 1) - KCI_Journals) / KCI_Journals,
            KCI_to_SJR_2Y_Rate = (lead(KCI_Journals, 2) - KCI_Journals) / KCI_Journals,
            KCI_to_SJR_3Y_Rate = (lead(KCI_Journals, 3) - KCI_Journals) / KCI_Journals,
            SJR_to_KCI_1Y_Rate = (lead(SJR_Journals, 1) - SJR_Journals) / SJR_Journals,
            SJR_to_KCI_2Y_Rate = (lead(SJR_Journals, 2) - SJR_Journals) / SJR_Journals,
            SJR_to_KCI_3Y_Rate = (lead(SJR_Journals, 3) - SJR_Journals) / SJR_Journals
        ) %>%
        # Reshape for visualization
        select(Year, 
               KCI_to_SJR_1Y_Rate, KCI_to_SJR_2Y_Rate, KCI_to_SJR_3Y_Rate,
               SJR_to_KCI_1Y_Rate, SJR_to_KCI_2Y_Rate, SJR_to_KCI_3Y_Rate) %>%
        pivot_longer(
            cols = -Year, 
            names_to = "Status_Change_Type", 
            values_to = "Rate"
        )
    
    # Rename for clarity
    status_change_data$Status_Change_Type <- recode(
        status_change_data$Status_Change_Type,
        "KCI_to_SJR_1Y_Rate" = "KCI → SJR (1 Year)",
        "KCI_to_SJR_2Y_Rate" = "KCI → SJR (2 Years)",
        "KCI_to_SJR_3Y_Rate" = "KCI → SJR (3 Years)",
        "SJR_to_KCI_1Y_Rate" = "SJR → KCI (1 Year)",
        "SJR_to_KCI_2Y_Rate" = "SJR → KCI (2 Years)",
        "SJR_to_KCI_3Y_Rate" = "SJR → KCI (3 Years)"
    )
    
    return(status_change_data)
}

# Create Visualization
plot_dual_database_changes <- function(status_change_data) {
    ggplot(status_change_data, aes(
        x = Year, 
        y = Rate, 
        color = Status_Change_Type, 
        group = Status_Change_Type
    )) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
        title = "Dual-Database Indexing Change Rates",
        x = "Year",
        y = "Indexing Change Rate",
        color = "Indexing Status Change"
    ) +
    scale_y_continuous(
        labels = percent,
        limits = c(-0.2, 0.2)  # Symmetric limits for better visualization
    ) +
    scale_x_continuous(
        breaks = seq(min(status_change_data$Year), max(status_change_data$Year), by = 1)
    ) +
    theme_minimal() +
    theme(
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(color = "black", size = 12, face = "bold"),
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        legend.text = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 12, face = "bold"),
        legend.position = "right"
    ) +
    scale_color_brewer(palette = "Set1")  # Use a color-blind friendly palette
}

# Main Execution
status_change_data <- prepare_status_change_data(jaccard_trends)
plot <- plot_dual_database_changes(status_change_data)

# Display and Save
print(plot)
ggsave(
    "dual_database_indexing_changes.png", 
    plot, 
    width = 12, 
    height = 7, 
    dpi = 300
)

#####################
#  Delisting Trend - Table 1.
#####################

# Initialize the delisting data frame
delisting_data <- data.frame(
    Year = years[-1],  # Exclude the first year since delisting is year-over-year
    KCI_Delisted = 0,
    SJR_Delisted = 0,
    Shared_Journals_Delisted = 0,
    KCI_Net_Change = 0,
    SJR_Net_Change = 0,
    Shared_Journals_Net_Change = 0
)

# Function to calculate shared journals
calculate_shared_journals <- function(issns1, issns2) {
    length(intersect(issns1, issns2))
}

# Loop through years to calculate delisting and net change metrics
for (i in 2:length(years)) {
    year <- years[i]
    prev_year <- years[i - 1]
    
    # Read data for the current and previous years
    prev_sjr_data <- read_sjr_data(prev_year)
    prev_kci_data <- read_kci_data(prev_year)
    curr_sjr_data <- read_sjr_data(year)
    curr_kci_data <- read_kci_data(year)
    
    if (!is.null(prev_sjr_data) && !is.null(prev_kci_data) && 
        !is.null(curr_sjr_data) && !is.null(curr_kci_data)) {
        
        # Extract ISSNs
        prev_sjr_issns <- unique(gsub("-", "", prev_sjr_data$Issn))
        prev_kci_issns <- unique(gsub("-", "", prev_kci_data$Issn))
        curr_sjr_issns <- unique(gsub("-", "", curr_sjr_data$Issn))
        curr_kci_issns <- unique(gsub("-", "", curr_kci_data$Issn))
        
        # Calculate shared journals for previous and current years
        prev_shared_journals <- calculate_shared_journals(prev_kci_issns, prev_sjr_issns)
        curr_shared_journals <- calculate_shared_journals(curr_kci_issns, curr_sjr_issns)
        
        # Calculate delisted journals (raw counts)
        KCI_delisted <- length(setdiff(prev_kci_issns, curr_kci_issns))
        SJR_delisted <- length(setdiff(prev_sjr_issns, curr_sjr_issns))
        Shared_Journals_delisted <- max(0, prev_shared_journals - curr_shared_journals)
        
        # Calculate net changes
        KCI_net_change <- length(curr_kci_issns) - length(prev_kci_issns)
        SJR_net_change <- length(curr_sjr_issns) - length(prev_sjr_issns)
        Shared_Journals_net_change <- curr_shared_journals - prev_shared_journals
        
        # Store the raw counts
        delisting_data$KCI_Delisted[i - 1] <- KCI_delisted
        delisting_data$SJR_Delisted[i - 1] <- SJR_delisted
        delisting_data$Shared_Journals_Delisted[i - 1] <- Shared_Journals_delisted
        delisting_data$KCI_Net_Change[i - 1] <- KCI_net_change
        delisting_data$SJR_Net_Change[i - 1] <- SJR_net_change
        delisting_data$Shared_Journals_Net_Change[i - 1] <- Shared_Journals_net_change
    }
}

# Now create Table 1 directly from the raw counts
table1 <- delisting_data

# Calculate mean row
mean_values <- colMeans(delisting_data[, -1], na.rm = TRUE)
mean_row <- data.frame(
    Year = "Mean",
    KCI_Delisted = round(mean_values[1], 1),
    SJR_Delisted = round(mean_values[2], 1),
    Shared_Journals_Delisted = round(mean_values[3], 1),
    KCI_Net_Change = round(mean_values[4], 1),
    SJR_Net_Change = round(mean_values[5], 1),
    Shared_Journals_Net_Change = round(mean_values[6], 1)
)

# Combine with the mean row
table1_final <- rbind(table1, mean_row)

# Save as a CSV file
write.csv(table1_final, 
          file = "journal_delisting_table.csv", 
          row.names = FALSE)

#####################
# Delisting Trends - Figure 5
#####################
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)

# First, make sure the Year column is numeric for proper plotting
table1_final$Year <- as.numeric(table1_final$Year)

# Extract the total journal counts from jaccard_trends
journal_totals <- jaccard_trends %>%
  select(Year, KCI_Journals, SJR_Journals, Shared_Journals)

# Join the totals with your delisting data
data_with_totals <- table1_final %>%
  left_join(journal_totals, by = "Year") %>%
  # Create lagged totals (previous year) for rate calculation
  mutate(
    KCI_Total_Prev = lag(KCI_Journals),
    SJR_Total_Prev = lag(SJR_Journals),
    Shared_Journals_Total_Prev = lag(Shared_Journals)
  ) %>%
  # Calculate delisting rates
  mutate(
    KCI_Delisting_Rate = KCI_Delisted / KCI_Total_Prev,
    SJR_Delisting_Rate = SJR_Delisted / SJR_Total_Prev,
    Shared_Journals_Delisting_Rate = Shared_Journals_Delisted / Shared_Journals_Total_Prev,
    KCI_Net_Change_Rate = KCI_Net_Change / KCI_Total_Prev,
    SJR_Net_Change_Rate = SJR_Net_Change / SJR_Total_Prev,
    Shared_Journals_Net_Change_Rate = Shared_Journals_Net_Change / Shared_Journals_Total_Prev
  ) %>%
  # Remove the first row (which has NA for rates due to lag)
  filter(!is.na(KCI_Total_Prev))

# 1. Create delisting rate plot
delisting_plot <- ggplot(data_with_totals, aes(x = Year)) +
    geom_line(aes(y = KCI_Delisting_Rate, color = "KCI"), size = 1) +
    geom_line(aes(y = SJR_Delisting_Rate, color = "SJR"), size = 1) +
    geom_line(aes(y = Shared_Journals_Delisting_Rate, color = "Shared KCI-SJR"), size = 1) +
    geom_point(aes(y = KCI_Delisting_Rate, color = "KCI"), size = 3) +
    geom_point(aes(y = SJR_Delisting_Rate, color = "SJR"), size = 3) +
    geom_point(aes(y = Shared_Journals_Delisting_Rate, color = "Shared KCI-SJR"), size = 3) +
    labs(
        title = "Journal Delisting Rate Trends",
        x = "",
        y = "Delisting Rate",
        color = "Journal Source"
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.10)) +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    scale_color_manual(values = c("KCI" = "blue", "SJR" = "red", "Shared KCI-SJR" = "darkgreen")) +
    theme_minimal() +
    theme(
        text = element_text(color = "black"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 14),
        plot.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "right"
    )

# 2. Create net change rate plot
net_change_rate_plot <- ggplot(data_with_totals, aes(x = Year)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_line(aes(y = KCI_Net_Change_Rate, color = "KCI"), size = 1) +
    geom_line(aes(y = SJR_Net_Change_Rate, color = "SJR"), size = 1) +
    geom_line(aes(y = Shared_Journals_Net_Change_Rate, color = "Shared KCI-SJR"), size = 1) +
    geom_point(aes(y = KCI_Net_Change_Rate, color = "KCI"), size = 3) +
    geom_point(aes(y = SJR_Net_Change_Rate, color = "SJR"), size = 3) +
    geom_point(aes(y = Shared_Journals_Net_Change_Rate, color = "Shared KCI-SJR"), size = 3) +
    labs(
        title = "Net Journal Change Rate",
        x = "Year",
        y = "Net Change Rate",
        color = "Journal Source"
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(-0.10, 0.10)) +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    scale_color_manual(values = c("KCI" = "blue", "SJR" = "red", "Shared KCI-SJR" = "darkgreen")) +
    theme_minimal() +
    theme(
        text = element_text(color = "black"),
        axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 14),
        plot.title = element_text(color = "black", size = 16, face = "bold"),
        legend.text = element_text(color = "black", size = 14),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "right"
    )

# 3. Combine the plots using patchwork with larger shared legend
combined_plot <- delisting_plot / net_change_rate_plot +
    plot_layout(guides = "collect") &
    theme(
        legend.position = "right",
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 15),
        legend.key.size = unit(1.2, "cm"),
        plot.title = element_text(size = 16, face = "bold")
    )

# 4. Display the combined plot
print(combined_plot)

# 5. Save the combined plot
ggsave("combined_journal_dynamics.png", combined_plot, width = 10, height = 12, dpi = 300)

######################################
# DUAL-DATABASE LONGEVITY INDEX (DDLI) - Figure 6
######################################

# Initialize dataframe to store DDLI results
ddli_data <- data.frame(
    Year = years,  # Include all years
    DDLI_1Y = rep(NA, length(years)),
    DDLI_2Y = rep(NA, length(years)),
    DDLI_3Y = rep(NA, length(years)),
    DDLI_5Y = rep(NA, length(years))
)

# Compute persistence of shared journals (Looking Backward)
for (i in 2:length(years)) {  # Start from second year, since first year has no past
    year <- years[i]

    # Get shared ISSNs in the past year (DB1_t-1 ∩ DB2_t-1)
    past_1y <- intersect(kci_sets[[as.character(years[i-1])]], sjr_sets[[as.character(years[i-1])]])
    if (length(past_1y) > 0) {
        present <- intersect(kci_sets[[as.character(year)]], sjr_sets[[as.character(year)]])
        ddli_data$DDLI_1Y[i] <- length(intersect(past_1y, present)) / length(past_1y)
    }

    if (i > 2) {  # Ensure at least 2 years of past data exist
        past_2y <- intersect(kci_sets[[as.character(years[i-2])]], sjr_sets[[as.character(years[i-2])]])
        if (length(past_2y) > 0) {
            ddli_data$DDLI_2Y[i] <- length(intersect(past_2y, present)) / length(past_2y)
        }
    }

    if (i > 3) {  # Ensure at least 3 years of past data exist
        past_3y <- intersect(kci_sets[[as.character(years[i-3])]], sjr_sets[[as.character(years[i-3])]])
        if (length(past_3y) > 0) {
            ddli_data$DDLI_3Y[i] <- length(intersect(past_3y, present)) / length(past_3y)
        }
    }

    if (i > 5) {  # Ensure at least 5 years of past data exist
        past_5y <- intersect(kci_sets[[as.character(years[i-5])]], sjr_sets[[as.character(years[i-5])]])
        if (length(past_5y) > 0) {
            ddli_data$DDLI_5Y[i] <- length(intersect(past_5y, present)) / length(past_5y)
        }
    }
}

# Print results
print(ddli_data)

# Generate DDLI plot
ddli_plot <- ggplot(ddli_data, aes(x = Year)) +
    geom_line(aes(y = DDLI_1Y, color = "1-Year Co-Indexing"), size = 1) +
    geom_line(aes(y = DDLI_2Y, color = "2-Year Co-Indexing"), size = 1) +
    geom_line(aes(y = DDLI_3Y, color = "3-Year Co-Indexing"), size = 1) +
    geom_line(aes(y = DDLI_5Y, color = "5-Year Co-Indexing"), size = 1) +
    geom_point(aes(y = DDLI_1Y, color = "1-Year Co-Indexing"), size = 3) +
    geom_point(aes(y = DDLI_2Y, color = "2-Year Co-Indexing"), size = 3) +
    geom_point(aes(y = DDLI_3Y, color = "3-Year Co-Indexing"), size = 3) +
    geom_point(aes(y = DDLI_5Y, color = "5-Year Co-Indexing"), size = 3) +
    labs(
        title = "Dual-Database Longevity Index (DDLI)",
        x = "Year",
        y = "Dual-Database Longevity Index (DDLI)",
        color = "Co-Indexing Period"
    ) +
    scale_y_continuous(limits = c(0.8, 1.00), labels = scales::percent) +  
    scale_x_continuous(breaks = seq(min(ddli_data$Year), max(ddli_data$Year), by = 1)) +  
    scale_color_manual(values = c("1-Year Co-Indexing" = "steelblue",
                                  "2-Year Co-Indexing" = "darkorange",
                                  "3-Year Co-Indexing" = "forestgreen",
                                  "5-Year Co-Indexing" = "firebrick")) +  
    theme_minimal() +
    theme(
        axis.title = element_text(size = 14, color = "black"),  
        axis.text = element_text(size = 12, color = "black"),   
        legend.text = element_text(size = 12),                 
        legend.title = element_text(size = 14),                
        plot.title = element_text(size = 16, face = "bold")    
    )

# Display the plot
print(ddli_plot)

# Save the plot
ggsave("dual_database_longevity_index.png", ddli_plot, width = 10, height = 6, dpi = 300)
