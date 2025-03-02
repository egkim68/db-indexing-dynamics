# Dual-Database Indexing Patterns
Author: Eungi Kim

Overview  
This project analyzes the dual indexing dynamics of journals in the Korea Citation Index (KCI) and SCImago Journal Rank (SJR) from 2008 to 2023. It involves computing bibliometric metrics such as the Jaccard Similarity Index (JSI), Within-Database Jaccard Similarity (WJS), Dual-Database Indexing Change Rates (DDICR), Delisting Rates, and the Dual-Database Longevity Index (DDLI). The scripts preprocess raw data, clean and standardize datasets, and conduct bibliometric analyses using Python and R.

1. Data Sources & Download Instructions  
Korea Citation Index (KCI) requires registration before downloading data. The files should be in Excel (.xls) format and can be obtained from the KCI portal. SCImago Journal Rank (SJR) data is available in CSV (.csv) format and should be downloaded yearly from 2008 to 2023 via the SJR Journal Ranking website. Once downloaded, save the files in the appropriate directories before running the scripts.  

2. Required Software  
Python 3.x is required for preprocessing scripts, along with the following libraries: pandas, xlrd, warnings, re, and os. R version 4.x is required for analysis and visualization, along with the following packages: readr, dplyr, ggplot2, tidyr, patchwork, stringi, scales, and reshape2.  

3. Preprocessing the Data  
Before running the R analysis, the raw data must be preprocessed using Python scripts. To preprocess KCI data, run the command `python preprocess_kci_data.py`. This script reads raw KCI Excel files, cleans special characters, standardizes column names, and converts data into a tab-delimited (.txt) format. To preprocess SJR data, run `python preprocess_sjr_data.py`. This script reads raw SJR CSV files, removes unnecessary characters, standardizes delimiters, and converts data into a tab-delimited (.txt) format. Adjust directory paths in the scripts to match file locations if necessary.  

4. Running the R Analysis  
Once preprocessing is complete, run `source("dual_db_indexing_analysis.r")` in R. This script loads the cleaned KCI and SJR data, computes bibliometric indices (JSI, WJS, DDICR, Delisting Rates, DDLI), and generates tables and visualizations.  

5. Output Files  
Visualization outputs include journal_indexing_dynamics.png (Jaccard similarity and journal counts over time), within_database_jaccard_similarity.png (Within-database Jaccard Similarity trends), jaccard_similarity_heatmaps.png (Heatmaps of Jaccard similarity trends), dual_database_indexing_changes.png (Dual-database indexing change rates), combined_journal_dynamics.png (Delisting and net change trends), and dual_database_longevity_index.png (Dual-Database Longevity Index (DDLI) trends). Data outputs include journal_delisting_table.csv, which provides detailed delisting and net change statistics, along with additional R data objects for further analysis if needed.  

6. Notes & Adjustments  
Ensure correct file encodings before running the scripts. KCI files should be CP949, while SJR files should be UTF-8. Adjust directory paths in the scripts to match your system. If errors occur, verify that preprocessing scripts completed successfully before running the R script.  
