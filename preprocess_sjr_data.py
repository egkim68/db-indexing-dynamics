import os
import re  # Import regular expressions module for text processing

# ############################################
# SJR DATA PREPROCESSING SCRIPT
# This script cleans and converts raw SJR CSV files into tab-delimited text files
# for further bibliometric analysis.
#
# INPUT: 
# - Raw SJR CSV files (`sjrYYYY.csv`) from 2008 to 2023.
# - Files are located in: `k:/data/sjr3/`
#
# OUTPUT: 
# - Cleaned, tab-delimited text files (`sjrYYYY.txt`).
# - Output files are saved in: `k:/data/sjr3/cleaned/`
# - Encoding: UTF-8
#
# FUNCTIONALITY:
# - Removes quotation marks from data.
# - Converts semicolons (not surrounded by spaces) into tabs.
# - Ensures output files are stored in the correct directory.
# - **This script skips Open Access (OA) files (`-o` versions).**
# ############################################

# Directories for input and output data
input_directory = "k:/data/sjr3/"
output_directory = "k:/data/sjr3/cleaned/"

# Ensure output directory exists
os.makedirs(output_directory, exist_ok=True)

# Function to preprocess a single SJR file
def preprocess_file(file_path, output_path):
    """
    Reads a raw SJR CSV file, processes it by removing quotation marks and adjusting delimiters,
    then saves the cleaned data as a tab-delimited text file.
    
    Parameters:
    - file_path (str): Path to the input CSV file.
    - output_path (str): Path to save the cleaned tab-delimited file.
    """
    try:
        with open(file_path, 'r', encoding='utf-8') as infile, open(output_path, 'w', encoding='utf-8') as outfile:
            for line in infile:
                # Remove all quotation marks from the text
                line = line.replace('"', '')
                
                # Replace semicolons (not surrounded by spaces) with tabs
                line = re.sub(r'(?<!\s);(?!\s)', '\t', line)

                # Write the cleaned line to the output file
                outfile.write(line)
        
        print(f"Successfully processed: {output_path}")
    
    except Exception as e:
        print(f"Error processing {file_path}: {e}")

# Process all standard SJR files from 2008 to 2023
for year in range(2008, 2023 + 1):
    input_file = os.path.join(input_directory, f"sjr{year}.csv")
    output_file = os.path.join(output_directory, f"sjr{year}.txt")
    
    if os.path.exists(input_file):
        preprocess_file(input_file, output_file)
    else:
        print(f"File not found: {input_file}")
