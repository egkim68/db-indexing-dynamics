import os
import pandas as pd
import warnings
import re
warnings.filterwarnings('ignore')  # Suppress OLE2 warnings

# #########################################################
# This script is for PREPROCESSING ONLY.
# It prepares raw KCI Excel files for bibliometric analysis in R.
# 
# INPUT: 
# - Raw Excel files (.xls) from the Korea Citation Index (KCI) dataset.
# - Files are stored in a specified directory (e.g., "e:/data/kci/").
# 
# OUTPUT: 
# - Cleaned, tab-delimited text files (.txt) in the same directory.
# - Text files are encoded in CP949 for compatibility with Korean datasets.
# 
# FUNCTIONALITY:
# - Cleans special characters and formatting inconsistencies.
# - Standardizes column names.
# - Ensures compatibility for further analysis in R.
# #########################################################

def clean_text(text):
    """Clean text by removing problematic characters"""
    if isinstance(text, str):
        # Replace bullet points and other special characters
        text = text.replace('\u2022', '-')  # Replace bullet with hyphen
        text = text.replace('\u2013', '-')  # Replace en dash
        text = text.replace('\u2014', '-')  # Replace em dash
        text = text.replace('\u2018', "'")  # Replace smart quotes
        text = text.replace('\u2019', "'")
        text = text.replace('\u201c', '"')
        text = text.replace('\u201d', '"')
        # Remove any other non-CP949 characters
        text = ''.join(char for char in text if ord(char) < 0x80 or char.isalpha())
        return text
    return text

def clean_column_name(col):
    """Clean column names by removing quotes, newlines, and extra spaces"""
    if isinstance(col, str):
        cleaned = col.replace('"', '').replace('\n', ' ').strip()
        cleaned = ' '.join(cleaned.split())  # Remove multiple spaces
        cleaned = clean_text(cleaned)  # Clean special characters
        return cleaned
    return col

def process_excel_file(xls_path, txt_path):
    """Read, clean, and convert a single Excel file to a tab-delimited text file"""
    try:
        # Read Excel file
        df = pd.read_excel(
            xls_path,
            engine="xlrd",
            na_filter=False
        )
        
        print(f"\nProcessing: {os.path.basename(xls_path)}")
        print(f"Original shape: {df.shape}")
        
        # Clean column names
        df.columns = [clean_column_name(col) for col in df.columns]
        
        # Clean data
        for col in df.columns:
            if df[col].dtype == 'object':
                df[col] = df[col].apply(clean_text)
        
        # Save as tab-delimited text file
        df.to_csv(
            txt_path,
            sep="\t",
            index=False,
            encoding='cp949',
            quoting=None
        )
        
        # Verify output file
        test_df = pd.read_csv(txt_path, sep='\t', encoding='cp949')
        print(f"Successfully converted - Rows: {len(test_df)}, Columns: {len(test_df.columns)}")
        return True
        
    except Exception as e:
        print(f"Error processing {os.path.basename(xls_path)}: {str(e)}")
        return False

# Define directory containing Excel files
xls_dir = "e:/data/kci/"

# Identify Excel files for years 2008-2023
year_files = [f for f in os.listdir(xls_dir) 
              if f.endswith('.xls') and 
              f.replace('.xls', '').isdigit() and 
              2008 <= int(f.replace('.xls', '')) <= 2023]

# Sort files in chronological order
year_files.sort(key=lambda x: int(x.replace('.xls', '')))

# Process each file
success_count = 0
total_files = len(year_files)
print(f"Found {total_files} year files to process")

for xls_file in year_files:
    xls_path = os.path.join(xls_dir, xls_file)
    txt_path = os.path.join(xls_dir, xls_file.replace(".xls", ".txt"))
    
    if process_excel_file(xls_path, txt_path):
        success_count += 1

print(f"\nProcessing complete!")
print(f"Successfully converted {success_count} out of {total_files} files")
