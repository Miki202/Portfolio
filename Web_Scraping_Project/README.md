# TechMart Smartphone Web Scraping Project

## Overview
This project is from my first year in FMI and it's a web-scraping tool designed 
to gather information about smartphones from **Techmart** and store it in a CSV file for analysis.
The scraped data includes specifications like price, url, and features,
providing a thorough dataset for smartphone comparison and market analysis.

## Features
- Scrapes smartphone data from TechMart’s online catalog.
- Extracts key information, including:
  - **Name**
  - **Price**
  - **Url**
  - **Specifications** (e.g., Screen size, Memory, Camera Resolution, etc.)
- Outputs the data to a structured **CSV file** for further analysis or visualization.
- Handles pagination to scrape multiple pages of results.
- Includes handling for an SSL certificate verification.

## Technologies Used
- **Python**: Core programming language.
- **BeautifulSoup**: For parsing HTML and extracting data.
- **Requests**: To send HTTP requests to TechMart's website.
- **csv**: For CSV export.

## Project Structure and Explanation of Files
- **Project_Presentation**: Includes the whole process of creating the project.
- **Techmart.py**: A Python file with the code.
- **products.csv**: The CSV output file.

