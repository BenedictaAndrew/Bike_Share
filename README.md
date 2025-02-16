# Benedicta_Chika.github.io


## TABLE OF CONTENT
### Introduction
[Project Overview](project-overview)
- [Business Task](business-task)
- [Stakeholders](stakeholders)

- ### Data Preparation
- [Data Sources](data-sources)
- [Data Organization](data-organization)
- [Data Credibility](data-credibility)
- [Data Privacy and Security](data-privacy-and-security)
- [Data Integrity Verification](data-integrity-verification)
- [Data Issues](data-issues)
- [Data Download and Storage](data-download-and-storage)

- ### Data Processing
- [Tools Used](tools-used)
- [Data Cleaning Steps](data-cleaning-steps)
- [Error Checking](error-checking)
- [Data Transformation](data-transformation)
- [Cleaning Documentation](cleaning-documentation)
- 
  
### Exploratory Data Analysis (EDA)
- [Summary Statistics](summary-statistics)
- [Visualization of Data](visualization-of-data)
- [Trends and Relationships](trends-and-relationships)
- [EDA Results](eda-results)

### Statistical Analysis and Modeling
- [Statistical Methods Used](statistical-methods-used)
- [Modeling Steps](modeling-steps)
- [Model Results](model-results)
- [Interpretation of Results](interpretation-of-results)
  
### Data Visualization
- [Visualization Techniques](visualization-techniques)
- [Visualizations Created](visualizations-created)
- [Interpretation of Visualizations](interpretation-of-visualizations)
  
### Findings and Recommendations
- [Summary of Findings](summary-of-findings)
- [Business Insights](business-insights)
- [Recommendations](recommendations)
- [Documentation](Documentation)
  
### R Scripts
- [GitHub Workflow](github-workflow)
- [Markdown Files](markdown-files)
- [Final Report and Presentation](final-report-and-presentation)
  
### Final Report and Presentation
- [Executive Summary](executive-summary)
- [Detailed Report](detailed-report)
- [Presentation Slides](presentation-slides)
- [Appendices](appendices)
  
### Appendices
- [Data Dictionary](Data-Dictionary)
- [Additional Analysis](additional-analysis)
- [References](references)

### Introduction
---
### Project Overview
The assignment entails studying Cyclistic bike-share data for Chicago in depth to uncover information that will drive strategic marketing decisions. Cyclistic is interested in optimizing year-to-year memberships by understanding how its customer segments (annual members and occasional riders) utilize its bike-share service differently. The analysis will entail the study of trip data for the past 12 months to discover patterns, trends, and Cyclistic user behavior. The ultimate goal is to provide actionable recommendations that will help Cyclistic convert occasional cyclists into annual members via data-driven marketing campaigns.

### Business Task
The business problem is to analyze Cyclistic bike-share data to understand how casual riders and annual members utilize the service differently. Specifically, the analysis aims to uncover behavioral patterns, usage trends, and preferences of these customer groups. The results will be utilized to create a new marketing campaign for casual riders to turn them into annual members. The strategy will be evidence-based, leveraging findings from the analysis to maximize customer retention and increase annual memberships to drive long-term growth for Cyclistic.

### Stakeholders
1. Lily Moreno: Director of Marketing at Cyclistic, responsible for campaign development and strategic initiatives to promote the bike-share program.
2. Cyclistic Marketing Analytics Team: Data analysts tasked with collecting, analyzing, and reporting data to guide marketing strategies.
3. Cyclistic Executive Team: Decision-makers who will approve the recommended marketing strategies based on data-driven insights.
4. Cyclistic Operations Team: Responsible for the operational aspects of the bike-share program, including bike maintenance, station management, and customer support.
5. Cyclistic IT Team: Manages data infrastructure, security, and technical support for data analysis.
6. Cyclistic Customer Service Team: Provides insights into customer feedback and user experience with the bike-share service.
7. Customers (Annual Members and Casual Riders): End-users whose behaviors and preferences will be analyzed to improve service offerings and membership conversion strategies.

These stakeholders play crucial roles in various aspects of the project, from data collection and analysis to strategy implementation and operational management.

### Data Preparation
---
### Data Sources 
1. Trip History: Historical data of bike trips taken by Cyclistic riders. This dataset typically includes information such as trip length, start time, end time, bike ID, user type (annual       member or casual rider), and possibly other demographic or user-level data.
2. Station Data: Information about the bike stations owned by Cyclistic, including station coordinates, each station's capacity, and other operating details.
3. Weather Data: Not always required but often included for seasonality exploration, temperature, rain, and wind speed weather data could provide insight into the way conditions affect bike     usage patterns.
4. Geographical Data: Geographic data based on a geographic information system (GIS) can be utilized to analyze spatial patterns of bicycle use across different regions of Chicag
5. Customer Data (Optional): Demographic data, personal preferences, and past membership history can be included if it is available and relevant.

### Data Organization
Trip Data:
- Format: The Date is maintained in CSV (Comma-Separated Values) format, which i uploaded to Rstuido and converted to database tables.
- Organization: Each row for a single bike trip is shown with columns for trip data such as trip duration, start time, end time, bike ID, user type, and possibly other features such as user demographics.
- Combining Data Files
In order to better handle the data, especially keeping in mind the number and that the data ranges from January 2024 to May 2024 in separate CSV files, I combined all of these into a single dataset. This will help have a seamless analysis process and also support keeping things consistent.
Steps to Combine Data Files
---
I. Locating the Files:
All the CSV files were stored in a particular folder on my local machine.

II. Merging Files with Command Prompt on Windows:
I concatenated all the CSV files into one using the Command Prompt. The following are the steps and commands followed:

a. Go to the Folder:
Open Command Prompt and navigate to the directory of your CSV files.

```
C:\Users\user\Desktop\Data Analytics\Coursea\Capstone project\Cyclistic_Trip_Data Cousera Capstone Project\Raw_data_for_cyclistic_trip_data\Csv
files extracted for Cyclistic trip analysis from 2024 data
```
b. Combine the Files:
Use the following command to concatenate all CSV files into a single file named Combined_2024_Cyclistic_trip_analysis_data.csv

```
copy *.csv Combined_2024_Cyclistic_trip_analysis_data.csv
```
c. Reason for Merging Files:
Combining the files simplifies the data processing. Instead of importing a sequence of files and joining them together in R, taking a lot of time, combining them before import reduces the load at the analysis phase. It also ensures data is consistent in structure and ready for use immediately.

d. Importing the Combined File into R:
After the files were merged, the next thing was to import the merged data file into R for processing and analysis.
```
# Load necessary libraries
install.packages("tidyverse")
library(tidyverse)
library(lubridate)

# Load the combined data file
combined_data <- read_csv("Combined_2024_Cyclistic_trip_analysis_data")

# Preview the first few rows of the data
head(Combined_2024_Cyclistic_trip_analysis_data)

# Check the structure of the data
str(Combined_2024_Cyclistic_trip_analysis_data)

```

- Key Fields:
### Data Credibility
