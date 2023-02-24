# Subspecialty_Analysis
## Purpose

###	Primary objective
Determine the proportion of study eligible children between the ages of 0 and 20 who used pediatric subspeciality care at least once in a given calendar year between 2010 and 2021 in any care setting (i.e., in an inpatient hospital, outpatient, emergency department, or community-based practice care setting), stratified by age group and sex.  

###	Secondary objective
For each pediatric subspecialty, determine the most common diagnoses among patients based on the number encounters in the outpatient or community-based practice care setting from 2010 to 2021.  

## Structure
following the PEDSnet "standard framework" structure, the code is divided into various folders including:
* **code**: contains the files that create local database tables from the larger PEDSnet database
* **specs**: contains the CSV files that define codes used to search for relevant terms such as provider and care site specialties
* **site**: used to customize the standard framework implementation for a specific project
* **reporting**: contains the R Markdown (.Rmd) folder used to create the PDF report, and the PDF report once created. 

The code structure is idiosyncratic to the PEDSnet implementation of the OHDSI standard.  If you are not using it, the essential code for creating the database tables that the .Rmd expects to see is in code/driver.R, with the specifications in the specs folder.  

The primary author Mitchell Maltenfort, PhD, can be contacted at maltenform@chop.edu. 
