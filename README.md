# Analysis of Special Education Finance Data in Oregon

The repository contains code for cleaning, analyzing, and visualizing data for a research project focusing on Oregon's special education funding mechanism. This was my submission for the Data Science Capstone Project, offered by College of Education, in Winter 2025.

## Directory Structre:

code/ : Contains .R scripts for cleaning and making analysis-ready data sets by data sources. Also contains plotting code used in reports.

data_raw/ : Contains requested raw data from Oregon Department of Education, and publically-available Common Core of Data for use in the study (not on github)

data_clean/ : Contains analysis-ready data after applying cleaning script on data_raw (not on Github)

_extension/ : Contains template for closeread quarto extension

docs/ : Place to render .qmds and used by Github to deploy the website

closeread_figures/ : Contains images to be added in the webpage descriptions

## To replicate:

- Clone the repository from the website.

- Request access for data by emailing me, havishak@uoregon.edu

- Load the R project.

- Run the code/replicate.R file to install all necessary packages.

-  After the project opens, in the terminal, write `quarto render`. This will run all the files and save the outputs in the docs folder. The local website version can be accessed at docs/index.html. Or, you can do **Build -> Render Website**.


## Data Sources and Cleaning Goals

1.  Funding allocation to districts based on student characteristics from 2011-2023

-   [x] There are multiple files for each year. Read file name and retain the last updated version by checking the date in the file name.

-   [x] Read the flagged version into R.

-   [x] Make a template of what information lies where:

    -   [x] Common header in all pages
    -   [x] Page number as footer in all pages
    -   [x] District name in the same line as a 4-digit District ID. Note: All public schools ADM in one block and non-public schools (charter, web, etc.) are in separate block
    -   [x] Before first entry for each school district, there is `year` Extended ADMw as the title.
    -   [x] Color (grey-white) background separated different schools (2 on 1 page).
    -   [x] First line in each shaded block: school name.
    -   [x] Rows: ADMr, Students in ESL, Students in Pregnant and Parenting Programs, `Total` IEP Students capped at 11% of District ADMr; Students on IEP Above 11% of ADMr; Students in Poverty; Students in Foster Care and Neglected/Delinquent; Remote Elementary School Correction; Small High School Correction. For different years, there might be differences in the row descriptions. Extract all rows of the file should be a function in itself.
    -   [x] There are 4 columns per row: 2 for recent year and 2 for previous year. The syntax for the first row of each year is `total` X `weight`= and the second row of each year is `weighted ADMw`.
    -   [x] Last row has weighted ADMw for each year (no row name).
    -   [x] The actual granted weighted ADMw is the max of weighted ADM for this and last year to allow schools to adapt to lower enrollment.

2.  School-level expenditure data from 2019-2023

-   [x] Read each file and toggle to sheet (usually 1) that has detailed listing.
-   [x] Summarize by Function Cd, Object Cd, Area of Responsibility
-   [x] Tease out pattern in Area of Responsibility and other codes

3.  District-level revenue data from 2019-2023

-   [x] Read each file and toggle to sheet (usually 1) that has detailed listing.
-   [x] Summarize into local, state, and federal revenue sources.

4.  Reimbursements to districts from their High-cost disability grant

-   [x] From year summary file, get grant aid and threshold
-   [x] From actual payment file, get all variables (actual expenditure, predicted, and reconciliatory payment)


## Data Products

I deployed my findings through a webpage using scrollytelling features. The website has four sections:

- Overview of School Funding in the US
- Walk-through the PDF-scraping process
- Walk-through example
- Report on initial findings 

## Current State

- I am running further analysis, but not updating the Github repository as I want to archive/preserve the close-read functionality. 
- Last updated: 4/8/2025