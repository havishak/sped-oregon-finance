# Analysis of Special Education Finance Data in Oregon

The repository contains code for cleaning, analyzing, and visualizing data for a research project focusing on Oregon's special education funding mechanism and expenditure patterns.

## Directory Structre:

code/ : Contains .R scripts for cleaning and making analysis-ready data sets by data sources. Also contains plotting code used in reports.
data_raw/ : Contains requested raw data from Oregon Department of Education, and publically-available Common Core of Data for use in the study (not on github)
data_clean/ : Contains analysis-ready data after applying cleaning script on data_raw (not on Github)
_extension/ : Contains template for closeread quarto extension
docs/ : Place to render .qmds and used by Github to deploy the website
closeread_figures/ : Contains images to be added in the webpage descriptions

## Data Sources and Cleaning To-Dos

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

5.  Data from Other Sources

-   [] School enrollment
-   [] Staff and school characteristics
-   [] Community characteristics

## Analysis and To-Dos

1. Funding Allocation Data

-   [] What is the distribution of average daily membership (ADM) across districts and across time?
-   [] What is the distribution of student subgroups that get additional funding across districts and across time?
-   [] Does the distribution of student subgroups that get additional funding vary by average daily membership?
-   [] Of the total allocation in ADM units, what percentage of funding comes from regular vs weighted student characteristics?
-   [] Given the 11% cap on funding allocation for students in special education, what proportion of students on IEP get any additional funding weights? How much weights do they get?

2. Revenue Data

-   [] What is the distribution of total revenue generated across districts and years? In per-pupil units?
-   [] What is the composition of revenue across different sources? 
-   [] What is the distribution of total revenue generated from different sources across districts and years? In per-pupil units?

3. High-Cost Disability grant Data

-   [] What is the distribution of students eligible for grant across years?
-   [] What is the distribution of total expenditures, expenditures above threshold, and reimbursements under the grant?

4. Expenditure Data

-   [] What is the distribution of expenditure across districts and time? In per-pupil units?
-   [] What is the distribution of expenditure by different expense categories across districts and time? In per-pupil units?
-   [] What is the distribution of expenditure by different expense categories flagged by subgroups across districts and time? In per-pupil units?
-   [] Is the distribution of expenses flagged by student subgroups vary from the distribution of expensed for general students across districts and time? In per-pupil units?

5.  Relationships between data



