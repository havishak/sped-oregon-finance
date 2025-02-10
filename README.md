# Analysis of Special Education Finance Data in Oregon

The repository contains code for cleaning, analyzing, and visualizing data for a research project focusing on Oregon's special education funding mechanism and expenditure patterns. 

# Data Sources and Cleaning To-Dos

1. Funding allocation to districts based on student characteristics from 2011-2023

- [ X ] There are multiple files for each year. Read file name and retain the last updated version by checking the date in the file name.
- [ X ] Read the flagged version into R.
- [ ] Make a template of what information lies where: 

	- [ ] Common header in all pages
	- [ ] Page number as footer in all pages
	- [ ] District name in the same line as a 4-digit District ID. Note: All public schools ADM in one block and non-public schools (charter, web, etc.) are in separate block
	- [ ] Before first entry for each school district, there is `year` Extended ADMw as the title. 
	- [ ] Color (grey-white) background separated different schools (2 on 1 page).
	- [ ] First line in each shaded block: school name.
	- [ ] Rows: ADMr, Students in ESL, Students in Pregnant and Parenting Programs, `Total` IEP Students capped at 11% of District ADMr; Students on IEP Above 11% of ADMr; Students in Poverty; Students in Foster Care and Neglected/Delinquent; Remote Elementary School Correction; Small High School Correction. For different years, there might be differences in the row descriptions. Extract all rows of the file should be a function in itself.
	- [ ] There are 4 columns per row: 2 for recent year and 2 for previous year. The syntax for the first row of each year is `total` X `weight`= and the second row of each year is `weighted ADMw`.
	- [ ] Last row has weighted ADMw for each year (no row name). 
	- [ ] The actual granted weighted ADMw is the max of weighted ADM for this and last year to allow schools to adapt to lower enrollment. 

2. School-level expenditure data from 2019-2023

- [ ] Read each file and toggle to sheet (usually 1) that has detailed listing.
- [ ] Summarize by Function Cd, Object Cd, Area of Responsibility
- [ ] Tease out pattern in Area of Responsibility and other codes

3. District-level revenue data from 2019-2023

- [ ] Read each file and toggle to sheet (usually 1) that has detailed listing.
- [ ] Summarize by Fund Cd and Source Cd

4. Reimbursements to districts from their High-cost disability grant

- [ ] From year summary file, get grant aid and threshold
- [ ] From actual payment file, get all variables (actual expenditure, predicted, and reconciliatory payment)

5. Data from Other Sources

# Analysis and To-Dos 

To be added
