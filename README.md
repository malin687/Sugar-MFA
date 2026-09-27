# Global Sugar Material Flow Analysis (MFA)

This repository contains the main analysis and visualization code, together with selected processed data, supporting the study:

**Nisnik et al. (2026), "Global flows, losses, and circularity of sugar from cultivation to end users," Resources, Conservation & Recycling.**
https://doi.org/10.1016/j.resconrec.2026.109000

The study develops a global material flow analysis (MFA) of sugarcane and sugar beet, tracing sugar flows from crop production through processing, trade, consumption, and end use, including losses and circularity across the system.

## Repository Contents

### Main analysis and visualization

`SC-SB-MFA-data-22.8.25.R`

Main R script used for the country-level material flow analysis of sugarcane and sugar beet. The script includes data processing and analysis across production, processing, trade, consumption, losses, and end-use allocation, as well as the generation of the main figures used in the study.

Most figures presented in the article were generated within the R workflow. 
The sector-level donut charts were generated separately in Python, while the 
Sankey diagram was created using e!Sankey.

### MFA data

`SC_2018.xlsx`

Sugarcane MFA data for the 2018 reference year.

`SB_2018.xlsx`

Sugar beet MFA data for the 2018 reference year.

### Sector-level donut charts

`Sugar_sectors_donut_chart_23.7.25.py`

The Python script generates the underlying sugarcane and sugar beet donut charts as PDF files. The figures used in the final publication were subsequently refined for presentation in Adobe InDesign.

The script uses:

- `Donut_Chart_sectors_data- sugarcane - 23.7.25.xlsx`
- `Donut_Chart_sectors_data- sugar_beet- 23.7.25.xlsx`

The Python script can be run from the repository directory and generates the corresponding sugarcane and sugar beet donut charts as PDF files.

## Data Sources

Besides FAO crop production statistics, this research draws on several complementary datasets to capture trade flows, consumption, and caloric conversion.

### Global Dietary Database (GDD)

Provides data on added sugar consumption by country, sex, and age group for 185 countries.

Used to analyze sugar intake patterns across populations.

https://www.globaldietarydatabase.org/data-download

### GENuS Dataset (Global Expanded Nutrient Supply)

Provides median daily caloric intake data for 22 age groups by sex.

Used to convert sugar consumption from calories to mass.

Reference:

https://dx.plos.org/10.1371/journal.pone.0146976

### Resource Trade Earth (Chatham House)

Used for global sugar trade across 222 countries in 2018, incorporating import-export data from both producing and non-producing countries.

https://resourcetrade.earth/

## Data Availability and Reproducibility

The repository contains the main analysis scripts and selected processed data files used in the study.

Some raw and external source datasets used in the full MFA workflow are not redistributed in this repository. These datasets were obtained from the original data providers and are referenced above or in the accompanying publication.

As a result, the complete R analysis is provided as research code documenting the analytical workflow, but some sections require external source data that are not included in this repository.

The Python visualization script and its corresponding input files are included and can be run directly from the repository directory.

## Software

## Software

The analysis was conducted primarily in R, with Python used for selected visualizations. e!Sankey was used to create the Sankey diagram, while Adobe Illustrator and Adobe InDesign were used for final figure preparation and layout.

The Python visualization script requires:

- pandas
- matplotlib
- openpyxl

## Citation

If you use the data or code in this repository, please cite:

Nisnik et al. (2026). "Global flows, losses, and circularity of sugar from cultivation to end users." *Resources, Conservation & Recycling*.  
https://doi.org/10.1016/j.resconrec.2026.109000