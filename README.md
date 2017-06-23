<div id="devex-badge"><a rel="Exploration" href="https://github.com/BCDevExchange/docs/blob/master/discussion/projectstates.md"><img alt="In production, but maybe in Alpha or Beta. Intended to persist and be supported." style="border-width:0" src="https://assets.bcdevexchange.org/images/badges/exploration.svg" title="In production, but maybe in Alpha or Beta. Intended to persist and be supported." /></a></div>

---

## FASST - Flow Analysis Statistical Summary Tool

This is a set of [R](http://www.r-project.org) scripts to help summarize, analyze, and visualize long-term and continuous
streamflow data. 

The scripts uses the [BCWaterDischargeAnalysis](https://github.com/bcgov/BCWaterDischargeAnalysis/) package and 
your own supplied continuous daily streamflow data in a .csv file to analyze and visualize long-term, annual, monthly, daily, low flows, and trending information.


### Usage

**Input Data**

- Streamflow data must be provided in a .csv file format with two columns: Date (YYYY-MM-DD format) and Discharge (in cubic meters per second). Column headers may differ than suggested. See the following for example:

         |    Date    | Discharge |
         -------------------------
         | 2000-01-30 |   1.035   |
         | 2000-01-31 |   2.400   |
         | 2000-02-01 |   1.899   |
         |     ...    |    ...    |
                     
- If you do not have your own data, daily streamflow data can be downloaded from the 
  [Water Survey of Canada website](https://wateroffice.ec.gc.ca/search/historical_e.html).
  
- The following is required for the analysis (along with other options listed within the scripts):
    - Stream or hydrometric station name (for naming folders, tables, and plots)
    - Upstream drainage basin area, in square kilometres, (optional; for water yield)
    - Choice of Calendar or Water Year analysis
    - Start and End Years to analyze

**Running Scripts**

There are two core scripts that are a part of the analysis, with further instructions within each. The analyses are computed using the 'BCWaterDischargeAnalysis' and 'zyp' packages, and tables and plots are produced using 'tidyr', 'dplyr', 'zoo', and 'ggplot2' packages. The scripts are as follows:

- FASST-R_flow_data_screening.R
    - Use this script to review your dataset for gaps, missing dates, and/or periods of interest to select your start and end years for the analysis in the 02_FASSTr_data_analysis.R script. Not required to run if your start and end years are known.
    - .csv and .pdf outputs are saved in a folder called '0. Data Screening'.
- FASST-R_flow_analysis.R
    - Use this script to analyze your streamflow data. Can either run the entire script, or individual sections of script (as long scripts in Modules 1 and 2 are run prior to other modules).
    - Can run this script without running the FASST-R_flow_data_screening.R script if start and end years of analyses are known.
    - .csv and .pdf outputs are saved in various folders, matching the Module headings.

**Outputs**

Analysis outputs of table (.csv) and plot (.pdf or others by selection) files are managed in the following folders and contained files (file names also contain the year type (Calendar or Water) and the years of the analysis):

- AnalysisOutput - [Stream Name]
    - 0. Data Screening (files from the FASST-R_flow_data_screening.R script)
             - Daily Mean Discharge Time-series.csv
             - Daily Mean Discharge Time-series.pdf
             - Daily Mean Discharge Time-series (logs cale).pdf
             - Annual Daily Discharge Time-series.pdf
             - Annual Daily Discharge Time-series (log scale).pdf
             - Annual Time-series Summary.csv
             - Annual Time-series Summary.pdf
             - Data Completion and Missing Days.csv
    - 1. Time-Series Record (full time-series records and summaries cut to specified start and end years)
             - Daily Mean Discharge Time-series.csv
             - Daily Mean Discharge Time-series.pdf
             - Daily Mean Discharge Time-series (logs cale).pdf
             - Annual Daily Discharge Time-series.pdf
             - Annual Daily Discharge Time-series (log scale).pdf
             - Annual Time-series Summary.csv
    - 2. Long-term Flows (Long-term summary statistics and percentiles)
             - Long-term Summary Statistics.csv
             - Long-term Summary Statistics.pdf
             - Long-term Percentiles.csv
             - Long-term Percentiles - Flow Duration.pdf
    - 3. Annual Flows (Annual summary statistics, total volumetric discharges, yields, and flow timing)
             - Annual Summary Statistics.csv
             - Annual Summary Statistics.pdf
             - Annual and Seasonal Yield (mm).csv
             - Annual and Seasonal Yield (mm).pdf
             - Annual and Seasonal Total Discharge (cubic metres).csv
             - Annual and Seasonal Total Discharge (cubic metres).pdf
             - Timing of Annual Flows.csv
             - Timing of Annual Flows.pdf
             - Days Outside of Normal.csv
             - Days Outside of Normal.pdf
    - 4. Monthly Flows (Month summary statistics and percentiles)
             - Monthly Summary Statistics.csv
             - Monthly Summary Statistics.pdf
    - 5. Daily Flows (Daily and daily cumulative summary statistics)
             - Daily Summary Statistics.csv
             - Daily Summary Statistics.pdf
             - Daily Cumulative Summary Statistics.csv
             - Daily Cumulative Summary Statistics.pdf
    - 6. Low Flows (Annual 1, 3, 7, & 30-day low flows and dates, and low flow frequency analyses)
             - Annual Low Flows.csv
             - Annual Low Flows.pdf
             - Annual Low Flows Date.pdf
             - Low Flow Frequency.csv
             - Low Flow Frequency.pdf
    - 7. Annual Trends (Inputs and results from trending analyses, not all files may appear due to trending analysis selection within script)
             - Annual Trending Metrics.csv
             - Trends Results - MK & SS (Yue and Pilon).csv
             - Trends Results - MK & SS (Yue and Pilon).pdf
             - Trends Results - MK & SS (Zhang).csv
             - Trends Results - MK & SS (Zhang).pdf
             - Trends Results - Linear Regression.csv
             - Trends Results - Linear Regression Plots.pdf
    - 8. BCWaterDischargeAnalysis Outputs (Raw outputs of the package)
             - Various .csv and .pdf output files created by the BCWaterDischargeAnalysis package. These files are the raw results from which the tables and plots for this script are created.


### Known Issues
 - Long-term statistics and percentiles not grouped by Water Years yet (BCWaterDischargeAnalysis issue)
 - Days above or below normal (Annual metric) not included for Water Year (BCWaterDischargeAnalysis issue)
 - General issues related to Water Year metrics for Oct, Nov, and Dec (BCWaterDischargeAnalysis issue)


### Project Status

We are actively developing this analysis, check the 
[issues](https://github.com/bcgov/FASST/issues/) for things we would 
like to fix or work on.

An Excel version of FASST (FASST-X) will be included upon completion.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/FASST/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

    Copyright 2017 Province of British Columbia

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at 

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.

This repository is maintained by [Jon Goetz](https://github.com/jongoetz
), BC Ministry of Environment.


