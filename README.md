
<!--
Copyright 2017 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
-->

## FASST-X - Flow Analysis Summary Statistics Tool for Excel

[![img](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)

The Flow Analysis Summary Statistics Tool for Excel (‘FASST-X’) is a
Macro-Enabled Excel workbook with worksheets to summarize and visualize
streamflow data. This tool summarizes continuous daily mean streamflow
data into various daily, monthly, annual, and long-term statistics, and
a log-Pearson Type III low flow frequency analysis, in both table and
plot formats.

For the R package `fasstr`, see <https://github.com/bcgov/fasstr>.

### Using FASST-X

Download the latest version of the FASST-X tool
[here](https://github.com/bcgov/FASSTX/raw/master/FASST-X%20Jan2018.xlsm)
or above, currently dated Jan 2018. Currently works just for
Windows-based Excel versions.

FASST-X requires daily streamflow data (in cubic metres per second) to
compute summary statistics. This data can be entered on the main ‘Flow
Data Input’ worksheet. Other parameters such as station name, drainage
basin area (used for yield runoff calculations), selection of calendar
or water years, and selection of start and end years can also be
selected on the main ‘Flow Data Input’ worksheet.

As there are many Macros and cell formula-based calculations, it may
take some time to calculate all the sheets when pressing the ‘Calculate
All Tabs’ button (please be patient). To view data availability, press
the ‘Update Data Screening Tabs’ to review the data if you wish before
calculating everything.

Click through the worksheets to view, copy, and/or export the various
result outputs.

| Worksheet Name                 | Contents                                                                                                                                                              |
|--------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Flow Data Input                | Streamflow and parameter data entry                                                                                                                                   |
| Data Availability              | Counts of available and missing data per year and per month                                                                                                           |
| Data Screening                 | Time series plot and table of the data record provided used for analysis                                                                                              |
| Long-term Statistics           | Long-term statistics (mean, median, minimum, maximum) of all daily values for given months and totals.                                                                |
| Percentiles & Flow Duration    | Percentiles of all daily values for given months and totals.                                                                                                          |
| Annual Statistics              | Annual statistics (mean, median, minimum, maximum) of all daily values for given years                                                                                |
| Total Yield & Volumetric Flows | Annual and seasonal total yield ( in mm) and volumetric flows (in cubic metres); annual dates of timing of flows (dates of 25, 33, 50, and 75% of total annual flows) |
| Annual Low Flows               | Values and dates of annual 1, 3, 7, and 30-day minimum flows                                                                                                          |
| Low Flow Frequencies           | Frequency analysis fitting annual low flows to a log-Pearson Type III distribution to determine frequency quantiles (ie. 7Q10)                                        |
| Monthly Statistics             | Monthly statistics (mean, median, minimum, maximum) of all daily values for all months for all given years                                                            |
| Daily Statistics               | Daily statistics (mean, median, minimum, maximum, selected percentiles) of all daily values for each day of the year                                                  |
| Daily Cumulative Statistics    | Daily cumulative statistics (mean, median, minimum, maximum, selected percentiles) of all daily values for each day of the year                                       |
| Annual Trending Metrics        | All annual and annual monthly values from the workbook merged.                                                                                                        |

### Project Status

This package is set for delivery. This package is maintained by the
Water Protection and Sustainability Branch of the [British Columbia
Ministry of Environment and Climate Change
Strategy](https://www2.gov.bc.ca/gov/content/environment/air-land-water/water).

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/FASSTX/issues/).

### How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

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
