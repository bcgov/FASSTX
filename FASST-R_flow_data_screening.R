
# Copyright 2017 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



###############################################################################
###     Flow Analysis Statistical Summary Tool in R (FASST-R)               ###
###                  ***  Flow Data Screening  ***                          ###
###############################################################################

### Version:      23 June 2017
### Source:       https://github.com/bcgov/FASST

### Contact:      Jon Goetz (BC ENV) https://github.com/jongoetz


## Description:   This script summarizes and displays continuous daily stream discharge information to examine data completeness and missing dates, 
#                 check for outliers, and indentify selected time periods to select start and end years of continous data to complete
#                 a streamflow data analysis (in the FASST-R_flow_analysis script).

## Instructions:  1) Save a .csv file (in the following format) in the working directory (or other) and type in the directory pathway below.
#                     |    Date    | Discharge |
#                     -------------------------
#                     | 2000-01-30 |   1.035   |
#                     | 2000-01-31 |   2.400   |
#                     | 2000-02-01 |   1.899   |
#                     |     ...    |    ...    |
#
#                 2) Type in the stream or hydrometric station name or number in Module 1 - this name is used to create folders and titles 
#                    in tables and figures, so chose appropriately.
#
#                 3) Run entire script and view files in '0. Data Screening' folder.



###############################################################################
# Module 1. Set streamflow station  parameters
###############################################################################

# Type in pathway to flow data and Station Information
Flowdata_Filepath   <- "example_data_08NM116.csv"

##Stream/Station Information
Stream_Name         <- "Mission Creek"

## Plot Output File Type:
plot.filetype       <- "pdf"         # pdf, tiff, png, bmp, ps, tex







###############################################################################
# Module 2. Load packages and functions, setup directories, and load data
###############################################################################

# Load packages for script
#library(BCWaterDischargeAnalysis)
library(dplyr)
library(tidyr)
library(ggplot2) 

# Create a handy function to add title headings to created CSV files
my.write <- function(x, file, header, f = write.csv, ...){
  datafile <- file(file, open = 'wt')   # create and open the file connection
  on.exit(close(datafile))  # close on exit
  if(!missing(header)) writeLines(header,con=datafile)   # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
  f(x, datafile,...)  # write the file using the defined function and required addition arguments  
}


# Create directories for results
report.dir <- paste0('Analysis Output - ',Stream_Name)  # current director
dir.create(report.dir)

screening.dir <- paste0('Analysis Output - ',Stream_Name,"/0. Data Screening")  # current director
dir.create(screening.dir)

# Load flow csv file and fill any gaps with NA
flow.data.csv <- read.csv(Flowdata_Filepath, col.names = c("Date","Q"), stringsAsFactors = FALSE)
flow.data.csv$Date <- as.Date(flow.data.csv$Date,"%Y-%m-%d")
flow.data <- merge(data.frame(Date=seq((as.Date((paste((as.numeric(format(min(flow.data.csv$Date),'%Y'))),01,01,sep="-")),"%Y-%m-%d")),(as.Date((paste((as.numeric(format(max(flow.data.csv$Date),'%Y'))),12,31,sep="-")),"%Y-%m-%d")), by="days")),
                   flow.data.csv,
                   by="Date",all = TRUE)




###############################################################################
# Module 3. Time-series Record
###############################################################################


# Add more date summary columns
flow.data$CalendarYear <- as.numeric(format(flow.data$Date,'%Y'))
flow.data$Month <- as.integer(format(flow.data$Date,'%m'))
flow.data$CalendarDOY <- as.integer(format(flow.data$Date,'%j'))
flow.data$Day <- as.integer(format(flow.data$Date,'%d'))
flow.data$WaterYear <- ifelse(flow.data$Month>=10,flow.data$CalendarYear+1,flow.data$CalendarYear) # water year (Oct 1- Sept 31)
flow.data$WaterDOY <- ifelse(flow.data$Month<10,flow.data$CalendarDOY+92,
                             ifelse((as.Date(with(flow.data, paste(CalendarYear+1,01,01,sep="-")),"%Y-%m-%d")-as.Date(with(flow.data, paste(CalendarYear,01,01,sep="-")),"%Y-%m-%d"))==366,
                                    flow.data$CalendarDOY-274,
                                    flow.data$CalendarDOY-273))


# Add 3, 7, and 30-day rolling means of daily flow (using function created above) and annual cumulative flow
flow.data <- flow.data %>%
  mutate(ThreeDayQ=round(zoo::rollapply(Q,3, mean, fill=NA, align="right"), digits = 3),
         SevenDayQ=round(zoo::rollapply(Q,7, mean, fill=NA, align="right"), digits = 3),
         ThirtyDayQ=round(zoo::rollapply(Q,30, mean, fill=NA, align="right"), digits = 3)) %>%  
  group_by(CalendarYear) %>%
  mutate(CumulativeQCY = cumsum(Q)) %>% 
  group_by(WaterYear) %>% 
  mutate(CumulativeQWY = cumsum(Q)) %>% 
  ungroup()


# Reorganize columns order
flow.data <- flow.data[,c(1,6,4,3,5,7,8,2,9:13)]




# Save the full timeseries record
flow.data.table <- flow.data
flow.data.table[is.na(flow.data.table)] <- ""
my.write(flow.data.table, file = paste0(screening.dir,"/Daily Mean Discharge Time-series (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),").csv"), row.names = FALSE,
         header = paste0("Daily Mean Discharge Time-series - ",Stream_Name," (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),")"))



###############################################################################
# Module 4. Save the time-series plots to check for outliers, etc
###############################################################################


### Plot the full time-series record (linear and log scale)

ggplot(data=flow.data, aes(x=Date, y=Q))+
  #ggtitle("Mean Daily Discharge")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  ylab("Discharge (cms)")+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8),expand = c(0, 0))+
  scale_x_date(breaks = scales::pretty_breaks(n = 12)) +
  theme( panel.border = element_rect(colour = "grey80", fill=NA, size=.5),
         panel.grid.minor.y = element_blank())+
  ggsave(file=file.path(screening.dir, paste("Daily Mean Discharge Time-series (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),").",plot.filetype,sep="")),
       height=6.35, width = 18)

ggplot(data=flow.data, aes(x=Date, y=Q))+
  #ggtitle("Full Streamflow Record")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  ylab("Discharge (cms)")+
  scale_y_log10()+ 
  scale_x_date(breaks = scales::pretty_breaks(n = 12)) +
  annotation_logticks(sides="l",base= 10,colour = "grey25",size=0.3,short = unit(.07, "cm"), mid = unit(.15, "cm"), long = unit(.2, "cm"))+
  theme( panel.border = element_rect(colour = "grey80", fill=NA, size=.5),
         panel.grid.minor.y = element_blank())+
  ggsave(file=file.path(screening.dir, paste("Daily Mean Discharge Time-series (log scale) (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),").",plot.filetype,sep="")),
       height=6.35, width = 18)


### Plot the full time-series record by year (linear and log scale)

ggplot(data=flow.data, aes(x=Date, y=Q))+
  #ggtitle("Annual Streamflow Records")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  facet_wrap(~CalendarYear, scales="free_x")+
  scale_x_date(date_labels = "%b")+
  ylab("Discharge (cms)")+
  ggsave(file=file.path(screening.dir, paste("Annual Daily Discharge Time-series (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),").",plot.filetype,sep="")),
       height=11, width = 17)

ggplot(data=flow.data, aes(x=Date, y=Q))+
  #ggtitle("Annual Streamflow Records")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  facet_wrap(~CalendarYear, scales="free_x")+
  scale_x_date(date_labels = "%b")+
  ylab("Discharge (cms)")+
  scale_y_log10()+ 
  annotation_logticks(sides="l",base= 10,colour = "grey25",size=0.3,short = unit(.07, "cm"), mid = unit(.15, "cm"), long = unit(.2, "cm"))+
  ggsave(file=file.path(screening.dir, paste("Annual Daily Discharge Time-series (log scale) (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),").",plot.filetype,sep="")),
       height=11, width = 17)



###############################################################################
# Module 5. Summarize annual data for metrics and missing flow values
###############################################################################

# Table of simple statistics by year to help check for outliers, etc
# Calculate annual summaries
flow.summary <- flow.data %>%
  group_by(CalendarYear) %>% 
  summarize("Minimum" = min (Q, na.rm=TRUE),
            "Maximum" = max (Q, na.rm=TRUE),
            "Mean" = round(mean(Q,na.rm=TRUE),4),
            "Standard Deviation"= round(sd(Q,na.rm=TRUE),4),
            "Days per Year"  = length(CalendarYear),
            "Number of Flow Values" = sum (!is.na(Q)),
            "Annual Flow Values Missing" = sum ( is.na(Q))) %>% 
  rename("Year" = CalendarYear)
# Calculate monthly summaries
flow.summary.months <- flow.data %>% 
  group_by(CalendarYear,Month) %>% 
  summarize(missing.days = sum ( is.na(Q))) %>% 
  spread(Month,missing.days)
colnames(flow.summary.months) <- c("Year","Jan Flow Values Missing", "Feb Flow Values Missing", "Mar Flow Values Missing", "Apr Flow Values Missing", "May Flow Values Missing","Jun Flow Values Missing","Jul Flow Values Missing","Aug Flow Values Missing","Sep Flow Values Missing","Oct Flow Values Missing","Nov Flow Values Missing","Dec Flow Values Missing")
# Combine annual and 
flow.summary <- merge(flow.summary,flow.summary.months, by="Year")


# Write the record summary
my.write(flow.summary[,1:7], file = paste0(screening.dir,"/Annual Time-series Summary (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),").csv"), row.names = FALSE,
         header = paste0("Annual Time-series Summary - ",Stream_Name," (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),")"))
my.write(flow.summary[,-(2:5)], file = paste0(screening.dir,"/Data Availability and Missing Dates (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),").csv"), row.names = FALSE,
         header = paste0("Data Availability and Missing Dates - ",Stream_Name," (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),")"))


### Plot the record summary 
flow.summary.plotdata <- flow.summary %>%
  select(Year,Minimum,Maximum,Mean,"Standard Deviation") %>% 
  gather(Statistic,Value,2:5)

ggplot(data=flow.summary.plotdata, aes(x=Year, y=Value))+
  #ggtitle("Annual Summary Statistics")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  geom_point(colour="firebrick3")+
  facet_wrap(~Statistic, ncol=2, scales="free_y")+
  ylab("Discharge (cms)")+
  xlab("Year")+
  ggsave(file=file.path(screening.dir, paste("Annual Time-series Summary (",min(flow.data$CalendarYear),"-",max(flow.data$CalendarYear),").",plot.filetype,sep="")))





