
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
###     ***  Flow Analysis and Trending  ***                                ###
###############################################################################

### Version:      23 June 2017
### Source:       https://github.com/bcgov/FASST

### Contact:      Jon Goetz (BC ENV) at jon.goetz@gov.bc.ca


## Description:   This script summarizes continuous daily stream discharge into various long-term, annual, monthly, and daily stream
#                 metrics for interpretation and visualization, and completes a trending analysis (using your selected method).

## Instructions:  1) Save a .csv file (in the following format) in the working directory (or other) and type in the directory pathway below
#                     |    Date    | Discharge |
#                     -------------------------
#                     | 2000-01-30 |   1.035   |
#                     | 2000-01-31 |   2.400   |
#                     | 2000-02-01 |   1.899   |
#                     |     ...    |    ...    |
#
#                 2) Type in the stream or hydrometric station name or number in Module 1. - this name is used to create folders and titles 
#                    in tables and figures, so chose appropriately.
#
#                 3) Type in the upstream drainage basin area (in square kilometres), if known. If not known, type in 0(zero).
#
#                 4) Choose the type of year to analyze, Calendar Year (Jan-Dec) or Water Year (Oct-Dec).
#                    If Calendar Year, type FALSE, if Water Year, type TRUE.
#
#                 5) Type in the start and end years of the analysis. As continuous data is ideal, select the first and last years of 
#                    complete data sets.
#
#                 6) Set other parameters and settings in Module 1. as required.
#
#                 7) Run the script and view files in created folders. Can either run entire script, or individual sections of script 
#                    (as long as parameters in Module 1. and run with Module 2. first)


###############################################################################
# Module 1. Set streamflow station and analysis parameters
###############################################################################

## Stream Data Filepath
Flowdata_Filepath      <- "example_data_08NM116.csv"     #flow.data <- flow.data


## Stream/Station Information
Stream_Name             <- "Mission Creek"   
Drainage_Basin_Area     <- 795


## Analysis Information
Water_Year              <- FALSE
Start_Year              <- 1971
End_Year                <- 2010


## Frequency Analysis Options
FA_Distribution            <- "PIII"     # "PIII" = Pearson Log III distribution; "weibull" = Weibull distribution
#FA_Distribtuion_Method     <- "MLE"     # "MOM" = Method of moments; "MLE" = maximum likelihood estimation.


## Trending Options
trends_method <- "All"                  # "MKSens_Zhang" (zyp) / "MKSens_YuePilon" (zyp) / "LinearRegression" / "All" / "None"
alpha <- 0.05


## Missing Values Control
na.rm                   <- list(na.rm.global=FALSE) # If FALSE then stats for months/year period may not be computed if there is missing a value
# If TRUE then stats for months/year period are computed if there is missing a value


## Plot Output File Type:
plot.type               <- "pdf"         # pdf, tiff, png, bmp, ps, tex




###############################################################################
# Module 2. Load packages and functions, setup directories, and load data
###############################################################################

# Load packages for script
library(BCWaterDischargeAnalysis)
library(dplyr)
library(tidyr)
library(ggplot2)


# Create a handy function to add title headings to the first row of outputted CSV files
my.write <- function(x, file, header, f = write.csv, ...){
  datafile <- file(file, open = 'wt')   # create and open the file connection
  on.exit(close(datafile))   # close on exit
  if(!missing(header)) writeLines(header,con=datafile)  # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
  f(x, datafile,...)  # write the file using the defined function and required addition arguments  
}

# Create year type label for plots and tables
yeartype.label <- ifelse(Water_Year,"Water Year","Calendar Year")
yeartype.years.label <- ifelse(Water_Year,paste0("Water Years ",Start_Year,"-",End_Year),paste0("Calendar Years ",Start_Year,"-",End_Year))

# Create directories for results
report.dir <- paste0('AnalysisOutput - ',Stream_Name)
dir.create(report.dir)
raw.dir <- paste0('AnalysisOutput - ',Stream_Name,"/8. BCWaterDischargeAnalysis Outputs")
dir.create(raw.dir)

### Load data from CSV file (create blank time-series from the start and end year and paste the flow values into the appropriate dates)
flow.data.csv <- read.csv(Flowdata_Filepath, col.names = c("Date","Q"), stringsAsFactors = FALSE)
flow.data.csv$Date <- as.Date(flow.data.csv$Date,"%Y-%m-%d")
flow.data <- merge(data.frame(Date=seq((as.Date((paste((as.numeric(format(min(flow.data.csv$Date),'%Y'))),01,01,sep="-")),"%Y-%m-%d")),(as.Date((paste((as.numeric(format(max(flow.data.csv$Date),'%Y'))),12,31,sep="-")),"%Y-%m-%d")), by="days")),
                   flow.data.csv, by="Date",all = TRUE)


###############################################################################
# Module 3. Time-series Record
###############################################################################

timeseries.dir <- paste0('AnalysisOutput - ',Stream_Name,"/1. Time-series Record")
dir.create(timeseries.dir)

# Gather data and add more date summary columns
timeseries <- flow.data

timeseries$CalendarYear <- as.numeric(format(timeseries$Date,'%Y'))
timeseries$Month <- as.integer(format(timeseries$Date,'%m'))
timeseries$CalendarDOY <- as.integer(format(timeseries$Date,'%j'))
timeseries$Day <- as.integer(format(timeseries$Date,'%d'))
timeseries$WaterYear <- ifelse(timeseries$Month>=10,timeseries$CalendarYear+1,timeseries$CalendarYear) # water year (Oct 1- Sept 31)
timeseries$WaterDOY <- ifelse(timeseries$Month<10,timeseries$CalendarDOY+92,
                              ifelse((as.Date(with(timeseries, paste(CalendarYear+1,01,01,sep="-")),"%Y-%m-%d")-as.Date(with(timeseries, paste(CalendarYear,01,01,sep="-")),"%Y-%m-%d"))==366,
                                     timeseries$CalendarDOY-274,
                                     timeseries$CalendarDOY-273))

# Add 3, 7, and 30-day rolling means of daily flow (using function created above) and annual cumulative flow
timeseries <- timeseries %>%
  mutate(ThreeDayQ=round(zoo::rollapply(Q,3, mean, fill=NA, align="right"), digits = 3),
         SevenDayQ=round(zoo::rollapply(Q,7, mean, fill=NA, align="right"), digits = 3),
         ThirtyDayQ=round(zoo::rollapply(Q,30, mean, fill=NA, align="right"), digits = 3)) %>% 
  group_by(CalendarYear) %>%
  mutate(CumulativeQ = cumsum(Q)) %>% 
  group_by(WaterYear) %>% 
  mutate(CumulativeQWY = cumsum(Q))%>%
  ungroup()

# Clip time-series to selected start and end years
if (Water_Year==TRUE){
  timeseries <- timeseries %>% filter(WaterYear >= Start_Year & WaterYear <= End_Year)
} else {
  timeseries <- timeseries %>% filter(CalendarYear >= Start_Year & CalendarYear <= End_Year)
}

# Reorganize columns order
timeseries <- timeseries[,c(1,6,4,3,5,7,8,2,9:13)]

# Save the full timeseries record as a table
timeseries.table <- timeseries
timeseries.table[is.na(timeseries.table)] <- ""
my.write(timeseries.table, file = paste0(timeseries.dir,"/Daily Mean Discharge Time-series (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Daily Mean Discharge Time-series - ",Stream_Name," (",yeartype.years.label,")"))


### Plot the full time-series record (linear and log scale)
#####################################

ggplot(data=timeseries, aes(x=Date, y=Q))+
  #ggtitle(paste0("Daily Discharge - ",Stream_Name," (",yeartype.years.label,")"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  scale_x_date(breaks = scales::pretty_breaks(n = 12)) +
  ylab("Discharge (cms)")+
  scale_y_continuous()+
  theme( panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
         panel.grid.minor.y = element_blank())+
  ggsave(file=file.path(timeseries.dir, paste("Daily Mean Discharge Time-series (",yeartype.years.label,").",plot.type,sep="")),
         height=6.35, width = 18)

ggplot(data=timeseries, aes(x=Date, y=Q))+
  #ggtitle(paste0("Daily Discharge - ",Stream_Name," (",yeartype.years.label,")"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  ylab("Discharge (cms)")+
  scale_y_log10()+ 
  scale_x_date(breaks = scales::pretty_breaks(n = 12)) +
  theme( panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
         panel.grid.minor.y = element_blank())+
  annotation_logticks(sides="l",base= 10,colour = "grey25",size=0.3,short = unit(.07, "cm"), mid = unit(.15, "cm"), long = unit(.2, "cm"))+
  ggsave(file=file.path(timeseries.dir, paste("Daily Mean Discharge Time-series (log scale) (",yeartype.years.label,").",plot.type,sep="")),
         height=6.35, width = 18)


### Plot the full time-series record by year (linear and log scale)
#####################################

ggplot(data=timeseries, aes(x=Date, y=Q))+
  #ggtitle(paste0("Daily Discharge by Year - ",Stream_Name," (",yeartype.years.label,")"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  facet_wrap(~CalendarYear, scales="free_x")+
  {if(Water_Year)facet_wrap(~WaterYear, scales="free_x")}+
  scale_x_date(date_labels = "%b")+
  ylab("Discharge (cms)")+
  theme( panel.border = element_rect(colour = "grey80", fill=NA, size=.1))+
  ggsave(file=file.path(timeseries.dir, paste("Annual Daily Discharge Time-series (",yeartype.years.label,").",plot.type,sep="")),
         height=11, width = 17)

ggplot(data=timeseries, aes(x=Date, y=Q))+
  #ggtitle(paste0("Daily Discharge by Year - ",Stream_Name," (",yeartype.years.label,")"))+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_line(colour="dodgerblue4")+
  facet_wrap(~CalendarYear, scales="free_x")+
  {if(Water_Year)facet_wrap(~WaterYear, scales="free_x")}+
  scale_x_date(date_labels = "%b")+
  ylab("Discharge (cms)")+
  scale_y_log10()+ 
  theme( panel.border = element_rect(colour = "grey80", fill=NA, size=.1))+
  annotation_logticks(sides="l",base= 10,colour = "grey25",size=0.3,short = unit(.07, "cm"), mid = unit(.15, "cm"), long = unit(.2, "cm"))+
  ggsave(file=file.path(timeseries.dir, paste("Annual Daily Discharge Time-series (log scale) (",yeartype.years.label,").",plot.type,sep="")),
         height=11, width = 17)


### Summarize data by year for overview
#####################################

if (Water_Year==TRUE){
  timeseries$AnalysisYear <- timeseries$WaterYear
} else {
  timeseries$AnalysisYear <- timeseries$CalendarYear
}

timeseries.summary <- timeseries %>% 
  group_by(AnalysisYear) %>% 
  summarize("Minimum" = min (Q, na.rm=TRUE),
            "Maximum" = max (Q, na.rm=TRUE),
            "Mean" = round(mean(Q,na.rm=TRUE),4),
            "Standard Deviation"= round(sd(Q,na.rm=TRUE),4),
            "Days per Year"  = length(CalendarYear),
            "Number of Flow Values" = sum (!is.na(Q)),
            "Annual Flow Values Missing" = sum ( is.na(Q))) %>% 
  rename("Year" = AnalysisYear)

my.write(timeseries.summary, file = paste0(timeseries.dir,"/Annual Time-series Summary (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Annual Time-series Summary - ",Stream_Name," (",yeartype.years.label,")"))




###############################################################################
# Module 4. Long-term - Summary Statistics
###############################################################################

longterm.dir <- paste0('AnalysisOutput - ',Stream_Name,"/2. Long-term Flows")  # current director
dir.create(longterm.dir)

# Run Analysis and wrangle data
longterm.stats.results <- compute.Q.stat.longterm(Station.Code=Stream_Name,
                                                  Station.Area=Drainage_Basin_Area,
                                                  flow=flow.data,
                                                  start.year=Start_Year,
                                                  end.year=End_Year,
                                                  write.stat.csv = TRUE,
                                                  write.stat.trans.csv = TRUE,
                                                  report.dir=raw.dir,
                                                  csv.nddigits = 4)

longterm.flows <- longterm.stats.results$Q.stat.longterm 
longterm.flows$Month <-   recode(longterm.flows$Month, "1" = "Jan","2"="Feb", "3"="Mar", "4"="Apr", "5"="May","6"="Jun","7"="Jul","8"="Aug","9"="Sep","10"="Oct","11"="Nov","12"="Dec", "Longterm"="Long-term")
if (Water_Year == TRUE) {
  longterm.flows$Month <- factor(longterm.flows$Month, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term"))
} else {
  longterm.flows$Month <- factor(longterm.flows$Month, levels=c("Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Long-term"))
}

# Save the table
longterm.table <- longterm.flows %>% rename("Mean"=mean,"Median"=median,"Maximum"=max,"Minimum"=min)
longterm.table[,2:4] <- round(longterm.table[,2:4],4)
longterm.table <- longterm.table %>% gather(Statistic,Value,2:5) %>% spread(Month,Value)
my.write(longterm.table,file = paste0(longterm.dir,"/Long-term Summary Statistics (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Long-term Summary Statistics - ",Stream_Name," (",yeartype.years.label,")"))



# Save the plot
longterm.stats <- longterm.flows[13,]
longterm.maxmin <- longterm.flows[1:12,c(1,4:5)] %>% gather(Stat,Value,2:3)
longterm.month <- longterm.flows[1:12,]
ggplot(longterm.maxmin, aes(Month,Value))+
  geom_col(aes(fill="Max-Min Range"))+
  geom_hline(aes(yintercept=longterm.stats$mean, colour="Long-term Mean"), size=.6, linetype=2)+
  geom_hline(aes(yintercept=longterm.stats$median, colour="Long-term Median"), size=.5, linetype=2)+
  geom_line(data=longterm.month,aes(x=longterm.month$Month,y=longterm.month$mean,group = 1, colour="Monthly Mean"), size=.6)+
  geom_point(data=longterm.month,aes(x=longterm.month$Month,y=longterm.month$mean,group = 1, colour="Monthly Mean"), size=3)+
  geom_line(data=longterm.month,aes(x=longterm.month$Month,y=longterm.month$median,group = 1, colour="Monthly Median"), size=.6)+
  geom_point(data=longterm.month,aes(x=longterm.month$Month,y=longterm.month$median,group = 1, colour="Monthly Median"), size=3)+
  scale_fill_manual(values = c("Max-Min Range"="lightblue"))+
  scale_colour_manual(values = c("Long-term Mean"="skyblue2","Long-term Median"="dodgerblue4", "Monthly Mean"="skyblue2","Monthly Median"="dodgerblue4"))+
  scale_y_log10(expand = c(0, 0))+   #scale_y_continuous(expand = c(0, 0))+   #
  annotation_logticks(base= 10,"l",colour = "grey25",size=0.3,short = unit(.07, "cm"), mid = unit(.15, "cm"), long = unit(.2, "cm"))+
  ylab("Discharge (cms)")+
  guides(fill=guide_legend(override.aes = list(linetype=0,shape='')),
         colour=guide_legend(override.aes = list(linetype=c(2,2,1,1), shape=c(NA,NA,16,16))))+
  #ggtitle(paste0("Longterm Flows - ",Stream_Name," (",yeartype.years.label,")"))+
  theme(legend.position = "right", legend.title = element_blank(),legend.margin=unit(0, "cm"),legend.justification = "top",
        panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
        plot.title = element_text(size=12, colour = "grey25",face="italic"),
        panel.grid = element_line(size=.2),
        panel.grid.major.x  = element_blank())+
  ggsave(filename = paste0(longterm.dir,"/Long-term Summary Statistics (",yeartype.years.label,").",plot.type),height=6, width = 11)




###############################################################################
# Module 5. Long-term - Percentiles and flow duration
###############################################################################

longterm.dir <- paste0('AnalysisOutput - ',Stream_Name,"/2. Long-term Flows")  # current director
dir.create(longterm.dir)

# Run Analysis and wrangle data
longterm.percentiles.results <- compute.Q.percentile.longterm(Station.Code=Stream_Name,
                                                              Station.Area=Drainage_Basin_Area,
                                                              flow=flow.data,
                                                              start.year=Start_Year,
                                                              end.year=End_Year,
                                                              write.stat.csv = TRUE,
                                                              write.stat.trans.csv = TRUE,
                                                              report.dir=raw.dir)

percentile.flows.long <- longterm.percentiles.results$Q.percentile.stat %>% gather(Percentile, Value, 2:24)
percentile.flows.long$Month[percentile.flows.long$Month=="All years"] <- "Long-term"
percentile.flows.long$Percentile <- 100-as.numeric(gsub("P","",percentile.flows.long$Percentile))
if (Water_Year == TRUE) {
  percentile.flows.long$Month <- factor(percentile.flows.long$Month, levels=c("Oct","Nov","Dec","Jan", "Feb", "Mar", "Apr", "May","Jun","Jul","Aug","Sep","Long-term"))
} else {
  percentile.flows.long$Month <- factor(percentile.flows.long$Month, levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec','Long-term'))
}

# save the table
percentile.flows.table <- percentile.flows.long %>% spread(Month,Value) %>% mutate(Percentile = 100-Percentile)
percentile.flows.table[,2:14] <- round(percentile.flows.table[,2:14],4)
my.write(percentile.flows.table,file = paste0(longterm.dir,"/Long-term Percentiles (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Long-term Percentile Flows - ",Stream_Name," (",yeartype.years.label,")"))

# Save the flow duration curves
ggplot(percentile.flows.long,aes(x=Percentile,y=Value,colour=Month))+
  geom_line()+
  scale_y_log10(expand = c(0, 0))+
  scale_x_continuous(expand =c(0,0),breaks = scales::pretty_breaks(n = 10))+
  #ggtitle(paste0("Flow Duration Curves - ",Stream_Name," (",yeartype.years.label,")"))+
  ylab("Discharge (cms)")+
  xlab("% Time flow equalled or exceeded")+
  scale_color_manual(values = c("Jan" = "dodgerblue3", "Feb" = "skyblue1", "Mar" = "turquoise","Apr" = "forestgreen", "May" = "limegreen","Jun" = "gold","Jul" = "orange", "Aug" = "red","Sep" = "darkred", "Oct" = "orchid", "Nov" = "purple3","Dec" = "midnightblue","Long-term" = "black"))+
  annotation_logticks(sides="l",base= 10,colour = "grey25",size=0.3,short = unit(.07, "cm"), mid = unit(.15, "cm"), long = unit(.2, "cm"))+
  theme(#plot.title = element_text(size=12, colour = "grey25",face="italic"),
    panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
    panel.grid = element_line(size=.2),
    legend.title = element_blank(),legend.justification = "top")+
  ggsave(filename = paste0(longterm.dir,"/Long-term Percentiles - Flow Duration (",yeartype.years.label,").",plot.type),height=6, width = 11)



###############################################################################
# Module 6. Annual Summary Statistics, Yield/Discharge
###############################################################################

annual.dir <- paste0('AnalysisOutput - ',Stream_Name,"/3. Annual Flows")  # current director
dir.create(annual.dir)

# Run Analysis and wrangle data
annual.stats.results <- compute.Q.stat.annual(Station.Code=Stream_Name,
                                              Station.Area=Drainage_Basin_Area,
                                              flow=flow.data,
                                              start.year=Start_Year,
                                              end.year=End_Year,
                                              write.cy.stat.csv = TRUE, #ifelse(Water_Year==TRUE,FALSE,TRUE),
                                              write.wy.stat.csv = TRUE, #ifelse(Water_Year==TRUE,TRUE,FALSE),        # write out statistics?
                                              write.stat.trans.csv=TRUE,  # write out statistics in transposed format
                                              write.flow.summary.csv=TRUE,
                                              write.lowflow.csv = TRUE,
                                              plot.stat.trend=TRUE,
                                              plot.cumdepart = TRUE,
                                              report.dir=raw.dir,
                                              na.rm=na.rm)

if (Water_Year == TRUE) {
  all.annual.temp1 <- annual.stats.results$Q.stat.annual[,c(1,99:115,19:21,23:25,116:119)]  ## JUST WATER YEAR STATS, MISSING DAY OF YEAR
  all.annual.temp2 <- annual.stats.results$Q.stat.annual[,c(1,22,26)] %>% mutate(Year=Year+1) #shift OND values to match WY
  all.annual <- merge(all.annual.temp1,all.annual.temp2,by="Year",all=TRUE) %>% filter(Year<=End_Year)
} else {
  all.annual <- annual.stats.results$Q.stat.annual[,c(1,2:21,23:25,116:119,22,26)]
}


### Annual Summary Statistics
#####################################

# Wrangle data
annual.flows <- all.annual[,c(1,10:12)]
colnames(annual.flows) <- c("Year","Minimum","Maximum","Mean") ### ADD MEDIAN

#Save the table
annual.flows.table <- annual.flows %>% mutate(Mean=round(Mean,4))
my.write(annual.flows.table,file = paste0(annual.dir,"/Annual Summary Statistics (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Annual Summary Statistics - ",Stream_Name," (",yeartype.years.label,")"))

#Save the plot
annual.flows.long <- annual.flows %>% gather(Statistic, Value,2:4)
ggplot(annual.flows.long,aes(Year,Value, colour=Statistic))+
  geom_line()+
  geom_point()+
  scale_y_log10()+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
  annotation_logticks(sides="l",base= 10,colour = "grey25",size=0.3,short = unit(.07, "cm"), mid = unit(.15, "cm"), long = unit(.2, "cm"))+
  #ggtitle(paste0("Annual Streamflow - ",Stream_Name," (",yeartype.years.label,")"))+
  ylab("Discharge (cms)")+
  xlab(paste(yeartype.label))+
  scale_color_manual(values = c("Maximum" = "dodgerblue3", "Mean" = "skyblue1", "Minimum" = "turquoise"),
                     labels = c("Maximum" = "Max. Daily", "Mean" = "Mean Daily", "Minimum" = "Min. Daily"))+
  theme(#plot.title = element_text(size=12, colour = "grey25",face="italic"),
    panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
    panel.grid = element_line(size=.2),
    legend.title = element_blank(), legend.justification = "top")+
  ggsave(filename = paste0(annual.dir,"/Annual Summary Statistics (",yeartype.years.label,").",plot.type),height=6, width = 11)



### Annual Yield Summary Statistics
#####################################

# Wrangle data
if (Water_Year==TRUE){
  annual.yield.flows <- all.annual[,c(1,14,30,22:24,27,28)]  #yield.flows <- stat.annual$Q.stat.annual[,c(1,13,15:18)]
  colnames(annual.yield.flows) <- c("Year","Total Annual Yield","Oct-Dec Yield","Jan-Mar Yield","Apr-Jun Yield","Jul-Sep Yield","Oct-Mar Yield","Apr-Sep Yield")
} else {
  annual.yield.flows <- all.annual[,c(1,14,22:24,30,27,28)]  #yield.flows <- stat.annual$Q.stat.annual[,c(1,13,15:18)]
  colnames(annual.yield.flows) <- c("Year","Total Annual Yield","Jan-Mar Yield","Apr-Jun Yield","Jul-Sep Yield","Oct-Dec Yield","Oct-Mar Yield","Apr-Sep Yield")
}

# Save the table
annual.yield.flows[,2:8] <- round(annual.yield.flows[,2:8],1)
my.write(annual.yield.flows,file = paste0(annual.dir,"/Annual and Seasonal Yield (mm) (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Annual and Seasonal Yield (mm) - ",Stream_Name," (",yeartype.years.label,")"))

# Save the plot
annual.yield.flows.long <- annual.yield.flows %>% gather(Statistic,Value,2:8)
if (Water_Year==TRUE){
  annual.yield.flows.long$Statistic = factor(annual.yield.flows.long$Statistic, levels=c("Total Annual Yield","Oct-Dec Yield","Jan-Mar Yield","Apr-Jun Yield","Jul-Sep Yield","Oct-Mar Yield","Apr-Sep Yield"))
} else {
  annual.yield.flows.long$Statistic = factor(annual.yield.flows.long$Statistic, levels=c("Total Annual Yield","Jan-Mar Yield","Apr-Jun Yield","Jul-Sep Yield","Oct-Dec Yield","Oct-Mar Yield","Apr-Sep Yield"))
}
ggplot(data=annual.yield.flows.long, aes(x=Year, y=Value))+
  geom_line(aes(colour=Statistic))+
  geom_point(aes(colour=Statistic))+
  facet_wrap(~Statistic, scales="free_x")+
  #ggtitle(paste0("Annual and Seasonal Yield - ",Stream_Name," (",yeartype.years.label,")"))+
  #theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  ylab("Water Yield (mm)")+
  xlab(paste(yeartype.label))+
  guides(colour=FALSE)+
  theme(panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
        panel.grid = element_line(size=.2))+
  scale_colour_manual(values = c("Total Annual Yield"="black","Jan-Mar Yield"="dodgerblue3", "Apr-Jun Yield"="forestgreen","Jul-Sep Yield"="red", "Oct-Dec Yield"="purple3","Oct-Mar Yield"="midnightblue", "Apr-Sep Yield"="orange"))+
  ggsave(filename = paste0(annual.dir,"/Annual and Seasonal Yield (mm) (",yeartype.years.label,").",plot.type),height=6, width = 11)



### Total Q flows
#####################################

# Wrangle data
if (Water_Year==TRUE){
  annual.totalQ.flows <- all.annual[,c(1,29,13,19:21,25,26)]  #totalQ <- stat.annual$Q.stat.annual[,c(1,13:14,19:26,99:105)]
  colnames(annual.totalQ.flows) <- c("Year","Annual Total","Oct-Dec Total","Jan-Mar Total","Apr-Jun Total","Jul-Sep Total","Oct-Mar Total","Apr-Sep Total")
} else {
  annual.totalQ.flows <- all.annual[,c(1,13,19:21,29,25,26)]  #totalQ <- stat.annual$Q.stat.annual[,c(1,13:14,19:26,99:105)]
  colnames(annual.totalQ.flows) <- c("Year","Annual Total","Jan-Mar Total","Apr-Jun Total","Jul-Sep Total","Oct-Dec Total","Oct-Mar Total","Apr-Sep Total")
}

# Save the table
annual.totalQ.flows[,2:8] <- round(annual.totalQ.flows[,2:8],1)
my.write(annual.totalQ.flows,file = paste0(annual.dir,"/Annual and Seasonal Total Discharge (cubic metres) (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Annual and Seasonal Total Discharge (cubic metres) - ",Stream_Name," (",yeartype.years.label,")"))

# Save the plot
annual.totalQ.long <- annual.totalQ.flows %>% gather(Statistic,Value,2:8)
if (Water_Year==TRUE){
  annual.totalQ.long$Statistic = factor(annual.totalQ.long$Statistic, levels=c("Annual Total","Oct-Mar Total","Jan-Mar Total","Apr-Jun Total","Jul-Sep Total","Oct-Dec Total","Apr-Sep Total"))
} else {
  annual.totalQ.long$Statistic = factor(annual.totalQ.long$Statistic, levels=c("Annual Total","Jan-Mar Total","Apr-Jun Total","Jul-Sep Total","Oct-Dec Total","Oct-Mar Total","Apr-Sep Total"))
}
ggplot(data=annual.totalQ.long, aes(x=Year, y=Value))+
  geom_line(aes(colour=Statistic))+
  geom_point(aes(colour=Statistic))+
  facet_wrap(~Statistic, scales="free_x")+
  #ggtitle(paste0("Annual and Seasonal Total Discharge - ",Stream_Name," (",yeartype.years.label,")"))+
  #theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  ylab("Total Discharge (cubic metres)")+
  xlab(paste(yeartype.label))+
  guides(colour=FALSE)+
  theme(panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
        panel.grid = element_line(size=.2))+
  scale_colour_manual(values = c("Annual Total"="black","Jan-Mar Total"="dodgerblue3", "Apr-Jun Total"="forestgreen","Jul-Sep Total"="red", "Oct-Dec Total"="purple3","Oct-Mar Total"="midnightblue", "Apr-Sep Total"="orange"))+
  ggsave(filename = paste0(annual.dir,"/Annual and Seasonal Total Discharge (cubic metres) (",yeartype.years.label,").",plot.type),height=6, width = 11)



### Timing of Annual Flows
#####################################

# Wrangle data
annual.timing.flows <- all.annual[,c(1,16:18)]#timing.flows <- stat.annual$Q.stat.annual[,c(1,16:18)]
colnames(annual.timing.flows) <- c("Year","Day of 25% of Flow","Day of 50% of Flow","Day of 75% of Flow")
if (Water_Year==TRUE){
  annual.timing.flows$"Date of 25% of Flow" <- as.Date(annual.timing.flows$"Day of 25% of Flow", origin = paste0(annual.timing.flows$Year-1,"-09-30"))
  annual.timing.flows$"Date of 50% of Flow" <- as.Date(annual.timing.flows$"Day of 50% of Flow", origin = paste0(annual.timing.flows$Year-1,"-09-30"))
  annual.timing.flows$"Date of 75% of Flow" <- as.Date(annual.timing.flows$"Day of 75% of Flow", origin = paste0(annual.timing.flows$Year-1,"-09-30"))
} else {
  annual.timing.flows$"Date of 25% of Flow" <- as.Date(annual.timing.flows$"Day of 25% of Flow", origin = paste0(annual.timing.flows$Year-1,"-12-31"))
  annual.timing.flows$"Date of 50% of Flow" <- as.Date(annual.timing.flows$"Day of 50% of Flow", origin = paste0(annual.timing.flows$Year-1,"-12-31"))
  annual.timing.flows$"Date of 75% of Flow" <- as.Date(annual.timing.flows$"Day of 75% of Flow", origin = paste0(annual.timing.flows$Year-1,"-12-31"))
}

#Save the table
annual.timing.table <- annual.timing.flows
my.write(annual.timing.table,file = paste0(annual.dir,"/Timing of Annual Flows (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Timing of Annual Flows - ",Stream_Name," (",yeartype.years.label,")"))

#Save the plot
annual.timing.flows.long <- annual.timing.flows[,c(1,2:4)] %>% gather(Percent,Date,2:4)
ggplot(annual.timing.flows.long,aes(Year,Date, colour=Percent))+
  geom_line()+
  geom_point()+
  #ggtitle(paste0("Timing of Annual Flows - ",Stream_Name," (",yeartype.years.label,")"))+
  ylab("Day of Year")+
  xlab(paste(yeartype.label))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_color_manual(values = c("Day of 25% of Flow" = "dodgerblue3", "Day of 50% of Flow" = "skyblue1", "Day of 75% of Flow" = "turquoise"))+
  theme(#plot.title = element_text(size=12, colour = "grey25",face="italic"),
    panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
    panel.grid = element_line(size=.2),
    legend.position = "right", legend.justification = "top", legend.title = element_blank())+
  ggsave(filename = paste0(annual.dir,"/Timing of Annual Flows (",yeartype.years.label,").",plot.type),height=6, width = 11)



### Days above Normal
#####################################

if (Water_Year==TRUE){
  annual.days.normal <- annual.stats.results$Q.stat.annual[,c(1,120:122)]  ## JUST CALENDAR YEAR STATS
  annual.days.normal[2:4] <- NA
} else {
  annual.days.normal <- annual.stats.results$Q.stat.annual[,c(1,120:122)]  ## JUST CALENDAR YEAR STATS
}

colnames(annual.days.normal) <- c("Year","Days Below 25th Percentile","Days Above 75th Percentile","Days Outside 25-75th Percentiles")

# Save the table
my.write(annual.days.normal,file = paste0(annual.dir,"/Days Outside of Normal (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Number of Days Outside of Normal (25-75th Percentiles) - ",Stream_Name," (",yeartype.years.label,")"))

# Save the plot
annnual.days.normal.long <- annual.days.normal %>% gather(Statistic,Value,2:4)
ggplot(annnual.days.normal.long,aes(Year,Value, colour=Statistic))+
  geom_line()+
  geom_point()+
  #ggtitle(paste0("Number of Days Outside of Normal (25-75th Percentiles) - ",Stream_Name," (",yeartype.years.label,")"))+
  ylab("Number of Days")+
  xlab("Year")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_color_manual(values = c("Days Below 25th Percentile" = "red2", "Days Above 75th Percentile" = "dodgerblue3", "Days Outside 25-75th Percentiles" = "purple3"))+
  theme(#plot.title = element_text(size=12, colour = "grey25",face="italic"),
    panel.border = element_rect(colour = "grey80", fill=NA, size=.1),
    panel.grid = element_line(size=.2),
    legend.position = "right", legend.title = element_blank(), legend.justification = "top")+
  ggsave(filename = paste0(annual.dir,"/Days Outside of Normal (",yeartype.years.label,").",plot.type),height=6, width = 11)








###############################################################################
# Module 7. Monthly Summary Statistics
###############################################################################

month.dir <- paste0('AnalysisOutput - ',Stream_Name,"/4. Monthly Flows")  # current director
dir.create(month.dir)


# Run Analysis and wrangle data
annual.stats.results <- compute.Q.stat.annual(Station.Code=Stream_Name,
                                              Station.Area=Drainage_Basin_Area,
                                              flow=flow.data,
                                              start.year=Start_Year,
                                              end.year=End_Year,
                                              write.cy.stat.csv = TRUE, #ifelse(Water_Year==TRUE,FALSE,TRUE),
                                              write.wy.stat.csv = TRUE, #ifelse(Water_Year==TRUE,TRUE,FALSE),        # write out statistics?
                                              write.stat.trans.csv=TRUE,  # write out statistics in transposed format
                                              write.flow.summary.csv=TRUE,
                                              write.lowflow.csv = TRUE,
                                              plot.stat.trend=TRUE,
                                              plot.cumdepart = TRUE,
                                              report.dir=raw.dir,
                                              na.rm=na.rm)

if (Water_Year == TRUE) {
  all.monthly.temp1 <- annual.stats.results$Q.stat.annual[,c(1,27:35,39:47,51:59,63:71,75:83,87:95)]
  all.monthly.temp2 <- annual.stats.results$Q.stat.annual[,c(1,36:38,48:50,60:62,72:74,84:86,96:98)] %>% mutate(Year=Year+1)
  all.monthly <- merge(all.monthly.temp1,all.monthly.temp2,by="Year",all=TRUE) %>% filter(Year<=End_Year)
  all.monthly <- all.monthly[,c(1,56:58,2:10,59:61,11:19,62:64,20:28,65:67,29:37,68:70,38:46,71:73,47:55)]
  monthly.flows <- all.monthly
  colnames(monthly.flows) <- c("Year","October Minimum","November Minimum","December Minimum","January Minimum","February Minimum","March Minimum","April Minimum","May Minimum","June Minimum","July Minimum","August Minimum","September Minimum","October Maximum","November Maximum","December Maximum","January Maximum","February Maximum","March Maximum","April Maximum","May Maximum","June Maximum","July Maximum","August Maximum","September Maximum","October Mean","November Mean","December Mean","January Mean","February Mean","March Mean","April Mean","May Mean","June Mean","July Mean","August Mean","September Mean","October Median","November Median","December Median","January Median","February Median","March Median","April Median","May Median","June Median","July Median","August Median","September Median","October 20th Percentile","November 20th Percentile","December 20th Percentile","January 20th Percentile","February 20th Percentile","March 20th Percentile","April 20th Percentile","May 20th Percentile","June 20th Percentile","July 20th Percentile","August 20th Percentile","September 20th Percentile","October 10th Percentile","November 10th Percentile","December 10th Percentile","January 10th Percentile","February 10th Percentile","March 10th Percentile","April 10th Percentile","May 10th Percentile","June 10th Percentile","July 10th Percentile","August 10th Percentile","September 10th Percentile")
} else {
  all.monthly <- annual.stats.results$Q.stat.annual[,c(1,27:98)]
  monthly.flows <- all.monthly
  colnames(monthly.flows) <- c("Year","January Minimum","February Minimum","March Minimum","April Minimum","May Minimum","June Minimum","July Minimum","August Minimum","September Minimum","October Minimum","November Minimum","December Minimum","January Maximum","February Maximum","March Maximum","April Maximum","May Maximum","June Maximum","July Maximum","August Maximum","September Maximum","October Maximum","November Maximum","December Maximum","January Mean","February Mean","March Mean","April Mean","May Mean","June Mean","July Mean","August Mean","September Mean","October Mean","November Mean","December Mean","January Median","February Median","March Median","April Median","May Median","June Median","July Median","August Median","September Median","October Median","November Median","December Median","January 20th Percentile","February 20th Percentile","March 20th Percentile","April 20th Percentile","May 20th Percentile","June 20th Percentile","July 20th Percentile","August 20th Percentile","September 20th Percentile","October 20th Percentile","November 20th Percentile","December 20th Percentile","January 10th Percentile","February 10th Percentile","March 10th Percentile","April 10th Percentile","May 10th Percentile","June 10th Percentile","July 10th Percentile","August 10th Percentile","September 10th Percentile","October 10th Percentile","November 10th Percentile","December 10th Percentile")
}


# Save the table
monthly.flows[,26:38] <- round(monthly.flows[,26:38],3)
my.write(monthly.flows,file = paste0(month.dir,"/Monthly Summary Statistics (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Annual Monthly Flow Summary Statistics - ",Stream_Name," (",yeartype.years.label,")"))

# save the plot
monthly.flows.long <- monthly.flows %>% gather(Statistic,Value,2:73) %>% 
  separate(Statistic,c("Month","Statistic"))
monthly.flows.long$Statistic[monthly.flows.long$Statistic=="10th"] <- "10th Percentile"
monthly.flows.long$Statistic[monthly.flows.long$Statistic=="20th"] <- "20th Percentile"

if (Water_Year == TRUE) {
  monthly.flows.long$Month = factor(monthly.flows.long$Month, levels=c('October','November','December','January','February','March','April','May','June','July','August','September'))
} else {
  monthly.flows.long$Month = factor(monthly.flows.long$Month, levels=c('January','February','March','April','May','June','July','August','September','October','November','December'))
}

pdf(file = paste0(month.dir,"/Monthly Summary Statistics (",yeartype.years.label,").pdf"),11,8.5)
for (stat in unique(monthly.flows.long$Statistic)) {
  monthly.plot.data <- monthly.flows.long %>% filter(Statistic==stat)
  monthly.flows.plot <-ggplot(data=monthly.plot.data, aes(x=Year, y=Value))+
    geom_line(aes(colour=Month), alpha=0.5)+
    geom_point(aes(colour=Month))+
    facet_wrap(~Month, scales="free_x")+
    ggtitle(paste0("Monthly ",stat," Flows"))+
    theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_line(size=.2),
          panel.border = element_rect(colour = "grey80", fill=NA, size=.1))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
    ylab("Discharge (cms)")+
    guides(colour=FALSE)+
    scale_colour_manual(values = c("January" = "dodgerblue3", "February" = "skyblue1", "March" = "turquoise",
                                   "April" = "forestgreen", "May" = "limegreen","June" = "gold",
                                   "July" = "orange", "August" = "red","September" = "darkred",
                                   "October" = "orchid", "November" = "purple3","December" = "midnightblue"))
  plot(monthly.flows.plot)
}
dev.off()



###############################################################################
# Module 8. Daily Statistics
###############################################################################

daily.dir <- paste0('AnalysisOutput - ',Stream_Name,"/5. Daily Flows")  # current director
dir.create(daily.dir)

# Wrangle data (not part of BCWaterDishcargeAnalysis package)
daily.flows.data <- merge(data.frame(Date=seq(as.Date((paste((as.numeric(format(min(flow.data$Date),'%Y'))),01,01,sep="-")),"%Y-%m-%d"), as.Date((paste((as.numeric(format(max(flow.data$Date),'%Y'))),12,31,sep="-")),"%Y-%m-%d"), by="days")),flow.data,by="Date",all = TRUE)

daily.flows.data$CalendarYear <- as.numeric(format(daily.flows.data$Date,'%Y'))
daily.flows.data$Month <- as.integer(format(daily.flows.data$Date,'%m'))
daily.flows.data$CalendarDOY <- as.integer(format(daily.flows.data$Date,'%j'))
daily.flows.data$Day <- as.integer(format(daily.flows.data$Date,'%d'))
daily.flows.data$WaterYear <- ifelse(daily.flows.data$Month>=10,daily.flows.data$CalendarYear+1,daily.flows.data$CalendarYear) # water year (Oct 1- Sept 31)
daily.flows.data$WaterDOY <- ifelse(daily.flows.data$Month<10,daily.flows.data$CalendarDOY+92,
                                    ifelse((as.Date(with(daily.flows.data, paste(CalendarYear+1,01,01,sep="-")),"%Y-%m-%d")-as.Date(with(daily.flows.data, paste(CalendarYear,01,01,sep="-")),"%Y-%m-%d"))==366,
                                           daily.flows.data$CalendarDOY-274,
                                           daily.flows.data$CalendarDOY-273))

daily.flows.data <- daily.flows.data %>% 
  group_by(CalendarYear) %>% 
  mutate(CumulativeQ = cumsum(Q)) %>% 
  group_by(WaterYear) %>% 
  mutate(CumulativeQWY = cumsum(Q)) %>% 
  ungroup()

if (Water_Year==TRUE){
  daily.flows.data$analysis.Year <- daily.flows.data$WaterYear
  daily.flows.data$analysis.DOY <- daily.flows.data$WaterDOY
  daily.flows.data$analysis.Date <- as.Date(daily.flows.data$WaterDOY, origin = "1899-09-30")
  daily.flows.data$analysis.CumQ <- daily.flows.data$CumulativeQWY
} else {
  daily.flows.data$analysis.Year <- daily.flows.data$CalendarYear
  daily.flows.data$analysis.DOY <- daily.flows.data$CalendarDOY
  daily.flows.data$analysis.Date <- as.Date(daily.flows.data$CalendarDOY, origin = "1899-12-31")
  daily.flows.data$analysis.CumQ <- daily.flows.data$CumulativeQ
}

### Daily Summary Statistics
#####################################

# Wrangle data and run analysis
daily.flows.data <- daily.flows.data %>% filter(analysis.Year >= Start_Year & analysis.Year <= End_Year)

daily.flows <- daily.flows.data %>% 
  group_by(analysis.Date,analysis.DOY)%>% 
  filter(analysis.DOY < 366) %>% # removes any day 366 during leap years; i.e. only the first 365 days of each year are summarized
  summarize(Mean=mean(Q, na.rm=TRUE),
            Minimum=min(Q, na.rm=TRUE),
            FifthPercentile=quantile(Q,.05, na.rm=TRUE),
            TwentyFifthPercentile=quantile(Q,.25, na.rm=TRUE),
            Median=median(Q, na.rm=TRUE),
            SeventyFifthPercentile=quantile(Q,.75, na.rm=TRUE),
            NinetyFifthPercentile=quantile(Q,.95, na.rm=TRUE),
            Maximum=max(Q, na.rm=TRUE))

# Save the table
daily.flows.table <- daily.flows %>% 
  rename("Date" = analysis.Date, "Day of Year" = analysis.DOY, "5th Percentile" = FifthPercentile, "25th Percentile" = TwentyFifthPercentile,"Median (50th Percentile)" = Median,"75th Percentile" = SeventyFifthPercentile,"95th Percentile" = NinetyFifthPercentile)
daily.flows.table$"Date" <- format(as.Date(daily.flows.table$"Date"),format="%b-%d")
daily.flows.table[,3:10] <- round(daily.flows.table[,3:10],4)
my.write(daily.flows.table,file = paste0(daily.dir,"/Daily Summary Statistics (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Daily Summary Statistics - ",Stream_Name," (",yeartype.years.label,")"))

# Save the daily discharge reports as PDFs
pdf(file = paste0(daily.dir,"/Daily Summary Statistics (",yeartype.years.label,").pdf"),8.5,4)
# Create the normals plot
daily.plot <- ggplot(daily.flows,aes(x=analysis.Date)) + 
  geom_ribbon(aes(ymin=Minimum,ymax=Maximum,fill = "Max-Min Range of Flow"))+
  geom_ribbon(aes(ymin=FifthPercentile,ymax=NinetyFifthPercentile,fill = "Range of 90% of Flow"))+
  geom_ribbon(aes(ymin=TwentyFifthPercentile,ymax=SeventyFifthPercentile,fill = "Range of 50% of Flow"))+
  geom_line(aes(y=Median, colour="Median Flow"), size=.5)+
  geom_line(aes(y=Mean, colour="Mean Flow"), size=.5) +
  scale_fill_manual(values = c("Max-Min Range of Flow" = "lightblue2" ,"Range of 90% of Flow" = "lightblue3", "Range of 50% of Flow" = "lightblue4")) +
  scale_color_manual(values = c("Mean Flow" = "paleturquoise", "Median Flow" = "dodgerblue4"),labels = c("Mean Flow", "Median Flow")) +
  scale_y_log10(expand = c(0, 0)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = as.Date(c(NA,as.character(max(daily.flows$analysis.Date)))),expand=c(0,0)) +
  xlab(NULL)+
  ylab("Discharge (cms)")+
  #ggtitle(paste0("Daily Stream Discharge - ",Stream_Name," (",Start_Year,"-",End_Year,")"))+
  theme(axis.text=element_text(size=6, colour = "grey25"),
        axis.title=element_text(size=8, colour = "grey25"),
        axis.ticks = element_line(size=.1, colour = "grey25"),
        axis.ticks.length=unit(0.05,"cm"),
        axis.title.y=element_text(margin=margin(0,0,0,0)),
        #plot.title = element_text(size=12, colour = "grey25",hjust = 0.5,face="italic"),
        panel.border = element_rect(colour = "grey50", fill=NA, size=.1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=.1),
        panel.background = element_rect(fill = "grey94"),
        legend.title = element_blank(),         #legend.position = "top", 
        legend.text = element_text(size=7, colour="grey25"),
        legend.box = "vertical",legend.justification = "top",
        legend.key.size = unit(0.4,"cm"),
        legend.margin=unit(0, "cm")) +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))+
  annotation_logticks(base= 10,"left",colour = "grey25",size=0.3,
                      short = unit(.07, "cm"), mid = unit(.15, "cm"), long = unit(.2, "cm"))
plot(daily.plot)
for (yr in unique(daily.flows.data$analysis.Year)){
  daily.year.data <- daily.flows.data %>% filter(analysis.Year==yr)
  daily.plot.year <- daily.plot +
    geom_line(data = daily.year.data, aes(x=analysis.Date, y=Q, colour= "yr.colour"), size=0.5) +
    scale_color_manual(values = c("Mean Flow" = "paleturquoise", "Median Flow" = "dodgerblue4", "yr.colour" = "red"),
                       labels = c("Mean Flow", "Median Flow",paste0(yr," Flows")))
  plot(daily.plot.year)   
}
dev.off()



### Daily Cumulative Summary Statistics
#####################################

# Wrangle data and run analysis
daily.cumulative.flows <- daily.flows.data %>% 
  filter(analysis.Year >= Start_Year & analysis.Year <= End_Year) %>% 
  group_by(analysis.Date,analysis.DOY)%>% 
  filter(analysis.DOY < 366) %>%
  mutate(analysis.CumQ=analysis.CumQ*.0864) %>%
  summarize(Mean=mean(analysis.CumQ, na.rm=TRUE),
            Minimum=min(analysis.CumQ, na.rm=TRUE),
            FifthPercentile=quantile(analysis.CumQ,.05, na.rm=TRUE),
            TwentyFifthPercentile=quantile(analysis.CumQ,.25, na.rm=TRUE),
            Median=median(analysis.CumQ, na.rm=TRUE),
            SeventyFifthPercentile=quantile(analysis.CumQ,.75, na.rm=TRUE),
            NinetyFifthPercentile=quantile(analysis.CumQ,.95, na.rm=TRUE),
            Maximum=max(analysis.CumQ, na.rm=TRUE))

## Save the table
daily.cumulative.table <- daily.cumulative.flows %>% 
  rename("Date" = analysis.Date, "Day of Year" = analysis.DOY, "5th Percentile" = FifthPercentile, "25th Percentile" = TwentyFifthPercentile,"Median (50th Percentile)" = Median,"75th Percentile" = SeventyFifthPercentile,"95th Percentile" = NinetyFifthPercentile)
daily.cumulative.table$"Date" <- format(as.Date(daily.cumulative.table$"Date"),format="%b-%d")
daily.cumulative.table[,3:10] <- round(daily.cumulative.table[,3:10],4)
my.write(daily.cumulative.table,file = paste0(daily.dir,"/Daily Cumulative Summary Statistics (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Daily Cumulative Summary Statistics (million cubic metres) - ",Stream_Name," (",yeartype.years.label,")"))

# Save the daily discharge reports as PDFs
pdf(file = paste0(daily.dir,"/Daily Cumulative Summary Statistics (",yeartype.years.label,").pdf"),8.5,4)
# Create the normals plot
daily.cumulative.plot <- ggplot(daily.cumulative.flows,aes(x=analysis.Date)) + 
  geom_ribbon(aes(ymin=Minimum,ymax=FifthPercentile,fill = "Min-5th Percentile"))+
  geom_ribbon(aes(ymin=FifthPercentile,ymax=TwentyFifthPercentile,fill = "5th-25th Percentile"))+
  geom_ribbon(aes(ymin=TwentyFifthPercentile,ymax=SeventyFifthPercentile,fill = "25th-75th Percentile"))+
  geom_ribbon(aes(ymin=SeventyFifthPercentile,ymax=NinetyFifthPercentile,fill = "75th-95th Percentile"))+
  geom_ribbon(aes(ymin=NinetyFifthPercentile,ymax=Maximum,fill = "95th Percentile-Max"))+
  scale_fill_manual(values = c("Min-5th Percentile" = "orange" ,"5th-25th Percentile" = "yellow", "25th-75th Percentile" = "skyblue1","75th-95th Percentile" = "dodgerblue2","95th Percentile-Max" = "royalblue4")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = as.Date(c(NA,as.character(max(daily.cumulative.flows$analysis.Date)))),expand=c(0,0)) +
  xlab(NULL)+
  ylab("Cumulative Discharge (million cubic metres)")+
  #ggtitle(paste0("Daily Cumulative Discharge - ",Stream_Name," (",yeartype.years.label,")"))+
  theme(axis.text=element_text(size=6, colour = "grey25"),
        axis.title=element_text(size=8, colour = "grey25"),
        axis.title.y=element_text(margin=margin(0,0,0,0)),
        axis.ticks = element_line(size=.1, colour = "grey25"),
        axis.ticks.length=unit(0.05,"cm"),
        #plot.title = element_text(size=12, colour = "grey25",hjust = 0.5,face="italic"),
        panel.border = element_rect(colour = "grey50", fill=NA, size=.1),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size=.1),
        panel.background = element_rect(fill = "grey94"),
        legend.title = element_blank(),        #legend.position = "top",
        legend.text = element_text(size=7, colour="grey25"),
        legend.box = "vertical",legend.justification = "top",
        legend.key.size = unit(0.4,"cm"),
        legend.margin=unit(0, "cm")) +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))
plot(daily.cumulative.plot)
# Add each year data to normals plot
for (yr in unique(daily.flows.data$analysis.Year)){
  daily.cumulative.year.data <- daily.flows.data %>% filter(analysis.Year==yr)
  daily.cumulative.plot.year <- daily.cumulative.plot +
    geom_line(data = daily.cumulative.year.data, aes(x=analysis.Date, y=analysis.CumQ*.0864, colour= "yr.colour"), size=1) +
    scale_color_manual(values = c("yr.colour" = "red"),
                       labels = c(paste0(yr," Flows")))
  plot(daily.cumulative.plot.year)
}
dev.off()





###############################################################################
# Module 9. Low Flows - Annual Low Flows and Dates
###############################################################################

lowflow.dir <- paste0('AnalysisOutput - ',Stream_Name,"/6. Low Flows")  # current director
dir.create(lowflow.dir)

# Run Analysis and wrangle data
annual.stats.results <- compute.Q.stat.annual(Station.Code=Stream_Name,
                                              Station.Area=Drainage_Basin_Area,
                                              flow=flow.data,
                                              start.year=Start_Year,
                                              end.year=End_Year,
                                              write.cy.stat.csv = TRUE, #ifelse(Water_Year==TRUE,FALSE,TRUE),
                                              write.wy.stat.csv = TRUE, #ifelse(Water_Year==TRUE,TRUE,FALSE),        # write out statistics?
                                              write.stat.trans.csv=TRUE,  # write out statistics in transposed format
                                              write.flow.summary.csv=TRUE,
                                              write.lowflow.csv = TRUE,
                                              plot.stat.trend=TRUE,
                                              plot.cumdepart = TRUE,
                                              report.dir=raw.dir,
                                              na.rm=na.rm)

if (Water_Year == TRUE) {
  lowflow.flows <- annual.stats.results$Q.stat.annual[,c(1,99:106)]
} else {
  lowflow.flows <- annual.stats.results$Q.stat.annual[,c(1:9)]
}

colnames(lowflow.flows) <- c("Year","1-Day Min.","Day of 1-Day Min.","3-Day Min.","Day of 3-Day Min.","7-Day Min.","Day of 7-Day Min.","30-Day Min.","Day of 30-Day Min.")
if (Water_Year==TRUE){
  lowflow.flows$"Date of 1-Day Min." <- as.Date(lowflow.flows$"Day of 1-Day Min.", origin = paste0(lowflow.flows$Year-1,"-09-30"))
  lowflow.flows$"Date of 3-Day Min." <- as.Date(lowflow.flows$"Day of 3-Day Min.", origin = paste0(lowflow.flows$Year-1,"-09-30"))
  lowflow.flows$"Date of 7-Day Min." <- as.Date(lowflow.flows$"Day of 7-Day Min.", origin = paste0(lowflow.flows$Year-1,"-09-30"))
  lowflow.flows$"Date of 30-Day Min." <- as.Date(lowflow.flows$"Day of 30-Day Min.", origin = paste0(lowflow.flows$Year-1,"-09-30"))
} else {
  lowflow.flows$"Date of 1-Day Min." <- as.Date(lowflow.flows$"Day of 1-Day Min.", origin = paste0(lowflow.flows$Year-1,"-12-31"))
  lowflow.flows$"Date of 3-Day Min." <- as.Date(lowflow.flows$"Day of 3-Day Min.", origin = paste0(lowflow.flows$Year-1,"-12-31"))
  lowflow.flows$"Date of 7-Day Min." <- as.Date(lowflow.flows$"Day of 7-Day Min.", origin = paste0(lowflow.flows$Year-1,"-12-31"))
  lowflow.flows$"Date of 30-Day Min." <- as.Date(lowflow.flows$"Day of 30-Day Min.", origin = paste0(lowflow.flows$Year-1,"-12-31"))
}

#Save the table
lowflow.table <- lowflow.flows[,c(1:3,10,4,5,11,6,7,12,8,9,13)]
lowflow.table[,c(2,5,8,11)] <- round(lowflow.table[,c(2,5,8,11)],4)
my.write(lowflow.table,file = paste0(lowflow.dir,"/Annual Low Flows (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Annual Low Flows - ",Stream_Name," (",yeartype.years.label,")"))

#Save the Plot of Low Flow Values
lowflow.Q.long <- lowflow.flows[,c(1,2,4,6,8)] %>% gather(Statistic, Value,2:5)
lowflow.Q.long$Statistic = factor(lowflow.Q.long$Statistic, levels=c("1-Day Min.", "3-Day Min.","7-Day Min.", "30-Day Min."))
ggplot(lowflow.Q.long,aes(Year,Value, colour=Statistic))+
  geom_line()+
  geom_point()+
  scale_y_continuous()+
  scale_x_continuous()+
  # ggtitle(paste0("Annual Low Flows - ",Stream_Name," (",yeartype.years.label,")"))+
  ylab("Discharge (cms)")+
  xlab(paste(yeartype.label))+
  scale_color_manual(values = c("1-Day Min." = "dodgerblue3", "3-Day Min." = "skyblue1", "7-Day Min." = "turquoise", "30-Day Min." = "cornflowerblue"))+
  theme(#plot.title = element_text(size=12, colour = "grey25",face="italic"),
    panel.border = element_rect(colour = "grey50", fill=NA, size=.1),
    panel.grid = element_line(size=.2),
    legend.title = element_blank(),legend.justification = "top")+
  ggsave(filename = paste0(lowflow.dir,"/Annual Low Flows (",yeartype.years.label,").",plot.type),height=5, width = 11)

#Save the Plot of Low Flow Dates
lowflow.date.long <- lowflow.flows[,c(1,3,5,7,9)] %>% gather(Statistic, Value,2:5)
lowflow.date.long$Statistic = factor(lowflow.date.long$Statistic, levels=c("Day of 1-Day Min.", "Day of 3-Day Min.","Day of 7-Day Min.", "Day of 30-Day Min."))
ggplot(lowflow.date.long, aes(Year,Value, colour=Statistic))+
  geom_line()+
  geom_point()+
  #ggtitle(paste0("Annual Low Flow Dates - ",Stream_Name," (",yeartype.label," ",Start_Year,"-",End_Year,")"))+
  ylab("Day of Year")+
  xlab(paste(yeartype.label))+
  scale_color_manual(values = c("Day of 1-Day Min." = "dodgerblue3", "Day of 3-Day Min." = "skyblue1", "Day of 7-Day Min." = "turquoise", "Day of 30-Day Min." = "royalblue"))+
  theme(#plot.title = element_text(size=12, colour = "grey25",face="italic"),
    panel.border = element_rect(colour = "grey50", fill=NA, size=.1),
    panel.grid = element_line(size=.2),
    legend.title = element_blank(),legend.justification = "top")+
  ggsave(filename = paste0(lowflow.dir,"/Annual Low Flows Date (",yeartype.years.label,").",plot.type),height=5, width = 11)


###############################################################################
# Module 10. Low Flows - Low Flow Frequency Analysis
###############################################################################

lowflow.dir <- paste0('AnalysisOutput - ',Stream_Name,"/6. Low Flows")  # current director
dir.create(lowflow.dir)

# Run Analysis and wrangle data
lowflow.freq.results <- compute.volume.frequency.analysis(Station.Code= Stream_Name,
                                                          flow        = flow.data,
                                                          start.year  = Start_Year,
                                                          end.year    = End_Year,
                                                          use.water.year = ifelse(Water_Year==TRUE,TRUE,FALSE),
                                                          roll.avg.days = c(1,3,7,30),
                                                          fit.quantiles = c(.5,.2,.1,.04,.02,.01,.005),
                                                          use.log = FALSE,
                                                          use.max = FALSE,
                                                          fit.distr = FA_Distribution,
                                                          #fit.distr.method = FA_Distribtuion_Method,
                                                          write.stat.csv = TRUE,
                                                          write.plotdata.csv = TRUE,
                                                          write.quantiles.csv = TRUE,
                                                          report.dir = raw.dir)

lowflow.freq.data <- lowflow.freq.results$Q.stat.trans
colnames(lowflow.freq.data) <- c("Year","1-Day Min.","3-Day Min.","7-Day Min.","30-Day Min.")
lowflow.freq.data[,-1] <- round(lowflow.freq.data[,-1],4)

# Save the low flow mininmum values table
#my.write(lowflow.freq.data,file = paste0(lowflow.dir,"/Low Flow Frequency Minimums (",yeartype.years.label,").csv"), row.names = FALSE,
#         header = paste0("Low Flow Frequency Minimums - ",Stream_Name," (",yeartype.years.label,")"))

# Save the low flow analysis results table
lowflow.freq.table <- lowflow.freq.results$fitted.quantiles.trans
colnames(lowflow.freq.table) <- c("Distribution","Probability","Return Period","1Q","3Q","7Q","30Q")
lowflow.freq.table[,-(1:3)] <- round(lowflow.freq.table[,-(1:3)],4)
lowflow.freq.table[,3] <- round(lowflow.freq.table[,3],3)
my.write(lowflow.freq.table,file = paste0(lowflow.dir,"/Low Flow Frequency (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Low Flow Frequency - ",Stream_Name," (",yeartype.years.label,")"))

# Save the low flow analysis results plot
lowflow.freq.plot <- lowflow.freq.results$freqplot+
  ggtitle(NULL)+   #ggtitle(paste0("Low Flow Frequencies - ",Stream_Name," (",yeartype.years.label,")"))+
  ylab("Low Flow Discharge (cms)")+
  theme(legend.position = "right",
        panel.border = element_rect(colour = "grey50", fill=NA, size=.1),
        panel.grid = element_line(size=.2))+
  ggsave(filename = paste0(lowflow.dir,"/Low Flow Frequency (",yeartype.years.label,").",plot.type),height=6, width = 11)




###############################################################################
# Module 11. Annual Trending
# *** Must run Modules 6 and 7 beforehand to complete
###############################################################################

trends.dir <- paste0('AnalysisOutput - ',Stream_Name,"/7. Annual Trends")  # current director
dir.create(trends.dir)


# Merge all annual trending metrics from analysis
all_annual_trending_metrics <- merge(annual.flows,lowflow.flows[,1:9], by="Year")
all_annual_trending_metrics <- merge(all_annual_trending_metrics,annual.yield.flows, by="Year")                                    
all_annual_trending_metrics <- merge(all_annual_trending_metrics,annual.totalQ.flows, by="Year")
all_annual_trending_metrics <- merge(all_annual_trending_metrics,annual.timing.flows[,1:4], by="Year")
all_annual_trending_metrics <- merge(all_annual_trending_metrics,annual.days.normal, by="Year")
all_annual_trending_metrics <- merge(all_annual_trending_metrics,monthly.flows, by="Year")

my.write(all_annual_trending_metrics,file = paste0(trends.dir,"/Trends Input - Annual Metrics (",yeartype.years.label,").csv"), row.names = FALSE,
         header = paste0("Annual Trending Metrics  - ",Stream_Name," (",yeartype.years.label,")"))



# Mann Kendall Sen's Slope - Yue and Pilon
#####################################

if (trends_method == "MKSens_YuePilon" | trends_method == "All") {
  
  library(zyp)
  
  # Calculate some summary stats
  trends.summary <- all_annual_trending_metrics %>%
    gather(Metric,Value,-1) %>% 
    dplyr::group_by(Metric) %>%
    dplyr::summarize(Mean= round(mean(Value, na.rm=TRUE),3),
                     Maximum= round(max(Value, na.rm=TRUE),3),
                     Minimum= round(min(Value, na.rm=TRUE),3))
  
  # Setup metrics for zyp analysis
  trending.metrics.zyp <- all_annual_trending_metrics %>% 
    gather(Metric,Value,-1) %>%
    spread(Year,Value)
  trending.metrics.zyp[,c(-1)] <- round(trending.metrics.zyp[,c(-1)],3)
  
  # Run zyp analysis and compute some statistics
  trends.results.zyp <- zyp.trend.dataframe(data.frame(trending.metrics.zyp), metadata.cols = 1, method = "yuepilon")
  trends.results <- merge(trends.results.zyp,trends.summary, by="Metric") %>% 
    mutate(Years = End_Year-Start_Year+1, # period length (i.e. 30 years)
           RelativeTrend = (trendp*Years)/intercept, # relative trend (normalizes per station)
           PercentChange = trendp/Mean*100) # percent change in the slope value in 2015 vs 1986
  
  # Save the table
  my.write(trends.results,file = paste0(trends.dir,"/Trends Results - MK & SS (Yue and Pilon) (",yeartype.years.label,").csv"), row.names = FALSE,
           header = paste0("Trends Results - MK & SS (Yue and Pilon) - ",Stream_Name," (",yeartype.years.label,")"))
  
  # Plot the results
  trends.plot.data <- trending.metrics.zyp %>% gather(Year,Value,-1) %>% mutate(Year=as.numeric(Year))
  pdf(file = paste0(trends.dir,"/Trends Results - MK & SS (Yue and Pilon) (",yeartype.years.label,").pdf"),8,5)
  for (metric in unique(trends.plot.data$Metric)){
    # Filter for metric
    trends.data.metric <- trends.plot.data %>% filter(Metric==metric)
    trends.results.metric <- trends.results %>% filter(Metric==metric)
    #int <- trends.results.metric$intercept - trends.results.metric$trend * (Start_Year)
    # Plot each metric
    trends.plot <- ggplot(trends.data.metric,aes(x=Year,y=Value))+
      geom_point()+  geom_line(alpha = 0.3) +
      ggtitle(paste0(metric,"   (Sig. = ",round(trends.results.metric$sig,3),")"))+
      xlab(paste(yeartype.label))+
      ylab("Units")+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      theme(panel.border = element_rect(colour = "grey50", fill=NA, size=.1),
        panel.grid = element_line(size=.2))
    # If sig. trend, plot trend
    if (trends.results.metric$sig < alpha & !is.na(trends.results.metric$sig)) {
      trends.plot <- trends.plot+
        geom_abline(slope = trends.results.metric$trend, intercept = (trends.results.metric$intercept - trends.results.metric$trend * (Start_Year)), colour="red")
    }
    plot(trends.plot)
  }
  dev.off()
}


# Mann Kendall Sen's Slope - Zhang
#####################################

if (trends_method == "MKSens_Zhang" | trends_method == "All") {
  
  library(zyp) # req'd for trends analysis
  
  # Calculate some summary stats
  trends.summary <- all_annual_trending_metrics %>%
    gather(Metric,Value,-1) %>% 
    dplyr::group_by(Metric) %>%
    dplyr::summarize(Mean= round(mean(Value, na.rm=TRUE),3),
                     Maximum= round(max(Value, na.rm=TRUE),3),
                     Minimum= round(min(Value, na.rm=TRUE),3))
  
  # Setup metrics for zyp analysis
  trending.metrics.zyp <- all_annual_trending_metrics %>% 
    gather(Metric,Value,-1) %>%
    spread(Year,Value)
  trending.metrics.zyp[,c(-1)] <- round(trending.metrics.zyp[,c(-1)],3)
  
  # Run zyp analysis and compute some statistics
  trends.results.zyp <- zyp.trend.dataframe(data.frame(trending.metrics.zyp), metadata.cols = 1, method = "zhang")
  trends.results <- merge(trends.results.zyp,trends.summary, by="Metric") %>% 
    mutate(Years = End_Year-Start_Year+1, # period length (i.e. 30 years)
           RelativeTrend = (trendp*Years)/intercept, # relative trend (normalizes per station)
           PercentChange = trendp/Mean*100) # percent change in the slope value in 2015 vs 1986
  
  # Save the table
  my.write(trends.results,file = paste0(trends.dir,"/Trends Results - MK & SS (Zhang) (",yeartype.years.label,").csv"), row.names = FALSE,
           header = paste0("Trends Results - MK & SS (Zhang) - ",Stream_Name," (",yeartype.years.label,")"))
  
  # Plot the results
  trends.plot.data <- trending.metrics.zyp %>% gather(Year,Value,-1) %>% mutate(Year=as.numeric(Year))
  pdf(file = paste0(trends.dir,"/Trends Results - MK & SS (Zhang) (",yeartype.years.label,").pdf"),8,5)
  for (metric in unique(trends.plot.data$Metric)){
    # Filter for metric
    trends.data.metric <- trends.plot.data %>% filter(Metric==metric)
    trends.results.metric <- trends.results %>% filter(Metric==metric)
    int <- trends.results.metric$intercept - trends.results.metric$trend * (Start_Year)
    # Plot each metric
    trends.plot <- ggplot(trends.data.metric,aes(x=Year,y=Value))+
      geom_point()+  geom_line(alpha = 0.3) +
      ggtitle(paste0(metric,"   (Sig. = ",round(trends.results.metric$sig,3),")"))+
      xlab(paste(yeartype.label))+
      ylab("Units")+
      scale_x_continuous(breaks = scales::pretty_breaks(n = 12))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      theme(panel.border = element_rect(colour = "grey50", fill=NA, size=.1),
            panel.grid = element_line(size=.2))
    # If sig. trend, plot trend
    if (trends.results.metric$sig < alpha & !is.na(trends.results.metric$sig)) {
      trends.plot <- trends.plot+
        geom_abline(slope = trends.results.metric$trend, intercept = int, colour="red")
    }
    plot(trends.plot)
  }
  dev.off()
}


# Linear Regression, adjusting for autocorrelation
# Credit: Carl Swartz SFU
#####################################

if (trends_method == "LinearRegression" | trends_method == "All") {
  
  options(useFancyQuotes=FALSE) # renders summary output corrects
  options(width=200)
  
  library(car)
  library(ggfortify)
  library(ggplot2)
  library(Kendall)
  library(lmtest)
  library(lsmeans)
  library(nlme)
  library(plyr)
  library(reshape2)
  
  # Set the Data
  trending.metrics.LR <- all_annual_trending_metrics %>% gather(Statistic,value,-1)
  trending.metrics.LR <- trending.metrics.LR[complete.cases(trending.metrics.LR),]
  
  # do the analysis for each statistic
  linear.reg.results <- dlply(trending.metrics.LR, "Statistic", function (x){
    # make sure that values are sorted
    x <- x[ order(x$Year),]
    
    # Naive fit
    prelimplot <- ggplot2::ggplot(data=x, aes(x=Year, y=value))+
      ggtitle(paste(x$Statistic[1], " - Trend analysis"))+
      xlab("Year")+ylab("log(stats.long Pelts)")+
      geom_point(size=4)+
      geom_smooth(method="lm", color="black")
    
    # Compute the naive fit
    naive.fit <- lm(value ~ Year, data=x)
    
    # Get the residual plots from the naive fit
    resid <- data.frame(resid=resid(naive.fit),Year=x$Year)
    resid$lagresid <- c(NA, resid$resid[1:(length(resid$resid)-1)])
    residtimeplot <- ggplot2::ggplot(data=resid, aes(x=Year, y=resid))+
      ggtitle(paste(x$Statistic[1], " - Time plot of residuals from naive fit"))+
      geom_point()+
      geom_line()+
      geom_hline(yintercept=0)
    residlagplot <- ggplot2::ggplot(data=resid, aes(x=lagresid, y=resid))+
      ggtitle(paste(x$Statistic[1], " - Lag plot of residuals from naive fit"))+
      geom_point()
    
    # other standard diagnostic plots
    plotdiag <- autoplot(naive.fit)
    
    # check for autocorrelation using Durbin-Watson test.
    # You can use the durbinWatsontest in the car package or the
    #                 dwtest in the lmtest package
    # For small sample sizes both are fine; for larger sample sizes use the lmtest package
    # Note the difference in the default direction of the alternative hypothesis
    
    DW1 <- car::durbinWatsonTest(naive.fit) # from the car package
    
    DW2 <-lmtest::dwtest(naive.fit) # from the lmtest package
    
    # Fit a model allowing for autocorrelation using gls
    ar1.fit <- nlme::gls(value ~ Year, data=x,
                         correlation=corAR1(form=~1))
    
    # Fit a model allowing for autocorrelation using arima
    #arima.fit <-  with(x, arima(value, xreg=Year, order=c(1,0,0)))
    
    # return the results
    list(prelimplot   =prelimplot,
         naive.fit    =naive.fit,
         residtimeplot=residtimeplot,
         residlagplot=residlagplot,
         plotdiag = plotdiag,
         DW1      = DW1,
         DW2      = DW2,
         ar1.fit  = ar1.fit
         #        arima.fit= arima.fit
    )
  })
  
  pdf(file = paste0(trends.dir,"/Trends Results - Linear Regression Plots (",yeartype.years.label,".pdf"),8,5)
  print(linear.reg.results)
  dev.off()
  
  
  
  linear.reg.report <- plyr::ldply(linear.reg.results, function(x){
    # extract summary of fit
    naive.slope <- coef(x$naive.fit)[2]
    naive.slope.se <- sqrt(diag(vcov(x$naive.fit)))[2]
    naive.slope.p  <- summary(x$naive.fit)$coefficients[ rownames(summary(x$naive.fit)$coefficients)=="Year",
                                                         "Pr(>|t|)"]
    autocorrelation   <- x$DW1$r
    autocorrelation.p <- x$DW2$p.value
    
    ar1.slope   <- coefficients(x$ar1.fit)[2]
    ar1.slope.se <- sqrt(diag(summary(x$ar1.fit)$varBeta))[2]
    ar1.slope.p <- summary(x$ar1.fit)$tTable[ rownames(summary(x$ar1.fit)$tTable)=="Year",
                                              "p-value"]
    
    data.frame(naive.slope=naive.slope,
               naive.slope.se    = naive.slope.se,
               naive.slope.p     = naive.slope.p,
               autocorrelation   = autocorrelation,
               autocorrelation.p = autocorrelation.p,
               ar1.slope         = ar1.slope,
               ar1.slope.se      = ar1.slope.se,
               ar1.slope.p       = ar1.slope.p)
    
  })
  my.write(linear.reg.report,file = paste0(trends.dir,"/Trends Results - Linear Regression (",yeartype.years.label,").csv "), row.names = FALSE,
           header = paste0("Trends Results - Linear Regression (fitting for autocorrelation) - ",Stream_Name," (",yeartype.years.label,")"))
  
  detach("package:plyr", unload=TRUE)
  
  }


##
##
##
##
## End of script!! Yayy!! 
