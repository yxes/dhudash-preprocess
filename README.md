# jhudash-leadteam

# Project Efforts

Our progress has been rather circuitous, but in the end we settled on a few things. 

One dataset we're using is [this CDC set](ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/DATA2010/State_data_tables/) (documentation available [here](http://wonder.cdc.gov/DATA2010/FTPSELEC.HTM)). 

## tl;dr

In brief, we are working on producing: 
- an R package to tidy the excel data into an easy-to-use format
- lead exposure and crime data, from the CDC data inventory and other sources
- a Shiny app to easily visualize 


## Data Description

The ftp site linked to above has data available for all fifty states. However, 
the formatting is difficult to use, due to several issues (e.g., differences in 
alignment, etc). And each file has data for virtually all fifty states, on 
along with many different variables concerning demographics.

Since each sheet contains statistics for a different health conditions, 
then overall there's plenty of data available for which a subset could be 
extracted and combined with, or compared to, other health statistics.










