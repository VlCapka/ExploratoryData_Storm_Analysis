---
title: "Analysis of economic and population health impact of storm weather events in the U.S."
author: "Vladimir Capka"
date: "2021-09-01"
output: 
  html_document: 
    keep_md: yes
    toc: TRUE
editor_options: 
  chunk_output_type: console
---



## Synopsis

Storms and severe weather can have significant impact on population health in terms of injuries and fatalities and can cause severe economic consequences based not only on property damage but also sector-specific parameters such as crop damage in agriculture and food supply. A large data set from U.S. National Oceanic and Atmospheric Administration ([NOAA](https://www.ncdc.noaa.gov/)) with close to 1 million observations describing weather events from 1950 through 2011 was used to evaluate which weather event types are most harmful to population health and which have the greatest economic impact. Number of fatalities and injuries were used to evaluate impact on population health whereas cost of property and crop damage were used to quantify economic impact. To determine which weather events had the greatest impact in these categories, a cumulative approach across the years was used. Large data set was reduced based on consistency of data logging categories during the the years recorded as well as on exploratory data analysis investigating the significance of contribution of various weather events to grand totals. Based on analysis performed, weather events causing the most fatalities and injuries include excessive heat, tornado and flood. Analysis also shows that the extent of economic cost of weather events was \~10-fold greater in property damage than in crop damage. Weather events causing most property damage were flood, hurricane and other storm-related events whereas, in contrast lack of water, drought, caused the most crop damage.

## Data processing

Loading required packages and data from the source.


```r
library(tidyverse)
library(lubridate)
library(RColorBrewer)
```


```r
if(!file.exists("storm_data.bz2")) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "storm_data.bz2")
}
storms <- read.csv("storm_data.bz2")
storms$EVTYPE <- tolower(storms$EVTYPE)
storms$BGN_DATE <- mdy_hms(storms$BGN_DATE)
```

<br>

#### Understanding the data

The data set comes from the US NOAA's National Weather Service (NWS) and contains weather data from January 1950 to November 2011. Due to evolution of data collecting and recording procedures over the years, there are unique periods in this time frame where different weather events were recorded. There are 48 official weather types per NOAA [NWS Instruction 10-1605](https://www.nws.noaa.gov/directives/sym/pd01016005curr.pdf). However, as indicated by NOAA [Storm Events Database](https://www.ncdc.noaa.gov/stormevents/details.jsp?type=eventtype), there are time periods when not all weather types were recorded. Up until 1955, only weather event tracked was tornado whereas from 1955 through 1996, only tornado, thunderstorm, wind and hail were recorded. Full weather events tracking including all 43 weather types started in 1996. For the purpose of this analysis where we want to evaluate weather event types causing highest economic impact and most harm to the population health, only the time period with the complete weather events tracking will be used. There are 15 years of complete weather event data, which should be a good approximation of what weather types have the most impact.

#### Data analysis approach

The data set contains close to a million observations. To answer key questions in this report, a data reduction approach is used. First, the data is filtered to only include time periods with complete weather event types tracking. Only variables needed for the analysis are kept during the data processing and only observations that resulted in population health or economic impact are kept. Data is aggregated per event type and summed over the time period to arrive at cumulative impact of various event types.\
Exploratory data analysis is performed to evaluate scale of contribution of various weather event types to the totals. The exploratory data analysis code is included, exploratory graphs are saved but not shown. This analysis revealed that vast majority of weather events have negligible impact compared to those with large cumulative impact over the years. After removing insignificant weather events (\<5% of maximum impact), data set contains only a few categories representing top 95% most impactful weather events. These event types are checked for misspellings, miscategorization, redundancy and coerced to logically meaningful categories. Final plots are produced with top three contributors highlighted in darker color.

#### Impact of weather events on population health

For the purpose of this analysis, the data is filtered to include only events starting from 1996 that resulted in fatalities or injuries. After aggregation by weather event type, number of fatalities and injuries are summed to indicate total harm to population health. The data is converted to tidy format for exploratory data analysis.\
Insignificant weather types totaling \<5% of maximum impact event are removed, data re-visualized and weather event types coerced to logically meaningful categories.\
Finally, data is prepared for final visualization.


```r
health <- select(storms, EVTYPE, FATALITIES, INJURIES, BGN_DATE) %>% 
        filter(BGN_DATE >= "1996-01-01", (FATALITIES > 0 | INJURIES > 0)) %>% 
        select(-BGN_DATE) %>% 
        group_by(EVTYPE) %>%
        summarize(across(.cols = everything(), sum)) %>%
        pivot_longer(cols = -EVTYPE, names_to = "Health_Effect", values_to = "Count")

# Exploratory data analysis plot. All events.
health_exploratory <- ggplot(health, aes(Count, reorder(EVTYPE, Count))) +
        geom_col() +
        facet_wrap(~Health_Effect, ncol = 2, nrow = 1, scales = "free_x")

# Remove insignificant events <5% max
totals <- group_by(health, Health_Effect) %>% 
        summarize(totals = max(Count))
h95 <- left_join(health, totals, by = "Health_Effect") %>% 
        filter(Count > 0.05*totals) 

# Exploratory data analysis top 95%.
health_exploratory2 <- ggplot(h95, aes(Count, reorder(EVTYPE, Count))) +
        geom_col() +
        facet_wrap(~Health_Effect, ncol = 2, nrow = 1, scales = "free")

# Clean up event type names
h95$event <- ifelse(h95$EVTYPE == "excessive heat", "heat", h95$EVTYPE)
h95$event <- ifelse(h95$EVTYPE == "flash flood", "flood", h95$event)
h95$event <- ifelse(grepl("wind", h95$EVTYPE), "high wind", h95$event)
h95$event <- ifelse(h95$EVTYPE == "heavy snow", "winter storm", h95$event)
h95$event <- ifelse(h95$EVTYPE == "rip currents", "rip current", h95$event)
h95$event <- ifelse(grepl("cold", h95$EVTYPE), "extreme cold", h95$event)

h95_clean <- group_by(h95, Health_Effect, event) %>% 
        summarize(sum(Count)) %>% 
        rename("Count" = "sum(Count)") %>% 
        ungroup() %>% 
        arrange(Health_Effect, -Count) %>% 
        mutate(order = row_number()) %>% 
        group_by(Health_Effect) %>% 
        mutate(top3 = rank(order) %in% 1:3)
```

<br>

#### Economic impact of weather events

To evaluate economic impact, data is filtered to include only events starting from 1996 that resulted in property or crop damage (and related multiplier variables). Letter-based multiplier variables are converted to numerical multipliers based on work of [David Hood and Eddie Song](https://rstudio-pubs-static.s3.amazonaws.com/58957_37b6723ee52b455990e149edde45e5b6.html). These are then used to calculate correct values for crop and property damage.\
Rest of the analysis is analogous to the above workflow for analysis of health effects (data aggregation, summation, exploratory data analysis, removal of insignificant events, event names coercion, data preparation for visualization).


```r
econ <- select(storms, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP, BGN_DATE) %>% 
        filter(BGN_DATE >= "1996-01-01", (PROPDMG > 0 | CROPDMG > 0)) %>% 
        select(-BGN_DATE)

econ$PROPDMGEXP <- ifelse(econ$PROPDMGEXP == "K", 1e3, econ$PROPDMGEXP)
econ$PROPDMGEXP <- ifelse(econ$PROPDMGEXP == "M", 1e6, econ$PROPDMGEXP)
econ$PROPDMGEXP <- ifelse(econ$PROPDMGEXP == "B", 1e9, econ$PROPDMGEXP)
econ$CROPDMGEXP <- ifelse(econ$CROPDMGEXP == "K", 1e3, econ$CROPDMGEXP)
econ$CROPDMGEXP <- ifelse(econ$CROPDMGEXP == "M", 1e6, econ$CROPDMGEXP)
econ$CROPDMGEXP <- ifelse(econ$CROPDMGEXP == "B", 1e9, econ$CROPDMGEXP)
econ$CROPDMGEXP <- as.numeric(econ$CROPDMGEXP)
econ$PROPDMGEXP <- as.numeric(econ$PROPDMGEXP)
econ$CROPDMGEXP <- ifelse(is.na(econ$CROPDMGEXP), 1, econ$CROPDMGEXP)
econ$PROPDMGEXP <- ifelse(is.na(econ$PROPDMGEXP), 1, econ$PROPDMGEXP)

econ <- mutate(econ, PROPDMG = PROPDMG*PROPDMGEXP, CROPDMG = CROPDMG*CROPDMGEXP) %>% 
    select(-c(PROPDMGEXP, CROPDMGEXP)) %>% 
    group_by(EVTYPE) %>% 
    summarize(across(.cols = everything(), sum)) %>% 
    pivot_longer(-EVTYPE, names_to = "Damage_Type", values_to = "Value")

# Exploratory data anlysis, all events.
econ_exploratory <- ggplot(econ, aes(Value, reorder(EVTYPE, Value))) +
    geom_col() +
    facet_wrap(~Damage_Type, ncol=2, scales = "free_x")

# Remove insignificant events <5% max.
econ_max <- group_by(econ, Damage_Type) %>% 
        summarize(emax = max(Value))
e95 <- left_join(econ, econ_max, by = "Damage_Type") %>% 
        filter(Value > 0.05*emax) 

# Exploratrory data analysis, top 95%.
econ_exploratory2 <- ggplot(e95, aes(Value, reorder(EVTYPE, Value))) +
    geom_col() +
    facet_wrap(~Damage_Type, ncol = 2, scales = "free")

# Clean up event type names
e95$event <- ifelse(e95$EVTYPE == "flash flood", "flood", e95$EVTYPE)
e95$event <- ifelse(e95$EVTYPE %in% c("hurricane/typhoon", "tropical storm", "hurricane"), "hurricane/\n /tropical storm", e95$event)
e95$event <- ifelse(e95$EVTYPE %in% c("extreme cold", "frost/freeze"), "frost/freeze/\n /extreme cold", e95$event)

e95_clean <- group_by(e95, Damage_Type, event) %>%
    summarize(Value = sum(Value)) %>% 
    ungroup() %>% 
    arrange(-Value) %>% 
    mutate(order = row_number()) %>% 
    group_by(Damage_Type) %>% 
    mutate(top3 = rank(order) %in% 1:3)
```

<br>

#### Alternate data analysis options

All code for data a analysis is included and readers can easily modify key parameters related to the analysis. For instance, displaying exploratory data analysis graphs or modifying significance value thresholds for inclusion of weather events in final visualizations.\
I believe the data analysis presented in this report answers key questions of identifying most significant weather event types that have the greatest impact on population health or economy. Nevertheless, more insights into weather patterns and their impact can be gained by employing alternative approaches such as, aggregating weather event types by year, quantifying health and economic impacts on an average yearly basis per event type, and then plotting trends over time. I thought such analysis would be beyond the scope of this report but nevertheless would be interesting to investigate.

## Results

Figure 1 shows cumulative weather impact on top 95% of most harmful weather events on population health in the U.S. as measured by total number of fatalities and injuries from 1996 to 2011. Top three most hamful events are highlighted and include heat, tornado, and flood. Interestingy, most deadly weather event appears to be heat-realated whereas, tornado appears to cause most injuries.


```r
ggplot(h95_clean, aes(-order, Count)) +
        geom_col(aes(fill = top3)) +
        facet_wrap(~Health_Effect, ncol = 2, nrow = 1, scales = "free")+
        coord_flip() +
        scale_x_continuous(breaks = -h95_clean$order,
                           labels = h95_clean$event,
                            expand = c(0.01,0)) +
        labs(x = element_blank(),
             y = "Incidence (count)",
             title = "Impact of weather events on population health in the US",
             subtitle = "Recorded between 1/1/1996 and 11/30/2011 by NOAA\n",
             caption = "\nFIGURE 1. Total number of fatalities and injuries caused by top 95% of most harmful weather events\nin the US from 1996 to 2011. Top three most hamful are highlighted. \nSource: NOAA") +
        theme_bw() +
        theme(legend.position = "none",
              panel.grid.major.y = element_blank(), 
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(), 
              text = element_text(size = 11),
              plot.caption = element_text(size = 12, hjust = 0),
              plot.title = element_text(hjust = 0.5, size = 15),
              plot.subtitle = element_text(hjust = 0.5, size = 10)) +
        scale_fill_brewer(palette = "Paired")
```

<img src="Storm_analysis_files/figure-html/results_health-1.png" style="display: block; margin: auto;" />

<br> Figure 2 shows cumulative economic impact of top 95% of most significant weather event types based on total property and crop damage value in billions of \$US throughout the U.S. from 1996 to 2011. Top three most impactful weather events are highlighted. The data indicates that the extent of damage on property values is \~10-fold higher compared to crop damage. Interestingly, most property damage was caused by water-related weather events such as flood and hurricanes whereas the greatest crop damage is associated with the "lack-of-water" weather event, drought.


```r
labels <- c("CROPDMG" = "CROP DAMAGE", "PROPDMG" = "PROPERTY DAMAGE")
ggplot(e95_clean, aes(-order, Value/1e9)) +
        geom_col(aes(fill = top3)) +
        facet_wrap(~Damage_Type, ncol = 2, scales = "free", labeller = as_labeller(labels)) +
        coord_flip() +
        scale_x_continuous(breaks = -e95_clean$order,
                           labels = e95_clean$event,
                            expand = c(0.01,0)) +
        scale_y_continuous(labels = scales::dollar_format(suffix = "B")) +
        labs(x = element_blank(),
             y = "Damage value (billions of $US)",
             title = "Economic impact of weather events across the US",
             subtitle = "Recorded between 1/1/1996 and 11/30/2011 by NOAA\n",
             caption = "\nFIGURE 2. Cumulative property and crop damage caused by top 95% of most impactful weather events \nin the US from 1996 to 2011. Top three most impactful are highlighted. \nSource: NOAA") +
        theme_bw() +
        theme(legend.position = "none",
              panel.grid.major.y = element_blank(), 
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(), 
              text = element_text(size = 11),
              plot.caption = element_text(size = 12, hjust = 0),
              plot.title = element_text(hjust = 0.5, size = 15),
              plot.subtitle = element_text(hjust = 0.5, size = 10)) +
        scale_fill_brewer(palette = "Paired")
```

<img src="Storm_analysis_files/figure-html/results_economy-1.png" style="display: block; margin: auto;" />

## Appendix

#### Key learning themes

-   R markdown, results caching, publishing
-   Data reduction
-   Exploratory data analysis
-   Data wrangling
-   `ggplot2` visualization
    -   Facet relabeling
    -   Data re-ordering
    -   Color options
    -   Theme options
    -   Plot annotations
    -   Scale dollar format

#### Session information


```r
sessionInfo()
```

```
## R version 4.1.0 (2021-05-18)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19042)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] RColorBrewer_1.1-2 lubridate_1.7.10   forcats_0.5.1      stringr_1.4.0     
##  [5] dplyr_1.0.7        purrr_0.3.4        readr_1.4.0        tidyr_1.1.3       
##  [9] tibble_3.1.2       ggplot2_3.3.4      tidyverse_1.3.1   
## 
## loaded via a namespace (and not attached):
##  [1] tidyselect_1.1.1  xfun_0.24         haven_2.4.1       colorspace_2.0-1 
##  [5] vctrs_0.3.8       generics_0.1.0    htmltools_0.5.1.1 yaml_2.2.1       
##  [9] utf8_1.2.1        rlang_0.4.11      pillar_1.6.1      glue_1.4.2       
## [13] withr_2.4.2       DBI_1.1.1         dbplyr_2.1.1      modelr_0.1.8     
## [17] readxl_1.3.1      lifecycle_1.0.0   munsell_0.5.0     gtable_0.3.0     
## [21] cellranger_1.1.0  rvest_1.0.0       evaluate_0.14     labeling_0.4.2   
## [25] knitr_1.33        fansi_0.5.0       highr_0.9         broom_0.7.7      
## [29] Rcpp_1.0.7        scales_1.1.1      backports_1.2.1   jsonlite_1.7.2   
## [33] farver_2.1.0      fs_1.5.0          hms_1.1.0         digest_0.6.27    
## [37] stringi_1.6.2     grid_4.1.0        cli_2.5.0         tools_4.1.0      
## [41] magrittr_2.0.1    crayon_1.4.1      pkgconfig_2.0.3   ellipsis_0.3.2   
## [45] xml2_1.3.2        reprex_2.0.0      assertthat_0.2.1  rmarkdown_2.9    
## [49] httr_1.4.2        rstudioapi_0.13   R6_2.5.0          compiler_4.1.0
```
