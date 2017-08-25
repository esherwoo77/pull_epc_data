# Get EPCHC Data and Tidy it Up
Ed Sherwood <esherwood@epchc.org>  
August 17, 2017  



## Download Environmental Protection Commission of Hillsborough County (EPCHC)
## Long-Term Water Quality Monitoring Dataset (1972-Present)

The EPCHC has been monitoring water quality in Tampa Bay for over 40 years. The latest Surface Water Quality Monitoring report was published in November 2014 and includes monitoring information from 2001-2010. You can view the report under the "Publications" link on EPCHC's main webpage here: <http://www.epchc.org/>.  For more details about particular monitoring stations see: <http://http://www.epchc.org/index.aspx?NID=219>.


```r
epchc_url <- "ftp://ftp.epchc.org/EPC_ERM_FTP/WQM_Reports/RWMDataSpreadsheet_ThroughCurrentReportMonth.xlsx"
#download.file(url = epchc_url, destfile = './data-raw/epchc.xlsx', method = "libcurl", mode = "wb")
```

## Correctly import xlsx file into R

Import formatted columns correctly into R and then assign them standardized names.


```r
epcnames <- readLines("./data-raw/epchc_column_names.csv")
epcsites <- c(6, 7, 8, 44, 52, 55, 70, 71, 73, 80, 36, 38, 40, 41, 46, 47, 50, 51, 60, 63, 64, 65, 66, 67, 68, 9,               11, 81, 84, 13, 14, 32, 33, 16, 19, 28, 82, 23, 24, 25, 90, 91, 92, 93, 95)
otb_stations <- c(36, 38, 40, 41, 46, 47, 50, 51, 60, 63, 64, 65, 66, 67, 68)
hb_stations <- c(6, 7, 8, 44, 52, 55, 70, 71, 73, 80)
mtb_stations <- c(9, 11, 81, 84, 13, 14, 32, 33, 16, 19, 28, 82)
ltb_stations <- c(23, 24, 25, 90, 91, 92, 93, 95)
bay_segments = c("OTB", "HB", "MTB", "LTB")
targets <- data.frame(bay_segment = c("OTB", "HB", "MTB", "LTB"),
                           name = c("Old Tampa Bay", "Hillsborough Bay", "Middle Tampa Bay", "Lower Tampa Bay"),
                           chla_target = c(8.5, 13.2, 7.4, 4.6),
                           chla_smallex = c(8.9, 14.1, 7.9, 4.8),
                           chla_thresh = c(9.3, 15.0, 8.5, 5.1),
                           la_target = c(0.83, 1.58, 0.83, 0.63),
                           la_smallex = c(0.86, 1.63, 0.87, 0.66),
                           la_thresh = c(0.88, 1.67, 0.91, 0.68))
epcdata <- read_xlsx("./data-raw/epchc.xlsx", 
                     sheet="RWMDataSpreadsheet", 
                     col_types = c("numeric", "numeric", "text", "text", "text", "text", "numeric", "numeric",                                       "text", "numeric", "numeric", "text", "date", "text", "numeric", "text",                                          "text", "numeric", "numeric", "numeric", "numeric", "text", "text", "text",                                       "numeric", "text", "numeric", "text", "numeric", "text", "numeric", "text",                                       "numeric", "text", "numeric", "text", "numeric", "text", "numeric", "text",                                       "numeric", "text", "numeric", "text", "numeric", "text", "numeric", "text",                                       "numeric", "text", "numeric", "text", "numeric", "text", "numeric", "text",                                       "numeric", "text", "numeric", "text", "numeric", "text", "numeric", "text",                                       "numeric", "text", "text", "text", "text", "text", "text", "text", "text",                                        "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",                                    "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",                                    "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",                                    "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",                                    "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",                                    "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",                                    "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",                                    "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",                                    "text", "text", "text", "text", "text", "text", "text", "text", "text"),
                     col_names = epcnames, 
                     skip=1, na="")
```

## Run some sanity checks on the data
Filter entire EPCHC dataset and plot standard sites used in TBEP bay segment analyses.

```r
#Filter entire EPCHC dataset
wqdata <- epcdata %>%
          select(StationNumber,
                 Latitude,
                 Longitude,
                 SampleTime,
                 Total_Depth_m,
                 Sample_Depth_m,
                 Secchi_Depth_m,
                 Secchi_Q,
                 Chlorophyll_a_uncorr_ugL,
                 Chlorophyll_a_uncorr_Q) %>% 
          mutate(epchc_station = as.numeric(StationNumber),
                 sd_m = as.numeric(Secchi_Depth_m),
                 sd_check = as.numeric((Total_Depth_m*3.2809)-(Secchi_Depth_m*3.2809)),
                 chla = as.numeric(Chlorophyll_a_uncorr_ugL),
                 yr = year(SampleTime),
                 mo = month(SampleTime)) %>% 
          filter(epchc_station %in% epcsites)
```

```
## Warning in evalq(as.numeric(Chlorophyll_a_uncorr_ugL), <environment>): NAs
## introduced by coercion
```

```r
#Assign NAs to VOB secchi disk depths or secchis <0.5ft from bottom -- Janicki protocols
wqdata <- within(wqdata, sd_m[Secchi_Q == ">"] <- NA)
wqdata <- within(wqdata, sd_m[sd_check < 0.5] <- NA)

wqdata$bay_segment <- ifelse(wqdata$epchc_station %in% hb_stations, "HB",
                             ifelse(wqdata$epchc_station %in% otb_stations, "OTB",
                                    ifelse(wqdata$epchc_station %in% mtb_stations, "MTB",
                                           ifelse(wqdata$epchc_station %in% ltb_stations, "LTB",NA))))


#Display station locations
wqsites <- wqdata %>% 
           select(epchc_station, Latitude, Longitude) %>% 
           unique()

map <- leaflet(wqsites) %>% 
              addProviderTiles(providers$CartoDB.Positron) %>% 
              addCircleMarkers(~Longitude, ~Latitude,
                               radius = 6,
                               color = 'black',
                               stroke = FALSE,
                               opacity = 0.8,
                               popup = ~as.character(paste('EPC Station:', epchc_station)), 
                               group = 'Water quality') %>% 
              addLayersControl(overlayGroups = c('Water quality'),
                               options = layersControlOptions(collapsed = FALSE))
map
```

<!--html_preserve--><div id="htmlwidget-d2f2c55c2db369861d24" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-d2f2c55c2db369861d24">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false}]},{"method":"addCircleMarkers","args":[[27.889299,27.8589,27.8524,27.786501,27.812901,27.8118,27.778,27.723801,27.693399,27.666,27.5884,27.662701,27.708401,27.7932,27.826099,27.855801,27.8818,27.9291,27.937401,27.9237,27.9904,27.972601,27.918501,27.8902,27.896999,27.8493,27.989901,27.9676,27.979401,27.945601,27.927799,27.9002,27.8519,27.908899,27.876499,27.8281,27.809601,27.7813,27.751101,27.729,27.625999,27.627899,27.5737,27.578899,27.6112],[-82.477402,-82.468597,-82.409302,-82.4272,-82.478897,-82.523201,-82.520302,-82.533798,-82.555901,-82.599197,-82.619301,-82.6679,-82.6092,-82.570702,-82.567497,-82.553299,-82.577499,-82.587303,-82.565002,-82.480698,-82.659302,-82.620201,-82.537903,-82.548798,-82.438202,-82.431396,-82.631599,-82.575996,-82.683296,-82.694298,-82.639702,-82.592003,-82.580803,-82.463203,-82.413803,-82.413101,-82.445999,-82.474098,-82.5718,-82.498703,-82.591499,-82.641502,-82.686798,-82.744102,-82.694702],6,null,"Water quality",{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":false,"color":"black","weight":5,"opacity":0.8,"fill":true,"fillColor":"black","fillOpacity":0.2,"dashArray":null},null,null,["EPC Station: 6","EPC Station: 7","EPC Station: 8","EPC Station: 9","EPC Station: 11","EPC Station: 13","EPC Station: 14","EPC Station: 16","EPC Station: 19","EPC Station: 23","EPC Station: 24","EPC Station: 25","EPC Station: 28","EPC Station: 32","EPC Station: 33","EPC Station: 36","EPC Station: 38","EPC Station: 40","EPC Station: 41","EPC Station: 44","EPC Station: 46","EPC Station: 47","EPC Station: 50","EPC Station: 51","EPC Station: 52","EPC Station: 55","EPC Station: 60","EPC Station: 63","EPC Station: 64","EPC Station: 65","EPC Station: 66","EPC Station: 67","EPC Station: 68","EPC Station: 70","EPC Station: 71","EPC Station: 73","EPC Station: 80","EPC Station: 81","EPC Station: 82","EPC Station: 84","EPC Station: 90","EPC Station: 91","EPC Station: 92","EPC Station: 93","EPC Station: 95"],null,null,null,null]},{"method":"addLayersControl","args":[[],"Water quality",{"collapsed":false,"autoZIndex":true,"position":"topright"}]}],"limits":{"lat":[27.5737,27.9904],"lng":[-82.744102,-82.409302]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
#Generate annual averages for each bay segment
tbdata <-subset(wqdata, bay_segment != "MTB")
tbmonchla <- tbdata %>% 
               select(yr, mo, bay_segment, chla) %>%
               drop_na() %>%
               group_by(yr, mo, bay_segment) %>%
               summarise(mean_chla = mean(chla))
tbyrchla <- tbmonchla %>% 
               select(bay_segment, yr, mean_chla) %>% 
               drop_na() %>% 
               group_by(bay_segment, yr) %>% 
               summarise(mean_chla = mean(mean_chla))
```

```
## Adding missing grouping variables: `mo`
```

```r
tbmonsdm <- tbdata %>% 
               select(yr, mo, bay_segment, sd_m) %>%
               drop_na() %>%
               group_by(yr, mo, bay_segment) %>%
               summarise(mean_sdm = mean(sd_m))

tbyrsdm <- tbmonsdm %>% 
               select(bay_segment, yr, mean_sdm) %>% 
               drop_na() %>% 
               group_by(bay_segment, yr) %>% 
               summarise(mean_sdm = mean(mean_sdm))
```

```
## Adding missing grouping variables: `mo`
```

```r
#Process MTB data using weighted averages of 3 subsegments
mtbdata <- subset(wqdata, bay_segment == "MTB")
mtbdata$baysegment <- ifelse(mtbdata$epchc_station %in% c(9, 11, 81, 84), "MT1",
                             ifelse(mtbdata$epchc_station %in% c(13, 14, 32, 33), "MT2",
                                    ifelse(mtbdata$epchc_station %in% c(16, 19, 28, 82), "MT3", NA)))
mtbmonthlychla <- mtbdata %>%
                select(yr, mo, baysegment, chla) %>% 
                drop_na() %>% 
                group_by(yr, mo, baysegment) %>% 
                summarise(mean_chla = mean(chla))
mtbmonchla <- mtbmonthlychla

mtbmonthlysdm <- mtbdata %>%
                select(yr, mo, baysegment, sd_m) %>% 
                drop_na() %>% 
                group_by(yr, mo, baysegment) %>% 
                summarise(mean_sd_m = mean(sd_m))
mtbmonsdm <- mtbmonthlysdm

mtbmonchla$chla <- ifelse(mtbmonchla$baysegment=="MT1", mtbmonchla$mean_chla*2108.7,
                        ifelse(mtbmonchla$baysegment=="MT2", mtbmonchla$mean_chla*1041.9,
                               ifelse(mtbmonchla$baysegment=="MT3", mtbmonchla$mean_chla*974.6, NA)))
mtbmonsdm$sdm <- ifelse(mtbmonsdm$baysegment=="MT1", mtbmonsdm$mean_sd_m*2108.7,
                        ifelse(mtbmonsdm$baysegment=="MT2", mtbmonsdm$mean_sd_m*1041.9,
                               ifelse(mtbmonsdm$baysegment=="MT3", mtbmonsdm$mean_sd_m*974.6, NA)))
mtbmoyrchla <- mtbmonchla %>%
                select(yr, mo, baysegment, chla) %>% 
                drop_na() %>% 
                group_by(yr, mo) %>% 
                summarise(sum_chla = sum(chla)) %>% 
                mutate(mean_chla = sum_chla/4125.2)

mtbyrchla <- mtbmoyrchla %>%
                select(yr, mean_chla) %>% 
                drop_na() %>% 
                group_by(yr) %>% 
                summarise(mean_chla = mean(mean_chla)) %>% 
                mutate(bay_segment = "MTB")
  

mtbmoyrsdm <- mtbmonsdm %>%
                select(yr, mo, baysegment, sdm) %>% 
                drop_na() %>% 
                group_by(yr, mo) %>% 
                summarise(sum_sdm = sum(sdm)) %>% 
                mutate(mean_sdm = sum_sdm/4125.2)

mtbyrsdm <- mtbmoyrsdm %>%
                select(yr, mean_sdm) %>% 
                drop_na() %>% 
                group_by(yr) %>% 
                summarise(mean_sdm = mean(mean_sdm)) %>% 
                mutate(bay_segment = "MTB")

#Put it all together
chladata <- bind_rows(tbyrchla, mtbyrchla)
sdmdata <- bind_rows(tbyrsdm, mtbyrsdm)
sdmdata$mean_la <- ifelse(sdmdata$bay_segment =="OTB", 1.49/sdmdata$mean_sdm,
                             ifelse(sdmdata$bay_segment =="HB", 1.61/sdmdata$mean_sdm,
                                    ifelse(sdmdata$bay_segment =="MTB", 1.49/sdmdata$mean_sdm,
                                           ifelse(sdmdata$bay_segment =="LTB", 1.84/sdmdata$mean_sdm,NA))))
```

## Plot mean annual Chl-a Values by Bay Segment {.tabset}

```r
cols <- c("Annual Mean"="red", "Management Target"="blue", "Regulatory Threshold"="blue", "Small Mag. Exceedance"="blue", "Large Mag. Exceedance"="blue")
for (i in seq_along(bay_segments)) {
   chlaplot <- chladata %>%
                 filter(bay_segment == bay_segments[i] & yr<2017) %>% 
                 ggplot(aes(x=yr)) + 
                   geom_point(aes(y=mean_chla, colour="Annual Mean"), size=3) +
                   geom_line(aes(y=mean_chla, colour="Annual Mean"), size=0.75) +
                   geom_hline(data=targets, aes(yintercept = as.numeric(chla_thresh[i]),
                                                colour="Regulatory Threshold")) +
                   ggtitle(paste(targets$name[i])) +
                   geom_text(data=targets, aes(1973,as.numeric(chla_thresh[i]),
                                 label = paste(chla_thresh[i], " ug/L"), hjust = 0.3, vjust = -0.3)) +
                   ylab("Mean Annual Chlorophyll-a (ug/L)") +
                   xlab("") +
                   scale_x_continuous(breaks=seq(1973,2017,by=1),
                                      labels=c(1973, rep("",3), 1977, rep("",3), 1981, rep("",3),
                                               1985, rep("",3), 1989, rep("",3), 1993, rep("",3),
                                               1997, rep("",3), 2001, rep("",3), 2005, rep("",3),
                                               2009, rep("",3), 2013, rep("",3), 2017),
                                      expand = c(0.035,0)) +
                   theme(plot.title = element_text(hjust = 0.5),
                         panel.grid.minor=element_blank(),
                         panel.grid.major=element_blank(),
                         legend.position = c(0.88, 0.95),
                         legend.background = element_rect(fill=NA)) +
                   scale_colour_manual(name="", values = cols,
                                       labels=c("Annual Mean", "Regulatory Threshold"))
   cat("###", paste(targets$name[i]), "\n")
   print(chlaplot)
   cat("\n\n")
} 
```

### Old Tampa Bay 
![](pull_epc_data_files/figure-html/plot_annual_averages-1.png)<!-- -->

### Hillsborough Bay 
![](pull_epc_data_files/figure-html/plot_annual_averages-2.png)<!-- -->

### Middle Tampa Bay 
![](pull_epc_data_files/figure-html/plot_annual_averages-3.png)<!-- -->

### Lower Tampa Bay 
![](pull_epc_data_files/figure-html/plot_annual_averages-4.png)<!-- -->

## Plot mean annual light attenuation values by Bay Segment

```r
for (i in seq_along(bay_segments)) {
sdmplot <- sdmdata %>%
                 filter(bay_segment == bay_segments[i] & yr<2017) %>% 
                 ggplot(aes(x=yr)) + 
                   geom_point(aes(y=mean_la, colour="Annual Mean"), size=3) +
                   geom_line(aes(y=mean_la, colour="Annual Mean"), size=0.75) +
                   geom_hline(data=targets, aes(yintercept = as.numeric(la_target[i]),
                                                colour="Management Target")) +
                   ggtitle(paste(targets$name[i])) +
                   geom_text(data=targets, aes(1973,as.numeric(la_target[i]),
                                 label = paste(la_target[i], " m-1"), hjust = 0.3, vjust = -0.3)) +
                   ylab("Mean Annual Light Attenuation (m-1)") +
                   xlab("") +
                   scale_x_continuous(breaks=seq(1973,2017,by=1),
                                      labels=c(1973, rep("",3), 1977, rep("",3), 1981, rep("",3),
                                               1985, rep("",3), 1989, rep("",3), 1993, rep("",3),
                                               1997, rep("",3), 2001, rep("",3), 2005, rep("",3),
                                               2009, rep("",3), 2013, rep("",3), 2017),
                                      expand = c(0.035,0)) +
                   theme(plot.title = element_text(hjust = 0.5),
                         panel.grid.minor=element_blank(),
                         panel.grid.major=element_blank(),
                         legend.position = c(0.88, 0.95),
                         legend.background = element_rect(fill=NA)) +
                   scale_colour_manual(name="", values = cols,
                                       labels=c("Annual Mean", "Management Target"))
print(sdmplot)
} 
```

![](pull_epc_data_files/figure-html/unnamed-chunk-1-1.png)<!-- -->![](pull_epc_data_files/figure-html/unnamed-chunk-1-2.png)<!-- -->![](pull_epc_data_files/figure-html/unnamed-chunk-1-3.png)<!-- -->![](pull_epc_data_files/figure-html/unnamed-chunk-1-4.png)<!-- -->


## Export as a Tidy CSV file




