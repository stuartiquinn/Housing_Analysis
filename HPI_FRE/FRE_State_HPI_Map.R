
if(!require(dplyr)){
  install.packages('dplyr')
  library(dplyr)
}

if(!require(data.table)){
  install.packages('data.table')
  library(data.table)
}

if(!require(zoo)){
  install.packages('zoo')
  library(zoo)
}

if(!require(ggplot2)){
  install.packages('ggplot2')
  library(ggplot2)
}

if(!require(scales)){
  install.packages('scales')
  library(scales)
}

if(!require(extrafont)){
  install.packages('extrafont')
  library(extrafont)
}

if(!require(extrafontdb)){
  install.packages('extrafontdb')
  library(extrafontdb)
}

if(!require(leaflet)){
  install.packages('leaflet')
  library(leaflet)
}

if(!require(RColorBrewer)){
  install.packages('RColorBrewer')
  library(RColorBrewer)
}

if(!require(tigris)){
  install.packages('tigris')
  library(tigris)
}

if(!require(stringr)){
  install.packages('stringr')
  library(stringr)
}

if(!require(htmltools)){
  install.packages('htmltools')
  library(htmltools)
}

if(!require(htmlwidgets)){
  install.packages('htmlwidgets')
  library(htmlwidgets)
}
##################Color Palette#################
#Dark blue, dark green, dark red, dark purple, dark orange
d_colors <- c("#003E51", "#004C45", "#990000", "#660066", "#CB6015")


#######################Data Clean + Load ###############################

# url <- 'http://www.freddiemac.com/finance/fmhpi/current/excel/states.xls'
# You will need to tidy the data in Excel and save as txt/csv before loading
# Steps of cleaning include: 1) make length wise; 2) add "type" column: state v. national
# 3) create "date" column

# 1) pivot wizard, alt+d+p (windows), multiple consolidated ranges, 
# # I will create the page field, select full data including row/column titles
# # check only value in pivot table, select value, should unfold into long dataset
# # You should have three columns titled: Row, Column, Value: rename as cycle, state, hpi respectively

### ***Will use ColumnRow at end to indicate an adjacent cell reference in Excel***

# 2) Create new column "type" and use =if(len(stateabbColumn) > 2, "National", "State")

# 3) Create three new columns, "month", "year" and "date" out of the "cycle" column from pivot
# # month: =right(cycleColumn,2)   year: =left(cycleColumn,4)  date: =date(yearColumn, monthColumn, "1")

#Load data from working directory once saved as csv or text file
d <- fread("hpi-data-fre-03-2016.txt")
d <- data.table(d) #if you want to use data.table for speed, I prefer dplyr syntax
d$date <- as.Date(d$date, format = "%m/%d/%Y") #convert date and verify change, this can be wonky with excel files


#############################Data Subsetting for Analysis##########

#Using dplyr get all 50 states, excluding national/dc, with hpy, date, state
head(d)
d_states <- d%>%
  dplyr::select(hpi, date, state, type)%>%
  filter(date > "1999-12-01" & type == "State")%>%
  dplyr::select(-type)
length(unique(d_states$state))
print(unique(d_states$state))

#Year over year
d_states <- d_states %>%
  group_by(state) %>%
  mutate(hpa12 = ((1+diff(hpi,12)/hpi)^1)-1)


############################Plotting Parameters##################


#Capture maximum hpi value by state to plot and include a field that we can 
#Use as an annotation

d_stateMax <- d_states%>%
  dplyr::select(date, hpi, state)%>%
  group_by(state)%>% #group all data by state
  mutate(max_label = paste(date,": max", round(hpi,0)))%>% #create lable that will read yyyy-mm-d:max_hpi level
  slice(which.max(hpi))%>% #find max for each state
  rename(hpi_max = hpi, date_max = date)%>%
  as.data.frame() #create as data.frame -- should do automatically

#Create minimum value annotation, post national peak date or else it will give us first point
d_stateMin <- d_states %>%
  dplyr::select(date, hpi, state)%>%
  filter(date > "2007-03-01")%>%
  group_by(state)%>%
  mutate(min_label = paste(date, ": min", round(hpi, 0)))%>%
  slice(which.min(hpi))%>%
  rename(hpi_min = hpi, date_min = date) %>%
  as.data.frame()

#Get most recent observation period from dataset
d_stateLast <- d_states %>%
  dplyr::select(date, hpi, state, hpa12)%>%
  group_by(state)%>%
  arrange(date)%>%
  slice(n())%>% #slice(c(1,n())) gets both first and last observation
  ungroup()%>%
  mutate(recent_label = paste(date, ": recent_per", round(hpi, 0)))%>%
  rename(hpi_recent = hpi, hpa12_recent = hpa12, date_recent = date)

#Combine all columns for mapping labels and re-title headers

d_varsAll <- merge(d_stateMin, d_stateMax, by = "state")
d_varsAll <- merge(d_varsAll, d_stateLast, by = "state")
head(d_varsAll)
d_varsAll <- d_varsAll%>%
  dplyr::select(state, date_min, hpi_min, date_max, hpi_max, date_recent, hpi_recent, hpa12_recent)

#Spatial file merge for map

#Our HPI list does not come with FIPS, which are a great joining tool with Census date
#Get fips and merge with most recent HPI
# d_fips <- state.fips #load fips data in maps package, includes dupes and some msas, only lower 48
# head(fips)
# fips$state <- str_replace(fips$polyname, ":.*","") #remove the msa names
# fips <- fips%>% #get rid of duplicate state listings
#   select(fips, abb, state)%>%
#   distinct(abb, .keep_all = T)
# 
# upper2 <- data.frame(fips = c(2, 15), #add back in Hawaii and alaska 
#                      abb = c("AK", "HI"),
#                      state = c("alaska", "hawaii"))
# 
# fips <- rbind(fips,upper2)
# 
# fips$fips <- str_pad(fips$fips, 2, pad = "0")

#Use tigris package to download state boundary file from census, defaults to 2014
d_statePoly <- states(detailed = F)
head(d_statePoly@data) #We will use STUSPS for match key since we do not have Fips
tail(statePoly@data$GEOID)
head(d_stateLast)

#Join our HPI data to poly file, inner join so we drop Guam, Puerto Rico, etc.
#that are contained in the full U.S. Census file

mapData <- geo_join(d_statePoly, d_varsAll, by_sp = "STUSPS", by_df="state", how = "inner")
length(mapData@data$STUSPS) #Verify we have 50 states + D.C. 

##############################Mappings the Data##########################

#First set your colors with colorQuantile from the leaflet package and RColorBrewer
# display.brewer.all()
colRamp <- colorNumeric(palette = "YlOrRd", domain = mapData@data$hpi_recent)

#Make pop-up for when hovering

popup <- paste("<b>HPI Max:</b>", d_varsAll$date_max, ": ", round(d_varsAll$hpi_max,0), "<br>",
              "<b>HPI Min:</b>", d_varsAll$date_min, ": ", round(d_varsAll$hpi_min,0), "<br>",
              "<b>HPI Most Recent:</b>", d_varsAll$date_recent, ": ", round(d_varsAll$hpi_recent,0), "<br>",
              "<b>HPI Year-over-Year:</b>", round((d_varsAll$hpa12_recent*100),2),"%","<br>")



# Resources for provider tile styles and setting bounding box view of map

# http://leaflet-extras.github.io/leaflet-providers/preview/index.html
# http://boundingbox.klokantech.com/

m <- leaflet()%>%
  addProviderTiles("CartoDB.Positron")%>%
  fitBounds(-129.3,24.3,-57.0,50.0)%>%
  addPolygons(data = mapData,
              fillColor = ~colRamp(mapData@data$hpi_recent),
              fillOpacity = 0.7, 
              weight = 0.2,
              popup = popup)%>%
  addLegend(pal = colRamp,
            values = mapData@data$hpi_recent,
            position = "bottomright",
            title = paste("Freddie Mac HPI", "<br>", "(log scale, Dec-2011 = 100)"))

m

saveWidget(m, file = "fre_hpi_map.html")


