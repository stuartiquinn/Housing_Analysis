# State_HPI_Freddie
Stuart_Quinn  
`r format(Sys.time(), '%d %B, %Y')`  
#About

This tutorial will provide a way to make state by state time-series facets graphs. For our purposes, we will be graphing [Freddie Mac Home-Price Index data][1]. The process can be applied to any state-level time-series data that that you would like without too significant of modification. **It is important to note** that when working with state data, you must verify if territories/districts are included (Washington D.C., Puerto Rico, Guam, Marshall Islands, etc.). For our purposes, we have only included the 50 states. 

*Detailed information about the dataset use and method can be found [here][2]*

##Install Necessary Packages


```r
###################Packages#####################
if(!require(dplyr)){
  install.packages('dplyr') #Subsetting data easily
  library(dplyr)
}

if(!require(data.table)){
  install.packages('data.table')  #Use some subsetting functionality (similar to dplyr)
  library(data.table)
}

if(!require(zoo)){
  install.packages('zoo') #Used for making time-series calculations
  library(zoo)
}

if(!require(ggplot2)){
  install.packages('ggplot2') #For graphing in more advanced than base graphics
  library(ggplot2)
}

if(!require(scales)){
  install.packages('scales') #Creates more flexibile formatting of axis in plot
  library(scales)
}

if(!require(extrafont)){
  install.packages('extrafont') #Gives additional font-faces to select from
  library(extrafont)
}

if(!require(extrafontdb)){
  install.packages('extrafontdb') #Loads the database of fonts
  library(extrafontdb)
}
```

##Select colors you will use for graph

R accepts hex colors and there are packages that can convert RGB to hex. You can call the colors directly in the plot, but I simply find it easier to keep them in a reference list. For colors you can use hex, RGB, common-sense titles that are already built into R ("blue", "red", etc.) or use supplemental packages like RColorBrewer. 

*Below we have created a vector of colors to reference in our plot to avoid writing each hex code each time*


```r
##################Color Palette#################
#Dark blue, dark green, dark red, dark purple, dark orange
d_colors <- c("#003E51", "#004C45", "#990000", "#660066", "#CB6015")
```

##Get dataset downloaded and tidy for import

###Tidy
**The majority of this work is handled in Excel,** with an explanation below. This will be updated in the future with an R fix (likely using dplyr). 

You will need to tidy the data in Excel and save as txt/csv before loading
Steps of cleaning include:

1. **Make length wise** 
+ Pivot wizard, alt+d+p (windows)
+ Multiple consolidated ranges
+ Create the page field
+ Select full data including row/column labels (exclude large headers) Pivot will be generated on new sheet
+ Check only value in pivot table, double-click value, should unfold into long data set
+ You should have three columns titled: Row, Column, Value: rename as cycle, state, hpi respectively

2. **Add new derived columns,** "type" column: state v. national
+ Create new column "type" and use =if(len(*stateabbColumn*) > 2, "National", "State")
*indicates adjacent cell reference column/row*

3. **Create "date" columns,** create three new columns, "month", "year" and "date" out of the "cycle" column from pivot
+ month: =right(*cycleColumn*,2)
+ year: =left(*cycleColumn*,4)
+ date: =date(*yearColumn*, *monthColumn*, "1")



###Format/Import
There are a number of ways to import data, I have used the data.table package for speed. You can import nearly any type of file, but flat .csv and .txt will give you the least headaches.


```r
#Load data from working directory once saved as csv or text file
d <- fread("hpi-data-fre-03-2016.txt")
d <- data.table(d) #if you want to use data.table for speed, I prefer dplyr syntax
d$date <- as.Date(d$date, format = "%m/%d/%Y") #convert date and verify change, this can be wonky with excel files
```

###Get data for plots

Using the dplyr package we will gather hpi measures by state from 2000 onward and filter out the national hpi measures and D.C. 


```r
#############################Data Subsetting for Analysis##########

#Using dplyr get all 50 states, excluding national/dc, with hpi, date, state
d_states <- d%>%
  select(hpi, date, state, type)%>% #select columns
  filter(date > "1999-12-01" & type == "State" & state != "DC")%>% #filter rows based on criteria
  select(-type) #drop unnecessary column
# length(unique(d_states$state)) #verify length is 50 
# print(unique(d_states$state)) #Verif you have removed all the proper states
```

##Plotting Data
###Plot Parameters that will be referenced

In the most simplistic of terms, think of your plot as if you were drawing a portrait of someone. You will need a canvas, a subject you intend to portray and a set of tools/colors to produce the result. Generally, you start with your subject or idea (the data or metric of interest in this case), which drives the size of canvas you will need to depict the subject most effectively (50 frames within a canvas). This step can be viewed as the staging of your canvas. In order to capture the dimensions of our subject (hpi, for 50 states, from 2000 to March-2016), we must design our canvas in a way that will contain all of that data. Our toolkit/colors is R and the libraries we installed. 

**Important parameters for graphing**

1. Max/Min of x (dates) and y-axis (hpi value) *full dataset - all plots*
In our analogy: the length (x-axis) and height (y-value) of the entire plot. Fortunately, we do not have to measure the individual frames within the canvas, since ggplot will do that for us


```r
############################Plotting Parameters##################

#Creates a variable for the first date and last date of our dataset for setting x-axis
#Note if your state data comes in at different times, this will not work
first_date <- head(as.Date(d_states$date),1)
last_date <- tail(as.Date(d_states$date),1)

#Creates a variable for roughly highest/lowest hpi values for y-axis
## We will create 50 value breaks, so rounded and we want a cushion for our axis

#Way to get roughly the value to be placed in, round as appropriate
# max(d_states$hpi)
# min(d_states$hpi)
max_hpi <- 275
min_hpi <- 50
```

2.  Max/Min/Most Recent reported period *by each grouped state -- indv. facets*
 + Max Value of HPI by State, likely near 2006/7
 + Min Value of HPI by State, *we filter here for after 2007-03-01, which is the national HPI peak.* This is to capture min from the most recent cycle
 + Most Recent HPI date reported by State, can be = to max value in some instances
 
These are the features of our subject (HPI) that we want to accentuate in our drawing to draw attention

```r
#Capture maximum hpi value by state to plot and include a field that we can 
#Use as an annotation

d_stateMax <- d_states%>% 
  select(date, hpi, state)%>%
  group_by(state)%>% #group all data by state
  mutate(max_label = paste(date,": max", round(hpi,0)))%>% #create lable that will read yyyy-mm-d:max_hpi level
  slice(which.max(hpi))%>% #find max for each state
  as.data.frame() #create as data.frame -- should do automatically

#Create minimum value annotation, post national peak date or else it will give us first point
d_stateMin <- d_states %>%
  select(date, hpi, state)%>%
  filter(date > "2007-03-01")%>%
  group_by(state)%>%
  mutate(min_label = paste(date, ": min", round(hpi, 0)))%>%
  slice(which.min(hpi))%>%
  as.data.frame()

#Get most recent observation period from dataset
d_stateLast <- d_states %>%
  select(date, hpi, state)%>%
  group_by(state)%>%
  arrange(date)%>%
  slice(n())%>% #slice(c(1,n())) gets both first and last observation
  ungroup()%>%
  mutate(recent_label = paste(date, ": recent_per", round(hpi, 0)))
```

3. Merge all data sets from previous two steps
 + Merge together one at a time by state

*TBD -- will update with [FIPS][3], rather than state name for future examples. This would simply allow for more options of merging and potentially better accuracy (match numeric v. alpha)*


```r
#Merge State minimum HPI values and Sttate maximum HPI values (inner join)
d_labelsAll <- merge(d_stateMin, d_stateMax, by = "state")
#Merge the dataset just combined with the most recent period
d_labelsAll <- merge(d_labelsAll, d_stateLast, by = "state")
#Remove duplicates from merging like dataset 
d_labelsAll <- d_labelsAll%>%
  select(state, min_label, max_label, recent_label)
```

##Building Actual Plot

This plot is relatively complex, so if you are unfamiliar with Hadley Wickham's ggplot2 package I would recommend searching through some [introductory material][4] online to familiarize yourself with what is happening below. Once you have worked with the package a few times, you will find it is quite intuitive (it is an R user favorite so there is a wealth of tutorials and assistance available). A full explanation is beyond the scope of this post, but here are some basics about how we are building the plot. 

Overall, I have ordered the plot into sections as follows:
1. Underlying time-series line-chart faceted by each state (including axis parameters for all plots) 
2. Colored plot points on max, min, recent
3. Labeling -- this is the underlying reference data from section (ii)
4. Theme of the plot, think of this as the "format" section of charts in excel (font-face, grids, title size, etc.)

In ggplot2 terminology: 
1. **Data:** We will use the subset data set of hpi values (d_states), our max/min parameters for the *entire* plot (see Important parameters, 1 above), the combined max/min/most recent for *each* state (see Important parameters, 3 above)
2. **Geoms:** We will use TBD for the time-series plot, HOLD for the max/min/recent of each state and HOLD for labeling. 
3. **Facets:** Facet wrap will make the plots by state and we assign 10 columns (therefore, 5 rows for 50 states)


```r
##########################Plotting#########

#i Base time-series line-plot -- think: canvas size, axis labels and number of frames (rows x columns)

p0 <- ggplot(data = d_states, aes(x = date, y = hpi))+ #Data input for graph, x axis is date, y is hpi
  geom_line(color = "black")+ #Create a line of that data across those coordinates
  scale_x_date(labels = date_format("%b-%y"), #x-axis reads as mon-Y (e.g. Jan-00)
               date_breaks = "4 year",        #label will be displayed every four years
               limits = as.Date(c(first_date, last_date)))+ #limited to max and min dates in dataset
  scale_y_log10(limits = c(min_hpi, max_hpi), # y-axis is converted to log scale and limited to max/min HPI value of all states
                breaks = seq(min_hpi, max_hpi, 50))+ #y-axis will create labels every 50 points on HPI
  facet_wrap(~state, ncol = 10)+ #Makes 50 of the plots, by state, with 10 columns and 5 rows

#ii  Colored points for max/min/recent
  #Creates one dot on the most recent, max and min by EACH state, where color is referenced from our list
  geom_point(data = d_stateLast, aes(x = date, y = hpi), size = 4, color = d_colors[3], alpha = 0.75)+
  geom_point(data = d_stateMax, aes(x = date, y = hpi), size = 3, color = d_colors[5], alpha = 0.75)+
  geom_point(data = d_stateMin, aes(x = date, y = hpi), size = 3, color = d_colors[1], alpha = 0.75)+
  #Creates a horizontal dotted dash line for the most recent value for easier reading
  geom_hline(data = d_stateLast, aes(yintercept = hpi), linetype = 2, color = d_colors[3])+

#iii Labeling the points above (ad-hoc legend)
  geom_text(data = d_labelsAll, aes(x = as.Date("2004-02-01"), y = 75, label = max_label), size = 3, family = "Arial Narrow")+
  geom_text(data = d_labelsAll, aes(x = as.Date("2004-01-01"), y = 65, label = min_label), size = 3, family = "Arial Narrow")+
  geom_text(data = d_labelsAll, aes(x = as.Date("2005-06-01"), y = 55, label = recent_label), size = 3, family = "Arial Narrow")+
  geom_point(aes(x= as.Date("2011-11-01"), y = 75), color = d_colors[5], size = 3)+
  geom_point(aes(x = as.Date("2011-11-01"), y = 65), color = d_colors[1], size = 3)+
  geom_point(aes(x = as.Date("2011-11-01"), y = 55), color = d_colors[3], size = 4)+

#iv Theme of the plot -- title of axes and top of plot
  labs(x = "",
       y = "HPI by State, (log scale, Dec-2001 = 100)",
       title = "Freddie Mac State Home Price Index")+
  theme_minimal()+ #built in theme
  #Setting the font-familiy and size of all of the labels of the plot
  theme(
    axis.text.x = element_text(size = 13, family = "Arial Rounded MT Bold", angle = -45), 
    axis.text.y = element_text(size = 13, family="Arial Rounded MT Bold"), 
    axis.title.y = element_text(size = 11, family = "Arial Rounded MT Bold"),
    strip.text = element_text(size = 13, family = "Arial Rounded MT Bold"),
    title = element_text(size = 17, family = "Arial Rounded MT Bold")
  )
p0
```

<img src="FRE_State_HPI_Facet_files/figure-html/plot-1.png" title="" alt="" style="display: block; margin: auto;" />

The annotations are still a little crowded, so we may want to break up the facet into 25 or better viewing.

[1]: http://www.freddiemac.com/finance/house_price_index.html
[2]: http://www.freddiemac.com/finance/fmhpi/docs/FMHPI.pdf
[3]: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
[4]: https://opr.princeton.edu/workshops/Downloads/2015Jan_ggplot2Koffman.pdf
