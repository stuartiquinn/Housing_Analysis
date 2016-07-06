
###################Packages#####################
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
d_states <- d%>%
  select(hpi, date, state, type)%>%
  filter(date > "1999-12-01" & type == "State" & state != "DC")%>%
  select(-type)
length(unique(d_states$state))
print(unique(d_states$state))

############################Plotting Parameters##################

#Creates a variable for the first date and last date of our dataset for setting x-axis
first_date <- head(as.Date(d_states$date),1)
last_date <- tail(as.Date(d_states$date),1)

#Creates a variable for roughly highest/lowest hpi values for y-axis
## We will create 50 value breaks, so rounded and we want a cushion for our axis
max_hpi <- 275
min_hpi <- 50

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


d_labelsAll <- merge(d_stateMin, d_stateMax, by = "state")
d_labelsAll <- merge(d_labelsAll, d_stateLast, by = "state")
d_labelsAll <- d_labelsAll%>%
  select(state, min_label, max_label, recent_label)

##########################Plotting#########

#Plot
p0 <- ggplot(data = d_states, aes(x = date, y = hpi))+
  geom_line(color = "black")+
  scale_x_date(labels = date_format("%b-%y"), date_breaks = "4 year",
               limits = as.Date(c(first_date, last_date)))+
  scale_y_log10(limits = c(min_hpi, max_hpi), breaks = seq(min_hpi, max_hpi, 50))+
  facet_wrap(~state, ncol = 10)+
  #Annotations of plot -- dots
  geom_point(data = d_stateLast, aes(x = date, y = hpi), size = 4, color = d_colors[3], alpha = 0.75)+
  geom_point(data = d_stateMax, aes(x = date, y = hpi), size = 3, color = d_colors[5], alpha = 0.75)+
  geom_point(data = d_stateMin, aes(x = date, y = hpi), size = 3, color = d_colors[1], alpha = 0.75)+
  geom_hline(data = d_stateLast, aes(yintercept = hpi), linetype = 2, color = d_colors[3])+
  #"Legend" Labels 
  geom_text(data = d_labelsAll, aes(x = as.Date("2004-02-01"), y = 75, label = max_label), size = 3, family = "Arial Narrow")+
  geom_text(data = d_labelsAll, aes(x = as.Date("2004-01-01"), y = 65, label = min_label), size = 3, family = "Arial Narrow")+
  geom_text(data = d_labelsAll, aes(x = as.Date("2005-06-01"), y = 55, label = recent_label), size = 3, family = "Arial Narrow")+
  geom_point(aes(x= as.Date("2011-11-01"), y = 75), color = d_colors[5], size = 3)+
  geom_point(aes(x = as.Date("2011-11-01"), y = 65), color = d_colors[1], size = 3)+
  geom_point(aes(x = as.Date("2011-11-01"), y = 55), color = d_colors[3], size = 4)+
  #Theme of the plot 
  labs(x = "",
       y = "HPI by State, (log scale, Dec-2001 = 100)",
       title = "Freddie Mac State Home Price Index")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(size = 13, family = "Arial Rounded MT Bold", angle = -45),
    axis.text.y = element_text(size = 13, family="Arial Rounded MT Bold"),
    axis.title.y = element_text(size = 11, family = "Arial Rounded MT Bold"),
    strip.text = element_text(size = 13, family = "Arial Rounded MT Bold"),
    title = element_text(size = 17, family = "Arial Rounded MT Bold")
  )
p0
#fonts() to view all fonts available with extrafont

