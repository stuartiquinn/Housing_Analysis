# FRE_Natl_HPI
Stuart_Quinn  
`r format(Sys.time(), '%d %B, %Y')`  
#DRAFT

##Install Packages

```r
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
```

##Load Data Frame

```r
#######################Data Clean + Load ###############################
# url <- 'http://lenkiefer.com/img/charts_may_8_2016/fmhpi.txt'

d <- fread("hpi-data-fre-03-2016.txt")
d <- data.table(d)


d$date <- as.Date(d$date, format = "%m/%d/%Y")
```

##Engineer a Few Additional Fields

```r
######################Engineer Fields with data.table package#######################33
statedataMod <- d[,hpa:=c(NA, ((1+diff(hpi)/hpi))^12)-1, by=state]#Month over month 
statedataMod <- statedataMod[,hpa12:=c(rep(NA,12), ((1+diff(hpi,12)/hpi))^1)-1, by = state] #Year over year
statedataMod <- statedataMod[,hpa3:=c(rep(NA, 3), ((1 + diff(hpi,3)/hpi))^4)-1, by = state] #Quarter over quarter


#Create lags of data

statedataMod <- statedataMod[, hpi12:=shift(hpi, 12), by = state]

#Compute rolling min/max
#12-mo min and max rolling, lookbackward "right"
statedataMod <- statedataMod[,hpi12min:=rollapply(hpi, 12, min, fill = NA, na.rm=F, align = "right"), by = "state"]
statedataMod <- statedataMod[,hpi12max:=rollapply(hpi, 12, max, fill = NA, na.rm=F, align = "right"), by = "state"]
statedataMod <- statedataMod[,hpi3roll:=rollmean(hpi, 3, align = "right", na.rm = F), by = "state"]

##############################Engineer fields with DPLYR solution###############33
statedataMod2 <- statedataMod %>%
  group_by(state)%>%
  mutate(hpi122 = shift(hpi,12))


#Month over Month
statedataMod2 <- statedataMod %>%
  group_by(state) %>%
  mutate(hpa = ((1+diff(hpi)/hpi)^12)-1)

#Year over year
statedataMod2 <- statedataMod %>%
  group_by(state) %>%
  mutate(hpa12 = ((1+diff(hpi,12)/hpi)^1)-1)

#Quarter-over-Quarter
statedataMod2 <- statedataMod%>%
  group_by(state)%>%
  mutate(hpa3 = ((1+diff(hpi,3)/hpi)^4)-1)
```

##Create Plot Parameters for Plotting

```r
##############################################Plots################################

d_natl <- statedataMod%>%
  select(hpi, hpi3roll, state, date)%>%
  filter(state == "US SA", date > "1999-12-01")



#Get most recent and max period to annotate plot

hpiMax <- d_natl %>%
  select(date, hpi)%>%
  mutate(max = paste(date, ": max", round(hpi,0)))%>%
  slice(which.max(hpi))%>%
  as.data.frame()


hpiMin <- d_natl %>%
  select(date, hpi)%>%
  filter(date > "2007-03-01")%>% #from most recent peak 
  mutate(min = paste(date,": min", round(hpi,0)))%>%
  slice(which.min(hpi))%>%
  as.data.frame()



hpiRecent <- last(d_natl) %>%
  select(date, hpi)%>%
  mutate(most_recent = paste(date, ": recent", round(hpi,0)))%>%
  as.data.frame()
```

##Plot

```r
ggplot(data = d_natl, aes(x = date, y = hpi))+
  geom_line(color = "black", size = 1.25)+
  scale_x_date(labels = date_format("%b-%y"), date_breaks = "4 year",
               limits = as.Date(c('2000-01-01', '2016-01=3-01')))+
  scale_y_log10(limits = c(70,200), breaks = seq(75, 200, 25))+
  geom_text(data = hpiRecent, aes(x = date, y = hpi+5, label = most_recent), color = "black", size = 7, hjust=1)+
  geom_point(data = hpiRecent, aes(x = date, y = hpi), color = "orange",
             size = 5)+
  geom_hline(data= hpiRecent, aes(yintercept = hpi), linetype = 2, color = 'orange')+
  geom_text(data = hpiMax, aes(x = date, y =hpi+10, label = max), color = "black", size = 7)+
  geom_point(data = hpiMax, aes(x = date, y = hpi), color = "red", size = 5)+
  geom_text(data = hpiMin, aes(x = date, y=hpi-5,label = min), color = "black", size = 7, hjust=1)+
  geom_point(data = hpiMin, aes(x = date, y = hpi), color = "blue", size = 5)+
  labs(x = "", 
       y = "FRE Home Price Index, (Log Scale, Dec-2000 = 100)",
       title = "Freddie Mac National Home Price Index",
       subtitle = "U.S. Seasonally Adjusted")+
  theme_minimal()+
  theme(
    axis.text = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 12, vjust = 1),
    plot.title = element_text(size = 22, face = "bold")
  )
```

<img src="FRE_Natl_HPI_files/figure-html/plot-1.png" title="" alt="" style="display: block; margin: auto;" />


