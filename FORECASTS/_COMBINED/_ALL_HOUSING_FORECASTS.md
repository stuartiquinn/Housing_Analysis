# COMBINED_FORECASTS
Stuart_Quinn  
`r format(Sys.Date(), '%Y-%b')`  



#Forecast Sources: 

* Mortgage Bankers Association
* Freddie Mac
* Fannie Mae

All sources require a very similar process -- only one source produces an Excel based file so in order to organize we will need to do quite a bit of cleaning. 

* Get the link to the data, whether Excel or .pdf (we will use tabulizer for .pdf files)
    **NOTE** that we store these files locally for input purposes, but the URL will work too
* Drop the unecessary rows/columns, create standard titles
* Standardize all column names, create the proper data types and add in NA columns even if they do not exist
* Combine files, create an average aggregation of select measures 
* Build plots



```r
######################################
# FREDDIE MAC FORECASTS
######################################

#Get data url, download data, import data
url <- 'http://www.freddiemac.com/finance/docs/december_2016_public.xls'
temp <- tempfile()
d <- download.file(url, temp, mode = 'wb')
file.rename(temp, paste0(temp, '.xls'))
```

```
## [1] TRUE
```

```r
d <- read_excel(paste0(temp, '.xls'), skip = 5)
```

```
## DEFINEDNAME: 00 00 00 0d 07 00 00 00 01 00 00 00 00 00 00 50 72 69 6e 74 5f 41 72 65 61 5f 4d 49 3c 01 00 2f 00 2f 00 
## DEFINEDNAME: 00 00 00 0d 07 00 00 00 00 00 00 00 00 00 00 50 72 69 6e 74 5f 41 72 65 61 5f 4d 49 3a 00 00 2c 00 0e 00 
## DEFINEDNAME: 00 00 00 0d 07 00 00 00 01 00 00 00 00 00 00 50 72 69 6e 74 5f 41 72 65 61 5f 4d 49 3c 01 00 2f 00 2f 00 
## DEFINEDNAME: 00 00 00 0d 07 00 00 00 00 00 00 00 00 00 00 50 72 69 6e 74 5f 41 72 65 61 5f 4d 49 3a 00 00 2c 00 0e 00 
## DEFINEDNAME: 00 00 00 0d 07 00 00 00 01 00 00 00 00 00 00 50 72 69 6e 74 5f 41 72 65 61 5f 4d 49 3c 01 00 2f 00 2f 00 
## DEFINEDNAME: 00 00 00 0d 07 00 00 00 00 00 00 00 00 00 00 50 72 69 6e 74 5f 41 72 65 61 5f 4d 49 3a 00 00 2c 00 0e 00 
## DEFINEDNAME: 00 00 00 0d 07 00 00 00 01 00 00 00 00 00 00 50 72 69 6e 74 5f 41 72 65 61 5f 4d 49 3c 01 00 2f 00 2f 00 
## DEFINEDNAME: 00 00 00 0d 07 00 00 00 00 00 00 00 00 00 00 50 72 69 6e 74 5f 41 72 65 61 5f 4d 49 3a 00 00 2c 00 0e 00
```

```r
#Remove the notes from the bottom of the file for reference
notes <- d[c(21:30), 1]

#Remove unecessary rows
d1 <- d[-c(2:4,6,8:10,20:nrow(d)),]

#Rename columns
colnames(d1) <- d1[1,]

#Drop more columns
d1 <- d1[-c(1,7,12),]

#Create standard header names
d1$Indicator <- c('mtg_rate_30yr','ten_yr_treas', 'housing_starts_saar', 'tot_home_sales_saar', 'hpi_yoy', 
                  'conventional', 'fha_va', 'tot_orig_1to4', 'refi_share')

#Store column names for transposing data
n <- d1$Indicator

#Transpose data
d_trans_fre <- as.data.frame(t(d1[,-1]), stringsAsFactors = F)
#Reapply the column names
colnames(d_trans_fre) <- n
#Ensure that all columns are numerics for calculations
d_trans_fre <- sapply(d_trans_fre, as.numeric)
#Convert back into data.frame
d_trans_fre <- as.data.frame(d_trans_fre, row.names = NULL)


#Create derived measures and ensure that all values have the proper digits

#Create purchase share -- less refi share
d_trans_fre$purchase_share<- round((100-d_trans_fre$refi_share),2)
#Get $ value of purchase originations by multiplying pct share by total volume
d_trans_fre$purchase_orig <- round((d_trans_fre$purchase_share/100)*d_trans_fre$tot_orig_1to4,2)
#Get refi share
d_trans_fre$refi_orig <- d_trans_fre$tot_orig_1to4 - d_trans_fre$purchase_orig
#Convert housing starts into thousands
d_trans_fre$housing_starts_saar <- d_trans_fre$housing_starts_saar*10^3
d_trans_fre$tot_home_sales_saar <- d_trans_fre$tot_home_sales_saar*10^3

#Store NA for missing variables
d_trans_fre$sf_starts <- NA
d_trans_fre$two_plus_starts <- NA
d_trans_fre$existing_sales_cnt <- NA
d_trans_fre$new_home_sales_cnt <- NA
d_trans_fre$med_exist_home_price <- NA
d_trans_fre$med_new_home_price <- NA

#Create sequence of dates for all quarterly reported data
d_trans_fre$date <- c(seq(as.Date('2016/03/01'), as.Date('2017/12/01'), by='quarter'),
                      seq(as.Date('2013/01/01'), as.Date('2018/01/01'), 'years'))

#Add in "META" information we will need for when we combine forcasts
d_trans_fre$date_type <- c(rep('Quarter',8),rep('Annual',6))
d_trans_fre$entity <- 'FRE'
d_trans_fre$forecast_date <- as.Date('2016/12/01')
d_trans_fre$forecast_published <- as.Date('2016/12/12')
d_trans_fre$actual_estimate <- c(rep('A',3),rep('E',5), rep('A',3), rep('E',3))

#Re-order columns into a consistent order
d_trans_fre_final <- d_trans_fre[,c(1,2,3,13,14,4,15,16,17,18,5,8,6,7,11,12,10,9,19,20,24,21,22,23)]

######################################
# FANNIE MAE FORECASTS
######################################


#Set the path to the downloaded .pdf file
path2pdf <- '~/R/2_Economic_Chartbook/5_Housing/data/Housing_Forecast_122016.pdf'

path2pdf2 <- '~/R/2_Economic_Chartbook/5_Housing/data/Economic_Forecast_122016.pdf'
#Extract the tables from the .pdf path (read-in data)
out <- extract_tables(path2pdf)

out2 <- extract_tables(path2pdf2)
tmp_d <- as.data.frame(out2[[1]], stringsAsFactors = F)

tmp_d_treas <- t(tmp_d[23,-1])
tmp_d_treas <- as.numeric(tmp_d_treas)
#Convert the page into a data.frame
d <- as.data.frame(out[[1]], stringsAsFactors = F)
colnames(d) <- d[1,]

d1 <- d[-c(1,2,5,8,10,12,13,17,19,20),]

colnames(d1)[1] <- 'Indicator'

d1$Indicator <- c('housing_starts_saar', 'sf_starts', 'two_plus_starts', 'new_home_sales_cnt',
                  'existing_sales_cnt', 'tot_home_sales_saar','med_new_home_price',
                  'med_exist_home_price', 'hpi_yoy', 'mtg_rate_30yr', 'tot_orig_1to4', 
                  'purchase_orig', 'refi_orig', 'refi_share')

#Transpose the data
n <- d1$Indicator
d_trans_fnma <- data.frame(t(d1[,-1]), stringsAsFactors = F)
#Add column names back 
colnames(d_trans_fnma) <- n

d_trans_fnma[] <- lapply(d_trans_fnma, function(x) as.numeric(gsub("[^[:alnum:]///' ]", "", as.character(x))))

d_trans_fnma$mtg_rate_30yr <- (d_trans_fnma$mtg_rate_30yr/10)
d_trans_fnma$hpi_yoy <- (d_trans_fnma$hpi_yoy/10)

d_trans_fnma$purchase_share <- round((100 - d_trans_fnma$refi_share),2)
d_trans_fnma$conventional <- NA
d_trans_fnma$fha_va <- NA
d_trans_fnma$ten_yr_treas <- tmp_d_treas

d_trans_fnma$date <- c(seq(as.Date('2016/03/01'), as.Date('2018/12/01'), by = 'quarter'), 
                       seq(as.Date('2015/01/01'), as.Date('2018/01/01'), by = 'years'))
d_trans_fnma$date_type <- c(rep('Quarter', 12), rep('Annual', 4))
d_trans_fnma$actual_estimate <- c(rep('A', 3), rep('E', 9), 'A', rep('E', 3))
d_trans_fnma$entity <- 'FNMA'
d_trans_fnma$forecast_date <- as.Date('2016/12/01')
d_trans_fnma$forecast_published <- as.Date('2016/12/20')


rownames(d_trans_fnma) <- seq(1,nrow(d_trans_fnma), 1)
d_trans_fnma_final <- d_trans_fnma[,c(10,18,1,2,3,6,5,4,8,7,9,11,16,17,12,13,15,14, 19,20,21,22,23,24)]


######################################
# MORTGAGE BANKERS ASSOC FORECASTS
######################################

#Requires that file is downloaded and stored locally
#Reads in .pdf


#Set the path to the downloaded .pdf file
path2pdf <- '~/R/2_Economic_Chartbook/5_Housing/data/MFF_DEC16.pdf'
#Extract the tables from the .pdf path (read-in data)
out <- extract_tables(path2pdf)
#Convert the page into a data.frame
d <- as.data.frame(out[[1]], stringsAsFactors = F)

colnames(d) <- d[1,]
#Remove uncessary rows (metrics)
d1 <- d[-c(1:2,6,12,15),]

colnames(d1)[1] <- 'Indicator'

d1$Indicator <- c('housing_starts_saar', 'sf_starts', 'two_plus_starts', 'existing_sales_cnt',
                  'new_home_sales_cnt', 'hpi_yoy', 'med_exist_home_price','med_new_home_price',
                  'mtg_rate_30yr', 'ten_yr_treas','tot_orig_1to4', 'purchase_orig', 'refi_orig',
                  'refi_share')

#Transpose the data
n <- d1$Indicator
d_trans_mba <- data.frame(t(d1[,-1]), stringsAsFactors = F)
#Add column names back 
colnames(d_trans_mba) <- n

#Remove all spaces present from importing
d_trans_mba$refi_share <- gsub('\\s', '',d_trans_mba$refi_share)
#Removes all commas that are present as characters in the dataset sep 1000s fields
#Convert to numeric values
d_trans_mba[] <- lapply(d_trans_mba, function(x) as.numeric(gsub('\\,', '', as.character(x))))
d_trans_mba <- as.data.frame(d_trans_mba, row.names = NULL)

d_trans_mba$tot_home_sales_saar <- d_trans_mba$existing_sales_cnt+d_trans_mba$new_home_sales_cnt
d_trans_mba$purchase_share <- round((100-d_trans_mba$refi_share),2)


d_trans_mba$conventional <- NA
d_trans_mba$fha_va <- NA

d_trans_mba$date <- c(seq(as.Date('2016/03/01'), as.Date('2018/12/01'), by='quarter'),
                      seq(as.Date('2015/01/01'), as.Date('2019/01/01'), 'years'))

d_trans_mba$date_type <- c(rep('Quarter',12),rep('Annual',5))
d_trans_mba$entity <- 'MBA'
d_trans_mba$forecast_date <- as.Date('2016/12/01')
d_trans_mba$forecast_published <- as.Date('2016/12/14')
d_trans_mba$actual_estimate <- c(rep('A',3), rep('E',9),'A', rep('E',4))


d_trans_mba_final <- d_trans_mba[,c(9,10,1,2,3,15,4,5,7,8,6,11,17,18,12,13,16,14,19,20,24,21,22,23)]
rownames(d_trans_mba_final) <- seq(1,nrow(d_trans_mba_final),1)



#################################################
# CLEAN UP ENVIRONMENT -- COMBINED & WRITE FILES
#################################################

######################################
#CAREFUL, WILL CLEAR ENVIRONMENT!!!!!#
######################################

#Remove everything except the final files we have created
rm(list = setdiff(ls(), ls(pattern = "final")))

#Combine all of the files 
d_full_final <- rbind(d_trans_fnma_final, d_trans_mba_final, d_trans_fre_final)
```

#Measures

We will observe a number of forecasts that can be broadly categorized as: (1) financial rates; (2) housing production and (3) mortgage production. 

**Our core measures of interest include:** 
  + 30 Year Mortgage Rate
  + 10-Year Treasury Rate
  + Total Home Sales (Millions, Seasonal Annual Adjusted Rate)
  + Existing Home Sales (Millions)
  + New Home Sales (Thousands)
  + Housing Starts (Millions, Seasonal Annual Adjusted Rate)
  + Total Originations of 1-4 Single Family Mortgages ($, Billions)
  + Refinance and Purchase Share of Originations (% Share)
  + Home Price Index -- Year-over-Year
  

```r
d_qtr_sub <- d_full_final%>%
  select(mtg_rate_30yr, tot_home_sales_saar, housing_starts_saar, hpi_yoy, tot_orig_1to4, 
         existing_sales_cnt, new_home_sales_cnt, ten_yr_treas,
         date,entity, date_type, purchase_share, refi_share)%>%
  filter(date_type == 'Quarter'& year(date) > 2014 & year(date)<2018)


d_qtr_sub_avg <- d_qtr_sub%>%
  select(-entity, -date_type)%>%
  group_by(date)%>%
  na.omit()%>%
  summarize_each(funs(mean))
```

#Plots

##Home Prices

```r
p1_hpi_agg <- ggplot(d_qtr_sub_avg)+
  geom_line(aes(x = date, y = hpi_yoy), lwd = 1.25)+
  geom_line(data = d_qtr_sub%>%filter(entity == 'MBA'), aes(x = date, y = as.numeric(hpi_yoy)), lwd = 1, 
            alpha = 0.8, color = 'gold')+
  geom_line(data = d_qtr_sub%>%filter(entity == 'FRE'), aes(x = date, y = as.numeric(hpi_yoy)), lwd = 1, 
            alpha = 0.8, color = 'seagreen')+
  geom_line(data = d_qtr_sub%>%filter(entity == 'FNMA'), aes(x = date, y = as.numeric(hpi_yoy)), lwd = 1, 
            alpha = 0.8, color = 'orangered')+
  scale_y_continuous(limits = c(0, 8))+
  theme_minimal()+
  labs(x = NULL,
       y = 'Home Price Index (Year-over-Year)',
       title = 'Home Price Change Percent (Year-over-Year)', 
       caption = str_wrap('MBA, Fannie Mae, FRE. Note: Freddie uses Q.o.Q. measure, which creates a lower average year-over-year. This will create a more conservative avg. measure for us.', 45))

p1_hpi_agg
```

<img src="_ALL_HOUSING_FORECASTS_files/figure-html/p1HPI-1.png" title="" alt="" style="display: block; margin: auto;" />


##Home Sales

```r
p2_sales <- ggplot(d_qtr_sub_avg)+
  geom_bar(aes(x = date, y = tot_home_sales_saar), stat = 'identity', fill = 'dodgerblue1')+
  geom_text(aes(x = date, y = tot_home_sales_saar/2, 
                label = paste(prettyNum(round(tot_home_sales_saar, 0), big.mark = ','),'K')), 
            size = 7, color = 'white', angle = 90)+
  scale_y_continuous(limits = c(0, 6500), labels = comma)+
  scale_x_date(breaks = seq(as.Date('2016-03-01'), as.Date('2017-12-01'), by = '3 months'))+
  labs(x = NULL, 
       y = 'Total Sales (Thousands, SAAR)', 
       title = 'Total Seasonally Adjust Annualized Sales', 
       subtitle = 'Avg of Forecasts',
       caption = 'Freddie Mac, Fannie Mae, MBA')+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.text.x = element_text(size = 12, vjust = 1, angle = 90),
    axis.text.y = element_text(size = 12, hjust = 0)
  )

p2_sales
```

<img src="_ALL_HOUSING_FORECASTS_files/figure-html/p2Sales-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
p2.1_sales <- ggplot(d_qtr_sub)+
  geom_bar(aes(x = date, y = tot_home_sales_saar, fill = entity), stat = 'identity')+
  scale_y_continuous(limits = c(0, 6500), labels = comma)+
  scale_x_date(breaks = seq(as.Date('2016-03-01'), as.Date('2017-12-01'), by = '3 months'))+
  scale_fill_manual(values = c('orangered', 'seagreen', 'gold'),
                    guide = guide_legend(title = 'Forecast', label.position = 'top'))+
  geom_text(aes(x = date, y = tot_home_sales_saar/2, 
                label = paste(prettyNum(round(tot_home_sales_saar, 0), big.mark = ','),'K')), 
            size = 7, color = 'white', angle = 90)+
  facet_wrap(~entity, nrow = 1)+
  labs(x = NULL, 
       y = 'Total Sales (Thousands, SAAR)', 
       title = 'Total Seasonally Adjust Annualized Sales', 
       subtitle = 'Individual Forecasts',
       caption = 'Freddie Mac, Fannie Mae, MBA')+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.text.x = element_text(size = 12, margin = margin(t = 0), angle = 90),
    axis.text.y = element_text(size = 12, hjust = 0),
    strip.text = element_text(size = 15, face = 'bold')
  )

p2.1_sales
```

<img src="_ALL_HOUSING_FORECASTS_files/figure-html/p2Sales-2.png" title="" alt="" style="display: block; margin: auto;" />

###Components of Sales


```r
d_sales_components <- d_qtr_sub %>%
  select(existing_sales_cnt, new_home_sales_cnt, date, entity)%>%
  na.omit()%>%
  melt(., id.vars = c('entity', 'date'))

#Change levels for when plotting Facet the Strip Titles read as below
levels(d_sales_components$variable) <- c('Existing Home Sales', 'New Home Sales')


p2.2_sales <- ggplot(d_sales_components, aes(x = as.factor(date), y = value, fill = entity))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_text(aes(y = value/2, label = prettyNum(value, big.mark = ',')),
            position = position_dodge(width = 0.9), size = 7, color = 'white', angle = 90)+
  scale_x_discrete(labels = function(x) format(as.Date(x), '%Y-%b'))+
  scale_fill_manual(values = c('orangered', 'gold'), 
                     guide = guide_legend(title = 'Forecast', label.position = 'top'))+
  facet_wrap(~variable, ncol = 1, scales = 'free_y')+
  labs(x = NULL, 
       y = 'Number of Sales (Thousands, SAAR)', 
       title = 'Components of Total Sales (Thousands, SAAR)', 
       subtitle = 'Individual Forecasts (No FRE)',
       caption = 'Fannie Mae and MBA')+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.text.x = element_text(size = 12, margin = margin(t = 0), angle = 90),
    axis.text.y = element_text(size = 12, hjust = 0),
    strip.text = element_text(size = 15, face = 'bold')
  )



p2.2_sales
```

<img src="_ALL_HOUSING_FORECASTS_files/figure-html/p3SaleComponents-1.png" title="" alt="" style="display: block; margin: auto;" />

##Originations

```r
p3_orig <- ggplot(d_qtr_sub_avg, aes(x = date, y = tot_orig_1to4))+
  geom_bar(stat = 'identity', fill = 'dodgerblue1')+
  geom_text(aes(x = date, y = tot_orig_1to4/2, label = paste0('$', prettyNum(round(tot_orig_1to4,0), big.mark = ','), 'B')),
            size = 7, color = 'white', angle = 90)+
  scale_x_date(breaks = seq(as.Date('2016-03-01'), as.Date('2017-12-01'), by = '3 months'),
               label = date_format('%Y-%b'))+
  labs(x = NULL, 
       y = 'Origination Volume ($B)',
       title = 'Quarterly 1-4 Single-Family Originations', 
       subtitle = 'Avg of Forecast',
       caption = 'Fannie Mae, Freddie Mac, MBA')+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.text.x = element_text(size = 12, margin = margin(t = 0)),
    axis.text.y = element_text(size = 12, hjust = 0),
    strip.text = element_text(size = 15, face = 'bold')
  )

p3_orig
```

<img src="_ALL_HOUSING_FORECASTS_files/figure-html/p3Orig-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
p3.1_orig <- ggplot(d_qtr_sub,
                    aes(x = as.factor(date), y = tot_orig_1to4, fill = entity))+
  geom_bar(stat = 'identity', position = 'dodge')+
  geom_text(aes(y = tot_orig_1to4/2, label = paste0('$',prettyNum(tot_orig_1to4, big.mark = ','),'B')),
                position = position_dodge(width = 0.9), size = 7, color = 'white', angle = 90)+
  scale_x_discrete(label = function(x) format(as.Date(x), '%Y-%b'))+
  scale_fill_manual(values = c('orangered', 'seagreen', 'gold'), 
                    guide = guide_legend(title = 'Forecast', label.position = 'top'))+
  labs(x = NULL, 
       y = 'Total Originations ($B)', 
       title = 'Total 1-4 Origination Forecast',  
       subtitle = 'Individual Forecasts',
       caption = 'Fannie Mae, Freddie Mac, MBA')+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.text.x = element_text(size = 12, margin = margin(t = 0), angle = 90),
    axis.text.y = element_text(size = 12, hjust = 0)
  )



p3.1_orig
```

<img src="_ALL_HOUSING_FORECASTS_files/figure-html/p3Orig-2.png" title="" alt="" style="display: block; margin: auto;" />

###Origination Share


```p

d_share <- d_qtr_sub%>%
  select(refi_share, purchase_share, date, entity)%>%
  melt(., id.vars=c('date', 'entity'))%>%
  group_by(date, entity)
 


p4_share <- ggplot(d_share, aes(x = as.factor(date), y = as.numeric(value), fill = variable))+
  geom_bar(stat = 'identity')+
  geom_text(aes(y = value-7, label = paste0(value,'%')), position = 'stack', vjust = 1, 
            size = 7, color = 'white', angle = 90)+
  facet_wrap(~entity)+
  scale_fill_manual(values = c('dodgerblue', 'firebrick'),
                    guide = guide_legend(title = 'Loan Purpose', label.position = 'top', angle = 90))+
  scale_x_discrete(labels = function(x) format(as.Date(x), '%Y-%b'))+
  labs(x = NULL, 
       y = 'Percent of Originations',
       title = 'Share of Originations by Loan Purpose', 
       subtitle = 'Individual Forecasts',
       caption = 'Fannie Mae, Freddie Mac, MBA')+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.text.x = element_text(size = 12, margin = margin(t = 0), angle = 90),
    axis.text.y = element_text(size = 12, hjust = 0),
    strip.text = element_text(size = 15, face = 'bold')
  )

p4_share

```


```r
d_rates_avg <- d_qtr_sub_avg%>%
  select(ten_yr_treas, mtg_rate_30yr, date)%>%
  melt(., id.vars = 'date')

d_rates_entity <- d_qtr_sub%>%
  select(ten_yr_treas, mtg_rate_30yr, date, entity)%>%
  melt(., id.vars=c('entity', 'date'))


p5_rates <- ggplot(d_rates_avg, aes(x = date, y = as.numeric(value), color = variable))+
  geom_line(lwd = 1.15)+
  scale_color_manual(values = c('dodgerblue1', 'firebrick1'), guide = guide_legend(title = 'Forecast'))+
  scale_x_date(breaks = seq(as.Date('2016-03-01'), as.Date('2017-12-01'), by = '3 months'),
                            label = date_format('%Y-%b'))+
  scale_y_continuous(limits = c(0,5.5), breaks = seq(0,5.5,0.25))+
  labs(
    x = NULL,
    y = 'Rate (Percent)',
    title = 'Ten-Year Treasury & 30 Year Fixed Rate Mortgage',
    subtitle = 'Avg of Forecasts',
    caption = 'Fannie Mae, Freddie Mac, MBA'
  )+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.text.x = element_text(size = 12, margin = margin(t = 0), angle = 45),
    axis.text.y = element_text(size = 12, hjust = 0),
    legend.position = 'bottom'
  )

p5_rates
```

<img src="_ALL_HOUSING_FORECASTS_files/figure-html/p5Rates-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
p5.1_rates <- ggplot(d_rates_entity, aes(x = date, y = as.numeric(value), color = entity))+
  geom_line(aes(linetype = variable),lwd = 1)+
  scale_color_manual(values = c('orangered', 'seagreen', 'gold'))+
  scale_linetype_manual(values = c('dotdash', 'solid'))+
  scale_x_date(breaks = seq(as.Date('2016-03-01'), as.Date('2017-12-01'), by = '3 months'),
               label = date_format('%Y-%b'))+
  labs(
    x = NULL,
    y = 'Rate (Percent)',
    title = 'Ten-Year Treasury & 30 Year Fixed Rate Mortgage',
    subtitle = 'Individual Forecasts',
    caption = 'Fannie Mae, Freddie Mac, MBA'
  )+
  guides(color = guide_legend(title = ''),
         linetype = guide_legend(title = ''))+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = 'transparent', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = 'transparent', color = NA),
    axis.text.x = element_text(size = 12, margin = margin(t = 0), angle = 90),
    axis.text.y = element_text(size = 12, hjust = 0),
    legend.position = 'bottom',
    legend.box = 'horizontal',
    legend.key = element_rect(fill = 'transparent', color = NA)
  )
                        

p5.1_rates
```

<img src="_ALL_HOUSING_FORECASTS_files/figure-html/p5Rates-2.png" title="" alt="" style="display: block; margin: auto;" />

##Table of Aggregation Averages


```r
datatable(d_qtr_sub_avg, class = 'stripe', extensions = 'Buttons',
          colnames = c('Period', '30-Yr Mortgage Rate', 'Total Home Sales (K,SA,Annualized)','Housing Starts (K, SA, Annualized', 'HPI-Year-over-Year', 'Total Originations ($B)','Existing Home Sales', 'New Home Sales', 'Ten Year Treasury'),
          options = list(dom = 'Bfrtip', 
                         buttons = 'csv'))%>%
 formatRound(.,2:ncol(d_qtr_sub_avg), digits = 1)
```

<!--html_preserve--><div id="htmlwidget-eadd9cdee54afb3a4f54" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-eadd9cdee54afb3a4f54">{"x":{"filter":"none","extensions":["Buttons"],"data":[["1","2","3","4","5","6","7","8"],["2016-03-01","2016-06-01","2016-09-01","2016-12-01","2017-03-01","2017-06-01","2017-09-01","2017-12-01"],[3.7,3.6,3.4,3.8,4.2,4.25,4.35,4.45],[5829,6068,5974.5,6108.5,6148,6254.5,6318,6301],[1151,1159,1145,1235,1230,1277.5,1302.5,1335],[5.8,5.6,5.45,5.35,5.15,5.05,4.8,4.7],[354.5,499.5,566.5,477,353.5,430.5,429.5,358.5],[5300,5503,5386.5,5524,5511,5596,5655.5,5630],[529,565,588,584.5,637,658.5,663,671],[1.9,1.8,1.6,2.1,2.4,2.5,2.6,2.7],[53,56.5,51.5,50,57,70.5,73,70.5],[47,43.5,48.5,50,43,29.5,27,29.5]],"container":"<table class=\"stripe\">\n  <thead>\n    <tr>\n      <th>Period\u003c/th>\n      <th>30-Yr Mortgage Rate\u003c/th>\n      <th>Total Home Sales (K,SA,Annualized)\u003c/th>\n      <th>Housing Starts (K, SA, Annualized\u003c/th>\n      <th>HPI-Year-over-Year\u003c/th>\n      <th>Total Originations ($B)\u003c/th>\n      <th>Existing Home Sales\u003c/th>\n      <th>New Home Sales\u003c/th>\n      <th>Ten Year Treasury\u003c/th>\n    \u003c/tr>\n  \u003c/thead>\n\u003c/table>","options":{"dom":"Bfrtip","buttons":"csv","columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10,11]}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 2, 1);\nDTWidget.formatRound(this, row, data, 3, 1);\nDTWidget.formatRound(this, row, data, 4, 1);\nDTWidget.formatRound(this, row, data, 5, 1);\nDTWidget.formatRound(this, row, data, 6, 1);\nDTWidget.formatRound(this, row, data, 7, 1);\nDTWidget.formatRound(this, row, data, 8, 1);\nDTWidget.formatRound(this, row, data, 9, 1);\nDTWidget.formatRound(this, row, data, 10, 1);\nDTWidget.formatRound(this, row, data, 11, 1);\n}"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->


#Export Data Sets for Use


```r
###########################################
# Build Function to Export Data into Excel#
###########################################


save.xlsx <- function(file, ...)
{
  require(xlsx, quietly = T)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = T))
  objnames <- as.character(fargs)[-c(1,2)]
  nobjects <- length(objects)

  for(i in 1:nobjects)
  {
    if(i == 1)
      
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    
    else
      
      write.xlsx(objects[[i]], file, sheetName = objnames[i], append = T)
  }

  print(paste('Workbook', file, 'has', nobjects, 'worksheets.'))

}

#####################################
#Save files to current working drive#
#####################################

# save.xlsx('2016_12_Forecasts_FNMA_FRE_MBA.xlsx', data.frame(d_trans_fnma_final), 
#           data.frame(d_trans_fre_final), data.frame(d_trans_mba_final), 
#           data.frame(d_full_final))
```

