
setwd(dir = "Documents/CrimeData/R/projet/")
#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------------#
# 2 - DATA PREPARATION
# Read datasets from csv files provided in the project directory.

# https://data.cityofchicago.org/api/views/ijzp-q8t2/rows.csv?accessType=DOWNLOAD
crime_data <- read.csv(file = "crimes_2001_to_present.csv")

# https://beta.bls.gov/dataViewer/view/timeseries/LAUCT171400000000004
unemployment <- read.csv(file="unemployment.csv")

# https://beta.bls.gov/dataViewer/view/timeseries/LAUCT171400000000003
unemployment_rate <- read.csv("unemployment_rate.csv")

# https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD
socio_economic_ind <- read.csv("socioeconomic_indicators.csv")

crime_data_bak <- crime_data
unemployment_bak <- unemployment
unemployment_rate_bak <- unemployment_rate
socio_economic_ind_bak <- socio_economic_ind


#-------------------------------------------------------------------------------------------------------#
# 2.1 - ABOUT THE DATA

## See basic details about the datasets

str(crime_data)
summary(crime_data)
dim(crime_data)

str(unemployment)
summary(unemployment)
dim(unemployment)

str(unemployment_rate)
summary(unemployment_rate)
dim(unemployment_rate)

str(socio_economic_ind)
summary(socio_economic_ind)
dim(socio_economic_ind)

# See crime primary type and location description levels.
unique(crime_data$Primary.Type)
unique(crime_data$Location.Description)

## Installing neccessary packages

# For alternative loading of #the %>% operator and other grammar of data manipulation functions.
if(!requireNamespace("dplyr")) install.packages("dplyr")
# For usage the mdy and other date manipulating functions.
if(!requireNamespace("lubridate")) install.packages("lubridate")
# For Visualization and Imputation of Missing Values.
if(!requireNamespace("naniar")) install.packages("naniar")
#
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")

# For Visualization and Imputation of Missing Values.
if(!requireNamespace("rgdal")) install.packages("rgdal")
if(!requireNamespace("sp")) install.packages("sp")
if(!requireNamespace("raster")) install.packages("raster")
if(!requireNamespace("prophet")) install.packages("prophet")
if(!requireNamespace("xts")) install.packages("xts")
if(!requireNamespace("highcharter")) install.packages("highcharter")

## Loading of libraries to be used.

library(dplyr)
library(lubridate)
library(naniar)
library(ggmap)
library(rgdal)
library(sp)
library(raster)
library(prophet)
library(xts)
library(highcharter)
library(ggplot2)
#-------------------------------------------------------------------------------------------------------#
# 2.2 - DATA CONDITIONING

## Data Conditioning -- Removing columns which are not used.
## Operations will be done on our backup variable to keep the original values intact.

crime_data_pp <-
  dplyr::select(crime_data,
                -c(ID, Case.Number, IUCR, FBI.Code, Description, Block, District,
                   Beat, Ward, X.Coordinate, Y.Coordinate, Location, Updated.On))

# See the structure of the new dataset.
str(crime_data_pp)

## Visualizing missing observations.
## Plot missing values percentage graph.

naniar::gg_miss_var(crime_data_pp,show_pct = TRUE) +
  labs(y = "Missing values percentage.") +
  ggtitle(label = "Missing Variables")

# See summary and count of missing observations and variables.
naniar::miss_var_summary(crime_data_pp)
naniar::n_miss(crime_data_pp)

# Omit any missing values and save it in to a variable.
# Check again if there are any missing variables.
crime_data_cln <- na.omit(crime_data_pp)
naniar::n_miss(crime_data_cln)


# Prepare date object and add it in the date column of the dataset.
# Also Prepare other coulmns needed.

date_object <- as.Date(mdy_hms(crime_data_cln$Date))
crime_data_cln$Month = months(date_object)
crime_data_cln$Weekday = weekdays(date_object)
crime_data_cln$Date = date_object
class(crime_data_cln$Date)


## Cleaning and Merging the unemployment & unemployemnt_rate datasets.

unemployment <- dplyr::select(unemployment, -c(Series.ID, Label))
rate <- unemployment_rate$Value
unemployment <- cbind(unemployment, rate)
unemployment$Month <- month.name[as.numeric(substr(unemployment$Period,start = 2,stop =3))]
unemployment <- dplyr::select(unemployment , -c(Period))
str(unemployment)


## Cleaning the socio economic indicator dataset.
naniar::n_miss(socio_economic_ind)
socio_economic_ind_cln <- na.omit(socio_economic_ind)
naniar::n_miss(socio_economic_ind_cln)


#-------------------------------------------------------------------------------------------------------#
# 2.3 - SUNRVEY AND VISSUALIZE

#LOAD CHICAGO MAP
ggmap::register_google(key = "[********REDACTED***********]") #API KEY FOR GOOGLE MAPS, IT MUST BE KEPT IN PRIVATE

chicago_map <- ggmap(get_googlemap(center = c(lon = -87.6298, lat = 41.8781),
                                   zoom = 10, scale = 2, maptype ='terrain', color = 'color'))
chicago_map

#Load community boundaries that shows the borders of each community area in chicago
community_bounderies <-
  shapefile('Boundaries - Community Areas (current)/geo_export_06da6292-cd0b-4809-bf57-7811582e6aa4.shp')

community_bounderies <- spTransform(x=community_bounderies, CRSobj=CRS("+init=epsg:4326"))

chicago_map + geom_polygon(data = fortify(community_bounderies),
                           aes(long, lat, group = group),
                           fill = "orange", colour = "red", alpha = 0.2)

chicago_map + geom_point(aes(x = Longitude, y = Latitude,  colour = '#0000ff'),
      data = crime_data_cln, alpha=0.05, size = 0.5) +
      geom_polygon(data = fortify(community_bounderies), aes(long, lat, group = group),
      fill = "orange", colour = "yellow", alpha = 0.2)+ theme(legend.position="bottom")



#Plot frequency of crimes by their primary type

crime_types <- aggregate(crime_data_cln$Primary.Type,
      by = list(Type = crime_data_cln$Primary.Type), FUN = length)

colnames(crime_types) <- c("Crime.Type", "Total")

crime_types_sorted <- crime_types[order(crime_types$Total, decreasing=T),]


ggplot(crime_types_sorted, aes(x=reorder(Crime.Type,Total), y=Total)) +
  geom_bar(aes(fill=Crime.Type), stat="identity") +
  coord_flip() + theme(legend.position = "none")



#Plot frequency of crimes by their Location Description

crime_locations <- aggregate(crime_data_cln$Location.Description,
                             by = list(Type = crime_data_cln$Location.Description), FUN = length)

colnames(crime_locations) <- c("Location", "Total")

crime_locations_sorted <- crime_locations[order(crime_locations$Total, decreasing=T),]

top_ten_crime_locs <- crime_locations_sorted[1:10,]

ggplot(top_ten_crime_locs, aes(x=reorder(Location,Total), y=Total)) +
  geom_bar(aes(fill=Location), stat="identity") +
  coord_flip() + theme(legend.position = "none")


options(scipen=999)


plot(x = crime_data_cln$Arrest, main="Arrest", col = c("blue", "red"))
plot(x = crime_data_cln$Domestic, main="Domestic Violence", col = c("blue", "red"))

#-------------------------------------------------------------------------------------------------------#
## 3 - MODEL BUILDING

#Show top 10 community areas with the highest crime rates

crime_areas <- aggregate(crime_data_cln$Community.Area,
                             by = list(Type = crime_data_cln$Community.Area), FUN = length)

colnames(crime_areas) <- c("Community", "Total")
list_of_communities = socio_economic_ind$COMMUNITY.AREA.NAME
Community.Area.Name <- factor(crime_areas, levels=list_of_communities)
Community.Area.Name <- list_of_communities[crime_areas$Community]
crime_areas_sorted <- crime_areas[order(crime_areas$Total, decreasing=T),]
crime_areas_sorted <- cbind(crime_areas[2:78,], Community.Area.Name)
top_ten_crime_areas <- crime_areas_sorted[1:10,]

ggplot(top_ten_crime_areas, aes(x=reorder(Community.Area.Name,Total), y=Total)) +
    geom_bar(aes(fill=Community.Area.Name), stat="identity") +
  coord_flip() + theme(legend.position = "none")

#show top 10 community areas with highest hardship indexes

socio_econ <- select(socio_economic_ind, -c(Community.Area.Number,PER.CAPITA.INCOME, PERCENT.HOUSEHOLDS.BELOW.POVERTY,PERCENT.AGED.16..UNEMPLOYED, PERCENT.OF.HOUSING.CROWDED, PERCENT.AGED.UNDER.18.OR.OVER.64, PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA))
colnames(socio_econ) <- c("Community", "Hardship")
socio_econ_sorted <- socio_econ[order(socio_econ$Hardship, decreasing=T),]
top_ten_socio <- socio_econ_sorted[1:10,]

ggplot(top_ten_socio, aes(x=reorder(Community,Hardship), y=Hardship)) +
  geom_bar(aes(fill=Hardship), stat="identity") +
  coord_flip() + theme(legend.position = "none")

#show density plot of crimes on map
chicago_map + stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha = 0.25),
      size = 0.01, bins = 30, data = crime_data_cln[crime_data_cln$Year>2011,], geom = "polygon") +
      geom_polygon(data = fortify(community_bounderies), aes(long, lat, group = group),
      fill = "orange", colour = "yellow", alpha = 0.2)

#show density plot of 6 chosen crimes on map
dangerous_crimes <- crime_data_cln[crime_data_cln$Primary.Type %in%
      c( 'HOMICIDE','ASSAULT','WEAPONS VIOLATION', 'BATTERY', 'ROBBERY', 'NARCOTICS'),]

chicago_map + stat_density2d( aes(x = Longitude, y = Latitude, fill = ..level.., alpha =..level..),
      size = 0.2, bins = 30, data = dangerous_crimes, geom = "polygon") +
      geom_density2d(data = dangerous_crimes, aes(x = Longitude, y = Latitude),
      size = 0.3) + facet_wrap(~ Primary.Type, nrow=2)

#show heatmap of crimes on each month of year
data_for_heatmap <- crime_data_cln %>%
      dplyr::select(Month, Year) %>%
      mutate(Year = as.ordered(Year)) %>%
      table()

heatmap(data_for_heatmap, Colv = NA, Rowv = NA, scale = "column")
table(crime_data_cln$Month)

#Calculate Time series of crime rate and unemployment
group_crimes_by_date <- crime_data_cln %>% group_by(Date) %>% summarise(Total = n())
time_series <- xts(group_crimes_by_date$Total, order.by=as.POSIXct(group_crimes_by_date$Date))


hchart(time_series, name = "Crimes") %>%
  hc_title(text = "Times Series of Crime Rates In Chicago") %>%
  hc_legend(enabled = TRUE)

unemp_ts <- xts(unemployment$rate, order.by = as.POSIXct(as.Date(ymd(paste(unemployment$Year, unemployment$Month, '1', sep="-")))))

hchart(time_series, name = "Unemployment") %>%
  hc_title(text = "Times Series of Unemployment In Chicago") %>%
  hc_legend(enabled = TRUE)

crimes_ts <- crime_data_cln %>% group_by(Date) %>% summarise(y = n()) %>% mutate(y = log(y))

crime_rate_ts <- xts(crimes_ts$y, order.by = as.POSIXct(crimes_ts$ds))

hchart(crime_rate_ts, name = "Crime Rate") %>%
    hc_add_series(unemp_ts, name = "Unemployment") %>%
    hc_title(text = "Times Series of Unemployment and Crime In Chicago") %>%
    hc_legend(enabled = TRUE)

#create a time series forecasting model to predict crime rate in the next 5 years
names(crimes_ts) <- c("ds", "y")
crimes_ts$ds <- factor(crimes_ts$ds)

model <- prophet(crimes_ts)
future <- make_future_dataframe(model, periods = 365 * 5)

forecast <- predict(model, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(model, forecast)
prophet_plot_components(model, forecast)

