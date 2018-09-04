library(reshape)
library(ggplot2)


# create empty dataframes for utility and weather data
utilityData <- data.frame()
weatherData <- data.frame()

# iterate through years and compile utility and weather data
for (i in (2014:2018))
  {
  currentUtility <- read.csv(paste(i,'.csv', sep=''))
  utilityData <- rbind(utilityData, currentUtility)
}


for (i in (2014:2018))
{
  currentWeather <- read.csv(paste('weather',i,'.csv', sep=''))
  weatherData <- rbind(weatherData, currentWeather)
}

# clean weather data for relevant columns
weatherDataRelevant <- weatherData[c('Date.Time', 'Max.Temp...C.', 'Min.Temp...C.', 'Mean.Temp...C.', 'Total.Rain..mm.', 
                                     'Total.Snow..cm.', 'Total.Precip..mm.', 'Snow.on.Grnd..cm.', 'Spd.of.Max.Gust..km.h.')]

# rename data columns to be able to merge
names(utilityData)[1] <- 'DATE'
names(weatherDataRelevant)[1] <- 'DATE'

# merge tables
dayUsageWeather <- merge(utilityData, weatherDataRelevant, by = 'DATE')

# set data for correlation by taking out date column
corData <-dayUsageWeather[2:(ncol(dayUsageWeather)-1)]

# get correlation
corr <- cor(corData,  use = "complete.obs", method = 'pearson')

# no significant correlation so move onto grouping by months

# taking date and overall usage and weather data
consData <- dayUsageWeather[c(1, 26:35)]

# setting up month and year columns
consData$DATE <- as.Date(consData$DATE)
consData$Month <- substr(consData$DATE,6,7)
consData$Year <- substr(consData$DATE, 1, 4)

# renaming the columns
names(consData) <- c("Date", "OnPeak", "MidPeak", "OffPeak", "MaxTemp", "MinTemp", "MeanTemp", "Rain_mm", "Snow_cm", "Prec_mm", "SnowGround_cm", "Month", "Year")

# aggregate the data at a month level
sumData <- aggregate(consData[names(consData[c(2:4, 8:11)])], by = consData["Month"], sum, na.rm=TRUE, na.action=NULL)
avgData <- aggregate(consData[names(consData[5:7])], by = consData["Month"], mean, na.rm=TRUE, na.action=NULL)

# merge the aggregated data
fullData <- merge(sumData, avgData, by = "Month")

# melt the dataframe into respective temperature data as x
meanTempLong <- melt(consData, id = "MeanTemp", measure = c("OnPeak", "MidPeak", "OffPeak"))
RainLong <- melt(consData, id = "Rain_mm", measure = c("OnPeak", "MidPeak", "OffPeak"))
SnowLong <- melt(consData, id = "Snow_cm", measure = c("OnPeak", "MidPeak", "OffPeak"))
PrecLong <- melt(consData, id = "Prec_mm", measure = c("OnPeak", "MidPeak", "OffPeak"))
SnowGLong <- melt(consData, id = "SnowGround_cm", measure = c("OnPeak", "MidPeak", "OffPeak"))

# plot the relationships
theme_set(theme_bw())

ggplot(data = meanTempLong, aes(x = MeanTemp, y = value, colour = variable)) + geom_point() +
  labs(subtitle="Mean Temperature vs. Usage", 
       y="Electricity Usage (kWh)", 
       x="Mean Temperature (C)", 
       title="Scatterplot",
       colour= "Time of Usage")

ggplot(data = RainLong, aes(x = Rain_mm, y = value, colour = variable)) + geom_point() +
  labs(subtitle="Rain vs. Usage", 
       y="Electricity Usage (kWh)", 
       x="Rain (mm)", 
       title="Scatterplot",
       colour= "Time of Usage")

ggplot(data = SnowLong, aes(x = Snow_cm, y = value, colour = variable)) + geom_point()+
  labs(subtitle="Snow vs. Usage", 
       y="Electricity Usage (kWh)", 
       x="Snow (cm)", 
       title="Scatterplot",
       colour= "Time of Usage")

ggplot(data = PrecLong, aes(x = Prec_mm, y = value, colour = variable)) + geom_point() +
  labs(subtitle="Precipitation vs. Usage", 
       y="Electricity Usage (kWh)", 
       x="Precipitation (mm)", 
       title="Scatterplot",
       colour= "Time of Usage")

ggplot(data = SnowGLong, aes(x = SnowGround_cm, y = value, colour = variable)) + geom_point() +
  labs(subtitle="Snow on Ground vs. Usage", 
       y="Electricity Usage (kWh)", 
       x="Snow on Ground (cm)", 
       title="Scatterplot",
       colour= "Time of Usage")
