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
corr <- cor(corData,  use = "complete.obs")