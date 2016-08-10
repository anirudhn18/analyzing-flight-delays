setwd("C:/Users/Anirudh Narayanan/Desktop/Some stuff/msba stuff/Summer/Stats/msba-stats-project/codes")

##WEATHER DATA PROCESSING
dataloc = '../data/raw/weather/'

weather_data = read.csv(paste0(dataloc,'msp-weather.txt'))
weather_data = rbind(weather_data,read.csv(paste0(dataloc,'dfw-weather.txt')))
weather_data$location = c(rep('MSP',396),rep('DFW',396))
colnames(weather_data) = tolower(gsub('\\.',"_",colnames(weather_data)))
weather_data$FL_DATE = format(as.Date(as.character(weather_data$cst)))


write.csv(weather_data,file = '../data/final/final_weather_data.csv',
          row.names = F)


##AIRPORT PROCESSING
dataloc = '../data/raw/delays/'
filelist = list.files(dataloc)

delay_data = data.frame()

for (a.file in filelist){
delay_data = rbind(delay_data,read.csv(paste0(dataloc,a.file)))
}

write.csv(delay_data[delay_data$ORIGIN %in% c('MSP','DFW'),],
          file= '../data/final/final_delay_data.csv',
          row.names = F)
colnames(delay_data) = tolower(colnames(delay_data))