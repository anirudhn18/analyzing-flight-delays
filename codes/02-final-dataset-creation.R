
#Only msp dataset

setwd("C:/Users/Anirudh Narayanan/Desktop/Some stuff/msba stuff/Summer/Stats/msba-stats-project/codes")

dataloc = '../data/final/'
delays = read.csv(paste0(dataloc,'final_delay_data.csv'),stringsAsFactors = F)
delays$date = format(as.Date(delays$FL_DATE))

weather = read.csv(paste0(dataloc,'final_weather_data.csv'),stringsAsFactors = F)
colnames(weather)[colnames(weather) == 'location'] <- 'ORIGIN'
weather$date = format(as.Date(weather$cst))

final = merge(x = delays, y = weather, by = c('date','ORIGIN'), all.x=T)

model_data = final[which(final$DEP_DELAY >= 10 & final$ORIGIN == 'DFW' & ),]
model_data$DEP_DELAY[which(model_data$DEP_DELAY>= 377)] = 377
model_data$has_events = nchar(model_data$events)
model_data$has_events[which(model_data$has_events > 0)] = 1


summary(model_data$DEP_DELAY)
quantile(model_data$DEP_DELAY,seq(0.9,1,0.005))


summary(lm(DEP_DELAY ~ mean_visibilitymiles + max_temperaturef + min_visibilitymiles + winddirdegrees
           + has_events, data = model_data))
