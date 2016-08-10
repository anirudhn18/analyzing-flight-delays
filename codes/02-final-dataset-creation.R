rm(list = ls())

setwd("C:/Users/Anirudh Narayanan/Desktop/Some stuff/msba stuff/Summer/Stats/msba-stats-project/codes")

dataloc = '../data/final/'
delays = read.csv(paste0(dataloc,'final_delay_data.csv'),stringsAsFactors = F)


weather = read.csv(paste0(dataloc,'final_weather_data.csv'),stringsAsFactors = F)
colnames(weather)[colnames(weather) == 'FLDATE'] <- 'FL_DATE'
colnames(weather)[colnames(weather) == 'location'] <- 'ORIGIN'


final = merge(x = delays, y = weather, by = c('FL_DATE','ORIGIN'), all.x=T)


model_data = final[which(final$DEP_DELAY >= 10 & final$ORIGIN == 'MSP' ),]
model_data = final[which(final$ORIGIN == 'MSP'),]
model_data$delay_treated = ifelse(model_data$DEP_DELAY>= 330,330,model_data$DEP_DELAY)
model_data$ln_delay = log(model_data$DEP_DELAY + 1)

##HAS EVENTS?
model_data$has_events = ifelse(nchar(model_data$events)>0, 1,0)

##MAJOR/MINOR AIRLINES
model_data$airline_class = ifelse(model_data$AIRLINE_ID %in% c(19805,19790,19393,19977,19531,19930,
                                                          20229,21294,20416,20435,21351,19690,
                                                          20368,21171,21578,21226,19961,21342,
                                                          20422),
                                  1,0)


##IS WEEKEND?
model_data$is_weekend = ifelse(model_data$DAY_OF_WEEK > 4, 1,0)

##HOUR OF DEPARTURE
model_data$dep_hr = model_data$CRS_DEP_TIME%/%100

summary(lm(delay_treated ~
            min_visibilitymiles +
            min_temperaturef +
            min_visibilitymiles +
            winddirdegrees +
            as.factor(events) +
            airline_class +
            QUARTER +
            AIR_TIME +
            dep_hr
            ,
           data = model_data))