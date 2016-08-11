rm(list = ls())

setwd("C:/Users/Anirudh Narayanan/Desktop/Some stuff/msba stuff/Summer/Stats/msba-stats-project/codes")

dataloc = '../data/final/'
delays = read.csv(paste0(dataloc,'final_delay_data.csv'),stringsAsFactors = F)


weather = read.csv(paste0(dataloc,'final_weather_data.csv'),stringsAsFactors = F)
colnames(weather)[colnames(weather) == 'FLDATE'] <- 'FL_DATE'
colnames(weather)[colnames(weather) == 'location'] <- 'ORIGIN'


final = merge(x = delays, y = weather, by = c('FL_DATE','ORIGIN'), all.x=T)


# model_data = final[which(final$DEP_DELAY >= 45 & final$ORIGIN == 'MSP' ),]
model_data = final[which(final$ORIGIN == 'DFW' & is.na(final$DEP_DELAY)==F),]


##TREATMENT OF Y VARIABLE
# model_data$delay_treated = ifelse(model_data$DEP_DELAY>= 330,330,model_data$DEP_DELAY)
model_data$delay2 = ifelse(model_data$DEP_DELAY < 0, 0 ,model_data$DEP_DELAY)
model_data$log_delay2 = log10(model_data$delay2 + 1)


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


##SEASONS
model_data$season = ifelse(model_data$MONTH %in% c(12,1,2),1,
                    ifelse(model_data$MONTH %in% c(3,4,5),2,
                    ifelse(model_data$MONTH %in% c(6,7,8,9),3,4)))

summary(lm( log_delay2 ~
            min_visibilitymiles +
            min_temperaturef +
            min_visibilitymiles +
            winddirdegrees +
            factor(events) +
            factor(airline_class) +
            factor(season) +
            factor(QUARTER) +
            DISTANCE +
            dep_hr
            ,
           # data = model_data[which(model_data$DEP_DELAY > 0),]))
           data = model_data))
