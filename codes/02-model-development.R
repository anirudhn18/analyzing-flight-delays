rm(list = ls())

setwd("C:/Users/Anirudh Narayanan/Desktop/Some stuff/msba stuff/Summer/Stats/msba-stats-project/codes")

dataloc = '../data/final/'
delays = read.csv(paste0(dataloc,'final_delay_data.csv'),stringsAsFactors = F)
delays$FL_DATE = as.Date(delays$FL_DATE)

weather = read.csv(paste0(dataloc,'final_weather_data.csv'),stringsAsFactors = F)
colnames(weather)[colnames(weather) == 'FLDATE'] <- 'FL_DATE'
colnames(weather)[colnames(weather) == 'location'] <- 'ORIGIN'
weather$FL_DATE = as.Date(weather$FL_DATE)

weather_prev = weather
weather_prev$FL_DATE = weather_prev$FL_DATE + 1
colnames(weather_prev) = paste0(colnames(weather_prev),"_prev")
colnames(weather_prev)[colnames(weather_prev) == 'FL_DATE_prev'] <- 'FL_DATE'
colnames(weather_prev)[colnames(weather_prev) == 'ORIGIN_prev'] <- 'ORIGIN'

final_inter = merge(x = delays, y = weather, by = c('FL_DATE','ORIGIN'), all.x=T)
final = merge(x = final_inter, y = weather_prev, by = c('FL_DATE','ORIGIN'),all.x=T)

# model_data = final[which(final$DEP_DELAY >= 45 & final$ORIGIN == 'MSP' ),]
model_data = final[which(final$ORIGIN == 'MSP' & is.na(final$DEP_DELAY)==F),]


##TREATMENT OF Y VARIABLE
# model_data$delay_treated = ifelse(model_data$DEP_DELAY>= 330,330,model_data$DEP_DELAY)
model_data$delay2 = ifelse(model_data$DEP_DELAY < 0, 0 ,model_data$DEP_DELAY)
model_data$log_delay2 = log(model_data$delay2 + 1)

  
##HAS EVENTS?
model_data$has_events = ifelse(nchar(model_data$events)>0, 1,0)

##MAJOR/MINOR AIRLINES
model_data$airline_class = ifelse(model_data$AIRLINE_ID %in% c(19805,19790,19393,19977,19531,19930,
                                                          20229,21294,20416,20435,21351,19690,
                                                          20368,21171,21578,21226,19961,21342,
                                                          20422),
                                  1,0)

## log DISTANCE
model_data$ln_dist = log(model_data$DISTANCE)
##IS WEEKEND?
model_data$is_weekend = ifelse(model_data$DAY_OF_WEEK > 4, 1,0)


##HOUR OF DEPARTURE
model_data$dep_hr = model_data$CRS_DEP_TIME%/%100


#TIME OF DAY
model_data$time_of_day = ifelse(model_data$dep_hr %in% c(1,2,3,23,24), 'night',
                  ifelse(model_data$dep_hr %in% c(4:10), 'morning',
                  ifelse(model_data$dep_hr %in% c(11:15), 'afternoon','evening')))


##SEASONS
model_data$season = ifelse(model_data$MONTH %in% c(12,1,2),'Winter',
                    ifelse(model_data$MONTH %in% c(3,4,5),'Spring',
                    ifelse(model_data$MONTH %in% c(6,7,8,9),'Summer','Fall')))

#TREATING PRECIPITATION
model_data$prec_new = as.numeric(ifelse(model_data$precipitationin == 'T', 0,
                             model_data$precipitationin))
model_data$prec_new_prev = as.numeric(ifelse(model_data$precipitationin_prev   == 'T', 0,
                                        model_data$precipitationin_prev))

table(model_data$events)


model_msp = lm( log_delay2 ~
                          mean_temperaturef +
                          cloudcover +
                          min_sea_level_pressurein+
                          min_visibilitymiles +
                          mean_wind_speedmph + 
                          max_gust_speedmph + 
                          min_humidity + 
                          factor(events) +
                          factor(airline_class) +
                          relevel(factor(MONTH), ref = '6') +
                          ln_dist +
                          prec_new +
                          relevel(factor(time_of_day),ref = 'evening') +
                          factor(is_weekend)
                        , data = model_data)

summary(model_msp)

write.csv(model_msp$coefficients,'msp_coef.csv')

cor(model_data[,c(41,64)])


rstd_model = rstandard(model_msp)
rstd_sample = sample(rstd_model,size = 5000, replace = F)
shapiro.test(rstd_sample)

cor(model_data[,c(24,41,36,38,39,27,30,43,48,53)])

columns = data.frame(cbind(colnames(model_data),c(1:ncol(model_data))))