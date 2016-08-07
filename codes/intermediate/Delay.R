weathermsp<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\weather.csv")
weathermsp$FL_DATE<- format(as.Date(weathermsp$CST), "%Y-%m-%d")

weatherdallas<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\dfwweather.csv")
weatherdallas$FL_DATE<- format(as.Date(weatherdallas$CST), "%Y-%m-%d")

jan<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\jan.csv")
feb<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\feb.csv")
mar<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\mar.csv")
apr<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\apr.csv")
may<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\may.csv")
june<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\june.csv")
july<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\july.csv")
aug<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\aug.csv")
sep<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\sep.csv")
oct<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\oct.csv")
nov<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\nov.csv")
dec<- read.csv("C:\\Users\\Hamsika\\Desktop\\Statistics\\project\\dec.csv")

delay<- rbind(jan,feb,mar,apr,may,june,july,aug,sep,oct,nov,dec)
msp<-delay[delay$ORIGIN_CITY_NAME=='Minneapolis, MN'& delay$ARR_DELAY>=10,]
dallas<-delay[delay$ORIGIN_CITY_NAME=='Dallas, TX'& delay$ARR_DELAY>=10,]

msp<-within(msp, rm('X'))
msp<-msp[complete.cases(msp),] #Removing NA


dallas<-dallas[complete.cases(dallas),] #Removing NA

delay_msp<-merge(msp, weathermsp, by = "FL_DATE", sort = FALSE)
delay_dallas<-merge(dallas, weatherdallas, by = "FL_DATE", sort = FALSE)

attach(delay_dallas)

#distance
linefit_dis<- lm(ARR_DELAY ~ DISTANCE)  
summary(linefit_dis)   

#temp
linefit_temp<- lm(ARR_DELAY ~ Mean.TemperatureF)  
summary(linefit_temp)  

linefit<- lm(ARR_DELAY ~ DISTANCE + Mean.TemperatureF + Mean.Humidity + Mean.VisibilityMiles+ Mean.Wind.SpeedMPH+ MeanDew.PointF+ WindDirDegrees)  
summary(linefit)  

#visibility
linefit_vis<- lm(ARR_DELAY ~ Mean.VisibilityMiles) 
summary(linefit_vis)
