library(lubridate)
library(tibble)
library(aimsir17)
library(dplyr)
library(ggplot2)

#------------------------------------------------------------------------------------------------------------------------------------------
# Create you own copy
my_obs <- subset(observations,
                 station %in% c("DUBLIN AIRPORT","MACE HEAD","SherkinIsland","BELMULLET"))


my_obs %>% print(n=24)

#-----------------------------------------------------------------------------------------------------------------------------------------

get_daily_summary <- function(dayNo, stat, attribute, f){
  x<-subset(my_obs, DayNumber==dayNo & station == stat)
  f(x[,attribute,drop=T])
}


#------------------------------------------------------------------------------------------------------------------------------------------
my_obs$DayNumber <- yday(my_obs$date)

my_obs$season <- NA
my_obs$season[my_obs$month==11 | my_obs$month==12 | my_obs$month==1]<-"01_winter"
my_obs$season[my_obs$month==2 | my_obs$month==3 | my_obs$month==4]<-"02_spring"
my_obs$season[my_obs$month==5 | my_obs$month==6 | my_obs$month==7]<-"03_summer"
my_obs$season[my_obs$month==8 | my_obs$month==9 | my_obs$month==10]<-"04_autumn"

#------------------------------------------------------------------------------------------------------------------------------------------
# Create an aggregate tibble for the summaries                  
daily_obs <- tibble(
  DayNumber       = numeric(),
  Day             = numeric(),
  Month           = numeric(),
  Station         = character(),
  TotalRainfall   = numeric(),
  MaxRainfall     = numeric(),
  MinRainfall     = numeric(),
  MeanMSL         = numeric(),
  MaxMSL          = numeric(),
  MinMSL          = numeric(),
  MeanTemperature = numeric(),
  MaxTemperature  = numeric(),
  MinTemperature  = numeric(),
  MeanWindspeed   = numeric(),
  MaxWindspeed    = numeric(),
  MinWindspeed    = numeric(),
  Season          = character()
)


# Populate the new tibble
for (dayNumber in unique(my_obs$DayNumber)){
  cat("Processing Day Number",dayNumber,"\n")
  for(stations in unique(my_obs$station)){
    # (3) Add row of data here - all tibble columns must be added.
    daily_obs <- dplyr::add_row(daily_obs,
                                DayNumber= dayNumber,
                                Day= my_obs[which(my_obs$DayNumber==dayNumber),"day",drop=T][1],
                                Month=my_obs[which(my_obs$DayNumber==dayNumber),"month",drop=T][1],
                                Station=stations,
                                TotalRainfall=get_daily_summary(dayNumber, stations, "rain",sum),
                                MaxRainfall=get_daily_summary(dayNumber, stations, "rain",max),
                                MinRainfall=get_daily_summary(dayNumber, stations, "rain",min),
                                MeanMSL=get_daily_summary(dayNumber, stations, "msl",mean),
                                MaxMSL=get_daily_summary(dayNumber, stations, "msl",max),
                                MinMSL=get_daily_summary(dayNumber, stations, "msl",min),
                                MeanTemperature=get_daily_summary(dayNumber, stations, "temp",mean),
                                MaxTemperature=get_daily_summary(dayNumber, stations, "temp",max),
                                MinTemperature=get_daily_summary(dayNumber, stations, "temp",min),
                                MeanWindspeed=get_daily_summary(dayNumber, stations, "wdsp",mean),
                                MaxWindspeed=get_daily_summary(dayNumber, stations, "wdsp",max),
                                MinWindspeed=get_daily_summary(dayNumber, stations, "wdsp",min),
                                Season=my_obs[which(my_obs$DayNumber==dayNumber),"season",drop=T][1]
                                ) 
  }
}

nrow(my_obs)/nrow(daily_obs)

#------------------------------------------------------------------------------------------------------------------------------------------

# (4) Check tibble against this one...
test <- my_obs %>% group_by(DayNumber,station) %>% 
  summarise(TotalRainfall=sum(rain),
            MaxRainfall=max(rain),
            MinRainfall=min(rain),
            MeanMSL=mean(msl),
            MaxMSL=max(msl),
            MinMSL=min(msl),
            MeanTemperature = mean(temp),
            MaxTemperature  = max(temp),
            MinTemperature  = min(temp),
            MeanWindspeed   = mean(wdsp),
            MaxWindspeed    = max(wdsp),
            MinWindspeed    = min(wdsp),
  )

#------------------------------------------------------------------------------------------------------------------------------------------------

# (5) Add all plots

#Plot1
ggplot(data=daily_obs,aes(x=Day,y=MeanTemperature,colour=Station))+geom_line()+facet_grid(Month~Station)+
  ggtitle("Plot 1: Annual Mean Temperatures for each Weather Station")+labs(y="Mean Temperature", x = "Day of Month")



#Plot2
ggplot(data=daily_obs,aes(x=MeanMSL,y=TotalRainfall,colour=Station))+geom_point()+geom_smooth()+facet_grid(Season~Station)+
  ggtitle("Plot 2: Mean Sea Level Pressure v Total Rainfall by Season and Station")+labs(y="Total Rainfall", x = "Mean Sea Level Pressure")


#Plot3
ggplot(data=daily_obs,aes(x=DayNumber,y=MeanTemperature,colour=Season))+geom_point()+facet_wrap(~Station)+
  ggtitle("Plot 3: Mean Daily Temperature by station and season")+labs(y="Mean Daily Temperature", x = "Day of the Year")


#Plot4
ggplot(data=daily_obs,aes(x=Season,y=MeanTemperature,colour=Season))+geom_boxplot()+facet_wrap(~Station)+
  ggtitle("Plot 4: Mean Daily Temperature Distribution by station and season")+labs(y="Mean Daily Temperature", x = "Season")


#Plot5
ggplot(data=daily_obs,aes(x=Season,y=MaxTemperature,colour=Season))+geom_boxplot()+facet_wrap(~Station)+
  ggtitle("Plot 5: Max Daily Temperature Distribution by station and season")+labs(y="Max Daily Temperature", x = "Season")


#Plot6
ggplot(data=daily_obs,aes(x=Season,y=MinTemperature,colour=Season))+geom_boxplot()+facet_wrap(~Station)+
  ggtitle("Plot 6: Min Daily Temperature Distribution by station and season")+labs(y="Min Daily Temperature", x = "Season")

#Plot7
ggplot(data=daily_obs,aes(x=Season,y=TotalRainfall,colour=Season))+geom_boxplot()+facet_wrap(~Station)+
  ggtitle("Plot 7: Total Rainfall Distribution by station and season")+labs(y="Total Rainfall", x = "Season")


#Plot8
ggplot(data=daily_obs,aes(x=Station,y=MaxWindspeed,colour=Station))+geom_boxplot()+facet_grid(.~as.factor(Month))+
  theme(axis.text.x=element_text(angle=90))+
  ggtitle("Plot 8: Max Daily Windspeed by Month")+labs(y="Maximum Wind Speed", x = "Stations")


#Plot9
ggplot(data=daily_obs,aes(x=Station,y=MeanMSL,colour=Station))+geom_boxplot()+facet_grid(.~as.factor(Month))+
  theme(axis.text.x=element_text(angle=90))+
  ggtitle("Plot 9: Mean Sea Level Pressure per month")+labs(y="Mean Sea Level Pressure", x = "Stations")


#Plot10
ggplot(data=daily_obs,aes(x=MeanMSL,y=MaxWindspeed,colour=Station))+geom_point()+geom_smooth()+facet_grid(Station~Season)+
  ggtitle("Plot 10: Mean Sea Level v Maximum Windspeed")+labs(y="Maximum WindSpeed", x = "Mean Sea Level Pressure")


#Plot11
ggplot(data=daily_obs,aes(x=MaxWindspeed,y=TotalRainfall,colour=Station))+geom_point()+geom_smooth()+facet_grid(Station~Season)+
  ggtitle("Plot 11: Max Windspeed v Total Rainfall")+labs(y="Total Rainfall", x = "Maximum Wind Speed")


#Plot12
ggplot(data=daily_obs)+
  geom_point(aes(y=MaxTemperature,x=DayNumber),colour="blue")+
  geom_point(aes(y=MeanTemperature,x=DayNumber),colour="red")+
  ggtitle("Plot 12: Variation between MaxTemperature and MeanTemperature throughout the year")+
  labs(y="MaxTemperature and MeanTemperature", x = "Day of the Year")







