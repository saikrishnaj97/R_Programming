#loading the libraries
install.packages("ggpubr")
library(aimsir17)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggpubr)

#-------------------------------------------------------------------------------------------------------------------------------------
#Generating the daily tibble

target <- c("BELMULLET", "MACE HEAD","NEWPORT" ,"ROCHES POINT", "SherkinIsland","VALENTIA OBSERVATORY")  #target vector containing the required stations

daily<- aimsir17::observations %>% 
  filter(station %in% target)  %>%    #filtering the observations only from target stations
  group_by(station,month,day) %>%     #grouping by station,month,day
  summarise(MeanMSL=mean(msl,na.rm = T),MaxWind = max(wdsp,na.rm=T)) %>%    #summarizing to get Mean MSL and MaxWind Speed for each day
  select(station,month,day,MeanMSL,MaxWind)    #selecting only the required columns to display  

daily     

unique(daily$station)    #printing the unique stations in the tibble
      
#-------------------------------------------------------------------------------------------------------------------------------------
#Plotting Maximum Daily Wind Speed vs Mean Daily Sea level Pressure for different stations

ggplot(data=daily,aes(x=MeanMSL,y=MaxWind))+geom_point()+geom_smooth(method="lm")+facet_wrap(~station)+
  labs(y="Maximum Daily Wind Speed", x = "Mean Daily Sea level Pressure")

#-------------------------------------------------------------------------------------------------------------------------------------
#Generating the models tibble which contains the data for each station and the results of the linear model

models<- daily %>%
  group_by(station) %>%          #grouping by station
  nest()    %>%                  #nesting the tibble to split into separate data frames based on a grouping variable station
  mutate(LM = map(data, ~lm(MaxWind~MeanMSL,data = .)))   #mutating the tibble to add new column LM

models

#-------------------------------------------------------------------------------------------------------------------------------------

results <- daily %>% 
  group_by(station) %>%   #grouping by station
  group_split() %>%       #group splitting the tibble
  map_df(~{               #using map_df to map the output as a dataframe/tibble
    mod <- lm (.$MaxWind~.$MeanMSL)       #creating a regression model for each split group 
    summ <- summary(mod)                  #summary of regression model 
    tibble(station=first(.$station),      #populating the tibble with station name,Intercept,Slope,RSquared value and Adjusted RSquared value for each station
           Intercept=mod$coefficients[1], 
           Slope=mod$coefficients[2],
           RSqr=summ$r.squared, 
           AdjRqr=summ$adj.r.squared)
  }) %>%
  arrange(desc(RSqr))  #arranging the tibble in descending order of RSquared values

results

#-------------------------------------------------------------------------------------------------------------------------------------

all <- left_join(models,results,by="station")    #left_joining model and results tibbles by the primary key-station

all

#-------------------------------------------------------------------------------------------------------------------------------------

all_plots <-  all %>%
  select(station,data,Intercept,Slope,RSqr,AdjRqr) %>%                                                   #selecting only the required columns from the all tibble
  pmap(~{                                                                                                #using pmap to create a list of plots
    title=paste(..1,"[Beta1=",round(..4,3),"][Beta0=",round(..3,3),"][RSqr=",round(..5,3),"]")           #creating the title that has to be added to each plot 
    ggplot(..2, aes(x=MeanMSL,y=MaxWind)) + geom_point()+geom_smooth(method="lm")+                       #plotting the data 
    geom_abline(slope=..4,intercept=..3,size=3.0,colour="red",alpha=0.5)+                                #using geom_abline() to draw the red line
    ggtitle(title)+                                                                                      #adding the title to the plot
    theme(axis.title=element_text(face="bold", size = 10),plot.title = element_text(face = "italic",size = 10))+    #using theme() in ggplot2 to control the appearance/font size of the labels    
    labs(y="Maximum Daily Wind Speed", x = "Mean Daily Sea Level Pressure")})                            #labelling the axes

ggarrange(plotlist = all_plots,nrow = 3,ncol = 2)                                                       #Displaying the plots in 3 rows and 2 columns
  

  


