library(dplyr)
library(ggplot2)
library(aimsir17)

#----------------------------------------------------------------------------------------------------------------------------------
target <- c("MACE HEAD","JOHNSTOWNII","ROCHES POINT")


weather<- filter(aimsir17::observations,station==target) %>%
          group_by(station,month,day) %>% 
          summarise(AvrMSL = mean(msl)) %>%
          mutate(Quarter = case_when(month %in% c(1,2,3) ~ "Q1",
                             month %in% c(4,5,6) ~ "Q2" , 
                             month %in% c(7,8,9) ~ "Q3",
                             month %in% c(10,11,12) ~ "Q4")) %>%
          select(station,month,day,AvrMSL,Quarter)

weather


#----------------------------------------------------------------------------------------------------------------------------------

power <- aimsir17::eirgrid17 %>% 
         group_by(month,day) %>% 
         summarise(MeanWindPower = mean(IEWindGeneration)) %>%
         select(month,day,MeanWindPower)
      
power
#----------------------------------------------------------------------------------------------------------------------------------

j_data <- left_join(power,weather,by = c("month", "day"))

j_data
#----------------------------------------------------------------------------------------------------------------------------------

#Plot1
ggplot(data=j_data,aes(x=AvrMSL,y=MeanWindPower))+geom_point()+geom_smooth()

#Plot2
ggplot(data=j_data,aes(x=AvrMSL,y=MeanWindPower,color=Quarter))+geom_point()+geom_smooth()

#Plot3
ggplot(data=j_data,aes(x=AvrMSL,y=MeanWindPower,color=station))+geom_point()+geom_smooth()

#Plot4
ggplot(data=j_data,aes(x=AvrMSL,y=MeanWindPower))+geom_point()+geom_smooth()+facet_grid(station~Quarter)

