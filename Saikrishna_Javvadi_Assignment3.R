set.seed(100)
data<-as.data.frame(ggplot2::mpg)
#-------------------------------------------------------------------------------------------------------------------------------
rows_sample<-sample(nrow(data), 10)
rows_sample

data[rows_sample,"cty"]<--999

#-------------------------------------------------------------------------------------------------------------------------------

replace_negatives<-function(x)
{
  ifelse(x<0,NA,x)
}

data<-data.frame(lapply(data,replace_negatives))

#-------------------------------------------------------------------------------------------------------------------------------

mean_by_class <-  sapply(unique(as.character(data$class)), 
                 function(x) { mean(data[data$class == x, 'cty'],na.rm = T) })



for (i in rows_sample)
{
  if(is.na(data$cty[i]))
     {
       data$cty[i]<- mean_by_class[[data$class[i]]] 
  }
}  

#Alternate method
#data$cty <- with(data, ave(cty, class,
#                          FUN = function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))))
#-------------------------------------------------------------------------------------------------------------------------------

summary(data)

#-------------------------------------------------------------------------------------------------------------------------------
mean(mpg$cty)
mean(data$cty)



