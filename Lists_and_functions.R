

mod <- lm(eruptions ~ waiting, data=faithful)

#----------------------------------------------------------------------------------------------------------------------------

#Function 1: get_data()
get_data<- function(a,x)
{
  a$model[[x]]
}

d <- get_data(mod,"waiting")
d[1:20]

#----------------------------------------------------------------------------------------------------------------------------

#Function 2: get_coefficient()
get_coefficient<-function(a,x)
{
  a$coefficients[x]
}

c <- get_coefficient(mod,"waiting")
c

#----------------------------------------------------------------------------------------------------------------------------

#Function 3: get_residuals()
get_residuals<-function(a)
{
  list("data" = a$residuals, "mean" = mean(a$residuals), "number" =length(mod$residuals))
}

r <- get_residuals(mod)
str(r)


#----------------------------------------------------------------------------------------------------------------------------

#Function 4: mod_summary()
mod_summary<-function(a)
{
  list("call"=summary(a)$call , "coeff"=summary(a)$coefficients[,1],"datasize"=list("rows"=nrow(a$model),"cols"=ncol(a$model)))
}

s <- mod_summary(mod)
s

