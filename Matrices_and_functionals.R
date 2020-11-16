#Name : Saikrishna Javvadi
#Student-ID: 20236648

---------------------------------------------------------------------------------------------------------------------------------------

set.seed(100)
CX101 <- rnorm(25,45,8)
CX102 <- rnorm(25,65,8)
CX103 <- rnorm(25,85,10)
CX104 <- rnorm(25,45,10)
CX105 <- rnorm(25,60,5)
CX106 <- rnorm(25,30,20)
CX107 <- rnorm(25,50,10)
CX108 <- rnorm(25,90,5)

#creating the matrix
res<-matrix(c(CX101,CX102,CX103,CX104,CX105,CX106,CX107,CX108), ncol=8)

#Naming the rows and columns of the matrix
rownames(res)<-c("Student_1","Student_2","Student_3","Student_4","Student_5","Student_6","Student_7","Student_8","Student_9"
               ,"Student_10","Student_11","Student_12","Student_13","Student_14","Student_15","Student_16","Student_17","Student_18"
               ,"Student_19","Student_20","Student_21","Student_22","Student_23","Student_24","Student_25")

colnames(res)<-c("CX101","CX102","CX103","CX104","CX105","CX106","CX107","CX108")

summary(res)

res[c(1,2,3,25),]

res[res[,"CX103"] > 100,]

---------------------------------------------------------------------------------------------------------------------------------------
#Replacing the values of all outliers(i.e <0 and >100 values) of all columns to NA
replace_by_na<- function(x)
{
  ifelse(x<0 | x>100,NA,x)
}

res<-apply(res, 2 , replace_by_na)

----------------------------------------------------------------------------------------------------------------------------------------
#Replacing the NA values by mean of that column for all the columns
replace_by_mean<- function(x)
{
  ifelse(is.na(x),mean(x,na.rm=TRUE),x)
}
 
res<- apply(res,2,replace_by_mean)

----------------------------------------------------------------------------------------------------------------------------------------
#Calculating the mean and range of marks for each student(i.e row) and binding these to new columns into the matrix
#2 ways of doing this is demonstrated below  
res<-cbind(res,Mean=rowMeans(res),Range=apply(res,1,function(x){diff( range(x))}))

#OR

find_range<-function(x)
{
  max(x)-min(x)
}
res<-cbind(res,Mean=rowMeans(res),Range=apply(res,1,find_range))

----------------------------------------------------------------------------------------------------------------------------------------
#Find the Student with the highest average and displaying the entire row with student number
highest<-res[which(res[,c("Mean")] == max(res[,c("Mean")])),,drop=F]
highest


