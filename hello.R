#!/usr/bin/env Rscript

#grouping function
group <- function(dataset, k){
  # K-Means Cluster Analysis
  fit <- kmeans(dataset, 2, algorithm="MacQueen") # 5 cluster solution
  # get cluster means
  aggregate(dataset, by=list(fit$cluster),FUN=mean)
  # append cluster assignment
  grouped <- data.frame(dataset, fit$cluster)
  return(fit$cluster)
}

# matematyka
d1=read.table("student-mat.csv",sep=",",header=TRUE)
# portugalski
d2=read.table("student-por.csv",sep=",",header=TRUE)
# lista unikalnych studentów
d3=merge(d1,d2, by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"), all = TRUE)


# selecting fatures
selected_fatures <- d3[,c("Dalc.y", "Walc.y")]# , "absences.y")]
#selected_fatures <- d2[,c("Dalc", "Walc")] #, "absences")]
selected_fatures <- na.omit(selected_fatures) # listwise deletion of missing
#selected_fatures <- scale(selected_fatures) # standardize variables]

grouped <- data.frame(selected_fatures, group_id=group(selected_fatures, 2))

plot(grouped$Dalc, grouped$Walc, col=grouped$group_id)
#library(rgl)
#plot3d(mydata$Dalc, mydata$Walc, mydata$absences, col=mydata$fit.cluster)
