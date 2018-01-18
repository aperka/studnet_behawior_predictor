#!/usr/bin/env Rscript

library(caret)

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
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"), suffix = c("_Mat", "_Por"))
d3$Dalc = pmax(d3$Dalc_Mat,d3$Dalc_Por)
d3$Dalc_Mat = NULL
d3$Dalc_Por = NULL
d3$Walc = pmax(d3$Walc_Mat,d3$Walc_Por)
d3$Walc_Mat = NULL
d3$Walc_Por = NULL

png(file = "dalc_hist.png", width = 8, height = 8, units = 'in', res = 400)
hist(d3$Dalc, breaks=c(1,2,3,4,5), col="blue")
dev.off()

png(file = "walc_hist.png", width = 8, height = 8, units = 'in', res = 400)
hist(d3$Walc, breaks=c(1,2,3,4,5), col="blue")
dev.off()

# selecting fatures
selected_fatures <- d3[,c("Dalc", "Walc")]# , "absences.y")]
#selected_fatures <- d2[,c("Dalc", "Walc")] #, "absences")]
selected_fatures <- na.omit(selected_fatures) # listwise deletion of missing
#selected_fatures <- scale(selected_fatures) # standardize variables]

grouped <- data.frame(selected_fatures, group_id=group(selected_fatures, 2))
png(file = "grouping.png", width = 8, height = 8, units = 'in', res = 400)
plot(grouped$Dalc, grouped$Walc, col=grouped$group_id)
dev.off()
#library(rgl)
#plot3d(mydata$Dalc, mydata$Walc, mydata$absences, col=mydata$fit.cluster)

#############
### Bayes ###
#############
students = d3
students$Dalc = NULL
students$Walc = NULL
students$Labels = as.factor(grouped$group_id)

print("Badanie wpływu zmiany wartości K dla walidacji skrośnej na dokładność klasyfikacji")
results = data.frame(matrix(ncol = 2, nrow = 0))
colnames(results) = c("k_param", "accuracy")
for (k in c(2, 5, 10, 20, 40, 60, 80, 100)){
    print(paste0("K = ", k))
    train_control = trainControl(method="repeatedcv", number=k, repeats = 50)
    grid = data.frame(fL=0, usekernel = TRUE, adjust=1)
    model = train(Labels~., data=students, trControl=train_control, tuneGrid=grid, method="nb")
    newRow = data.frame(k_param=k , accuracy=model["results"][[1]]$Accuracy)
    results = rbind(results,newRow)
}
png(file = "k_accuracy.png", width = 8, height = 8, units = 'in', res = 500)
plot(results$k_param, results$accuracy, type="o", xlab="Wartosc K", ylab="Dokladnosc")
dev.off()

print("Badanie wpływu ograniczania zestawu cech na dokładność klasyfikacji")
results = data.frame(matrix(ncol = 2, nrow = 0))
colnames(results) = c("feature", "accuracy")
n = ncol(students) - 1;
for(itt in 1:n) {
    print(paste0("Rejecting: ", colnames(students)[itt]))
    train_control = trainControl(method="repeatedcv", number=10, repeats=10)
    reducedStudents = students
    reducedStudents[itt] = NULL
    grid = data.frame(fL=0, usekernel = TRUE, adjust=1)
    model = train(Labels~., data=reducedStudents, trControl=train_control, tuneGrid=grid, method="nb")
    newRow = data.frame(feature=colnames(students)[itt] , accuracy=model["results"][[1]]$Accuracy)
    results = rbind(results,newRow)
}
png(file = "reject_accuracy.png", width = 8, height = 8, units = 'in', res = 600)
plot(results$feature, results$accuracy, type="o", xlab="", xaxt="n", ylab="Dokladnosc")
axis(1, at=1:n, labels=results$feature, las = 2, cex.axis = 0.66)
dev.off()


