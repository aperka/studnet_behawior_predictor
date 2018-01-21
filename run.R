#!/usr/bin/env Rscript

library(caret)

#grouping function
group <- function(dataset, k){
  # K-Means Cluster Analysis
  fit <- kmeans(dataset, 2, algorithm="MacQueen") # k cluster solution
  # get cluster means
  aggregate(dataset, by=list(fit$cluster),FUN=mean)
  # append cluster assignment
  grouped <- data.frame(dataset, fit$cluster)
  return(fit$cluster)
}

dist_group <- function(dataset, k, dist_method, clust_method, save_path){
  # Ward Hierarchical Clustering
  d <- dist(dataset, method = dist_method) # distance matrix
  fit <- hclust(d, method=clust_method)
  png(file = paste(save_path ,clust_method,"_",dist_method,".png", sep=""), width = 80, height = 8, units = 'in', res = 2000)
  plot(fit) # display dendogram
  groups <- cutree(fit, k=k) # cut tree into k clusters
  # draw dendogram with red borders around the k clusters 
  rect.hclust(fit, k=k, border="red")
  dev.off()
  return(groups)
}

# matematyka
d1=read.table("datasets/student-mat.csv",sep=",",header=TRUE)
# portugalski
d2=read.table("datasets/student-por.csv",sep=",",header=TRUE)
# lista unikalnych studentów
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"), suffix = c("_Mat", "_Por"))
d3$Dalc = pmax(d3$Dalc_Mat,d3$Dalc_Por)
d3$Dalc_Mat = NULL
d3$Dalc_Por = NULL
d3$Walc = pmax(d3$Walc_Mat,d3$Walc_Por)
d3$Walc_Mat = NULL
d3$Walc_Por = NULL

for (subject in c("mat", "por")){
  d3 = read.table(paste("datasets/student-",subject ,".csv", sep=""), sep=",",header=TRUE)
  
  png(file = paste(subject,"/histograms/dalc_hist.png", sep=""), width = 8, height = 8, units = 'in', res = 400)
  hist(d3$Dalc, breaks=c(1,2,3,4,5), col="blue")
  dev.off()
  
  png(file =paste(subject,"/histograms/walc_hist.png", sep=""), width = 8, height = 8, units = 'in', res = 400)
  hist(d3$Walc, breaks=c(1,2,3,4,5), col="blue")
  dev.off()
  
  # selecting fatures
  selected_fatures <- d3[,c("Dalc", "Walc")]# , "absences.y")]
  selected_fatures <- na.omit(selected_fatures) # listwise deletion of missing
  
  for (dist_methode in c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")){
    for(clust_method in c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid")){
      groupng_dendograms = paste(subject, "/grouping_plots/dendograms/",sep="")
      grouped <- data.frame(selected_fatures, group_id=dist_group(selected_fatures, 2, dist_methode, clust_method, groupng_dendograms))
      write.csv(grouped, file = paste(subject,"/csv/",clust_method,"_",dist_methode,".csv", sep=""))
      png(file = paste(subject,"/grouping_plots/grouping/",clust_method,"_",dist_methode,".png", sep=""), width = 16, height = 8, units = 'in', res = 400)

      par(mfrow=c(1,2), oma=c(0,0,2,0))
      plot(grouped$Dalc, grouped$Walc, col=grouped$group_id, pch=grouped$group_id)
      hist(grouped$group_id, breaks=c(0,1,2), col=c("gray", "red"))
      title(paste("Dist methode:", dist_methode, "\nHclust methode:", clust_method), outer=TRUE)
      #plot(grouped$Dalc, grouped$Walc, col=grouped$group_id+1, pch=grouped$group_id)
      dev.off()
      png(file = paste(subject,"/grouping_plots/histograms/",clust_method,"_",dist_methode,".png", sep=""), width = 8, height = 8, units = 'in', res = 400)
      hist(grouped$group_id, breaks=c(0,1,2), col=grouped$group_id)
      dev.off()
    }
  }
}



#library(rgl)
#plot3d(mydata$Dalc, mydata$Walc, mydata$absences, col=mydata$fit.cluster)

#############
### Bayes ###
#############
grouped = read.table("csv/ward.D_manhattan.csv",sep=",",header=TRUE)
students = d3
students$Dalc = NULL
students$Walc = NULL
students$Label = as.factor(grouped$group_id)

png(file = "histograms/grouping_hist.png", width = 8, height = 8, units = 'in', res = 400)
hist(grouped$group_id, col="blue")
dev.off()

print("Badanie wpływu zmiany wartości K dla walidacji skrośnej na dokładność klasyfikacji")
results = data.frame(matrix(ncol = 2, nrow = 0))
colnames(results) = c("k_param", "accuracy")
for (k in c(2, 5, 10, 20, 40, 60, 80, 100)){
    print(paste0("K = ", k))
    train_control = trainControl(method="repeatedcv", number=k, repeats = 50)
    grid = data.frame(fL=0, usekernel = TRUE, adjust=1)
    model = train(Label~., data=students, trControl=train_control, tuneGrid=grid, method="nb")
    newRow = data.frame(k_param=k , accuracy=model["results"][[1]]$Accuracy)
    results = rbind(results,newRow)
}
png(file = "bayes_plots/k_accuracy.png", width = 8, height = 8, units = 'in', res = 500)
plot(results$k_param, results$accuracy, type="o", xlab="Wartosc K", ylab="Dokladnosc")
dev.off()

print("Badanie wpływu ograniczania zestawu cech na dokładność klasyfikacji")
results = data.frame(matrix(ncol = 2, nrow = 0))
for(itt in 1:31) {
    print(paste0("Rejecting: ", gsub("_Mat", "", colnames(students)[itt])))
    train_control = trainControl(method="repeatedcv", number=20, repeats=10)
    reducedStudents = students
    if (itt < 14) {
        reducedStudents[itt] = NULL
    } else {
        reducedStudents[itt + 18] = NULL
        reducedStudents[itt] = NULL
    }
    grid = data.frame(fL=0, usekernel = TRUE, adjust=1)
    model = train(Label~., data=reducedStudents, trControl=train_control, tuneGrid=grid, method="nb")
    newRow = data.frame(feature=gsub("_Mat", "", colnames(students)[itt]), accuracy=model["results"][[1]]$Accuracy)
    results = rbind(results,newRow)
}
png(file = "bayes_plots/reject_accuracy.png", width = 8, height = 8, units = 'in', res = 600)
plot(results$feature, results$accuracy, type="o", xlab="", xaxt="n", ylab="Dokladnosc")
axis(1, at=1:31, labels=results$feature, las = 2, cex.axis = 0.75)
dev.off()


