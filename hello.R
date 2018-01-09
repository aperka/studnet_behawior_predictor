#!/usr/bin/env Rscript

# matematyka
d1=read.table("student-mat.csv",sep=",",header=TRUE)
# portugalski
d2=read.table("student-por.csv",sep=",",header=TRUE)
# lista unikalnych studentów
d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
tail(d3, n=5)
