# library(gridExtra) 
# library(grid)
# library(gridBase)
# library(ggplot2)
# library(RColorBrewer)
library(tidyverse)

gradeBooktest = read_csv("gradeBookCSV_WPR3.csv", stringsAsFactors = F)

admindata = gradeBooktest[-c(1:3),c(1:10)]
names(admindata) = as.character(unlist(gradeBooktest[3,c(1:10)]))

data = gradeBooktest[-(1:3),-c(1:10)]
names(data) = c(as.character(unlist(gradeBooktest[2,c(11:34)])), 
                as.character(unlist(gradeBooktest[1,-c(1:34)])))

datatots = cbind(admindata,data)

#string of max points for each assignment
maxpts=gradeBooktest[3,]
maxpts[,c(1:10)]=0
maxpts[,-c(1:10)]=as.numeric(as.character(maxpts[,-c(1:10)]))

