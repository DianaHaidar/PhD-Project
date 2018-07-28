library(data.table)
library(dplyr)

fp <- file.path('C:/Users/ID916780/Dropbox/PhD - My folder/RStudio/oneclass.test/10 fold cross validation',
                'wilcoxontest.csv')
data <- fread(input = fp, showProgress = FALSE)
summary(data)

library(MASS)         # load the MASS package 
wilcox.test(data$Y1, data$Y2, paired=TRUE) 
