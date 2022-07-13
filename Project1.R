# Title/purpose of the assignment: Project1.R
# Author: Zoey Le
# Created on: 1/4/2022
# purpose: project 1

#load the dataset
#import library

install.packages('randomcoloR')
library(randomcoloR)
library(datasets)

#Question1:
  #a. How many unique 'Subject' are there in the data-frame:
  #based on the description of the data, there are 12 subjects
  df <- Theoph
  unique_subject <- length(unique(df$Subject))
  print(unique_subject)
  
  #b. What is the number of total records (rows):
  #based on the description of the data, there are 132 rows
  num_row <- nrow(df)
  print(num_row)
  
  #c.How many records are there for each subject:
  subject <- c(1:12)
  num = c()
  for (i in c(1:12)) {
    num <- c(num, sum(df$Subject == i))
  }
  data.frame(subject)
  data.frame(num)
  count_table <- cbind(subject, num)
  print(count_table)
  
  #d.List the unique weights (Wt) in the dataframe
  uniq_wt <- unique(df$Wt)
  print(uniq_wt)
  
  #e.List the unique dosage(Dose) in the dataframe
  uniq_dose <- unique(df$Dose)
  print(uniq_dose)
  
#Question2:
  #Normalize the column (conc):
    mean_dose = mean(uniq_dose)
    norm_conc <- (mean_dose/df$Dose)*df$conc
    data.frame(norm_conc)
    myTheoph <- cbind(df, norm_conc)
    
#Question3:
    #calculate the mean of time point
    c1 <- myTheoph$norm_conc[myTheoph$Subject == 1]
    tabletest <- data.frame(c1)
    tabletest$c2 <- myTheoph$norm_conc[myTheoph$Subject == 2]
    tabletest$c3 <- myTheoph$norm_conc[myTheoph$Subject == 3]
    tabletest$c4 <- myTheoph$norm_conc[myTheoph$Subject == 4]
    tabletest$c5 <- myTheoph$norm_conc[myTheoph$Subject == 5]
    tabletest$c6 <- myTheoph$norm_conc[myTheoph$Subject == 6]
    tabletest$c7 <- myTheoph$norm_conc[myTheoph$Subject == 7]
    tabletest$c8 <- myTheoph$norm_conc[myTheoph$Subject == 8]
    tabletest$c9 <- myTheoph$norm_conc[myTheoph$Subject == 9]
    tabletest$c10 <- myTheoph$norm_conc[myTheoph$Subject == 10]
    tabletest$c11 <- myTheoph$norm_conc[myTheoph$Subject == 11]
    tabletest$c12 <- myTheoph$norm_conc[myTheoph$Subject == 12]
    
    for (i in range(1:length(tabletest))){
      tabletest$mean <- rowMeans(tabletest, na.rm=TRUE)
    }
    
    #create function to graph
    
    PlotBarPlot <- function (x, y, type)
    {
      n <- length(x)
      palette <- distinctColorPalette(n)
      barplot(height = y, width = 3, space = 1, names.arg = x,
              col = palette, axes = TRUE,
              main = paste("distribution of dose conc. using ", type,
                           sep = ""), xlab = "Time points", ylab = "Dose conc.")
    }
    
    #draw the graph
    pdf('plot.pdf')
    PlotBarPlot(y = tabletest$mean, x = c(1:11), type = 'mean')
    dev.off()
    
#Question4
    
    x1 <- mean(df$Wt[df$Wt <=55])
    x2 <- mean(df$Wt[55< df$Wt & df$Wt<=65])
    x3 <- mean(df$Wt[65< df$Wt & df$Wt <=75])
    x4 <- mean(df$Wt[df$Wt>75])
    #Create Table
    Weight_range <- c('<=55','>55 and <=65','>65 and <=75','>75')
    Table1 <- data.frame(Weight_range)
    Table1$Mean_of_dose <- c(x1,x2,x3,x4)
    print(Table1)
    
    #--> there is an increase in the average number of doses as the weight increases
    
#5. It takes me around a week to finish the project. This a great way to practice.
    #I know there should be more easier ways to handle some tasks (particularly q3), 
    # but I decided to work around and challenge myself. Looking forward to see the solutions.

    
  