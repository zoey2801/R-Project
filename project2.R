#Project 2
# Author: Zoey Le
# Created on: 4/23/2022

#Load data
if(!exists('iris')){library('datasets')}
head(iris)
iris
#Question1:
  #a. How many unique 'Species' in the data-frame
  length(unique(iris$Species)) #3
  #b. Total number of records (# rows)
  nrow(iris) #150
  #c. Number of records per species
  length(which(iris$Species == 'setosa')) #50
  length(which(iris$Species == 'versicolor')) #50
  length(which(iris$Species == 'virginica')) #50
  #d. How many attributes in the dataset?
  ncol(iris) #5
  #e. Provide summary
  tapply(iris$Sepal.Length, iris$Species,summary)
  tapply(iris$Sepal.Width, iris$Species,summary)
  tapply(iris$Petal.Length, iris$Species,summary)
  tapply(iris$Petal.Width, iris$Species,summary)
  
  #median
  tapply(iris$Sepal.Length,iris$Species,median)
  tapply(iris$Sepal.Width,iris$Species,median)
  tapply(iris$Petal.Length,iris$Species,median)
  tapply(iris$Petal.Width,iris$Species,median)
  
  #min values for all species combined
  apply(iris[1:4],2,min)
  #max values for all species combined
  apply(iris[1:4],2,max)
  #mean values for all species combined
  apply(iris[1:4],2,mean)
  #median values for all species combined
  apply(iris[1:4],2,median)
  
#Question 2:
  #a.Relationship between Petal.Length and Petal.Width
  cor(iris$Petal.Length, iris$Petal.Width)
    #0.9628 --> There is a strong positive relationship between 2 variables
  #b. Relationship between Sepal.Length and Sepal.Width
  cor(iris$Sepal.Length,iris$Sepal.Width)
    #-0.1175 --> Negative relationship, yet this value closer to 0 which indicates that these 2 variables don't have a strong relationship

#Question 3:
  iris$ratio_Sepal <- iris$Sepal.Width/iris$Sepal.Length
  iris$ratio_Petal <- iris$Petal.Width/iris$Petal.Length
  pdf('width_length_relationship.pdf')
  boxplot(iris$ratio_Petal ~ iris$Species,xlab = 'Iris',ylab = 'Width/Length',names = c('Setosa','Virginca','Versicolor'),main = 'Iris Species Petal and Sepal Width/Length Relationship', xlim = range(-1:10),ylim=(0.0:1.0),col=c('Purple','Cyan','Red'))
  boxplot(iris$ratio_Petal, xlab = 'Petal combined',ylab='Width/Length', at=1:1-1,add = TRUE)
  boxplot(iris$ratio_Sepal, xlab = 'Sepal combined',ylab='Width/Length', at=1:1+5,add = TRUE)
  boxplot(iris$ratio_Sepal ~ iris$Species,xlab = 'Iris',ylab = 'Width/Length',names = c('Setosa','Virginca','Versicolor'), xlim = range(-1:20),ylim=(0.0:1.0),col=c('Purple','Cyan','Red'),at=1:3+6,add=TRUE)
  dev.off()
  
#4. Feedback: I spent around 2 days to complete the project. 
  # The project is comprehensive because we must flexibly apply all the functions we have learned throughout the class.
  #It is a fun project.
  
  
  