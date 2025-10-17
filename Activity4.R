#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
install.packages("tidyverse")
install.packages("ggpot2")
library(tidyverse)
library(ggplot2)
#dply::filter(...) - use when conflict
#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

#subset from flower dataset
versicolor <- iris[iris$Species == "versicolor",]

#create empty list
reg <- list()

#create vector of formulas
var <- list(iris$Sepal.Length ~ iris$Sepal.Width, 
                iris$Petal.Length ~ iris$Petal.Width,
                iris$Sepal.Width ~ iris$Petal.Length) 

#create for loop
for (i in 1:3){
  reg[[i]] <- summary(lm(var[[i]], data = versicolor))
}

reg


# hint: consider using a list, and also new vectors for regression variables



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#create new data frame called new_iris
#left_join matches values from y to x; keeps all rows from iris data set and adds "height"
new_iris <- iris %>% left_join(height, by = "Species") #joins data set by matching "species" column




#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)


#3a. now make the same plot in ggplot

ggplot(iris, aes(x = Sepal.Length, #common aesthetic values (aes)
                 y = Sepal.Width)) + geom_point() #geom_point used to represent data points

#3b. make a scatter plot with ggplot and get rid of  busy grid lines

#same code structure as 3a but add theme_minimal()
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point() +
  theme_minimal() #customizes theme to minimal; default them is grey background

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species, #maps the species color to aes
                 size = Petal.Length)) + #correlates petal length with dot size; maps variable from data to size of geom  
  geom_point() +
  theme_minimal() +
  theme(panel.grid = element_blank()) + #removes major and minor grid lines from plot
  labs(title = "Iris Sepal Measurements", #adds chart title
       x = "Sepal Length (cm)", #adds axis titles
       y = "Sepal Width (cm)")

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

##In plot, everything is specified as parameters in one line of code
##ggplot uses various layers with data and aesthetics (aes) then geom_point() adds the points
##each layer has its specified function: aes() - aesthetics, geom() - plot type, labs() - labels, theme() - appearance 
