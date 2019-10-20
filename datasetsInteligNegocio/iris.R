install.packages("mboost")
install.packages("TH.data")
library(grid)
library(gridExtra)
library(RODBC)

#setear el directorio de trabajo  
setwd("C:/Users/joaju/OneDrive/Documentos/datasets")


str(iris)

data("bodyfat", package = "TH.data")
str(bodyfat)

dim(iris)
names(iris)

attributes(iris)

#first five rows of data
#The first or last rows of data can be retrieved with head() or tail()
iris[1:5,]

head(iris)

tail(iris)

#We can also retrieve the values of a single column. 
#For example, the first 10 values of Sepal.Length
iris[1:10, "Sepal.Length"]

iris$Sepal.Length[1:10]

#summary() returns the minimum, maximum, mean, median, 
#and the first (25%) and third (75%) quartiles
summary(iris)

#otra forma de obtenerlo: mean(),median(), range(), quantile()
quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, c(.1,.3,.65))

# we check the variance of Sepal.Length with var() and its distribution with
# histogram and density using hist() and density()
var(iris$Sepal.Length)
hist(iris$Sepal.Length)

plot(density(iris$Sepal.Length))

#The frequency of factors can be calculated with function table() and then 
#plotted as a pie chart with pie() or a bar chart with barplot()
table(iris$Species)

pie(table(iris$Species))
barplot(table(iris$Species))

#despues de chequear y calcular las distribuciones individuales de cada variable
# investigamos las relaciones entre dos variables del dataset
# calculate covariance and correlation between with cov() and cor()
cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])

cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])

#Next, we compute the stats of Sepal.Length of every Species with aggregate()
aggregate(Sepal.Length ~ Species, summary, data=iris)

# cada una de las variables en funcion de la especie
boxplot(Sepal.Length~Species, data=iris)

# A scatter plot can be drawn for two numeric variables with plot() as below. Using
# function with(), we do not need to add "iris$" before variable names. In the code
# below, the colors (col) and symbols (pch) of points are set to Species
with(iris, plot(Sepal.Length, Sepal.Width, col=Species, pch=as.numeric(Species)))

#We can use jitter() to add a small amount of noise to the data before plotting
plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))

# A matrix of scatter plots can be produced with function pairs()
pairs(iris)

library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

library(rgl)
plot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)

#A heat map presents a 2D display of a data matrix, which can be generated with
#heatmap() in R. With the code below, we calculate the similarity between different
#flowers in the iris data with dist() and then plot it with a heat map
distMatrix <- as.matrix(dist(iris[,1:4]))
heatmap(distMatrix)

library(lattice)
levelplot(Petal.Width~Sepal.Length*Sepal.Width, iris, cuts=9,
          col.regions=grey.colors(10)[10:1])

filled.contour(volcano, color=terrain.colors, asp=1, 
          plot.axes=contour(volcano, add=T))

persp(volcano, theta=25, phi=30, expand=0.5, col="lightblue")

library(MASS)
parcoord(iris[1:4], col=iris$Species)

library(lattice)
#no me anda
parallelplot(~iris[1:4] / Species, data=iris)

library(ggplot2)
qplot(Sepal.Length, Sepal.Width, data=iris, facets=Species ~.)

#saving charts into PDF and PS files with pdf() and postscript()
# save as a PDF file
pdf("myPlot.pdf")
x <- 1:50
plot(x, log(x))
graphics.off()

## Arboles de decision
# es el unico algortimo que permite variables categoricas
install.packages("party")
str(iris)

#Sepal.Length, Sepal.Width, Petal.Length, and Petal.Width are used to
#predict the Species of flowers. Before modeling, the iris data is split
#below into two subsets: training (70%) and test (30%). 
#The random seed se establece en un valor fijo a continuación 
#para que los resultados sean reproducibles.

set.seed(1234)
#separame el 70 y 30 % al azar
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
#archivo entrenamiento
trainData <- iris[ind==1,]
#archivo de datos
testData <- iris[ind==2,]

# luego con estos dos grupos build a decision tree, 
# and check the prediction result.
# In the code below, myFormula specifies that Species is the
# target variable and all other variables are independent variables.

library(party)
library(partykit)
#estimame la especie en funcion de estas 4 variables
myFormula <- Species ~ Sepal.Length + Sepal.Width +  Petal.Length + Petal.Width

library(grid)
library(libcoin)
library(mvtnorm)
iris_ctree <- ctree(myFormula, data=trainData)

# check the prediction (pagina 26 del libro)
table(predict(iris_ctree), trainData$Species)

# After that, we can have a look at the built tree by printing 
# the rules and plotting the tree
print(iris_ctree)

plot(iris_ctree)

plot(iris_ctree, type="simple")

#After that, the built tree needs to be tested with test data.
# predict on test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)

#data("bodyfat", package = "mboost")
dim(bodyfat)

attributes(bodyfat)

bodyfat[1:5,]

#Next, the data is split into training and test subsets,
#and a decision tree is built on the training data.

set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace = TRUE, prob = c(0.7,0.3))

#################################
# titanic
install.packages("titanic")
library(titanic)
str(Titanic)
df <- as.data.frame(Titanic)
head(df)