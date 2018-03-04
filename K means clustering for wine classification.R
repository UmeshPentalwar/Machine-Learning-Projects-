#Building K means model to classify the wine into two categories using the historical data 
#and evaluating the model with the tain data 
#Using wine data present at http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
#Downlod wine features data into working directory and load it in the workspace

df1 <- read.csv('winequality-red.csv', sep = ';')
df2 <- read.csv('winequality-white.csv',sep = ';')

print(str(df1))
print(str(df2))

print(summary(df1))
print(summary(df2))

#Check for missing values 
print(any(is.na(df1)))   
print(any(is.na(df2)))
#No NA values in both the datasets 

#Add a label column to both the datasets 
df1$label <- sapply(df2$citric.acid , function(x){return('Red')})
df2$label <- sapply(df2$citric.acid , function(x){return('White')})

#Combine two datasets to a single one needed to create cluster 
wine <- rbind(df1,df2)


#Exploratory data analysis 

#White & Red quantity comparison 
b <- ggplot(wine,aes(label))+geom_bar(aes(fill = label))+scale_fill_manual(values = c('#ae4554','#faf7ea'))
print(b)
# White wine observations are more compared to red wine 

#Histogram of residual sugar from the wine data. Color by red and white wines
h1 <- ggplot(wine,aes(residual.sugar))+geom_histogram(aes(fill=factor(label)),bins = 50,color = 'black' )+scale_fill_manual(values=c('#ae4554','#faf7ea'))
print(h1)

#Histogram of citric.acid from the wine data. Color by red and white wines
h2 <- ggplot(wine,aes(citric.acid))+geom_histogram(aes(fill=factor(label)),bins = 50,color = 'black' )+scale_fill_manual(values=c('#ae4554','#faf7ea'))
print(h2)

#Histogram of alcohol from the wine data. Color by red and white wines
h3 <- ggplot(wine,aes(alcohol))+geom_histogram(aes(fill=factor(label)),bins = 50,color = 'black' )+scale_fill_manual(values=c('#ae4554','#faf7ea'))
print(h3)

#scatterplot of residual.sugar versus citric.acid, color by red and white wine
s1 <- ggplot(wine,aes(citric.acid,residual.sugar))+geom_point(aes(color = label),alpha = 0.2)+scale_color_manual(values = c('#ae4554','#faf7ea'))+theme_dark()
print(s1)

#scatterplot of volatile.acidity versus residual.sugar, color by red and white wine
s2 <- ggplot(wine,aes(volatile.acidity,residual.sugar))+geom_point(aes(color = label),alpha = 0.2)+scale_color_manual(values = c('#ae4554','#faf7ea'))+theme_dark()
print(s2)

#Grabing the wine data without the label
clus.data <- wine[-13]

#Creating the wine cluster 
wine.cluster <- kmeans(clus.data,2)
print(wine.cluster)

#cluster evaluation through confusion matrix 
print(table(wine$label , wine.cluster$cluster))

#Printing the cluster 
library(cluster)
print(clusplot(wine[1:12],wine.cluster$cluster , color = T,shade = T,labels = 0,lines = 0))

#The model for 2 clusters did not classified the two wine types perfectly as both the wine types have some 
#resembling features 




















