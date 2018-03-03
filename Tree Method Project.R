## Tree methods to classify schools as Private or Public based off their features.
## Using College data frame from ISLR library 

library(ISLR)
data('College')

df <- College

# Check the head of data frame 
print(head(df))

#Explore the structure of df
print(str(df))

#Check df for missing values 
print(any(is.na(df))) #### no missing values 

#Exploratory data analysis 

#Scatterplot of Grad.Rate versus Room.Board,colored by the Private column
library(ggplot2)
g <- ggplot(df, aes(Room.Board,Grad.Rate))+geom_point(aes(color = Private),size = 2, alpha = .5)
print(g)
#Grade.Rate is greater for larger Room.Board values 

#Histogram of full time undergrad students, color by Private
h1 <- ggplot(df,aes(F.Undergrad))+geom_histogram(aes(fill = Private),bins = 50,color = 'black')
print(h1)

#Histogram of Grad.Rate colored by Private
h2 <- ggplot(df,aes(Grad.Rate))+geom_histogram(aes(fill = Private),color = 'black',bins = 50)
print(h2)
#Colleges with higher grade rate tend to be private, there exist college with Grade.Rate higher than 100% .

#Finding and correcting the outliers 
subset(df ,Grad.Rate > 100)
df['Cazenovia College',"Grad.Rate"] <- 100 

#Spliting the data into train and test dataset 
library(caTools)
sample <- sample.split(df$Private , SplitRatio = 0.7)
train <- df[sample , ]
test <- df[!sample,]

#building decision tree model and evaluating it 
tree <- rpart(Private~.,data = train , method = 'class')
tree.pred <- predict(tree , test)
tree.pred <- as.data.frame(tree.pred)
head(tree.pred)
#Create a function to bind new result column to tree.pred
pred.fun <- function(x){
           if(x >0.50)
           {return('Yes')}
           else{return('No')}}

#Bind the result column 
tree.pred$Result <- sapply(tree.pred$Yes ,pred.fun)

#Check the model accuracy by confusion matrix 
print(table(test$Private,tree.pred$Result))
accuracy <- (57+160)/(57+160+9+7)*100

#Plot the decision tree 
library(rpart.plot)
print(prp(tree))

#Create Random Forest model 
model <- randomForest(Private~.,data = train,importance = T )
#Confusion matrix for training dataset 
print(model$confusion)
print(model$importance)

#Evaluate the model for test data 
pred.Result <- predict(model,test)
#Confusion matrix of test dataset 
print(table(pred.Result,test$Private))
model.accuracy <- (56+162)/(56+7+8+162)*100

#Tree model accuraccy = 93.13%
#Random Forest model accuracy = 93.56%













