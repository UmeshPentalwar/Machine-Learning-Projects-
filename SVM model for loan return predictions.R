# Building SVM model for the prediction of borrowers paying the loan back.
# Using historical loan data from "LendingClub" available at https://www.lendingclub.com/info/download-data.action 
# Download the csv file to working directory and input it in workspace 

loans <- read.csv('loan_data.csv')
print(head(loans))
print(str(loans))
print(summary(loans))

#Data clensing 

#Missing values check 
print(any(is.na(loans)))   ## No NAs 
#Columns nq.last.6mths,delinq.2yrs,pub.rec,not.fully.paid,credit.policy contain catagorical data
#Coversion to factors 
loans$nq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)
loans$credit.policy <- factor(loans$credit.policy)

      
#Exploratory data analysis 

#Histogram of fico scores colored by not.fully.paid
h1 <- ggplot(loans,aes(fico))+geom_histogram(aes(fill=not.fully.paid),bins = 40,color = 'black',alpha = .5)
      + scale_fill_manual(values = c('red','green'))+ggtitle ('Fico Score Histogram')
print(h1)

#fully paid & not fully paid comparison 
b1 <- ggplot(loans,aes(not.fully.paid))+geom_bar(aes(fill = not.fully.paid),alpha = .5,color = 'black')+
      ggtitle('Loan paid summary')+scale_fill_manual(values = c('red','green'))
print(b1)

#barplot of purpose counts, colored by not.fully.paid. Use position=dodge in the geom_bar argument
b2 <- ggplot(loans,aes(purpose))+geom_bar(aes(fill = not.fully.paid),color = 'black',alpha=.5,position = 'dodge')+
      scale_fill_manual(values=c('red','green'))+ggtitle('Bar plot for loan purpose')  
print(b2)

#scatterplot of fico score versus int.rate 
g <- ggplot(loans,aes(int.rate,fico))+geom_point(aes(color = not.fully.paid),alpha = .5)
print(g)


#Creating train & test datasets 
library(caTools)
sample <- sample.split(loans$fico , SplitRatio = 0.7)
train <- loans[sample,]
test <- loans[!sample,]

#Model building and evaluation 
library(e1071)
model <- svm(not.fully.paid~.,data = train, kernel = 'radial')
print(summary(model))

pred.result <- predict(model,test)
#confusion matrix 
print(table(pred.result,test$not.fully.paid))
#Accuracy not so good 

#Tuning the model 
tune.para <- tune(svm , not.fully.paid~.,data = train,kernel = 'radial',ranges = list(cost=10^(-1:2),gamma = c(0.1,1)))
print(tune.para)

#Tuning the model using cost = 10 & gamma = 0.1
tuned.model <- svm(not.fully.paid~.,data= train ,kernel= 'radial',cost= 10,gamma = 0.1)
results <- predict(tuned.model,test)
#confusion matrix after tuning 
print(table(results,test$not.fully.paid))

##Accuracy of svm increases after tuning 




