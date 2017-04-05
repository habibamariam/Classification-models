#logistic regression
# Data Preprocessing template

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)kern

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])
#fitting lclassifier
classifier= svm(formula=Purchased~.,data = training_set,type='C-classification',kernel='radial')

'predicting the test set results'

y_pred=predict(classifier,newdata = test_set[-3])


#evauation of results by making confusion matrix
cm=table(test_set[,3],y_pred)

#visualize the results

set=training_set
X1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
X2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(X1,X2)
colnames(grid_set)=c('Age','EstimatedSalary')
y_grid= predict(classifier,newdata = grid_set)
plot(set[,-3],
     main='kernel SVM',
     xlab='Age',ylab='estimated salary',
     xlim=range(X1),ylim=range(X2))

contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3'))


