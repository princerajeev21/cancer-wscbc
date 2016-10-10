#STEP 1 - DATA INGESTION
#load data from csv file

cancer_data <- read.csv("breast_cancer.csv",stringsAsFactors = FALSE)
#View(cancer_data)
#check the structure of the data
str(cancer_data)

#STEP 2 - DATA PREPARATION
#since ID provides no useful information for future predictions, we remove it.
cancer_data <- cancer_data[-1]

#drop field with NA values
cancer_data <- cancer_data[-32]

#recheck the new structure of data
str(cancer_data)


#we are to predict if a case of cancer is benign or malignant.
#the diagonis field is of most importance
#check the diagonis field
table(cancer_data$diagnosis)

#check the data type of diagnosis field
class(cancer_data$diagnosis)

#Most machine learnig Algorithms work on factors, so we will convert
#diagnosis field from character into factor and 
#also provide informative labels to its data

cancer_data$diagnosis <- factor(cancer_data$diagnosis,levels=c('B','M'),labels=c("Benign","Malignant"))


#recheck the class of diagnosis field
class(cancer_data$diagnosis)


table(cancer_data$diagnosis)


#calculate te percentage of each cases of cancer
round(prop.table(table(cancer_data$diagnosis)) * 100,digits=1)


#lets now take a look at the other fields
summary(cancer_data)

#we are interested in the way a cancer is classified
#we use radius_mean, area_mean and smoothness_mean to deteremine so
#lets take a look at these three fields in detail


summary(cancer_data[c("radius_mean","area_mean","smoothness_mean")])


#STEP 3 - DATA TRANSFORMATION


#since we need equally scaled values for using
#k-NN method, which is not available in the fields selected
#we will use a normalize function to normalize the values


#Define the normalize function
normalize <- function(x){
  return(((x-min(x)))/(max(x)-min(x)))
}


#test the normalize function
normalize(c(1, 2, 3, 4, 5))

normalize(c(10, 20, 30, 40, 50))


#the normalize function works fine


#Lets apply the normalize function to all fields using
# lapply() function which applies the function to all fields
#without the need to write the code for each field

#after using lapply() function we will convert all the fields
#into data-frames as they are all currently numeric


cancer_data_normal <- as.data.frame(lapply(cancer_data[2:31],normalize))


#lets check the new data
summary(cancer_data_normal[c("radius_mean","area_mean","smoothness_mean")])



#now all three fields range between 0 and 1


#STEP 4 - CREATING TRAINING AND TEST DATA SETS

#divide first 469 records into training set 
#and last 100 records into test set


cancer_data_train <- cancer_data_normal[1:469, ]
cancer_data_test <- cancer_data_normal[470:569, ]


#separate the target field aka "diagnosis" field and
#put it into factor vectors using labels



cancer_data_train_labels <- cancer_data[1:469,1]
cancer_data_test_labels <- cancer_data[470:569,1]



#STEP 5 - TRAINING A MODEL ON THE DATA

#import necessary packagaes
require(class)


#train the model using k-NN model with k=21

cancer_data_test_pred <- knn(train=cancer_data_train,test=cancer_data_test,
                             cl=cancer_data_train_labels,k=21)


#STEP 6 - EVALUATING MODEL PERFORMANCE

require(gmodels)
CrossTable(x=cancer_data_test_labels,y=cancer_data_test_pred,prop.chisq=FALSE)
