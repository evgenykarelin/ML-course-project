# Course Project
Evgeny Karelin  
October 8, 2017  



## Executive summary

The goal of project is to build a prediction model for performing of the 
Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification (Class A), 
throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), 
lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E). Data 
from accelerometers on the belt, forearm, arm, and dumbell of 6 participants is used for analysis.

## Data preprocessing

Initial dataset containes a lot of missing values. This chunk of code calculates the
NA-rate for each variable. Also first 7 variables containing not relevant information are being removed.

```r
library(caret)
```

```
Loading required package: lattice
```

```
Loading required package: ggplot2
```

```r
train<-read.csv("pml-training.csv", na.strings = c("NA","","#DIV/0!"))
test<-read.csv("pml-testing.csv", na.strings = c("NA","","#DIV/0!"))

train<-train[,-(1:7)]
test<-test[,-(1:7)]

naExplore<-function(dt)
{
        n=nrow(dt)
        naCount<-apply(dt, 2, f<-function(x) sum(is.na(x)))
        naCount<-sort(naCount, decreasing = TRUE)/n
        naCount
}

NArate<-naExplore(train)
table(NArate)
```

```
NArate
                0 0.979308938946081 0.979359902150647 0.979410865355213 
               53                67                 1                 1 
0.979512791764346 0.979563754968912 0.979767607787178 0.979818570991744 
                1                 4                 1                 4 
 0.97986953419631 0.980939761492203 0.983233105697686 0.983284068902253 
                2                 2                 1                 1 
0.983385995311385 0.983538884925084  0.98358984812965 0.983640811334217 
                2                 1                 4                 2 
                1 
                6 
```

So, there are 53 variables without NA. The rest have over 95% of NA and couldn't be used as predictors.


```r
# Subset zero NA-rate variables
vars<-names(NArate[NArate==0])
dt<-train[,vars]
vars<-vars[1:(length(vars)-1)]
test<-test[,vars]
```


## Training model

Training random forest model using 5 folds cross-validation.

```r
set.seed(12345)
inTrain<-createDataPartition(y=dt$classe, p=0.6, list=F)
training<-dt[inTrain,]
testing<-dt[-inTrain,]

train_control <- trainControl(method="cv", number=5)
modelFit<-train(classe~., data=training, method="rf", trControl=train_control)
```

```
randomForest 4.6-12
```

```
Type rfNews() to see new features/changes/bug fixes.
```

```

Attaching package: 'randomForest'
```

```
The following object is masked from 'package:ggplot2':

    margin
```

```r
modelFit
```

```
Random Forest 

11776 samples
   52 predictor
    5 classes: 'A', 'B', 'C', 'D', 'E' 

No pre-processing
Resampling: Cross-Validated (5 fold) 
Summary of sample sizes: 9421, 9420, 9420, 9421, 9422 
Resampling results across tuning parameters:

  mtry  Accuracy   Kappa    
   2    0.9896404  0.9868941
  27    0.9897252  0.9870019
  52    0.9864130  0.9828111

Accuracy was used to select the optimal model using  the largest value.
The final value used for the model was mtry = 27.
```

```r
prediction<-predict(modelFit, newdata=testing)
confusionMatrix(prediction,testing$classe)
```

```
Confusion Matrix and Statistics

          Reference
Prediction    A    B    C    D    E
         A 2227   10    0    0    0
         B    5 1502    7    0    2
         C    0    6 1357   18    3
         D    0    0    4 1265    5
         E    0    0    0    3 1432

Overall Statistics
                                          
               Accuracy : 0.992           
                 95% CI : (0.9897, 0.9938)
    No Information Rate : 0.2845          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9898          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: A Class: B Class: C Class: D Class: E
Sensitivity            0.9978   0.9895   0.9920   0.9837   0.9931
Specificity            0.9982   0.9978   0.9958   0.9986   0.9995
Pos Pred Value         0.9955   0.9908   0.9805   0.9929   0.9979
Neg Pred Value         0.9991   0.9975   0.9983   0.9968   0.9984
Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
Detection Rate         0.2838   0.1914   0.1730   0.1612   0.1825
Detection Prevalence   0.2851   0.1932   0.1764   0.1624   0.1829
Balanced Accuracy      0.9980   0.9936   0.9939   0.9911   0.9963
```

In sample error is 0.986, what is very good result and out of sample error is even better and equals 0.993.

The prediction on validation data set

```r
predict(modelFit, newdata=test)
```

```
 [1] B A B A A E D B A A B C B A E E A B B B
Levels: A B C D E
```

