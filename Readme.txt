Loan prediction 

gender , self employed , app income , coapp income , loan amount , loan status
 credit history , property area ,education , marital status


barplot for categorical and prop. table 
b/w train and test set such that to find if NA values are impacting
prop. levels b/w the train and test 

 
boxplot for numerical - to find the skewness and outliers

Missing values imputation 

Fulldata = train + test  - dependent var is removed
------------------------------------------------------------------------------
applicant income vs married 

coapp income vs married 

New variable = total income = app income + coapp income 

total income vs married 

male applicant has higher income than its coapplicant 


So if coapp income is zero martial status = No and yes otherwise 
-----------------------------------------------------------------------------------------
For gender 

rpart is used
gender is dependent var and all other are independent var 
gtrain<-alldata2[!is.na(alldata2$Gender),1:7]
gtest<-alldata2[is.na(alldata2$Gender),1:7]
genFit<-rpart(data=gtrain,Gender~.,xval=

acc<-sum(p==gtrain[,1])/length(p)
accuracy of 80% 
----------------------------------------------------------------------------------
self employed 
 with mode value
=NO
------------------------------------------------------------------
Credit history  - levels 0,1


if the credit history is NA this means the applicant has no record 
So i have imputed it as "2" 
so that it can determined separately
------------------------------------------------------------------------------
Loan amount term 

The vast majority of the loans had a term of 360 months, so I just use this to fill in the missing values.
 Note that term of 350 occurs only once and it is in the test set. 
I’ll just assume that this was a misstype and should be 360. Similary, the 6 was probably meant to be 60.
-----------------------------------------------------------------------------

Log tranformation 

loan amount 
app income 
coapp income


-----------------------------------------------------------------------------
random forest is used

81.6% accuracy

















































