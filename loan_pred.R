train = read.csv("C:\\Users\\Gaurav_Gola\\Desktop\\project\\loan prediction\\train.csv",na.strings = c(""," ",NA))
test = read.csv("C:\\Users\\Gaurav_Gola\\Desktop\\project\\loan prediction\\test.csv",na.strings = c(""," ",NA))

library(mlr)
summarizeColumns(train)
summarizeColumns(test)

#Data visualization
# for Character 

 for(i in colnames(train)){
   if(i != "Loan_ID"){
     
     for(j in colnames(test)){
       
         
         if(i==j){
           if(is.factor(train[,i])){
             
             par(mfrow=c(1,2))
             barplot(table(train[,i]),main =paste("train",i))
             barplot(table(test[,j]),main=paste("test",j))
             #print(paste("train",i))
             print(c(paste("train",i),prop.table(table(train[,i]))))
             #print(paste("test",j))
             print(c(paste("test",i),prop.table(table(test[,j]))))
           }
           }
           
         }
   }
 }

#for numeric

for(i in colnames(train)){
  if(i != "Loan_Amount_Term"){
    
    for(j in colnames(test)){
      
      
      if(i==j){
        if(is.numeric(train[,i])){
          
          par(mfrow=c(1,2))
          boxplot(train[,i],main =paste("train",i))
          boxplot(test[,j],main=paste("test",j))
         
        }
      }
      
    }
  }
}

unique(train$Loan_Amount_Term)
par(mfrow=c(1,2))
hist(train$Loan_Amount_Term)
hist(test$Loan_Amount_Term)



## Credit History
## unique(train$Credit_History)
#it should be in  factor
train$Credit_History = as.factor(train$Credit_History)
test$Credit_History = as.factor(test$Credit_History)

par(mfrow=c(1,2))
barplot(table(train$Credit_History),main="train credit history")
barplot(table(test$Credit_History),main="test credit history")

prop.table(table(train$Credit_History))
prop.table(table(test$Credit_History)) 


#### Loan_Status by other variables

## for character

for(i in colnames(train)){
  library(ggplot2)
  if(i!=c("Loan_ID","Loan_Amount_Term","Loan_Status")){
    if(is.factor(train[,i])){
      par(mfrow=c(1,3))
      print(ggplot(train, aes(x=train$Loan_Status))+geom_bar()+facet_grid(.~train[,i])+ggtitle(i))
      
    }
  }
}

# for numerical variable relation b/t dependent var(Loan status)

for(i in colnames(train)){
  library(ggplot2)
  if(i!= c("Loan_ID","Loan_Amount_Term","Loan_Status")){
    if(is.numeric(train[,i])){
      par(mfrow=c(1,3))
      print(ggplot(train,aes(x=train$Loan_Status,y=train[,i]))+geom_boxplot()+ggtitle(i))
      
      
    }
  }
}

#MIssing values imputation 
 full_data = rbind(train[,2:12],test[2:12])

#The first variables I will deal with are Applicant Income and Coapplicant Income.
 #Some of the applicants are males, so, presumably, the coapplicants are female and vice versa.

library(ggplot2)
 #Applicants with higher than 20000 income have been truncated from the plot 
 print(ggplot(data=full_data[full_data$ApplicantIncome<20000,],aes(ApplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))
 #No difference observed 
 
 
 
 # so check in coapplicant income
 print(ggplot(data=full_data[full_data$ApplicantIncome<20000,],aes(CoapplicantIncome,fill=Married))+geom_bar(position="dodge")+facet_grid(Gender~.))
 
 # coapplicant income :-
 # male married - high income
 # male unmarried - low income (almost zero)
 #for most of the male unmarried applicants, the coapplicant has zero income or low income in general,
 #though there are a few cases where the coapplicant has high income.
 
 
 # female married - low income
 #Again more female applicants are not married , 
 #and for almost all of these applicants the coapplicant has zero income. I assume this means there is no coapplicant
 
 
 #in  male married -- coapplicant zero or low
 # in female inmarried - copplicant zero or low 
 
 
 library(plyr)
 full_data2<-mutate(full_data,TotalIncome=ApplicantIncome+CoapplicantIncome)
 print(ggplot(data=full_data2,aes(TotalIncome,fill=Married))+geom_bar(position="dodge")
       +facet_grid(Gender~.))

 
 ## Furthermore, it seems reasonable to impute marital status as
 #"No" when the coapplicant income is zero, and "Yes", otherwise.
 full_data2$Married[is.na(full_data2$Married) & full_data2$CoapplicantIncome==0]<-"No"
 full_data2$Married[is.na(full_data2$Married)]<- "Yes"

 prop.table(table(full_data2$Married))

 #Gender and dependents
 
 any(is.na(full_data2$Gender))
 any(is.na(full_data2$Dependents))
 
 # checking missing values with condition:
  
 full_data2[is.na(full_data2$Gender) & is.na(full_data2$Dependents),]
 
 #This applicant is not married but has higher income than the coapplicant. I'll impute this one as "Male".
 
 full_data2$Gender[is.na(full_data2$Gender) & is.na(full_data2$Dependents)] <- "Male"
 
 print(ggplot(full_data2,aes(x=Dependents, fill=Gender)) + geom_bar() + facet_grid(.~Married)) 
 
 #It looks safe to impute the number of dependents for the unmarried males and females as the mode=0. 
 #The mode for the married applicants is also zero, but the other values are more significant
 #than in the unmarried case. All the missing ones are male applicants. I will use rpart to predict the
 #number of dependents for this population, using applicant income,coapplicant income, loan amount, 
 #loan term and property area as predcitors.
 
 # imputing for unmarried 
 full_data2$Dependents[is.na(full_data2$Dependents) & full_data2$Married=="No"]= "0"
 
# for married we will do rpart 
 subset =   full_data2[(full_data2$Gender=="Male" & full_data2$Married=="Yes"),c(3,6:9,11)]
 
 subtrain=subset[!is.na(subset$Dependents),]
 subtest= subset[is.na(subset$Dependents),]
 library(rpart)
 library(rattle)
 depFit <- rpart(data=subtrain,Dependents~.,xval=3)
 fancyRpartPlot(depFit)
 
 p=predict(depFit,subtrain,type="class")
 p
 acc = sum(p==subtrain[,1])/length(p)
 
 acc
 
 
 full_data2$Dependents[is.na(full_data2$Dependents) & full_data2$Gender=="Male" & full_data2$Married == "Yes"]= predict(depFit,newdata=subtest,type="class")
 
 
 
# for missing genders
 #rpart 
 
 gendertrain = full_data2[!is.na(full_data2$Gender),1:7]
 gendertest<-full_data2[is.na(full_data2$Gender),1:7]
 genFit<-rpart(data=gendertrain,Gender~.,xval=3)
 fancyRpartPlot(genFit)
 
 p = predict(genFit,gendertrain,type="class")
 acc<-sum(p==gendertrain[,1])/length(p)
 acc
 
 
 full_data2$Gender[is.na(full_data2$Gender)]=predict(genFit,gendertest,type="class") 
 
 
 # Self employment missing values on the basis of mode #prop.table#
 
 full_data2$Self_Employed[is.na(full_data$Self_Employed)] = "No"
 

#Credit history
 # Credit history. I think this variable should be treated carefully. If the credit history is not 
 #available, this means that the applicant has not had many credit activities in the past, 
 #so these applicants should be treated as a separate category. Recoding:
 
 library(car)
 full_data2$Credit_History<-recode(full_data2$Credit_History,"NA=2")

 
 #Loan Amount
 #logistic regression 
 
 ltrain = full_data2[!is.na(full_data2$LoanAmount) & full_data2$LoanAmount<500,c(1:8,10)]
 ltest = full_data2[is.na(full_data2$LoanAmount),c(1:8,10)]
 loanFit = glm(data=ltrain,LoanAmount~.,na.action=na.exclude)
 #impute
 full_data2$LoanAmount[is.na(full_data2$LoanAmount)] <- predict(loanFit,newdata=ltest)



#loan amount term
 full_data2$Loan_Amount_Term <- as.factor(full_data2$Loan_Amount_Term)
 
 full_data2$Loan_Amount_Term[is.na(full_data2$Loan_Amount_Term)]<-"360"
 full_data2$Loan_Amount_Term <- recode(full_data2$Loan_Amount_Term,"'350'='360';'6'='60'")

alldata2 = full_data2

####################################################################


numDependents <- recode(alldata2$Dependents,"'3+'='3' ")
numDependents <- as.numeric(as.character(numDependents))
alldata2$FamilySize <- ifelse((alldata2$CoapplicantIncome>0 |alldata2$Married=="Y"),numDependents+2,numDependents+1)
alldata2$IncomePC <- alldata2$TotalIncome/alldata2$FamilySize


alldata2$LoanAmountByTotInc <- alldata2$LoanAmount/alldata2$TotalIncome
alldata2$LoanAmountPC <- alldata2$LoanAmount/alldata2$IncomePC

alldata2$Loan_Amount_Term <- as.numeric(as.character(alldata2$Loan_Amount_Term))
alldata2$LoanPerMonth <- alldata2$LoanAmount/alldata2$Loan_Amount_Term

alldata2$LoanPerMOnthByTotInc  <- alldata2$LoanPerMonth/alldata2$TotalIncome
alldata2$LoanPerMonthPC <- alldata2$LoanPerMonth/alldata2$LoanAmountPC

#make loan term variable factor again
alldata2$Loan_Amount_Term <- as.factor(alldata2$Loan_Amount_Term)


logbins<-cut(ifelse(alldata2$ApplicantIncome<2.72,0,log(alldata2$ApplicantIncome)),breaks=20)

alldata2$LogApplicantIncome <- ifelse(alldata2$ApplicantIncome<2.72,0,log(alldata2$ApplicantIncome))
alldata2$LogCoapplicantIncome <- ifelse(alldata2$CoapplicantIncome<2.72,0,log(alldata2$CoapplicantIncome))

summary(alldata2$LoanAmount)

alldata2$LogLoanAmount <- log(alldata2$LoanAmount)

summary(alldata2$TotalIncome)


alldata2$LogTotalIncome <- log(alldata2$TotalIncome)

summary(alldata2$IncomePC)

alldata2$IncomePC <- log(alldata2$IncomePC)

summary(alldata2$LoanAmountByTotInc)

alldata2$LogLoanAmountPC <- log(1000*alldata2$LoanAmountPC)
alldata2$LogLoanPerMOnth <- log(alldata2$LoanPerMonth)


alldata2$LogLoanPerMOnthPC <- log(alldata2$LoanPerMonthPC)




nums <- sapply(alldata2,class)=="numeric"
numvars <- alldata2[,nums]
m<-cor(numvars)
v<-as.vector(m) 
id1<- rep(rownames(m),17)
id2<-as.vector(sapply(rownames(m),function(x)rep(x,17)))
d<-data.frame(v,id1,id2)
d<-d[d$v>0.8 & d$v<1,]
d

d<-d[c(1:5,8),]
d



newtrain <- cbind(Loan_Status=train$Loan_Status,alldata2[1:614,])

#bogus Loan status for test set
Loan_Status <- as.factor(sample(c("N","Y"),replace=TRUE,size=dim(test)[1]))
newtest <- cbind(Loan_Status,alldata2[615:981,])

#create task
trainTask <- makeClassifTask(data = newtrain,target = "Loan_Status")
testTask <- makeClassifTask(data = newtest, target = "Loan_Status")

#normalize the variables
trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")



###################################################################


rf <- makeLearner("classif.randomForest", predict.type = "response"
                  , par.vals = list(ntree = 200, mtry = 3))
rf$par.vals <- list(importance = TRUE)

#set tunable parameters
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 2, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

#let's do random search for 100 iterations
rancontrol <- makeTuneControlRandom(maxit = 100L)

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#hypertuning
set.seed(11)
rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = trainTask, par.set = rf_param, control = rancontrol)
#cv accuracy
rf_tune$y

rf_tune$x


#using hyperparameters for modeling
tunedrf <- setHyperPars(rf, par.vals = rf_tune$x)

#train a model
rforest <- train(tunedrf, trainTask)
getLearnerModel(rforest)


#make predictions
rfmodel <- predict(rforest, testTask)

#submission file
submit2 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = rfmodel$data$response)
# write.csv(submit2, "sol2.csv",row.names = F)

submit<-cbind(submit1$Loan_Status,submit2$Loan_Status)
sum(submit[,1]==submit[,2])


LP = write.csv(submit2,file = "LP.csv")
getwd()



################
tree <- makeLearner("classif.rpart", predict.type = "response")

#set 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#Search for hyperparameters
treepars <- makeParamSet(
  makeIntegerParam("minsplit",lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

#try 100 different combinations of values
tpcontrol <- makeTuneControlRandom(maxit = 100L)

#hypertune the parameters
rm(acc)
set.seed(11)
treetune <- tuneParams(learner = tree, resampling = set_cv, 
                       task = trainTask, par.set = treepars, control = tpcontrol, measures = acc)
treetune


#using hyperparameters for modeling
tunedtree <- setHyperPars(tree, par.vals=treetune$x)

#train the model
treefit <- train(tunedtree, trainTask)
par(mfrow=c(1,1))
fancyRpartPlot(getLearnerModel(treefit))

#make predictions
treepred <- predict(treefit, testTask)

#create a submission file
submit1 <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = treepred$data$response)
 write.csv(submit1, "sol1.csv",row.names = F)




























