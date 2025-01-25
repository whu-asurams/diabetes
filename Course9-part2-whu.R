
##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# We will use the diabtes_dataset from kapple website for this project.
# Here is the download information
# Download latest version
# path = kagglehub.dataset_download("akshaydattatraykhare/diabetes-dataset"
# The website is https://www.kaggle.com/datasets/akshaydattatraykhare/diabetes-dataset

# However, it is very tricky to download directly from kapple, which requires login.
# Instead, I download the diabetes.csv file and uoload to my github repo.
# The link is https://github.com/whu-asurams/diabetes/archive/refs/heads/main.zip for the whole repo.
# Afterward, an unzip is needed.

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)


options(timeout = 120)
options(digits = 5)

diabetes_file<- "diabetes.zip"


# Download the whole github repo from my github account
path = "https://github.com/whu-asurams/diabetes/archive/refs/heads/main.zip"
if(!file.exists(diabetes_file))
  download.file(path, diabetes_file)

# Unzip the daibetes.cvs file only
csv_file <- "diabetes-main/diabetes.csv"
if(!file.exists(csv_file))
  unzip(diabetes_file, csv_file)


# Subsection 1.2. Basic info of the dataset.
# Read the csv file into R
dl <- read.csv(csv_file, header=TRUE, stringsAsFactors=FALSE)

#Rename DiabetesPedigreeFunction to DPF for display purpose
dl <- dl %>% rename(DPF = DiabetesPedigreeFunction)


# Inspect the structure of the dataset.
str(dl)

# Data range of each variable

vv<- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction", "Age")


res<- lapply(vv, function (vv){
  print(vv)
  va<- test_part[[vv]]
  class(va)
  if(class(va) == "integer")
    sprintf("%-25s: integer, range = (%d, %d)", vv, min(test_part[[vv]]), max(test_part[[vv]]))
  else
    sprintf("%-25s: numeric, range = (%.4f, %.4f)", vv, min(test_part[[vv]]), max(test_part[[vv]]))
})

unlist(res)



# Subsection 1.3. Split the dataset into train_part and test_part.

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier

test_index <- createDataPartition(y = dl$Age, times = 1, p = 0.2, list = FALSE)
train_part <- dl[-test_index,]
test_part <- dl[test_index,]

# Clean temporary varibales 
rm(dl, test_index)

print("The summary of train_part")
str(train_part)
print("=========================================")

print("The summary of test_part")
str(test_part)


###################################
# Section 2. Analysis/Mathod
###################################


# Subsection 2.1. Inspect each predicator (variable)


library(dplyr)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

pa1 <- train_part %>% ggplot(aes(x=Pregnancies))  +
      geom_density(fill = "grey", bw=1)   +
      xlab("Number of Pregnancies") +
      ggtitle("1")
pa2 <- train_part %>% ggplot(aes(x=Glucose)) +      
      geom_density(fill = "grey", bw=10) +
      xlab("Glucose") +
      ggtitle("2")
pa3 <- train_part %>% ggplot(aes(x=BloodPressure)) +
      geom_density(fill = "grey", bw=5) +  
      xlab("Blood Pressure") +        
      ggtitle("3")
pa4 <- train_part %>% ggplot(aes(x=SkinThickness)) +
      geom_density(fill = "grey", bw = 1) +  
      xlab("Skin Thickness") +
      ggtitle("4")
pa5 <- train_part %>% ggplot(aes(x=Insulin))  +
      geom_density(fill = "grey", bw = 10)   +
      xlab("Insulin") +
      ggtitle("5")
pa6 <- train_part %>% ggplot(aes(x=BMI)) +      
      geom_density(fill = "grey", bw = 1) +
      xlab("BMI") +
      ggtitle("6")
pa7 <- train_part %>% ggplot(aes(x=DiabetesPedigreeFunction)) +
      geom_density(fill = "grey", bw = 1) +  
      xlab("Diabetes Pedigree Function") +        
      ggtitle("7")
pa8 <- train_part %>% ggplot(aes(x=Age)) +
      geom_density(fill = "grey", bw = 1) +  
      xlab("Age") +        
      ggtitle("8")

grid.arrange(pa1,pa2,pa3,pa4,pa5,pa6,pa7,pa8, ncol=4)



# Subsection 2.2. Correlations


outcome<- train_part$Outcome
res<- lapply(vv, function(vv){
  va<- train_part[[vv]]
  cor_res<-cor.test(va, outcome)
  sprintf("%-25s: t=%2.2f, df=%4d, p-value=%1.1e %s 0.05, 95 percent confidence interval=(%0.5f, %0.5f), cor=%.5f", 
          v1, cor_res$statistic, cor_res$parameter, cor_res$p.value, ifelse(cor_res$p.value<=0.05, "<=", ">"), cor_res$conf.int[1], cor_res$conf.int[2], cor_res$estimate)
})
names(res)<- vv
unlist(res)


#Subsection 2.3. Bar charts against Outcome

pb1 <- train_part %>%
  group_by(Pregnancies, Outcome) %>%
  summarize(n=n())%>%
  ggplot(aes(x=Pregnancies, y=n, fill=as.factor(Outcome)))  +
  geom_bar(stat="identity", show.legend = TRUE)   +
  xlab("Number of Pregnancies") +
  ggtitle("1")
#pb1

breaks<- c(0, 100, 130, 150, 200)
pb2 <- train_part %>%
  mutate(glucose_cut = cut(Glucose, breaks=breaks, include.lowest=FALSE, right=FALSE)) %>%
  group_by(glucose_cut, Outcome) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=glucose_cut, y=n, fill=as.factor(Outcome)))  +
  geom_bar(stat="identity", show.legend = TRUE)   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="none")+
  xlab("Glucose") +
  ggtitle("2")
#pb2

breaks<- c(0, 60, 80, 100, 130, 150, 200)
pb3 <- train_part %>% 
  mutate(blood_pressure_cut = cut(BloodPressure, breaks=breaks, include.lowest=FALSE, right=FALSE)) %>%
  group_by(blood_pressure_cut, Outcome) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=blood_pressure_cut, y=n, fill=as.factor(Outcome))) +
  geom_bar(stat="identity", show.legend = TRUE)   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="none")+
  xlab("Blood Pressure") +        
  ggtitle("3")
#pb3

breaks<- seq(0, 100, 10)
pb4 <- train_part %>% 
  mutate(skin_thickness_cut = cut(SkinThickness, breaks=breaks, include.lowest=FALSE, right=FALSE)) %>%
  group_by(skin_thickness_cut, Outcome) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=skin_thickness_cut, y=n, fill=as.factor(Outcome))) +
  geom_bar(stat="identity", show.legend = TRUE)   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "none")+
  xlab("Skin Thickness") +
  ggtitle("4")
#pb4

breaks<- c(0, 2, 25, 100, 500)
pb5 <- train_part %>% 
  mutate(insulin_cut = cut(Insulin, breaks=breaks, include.lowest=FALSE, right=FALSE)) %>%
  group_by(insulin_cut, Outcome) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=insulin_cut, y=n, fill=as.factor(Outcome))) +
  geom_bar(stat="identity", show.legend = TRUE)   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="none")+
  xlab("Insulin") +
  ggtitle("5")
#pb5

breaks<- c(0, 18.5, 24.9, 29.9)
pb6 <- train_part %>%
  mutate(bmi_cut = cut(BMI, breaks=breaks, include.lowest=FALSE, right=FALSE)) %>%
  group_by(bmi_cut, Outcome) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=bmi_cut, y=n, fill=as.factor(Outcome))) +
  geom_bar(stat="identity", show.legend = TRUE)   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="none")+
  xlab("BMI") +
  ggtitle("6")
#pb6

breaks<- seq(0, 2.6, 0.2)
pb7 <- train_part %>% 
  mutate(dpf_cut = cut(DiabetesPedigreeFunction, breaks=breaks, include.lowest=FALSE, right=FALSE)) %>%
  group_by(dpf_cut, Outcome) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=dpf_cut, y=n, fill=as.factor(Outcome))) +
  geom_bar(stat="identity", show.legend = TRUE)   +
  xlab("Diabetes Pedigree Function") +      
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="none")+
  ggtitle("7")
#pb7

breaks<- seq(20, 85, 5)
pb8 <- train_part %>% 
  mutate(age_cut = cut(Age, breaks=breaks, include.lowest=FALSE, right=FALSE)) %>%
  group_by(age_cut, Outcome) %>%
  summarize(n=n()) %>%
  ggplot(aes(x=age_cut, y=n, fill=as.factor(Outcome))) +
  geom_bar(stat="identity", show.legend = TRUE)   +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position="none")+
  xlab("Age") +        
  ggtitle("8")
#pb8

grid.arrange(pb1,pb2,pb3,pb4, ncol=2)

grid.arrange(pb5,pb6,pb7,pb8, ncol=2)




#  Subsection 3.1. Combinations of predictors

#For train_part

x1<- train_part[,-9]
x2<- train_part%>%
  select(Pregnancies, Glucose, Insulin, BMI, DPF, Age)
x3<- train_part%>%
  select(Pregnancies, Glucose, BMI, Age)

y<-factor(train_part$Outcome)

#For test_part
xt1<- test_part[,-9]
xt2<- test_part%>%
  select(Pregnancies, Glucose, Insulin, BMI, DPF, Age)
xt3<- test_part%>%
  select(Pregnancies, Glucose, BMI, Age)

yt<- factor(test_part$Outcome)




# Models to be used
models <- c("glm", "lda", "naive_bayes", "knn", "gamLoess", "qda", "rf")



# a function that train, predict and plot

train_pred<- function(x,xt, y, yt, title){
  set.seed(1, sample.kind = "Rounding")
  fits <- lapply(models, function(model){ 
    print(model)
    train(x, y, method = model)
  }) 
  names(fits)<- models

  train_accuracy<- lapply(models, function(model){ 
    max(fits[[model]]$results$Accuracy)
  }) 

  train_accuracy<- unlist(train_accuracy)

  pred_avg <- lapply(models, function(model){ 
    test_preds <- predict(fits[[model]], xt)
    avg<- mean(test_preds == yt)
    avg
  }) 

  pred_avg<- unlist(pred_avg)


  ggplot() +
  geom_point(aes(x=models, y= train_accuracy), colour = "black")   +
  geom_point(aes(x=models, y= pred_avg), colour = "red", size=3)   +
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position="top")+
  xlab("Models") +
  ylab("Accuracy")+
  ggtitle(title)
  
  train_accuracy
}

# function to train a dataset on each of the 7 models
train_models<- function(x, y){
  set.seed(1, sample.kind = "Rounding")
  fits <- lapply(models, function(model){ 
    print(model)
    train(x, y, method = model)
  }) 
  names(fits)<- models
  fits
}

# A function to extract training accuracy from the trained models.

calculate_train_accuracies<- function(models, fits){
  train_accuracy<- lapply(models, function(model){ 
    max(fits[[model]]$results$Accuracy)
  })
  #aa<- as.factor(unlist(aa))
  train_accuracy<- unlist(train_accuracy)
  train_accuracy
}  
  
# function to test the trained model on test dataset

calculate_test_preds<- function(models, fits, xt){  
  test_pred <- lapply(models, function(model){ 
    predict(fits[[model]], xt)
  }) 
  test_pred
}

# a function that calculate the test prediction accuracy

pred_accuracy<- function(tp, yt){
  avg<- lapply(tp, function(tp){
    mean(tp == yt)
  })
  unlist(avg)
}  

# A function to plot the training accuracy and test prediction accuracy

plot_accuracy<- function(models, train_accuracy, pred_avg, title){  
    ggplot() +
    geom_point(aes(x=models, y= train_accuracy), colour = "black")   +
    geom_point(aes(x=models, y= pred_avg), colour = "red", size=3)   +
    theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position="top")+
    xlab("Models") +
    ylab("Accuracy")+
    ggtitle(title)
}

# Train, predict, and plot  on each combination.

train_rate1<- train_pred(x1, xt1, y, yt, "All eight predictors")
train_rate2<- train_pred(x2, xt2, y, yt, "Pregnancies, Glucose, Insulin, BMI, DPF, Ag")
train_rate2<- train_pred(x3, xt3, y, yt, "Pregnancies, Glucose, BMI, Age")


# obtain the trained model on each data combination
fits_1<- train_models(x1, y)
fits_2<- train_models(x2, y)
fits_3<- train_models(x3, y)

# obtain training accuracy on each data combination
train_accuracy_1<- calculate_train_accuracies(models, fits_1)
train_accuracy_2<- calculate_train_accuracies(models, fits_2)
train_accuracy_3<- calculate_train_accuracies(models, fits_3)

# use the trained model to predict the test set on each combination
test_prediction_1<- calculate_test_preds(models, fits_1, xt1)
test_prediction_2<- calculate_test_preds(models, fits_2, xt2)
test_prediction_3<- calculate_test_preds(models, fits_3, xt3)

# obtain the test prediction accuracy

pred_avg_1<- pred_accuracy(test_prediction_1, yt)
pred_avg_2<- pred_accuracy(test_prediction_2, yt)
pred_avg_3<- pred_accuracy(test_prediction_3, yt)


# plot training and test prediction accuracy on each combination
plot_accuracy(models, train_accuracy_1, pred_avg_1, "All eight predictors")
plot_accuracy(models, train_accuracy_2, pred_avg_2, "Pregnancies, Glucose, Insulin, BMI, DPF, Age")
plot_accuracy(models, train_accuracy_3, pred_avg_3, "Pregnancies, Glucose, BMI, Age")#plot_accuracy(models, 


# Subsection 3.3 Emsemble of models

# Ensemble model by majority vote
model_ensemble<- function(tp){
  dp<- as.data.frame(tp)
  names(dp)<- models
  dp<- dp%>%
    mutate(votes = rowMeans(dp==1),
         y_hat = ifelse(votes>=0.5, 1, 0))

  mean(dp$y_hat==yt)
}

# prediction of combination 1 using the ensemble model
model_ensemble(test_prediction_1)

# prediction of combination 2 using the ensemble model
model_ensemble(test_prediction_2)

# prediction of combination 3 using the ensemble model
model_ensemble(test_prediction_3)

  