library(readr)
library(tidyverse)
library(ggplot2)
library(nlme)
library(car)
library(MASS)

df <- read.table("ClassAssessment.txt", header = TRUE)

# Converting Semester to a categorical variable instead of numerical
df$Semester <- factor(df$Semester)

# Summary data 
ggplot(df, aes(x = Semester, y = Final)) +
  geom_boxplot() + labs(title = "Final Score and Different Semesters", x = "Semester", y = "Final Score")

# correlation test for Final and Exam 3
cor(df$Exam3,df$Final)
cor(df$Quiz,df$Final)

pedagogy.model <- gls(model=Final ~ Semester + Exam1 + Exam2 + Exam3 + HW + Quiz, weights = varFixed(value = ~1/NStudents), method = 'ML', data = df)

# multivariate regression model 
pedagogy.lm.model <- lm(Final ~ Semester + Exam1 + Exam2 + Exam3 + HW + Quiz, data = df)

# Linearity: Added variable plots 
avPlots(pedagogy.lm.model, layout=c(5, 3))


# Independence: theoretical 
# talk about every semester being independent

# Normal
ggplot()+geom_histogram(mapping=aes(x=resid(object=pedagogy.model, type = "pearson"))) + labs(x="Standardized Residuals", y = "Count")
ks.test(resid(object = pedagogy.model, type = "pearson"),"pnorm")

# Equal variance 
ggplot(data =  df, mapping=aes(x=fitted(pedagogy.model), y = resid(object = pedagogy.model, type = "pearson"))) +
  geom_point() + 
  geom_smooth(method = lm) + labs(x="Fitted Values", y = "Residuals")

# Cross Validation 

source("predictgls.R") 

n <- 30
n.cv <- 100 #Number of CV studies to run
n.test <- 6 #Number of observations in a test set
rpmse <- rep(x=NA, times=n.cv) # Create an empty vector for rpmse
bias <- rep(x=NA, times=n.cv) # Create an empty vector for bias
wid <- rep(x=NA, times=n.cv) # Create an empty vector for width
cvg <- rep(x=NA, times=n.cv) # Creare an empty vector for coverage

for(cv in 1:n.cv){
  
  #Select test observations
  test.obs <- sample(x=1:n, size=n.test)
  
  #Split into test and training sets
  test.set <- df[test.obs,]
  train.set <- df[-test.obs,]
  
  #Fit a gls() using the training data
  train.gls <- gls(model=Final ~ Semester + Exam1 + Exam2 + Exam3 + HW + Quiz, weights = varFixed(value = ~NStudents), method = 'ML', data = df)
  
  #Generate predictions for the test set
  my.preds <- predictgls(train.gls, newdframe=test.set, level = 0.97)
  
  #Calculate bias
  bias[cv] <- mean(my.preds[,'Prediction']-test.set[['Final']])
  
  #Calculate RPMSE
  rpmse[cv] <- (test.set[['Final']]-my.preds[,'Prediction'])^2 %>% mean() %>% sqrt()
  
  #Calculate Coverage
  cvg[cv] <- ((test.set[['Final']] > my.preds[,'lwr']) & (test.set[['Final']] < my.preds[,'upr'])) %>% mean()
  
  #Calculate Width
  wid[cv] <- (my.preds[,'upr'] - my.preds[,'lwr']) %>% mean()
  
}

mean(rpmse)
#sd(df$Final)

mean(wid)
#range(df$Final)

# What class activities are associated with improved learning
summary(pedagogy.model)$tTable 

# Of those activities you identified as having a positive association on learning, how much of an impact does each activity have? 
# intervals(pedagogy.model, level=.95)
confint(pedagogy.model,level=.95)

# How well do the class activities currently explain learning? 
# Want r squared
summary(pedagogy.lm.model)$r.squared


#Did you identify any semesters that were better or worse in terms of student learning? If so, which and by how much?
# None of the semester effects were significant.. 
reduced.model <- gls(model=Final ~ Exam1 + Exam2 + Exam3 + HW + Quiz, weights = varFixed(value=~1/NStudents), method = 'ML', data = df)
anova(pedagogy.model,reduced.model)