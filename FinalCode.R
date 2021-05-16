
#Packages ----
library(tidyverse)
library(MASS)
library(car)
library(caret) # For modeling
library(rpart.plot) # For plotting trees
# Data ----
df.r <- read_rds("Data/fundraising.rds")

attach(df.r)



#EDA ----
sum(is.na(df.r)) # no missing data
summary(df.r)


## Categorical Variables ----
#Zipcode is spatial information kindof. 
#Each group is the thousandth unit. Unfortunately because it is not the exact zipcode it would be hard to make use of these variables
#If i had the exact zipcode we could for example get spatial info to help in the analysis. For example maybe veteran population.
table(zipconvert2) #Mostly No few yes
table(zipconvert3) #Mostly No few Yes
table(zipconvert4) #Mostly No few Yes
table(zipconvert5) #Even amount of no and yes

### Zipcode Contingency Tables ----

table(zipconvert2, target)
table(zipconvert3, target)
table(zipconvert4, target)
table(zipconvert5, target)
# Zipcode seems to be split evenly among donors and no donors solidifying this variables might not belong in a model.


### Home Owner Contingency Tables ----
table(homeowner, target)
# Same with homeowner seems to be close. We imagine that homeowners own more money.
chisq.test(table(homeowner, target))
#Running Chi-Square Test we get a p value is .15 might mean they are not independent but it is weak relationship

### Female Contingency Tables ----
table(female, target)
# Females seem to slightly donate more often so this variable may be included in the model
chisq.test(table(female, target))
#Running Chi-Square Test we get a p value is .18 might mean they are not independent but it is weak relationship


## Accessing Normality ----
# I am going to do this a lot so I made a function to help me out. It does a histogram and qqplot as well as the Shapiro-Wilk test
Normal <- function(vector){
  hist(vector, main = paste("Histogram of", deparse(substitute(vector))))
  qqPlot(vector, main = paste("qqplot of", deparse(substitute(vector))))
  shapiro.test(vector)
}

BoxCox.Transform <- function(vector){
  #Adds 1 to the data then does a box cox transform
  #Only works if the data is postive
  x <- 1:length(vector)
  y <- vector+1
  bc<- boxcox(y~x)
  lambda <- bc$x[which.max(bc$y)]
  transform.y <- ((y)^lambda-1)/lambda
  transform.y
}

## Accessing Relationship ----

Relationship <- function(vector){
  H <- ggplot(data = df.r, aes(x = vector, fill = target)) +
    geom_histogram() +
    labs(x = deparse(substitute(vector)))
  B <- ggplot(data = df.r, aes(x = vector, fill = target)) +
    geom_boxplot() +
    labs(x = deparse(substitute(vector)))
  A <- anova(lm(vector ~ target))
  K <- kruskal.test(vector ~ target)
  print(H)# For histogram
  print(B)# For Boxplot
  print(A)# For anova
  print(K)# For kruskal test
}

ncol(select_if(df.r, is.numeric)) # There are 14 numeric columns
## Numeric Variables ----
### Number of Children ----
summary(num_child) #Ranges from 1 to 5
table(num_child, target)
# It seems most families have 1 child. Surprising there are no households that have zero children.
# Like Female and Homeowner there is some information contained here.
#Since the distance between the levels of children are the same I will keep it a numeric.
### Income ----
summary(income) #Ranges from 1 to 7
table(income, target)
# It seems that higher income could lead to more donations but has a small effect. 



### Wealth ----
summary(wealth) #Ranges from 0 to 9
table(wealth, target)
# It seems rich people will donate more. In wealth 7 there is a big difference. In wealth less than 5 Most people do not donate.
# Perhaps I should make a dummy variable at the less than 5 or greater than 5 for rich and poor.


### Home Value ----
#### Univariate ----
#This is home neighberhood average value.
summary(home_value) #from to 0 to 5945
# It is strange that home_value is zero. These might be outliers. One possible explanation is living on a military base.
sum(home_value == 0) #only 22 observation have 0 home_value

Normal(home_value)
#The histogram is very right skewed. Very not normal. Perhaps doing a log transformation on it.
Normal(log(home_value + 1))
#Doing a log transformation improved but still not normal let's try a box cox transformation
Normal(BoxCox.Transform(home_value))
# BoxCox doesn't not seem too different then just a log. We will be just using a log since that is simpler


#### Relationship with Donor ----
Relationship(home_value)
#There does not seem to be a difference between the values of home_value and donors
Relationship(log(home_value + 1))
# Even Transformed there is does not seem to be a relationship between home_value and donors


### Median Family Income ----
#### Univariate ----
summary(med_fam_inc) # From 0 to 1500
sum(med_fam_inc == 0) # Only 18 have a zero
#Perhaps outliers

Normal(med_fam_inc)
#This histogram is right skewed. And med_fam_inc is not normal as well

Normal(log(med_fam_inc +1))
Normal(sqrt(med_fam_inc))
Normal(BoxCox.Transform(med_fam_inc))
#Various transformations help but do not make the data normal
#Dropping zeros
Normal(BoxCox.Transform(med_fam_inc[med_fam_inc > 0]))
#Dropping the zeros seems to have helped the data become more normal. Might end up doing that for model. 

#### Relationship with Donor ----

Relationship(med_fam_inc)
Relationship(log(med_fam_inc +1))
#Again there does not seem to be much of a relationship with med_fam_inc with the Target. This histogram and boxplots are right
#on top of eachother. THe tests also fail to reject of difference. This variable may not be helpful in modeling.




### Average Family Income ----
#### Univariate ----
summary(avg_fam_inc) #From 0 to 1331
sum(avg_fam_inc == 0)
#Zeros here 18
Normal(avg_fam_inc)
#more Right skewed data.
Normal(sqrt(avg_fam_inc+1))
Normal(log(avg_fam_inc+1))
Normal(BoxCox.Transform(avg_fam_inc))
#Again these transforms help with normality but don't fix it.
#### Relationship with Donor ----
Relationship(avg_fam_inc)
#Once again avg_fam_inc does not not look good for explaining donation
Relationship(BoxCox.Transform(avg_fam_inc))
# Transforming does not help.






#Percent Earning less then 15k ----
summary(pct_lt15k) #From 0 to 90 percent
sum(pct_lt15k == 0)
# 174 observations that have zero meaning very rich neighborhood

#### Univariate ----
Normal(pct_lt15k)
#Very right skewed data. Lots more rich neighborhoods vs poor.

Normal(BoxCox.Transform(pct_lt15k))
Normal(log(pct_lt15k + 1))
Normal(sqrt(pct_lt15k))
#Does not make the data normal but helps. I think sqrt is the best one here.

#### Relationship with Donor ----
Relationship(pct_lt15k)
#There does not seem to be a strong relationship with donors. The distributions overlap
Relationship(sqrt(pct_lt15k))

#Since the above variables were based on spatial information let's see if they are correlated with each other

cor(cbind(home_value, med_fam_inc, avg_fam_inc, pct_lt15k))
# Yes as expected this variables are correlated with eachother. Based on the results so far I probably would not model with these variables.



#Lifetime Number of Promotions Received ----
#### Univariate ----
summary(num_prom) #11 to 157
# That means everyone one in this data set has received marketing before. That is not good since the model may not be good 
# for predicting new donors.

Normal(num_prom)
#Right skewed data not normal.
Normal(log(num_prom))
Normal(BoxCox.Transform(num_prom))
Normal(sqrt(num_prom))
#Transforming helps a tiny bit but not really.

#### Relationship with Donor ----
Relationship(num_prom)
#Yahoo! We got our first variable that tells us something about donors. As number of Promotions increases we get more donors.
#Which that makes sense.
Relationship(sqrt(num_prom))
#Transforming the data does not change the result




#Dollar amount of lifetime gifts to date ----
#### Univariate ----
summary(lifetime_gifts) #Ranges from 15 to 5674.9
#Those large values have got to be outliers.

Normal(lifetime_gifts)
#Not normal right skewed
Normal(log(lifetime_gifts))
Normal(BoxCox.Transform(lifetime_gifts))
#Transforming helps a lot here. BoxCox helps a lot here.

#### Relationship with Donor ----
Relationship(lifetime_gifts)
#Interestingly ANOVA says no while Kruskal Wallis says yes. Let's try transformed
Relationship(BoxCox.Transform(lifetime_gifts))
#Yeah there is a slight increase with lifetime gifts goes up.





#Dollar amount of largest gift ----
#### Univariate ----
summary(largest_gift) #range 5 to 1000

Normal(largest_gift) #Not normal most people donate small amounts of money
Normal(log(largest_gift))
Normal(BoxCox.Transform(largest_gift))
##### Relationship with Donor ----
Relationship(largest_gift)
Relationship(BoxCox.Transform(largest_gift))
#Interestingly it seems that Largest gift could slightly lean towards not donating. This could be the case if you
#had just a big spender who only makes one donation.

# Dollar amount of most recent gift ----
#### Univariate ----
summary(last_gift) #From 0 to 219
#No really high numbers here. This means that big spenders donated again.

Normal(last_gift)
#not normal of course.

Normal(log(last_gift+1)) #log helps

##### Relationship with Donor ----
Relationship(last_gift)
Relationship(log(last_gift+1))
#Seems that if the last gift someone gave was a large amount then are less likely to donate again which make sense

#Numbers of Months Since last donation ----
##### Univariate ----
summary(months_since_donate) #From 17 to 37
Normal(months_since_donate)
#Interesting maybe my most left skewed variable. Most people will wait longer.

Normal(sqrt(max(months_since_donate+1) - months_since_donate)) #Unique trasform for left skewed data. Helps a tiny bit

##### Relationship with Donor ----
Relationship(months_since_donate)
Relationship(sqrt(max(months_since_donate+1) - months_since_donate))

# This one is harder to interpret. But I think the longer people wait will donate more. Or it means that donation people
#donate at a usually time. I am not sure.


# Number of months between first and second gift ----
##### Univariate ----
summary(time_lag) #Ranges to 0 to 77
#Most people donate in a short time frame, less than a year
Normal(time_lag)
#More right skewed not normal data.
Normal(log(time_lag + 1))# Log helps a lot.
Normal(BoxCox.Transform(time_lag)) #BoxCox does help
#log helps more I think.

##### Relationship with Donor ----
Relationship(time_lag) #Okay seems like there is not difference

Relationship(log(time_lag + 1)) # Yep seems to be very similar






# Average dollar amoung of gifts to date ----
#### Univariate ----
summary(avg_gift) #From 2.139 to 122.167

Normal(avg_gift)
#More right skew not normal

Normal(log(avg_gift)) #Seems to have worked fine

Normal(BoxCox.Transform(avg_gift)) #WOW boxcox got it really close.

##### Relationship with Donor ----

Relationship(avg_gift) #seems the larger the gift less likely to donate again maybe
Relationship(BoxCox.Transform(avg_gift))
#Yep that seems to be the case that the larger the gift less likely to donate again and this agrees with previous variables






## Handling Outliers ----
# We have seen that there are outliers in our dataset. #We will be using mahalanobis distance to drop outliers.
#This is done by using the numeric variables and geting rid of values that have very high or low distances. But since
#we are unsure how this might effect the models we will create a copy dataset.
df.r.num <- select_if(df.r,is.numeric)

df.r.num$mahal <- mahalanobis(df.r.num, colSums(df.r.num), cov(df.r.num))

Out_ind <- which(df.r.num$mahal %in% boxplot.stats(df.r.num$mahal)$out)
length(Out_ind) #61 outiler observations
df.r.o<- df.r[-Out_ind,]

#Modeling ----
##Variable Selection ----

## Data with Outliers ----
#Based off my analysis of each of the variables I will be selecting the ones I feel provide insight into modeling
df.m <- df.r%>% 
  dplyr::select(homeowner, female, num_child, income, wealth, num_prom, lifetime_gifts, largest_gift, last_gift, months_since_donate, avg_gift, target)

#Creating a separate dataset for modeling that is based off transformed data
df.m.t <- df.m %>% 
  mutate(wealth.rich = ifelse(wealth > 5, 1, 0),
         num_prom.log = log(num_prom),
         lifetime_gifts.bc = BoxCox.Transform(lifetime_gifts),
         largest_gift.bc =  BoxCox.Transform(largest_gift),
         last_gift.log = log(last_gift + 1),
         months_since_donate.sqrt = sqrt(max(months_since_donate+1) - months_since_donate),
         avg_gift.bc = BoxCox.Transform(avg_gift)) %>% 
  dplyr::select(-wealth, -num_prom, -lifetime_gifts, -largest_gift, -last_gift, -months_since_donate, -avg_gift)
## Data Without Outliers ----
df.m.o <- df.r.o%>% 
  dplyr::select(homeowner, female, num_child, income, wealth, num_prom, lifetime_gifts, largest_gift, last_gift, months_since_donate, avg_gift, target)

df.m.o.t <- df.m.o %>% 
  mutate(wealth.rich = ifelse(wealth > 5, 1, 0),
         num_prom.log = log(num_prom),
         lifetime_gifts.bc = BoxCox.Transform(lifetime_gifts),
         largest_gift.bc =  BoxCox.Transform(largest_gift),
         last_gift.log = log(last_gift + 1),
         months_since_donate.sqrt = sqrt(max(months_since_donate+1) - months_since_donate),
         avg_gift.bc = BoxCox.Transform(avg_gift)) %>% 
  dplyr::select(-wealth, -num_prom, -lifetime_gifts, -largest_gift, -last_gift, -months_since_donate, -avg_gift)

##Partition ----
set.seed(12345)
partion <- .80
tt <- sample(nrow(df.m), nrow(df.m)*partion)
df.m.train <- df.m[tt,]
df.m.test <- df.m[-tt,]

df.m.t.train <- df.m.t[tt,]
df.m.t.test <- df.m.t[-tt,]



#For fun we will make a train and test split with all the variables
df.r.train <- df.r[tt,]
df.r.test <-  df.r[-tt,]

### Parition for Outliers ----
tt <- sample(nrow(df.m.o), nrow(df.m.o)*partion)

df.r.o.train <- df.r.o[tt,]
df.r.o.test <- df.r.o[-tt,]

df.m.o.train <- df.m.o[tt,]
df.m.o.test <- df.m.o[-tt,]

df.m.o.t.train <- df.m.o.t[tt,]
df.m.o.t.test <- df.m.o.t[-tt,]

## Logistic Regression ----
#A good base line let's do a logistic Regression
### Model without transformation ----
train_control <- trainControl(method = "cv", number = 10)
model.log.m <- train(target ~ .,
               data = df.m.train,
               trControl = train_control,
               method = "glm",
               family=binomial())

model.log.m.pred <- predict(model.log.m, newdata = df.m.test)

confusionMatrix(model.log.m.pred, df.m.test$target) # Acc .55

### Model with transformations ----

model.log.t <- train(target ~ .,
               data = df.m.t.train,
               trControl = train_control,
               method = "glm",
               family=binomial())

model.log.t.pred <- predict(model.log.t, newdata = df.m.t.test)

confusionMatrix(model.log.t.pred, df.m.t.test$target) #Acc .55
#Seems in logistic regression is getting a 55% accuracy with both the transformed data and non transformed data

### Model with all variables No Transform ----

model.log.r <- train(target ~ .,
                 data = df.r.train,
                 trControl = train_control,
                 method = "glm",
                 family=binomial())

model.log.r.pred <- predict(model.log.r , newdata = df.r.test)

confusionMatrix(model.log.r.pred, df.r.test$target)  #Acc .5433

### Model with no outliers ----
model.log.m.o <- train(target ~ .,
               data = df.m.o.train,
               trControl = train_control,
               method = "glm",
               family=binomial())

model.log.m.o.pred <- predict(model.log.m.o, newdata = df.m.o.test)

confusionMatrix(model.log.m.o.pred, df.m.o.test$target) #Acc .5663

### Model with no outliers with Transform ----
model.log.m.o.t <- train(target ~ .,
                       data = df.m.o.t.train,
                       trControl = train_control,
                       method = "glm",
                       family=binomial())

model.log.m.o.t.pred <- predict(model.log.m.o.t, newdata = df.m.o.t.test)

confusionMatrix(model.log.m.o.t.pred, df.m.o.t.test$target) # Acc .5612

### Model with no outliers all variables ----
model.log.r.o <- train(target ~ .,
                         data = df.r.o.train,
                         trControl = train_control,
                         method = "glm",
                         family=binomial())

model.log.r.o.pred <- predict(model.log.r.o, newdata = df.r.o.test)

confusionMatrix(model.log.r.o.pred, df.r.o.test$target) # Acc .5476



### Summary ----
#Running logistic regression we get a 55% accuracy with the variables we selected and transformed. Compared to logistic regression that was ran with all the variables we gained 1%.
#When we remove outliers we get an increase in accuracy to .5663 in the un-transformed data and .5612 in the transformed data
# When using all the variables there is only a slight increase in accuracy with the all variables model. It looks like removing the outliers did our model good.



## Decision Tree ----
#After running a logistic model and getting a highest Accuracy .5663 let's see how decision trees compares.


### Model with no transformations ----
train_control.tree <- trainControl(method="repeatedcv", number=10, repeats=3)

model.tree.m = train(target ∼ ., data=df.m.train, trControl=train_control.tree,method='rpart')


rpart.plot(model.tree.m$finalModel)

model.tree.m.pred <- predict(model.tree.m, newdata = df.m.test)

confusionMatrix(model.tree.m.pred , df.m.test$target) # Got .5383 accuracy


### Model with transformations ----
model.tree.m.t = train(target ∼ ., data=df.m.t.train, trControl=train_control.tree,method='rpart')


rpart.plot(model.tree.m.t$finalModel)

model.tree.m.t.pred <- predict(model.tree.m.t, newdata = df.m.t.test)

confusionMatrix(model.tree.m.t.pred , df.m.t.test$target) # Got .5383 accuracy


### Model with all variables  ----
model.tree.r <- train(target ~ .,
                     data = df.r.train,
                     trControl = train_control.tree,
                     method = "rpart")

model.tree.r.pred <- predict(model.tree.r , newdata = df.r.test)

confusionMatrix(model.tree.r.pred, df.r.test$target) #.5383 accuracy


### Model with no outliers ----
model.tree.m.o <- train(target ~ .,
                       data = df.m.o.train,
                       trControl = train_control.tree,
                       method = "rpart")

model.tree.m.o.pred <- predict(model.tree.m.o, newdata = df.m.o.test)

confusionMatrix(model.tree.m.o.pred, df.m.o.test$target) #Acc .5493
#getting rid of outliers hurt the model. Which makes sense the model is not seeing those types of observations.

### Model with no outliers with Transform ----
model.tree.m.o.t <- train(target ~ .,
                         data = df.m.o.t.train,
                         trControl = train_control.tree,
                         method = "rpart")

model.tree.m.o.t.pred <- predict(model.tree.m.o.t, newdata = df.m.o.t.test)

confusionMatrix(model.tree.m.o.t.pred, df.m.o.t.test$target) # Acc .5493

### Model with no outliers all variables ----
model.tree.r.o <- train(target ~ .,
                       data = df.r.o.train,
                       trControl = train_control.tree,
                       method = "rpart")

model.tree.r.o.pred <- predict(model.tree.r.o, newdata = df.r.o.test)

confusionMatrix(model.tree.r.o.pred, df.r.o.test$target) # Acc .5646

### Summary ----
# Transforming variables also didn't have an effect. The best model here was model with no outliers with .5646 acc
#with underperfroms from logisitc regression nest model.


## Random Forest ----
#Now let's see how random forest compares

train_control.forest <- trainControl(method="repeatedcv", number=10)

### Model with no transformations ----


model.forest.m = train(target ∼ ., data=df.m.train, 
                       trControl=train_control.forest,
                       method='rf')



model.forest.m.pred <- predict(model.tree.m, newdata = df.m.test)

confusionMatrix(model.forest.m.pred , df.m.test$target) #.5383 accuracy


### Model with transformations ----
model.forest.m.t = train(target ∼ ., data=df.m.t.train, trControl=train_control.forest,method='rf')



model.forest.m.t.pred <- predict(model.forest.m.t, newdata = df.m.t.test)

confusionMatrix(model.forest.m.t.pred , df.m.t.test$target) #.55 accuracy


### Model with all variables  ----
model.forest.r <- train(target ~ .,
                      data = df.r.train,
                      trControl = train_control.forest,
                      method = "rf")

model.forest.r.pred <- predict(model.forest.r , newdata = df.r.test)

confusionMatrix(model.forest.r.pred, df.r.test$target) #.5617


### Model with no outliers ----
model.forest.m.o <- train(target ~ .,
                        data = df.m.o.train,
                        trControl = train_control.forest,
                        method = "rf")

model.forest.m.o.pred <- predict(model.forest.m.o, newdata = df.m.o.test)

confusionMatrix(model.forest.m.o.pred, df.m.o.test$target) #.5578

### Model with no outliers with Transform ----
model.forest.m.o.t <- train(target ~ .,
                          data = df.m.o.t.train,
                          trControl = train_control.forest,
                          method = "rf")

model.forest.m.o.t.pred <- predict(model.forest.m.o.t, newdata = df.m.o.t.test)

confusionMatrix(model.forest.m.o.t.pred, df.m.o.t.test$target)  #.5578

### Model with no outliers all variables ----
model.forest.r.o <- train(target ~ .,
                        data = df.r.o.train,
                        trControl = train_control.forest,
                        method = "rf")

model.forest.r.o.pred <- predict(model.forest.r.o, newdata = df.r.o.test) #.517

confusionMatrix(model.forest.r.o.pred, df.r.o.test$target) 

### Summary ----


