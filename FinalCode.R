
#Packages ----
library(tidyverse)
library (tree)
library(MASS)
library(car)

# Data ----
df.r <- read_rds("Data/fundraising.rds")

attach(df.r)

#Partition ----
set.seed(12345)
partion <- .80
tt <- sample(nrow(df.r), nrow(df.r)*partion)
df.r.train <- df.r[tt,]
df.r.test <- df.r[-tt,]


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
  bc<- boxcox(vector+1~x)
  lambda <- bc$x[which.max(bc$y)]
  transform.y <- ((vector)^lambda-1)/lambda
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
range(num_child) #Ranges from 1 to 5
table(num_child, target)
# It seems most families have 1 child. Surprising there are no households that have zero children.
# Like Female and Homeowner there is some information contained here.
#Since the distance between the levels of children are the same I will keep it a numeric.

### Income ----
range(income) #Ranges from 1 to 7
table(income, target)
# It seems that higher income could lead to more donations but has a small effect. 


### Wealth ----
range(wealth) #Ranges from 0 to 9
table(wealth, target)
# It seems rich people will donate more. In wealth 7 there is a big difference. In wealth less than 5 Most people do not donate.
# Perhaps I should make a dummy variable at the less than 5 or greater than 5 for rich and poor.


### Home Value ----
#### Univariate ----
#This is home neighberhood average value.
range(home_value) #from to 0 to 5945
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
range(med_fam_inc) # From 0 to 1500
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
range(avg_fam_inc) #From 0 to 1331
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
range(pct_lt15k) #From 0 to 90 percent
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
range(num_prom) #11 to 157
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
