# Exam I
# Write R code to answer the questions and 
# save this file as Yourname_ExamI.R
# Submit it on Canvas before 6:30 PM
# under Modules --> Exams --> Exam I.


##### Question 1 -- 2 points
# Write R code to create the data frame given 
# to you in class. Assign data types as 
# asked in the question. 
Ranking<-c(1,2,3,4,5,6,7,8,9,10)
University<-c("Texas A&M University", "University of Central Florida", "Ohio State University",
              "University of Florida","University if Illinois Urbana-Champaign",
              "University of Minnesota,","Arizona State University", "Florida International University",
              "University of Texas at Austin", "Michigan State University")
City<-c("College Station","Orlando","Columbus","Gainesville","Urbana-Champaign","Minneapolis",
        "Tempe","Miami", "Austin","East Lansing")
State<-c("TX", "FL", "OH", "FL", "IL", "MN", "AZ", "FL", "TX", "MI")
TX_or_FL<- c(TRUE, TRUE, FALSE, TRUE, FALSE,FALSE, FALSE, TRUE,TRUE,FALSE)
Enrollments<-c(79114,68442,67620,61112,60540,57688,56644,55687,54955,52384)
mydata<-data.frame(Ranking,University,City,State,TX_or_FL,Enrollments)
State<-factor(State)
TX_or_FL<-as.logical(TX_or_FL)
Ranking<-as.character(Ranking)

# The remaining questions are for 0.4 points each
# unless otherwise stated
# WRITE R CODE AND COPY YOUR OUTPUT AS COMMENTS
# WHEREVER FEASIBLE


##### Question 2
# create a subset from (1) where state is FL
# and the enrollment is more than 60,000.
# You can use any method you want.
FloridaOnly<-subset(mydata,State=="FL" & Enrollments>60000 ,select = c(State, Enrollments))

##### Question 3
# create a subset from (1) where TX_or_FL is not TRUE
# and the enrollment is less than 60,000.
# You can use any method you want.
NotTrue<-subset(mydata, TX_or_FL != TRUE & Enrollments<60000, select = c(TX_or_FL, Enrollments))

# Questions 4 to 11 are based on the datafile iris.csv.
# It lists the SEPAL.Length, SEPAL.Width, PETAL.Length,
# PETAL.Width, and the 3 species of iris flower


##### Question 4 -- 0.8 points
# read the file iris.csv as my_iris
# Make sure that iris.csv is in your working directory
# Change the variable names so that they are in 
# lowercase without any period. Remove space ' ' and
# 'cm' from sepal length variable and convert the variable into
# numeric data type
irisdata<-read.csv("iris.csv")
na;mes(irisdata)<-tolower(gsub("[. ]", "", names(irisdata)))
irisdata$sepallength<-as.numeric(gsub("[cm]", "", irisdata$sepallength ))

##### Question 5
# Find the number of rows, columns, 
# and data types for each variables
# Write R code and copy the output as comments
str(irisdata)
# data.frame':	150 obs. of  5 variables:
# $ sepallength: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# $ sepalwidth : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# $ petallength: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# $ petalwidth : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# $ species    : chr  "Setosa" "Setosa" "Setosa" "Setosa" ...
nrow(irisdata)
# [1] 150

##### Question 6
# Create boxplot for petal length for the 3 species. 
# Give appropriate title and axis labels.
# How does the petal length differs by species?
boxplot(irisdata$petallength~irisdata$species,
        main= "Petal Length Across Species",
        xlab= "Species",
        ylab= "Petal Length",
        col= "lavender")
#The species Virginica has a significantly longer petal length compared to
#Veriscolor, which is betwen Virginica and Setosa, and Setosa


##### Question 7
# Create a scatter plot with petal width on the X axis
# and petal length on the y axis with appropriate title and
# axis labels
# What is the relationship between the two variables?
plot(irisdata$petalwidth, irisdata$petallength,
     main = "Petal Length vs Petal Width",
     xlab = "Petal Width",
     ylab = "Petal Lenth",
     pch = 19,
     col = "limegreen")
cor(irisdata$petallength,irisdata$petalwidth, use = "complete.obs")
# [1] 0.9628654
# With the correlation being 0.9628654, it is safe to say that the two variables
# has a strong relationship



##### Question 8
# Use tapply to find the median petal length for all 3 species
filtered<-subset(irisdata, petallength>0 & species >999)
tapply(filtered$petallength,filtered$species,median,na.rm=TRUE)
#     Setosa   Versicolor  Virginica 
#      1.50       4.35       5.55 

##### Question 9
# Create a subset where petal length > 1.5 and species is Setosa
# and sepal length > 5
# how many flowers are there in this subset?
answer9<-subset(irisdata, species == "Setosa" & petallength>1.5 
                | sepallength>5)
nrow(answer9)
# [1] 125


##### Question 10
# Count the number of flowers where petal length > 1.5 and the species
# is NOT Setosa
answer10<-subset(irisdata, petallength>1.5 & species != "Setosa")
nrow(answer10)
# [1] 100


##### Question 11
# Count the number of flowers where petal length > 1.5 and the species
# is NOT Setosa
answer11<-subset(irisdata, petallength>1.5 & species != "Setosa")
nrow(answer11)
# [1] 100

# Remaining questions are based on sales_data_USA.csv.
# It consists of transactions of a retail chain in 3 states and includes
# revenue from transaction, units of items sold, product categories,
# and other customer information.
# The variable names are self-explanatory.


##### Question 12 -- 0.8 points
# Read the file.
# remove '$' and space from Revenue and convert it
# to numeric data type
salesdata<-read.csv("sales_data_USA.csv")
salesdata$Revenue<-as.numeric(gsub("[$]", "", salesdata$Revenue))

#### Question 13
# List the number of transactions from each of the 3 states
# given in the dataset
table(salesdata$State)
#   CA   OR   WA 
#  2733 2262 4567 


##### Question 14
# create histogram for Revenue and give
# appropriate title and axis labels
hist(salesdata$Revenue,
     main = "Revenue Histogram",
     xlab = "Revenue",
     ylab = "Amount ($)",
     col = "yellow")

##### Question 15
# how many observations are there in the state of CA
# where Revenue is greater than or equal to 10
California<-subset(salesdata, State == "CA" & Revenue >= 10)
nrow(California)
# [1] 1484

##### Question 16
# Create a scatter plot with UnitsSold on the x axis
# and Revenue on the y axis. Give appropriate title
# and axis labels.
# Comment on the relationship between the two variables
plot(salesdata$UnitsSold, salesdata$Revenue,
     main = "Units Sold vs Revenue",
     xlab =  "Units Sold",
     ylab = "Revenue ($)",
     pch = 19,
     col = "orange")
cor(salesdata$UnitsSold, salesdata$Revenue, use = "complete.obs")
# [1] 0.2981467
# Units Sold and Revenue have a very weak correlation.

##### Question 17
# What is the average Revenue for transaction
# where the number of children is 3 and state is CA
mean(salesdata$Revenue[salesdata$Children == "3" & salesdata$State== "CA"], na.rm = TRUE)
# [1] 13.3978

##### Question 18
# List the number of transactions for each Product 
# for the state of CA
table(salesdata$Product, salesdata$State =="CA")
#                TRUE
# Drink           258
# Food           1974
# Non-Consumable  501

##### Question 19
# Find average number of children of Female customers where
# Category of transaction is Vegetables
mean(salesdata$Children[salesdata$Gender == "F"& salesdata$Category == "Vegetables"], na.rm = TRUE)
# [1] 2.598639