#checking my working directory
getwd()

#importing csv file to our environment
space_missions1_1_ <- read_csv("space_missions1 (1).csv")

#view the entire file
View(space_missions1_1_)

#read headers of the dataset along with the data(upto 6 rows by default)
head(space_missions1_1_)

#read first 15 rows of the of the datset
head(space_missions1_1_,15)

#read the bottom rows of the dataset(last 6 rows by default)
tail(space_missions1_1_)

#read the bottom 30 rows of the dataset
tail(space_missions1_1_,10)

# No of Columns - Variables Count
ncol(space_missions1_1_)  

# No of rows - Observations
nrow(space_missions1_1_)

# DEtails regarding variable types
str(space_missions1_1_)

#quarters,mean,median and mode
summary(space_missions1_1_)

# Eliminating Column One which is serial numbers
space_missions1_1_=space_missions1_1_[-1] 
head(space_missions1_1_)

# Univariate Data Table
table(space_missions1_1_$Rocket)

# Univariate Data Table
table(space_missions1_1_$RocketStatus)

# Univariate Data Table
table(space_missions1_1_$MissionStatus)

# Bivariate Data Table
table(space_missions1_1_$Year,space_missions1_1_$Rocket)

table(space_missions1_1_$Rocket,space_missions1_1_$Price)

#PLOTS

# Lineplot
plot(space_missions1_1_$Year,type="l") 
space_missions1_1_

#barplot
barplot(table(space_missions1_1_$Year),col=c("Red","Green"))

barplot(table(space_missions1_1_$Year),col=c("Red","Yellow","Blue","Green"))
barplot(table(space_missions1_1_$Price),col=c("Red","Yellow","Blue","Green"))

# Pie chart
pie(table(space_missions1_1_$Year)) 
pie(table(space_missions1_1_$Price)) 
pie(table(space_missions1_1_$Year),col=c("Red","Yellow","Green","Blue"))

#chisquare statistical tests
# Assuming 'space_missions' is your dataset loaded in RStudio

# Perform chi-square test between two categorical variables
chisq.test(space_missions1_1_$Rocket,space_missions1_1_$MissionStatus)
chisq.test(space_missions1_1_$Year,space_missions1_1_$MissionStatus)
chisq.test(space_missions1_1_$Price,space_missions1_1_$MissionStatus)

#returns mean of the given variables
t.test(space_missions1_1_$Year,space_missions1_1_$Price) 

#mean difference
t.test(space_missions1_1_$Year,space_missions1_1_$Price,paired=TRUE)

#sample estimates
t.test(space_missions1_1_$Price,mu=70)

#single factor
testanova=aov(space_missions1_1_$Year~space_missions1_1_$Price) # Anova Single Factor 
summary(testanova)

#To compare the means of more than two groups
anova(lm(Price ~ Year, data = space_missions1_1_))

#correlation analysis
cor.test(space_missions1_1_$Price,space_missions1_1_$MissionStatus)

#To predict one numerical variable based on another
lm(Price ~ RocketStatus, data = space_missions1_1_ )

#logistical regression
#To predict one numerical variable based on another
glm(MissionStatus ~ Price + RocketStatus, data = space_missions1_1_  , family = "binomial")

#subset
R=subset(space_missions1_1_,space_missions1_1_$MissionStatus=="R") 
SAS=subset(space_missions1_1_,space_missions1_1_$MissionStatus=="SAS") 
SPSS=subset(space_missions1_1_,space_missions1_1_$MissionStatus=="SPSS") 
STATA=subset(space_missions1_1_,space_missions1_1_$MissionStatus=="Stata")
mean(R$Price)
mean(SAS$Price)
mean(SPSS$Price)
mean(STATA$Price)

#adding new columns
space_missions1_1_$Company <- 0 # Add a new column filled with zeros
print(space_missions1_1_)
# Display the updated dataset

space_missions1_1_$PayloadMass <- 0 # Add a new column filled with zeros
print(space_missions1_1_)

space_missions1_1_$DistanceKm <- 0 # Add a new column filled with zeros
print(space_missions1_1_)

#plots
#histogram
hist(space_missions1_1_$Price)
plot(space_missions1_1_$Price,space_missions1_1_$PayloadMass)

#data Filtering
subset(space_missions1_1_, Year== 1958)
subset(space_missions1_1_, Year== 1985)
subset(space_missions1_1_, Year== 1989)

#data transformation
space_missions1_1_$success_rate <- sum(space_missions1_1_$MissionStatus == 1) / nrow(space_missions1_1_)

#data aggregation
aggregate(PayloadMass ~ Company, data = space_missions1_1_, FUN = mean)

# Identify and handle missing data
na.omit(space_missions1_1_)

#Summary of the dataset
View(space_missions1_1_)
summary(space_missions1_1_)





