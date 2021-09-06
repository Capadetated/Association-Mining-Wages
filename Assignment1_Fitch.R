# This program contains all the R code used to perform association rules analysis on the Wages dataset
# Data 630 Week 3 Module 2 Assignment 1
# Ted Fitch
# Last updated 08JUN21

# Section 1 - Introduction/Installing Packages /////
# Set directories/libraries
install.packages("arules")
library("arules")
#install arulesViz package only once
install.packages("TSP")
install.packages("data.table")
install.packages("arulesViz")
library("arulesViz")

# Section 2 - Upload and Explore File /////
# Set working directory
setwd("C:/Users/soari/Documents/Assignments/Data Analytics/UMGC/Summer 2021 Data 630/Assignment 1")

# Pull the data 
w=read.table("wages.csv",sep=",",header=TRUE)

# Explore the data
str(w)
summary(w)
View(w)

# barplot of sex distribution
x<- w$sex
cnt <- table(x)
cnt
barplot (cnt,main="Distribution of Sex",
         xlab="Sex (1 = female)",
         ylab="Count",
         border="brown",
         col="brown", space =1.0,beside=TRUE,ylim=range(pretty(c(0, cnt))))

# barplot of marriage distribution
x<- w$marital_status
cnt <- table(x)
cnt
barplot (cnt,main="Distribution of Marital Status",
         xlab="Marital Status (1 = married)",
         ylab="Count",
         border="brown",
         col="brown", space =1.0,beside=TRUE,ylim=range(pretty(c(0, cnt))))

# Histogram of wages
hist(w$wage,main="Distribution of Wages",
     xlab="Wage (dollars per hour)",
     ylab="Frequency",col="brown")


# Section 3 - Preprocessing /////
# Check for null values:
Nw<-subset(w, complete.cases(w))
# Use square brackets
Nw<-w[complete.cases(w),]
# Use na.omit
Nw<-na.omit(w)
# Display the Rows with Missing Values
w[!complete.cases(w),]
nrow(w[!complete.cases(w),])
# Number of Missing Values in a Data Row
apply(w, 1, function (w) sum(is.na(w)))
# Number of Missing Values for Each Variable
apply(w, 2, function (w) sum(is.na(w)))

# No ID variable to remove

# Turn numeric variables into factors
# Show type of variables
# Individually
summary(w$sex)
summary(w$union)
summary(w$race)
summary(w$occupation)
summary(w$sector)
summary(w$marital_status)
# All at once:
str(w)

# Transform variables
w$south<-factor(w$south)
w$sex<-factor(w$sex)
w$union<-factor(w$union)
w$race<-factor(w$race)
w$occupation<-factor(w$occupation)
w$sector<-factor(w$sector)
w$marital_status<-factor(w$marital_status)

# Show type of variables after conversion
# Individually
summary(w$south)
summary(w$sex)
summary(w$union)
summary(w$race)
summary(w$occupation)
summary(w$sector)
summary(w$marital_status)
# All at once:
str(w)

# Discretization
w$age<-discretize(w$age, method="interval", breaks=6)
w$wage<-discretize(w$wage, method="interval", breaks=6)
# Specific bins chosen for edu to account for edu levels/degrees
w$education<-discretize(w$education, method="fixed", breaks=c(-Inf, 12, 14, 16, Inf))
w$experience<-discretize(w$experience, method="interval", breaks=6)
# Shows breakdown / that discretization worked
summary(w)

# Section 4 - Run the method with default parameters /////
rules<-apriori(w)
rules
inspect(rules)
inspect(rules[1:10])
inspect(rules[10:20])

# Section 5 - Different support and confidence values /////
rules <- apriori(w, parameter= list(supp=0.4, conf=0.7))
rules
inspect(rules[1:10])

# Section 6 - Display specific measures /////
# wages
rules<-apriori(w, parameter= list(supp=0.1, conf=0.7), appearance=list(rhs=c("wage=[1,8.25)", "wage=[8.25,15.5)", "wage=[15.5,22.8)", "wage=[22.8,30)", "wage=[30,37.2)", "wage=[37.2,44.5]"), default="lhs"))
inspect(rules)
rules
inspect(rules[1:10])
inspect(rules[10:20])
summary(w$wage)

# Section 7 - Pruning rules /////
#Sort the rules by lift
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#Remove the redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm=T) >= 1
subset.matrix
redundant
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
summary(rules.pruned)
inspect(rules.pruned[1:10])

# Section 8 - Plot the rules /////
plot(rules.pruned)
plot(rules.pruned, method="graph", control=list(type="items"))
plot(rules.pruned, method="paracoord", control=list(reorder=TRUE))
plot(rules.pruned, method = "grouped")
plot(rules.pruned, method="matrix", measure=c("lift", "confidence"))


# End of script