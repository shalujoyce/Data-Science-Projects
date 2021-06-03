##############################################################################################
R for Data Analysis Project on Super Market Sales
Prepared by Shalu Vavakunju
##############################################################################################
#Changing directory
##############################################################################################
setwd("C:\\Users\\joyce\\OneDrive\\Desktop\\DATASCIENCE\\R\\Project")
getwd()

##############################################################################################
# loading csv data to dataframe 
##############################################################################################
df<-read.csv("supermarket_sales.csv")
#train<-read.csv("supermarket_sales)

df[df=='']<-NA #converting Null to Na


##############################################################################################
#Getting familiar with data
##############################################################################################
dim(df) ###checking shape of the data
colnames(df) ###checking column names
str(df) ###checking data types
summary(df) ###checking summary of data

date <- as.Date(df$Date, "%m/%d/%Y") #converting character data type of Date column to Date data type

#It looks like the gross margin percentage column has only a singe value in all rows, 4.761904762
unique(df[c("gross.margin.percentage")])
# Drop the column gross margin percentage from data frame and we don't have any knowledge to extract meaningful features from this column
df[,c("gross.margin.percentage")]<-NULL


# Checking the head of the dataset
head(df)
head(df,5)


# Checking the tail of the dataset
tail(df)

# number of missing values:
colSums(is.na(df))
#Percentage of missing values:
#round(colMeans(is.na(df))*100,2)#2:digit=2

#No NA data available in my data set

#data cleansing is 1)Handling duplicate data 2)Handling Missing Values 3)Handling outliers
#make a copy
df_org<-df
##########################################################################################
#Duplicate Data
#########################################################################################
duplicated(df)# returns for you TRUE for observation that is duplicated otherwise returns FALSE
#Note: an observation considered duplicated if values of all features are exactly the same as another observation

#How many duplicated data are there?
sum(duplicated((df)))
r1<-which(duplicated(df))
df<-df[-r1,]

#make a copy
df_org1<-df


# number of missing values:
colSums(is.na(df))


#checking columns 
names(df)
#or
colnames(df)

#data manipulation- loading libraries
library(dplyr)
library(dplyr)
library(ggplot2)
library(magrittr)
install.packages("lubridate")
library(lubridate)
install.packages("plotrix")
library(plotrix)

###############################################################################################################
# Q1. calculate the average of the total column for each branch and also count the number of observations per group.

############################################################################################################
#We  want to get a general overview of sales at each branch. 
#The average sales amount and the number of sales at each branch can be calculated as follows:
library(dplyr)
sup_branch_total = group_by(df, Branch) # the observations are grouped by the branch column.
dplyr::summarise(sup_branch_total, Avg_total = mean(Total), qty = n()) # we can aggregate the the number of occurrence with n().

#summarise function is used to calculate the average of the total column for each branch and 
#the number of observations per group.

#Total Purchase Amount by branch 

#Continouse Vs. Categorical  : For summaraization: group by categorical column an aggregate for numerical column
#                              For visualization: Grouped box plot,...

Total_mean = aggregate(Total~ Branch, df , mean)#finding aggregate of Total grouped by mean
Total_mean

qplot(Branch, Total, data = df, 
      geom="boxplot", fill = Branch)






###############################################################################################

#Q2. Distribution of Branch Vs Product.line
##################################################################################################
#Bivariate analysis
#Categorical Vs categorical---grouped bar chart  
# Here we will check which product line  have more sales at different branches


ggplot(df, 
       aes(x = Branch, 
           fill = Product.line)) + 
  geom_bar(position = "dodge")

#bar chart is showing which product line is more on sale at each branch

#######################################################################################
#Q3. create a sales pattern based on Location
################################################################################################
#Continouse Vs. Categorical  : For summaraization: group by categorical column an aggregate for numerical column
#                              For visualization: Grouped box plot,...
p<-ggplot(df, aes(x=Total, fill=City, color=City)) +
  geom_histogram(position="identity", alpha=0.5)      # showing sales by location
p

q<-ggplot(df, aes(x=Total, fill=City, color=Product.line)) +
  geom_histogram(position="identity", alpha=0.5)        #showing sales by location for each product line
q





##############################################################################################################
#Q4. find out if Location or branches make more sale on a specific day of the week. 
##################################################################################################################

#Let's first create a column that contains the weekday of the dates. We will use the wday function of the 
#lubridate package which makes it easier to work with dates and times in R.
install.packages("lubridate")
library(lubridate)
df <- mutate(df, week_day = wday(date))

#The mutate function of the dplyr package allows for adding new columns based on existing ones. 
#The supermarket tibble now has a new column called week_day.
#We can create a bar plot to see the number of purchases per week day. 
#Here is how a bar plot is created using the ggplot2 package under tidyverse.
library(ggplot2)
ggplot(df) + geom_bar(mapping = aes(x=week_day, color=City), fill='white', position='dodge')
#The ggplot function accepts the data and creates an empty graph. 
#The second step adds a new layer on the graph based on the given mappings and plotting type. 
#The geom_bar function creates a bar plot. The color parameter differentiates the values based on the discrete values in the given column.
#The position parameter is set as "dodge" to put the bars for each category side-by-side.

ggplot(df) + geom_bar(mapping = aes(x=week_day, color=Branch), fill='white', position='dodge')

####################################################################################################################
#Q5.what is the distribution of the total sales amounts. 
###################################################################################################################

#It gives us an overview of how much customers are likely to spend per shopping.
#One option to check the distribution of a continuous variable is creating a histogram.
#It divides the value range into discrete bins and count the number of observations (i.e. rows) in each bin.
#We can use the geom_histogram function of the ggplot2 package to create a histogram as below.
ggplot(df) + geom_histogram(mapping = aes(x=Total, color=Gender), bins=15, fill='pink')
#The gender column is passed to the color parameter to check the distribution for males and females separately. 
#The bins parameter sets the number of bins.

######################################################################################################################

#Q6.what is the distribution of Product.line and is there any relation between Product.line and Gender?
###############################################################################################################
b=table(df$Product.line)
b

barplot(b,main="Using BarPlot to display Product Line Comparision",
        ylab="Count",
        xlab="Product.line",
        col=rainbow(2),
        legend=rownames(b),
        args.legend = list(x ='topright', bty='n', inset=c(-0.35,0)))
#Customers purchased Fashion accessories products more when  compared to other product lines

#to detect relationship, we will implement chisquare test for categorical vs Categorical
tbl <-table(df$Product.line,df$Gender)

tbl
                 # the contingency table

chisq.test(tbl)

#since p values is greater than .05, we failed to reject null hypothesis

############################################################################################################

#Q7.what is the gender distribution across our supermarket_sales dataset.
#############################################################################################################

a=table(df$Gender)

barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=rainbow(2),
        legend=rownames(a))

install.packages("plotrix")
library(plotrix)
pct=round(a/sum(a)*100)
lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
pie3D(a,labels=lbs,
      main="Pie Chart Depicting Ratio of Female and Male")

#############################################################################################################
#Q8.
# Visualize the distribution of Product.line by gender
unique(df$Product.line)
#using pipe operator to combine data set and function
df %>% 
  ggplot(aes(x = Gender, fill = Product.line)) +
  geom_bar(position = "fill")


###################################################################################################################
#Q9.Visualize the distribution of all ratings by gender
#will check how the rating goes with gender data
df %>%
  ggplot(aes(x = Gender, fill = factor(Rating))) + geom_bar(position = "fill")
         
################################################################################################################
#Q10.Analyse the Rating provided by Customers based on Customer.type

#Bivarate Analysis for continuous(Rating ) Vs. categorical (Customer.type)
# Visulization
summary(df$Rating)
boxplot(Rating ~ Customer.type,data=df, main="Rating by Customers",
        xlab="Customer.type", ylab="Ratingt",col="blue")

library(ggplot2)
qplot(Customer.type, Rating, data = df, 
      geom="boxplot", fill = Customer.type)

# Changing histogram plot fill colors by Customer.type and using semi-transparent fill
p<-ggplot(df, aes(x=Rating, fill=Customer.type, color=Customer.type)) +
  geom_histogram(position="identity", alpha=0.5)
p

# Add mean lines
library(plyr)
mu <- ddply(df, "Customer.type", summarise, grp.mean=mean(Rating,na.rm=T))
head(mu)
p<-p+geom_vline(data=mu, aes(xintercept=grp.mean, color=Customer.type),
                linetype="dashed")
p


######################################################################################################

