# Load packages for analysis and this section will have all the required libraries mentioned for better clarity
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('rpart') # for decision tree
library('ROCR')
library('rpart.plot')
library('ROCR')
library('randomForest')
library('corrr')
library('corrplot')
library('glue')
library('caTools')
library('data.table')
require("knitr")
require("geosphere")
require("gmapsdistance")
require("tidyr")
#source("distance.R")
library('car')
library('caret')
library('gclus')
library('visdat')
library('psych')
library('leaflet')
library('leaflet.extras')

## Load the data

coffe_review_data <- read.csv("Coffeeinput.csv")

coffe_table <- data.table(coffe_review_data)

str(coffe_review_data)  # Understand the variables
summary(coffe_review_data) # Understand different varaibles and their data ranges


ggplot(coffe_review_data, aes(x=Days_between_Purchase)) + geom_histogram(binwidth=5,colour="black", fill="white")

#Add a line for the mean:

# Plotting days between purchase dependeing on SEC and Price Consciousness
ggplot(coffe_review_data, aes(x=Days_between_Purchase)) +
  geom_histogram(binwidth=20, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(Days_between_Purchase, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  facet_grid(coffe_review_data$SEC ~ coffe_review_data$Price_Conscious)+ ## Divide with "SEC" vertical, "Price Consciousness" horizontal
  ggtitle("Histogram-Days between purchase dependent on SEC and Price Consciousness")

#Plotting "number of packs sold on brand as vertical and SEC as horizontal
ggplot(coffe_review_data, aes(x=coffe_review_data$No_of_Packet)) +
  geom_histogram(binwidth=20, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(No_of_Packet, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1) +
  facet_grid(coffe_review_data$Brand ~ coffe_review_data$SEC)+ ## Divide with "Brand" vertical, "SEC" horizontal
  ggtitle("Histogram -Number of Packs Sold w.r.t Brand Vertical and SEC horizontal wise")

# We need to find out number of packet sold, how frequently the bought per Brand for SEC wise..This will reveal which brand sold most 
# and bought more frequently...
# group by clause will be SEC, Brand; sum clause will total of packet sold; select will be mean and median of no of days between purchase
#SELECT SUM(No_of_Packet) group by SEC,BRAND
summary.table <- coffe_table[,.(No_of_Packet.Sum = sum(No_of_Packet)),by=.(SEC,Brand)] 

# Ordering the data for better visibility
summary.table[order(SEC,No_of_Packet.Sum)]


## There can be possibility that some big family bought multiple packets together and total packets sold in that case may not represent the
## brand popularity. So another way to validate the above data will be to find out the median of the days be
summary.table.mean <- coffe_table[, .(mean_Days_between_Purchase =mean(Days_between_Purchase)), by = c("SEC", "Brand")]
summary.table.mean[order(SEC,mean_Days_between_Purchase)]
write.csv(summary.table.mean, file = "summary_table_mean_analysis")

# Now aim is to find out "How frequently household buy coffee".  So here the analysis will be based on SEC.
summary.table.frequency <- coffe_table[,.(mean_Days_between_Purchase =mean(Days_between_Purchase)), by = SEC]
summary.table.frequency[order(mean_Days_between_Purchase)]

#by = c("SEC","Brand")
# Quantity Purchased Analysis
summary.table.quantity_analysis <- coffe_table[,.(mean_No_of_Packet =mean(No_of_Packet)), by = SEC]
summary.table.quantity_analysis[order(mean_No_of_Packet)]


# Now aim is to find out "How frequently household buy coffee".  So here the analysis will be based on SEC.
summary.table.idfrequence <- coffe_table[,.(mean_Days_between_Purchase =mean(Days_between_Purchase)), by =IDNo]
summary.table.idfrequence[order(mean_Days_between_Purchase)]

## Find out outliers
summary.table.outlier <-coffe_table[coffe_table$Days_between_Purchase %in% c("34","741")]
summary.table.outlier[order(Days_between_Purchase)]


# Brand wise sales 
ggplot(data = coffe_review_data,aes(coffe_review_data$Brand)) +
  geom_histogram(bins = 30,
                 fill="blue",
                 colour = barlines) + 
  labs(title="Histogram for Brand") +
  labs(x = "Brand Level", y = 'count')


myhrcorcoffe_review_data<- cor(coffe_review_data)
corrplot(corr = myhrcorcoffe_review_data, method="color", type="upper", addCoef.col = "red", order ="AOE")

## Brand Merging Analysis
#we have to find out the "Selling-price per packet of coffee" of Brand 3 and Brand 4. Similarly we have to find out unique 
#value for the "Monthly income of household" and "Age of householder" from their sales record. 
summary.table.brand_3_4 <- coffe_table[coffe_table$Brand %in% c("3","4"),("Brand"), by = c("Brand", "Price_per_Packet")]
## Probability calculation....This logic will help us to find out count of people for different "Socioeconomic level of householder".

#These will show in table format and with relative frequency
with(coffe_review_data, table(coffe_review_data$SEC))
with(coffe_review_data, table(coffe_review_data$SEC)/sum(table(coffe_review_data$SEC)))

## Here we will analyse frequencies of the coffe packet bought based oi=n SEC and selling price per packet

with(coffe_review_data, table(coffe_review_data$SEC)/sum(table(coffe_review_data$SEC)))

with(coffe_review_data, table(coffe_review_data$SEC, coffe_review_data$Price_per_Packet))

with(coffe_review_data, table(coffe_review_data$SEC, coffe_review_data$Price_per_Packet)/sum(table(coffe_review_data$SEC, coffe_review_data$Price_per_Packet)))

with(coffe_review_data, table(coffe_review_data$ID))

## There are possiblity that same ID is existing as they may have purchased multiple times
coffe_review_data_ID_Wise_Summ <- with(coffe_review_data, table(coffe_review_data$ID,coffe_review_data$Price_per_Packet))
## This will give us total number of ID or customer
length(coffe_review_data_ID_Wise_Summ)
#Our next aim is to find out number of customers in different income segment

coffe_review_data_ID_Wise_Summ <- with(coffe_review_data, table(coffe_review_data$ID,coffe_review_data$Price_per_Packet))
## This will give us total number of ID or customer. It will automatically ignore duplicate data
##We need to find out how many are in different class (Upper Class, Higher Middle Class, Middle Class, Lower Middle class and Lower class)
coffe_table_ME <- coffe_table
# Unique rows using only a subset of columns
coffe_table_original <- dim(unique(coffe_table_ME[,list(coffe_table_ME$IDNo)]))    # 29 2
#Now we need to find out - Actually Bought "High Cost Coffee" | person is "Higher Class




