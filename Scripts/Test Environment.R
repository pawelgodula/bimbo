###################################################################################################
#  DATA PREPARATION
###################################################################################################


###------------------------------------------------------------------------------------------------
### DOWNLOAD FUNCTIONS AND DATA


# Get libraries and functions
rm(list = ls())
library(data.table)


source('C:/Users/maciej.gorgol/Desktop/ML/GB/K1 Project/Scripts/Definitions GB.R')
#source('C:/Users/maciej.gorgol/Desktop/ML/GB/K1 Project/Scripts/Data Functions.R')

# Get training data
train = fread('C:/Users/maciej.gorgol/Desktop/ML/GB/K1 Project/input/train.csv', colClasses = "numeric")

###------------------------------------------------------------------------------------------------
### HOUSEKEEPING

# Rename in English
names(train) = train.col.names

# Set a table key to enable fast aggregations
setkey(train, Product_ID, Sales_channel, Client_ID)


###------------------------------------------------------------------------------------------------
### DIVIDE TRAINING SET FOR CROSS-VALIDATON
### Info: Divide the sample into three weeks depending on the

# Divide into 3 samples
train.12 = train[Week %in% 3:7]
train.3 = train[Week %in% 8:9]




###------------------------------------------------------------------------------------------------
### TEST: TRAINING MEANS SCRIPT IN-SAMPLE


median = train.12[, median(Adjusted_Demand)]
mean_Prod = train.12[, exp(mean(log(Adjusted_Demand+1)))-1, by = Product_ID]
setnames(mean_Prod,"V1","M2")
mean_Agent_Prod = train.12[, exp(mean(log(Adjusted_Demand+1))),by = .(Product_ID, Sales_channel, Client_ID)]
setnames(mean_Agent_Prod,"V1","M3")



###------------------------------------------------------------------------------------------------
### TEST: TESTING MEANS SCRIPT OUT-OF-SAMPLE

test1 =  train.3[,.(Product_ID, Sales_channel, Client_ID, Adjusted_Demand)]
setkey(test1, Product_ID, Sales_channel, Client_ID)

submit1 = merge(test1, mean_Agent_Prod, all.x = TRUE)
submit1$M2 = merge(test1, mean_Prod, by = "Product_ID", all.x = TRUE)$M2
submit1$Pred = trunc(submit1$M3)
submit1[is.na(M3)]$Pred = round(submit1[is.na(M3)]$M2)
submit1[is.na(Pred)]$Pred = median
setnames(submit1,"Pred","Pred_Adjusted_Demand")


###------------------------------------------------------------------------------------------------
### EVALUATE USING LOSS FUNCTION

submit = submit1[,.(Adjusted_Demand, Pred_Adjusted_Demand)]
real_demand = submit[,.(Adjusted_Demand)]
pred_demand = submit[,.(Pred_Adjusted_Demand)]

in.sample.error = GetRMSLE(pred_demand, real_demand)


