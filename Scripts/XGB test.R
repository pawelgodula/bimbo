###################################################################################################
#  TRY XGB ON DATA  
###################################################################################################



train.12.data = train.12[,.(Product_ID, Sales_channel, Client_ID)]
train.12.labels = train.12[,.(Adjusted_Demand)]

train.12.data.matrix = as.matrix(train.12.data)
train.12.labels.matrix = as.numeric(as.vector(train.12.labels))

train.3.labels = train.3[,.(Adjusted_Demand)]
real_demand = train.3.labels$Adjusted_Demand

train.3.data = train.3[,.(Product_ID, Sales_channel, Client_ID)]
train.3.data.matrix = as.matrix(train.3.data)


bst.12 = xgboost(data = train.12.data.matrix, 
                    label = log(train.12.labels$Adjusted_Demand + 1),
                    max.depth = 6, 
                    eta = 0.3, 
                  #  nthread = 2, 
                    nround = 5, 
                    objective = "reg:linear")


pred = predict(bst.12, train.3.data.matrix)
pred_demand = pred


n = length(pred_demand)
av.error.sq = sum((pred_demand - log(real_demand + 1)) ^ 2) / n
av.error = av.error.sq ^ 0.5


###################################################################

train1 = train.12.data
median = log(median + 1)
mean_Prod = log(mean_Prod + 1)
mean_Agent_Prod = log(mean_Agent_Prod + 1)

train.input1 = merge(train1, mean_Agent_Prod, all.x = TRUE)
train.input1$M2 = merge(train1, mean_Prod, by = "Product_ID", all.x = TRUE)$M2
train.input1$Pred = trunc(train.input1$M3)
train.input1[is.na(M3)]$Pred = round(train.input1[is.na(M3)]$M2)
train.input1[is.na(Pred)]$Pred = median

train.12.data.matrix = as.matrix(train.input1)
train.12.labels = train.12[,.(Adjusted_Demand)]


###
train1 = train.12.data
median = log(median + 1)
mean_Prod = log(mean_Prod + 1)
mean_Agent_Prod = log(mean_Agent_Prod + 1)

train.input1 = merge(train1, mean_Agent_Prod, all.x = TRUE)
train.input1$M2 = merge(train1, mean_Prod, by = "Product_ID", all.x = TRUE)$M2
train.input1$Pred = trunc(train.input1$M3)
train.input1[is.na(M3)]$Pred = round(train.input1[is.na(M3)]$M2)
train.input1[is.na(Pred)]$Pred = median

train.12.data.matrix = as.matrix(train.12.data)

















bst.12 = xgboost(data = train.12.data.matrix, 
                 label = log(train.12.labels$Adjusted_Demand + 1),
                 max.depth = 6, 
                 eta = 0.3, 
                 #  nthread = 2, 
                 nround = 5, 
                 objective = "reg:linear")

