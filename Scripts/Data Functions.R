###################################################################################################
#  Basic data maniputlation 
###################################################################################################

GetSamples = function(train.set, in.sample.size = 1e6, out.sample.size = 5e5){
 
  x = 1 : nrow(train.set)
  sampl = sample(x, in.sample.size + out.sample.size, replace = FALSE)
  
  train.set = train.set[sampl[1:in.sample.size]]
  test.set = test.set[sampl[(in.sample.size + 1) : out.sample.size]]
  res = list(train.set = train.set, test.set = test.set)
  
  return(res)
}


MeansScriptTraining = function(train){
  median = train[, median(Adjusted_Demand)]
  mean_Prod = train[, exp(mean(log(Adjusted_Demand+1)))-1, by = Product_ID]
  setnames(mean_Prod,"V1","M2")
  mean_Agent_Prod = train[, exp(mean(log(Adjusted_Demand+1))),by = .(Product_ID, Sales_channel, Client_ID)]
  setnames(mean_Agent_Prod,"V1","M3")
  return(mean_Agent_Prod)
}


# 
# GetRMSLE = function(pred_demand, real_demand){
#   
#   n = nrow(pred_demand)
#   av.error.sq = sum((log(pred_demand + 1) - log(real_demand + 1)) ^ 2) / n
#   av.error = av.error.sq ^ 0.5
#   
#   return(av.error)
# }

GetRMSLE = function(pred_demand, real_demand, numeric.column = FALSE){
  
  if (numeric.column == TRUE){
    n = length(pred_demand)
  } else {
    n = nrow(pred_demand)
  }
  
  av.error.sq = sum((log(pred_demand + 1) - log(real_demand + 1)) ^ 2) / n
  av.error = av.error.sq ^ 0.5
  
  return(av.error)
}






