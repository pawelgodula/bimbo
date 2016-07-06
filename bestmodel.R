### ten skrypt generuje score 0.49533 na public LB. Dobry punkt wyjscia.

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.
#Load data.table for fast reads and aggreagtions
library(data.table)

# Input data files are available in the "../input/" directory.

# Read in only required columns and force to numeric to ensure that subsequent 
# aggregation when calculating medians works
train <- fread('../input/train.csv', 
               select = c('Semana', 'Agencia_ID', 'Cliente_ID', 'Producto_ID', 'Demanda_uni_equil'),
               colClasses=c(Semana="numeric", Agencia_ID="numeric", Cliente_ID="numeric",Producto_ID="numeric",Demanda_uni_equil="numeric"))

# set a table key to enable fast aggregations
setkey(train, Producto_ID, Agencia_ID, Cliente_ID)
 

#calculate the overall median
median <- train[, median(Demanda_uni_equil)]

#calculate the product overall mean; call it M2
mean_Prod <- train[, exp(mean(log(Demanda_uni_equil+1)))*0.57, by = Producto_ID]
setnames(mean_Prod,"V1","M2")

#calculate the agent and product  mean; call it M3
mean_Agent_Prod <- train[, exp(mean(log(Demanda_uni_equil+1)))-1,by = .(Producto_ID,Agencia_ID,Cliente_ID)]
setnames(mean_Agent_Prod,"V1","M3")

###################################################################################################
#  
# That's the 'modeling' done now need to apply scoring to test set
# 
###################################################################################################

# Read in Test data 
# Read in only required columns and force to numeric

test <- fread('../input/test.csv', 
               select = c('id','Agencia_ID', 'Producto_ID', 'Cliente_ID'),
               colClasses=c(Agencia_ID="numeric",Producto_ID="numeric", Cliente_ID="numeric"))


# set a table key to enable fast joins to predictions
setkey(test, Producto_ID, Agencia_ID, Cliente_ID)

# Create table called submit that joins medians (in field M3) by Product and Agent to test data set
submit <- merge(test, mean_Agent_Prod, all.x = TRUE)

# add column M2 that contains mean by Product
submit$M2 <- merge(test, mean_Prod, by = "Producto_ID", all.x = TRUE)$M2

# Now create Predictions column; intially set to be M3 which contains mean by product and agent
submit$Pred <- round(submit$M3,1)

# where mean by product and agent is null use mean by product (M2)
submit[is.na(M3)]$Pred <- round(submit[is.na(M3)]$M2)

# where median by product is null use overall median
submit[is.na(Pred)]$Pred <- median

# now relabel columns ready for creating submission
setnames(submit,"Pred","Demanda_uni_equil")


# Write out submission file.
# Any results you write to the current directory are saved as output.
write.csv(submit[,.(id,Demanda_uni_equil)],"submit_med_by_Agency_Client.csv", row.names = FALSE)
