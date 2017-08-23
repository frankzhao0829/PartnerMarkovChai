
# library(devtools)
# devtools::install_github("hadley/multidplyr")

library(dplyr)
library(arules)
library(plotly)
library(markovchain)
library(data.table)
library(tidyr)
library(multidplyr)

setwd("/home/zhaoch/R_Workspace/Partner Forecasting/Data")

# READ IN TOTAL DATA SET
#all_region_dat <- fread("All_Partner_Merged_11-17.csv")
all_region_dat <- fread("CSIS_5-26-17.csv")
# all_region_dat <- all_region_dat[,V1:=NULL]


all_region_dat <- all_region_dat %>% 
  select(Fiscal.Quarter                     = `Fiscal Quarter`,
         PPID, 
         Partnername,
         Current.Product.Segment            = `Product Segment`,
         Customer.Sub.Region                = `Customer HQ Organization`,          
         quota                              = `Sales Comp Est Net CLC`,
         sellout                            = `Ref Net Sellout for Quota Perf CLC`) %>%
  mutate(quota = as.numeric(gsub("\\)", "-", gsub("\\)", "", gsub(",", "", gsub("\\$", "", quota))))),
         sellout = as.numeric(gsub("\\)", "-", gsub("\\)", "", gsub(",", "", gsub("\\$", "", sellout)))))) %>% 
  group_by(Fiscal.Quarter, PPID, Partnername, Current.Product.Segment, Customer.Sub.Region) %>% 
  summarise(sellout = sum(sellout), quota = sum(quota))

# # Data Cleansing #2: Filter only partners more than 0 sellout for last fiscal year
# Partn_filter <- all_region_dat %>% 
#   filter(Fiscal.Quarter %in% c('FY16-Q1', 'FY16-Q2', 'FY16-Q3', 'FY16-Q4')) %>%
#   group_by(Fiscal.Quarter, PPID, Partnername, Current.Product.Segment, Customer.Sub.Region) %>% 
#   summarise(sellout_agg = sum(sellout)) %>%
#   filter(sellout_agg > 0)


#function to remove na values
f_dowle3 = function(DT) {
  # either of the following for loops
  
  # by name :
  # for (j in names(DT))
  #  set(DT,which(is.na(DT[[j]])),j,0)
  
  # or by number (slightly faster than by name) :
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

#run the function
f_dowle3(all_region_dat)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


input<-NULL 
next_state <- 5
bins1 <- 3
bins2 <- 20
simulations <- 500
min_value <- as.numeric(bins1)
max_value <- as.numeric(bins2)  
iterations <- c(min_value:max_value)


#sample.data <- all_region_dat[which(all_region_dat$Partnername %in% c("SCC", "TAISA", "COPIAFAX","Velem LTD")),]

# # Find number of partners
# Customer_filter <- all_region_dat %>%
#   as.data.frame() %>%
#   select(Fiscal.Quarter, PPID, Partnername, Customer.Sub.Region, Current.Product.Segment, sellout) %>%
#   group_by(Fiscal.Quarter, PPID, Partnername, Customer.Sub.Region, Current.Product.Segment) %>%
#   summarize(total.count=n()) %>%
#   filter(total.count>=6)
# 
# length(unique(Customer_filter$Partnername))
# length(unique(all_region_dat$Partnername))

# Remove FY14-Q1 data
all_region_dat <- all_region_dat %>%
  filter(Fiscal.Quarter != "FY14-Q1")

# Add zero sellout rows to keep each partner has same rows (time period) of data
Fiscal.Quarter <- c('FY15-Q1', 'FY15-Q2', 'FY15-Q3', 'FY15-Q4', 
                    'FY16-Q1', 'FY16-Q2', 'FY16-Q3', 'FY16-Q4', 
                    'FY17-Q1', 'FY17-Q2', 'FY17-Q3', 'FY17-Q4')
Fiscal.Quarter <- as.data.frame(Fiscal.Quarter) %>%
  mutate(k=1)

Customer_filter1 <- all_region_dat %>%
  as.data.frame() %>%
  select(Fiscal.Quarter, PPID, Partnername, Customer.Sub.Region, Current.Product.Segment, sellout) %>%
  group_by(PPID, Partnername, Customer.Sub.Region, Current.Product.Segment) %>%
  summarize(total.count=n()) %>%
  filter(total.count>=6) %>%
  select(-total.count)

Customer_filter <- all_region_dat %>%
  as.data.frame() %>%
  inner_join(Customer_filter1, by=c("PPID", "Partnername", "Current.Product.Segment", "Customer.Sub.Region"))

Dummy <- Customer_filter %>%
  select(PPID, Customer.Sub.Region, Partnername, Current.Product.Segment) %>%
  mutate(k=1) %>%
  full_join(Fiscal.Quarter, by='k') %>%
  select(-k)

Add_records <- anti_join(Dummy, Customer_filter, by=c("PPID", "Partnername", "Current.Product.Segment", "Customer.Sub.Region", "Fiscal.Quarter")) %>%
  unique()

all_region_dat <- bind_rows(Customer_filter, Add_records)

# Convert NA to 0
all_region_dat[is.na(all_region_dat$sellout), "sellout"] <- 0
all_region_dat[is.na(all_region_dat$quota), "quota"] <- 0

all_region_dat <- all_region_dat %>%
  arrange(PPID, Customer.Sub.Region, Partnername, Current.Product.Segment, Fiscal.Quarter)

# Create function for model
batch_function <- function(seg_part){
  
  input<-NULL 
  next_state <- 5
  bins1 <- 3
  bins2 <- 20
  simulations <- 500
  min_value <- as.numeric(bins1)
  max_value <- as.numeric(bins2)  
  iterations <- c(min_value:max_value)
  
  data.partition <<- seg_part
  sellout <- seg_part$sellout
  quota <- seg_part$quota
  seg_month <- seg_part$Fiscal.Quarter
  
  #Remove current and next quarter data to avoid comparing predictions to non-existent data
  if(length(sellout) > 2) {
    sellout <- head(sellout, (length(sellout)-2))
    quota <- head(quota, (length(quota)-2))
    seg_month <- head(seg_month, (length(seg_month)-2))
  }
  
  fiscal.Q <- tail(seg_month, n = 3)
  # rfm_val <- seg_part$combinedScore[1]
  # rfm_cmb <- seg_part$RFM_Comb[1]
  # prior_rfm_val <- seg_part$Prior_combinedScore[1]
  # prior_rfm_cmb <- seg_part$Prior_RFM_Comb[1]
  
  t <- next_state
  
  
  if(length(sellout) < 6){
    
    predictions <- as.data.frame( rep(NA,next_state))
    names(predictions)<- "Pred"
    sellout_append <- c( rep(NA,next_state)) #this is not correct as there could be data here
    upr <- c(rep(NA,next_state))
    lwr <- c(rep(NA,next_state))
    index_min <- c(rep(NA,next_state))
    fiscal.Q <- c(rep(NA,next_state))
  }
  
  
  if(length(sellout) >= 6){
    
    sellout_train <- sellout[1:(length(sellout)-3)]
    sellout_test <- tail(sellout, n = 3)
    #current_state <- tail(sellout_train, n=1)
    next_predctions <- array(0, dim = c(length(iterations),simulations,t))
    next_state_result <- matrix(0,nrow = simulations, ncol = t)
    sum_np <- matrix(0,nrow = length(iterations), ncol = t)
    res <- matrix(0,nrow = length(iterations), ncol = t)
    for(n in iterations){
      
      
      if(length(unique(quantile(sellout_train, na.rm=TRUE))) > 1){
        categories <- discretize(sellout_train, method = "interval", categories = n, labels = c(1:n))
        cutpoints <- discretize(sellout_train, method = "interval", categories = n, onlycuts = TRUE)
      }
      
      if(length(unique(quantile(sellout_train, na.rm=TRUE))) == 1){
        categories <- discretize(sellout_train, method = "interval", categories = 1, labels = c(1))
        cutpoints <- discretize(sellout_train, method = "interval", categories = 1, onlycuts = TRUE)
      }
      
      cat_values <- vector() 
      for(m in 1:n){
        cat_values <- append(cat_values, mean(cutpoints[m:(m+1)]))
      }
      #print (categories)
      #print (cutpoints)
      #print (cat_values)
      #print (current_state)
      #add the mode of all the training categories to avoid brand new occuring state with unknown next transition issue which will show not enough positive probabilities
      #one row of a transition matrix is zero i.e. 6 means this is an absorbing  state
      categories <- append(categories, getmode(categories))
      #print (categories)
      markovchain_model <- markovchainFit(as.character(categories))
      #print (markovchain_model)
      #print (summary(markovchain_model))
      #print(categories[length(categories)])
      for(k in 1:simulations){
        next_state_result[k,] <- markovchainSequence(t, markovchain_model$estimate, t0 = categories[length(categories)])
        next_predctions[(n-iterations[1]+1),k,] <- cat_values[as.numeric(next_state_result[k,])]
        #print (next_predctions[(n-iterations[1]+1),k,])
      }
      #print (dim(next_predctions[(n-iterations[1]+1),,]))
      if(t > 1){
        res[(n-iterations[1]+1),] <- abs(colMeans(next_predctions[(n-iterations[1]+1),,]) - sellout_test[1:t])
        sum_np[(n-iterations[1]+1),] <- colMeans(next_predctions[(n-iterations[1]+1),,])
      }else{
        res[(n-iterations[1]+1),] <- abs(mean(next_predctions[(n-iterations[1]+1),,]) - sellout_test[1:t])
        sum_np[(n-iterations[1]+1),] <- mean(next_predctions[(n-iterations[1]+1),,])
      }
      
      
    }
    prediction <- vector()
    prediction_lower <- vector()
    prediction_upper <- vector()
    
    
    
    if (t > 3) {
      
      rowsums.res <- rowSums(res[,1:3])
      
      index_min <- which.min(rowsums.res)
      index_min <- c(rep(index_min, 3))
      
      # Use the iterations that have the least error in the testing sample for the future predictions
      index_min <- append(index_min, rep(getmode(index_min),t-3))
      
    } else {
      rowsums.res <- rowSums(res)
      index_min <- which.min(rowsums.res)
      index_min <- c(rep(index_min, t))
    }
    
    
    # Update month label if user choose more than 3 Next State
    if (t > 3) {
      seg_month <- as.character(seg_month)
      
      i=1
      while (i+3 <= t) {
        lastqtr <- seg_month[length(seg_month)]
        
        if (as.numeric(substring(lastqtr,nchar(lastqtr))) == 4) {
          nextqtr <- lastqtr 
          nextyr <- as.numeric(substr(nextqtr, 3, 4)) + 1
          
          substr(nextqtr, 3, 4) <- as.character(nextyr)
          substring(nextqtr, nchar(nextqtr)) <- as.character(1)
          
          seg_month[length(seg_month) + 1] <- nextqtr
          
        } else {
          nextqtr <- lastqtr 
          nextqtrn <- as.character(as.numeric(substring(nextqtr,nchar(nextqtr))) + 1)
          substring(nextqtr,nchar(nextqtr)) <- nextqtrn
          
          seg_month[length(seg_month) + 1] <- nextqtr
        }
        i <- i + 1
      }
      
      seg_month <- factor(seg_month)
    }
    
    
    #  rowsums.res <- rowSums(res)
    # index_min <- which.min(rowsums.res)
    #index_min <- c(rep(index_min, next_state))
    
    
    #print(index_min)
    #print(res)
    
    for(s in 1:t){
      
      #print(s)
      #print(index_min[s])
      prediction <- append(prediction,sum_np[index_min[s],s])
      #print(prediction)
      #print ("simulation")
      #print (next_predctions[index_min[s],,s])
      #if all the simulation points are constant, the t.test will show errors because the model will always predict a same status that's why we add the mode of all the training categories as the extra state
      # print (sd(next_predctions[index_min[s],,s]))
      
      if(sd(next_predctions[index_min[s],,s]) == 0){
        prediction_lower <- append(prediction_lower, prediction[s])
        prediction_upper <- append(prediction_upper, prediction[s])
      }else{
        
        
        conf_interv <- t.test(next_predctions[index_min[s],,s],alternative="two.sided",mu = 0,conf.level=0.95)$conf.int
        prediction_lower <- append(prediction_lower, conf_interv[1])
        prediction_upper <- append(prediction_upper, conf_interv[2])
        
      }
    }
    predictions <- as.data.frame( round(prediction, 0))
    names(predictions)<- "Pred"
    sellout_append <-sellout_test
    upr <-  prediction_upper
    lwr <- prediction_lower
    
    if(t > 3 ){
      
      sellout_append <- c(sellout_append, rep(NA, t-3))
      fiscal.Q <- tail(seg_month,n=t)
      
    }
    
  }
  # 
  # print("sellout")
  # print(sellout_append)
  # print("lwr")
  # print(lwr)
  # print("upr")
  # print(upr)
  # print("index_min")
  # print(index_min)
  # print("fiscal")
  # print(fiscal.Q)
  # print("seg_month")
  # print(seg_month)
  # print("predictions")
  # print(predictions)
  
  
  
  
  output_values <- data.frame(sellout_append, lwr, predictions, upr, index_min, fiscal.Q)
  
  return(output_values)
  
  
  # all_predictions <- data.frame(prediction= prediction, lwr= prediction_lower, upr= prediction_upper)
  
  #preds <- data.frame(pred = prediction, lwr= prediction_lower, upr =prediction_upper)
  #preds <- unite(preds, predictions, c(pred,lwr,upr), sep= "_" , remove=TRUE)
  
  
}


# Running the model using parallel package

library(parallel)
numCores <- detectCores(all.tests = TRUE, logical = TRUE)

cluster <- create_cluster(cores = numCores - 1)
set_default_cluster(cluster)

cluster_library(cluster, "dplyr")
cluster_library(cluster, "arules")
cluster_library(cluster, "markovchain")
cluster_library(cluster, "data.table")
cluster_library(cluster, "tidyr")
cluster_assign_value(cluster, "batch_function", batch_function)
cluster_assign_value(cluster, "getmode", getmode)


data.parallel <- all_region_dat %>% partition(Partnername, PPID, Current.Product.Segment, Customer.Sub.Region)

system.time({
  
  #cluster_library(data.sample, c("dplyr", "arules", "markovchain", "data.table", "tidyr"))
  predictions <- data.parallel %>% do(batch_function(.)) %>% collect()
})

write.csv(predictions, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/predictions.csv", row.names = FALSE)

# # Read Predictions file
# predictions <- read.csv(file = "predictions.csv", header = T, stringsAsFactors = F)


