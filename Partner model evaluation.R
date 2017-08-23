
source("/home/zhaoch/R_Workspace/Partner Forecasting/Script/AccMetric.R")

library(rminer)
library(scales)

setwd("/home/zhaoch/R_Workspace/Partner Forecasting/Data")


# Read Predictions file
predictions <- read.csv(file = "predictions.csv", header = T, stringsAsFactors = F)

## REC curve
predictions1 <- predictions %>%
  filter(! is.na(sellout_append))
m1 <- mmetric(predictions1$sellout_append, predictions1$Pred, c('REC'))
rec <- as.data.frame(m1$rec)
colnames(rec) <- c("tolerance", "accuracy")

ggplot(rec, aes(x = tolerance, y = accuracy)) + 
  geom_line() + ggtitle("REC Curve for Partners (Add zero records)") +
  scale_x_continuous(label=comma, limits=c(0,50000))


# Create REC Curve by average sellout groups
all_region_dat1 <- all_region_dat %>% 
  filter(! Fiscal.Quarter %in% c('FY17-Q3', 'FY17-Q4')) %>%
  group_by(PPID, Partnername, Current.Product.Segment, Customer.Sub.Region) %>%
  summarise(sellout = mean(sellout)) %>%
  mutate(sellout_group = if_else(sellout <= 5000, "<= $5K", 
                                 if_else(sellout < 10000,   "Between $5K and $10K",
                                         if_else(sellout < 30000,   "Between $10K and $30K",
                                                 if_else(sellout < 100000,  "Between $30K and $100K",
                                                         if_else(sellout < 1000000, "Between $100K and $1M",
                                                                 if_else(sellout < 5000000, "Between $1M and $5M", 
                                                                         "More than $5M")))))))


predictions <- predictions %>%
  left_join(all_region_dat1, by=c("PPID", "Partnername", "Current.Product.Segment", "Customer.Sub.Region")) %>%
  select(-sellout)

predictions1 <- predictions1 %>%
  left_join(all_region_dat1, by=c("PPID", "Partnername", "Current.Product.Segment", "Customer.Sub.Region")) %>%
  select(-sellout)


# Calculate different accuracy metrics
predictions1 <- predictions1 %>%
  mutate(group=paste(PPID, Partnername, Current.Product.Segment, Customer.Sub.Region, sep="|"))

group <- as.character(predictions1$group)

out <- ModelEvaluate(predictions1$Pred, predictions1$sellout_append, group)

ACC <- as.data.frame(out$ACC)

write.csv(ACC, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/ACC.csv", row.names = FALSE)

# # Read Accuracy file
# ACC <- read.csv(file = "ACC.csv", header = T, stringsAsFactors = F)


# Filter out Partners with negative sellout or prediction quarters
length(unique(predictions1$group))

neg <- predictions1 %>%
  filter(sellout_append < 0 | Pred < 0) %>%
  select(group) %>%
  unique()

predictions2 <- predictions1 %>%
  filter(! group %in% neg$group)

group2 <- as.character(predictions2$group)

out2 <- ModelEvaluate(predictions2$Pred, predictions2$sellout_append, group2)

ACC2 <- as.data.frame(out2$ACC)
# range(ACC2$MASE[! is.na(ACC2$MASE)]) 0 Inf
# range(ACC2$MASE[(! is.na(ACC2$MASE)) & is.finite(ACC2$MASE)]) 0 2802

# range(ACC2$sMAPE[! is.na(ACC2$sMAPE)]) 0 100

write.csv(ACC2, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/ACC_Positive_Only.csv", row.names = FALSE)
# ACC2 <- read.csv(file = "ACC_Positive_Only.csv", header = T, stringsAsFactors = F)

# write.csv(predictions, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/predictions.csv", row.names = FALSE)

# Visualize accuracy metrics sMAPE and MASE
ggplot(ACC, aes(x=sMAPE)) + 
  geom_histogram(aes(y=..count..),     
                 binwidth=2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(name = "Symmetric mean absolute percentage error (sMAPE)",
                     limits = c(-2, 100), breaks = seq(0, 100, 5)) +
  scale_y_continuous(limits = c(0, 3000)) + 
  ggtitle("Frequency histogram of sMAPE")


ggplot(ACC, aes(x=MASE)) + 
  geom_histogram(aes(y=..count..),     
                 binwidth=0.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(name = "Mean absolute scaled error (MASE)",
                     limits = c(-2, 20), breaks = seq(0, 20, 1)) +
  scale_y_continuous(limits = c(0, 25000)) + 
  ggtitle("Frequency histogram of MASE")


# Create accuracy metrics for different groups
group_seg     <- as.character(predictions2$Current.Product.Segment)
group_HQ      <- as.character(predictions2$Customer.Sub.Region)
group_sellout <- as.character(predictions2$sellout_group)

outseg     <- ModelEvaluate(predictions2$Pred, predictions2$sellout_append, group_seg)
outHQ      <- ModelEvaluate(predictions2$Pred, predictions2$sellout_append, group_HQ)
outsellout <- ModelEvaluate(predictions2$Pred, predictions2$sellout_append, group_sellout)

write.csv(outseg$ACC, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/ACC_by_Seg.csv", row.names = FALSE)
write.csv(outHQ$ACC, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/ACC_by_HQ.csv", row.names = FALSE)
write.csv(outsellout$ACC, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/ACC_by_Sellout.csv", row.names = FALSE)


# For accreditation groups 
predictions_accred <- read.csv(file = "predictions_accred.csv", header = T, stringsAsFactors = F)
predictions_accred <- predictions_accred %>%
  filter((! sellout_append == "#N/A") & (! Pred == "#N/A") & (! Accreditation == "#N/A"))
predictions_accred$Pred <- as.numeric(predictions_accred$Pred)
predictions_accred$sellout_append <- as.numeric(predictions_accred$sellout_append)

group_accred <- as.character(predictions_accred$Accreditation)
outaccred    <- ModelEvaluate(predictions_accred$Pred, predictions_accred$sellout_append, group_accred)
write.csv(outaccred$ACC, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/ACC_by_Accreditation.csv", row.names = FALSE)


## Create histogram of different accuracy metrics for all models
# sMAPE
ggplot(ACC2, aes(x=sMAPE)) + 
  geom_histogram(aes(y=..count..),     
                 binwidth=2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(name = "Symmetric mean absolute percentage error (sMAPE)",
                     limits = c(-2, 102), breaks = seq(0, 100, 5)) +
  scale_y_continuous(limits = c(0, 6000)) + 
  ggtitle("Frequency histogram of sMAPE")

# MASE
ggplot(ACC2, aes(x=MASE)) + 
  geom_histogram(aes(y=..count..),     
                 binwidth=0.2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(name = "Mean absolute scaled error (MASE)",
                     limits = c(-2, 15), breaks = seq(0, 15, 1)) +
  scale_y_continuous(limits = c(0, 10000)) + 
  ggtitle("Frequency histogram of MASE")

# R2
R2 <- as.data.frame.factor(ACC2$R2)
R2$`ACC2$R2` <- as.numeric(levels(R2$`ACC2$R2`))[R2$`ACC2$R2`]
colnames(R2) <- "R2"

ggplot(R2, aes(x=R2)) +
  geom_histogram(aes(y=..count..),     
                 binwidth=0.02,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_x_continuous(name = "R-Square (R2)",
                     limits = c(-0.02, 1.02), breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(limits = c(0, 3000)) + 
  ggtitle("Frequency histogram of R2")


# Export historical data to build Excel visualization tool
# filter <- predictions %>%
#   select(PPID, Partnername, Customer.Sub.Region, Current.Product.Segment) %>%
#   unique()
# 
# filter <- all_region_dat %>%
#   inner_join(filter, by = c("PPID", "Partnername", "Customer.Sub.Region", "Current.Product.Segment")) %>%
#   filter(! Fiscal.Quarter %in% c("FY16-Q4", "FY17-Q1", "FY17-Q2", "FY17-Q3", "FY17-Q4")) %>%
#   mutate(sellout_append = sellout, fiscal.Q = Fiscal.Quarter, 
#          lwr = NA, Pred= NA, upr = NA, index_min = NA) %>%
#   select(Partnername, PPID, Current.Product.Segment, Customer.Sub.Region, sellout_append, lwr, Pred, upr, index_min, fiscal.Q)
# 
# write.csv(filter, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/Previous_data.csv", row.names = FALSE)


# Overall REC Curve
m1 <- mmetric(predictions2$sellout_append, predictions2$Pred, c('REC'))
rec <- as.data.frame(m1$rec)
colnames(rec) <- c("tolerance", "accuracy")

ggplot(rec, aes(x=tolerance, y=accuracy)) + geom_line() +
  scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,50000), labels = comma) + 
  ggtitle("REC Curve for Partners")

# REC by groups
rec_seg     <- as.data.frame(outseg$REC)
rec_HQ      <- as.data.frame(outHQ$REC)
rec_sellout <- as.data.frame(outsellout$REC)
rec_accred  <- as.data.frame(outaccred$REC)


ggplot(rec_seg, aes(x=tolerance, y=accuracy, group=Label, color=Label)) + geom_line() +
  scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,20000), labels = comma) + 
  ggtitle("REC Curve by Product Segments")

ggplot(rec_HQ, aes(x=tolerance, y=accuracy, group=Label, color=Label)) + geom_line() +
  scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,20000), labels = comma) + 
  ggtitle("REC Curve by Country HQ")

ggplot(rec_accred, aes(x=tolerance, y=accuracy, group=Label, color=Label)) + geom_line() +
  scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,50000), labels = comma) + 
  ggtitle("REC Curve by Accreditation Level")


# REC by avg sellout in 2 graph or in 1 graph
rec_sellout <- rec_sellout %>% 
  mutate(Order = if_else(Label=="overall", 1, 
                         if_else(Label=="<= $5K", 2,
                                 if_else(Label=="Between $5K and $10K", 3,
                                         if_else(Label=="Between $10K and $30K", 4, 
                                                 if_else(Label=="Between $30K and $100K", 5, 
                                                         if_else(Label=="Between $100K and $1M", 6,
                                                                 if_else(Label=="Between $1M and $5M", 7, 8)))))))) %>%
  filter(! Label == "overall")
rec_sellout$Label  <- with(rec_sellout, reorder(Label, Order))

g_1 <- ggplot(rec_sellout[rec_sellout$Order %in% c(2,3,4,5) ,], aes(x=tolerance, y=accuracy, group=Label, color=Label)) + geom_line() +
  scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,50000), labels = comma, breaks = seq(0,50000,by=10000)) + 
  ggtitle("REC Curve by average Partner Sellout Values")
g_2 <- ggplot(rec_sellout[rec_sellout$Order %in% c(6,7,8) ,], aes(x=tolerance, y=accuracy, group=Label, color=Label)) + geom_line() +
  scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,5000000), labels = comma, breaks = seq(0,5000000,by=1000000)) + 
  ggtitle("REC Curve by average Partner Sellout Values")

multiplot(g_1, g_2, cols=1)

ggplot(rec_sellout, aes(x=tolerance, y=accuracy, group=Label, color=Label)) + geom_line() +
  scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,1000000), labels = comma, breaks = seq(0,1000000,by=100000)) + 
  ggtitle("REC Curve by average Partner Sellout Values")


# REC by HQ in 4 subregions graph
predictions2 <- predictions2 %>%
  mutate(Sub_Region = if_else(Customer.Sub.Region %in% c("AGree", "Middle East", "Saudi Arabia", "South Africa", "Turkey"), "MEMA",
                              if_else(Customer.Sub.Region %in% c("CZSK", "ESE", "Hungary", "Israel", "Poland", "Russian Federation"), "CEE&I",
                                      if_else(Customer.Sub.Region %in% c("Austria", "Belux", "Denmark", "Finland & Baltics", "Netherlands", "Norway", "Sweden", "Switzerland"), "GWE",
                                              if_else(Customer.Sub.Region %in% c("France", "Germany", "Italy", "Portugal", "Spain", "Ireland", "United Kingdom", "HP Europe", "Other"), "Europe/Other",
                                                      "NA")))),
         Label_Region = paste(Customer.Sub.Region, Sub_Region, sep="|"))

group_SubRegion <- as.character(predictions2$Label_Region)

outsbreg <- ModelEvaluate(predictions2$Pred, predictions2$sellout_append, group_SubRegion)

write.csv(outsbreg$ACC, "/home/zhaoch/R_Workspace/Partner Forecasting/Data/ACC_by_SubRegion.csv", row.names = FALSE)

rec_subregion <- as.data.frame(outsbreg$REC)

rec_subregion1 <- data.frame(do.call('rbind', strsplit(as.character(rec_subregion$Label),'|',fixed=TRUE)))

rec_subregion <- cbind(rec_subregion, rec_subregion1)

colnames(rec_subregion) <- c("Label", "tolerance", "accuracy", "HQ_Region", "Sub_Region")

g1 <- ggplot(rec_subregion[rec_subregion$Sub_Region=="MEMA", ], aes(x=tolerance, y=accuracy, group=HQ_Region, color=HQ_Region)) + 
  geom_line() + scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,20000), labels = comma) + 
  ggtitle("REC Curve by HQ Regions in MEMA")
g2 <- ggplot(rec_subregion[rec_subregion$Sub_Region=="CEE&I", ], aes(x=tolerance, y=accuracy, group=HQ_Region, color=HQ_Region)) + 
  geom_line() + scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,20000), labels = comma) + 
  ggtitle("REC Curve by HQ Regions in CEE&I")
g3 <- ggplot(rec_subregion[rec_subregion$Sub_Region=="GWE", ], aes(x=tolerance, y=accuracy, group=HQ_Region, color=HQ_Region)) + 
  geom_line() + scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,20000), labels = comma) + 
  ggtitle("REC Curve by HQ Regions in GWE")
g4 <- ggplot(rec_subregion[rec_subregion$Sub_Region=="Europe/Other", ], aes(x=tolerance, y=accuracy, group=HQ_Region, color=HQ_Region)) + 
  geom_line() + scale_x_continuous(name = "sellout tolerance ($)", limits = c(0,30000), labels = comma) + 
  ggtitle("REC Curve by HQ Regions in Europe/Other")


multiplot(g1, g2, g3, g4, cols=2)



# Generate ave sellout and count for each category for powerpoint slides
all_region_dat %>%
  group_by(Current.Product.Segment) %>%
  summarise(total.count=n(),
            avg_sellout = mean(sellout))

test <- all_region_dat %>%
  group_by(Customer.Sub.Region) %>%
  summarise(avg_sellout = mean(sellout))  

all_region_dat1 %>%
  group_by(sellout_group) %>%
  summarise(total.count=n(),
            avg_sellout=mean(sellout))

predictions_accred %>% 
  select(Partnername, PPID, Customer.Sub.Region, Current.Product.Segment, Accreditation) %>%
  unique() %>%
  group_by(Accreditation) %>%
  summarise(total.count=n())



# Eric's idea: Hierarchical aggregate forecasting (Marketing mix modelling) test using DC Networking, South Africa

all_region_dat_hier <- all_region_dat %>%
  filter(Current.Product.Segment == "DC Networking" & Customer.Sub.Region == "South Africa") %>%
  group_by(Fiscal.Quarter, Current.Product.Segment, Customer.Sub.Region) %>%
  summarise(sellout = sum(sellout),
            quota   = sum(quota))

predictions_hier <- batch_function(all_region_dat_hier)

out_hier <- ModelEvaluate(predictions_hier[1:3, "Pred"], predictions_hier[1:3, "sellout_append"])

ACC_hier <- out_hier$ACC


# Load data
predictions_hier_test <- read.csv(file = "Predictions_Hier_Test.csv", header = T, stringsAsFactors = F)

Good <- predictions_hier_test %>%
  filter(PPID %in% c("1-1SO-835", "1-1SC-1729", "1-1SG-618", "1-9G10QC", 
                     "1-1SC-598", "1-15PYP-14", "1-1SW-1413", "1-1S4-1021")) %>%
  select(-MASE) %>%
  arrange(fiscal.Q)

Overall <- predictions_hier_test %>%
  filter(PPID %in% c("Overall")) %>%
  select(-MASE) %>%
  arrange(fiscal.Q)

Bad <- predictions_hier_test %>%
  filter(!PPID %in% c("1-1SO-835", "1-1SC-1729", "1-1SG-618", "1-9G10QC", 
                      "1-1SC-598", "1-15PYP-14", "1-1SW-1413", "1-1S4-1021", "Overall")) %>%
  select(-MASE) %>%
  arrange(fiscal.Q)


Good_Pred <- Good %>%
  group_by(fiscal.Q) %>%
  summarise(sellout_append_r = sum(as.numeric(sellout_append)),
            lwr_r  = sum(as.numeric(lwr)),
            Pred_r = sum(as.numeric(Pred)),
            upr_r  = sum(as.numeric(upr)))

Overall_Reduce <- cbind(Overall, Good_Pred[, 2:5])

Overall_Reduce <- Overall_Reduce %>%
  mutate(sellout_append = as.numeric(sellout_append) - sellout_append_r,
         lwr  = as.numeric(lwr)  - lwr_r,
         Pred = as.numeric(Pred) - Pred_r,
         upr  = as.numeric(upr)  - upr_r) %>%
  select(-sellout_append_r, -lwr_r, -Pred_r, -upr_r)

df <- data.frame(a=1:2, b=letters[1:2]) 
df[rep(seq_len(nrow(df)), each=2),]

Overall_Reduced <- do.call("rbind", replicate(15, Overall_Reduce, simplify = FALSE))
Overall_Reduced <- Overall_Reduced %>%
  mutate(sellout_append_t = sellout_append,
         lwr_t = lwr,
         Pred_t = Pred,
         upr_t = upr) %>%
  select(-sellout_append, -lwr, -Pred, -upr)


Bad_agg <- Bad %>%
  mutate(sellout_append = as.numeric(sellout_append)) %>%
  group_by(PPID) %>%
  summarise(sellout_total = sum(sellout_append, na.rm=T)) %>%
  mutate(sellout_entire = sum(sellout_total, na.rm=T)) %>%
  mutate(ratio = sellout_total/sellout_entire)


Bad_final <- Bad %>%
  left_join(Bad_agg, by="PPID") %>%
  mutate(sellout_append = as.numeric(sellout_append)) %>%
  arrange(PPID, fiscal.Q) %>%
  cbind(Overall_Reduced[, c("sellout_append_t", "lwr_t", "Pred_t", "upr_t")]) %>%
  mutate(Pred_hier = Pred_t*ratio) %>%
  filter(fiscal.Q %in% c("FY16-Q4", "FY17-Q1", "FY17-Q2"))


group_hier <- as.character(Bad_final$PPID)
out_hier <- ModelEvaluate(Bad_final$Pred_hier, Bad_final$sellout_append, group_hier)

ACC_hier <- out_hier$ACC

write.csv(Bad_final, "Bad_final.csv")
write.csv(ACC_hier, "ACC_hier.csv")

