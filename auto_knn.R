# Install and load the packages
install.packages("tidyverse","class","caret")
install.packages("FSelector")
install.packages("remotes")
install.packages("ROSE")
library(tidyverse)
library(class)
library(caret)
library(FSelector)
library(ROSE)

# load the data and make a copy 
auto_insurance <- read.csv("Marketing & CLV.csv")
use <- auto_insurance 
auto_insurance
str(auto_insurance)

# Clean the data  
# task 1: identify any duplicated data 
use[duplicated(use$Customer)] 

# task 2: identify any missing data 
sum(is.na(use))

# Perform EDA 
table(use$State)
ggplot(use, aes(x = State))+
  geom_bar(stat = "count", width = 0.8, fill = "grey")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  labs(title = "Bar chart to display state comparision",
       xlab = "State", ylab = "Count")

ggplot(use, aes(x = Response))+
  geom_bar(stat = "count", width = 0.8, fill = "grey")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+
  labs(title = "Bar chart to display state comparision",
       xlab = "Respend", ylab = "Count")

# we need to code categorical features into numerical format
# target encode function 
target_enc <- function(data, enc_col, tar_col, k_col,
                       kmin, kmax){
  require(tidyverse)
  col_name <- paste0("tar_enc_", enc_col)
  
  temp <- map_df(kmin:kmax, function(k){
    
    xtrain <- data[data[k_col] != k, ]
    xvalid <- data[data[k_col] == k, ]
    
    feat <- xtrain %>% group_by_at(enc_col) %>%
      summarise_at(.vars = tar_col, mean)
    
    colnames(feat)[2] <- col_name
    temp_df <- xvalid %>% left_join(feat, by = enc_col) %>% 
      select(all_of(enc_col), all_of(col_name))
    
    return(temp_df)
  })
  
  temp_enc <- temp %>% group_by_at(enc_col) %>% 
    summarise_at(.vars = col_name, .funs = mean) %>% 
    ungroup()
  
  data %>% left_join(temp_enc, by = enc_col)
  
}

# simple encoding function 
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

# encode response (NO <- 1, Yes <- 2)
encode_ordinal(use$Response, order = c("No", "Yes"))
table(unlist(use$Response))
use[["response_encoded"]] <- encode_ordinal(use[["Response"]])

# encode state (Arizona <- 1, California <- 2, Nevada <- 3, Oregon <- 4, Washington <- 5)
encode_ordinal(use$State, order = c("Arizona", "California", "Nevada", "Oregon", "Washington"))
use[["state_encoded"]] <- encode_ordinal(use[["State"]])

# encode coverage (Basic <-1, Extended <-2, Premium <- 3)
encode_ordinal(use$Coverage, order = c("Basic", "Extended", "Premium"))
use[["coverage_encoded"]] <- encode_ordinal(use[["Coverage"]])

# encode education (high school or below <- 1, college <- 2, bachelor <- 3, master <- 4, doctor <- 5)
encode_ordinal(use$Education, order = c("High School or Below", "College", "Bachelor", "Master", "Doctor"))
use[["education_encoded"]] <- encode_ordinal(use[["Education"]])

# encode employment status (employed <- 1, unemployed <- 2, medical leave <- 3, disabled <- 4, retired <- 5)
encode_ordinal(use$EmploymentStatus, order = c("Employed", "Unemployed", "Medical Leave", "Disabled", "Retired"))
use[["employment_encoded"]] <- encode_ordinal(use[["EmploymentStatus"]])

# encode gender (f <- 1, m <-2)
encode_ordinal(use$Gender, order = c("F", "M"))
use[["gender_encoded"]] <- encode_ordinal(use[["Gender"]])

# encode location (rural <- 1, suburban <- 2, urban <- 3)
encode_ordinal(use$Location.Code, order = c("Rural", "Suburban", "Urban"))
use[["location_encoded"]] <- encode_ordinal(use[["Location.Code"]])

# encode marital status (single <- 1, married <- 2, divorced <- 3)
encode_ordinal(use$Marital.Status, order = c("Single", "Married", "Divorced"))
use[["marital_encoded"]] <- encode_ordinal(use[["Marital.Status"]])

# encode policy (pl1 <- 1, pl2 <-2, pl3 <- 3, cl1 <- 4, cl2 <- 5, cl3 <- 6, sl1 <- 7, sl2 <- 8, sl3 <- 9)
encode_ordinal(use$Policy, order = c("Personal L1", "Personal L2", "Personal L3",
                                     "Corporate L1", "Corporate L2", "Corporate L3",
                                     "Special L1", "Special L2", "Special L3"))
use[["policy_encoded"]] <- encode_ordinal(use[["Policy"]])

# encode vehicle class (four-door <- 1, luxury car <- 2, luxury suv <- 3, sport car <- 4, suv <- 5, two-door <- 6)
encode_ordinal(use$Vehicle.Class, order = c("Four-Door Car", "Luxury Car", "Luxury SUV",
                                            "Sports Car", "SUV", "Two-Door Car"))
use[["vehicle_encoded"]] <- encode_ordinal(use[["Vehicle.Class"]])

# encode vehicle size 
encode_ordinal(use$Vehicle.Size, order = c("Small", "Medsize", "Large"))
use[["vehicle_size_encoded"]] <- encode_ordinal(use[["Vehicle.Size"]])

# filter the columns that we need and are encoded 
new_use <- use %>%
  select(-c(Response, State, Coverage, Education, EmploymentStatus, Gender, Location.Code,
            Marital.Status, Policy, Vehicle.Class, Vehicle.Size, Customer, Effective.To.Date, 
            Policy.Type, Renew.Offer.Type, Sales.Channel))
View(new_use)

# normalise the data (min-max method)
new_normal_use <- new_use
process <- preProcess(new_normal_use, method = c("range"))
new_normal_use <- predict(process,new_normal_use)
View(new_normal_use)

# feature selection 
# information gain 
set.seed(111)
weights_new_normal_use <- information.gain(response_encoded~., new_normal_use)
print(weights_new_normal_use)
subset_info_auto_0.2 <- cutoff.k.percent(weights_new_normal_use, 0.2)
subset_info_auto_0.4 <- cutoff.k.percent(weights_new_normal_use, 0.4)
print(subset_info_auto_0.2)
print(subset_info_auto_0.2)

# relief
weights_new_normal_use <- relief(response_encoded~., new_normal_use, neighbours.count = 5, sample.size = 20)
subset_relief_auto_0.2 <-  cutoff.k.percent(weights_new_normal_use, 0.2)
subset_relief_auto_0.4 <-  cutoff.k.percent(weights_new_normal_use, 0.4)
print(subset_relief_auto_0.2)
print(subset_relief_auto_0.4)
print(weights_new_normal_use)

# CFS
subset_new_normal_use_cfs <- cfs(response_encoded~., new_normal_use)
print(subset_new_normal_use_cfs)

# FCFB
weights_new_normal_use_fcbf <- symmetrical.uncertainty(response_encoded~., new_normal_use)
subset_fcbf_auto_0.2 <- cutoff.k.percent(weights_new_normal_use_fcbf, 0.2)
subset_fcbf_auto_0.4 <- cutoff.k.percent(weights_new_normal_use_fcbf, 0.4)
print(subset_fcbf_auto_0.4)
print(weights_new_normal_use_fcbf)

# Chi-square
weights_new_normal_use_chi <- chi.squared(response_encoded~., new_normal_use)
subset_chi_auto_0.2 <- cutoff.k.percent(weights_new_normal_use_chi, 0.2)
subset_chi_auto_0.4 <- cutoff.k.percent(weights_new_normal_use_chi, 0.4)
print(subset_chi_auto_0.4)
print(weights_new_normal_use_chi)

# We use two set of feature which are FCFB and Relief
fcfb <- new_normal_use %>% 
  select(c("employment_encoded","state_encoded", "Income", "Total.Claim.Amount",
           "location_encoded","marital_encoded","response_encoded"))
relief <- new_normal_use %>% 
  select(c("vehicle_size_encoded","Number.of.Policies", "Number.of.Open.Complaints",
           "Months.Since.Last.Claim","Months.Since.Policy.Inception","employment_encoded","response_encoded"))

# deal with unbalanced dataset (Randomly Over Sampling method)
test_fcfb$response_encoded<- as.factor(test_fcfb$response_encoded)

fcfb_rose <- ROSE(response_encoded ~ ., data = fcfb, seed = 1)$data
table(fcfb_rose$response_encoded)

relief_rose <- ROSE(response_encoded ~ ., data = relief, seed = 1)$data
table(relief_rose$response_encoded)

# spilt the data into 70(train) / 30(test)
set.seed(1)

sample <- sample(c(TRUE, FALSE), nrow(fcfb_rose), replace=TRUE, prob=c(0.7,0.3))
train_fcfb  <- fcfb_rose[sample, ]
test_fcfb   <- fcfb_rose[!sample, ]

set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(relief_rose), replace=TRUE, prob=c(0.7,0.3))
train_relief  <- relief_rose[sample, ]
test_relief   <- relief_rose[!sample, ]

# build knn model 
# FCFB, k = 17 is the best with accuracy 67.98% kappa =  0.3583399
trControl <- trainControl(method = "cv", number = 5)
fit <- train(response_encoded~ .,
             method = "knn",
             tuneGrid = expand.grid(k = 50:95),
             trControl = trControl,
             metric = "Accuracy",
             data = fcfb_rose)
fit_50 <-  train(response_encoded~ .,
                 method = "knn",
                 tuneGrid = expand.grid(k = 1:50),
                 trControl = trControl,
                 metric = "Sensitivity",
                 data = fcfb_rose)

# relief k = 18 is the best with accuracy 66.9%
relief_rose$response_encoded<- as.factor(relief_rose$response_encoded)
fit_relief <- train(response_encoded~ .,
             method = "knn",
             tuneGrid = expand.grid(k = 50:95),
             trControl = trControl,
             metric = "Accuracy",
             data = relief_rose)
fit_relief

# k = 18 
fit_relief_50 <- train(response_encoded~ .,
                 method = "knn",
                 tuneGrid = expand.grid(k = 1:50),
                 trControl = trControl,
                 metric = "Accuracy",
                 data = relief_rose)
fit_relief_50

table(buy_pred_fcfb_95, buy_actual_fcfb_95)
mean(buy_pred_fcfb_95 == buy_actual_fcfb_95)

# show the confusion matrix (FCFB with k = 17)
buy_pred_fcfb_17 <- knn(train = train_fcfb[-7], test = test_fcfb[-7], cl = buy_type,
                        k = 17)
buy_actual_fcfb_17 <- test_fcfb$response_encoded
table(buy_pred_fcfb_17, buy_actual_fcfb_17)
mean(buy_pred_fcfb_17== buy_actual_fcfb_17)