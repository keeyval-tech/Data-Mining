library("rsample")
library("caret")
library("C50")
library("rJava")
library("RWeka")
library("dplyr")
library(FSelector)
library(Boruta)
library(MLmetrics)
library(kernlab)
library(randomForest)
library("e1071")
library(pROC)
library(kknn)


###################### PRE-Processing #############################


#assigning the given csv file to a dataframe
assign.df <- read.csv("/Users/keval/Downloads/project_dataset_5K.csv")

View(assign.df)


# total missing values
sum(is.na(assign.df))


# returns colums with missing values
misscol <- colSums(is.na(assign.df))

# retuns only those coloms which have less than 3000 missing values
cols_to_keep <- misscol <= 3000


# creates a new dataframe with coloums having less than 3000 missing values
cleaned_data_col <- assign.df[, cols_to_keep]


sum(is.na(cleaned_data_col))

View(cleaned_data_col)


# CHECKING FOR ATTRIBUTES WITH ZERO VARIANCE
nearZeroVar(cleaned_data_col, names= TRUE)

# removing attributes with zero variance
cleaned_data_col <- cleaned_data_col %>%
  select(-c(IYEAR, SAFETIME, CTELNUM1, CELLFON5, CADULT1, PVTRESD3, CVDSTRK3, 
            CHCKDNY2, DIFFDRES, DRNKDRI2, USENOW3, VIRCOLON, HIVRISK5, TRNSGNDR,
            QSTLANG, X_VIRCOLN))

View(cleaned_data_col)

### Replaced NA values 
getMode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Changes Char NA data to mode and Num NA data to median
cleaned_data_col <- lapply(cleaned_data_col, function(x) {
  # For categorical data
  if(is.factor(x) || is.character(x)) {
    mode_value <- getMode(x[!is.na(x)])
    x[is.na(x)] <- mode_value
  } 
  # For numeric data, replacing NA with median as an example
  else if(is.numeric(x)) {
    median_value <- median(x, na.rm = TRUE)
    x[is.na(x)] <- median_value
  }
  return(x)
})

# Convert to a data frame 
cleaned_data_col <- as.data.frame(cleaned_data_col)

# Check again for NA values
sum(is.na(cleaned_data_col))


# Checking for correlation between 2 attributes 
cc <- cor(cleaned_data_col[,1:135], use = "complete.obs")
cc

# finding coloums with more than 80% correlation 
high_cor <- findCorrelation(cc, cutoff = 0.8, verbose = TRUE)

#creating a new data frame called data_cleaned with coloums
#less than 80% correlation
data_cleaned <- cleaned_data_col[,-high_cor]

View(data_cleaned)


data_cleaned$Class <- ifelse(data_cleaned$Class == 'Y', 1, 0)

## Using Box plot to check for any major outliers 
#for FMONTH
ggplot(data_cleaned) + geom_boxplot(aes(y= FMONTH)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#for X_STATE
ggplot(data_cleaned) + geom_boxplot(aes(y= X_STATE)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for IDATE
ggplot(data_cleaned) + geom_boxplot(aes(y= IDATE)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#for IDAY
ggplot(data_cleaned) + geom_boxplot(aes(y= IDAY)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for DISPCODE
ggplot(data_cleaned) + geom_boxplot(aes(y= DISPCODE)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for X_PSU
ggplot(data_cleaned) + geom_boxplot(aes(y= X_PSU)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#for CELLSEX
ggplot(data_cleaned) + geom_boxplot(aes(y= CELLSEX)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CSTATE1
ggplot(data_cleaned) + geom_boxplot(aes(y= CSTATE1)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for LANDLINE
ggplot(data_cleaned) + geom_boxplot(aes(y= LANDLINE)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for HHADULT
ggplot(data_cleaned) + geom_boxplot(aes(y= HHADULT)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for SEXVAR
ggplot(data_cleaned) + geom_boxplot(aes(y= SEXVAR)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for GENHLTH
ggplot(data_cleaned) + geom_boxplot(aes(y= GENHLTH)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#for PHSYHLTH
ggplot(data_cleaned) + geom_boxplot(aes(y= PHYSHLTH)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for HLTHPLN1
ggplot(data_cleaned) + geom_boxplot(aes(y= HLTHPLN1)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for PERSDOC2
ggplot(data_cleaned) + geom_boxplot(aes(y= PERSDOC2)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for MEDCOST
ggplot(data_cleaned) + geom_boxplot(aes(y= MEDCOST)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CHECKUP1
ggplot(data_cleaned) + geom_boxplot(aes(y= CHECKUP1)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for SLEPTIM1
ggplot(data_cleaned) + geom_boxplot(aes(y= SLEPTIM1)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CVDINFR4
ggplot(data_cleaned) + geom_boxplot(aes(y= CVDINFR4)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CVDCRHD4
ggplot(data_cleaned) + geom_boxplot(aes(y= CVDCRHD4)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for ASTHMA3
ggplot(data_cleaned) + geom_boxplot(aes(y= ASTHMA3)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CHCSCNCR
ggplot(data_cleaned) + geom_boxplot(aes(y= CHCSCNCR)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CHCOCNCR
ggplot(data_cleaned) + geom_boxplot(aes(y= CHCOCNCR)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CHCCOPD2
ggplot(data_cleaned) + geom_boxplot(aes(y= CHCCOPD2)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for HAVARTH4
ggplot(data_cleaned) + geom_boxplot(aes(y= HAVARTH4)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



#for DIABETE4
ggplot(data_cleaned) + geom_boxplot(aes(y= DIABETE4)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for LASTDEN4
ggplot(data_cleaned) + geom_boxplot(aes(y= LASTDEN4)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for RMVTETH4
ggplot(data_cleaned) + geom_boxplot(aes(y= FMONTH)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for MARITAL
ggplot(data_cleaned) + geom_boxplot(aes(y= MARITAL)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for RENTHOM1
ggplot(data_cleaned) + geom_boxplot(aes(y= RENTHOM1)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CPDEOM1B
ggplot(data_cleaned) + geom_boxplot(aes(y= CPDEMO1B)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for VETERAN3
ggplot(data_cleaned) + geom_boxplot(aes(y= VETERAN3)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for EMPLOY1
ggplot(data_cleaned) + geom_boxplot(aes(y= EMPLOY1)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for CHILDREN
ggplot(data_cleaned) + geom_boxplot(aes(y= CHILDREN)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#for INCOME2
ggplot(data_cleaned) + geom_boxplot(aes(y= INCOME2)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for WEIGHT2
ggplot(data_cleaned) + geom_boxplot(aes(y= WEIGHT2)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for HEIGHT3
ggplot(data_cleaned) + geom_boxplot(aes(y= HEIGHT3)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for DEAF
ggplot(data_cleaned) + geom_boxplot(aes(y= DEAF)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for BLIND
ggplot(data_cleaned) + geom_boxplot(aes(y= BLIND)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for DECIDE
ggplot(data_cleaned) + geom_boxplot(aes(y= DECIDE)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for DIFFWALK
ggplot(data_cleaned) + geom_boxplot(aes(y= DIFFWALK)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for DIFFALON
ggplot(data_cleaned) + geom_boxplot(aes(y= DIFFALON)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



#for SMOKE100
ggplot(data_cleaned) + geom_boxplot(aes(y= SMOKE100)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



#for AVEDRNK3
ggplot(data_cleaned) + geom_boxplot(aes(y= AVEDRNK3)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for DRNK3GES
ggplot(data_cleaned) + geom_boxplot(aes(y= DRNK3GE5)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

#for MAXDRNKS
ggplot(data_cleaned) + geom_boxplot(aes(y= MAXDRNKS)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for FLUSHOT7
ggplot(data_cleaned) + geom_boxplot(aes(y= FLUSHOT7)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for FLSHTMY3
ggplot(data_cleaned) + geom_boxplot(aes(y= FLSHTMY3)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for SHINGLE2
ggplot(data_cleaned) + geom_boxplot(aes(y= SHINGLE2)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


#for PNEUVAC4
ggplot(data_cleaned) + geom_boxplot(aes(y= FMONTH)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



### Reading the preprocessed data ####
write.csv(data_cleaned, "preprocessed_data.csv", row.names = FALSE)

df <- data_cleaned

set.seed(123)

### Splitting data into 70 and 30 #####
splt <- initial_split(df, prop = 0.7, strata = Class)


#train and test Data
tr <- training(splt)
ts <- testing(splt)

### Dowloading intial train and test data ###

write.csv(tr , "initial_train.csv", row.names = FALSE)
write.csv(ts , "initial_test.csv", row.names = FALSE)



### Balancing the train data (undersample)######
# Identify the class with fewer samples
class_counts <- table(tr$Class)
minority_class <- names(class_counts)[which.min(class_counts)]
minority_count <- min(class_counts)

# Undersample the majority class
undersampled_majority <- tr[tr$Class != minority_class, ]
undersampled_majority <- undersampled_majority[sample(nrow(undersampled_majority), minority_count), ]

# Combine balanced data
balanced_data <- rbind(subset(tr, Class == minority_class), undersampled_majority)

# Shuffle the rows
balanced_data <- balanced_data[sample(nrow(balanced_data)), ]

# Check class distribution
table(balanced_data$Class)

balanced_data_us <- balanced_data


### Balanced the train data (Oversampled) ####

# Verify the change
table(tr$Class)

# Assuming now Class 0 is the majority and Class 1 is the minority

# Calculate the count of each class
class_0_count <- sum(tr$Class == 0)
class_1_count <- sum(tr$Class == 1)

# Perform random oversampling of the minority class (Class 1)
oversampled_class_1 <- tr %>%
  filter(Class == 1) %>%
  sample_n(size = class_0_count, replace = TRUE) # Oversample with replacement

# Combine the oversampled Class 1 with the original Class 0 observations
balanced_data_os <- bind_rows(oversampled_class_1, filter(tr, Class == 0))

# Check the new class distribution
table(balanced_data_os$Class)

################################################################################

### Undersample feature selection - Information Gain ####

feature_infogain <- information.gain(Class~., balanced_data_us)

feature_infogain <- cbind(rownames(feature_infogain), data.frame(feature_infogain, row.names = NULL))

names(feature_infogain) <- c("Attribute", "Info Gain")

sorted.feature_infogain <- feature_infogain[order(-feature_infogain$'Info Gain'),]

selected_components <- sorted.feature_infogain[1:10,]

selected_components
# Extract the names of the top 10 features
top_features <- as.character(selected_components$Attribute)

# Subset the original dataset to include only the selected features and the target variable
final_dataset_info_gain <- balanced_data_us[, c(top_features, "Class")]

# View the first few rows of the new dataset to confirm
head(final_dataset_info_gain)

View(final_dataset_info_gain)



########### Undersample feature selection - CFS ####

feature_cfs <- cfs(Class~., balanced_data_us)

feature_cfs

dataset_cfs <- balanced_data_us[, feature_cfs]

dataset_cfs$Class <- balanced_data_us$Class


View(dataset_cfs)

table(dataset_cfs$Class)


############### Undersample feature selection - boruta ####
set.seed(123)
feature_boruta <- Boruta(Class~., data = balanced_data)
feature_boruta

# Get the list of important attributes
important_attributes <- getSelectedAttributes(feature_boruta)

dataset_boruta <- balanced_data[, important_attributes]
View(dataset_boruta)

# Add the "Class" attribute to the new dataset
dataset_boruta$Class <- balanced_data$Class

table(dataset_boruta$Class)


############# Oversample feature Selection - Information Gain ###############
set.seed(123)

### Feature selection using informaion gain
balanced_data_oversampled <- balanced_data_os

feature_infogain <- information.gain(Class~., balanced_data_oversampled)

feature_infogain <- cbind(rownames(feature_infogain), data.frame(feature_infogain, row.names = NULL))

names(feature_infogain) <- c("Attribute", "Info Gain")

sorted.feature_infogain <- feature_infogain[order(-feature_infogain$'Info Gain'),]

selected_components <- sorted.feature_infogain[1:20,]

selected_components
# Extract the names of the top 10 features
top_features <- as.character(selected_components$Attribute)

# Subset the original dataset to include only the selected features and the target variable
final_dataset_info_gain_os <- balanced_data_oversampled[, c(top_features, "Class")]

# View the first few rows of the new dataset to confirm
head(final_dataset_info_gain_os)
table(final_dataset_info_gain_os$Class)



############# Oversample feature Selection - CFS ###############
set.seed(123)

balanced_data_oversampled_cfs<- balanced_data_os

feature_cfs <- cfs(Class~., balanced_data_oversampled_cfs)

feature_cfs

dataset_cfs_os <- balanced_data_oversampled_cfs[, feature_cfs]

dataset_cfs_os$Class <- balanced_data_oversampled_cfs$Class


table(dataset_cfs_os$Class)


################ Oversample feature Selection - Boruta ###############
set.seed(123)
feature_boruta <- Boruta(Class~., data = balanced_data_os)
feature_boruta

# Get the list of important attributes
important_attributes <- getSelectedAttributes(feature_boruta)

dataset_boruta_os <- balanced_data_os[, important_attributes]
View(dataset_boruta_os)

# Add the "Class" attribute to the new dataset
dataset_boruta_os$Class <- balanced_data_os$Class

table(dataset_boruta_os$Class)


#############################################################################

################## Undersampled Info Gain on 6 classifiers ###############

########### Using KNN #######################

dataset_info_gain_us_knn <- final_dataset_info_gain
test_data_info_gain_us_knn <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_info_gain_us_knn$Class <- factor(dataset_info_gain_us_knn$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Setting up trainControl with repeated cross-validation
train_control <- trainControl(method = "repeatedcv", number = 20, repeats = 5, summaryFunction = defaultSummary)

# Training a KNN model with data preprocessing and tuning
knn_model_tuning <- train(Class~., data = dataset_info_gain_us_knn, method = "knn",
                          trControl = train_control, preProcess = c("center", "scale"),
                          tuneLength = 10)

# Display the model details
knn_model_tuning

###test KNN
# Convert 'Class' variable in test data to factor
test_data_info_gain_us_knn$Class <- factor(test_data_info_gain_us_knn$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_knn <- predict(knn_model_tuning, newdata = test_data_info_gain_us_knn)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_knn, reference = test_data_info_gain_us_knn$Class)

conf_matrix

#### Performance metrics for KNN
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)

### roc


prediction_rf_probs <- predict(knn_model_tuning, newdata = test_data_info_gain_us_knn, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_info_gain_us_knn$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_info_gain_us_knn$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
# Assuming equal weights for simplicity; adjust weights based on your specific needs
weights <- c(0.5, 0.5)  # Adjust based on class distribution or other criteria if needed
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



############# Using Random forest ##############

# Reading the datasets
dataset_info_gain_us_rf <- final_dataset_info_gain
test_data_info_gain_us_rf <- ts

set.seed(123)

# Factoring the class variable in the dataset
dataset_info_gain_us_rf$Class <- factor(dataset_info_gain_us_rf$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Verify the change
print(table(dataset_info_gain_us_rf$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "cv",summaryFunction = twoClassSummary, 
                              classProbs = TRUE, savePredictions = TRUE)

View(dataset_info_gain_us_rf)
# Training a rf model
rf_model_tuning <- train(x = dataset_info_gain_us_rf[,-which(names(dataset_info_gain_us_rf) == "Class")], y = dataset_info_gain_us_rf$Class, method = "rf",
                         ntree = 500, importance = TRUE, metric = "ROC",
                         trControl = train_control)

rf_model_tuning


# Ensure test_data$Class is correctly factored
test_data_info_gain_us_rf$Class <- factor(test_data_info_gain_us_rf$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))
table(test_data_info_gain_us_rf$Class)

# Generate predictions
prediction_rf <- predict(rf_model_tuning, newdata = test_data_info_gain_us_rf)

# Inspect the predictions to ensure they're valid and contain both classes
prediction_rf

# Recalculate the confusion matrix with corrected factor levels
conf_matrix <- confusionMatrix(data = prediction_rf, reference = test_data_info_gain_us_rf$Class)

# Print the confusion matrix
conf_matrix

#### Performance metrics for Random Forest
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rf_model_tuning, newdata = test_data_info_gain_us_rf, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_info_gain_us_rf$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_info_gain_us_rf$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
# Assuming equal weights for simplicity; adjust weights based on your specific needs
weights <- c(0.5, 0.5)  # Adjust based on class distribution or other criteria if needed
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


####### Using Logistical Regression ###########

# Reading the datasets
dataset_info_gain_us_lr <- final_dataset_info_gain
test_data_info_gain_us_lr <- ts

set.seed(123)

# Ensure 'Class' is a factor in both datasets with the same levels
dataset_info_gain_us_lr$Class <- factor(dataset_info_gain_us_lr$Class)
test_data_info_gain_us_lr$Class <- factor(test_data_info_gain_us_lr$Class, levels = levels(dataset_info_gain_us_lr$Class))

# Train the Logistic Regression model
log_reg_model <- train(Class ~ ., data = dataset_info_gain_us_lr, method = "glm", family = "binomial")

# Predict using the trained Logistic Regression model on the test data
prediction_log_reg <- predict(log_reg_model, newdata = test_data_info_gain_us_lr)

# Calculate confusion matrix
conf_matrix_log_reg <- confusionMatrix(data = prediction_log_reg, reference = test_data_info_gain_us_lr$Class)

# Print confusion matrix
conf_matrix_log_reg


#### Performance metrics for Logistical Regression
cm <- conf_matrix_log_reg$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)

### roc


prediction_rf_probs <- predict(log_reg_model, newdata = test_data_info_gain_us_lr, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_info_gain_us_lr$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_info_gain_us_lr$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



########### Using Rpart #######
# Reading the datasets
dataset_info_gain_us_rpart <- final_dataset_info_gain
test_data_info_gain_us_rpart <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_info_gain_us_rpart$Class <- factor(dataset_info_gain_us_rpart$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Setting up trainControl as before
train_control <- trainControl(method = "cv", number = 500, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)

# Attempting to train the model again with the updated Class variable
rpart_model <- train(Class ~ ., data = dataset_info_gain_us_rpart, method = "rpart",
                     trControl = train_control, tuneLength = 10, metric = "ROC")

rpart_model


##testing the rpart
# Convert 'Class' variable in test data to factor
test_data_info_gain_us_rpart$Class <- factor(test_data_info_gain_us_rpart$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

# Generating predictions
prediction_rpart <- predict(rpart_model, newdata = test_data_info_gain_us_rpart, type = "raw")


# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_rpart, reference = test_data_info_gain_us_rpart$Class)

conf_matrix


#### Performance metrics for Rpart
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rpart_model, newdata =test_data_info_gain_us_rpart, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_info_gain_us_rpart$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_info_gain_us_rpart$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



####### Using J48 ######
# Reading the datasets
dataset_info_gain_us_J48 <- final_dataset_info_gain
test_data_info_gain_us_J48 <- ts

set.seed(123)

# Factoring the class variable in the dataset
dataset_info_gain_us_J48$Class <- factor(dataset_info_gain_us_J48$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

View(dataset_info_gain_us_J48)


###Using tuning

train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 5, summaryFunction = defaultSummary)


df_j48_model <- train(Class~., data = dataset_info_gain_us_J48, method= "J48", 
                      trControl = train_control)

df_j48_model


####testing the model
# Convert 'Class' variable in test data to factor
test_data_info_gain_us_J48$Class <- factor(test_data_info_gain_us_J48$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))


pred_using_J48 <- predict(df_j48_model, newdata = test_data_info_gain_us_J48, type = "raw")

### performance measures

# Calculate confusion matrix
performance_measures_J48 <- confusionMatrix(data = pred_using_J48, reference = test_data_info_gain_us_J48$Class)

performance_measures_J48

#### Performance metrics for J48
cm <- performance_measures_J48$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(df_j48_model, newdata =test_data_info_gain_us_J48, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_info_gain_us_J48$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_info_gain_us_J48$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


########### Using SVM ########

# Reading the datasets
dataset_info_gain_us_SVM <- final_dataset_info_gain
test_data_info_gain_us_SVM <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_info_gain_us_SVM$Class <- factor(dataset_info_gain_us_SVM$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Verify the change
print(table(dataset_info_gain_us_SVM$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "repeatedcv", number = 20, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a neural network model
SVM_model_tuning <- train(Class ~ ., data = dataset_info_gain_us_SVM, method = "svmRadial",
                          trControl = train_control)

SVM_model_tuning

# Test SVM
test_data_info_gain_us_SVM$Class <- factor(test_data_info_gain_us_SVM$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_svm <- predict(SVM_model_tuning, newdata = test_data_info_gain_us_SVM)

prediction_svm
# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_svm, reference = test_data_info_gain_us_SVM$Class)

# Print the confusion matrix
conf_matrix

#### Performance metrics for SVM
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(SVM_model_tuning, newdata =test_data_info_gain_us_SVM, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_info_gain_us_SVM$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_info_gain_us_SVM$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


################################################################################

################## Undersampled CFS on 6 classifiers ###############

########### Using KNN #######################
# Reading the datasets
dataset_cfs_us_knn <- dataset_cfs
test_data_cfs_us_knn <- ts

set.seed(123)

# Factoring the class variable in the dataset
dataset_cfs_us_knn$Class <- factor(dataset_cfs_us_knn$Class, levels = c("0", "1"), 
                            labels = c("Class0", "Class1"))

# Setting up trainControl with repeated cross-validation
train_control <- trainControl(method = "repeatedcv", number = 60, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a KNN model with data preprocessing and tuning
knn_model_tuning <- train(Class~., data = dataset_cfs_us_knn, method = "knn",
                          trControl = train_control, preProcess = c("center", "scale"),
                          tuneLength = 10)


knn_model_tuning

###test KNN
# Convert 'Class' variable in test data to factor
test_data_cfs_us_knn$Class <- factor(test_data_cfs_us_knn$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_knn <- predict(knn_model_tuning, newdata = test_data_cfs_us_knn)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_knn, reference = test_data_cfs_us_knn$Class)

conf_matrix

#### Performance metrics for KNN
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(knn_model_tuning, newdata =test_data_cfs_us_knn, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_cfs_us_knn$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_cfs_us_knn$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


########## Using Random Forest #######

# Reading the datasets
dataset_cfs_us_rf <- dataset_cfs
test_data_cfs_us_rf <- ts


set.seed(123)
# Factoring the class variable in the dataset
dataset_cfs_us_rf$Class <- factor(dataset_cfs_us_rf$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Verify the change
print(table(dataset_cfs_us_rf$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "cv",summaryFunction = twoClassSummary, 
                              classProbs = TRUE, savePredictions = TRUE)

View(dataset_cfs_us_rf)
# Training a rf model
rf_model_tuning <- train(x = dataset_cfs_us_rf[,-which(names(dataset_cfs_us_rf) == "Class")], y = dataset_cfs_us_rf$Class,
                         method = "rf",ntree = 500, importance = TRUE, 
                         metric = "ROC", trControl = train_control)

rf_model_tuning


# Ensure test_data$Class is correctly factored
test_data_cfs_us_rf$Class <- factor(test_data_cfs_us_rf$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))
table(test_data_cfs_us_rf$Class)

# Generate predictions
prediction_rf <- predict(rf_model_tuning, newdata = test_data_cfs_us_rf)

# Inspect the predictions to ensure they're valid and contain both classes
prediction_rf

# Recalculate the confusion matrix with corrected factor levels
conf_matrix <- confusionMatrix(data = prediction_rf, reference = test_data_cfs_us_rf$Class)

# Print the confusion matrix
conf_matrix

#### Performance metrics for Random Forest
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rf_model_tuning, newdata =test_data_cfs_us_rf, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_cfs_us_rf$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_cfs_us_rf$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


####### Using Logistical Regression ##########

# Reading the datasets
dataset_cfs_us_lr <- dataset_cfs
test_data_cfs_us_lr <- ts

set.seed(123)

# Ensure 'Class' is a factor in both datasets with the same levels
dataset_cfs_us_lr$Class <- factor(dataset_cfs_us_lr$Class)
test_data_cfs_us_lr$Class <- factor(test_data_cfs_us_lr$Class, levels = levels(dataset_cfs_us_lr$Class))

# Train the Logistic Regression model
log_reg_model <- train(Class ~ ., data = dataset_cfs_us_lr, method = "glm", family = "binomial")

# Predict using the trained Logistic Regression model on the test data
prediction_log_reg <- predict(log_reg_model, newdata = test_data_cfs_us_lr)

# Calculate confusion matrix
conf_matrix_log_reg <- confusionMatrix(data = prediction_log_reg, reference = test_data_cfs_us_lr$Class)

# Print confusion matrix
conf_matrix_log_reg

#### Performance metrics for Logistical Regression
cm <- conf_matrix_log_reg$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(log_reg_model, newdata =test_data_cfs_us_lr, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_cfs_us_lr$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_cfs_us_lr$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



############# Using Rpart ###############

# Reading the datasets
dataset_cfs_us_rpart <- dataset_cfs
test_data_cfs_us_rpart <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_cfs_us_rpart$Class <- factor(dataset_cfs_us_rpart$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Setting up trainControl as before
train_control <- trainControl(method = "cv", number = 500, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)

# Attempting to train the model again with the updated Class variable
rpart_model <- train(Class ~ ., data = dataset_cfs_us_rpart, method = "rpart",
                     trControl = train_control, tuneLength = 10, metric = "ROC")

rpart_model


##testing the rpart
# Convert 'Class' variable in test data to factor
test_data_cfs_us_rpart$Class <- factor(test_data_cfs_us_rpart$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

# Generating predictions
prediction_rpart <- predict(rpart_model, newdata = test_data_cfs_us_rpart, type = "raw")


# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_rpart, reference = test_data_cfs_us_rpart$Class)

conf_matrix

#### Performance metrics for Logistical Regression
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rpart_model, newdata =test_data_cfs_us_rpart, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_cfs_us_rpart$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_cfs_us_rpart$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



############### Using J48 ############

# Reading the datasets
dataset_cfs_us_J48 <- dataset_cfs
test_data_cfs_us_J48 <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_cfs_us_J48$Class <- factor(dataset_cfs_us_J48$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


###Using tuning

train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 5, summaryFunction = defaultSummary)

# Convert the 'Class' variable to factor

df_j48_model <- train(Class~., data = dataset_cfs_us_J48, method = "J48",
                      trControl = train_control)

df_j48_model


####testing the model
# Convert 'Class' variable in test data to factor
test_data_cfs_us_J48$Class <- factor(test_data_cfs_us_J48$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))


pred_using_J48 <- predict(df_j48_model, newdata = test_data_cfs_us_J48, type = "raw")

### performance measures

# Calculate confusion matrix
performance_measures_J48 <- confusionMatrix(data = pred_using_J48, reference = test_data_cfs_us_J48$Class)

performance_measures_J48


#### Performance metrics for J48
cm <- performance_measures_J48$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(df_j48_model, newdata =test_data_cfs_us_J48, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_cfs_us_J48$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_cfs_us_J48$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results




######### Using SVM #################

# Reading the datasets
dataset_cfs_us_svm <- dataset_cfs
test_data_cfs_us_svm <- ts

set.seed(123)

# Factoring the class variable in the dataset
dataset_cfs_us_svm$Class <- factor(dataset_cfs_us_svm$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Verify the change
print(table(dataset_cfs_us_svm$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "repeatedcv", number = 20, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a neural network model
SVM_model_tuning <- train(Class ~ ., data = dataset_cfs_us_svm, method = "svmRadial",
                          trControl = train_control)

SVM_model_tuning

# Test SVM
test_data_cfs_us_svm$Class <- factor(test_data_cfs_us_svm$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_svm <- predict(SVM_model_tuning, newdata = test_data_cfs_us_svm)

prediction_svm
# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_svm, reference = test_data_cfs_us_svm$Class)

# Print the confusion matrix
conf_matrix

#### Performance metrics for SVM

cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(SVM_model_tuning, newdata =test_data_cfs_us_svm, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_cfs_us_svm$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_cfs_us_svm$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



#################################################################################

################## Undersampled Boruta on 6 classifiers ###############

########### Using KNN #######################

#Reading Dataset
dataset_boruta_us_knn <- dataset_boruta
test_data_boruta_us_knn <- ts


set.seed(123)

# Factoring the class variable in the dataset
dataset_boruta_us_knn$Class <- factor(dataset_boruta_us_knn$Class, levels = c("0", "1"), 
                               labels = c("Class0", "Class1"))

# Setting up trainControl with repeated cross-validation
train_control <- trainControl(method = "repeatedcv", number = 100, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a KNN model with data preprocessing and tuning
knn_model_tuning <- train(Class~., data = dataset_boruta_us_knn, method = "knn",
                          trControl = train_control, preProcess = c("center", "scale"),
                          tuneLength = 10)


knn_model_tuning

###test KNN
# Convert 'Class' variable in test data to factor
test_data_boruta_us_knn$Class <- factor(test_data_boruta_us_knn$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_knn <- predict(knn_model_tuning, newdata = test_data_boruta_us_knn)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_knn, reference =  test_data_boruta_us_knn$Class)

conf_matrix

#### Performance metrics for KNN

cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(knn_model_tuning, newdata =test_data_boruta_us_knn, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_boruta_us_knn$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_boruta_us_knn$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


######## Using Random Forest #######


#Reading Dataset
dataset_boruta_us_rf <- dataset_boruta
test_data_boruta_us_rf <- ts


set.seed(123)
# Factoring the class variable in the dataset
dataset_boruta_us_rf$Class <- factor(dataset_boruta_us_rf$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Verify the change
print(table(dataset_boruta_us_rf$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "cv",summaryFunction = twoClassSummary, 
                              classProbs = TRUE, savePredictions = TRUE)

# Training a rf model
rf_model_tuning <- train(x = dataset_boruta_us_rf[,-which(names(dataset_boruta_us_rf) == "Class")], y = dataset_boruta_us_rf$Class,
                         method = "rf",ntree = 5, importance = TRUE, 
                         metric = "ROC", trControl = train_control)

rf_model_tuning


# Ensure test_data$Class is correctly factored
test_data_boruta_us_rf$Class <- factor(test_data_boruta_us_rf$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))
table(test_data_boruta_us_rf$Class)

# Generate predictions
prediction_rf <- predict(rf_model_tuning, newdata = test_data_boruta_us_rf)

# Inspect the predictions to ensure they're valid and contain both classes
prediction_rf

# Recalculate the confusion matrix with corrected factor levels
conf_matrix <- confusionMatrix(data = prediction_rf, reference = test_data_boruta_us_rf$Class)

# Print the confusion matrix
conf_matrix


#### Performance metrics for Random Forest

cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)

### roc


prediction_rf_probs <- predict(rf_model_tuning, newdata =test_data_boruta_us_rf, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_boruta_us_rf$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_boruta_us_rf$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results




########### Using Logistical Regression ##########

#Reading Dataset
dataset_boruta_us_lr <- dataset_boruta
test_data_boruta_us_lr <- ts

set.seed(123)
# Ensure 'Class' is a factor in both datasets with the same levels
dataset_boruta_us_lr$Class <- factor(dataset_boruta_us_lr$Class)
test_data_boruta_us_lr$Class <- factor(test_data_boruta_us_lr$Class, levels = levels(dataset_boruta_us_lr$Class))

# Train the Logistic Regression model
log_reg_model <- train(Class ~ ., data = dataset_boruta_us_rf, method = "glm", family = "binomial")

# Predict using the trained Logistic Regression model on the test data
prediction_log_reg <- predict(log_reg_model, newdata = test_data_boruta_us_lr)

# Calculate confusion matrix
conf_matrix_log_reg <- confusionMatrix(data = prediction_log_reg, reference = test_data_boruta_us_lr$Class)

# Print confusion matrix
conf_matrix_log_reg

#### Performance metrics for Logistical Regression
cm <- conf_matrix_log_reg$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(log_reg_model, newdata =test_data_boruta_us_lr, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_boruta_us_lr$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_boruta_us_lr$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


############### Using Rpart ###################

#Reading Dataset
dataset_boruta_us_rpart <- dataset_boruta
test_data_boruta_us_rpart <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_boruta_us_rpart$Class <- factor(dataset_boruta_us_rpart$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Setting up trainControl as before
train_control <- trainControl(method = "cv", number = 500, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)

# Attempting to train the model again with the updated Class variable
rpart_model <- train(Class ~ ., data = dataset_boruta_us_rpart, method = "rpart",
                     trControl = train_control, tuneLength = 10, metric = "ROC")

rpart_model


##testing the rpart
# Convert 'Class' variable in test data to factor
test_data_boruta_us_rpart$Class <- factor(test_data_boruta_us_rpart$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

# Generating predictions
prediction_rpart <- predict(rpart_model, newdata = test_data_boruta_us_rpart , type = "raw")


# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_rpart, reference = test_data_boruta_us_rpart$Class)

conf_matrix

#### Performance metrics for Rpart
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rpart_model, newdata =test_data_boruta_us_rpart, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_boruta_us_rpart$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_boruta_us_rpart$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


######################### Using J48 ################

#Reading Dataset
dataset_boruta_us_J48 <- dataset_boruta
test_data_boruta_us_J48 <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_boruta_us_J48$Class <- factor(dataset_boruta_us_J48$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))



###Using tuning

train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 5, summaryFunction = defaultSummary)

# Convert the 'Class' variable to factor

df_j48_model <- train(Class~., data = dataset_boruta_us_J48, method = "J48",
                      trControl = train_control)

df_j48_model


####testing the model
# Convert 'Class' variable in test data to factor
test_data_boruta_us_J48$Class <- factor(test_data_boruta_us_J48$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))


pred_using_J48 <- predict(df_j48_model, newdata = test_data_boruta_us_J48, type = "raw")

### performance measures

# Calculate confusion matrix
performance_measures_J48 <- confusionMatrix(data = pred_using_J48, reference = test_data_boruta_us_J48$Class)

performance_measures_J48


#### Performance metrics for J48
cm <- performance_measures_J48$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)

### roc


prediction_rf_probs <- predict(performance_measures_J48, newdata =test_data_boruta_us_J48, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_boruta_us_J48$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_boruta_us_J48$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


##################### Using SVM #############


#Reading Dataset
dataset_boruta_us_SVM <- dataset_boruta
test_data_boruta_us_SVM <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_boruta_us_SVM $Class <- factor(dataset_boruta_us_SVM $Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Verify the change
print(table(dataset_boruta_us_SVM $Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "repeatedcv", number = 20, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a neural network model
SVM_model_tuning <- train(Class ~ ., data = dataset_boruta_us_SVM , method = "svmRadial",
                          trControl = train_control)

SVM_model_tuning

# Test SVM
test_data_boruta_us_SVM$Class <- factor(test_data_boruta_us_SVM$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_svm <- predict(SVM_model_tuning, newdata = test_data_boruta_us_SVM)

prediction_svm
# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_svm, reference = test_data_boruta_us_SVM$Class)

# Print the confusion matrix
conf_matrix

#### Performance metrics for SVM
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(SVM_model_tuning, newdata =test_data_boruta_us_SVM, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_boruta_us_SVM$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_boruta_us_SVM$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


#############################################################################

################## Oversample Info Gain on 6 classifiers ###############

########### Using KNN #######################

dataset_info_gain_os_knn <- final_dataset_info_gain_os
test_data_os_knn <- ts

table(dataset_info_gain_os_knn$Class)
set.seed(123)
# Factoring the class variable in the dataset
dataset_info_gain_os_knn$Class <- factor(dataset_info_gain_os_knn$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Setting up trainControl with repeated cross-validation
train_control <- trainControl(method = "repeatedcv", number = 20, repeats = 5, summaryFunction = defaultSummary)

# Training a KNN model with data preprocessing and tuning
knn_model_tuning <- train(Class~., data = dataset_info_gain_os_knn, method = "knn",
                          trControl = train_control, preProcess = c("center", "scale"),
                          tuneLength = 10)


knn_model_tuning

###test KNN
# Convert 'Class' variable in test data to factor
test_data_os_knn$Class <- factor(test_data_os_knn$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_knn <- predict(knn_model_tuning, newdata = test_data_os_knn)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_knn, reference = test_data_os_knn$Class)

conf_matrix

#### Performance metrics for KNN
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(knn_model_tuning, newdata =test_data_os_knn, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_knn$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_knn$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



##############USING RPART###########

dataset_info_gain_os_rpart <- final_dataset_info_gain_os
test_data_os_rpart <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_info_gain_os_rpart$Class <- factor(dataset_info_gain_os_rpart$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Setting up trainControl as before
train_control <- trainControl(method = "cv", number = 500, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)

# Attempting to train the model again with the updated Class variable
rpart_model <- train(Class ~ ., data = dataset_info_gain_os_rpart, method = "rpart",
                     trControl = train_control, tuneLength = 10, metric = "ROC")

rpart_model


##testing the rpart
# Convert 'Class' variable in test data to factor
test_data_os_rpart$Class <- factor(test_data_os_rpart$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

# Generating predictions
prediction_rpart <- predict(rpart_model, newdata = test_data_os_rpart, type = "raw")


# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_rpart, reference = test_data_os_rpart$Class)

conf_matrix

#### Performance metrics for rpart
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rpart_model, newdata =test_data_os_rpart, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_rpart$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_rpart$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



###########USING RANDOM FOREST #########


# Reading the datasets
dataset_info_gain_os_rf <- final_dataset_info_gain_os
test_data_os_rf <- ts


set.seed(123)
# Factoring the class variable in the dataset
dataset_info_gain_os_rf$Class <- factor(dataset_info_gain_os_rf$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Verify the change
print(table(dataset_info_gain_os_rf$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "cv",summaryFunction = twoClassSummary, 
                              classProbs = TRUE, savePredictions = TRUE)

# Training a rf model
rf_model_tuning <- train(x = dataset_info_gain_os_rf[,-which(names(dataset_info_gain_os_rf) == "Class")], y = dataset_info_gain_os_rf$Class, method = "rf",
                         ntree = 500, importance = TRUE, metric = "ROC",
                         trControl = train_control)

rf_model_tuning


# Ensure test_data_os_rf$Class is correctly factored
test_data_os_rf$Class <- factor(test_data_os_rf$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))
table(test_data_os_rf$Class)

# Generate predictions
prediction_rf <- predict(rf_model_tuning, newdata = test_data_os_rf)


# Inspect the predictions to ensure they're valid and contain both classes
prediction_rf

# Recalculate the confusion matrix with corrected factor levels
conf_matrix <- confusionMatrix(data = prediction_rf, reference = test_data$Class)

# Print the confusion matrix
conf_matrix

#### Performance metrics for random Forest
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rf_model_tuning, newdata =test_data_os_rf, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_rf$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_rf$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results





################### Using logistical regression#########
# Reading the datasets
dataset_info_gain_os_lr <- final_dataset_info_gain_os
test_data_os_lr <- ts

set.seed(123)
# Ensure 'Class' is a factor in both datasets with the same levels
dataset_info_gain_os_lr$Class <- factor(dataset_info_gain_os_lr$Class)
test_data_os_lr$Class <- factor(test_data_os_lr$Class, levels = levels(dataset_info_gain_os_lr$Class))

# Train the Logistic Regression model
log_reg_model <- train(Class ~ ., data = dataset_info_gain_os_lr, method = "glm", family = "binomial")

# Predict using the trained Logistic Regression model on the test data
prediction_log_reg <- predict(log_reg_model, newdata = test_data_os_lr)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_log_reg, reference = test_data_os_lr$Class)

# Print confusion matrix
conf_matrix

#### Performance metrics for logistical regression
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(log_reg_model, newdata =test_data_os_lr, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_lr$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_lr$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results




############### Using J48 ###############

# Reading the datasets
dataset_info_gain_os_J48 <- final_dataset_info_gain_os
test_data_os_J48 <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_info_gain_os_J48$Class <- factor(dataset_info_gain_os_J48$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

###Using tuning

train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 5, summaryFunction = defaultSummary)


df_j48_model <- train(Class~., data = dataset_info_gain_os_J48, method = "J48", trControl = train_control)

df_j48_model


####testing the model
# Convert 'Class' variable in test data to factor
test_data_os_J48$Class <- factor(test_data_os_J48$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))


pred_using_J48 <- predict(df_j48_model, newdata = test_data_os_J48, type = "raw")

### performance measures

# Calculate confusion matrix
performance_measures_J48 <- confusionMatrix(data = pred_using_J48, reference = test_data_os_J48$Class)

performance_measures_J48

cm <- performance_measures_J48$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- performance_measures_J48$byClass['Balanced Accuracy']
MCC_1 <- performance_measures_J48$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- performance_measures_J48$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)



### roc


prediction_rf_probs <- predict(df_j48_model, newdata =test_data_os_J48, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_J48$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_J48$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results







################ Using SVM ###########

# Reading the datasets
dataset_info_gain_os_svm <- final_dataset_info_gain_os
test_data_os_svm <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_info_gain_os_svm$Class <- factor(dataset_info_gain_os_svm$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Verify the change
print(table(dataset_info_gain_os_svm$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "repeatedcv", number = 20, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a neural network model
SVM_model_tuning <- train(Class ~ ., data = dataset_info_gain_os_svm, method = "svmRadial",
                          trControl = train_control)

SVM_model_tuning

# Test SVM
test_data_os_svm$Class <- factor(test_data_os_svm$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_svm <- predict(SVM_model_tuning, newdata = test_data_os_svm)

prediction_svm
# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_svm, reference = test_data_os_svm$Class, positive = "Class1")

conf_matrix


### performance measure for SVM
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- performance_measures_J48$byClass['Balanced Accuracy']
MCC_1 <- performance_measures_J48$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- performance_measures_J48$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(df_j48_model, newdata =test_data_os_svm, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_svm$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_svm$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results


################## Oversample CFS on 6 classifiers ###############

########### Using KNN #######################

# Reading the datasets
dataset_cfs_os_knn <- dataset_cfs_os
test_data_os_knn <- ts

View(dataset_cfs_os_knn)
set.seed(123)

# Factoring the class variable in the dataset
dataset_cfs_os_knn$Class <- factor(dataset_cfs_os_knn$Class, levels = c("0", "1"), 
                               labels = c("Class0", "Class1"))

# Setting up trainControl with repeated cross-validation
train_control <- trainControl(method = "repeatedcv", number = 60, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a KNN model with data preprocessing and tuning
knn_model_tuning <- train(Class~., data = dataset_cfs_os_knn, method = "knn",
                          trControl = train_control, preProcess = c("center", "scale"),
                          tuneLength = 10)


knn_model_tuning


###test KNN
# Convert 'Class' variable in test data to factor
test_data_os_knn$Class <- factor(test_data_os_knn$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_knn <- predict(knn_model_tuning, newdata = test_data_os_knn)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_knn, reference = test_data_os_knn$Class)

conf_matrix


cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(knn_model_tuning, newdata =test_data_os_knn, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_knn$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_knn$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results




########### Using Rpart ##################

# Reading the datasets
dataset_cfs_os_rpart <- dataset_cfs_os
test_data_os_rpart <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_cfs_os_rpart$Class <- factor(dataset_cfs_os_rpart$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Setting up trainControl as before
train_control <- trainControl(method = "cv", number = 500, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)

# Attempting to train the model again with the updated Class variable
rpart_model <- train(Class ~ ., data = dataset_cfs_os_rpart, method = "rpart",
                     trControl = train_control, tuneLength = 10, metric = "ROC")

rpart_model


##testing the rpart
# Convert 'Class' variable in test data to factor
test_data_os_rpart$Class <- factor(test_data_os_rpart$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

# Generating predictions
prediction_rpart <- predict(rpart_model, newdata = test_data_os_rpart, type = "raw")


# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_rpart, reference = test_data_os_rpart$Class)

conf_matrix

cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rpart_model, newdata =test_data_os_rpart, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_rpart$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_rpart$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results





############## Using Random Forest ####################

# Reading the datasets
dataset_cfs_os_rf <- dataset_cfs_os
test_data_os_rf <- ts

set.seed(123)
View(dataset_cfs_os_rf)
# Factoring the class variable in the dataset
dataset_cfs_os_rf$Class <- factor(dataset_cfs_os_rf$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Verify the change
print(table(dataset_cfs_os_rf$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "cv",summaryFunction = twoClassSummary, 
                              classProbs = TRUE, savePredictions = TRUE)

# Training a rf model
rf_model_tuning <- train(x = dataset_cfs_os_rf[,-which(names(dataset_cfs_os_rf) == "Class")], y = dataset_cfs_os_rf$Class, method = "rf",
                         ntree = 50, importance = TRUE, metric = "ROC",
                         trControl = train_control)

rf_model_tuning


# Ensure test_data$Class is correctly factored
test_data_os_rf$Class <- factor(test_data_os_rf$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))
table(test_data_os_rf$Class)

# Generate predictions
prediction_rf <- predict(rf_model_tuning, newdata = test_data_os_rf)


# Inspect the predictions to ensure they're valid and contain both classes
prediction_rf

# Recalculate the confusion matrix with corrected factor levels
conf_matrix <- confusionMatrix(data = prediction_rf, reference = test_data_os_rf$Class)

# Print the confusion matrix
conf_matrix


cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rf_model_tuning, newdata =test_data_os_rf, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_rf$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_rf$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results





############# Using Logistical Regression##############

# Reading the datasets
dataset_cfs_os_lr <- dataset_cfs_os
test_data_os_lr <- ts

set.seed(123)
# Ensure 'Class' is a factor in both datasets with the same levels
dataset_cfs_os_lr$Class <- factor(dataset_cfs_os_lr$Class)
test_data_os_lr$Class <- factor(test_data_os_lr$Class, levels = levels(dataset_cfs_os_lr$Class))

# Train the Logistic Regression model
log_reg_model <- train(Class ~ ., data = dataset_cfs_os_lr, method = "glm", family = "binomial")

# Predict using the trained Logistic Regression model on the test data
prediction_log_reg <- predict(log_reg_model, newdata = test_data_os_lr)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_log_reg, reference = test_data_os_lr$Class)

# Print confusion matrix
conf_matrix


cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(log_reg_model, newdata =test_data_os_lr, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_lr$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_lr$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results




#################### USing J48 ##########

# Reading the datasets
dataset_cfs_os_J48 <- dataset_cfs_os
test_data_os_J48 <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_cfs_os_J48$Class <- factor(dataset_cfs_os_J48$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))



###Using tuning

train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 5, summaryFunction = defaultSummary)


df_j48_model <- train(Class~., data = dataset_cfs_os_J48, method = "J48",
                      trControl = train_control)

df_j48_model


####testing the model
# Convert 'Class' variable in test data to factor
test_data_os_J48$Class <- factor(test_data_os_J48$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))


pred_using_J48 <- predict(df_j48_model, newdata = test_data_os_J48, type = "raw")

### performance measures

# Calculate confusion matrix
performance_measures_J48 <- confusionMatrix(data = pred_using_J48, reference = test_data_os_J48$Class)

performance_measures_J48

cm <- performance_measures_J48$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- performance_measures_J48$byClass['Balanced Accuracy']
MCC_1 <- performance_measures_J48$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- performance_measures_J48$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(df_j48_model, newdata =test_data_os_J48, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_J48$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_J48$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results




################# Using SVM ##################

# Reading the datasets
dataset_cfs_os_svm <- dataset_cfs_os
test_data_os_svm <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_cfs_os_svm$Class <- factor(dataset_cfs_os_svm$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Verify the change
print(table(dataset_cfs_os_svm$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "repeatedcv", number = 20, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a neural network model
SVM_model_tuning <- train(Class ~ ., data = dataset_cfs_os_svm, method = "svmRadial",
                          trControl = train_control)

SVM_model_tuning

# Test SVM
test_data_os_svm$Class <- factor(test_data_os_svm$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_svm <- predict(SVM_model_tuning, newdata = test_data_os_svm)

prediction_svm
# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_svm, reference = test_data_os_svm$Class)

cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(SVM_model_tuning, newdata =test_data_os_svm, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_svm$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_svm$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



#############################################################################

################## Oversample Boruta on 6 classifiers ###############

########### Using KNN #######################


# Reading the datasets
dataset_boruta_os_knn <- dataset_boruta_os
test_data_os_knn <- ts

set.seed(123)

# Factoring the class variable in the dataset
dataset_boruta_os_knn$Class <- factor(dataset_boruta_os_knn$Class, levels = c("0", "1"), 
                               labels = c("Class0", "Class1"))

# Setting up trainControl with repeated cross-validation
train_control <- trainControl(method = "repeatedcv", number = 60, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a KNN model with data preprocessing and tuning
knn_model_tuning <- train(Class~., data = dataset_boruta_os_knn, method = "knn",
                          trControl = train_control, preProcess = c("center", "scale"),
                          tuneLength = 10)


knn_model_tuning


###test KNN
# Convert 'Class' variable in test data to factor
test_data_os_knn$Class <- factor(test_data_os_knn$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_knn <- predict(knn_model_tuning, newdata = test_data_os_knn)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_knn, reference = test_data_os_knn$Class)

conf_matrix

cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(knn_model_tuning, newdata = test_data_os_knn, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_knn$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_knn$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results




################## Using Rpart ##########################

# Reading the datasets
dataset_boruta_os_rpart <- dataset_boruta_os
test_data_os_rpart <- ts

set.seed(123)


# Factoring the class variable in the dataset
dataset_boruta_os_rpart$Class <- factor(dataset_boruta_os_rpart$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Setting up trainControl as before
train_control <- trainControl(method = "cv", number = 500, summaryFunction = twoClassSummary, classProbs = TRUE, verboseIter = TRUE)

# Attempting to train the model again with the updated Class variable
rpart_model <- train(Class ~ ., data = dataset_boruta_os_rpart, method = "rpart",
                     trControl = train_control, tuneLength = 10, metric = "ROC")

rpart_model


##testing the rpart
# Convert 'Class' variable in test data to factor
test_data_os_rpart$Class <- factor(test_data_os_rpart$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

# Generating predictions
prediction_rpart <- predict(rpart_model, newdata = test_data_os_rpart, type = "raw")


# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_rpart, reference = test_data_os_rpart$Class)

conf_matrix
cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)


### roc


prediction_rf_probs <- predict(rpart_model, newdata = test_data_os_rpart, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_rpart$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_rpart$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
weights <- c(0.5, 0.5)  
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results



###################### Using Random Forest ##################
# Reading the datasets
dataset_boruta_os_rf <- dataset_boruta_os
test_data_os_rf <- ts


set.seed(123)
View(dataset_boruta_os_rf )
# Factoring the class variable in the dataset
dataset_boruta_os_rf$Class <- factor(dataset_boruta_os_rf$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))


# Verify the change
print(table(dataset_boruta_os_rf$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "cv",summaryFunction = twoClassSummary, 
                              classProbs = TRUE, savePredictions = TRUE)

# Training a rf model
rf_model_tuning <- train(x = dataset_boruta_os_rf[,-which(names(dataset_boruta_os_rf) == "Class")], y = dataset_boruta_os_rf$Class, method = "rf",
                         ntree = 500, importance = TRUE, metric = "ROC",
                         trControl = train_control)

rf_model_tuning


# Ensure test_data$Class is correctly factored
test_data_os_rf$Class <- factor(test_data_os_rf$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))
table(test_data_os_rf$Class)

# Generate predictions
prediction_rf <- predict(rf_model_tuning, newdata = test_data_os_rf)


# Inspect the predictions to ensure they're valid and contain both classes
prediction_rf

# Recalculate the confusion matrix with corrected factor levels
conf_matrix <- confusionMatrix(data = prediction_rf, reference = test_data_os_rf$Class)

# Print the confusion matrix
conf_matrix

cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)

### roc


prediction_rf_probs <- predict(rf_model_tuning, newdata = test_data_os_rf, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_rf$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_rf$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
# Assuming equal weights for simplicity; adjust weights based on your specific needs
weights <- c(0.5, 0.5)  # Adjust based on class distribution or other criteria if needed
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results





############# Using Logistical Regession ##################

# Reading the datasets
dataset_boruta_os_lr <- dataset_boruta_os
test_data_os_lr <- ts

set.seed(123)
# Ensure 'Class' is a factor in both datasets with the same levels
dataset_boruta_os_lr$Class <- factor(dataset_boruta_os_lr$Class)
test_data_os_lr$Class <- factor(test_data_os_lr$Class, levels = levels(dataset_boruta_os_lr$Class))

# Train the Logistic Regression model
log_reg_model <- train(Class ~ ., data = dataset_boruta_os_lr, method = "glm", family = "binomial")

# Predict using the trained Logistic Regression model on the test data
prediction_log_reg <- predict(log_reg_model, newdata = test_data_os_lr)

# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_log_reg, reference = test_data_os_lr$Class)

# Print confusion matrix
conf_matrix

cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)

### roc


prediction_rf_probs <- predict(rf_model_tuning, newdata = test_data_os_lr, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_lr$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_lr$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
# Assuming equal weights for simplicity; adjust weights based on your specific needs
weights <- c(0.5, 0.5)  # Adjust based on class distribution or other criteria if needed
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results





################# Using J48 ##########################

# Reading the datasets
dataset_boruta_os_J48 <- dataset_boruta_os
test_data_os_J48 <- ts

set.seed(123)
# Factoring the class variable in the dataset
dataset_boruta_os_J48$Class <- factor(dataset_boruta_os_J48$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))




### Building Decision tree on df_for_classifier

###Using tuning

train_control <- trainControl(method = "repeatedcv", number = 10,
                              repeats = 5, summaryFunction = defaultSummary)


df_j48_model <- train(Class~., data = dataset_boruta_os_J48, method = "J48",
                      trControl = train_control)

df_j48_model


####testing the model
# Convert 'Class' variable in test data to factor
test_data_os_J48$Class <- factor(test_data_os_J48$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))


pred_using_J48 <- predict(df_j48_model, newdata = test_data_os_J48, type = "raw")

### performance measures

# Calculate confusion matrix
performance_measures_J48 <- confusionMatrix(data = pred_using_J48, reference = test_data_os_J48$Class)

performance_measures_J48


cm <- performance_measures_J48$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)

### roc


prediction_rf_probs <- predict(rf_model_tuning, newdata = test_data_os_J48, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_J48$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_J48$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
# Assuming equal weights for simplicity; adjust weights based on your specific needs
weights <- c(0.5, 0.5)  # Adjust based on class distribution or other criteria if needed
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results







################## Using SVM ###################

# Reading the datasets
dataset_boruta_os_svm <- dataset_boruta_os
test_data_os_svm <- ts


set.seed(123)
# Factoring the class variable in the dataset
dataset_boruta_os_svm$Class <- factor(dataset_boruta_os_svm$Class, levels = c("0", "1"), labels = c("Class0", "Class1"))

# Verify the change
print(table(dataset_boruta_os_svm$Class))

# Setting up trainControl for cross-validation
train_control <- trainControl(method = "repeatedcv", number = 20, repeats = 5,
                              summaryFunction = defaultSummary)

# Training a neural network model
SVM_model_tuning <- train(Class ~ ., data = dataset_boruta_os_svm, method = "svmRadial",
                          trControl = train_control, prob.model = TRUE)

SVM_model_tuning

# Test SVM
test_data_os_svm$Class <- factor(test_data_os_svm$Class, levels =c("0", "1"), labels = c("Class0", "Class1"))

prediction_svm <- predict(SVM_model_tuning, newdata = test_data_os_svm)

prediction_svm
# Calculate confusion matrix
conf_matrix <- confusionMatrix(data = prediction_svm, reference = test_data_os_svm$Class)


cm <- conf_matrix$table

# Get the levels from the confusion matrix directly
levels_from_cm <- rownames(cm)

# Dynamically access TP, FN, FP, and TN based on the actual levels
TP_0 <- cm[levels_from_cm[1], levels_from_cm[1]]
FN_0 <- cm[levels_from_cm[2], levels_from_cm[1]]
FP_0 <- cm[levels_from_cm[1], levels_from_cm[2]]
TN_0 <- cm[levels_from_cm[2], levels_from_cm[2]]

TP_1 <- cm[levels_from_cm[2], levels_from_cm[2]]
FN_1 <- cm[levels_from_cm[1], levels_from_cm[2]]
FP_1 <- cm[levels_from_cm[2], levels_from_cm[1]]
TN_1 <- cm[levels_from_cm[1], levels_from_cm[1]]

# Function to calculate F1 score
calculate_F1 <- function(Precision, Recall) {
  (2 * Precision * Recall) / (Precision + Recall)
}

# Calculate metrics for Class 0
TPR_0 <- TP_0 / (TP_0 + FN_0)
FPR_0 <- FP_0 / (FP_0 + TN_0)
Precision_0 <- TP_0 / (TP_0 + FP_0)
Recall_0 <- TPR_0
F1_Score_0 <- calculate_F1(Precision_0, Recall_0)

# Calculate metrics for Class 1
TPR_1 <- TP_1 / (TP_1 + FN_1)
FPR_1 <- FP_1 / (FP_1 + TN_1)
Precision_1 <- TP_1 / (TP_1 + FP_1)
Recall_1 <- TPR_1
F1_Score_1 <- calculate_F1(Precision_1, Recall_1)

# Calculate MCC using confusionMatrix's built-in statistic
MCC_0 <- conf_matrix$byClass['Balanced Accuracy']
MCC_1 <- conf_matrix$byClass['Balanced Accuracy']

# Kappa for the overall model
Kappa <- conf_matrix$overall['Kappa']

# Weighted averages
Weighted_TPR <- (TPR_0 * (TP_0 + FN_0) + TPR_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)
Weighted_FPR <- (FPR_0 * (FP_0 + TN_0) + FPR_1 * (FP_1 + TN_1)) / (FP_0 + TN_0 + FP_1 + TN_1)
Weighted_Precision <- (Precision_0 * (TP_0 + FP_0) + Precision_1 * (TP_1 + FP_1)) / (TP_0 + FP_0 + TP_1 + FP_1)
Weighted_Recall <- Weighted_TPR # Since Recall = TPR
Weighted_F1_Score <- (F1_Score_0 * (TP_0 + FN_0) + F1_Score_1 * (TP_1 + FN_1)) / (TP_0 + FN_0 + TP_1 + FN_1)

### roc


prediction_rf_probs <- predict(SVM_model_tuning, newdata = test_data_os_svm, type="prob")

# Calculate ROC curve and AUC for Class 1
roc_obj_class1 <- roc(test_data_os_svm$Class, prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class1 <- auc(roc_obj_class1)
print(paste("AUC for Class 1:", auc_class1))

# Calculate ROC curve and AUC for Class 0
roc_obj_class0 <- roc(test_data_os_svm$Class, 1 - prediction_rf_probs[, "Class1"], levels = c("Class0", "Class1"))
auc_class0 <- auc(roc_obj_class0)
print(paste("AUC for Class 0:", auc_class0))

# Calculate the weighted average of AUC
# Assuming equal weights for simplicity; adjust weights based on your specific needs
weights <- c(0.5, 0.5)  # Adjust based on class distribution or other criteria if needed
weighted_auc <- sum(weights * c(auc_class0, auc_class1))
print(paste("Weighted average ROC:", weighted_auc))

# Combine everything into a matrix or data frame
results <- rbind(
  c(TPR_0, FPR_0, Precision_0, Recall_0, F1_Score_0, weighted_auc, MCC_0, Kappa),
  c(TPR_1, FPR_1, Precision_1, Recall_1, F1_Score_1, weighted_auc, MCC_1, Kappa),
  c(Weighted_TPR, Weighted_FPR, Weighted_Precision, Weighted_Recall, Weighted_F1_Score, weighted_auc, (MCC_0 + MCC_1) / 2, Kappa)
)
colnames(results) <- c("TPR", "FPR", "Precision", "Recall", "F1-Measure", "ROC", "MCC", "Kappa")
rownames(results) <- c("Class 0", "Class 1", "Weighted Average")

results
