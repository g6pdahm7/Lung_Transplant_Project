#' Final Project
#' Prepared by Mausam, Ahmed, and Vian.

#' Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
library(car)
library(funModeling)
library(corrplot)
library(mice)
library(glmnet)

#' Columns that were not highlighted were removed a priori
#' on Excel. Since we are using Git, the same file should 
#' be available for all users. Should someone prefer to
#' remove the columns through code, you can find the 
#' raw file in the repo, and use the commented code.

#' Uploading the data
data <- read_excel("transfusion_data.xlsx")

#' Preliminary view of the data
#View(data)
status(data)

#' Remove unnecessary columns
#' columns_to_remove <- c(
#  "TX DB ID", "OR Date", "Coronary Artery Disease", "Hypertension",
#  "Diabetes (insulin)", "Diabetes (diet/OHGs)", "GERD/PUD", "Renal Failure",
#  "Stroke/CVA", "Liver Disease", "Thyroid Disease", "DCD vs DBD",
#  "Protamine (Y=1 N=0)", "Intra_Albumin 5% (mL)", "Intra_Crystalloid (mL)",
#  "Intra_Cell Saver returned (mL)", "Intra_PCC/Octaplex", "Blood Loss",
#  "Urine Output", "Fluid Balance", "Tranexamic Acid Used",
#  "ICU Admission Date/Time", "ICU Discharge Date/Time", "Date of Extubation",
#  "Duration of Ventilation", "PostImmediate_PTT", "PostImmediate_Fibrinogen",
#  "PostImmediate_Creatinine", "PostDay1_Hb", "PostDay1_Hct",
#  "PostDay1_Platelets", "PostDay1_PT", "PostDay1_INR", "PostDay1_PTT",
#  "PostDay1_Fibrinogen", "PostDay1_Creatinine",
#  "Need for reoperation for bleeding within 24h"
#)

#' Remove the columns
#data <- data %>% select(-all_of(columns_to_remove))

#' For neatness, we will rename the columns to remove any
#' spaces or brackets that may exist. 
colnames(data) <- gsub("[[:space:]]|[()]", "_", colnames(data))

#' Next we will do some basic cleaning, without any imputation.

#' First we will start by reformatting the gender column. 
#' Column will be renamed, and appropriate gender will be inputted.
colnames(data)[colnames(data) == "Gender__male_"] <- "Gender"
data$Gender <- ifelse(data$Gender == "TRUE", "Male", "Female")

#' Next, we will look at the proportions of NA for each column.
# LAS_score                         0.062500000     #IMPUTE (numeric)
# Pre_PTT                           0.005208333     #IMPUTE (numeric)
# Pre_Fibrinogen                    0.973958333     #REMOVE >30% missingness
# Duration_of_ICU_Stay__days_       0.005208333     #REMOVE (redundant)
# DEATH_DATE                        0.833333333     # Leave as NA
# RBC_0-24hrs                       0.687500000     # Replace NA with 0
# RBC_24-48hrs                      0.729166667     0     0
# RBC_48-72hrs                      0.750000000
# FFP_0-24hrs                       0.802083333     0     0
# FFP_24-48hrs                      0.828125000     0     0
# FFP_48-72hrs                      0.828125000
# Plt_0-24hrs                       0.802083333     0     0
# Plt_24-48hrs                      0.812500000     0     0
# Plt_48-72hrs                      0.822916667
# Cryo_0-24hrs                      0.807291667     0     0
# Cryo_24-48hrs                     0.828125000     0     0
# Cryo_48-72hrs                     0.828125000

#' Some initial thoughts: 
#' Pre_Fibrinogen will have to be excluded due to 
#' missingness greater than 30%.

#' Next, we will perform a simple EDA prior to 
#' handling the any missingness.

#' Before that, I want to create a logical column
#' that states whether or not someone had a transplant.
data <- data %>%
  mutate(Transfusion = ifelse(Total_24hr_RBC > 0, TRUE, FALSE))

#' Bar plot for the number of people with and without transfusions
ggplot(data, aes(x = as.factor(Transfusion))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Number of People Who Had Transfusions",
    x = "Transfusion (TRUE = Yes, FALSE = No)",
    y = "Count"
  ) +
  theme_minimal()

#' Bar plot for Type of transplant
ggplot(data, aes(x = Type)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Transplant Types", x = "Type", y = "Count") + 
  theme_minimal()

#' Histogram of `Age`
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Frequency") + 
  theme_minimal()

#' Relationship between BMI and ICU Length of Stay
ggplot(data, aes(x = BMI, y = ICU_LOS)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "BMI vs ICU Length of Stay", x = "BMI", y = "ICU Stay Duration (days)") + 
  theme_minimal()

#' Stacked bar chart for Gender by Type
ggplot(data, aes(x = Gender, fill = Type)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("steelblue", "#B0C4DE", "#D3D3D3")) +
  labs(title = "Proportion of Transplant Types by Gender", x = "Gender", y = "Proportion") +
  theme_minimal()

#' Boxplot of total RBC transfusion based on 30-day survival
ggplot(data, aes(x = ALIVE_30DAYS_YN, y = Total_24hr_RBC)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(title = "Total RBC Transfusion by 30-Day Survival", x = "Survived 30 Days (Y/N)", y = "Total RBC Units") + 
  theme_minimal()

#' Next, I want to create a simple correlation plot of some of 
#' the patient characteristics, and some of the general outcomes:

#' Create a new data set to view correction
correlation_data <- data

#' We will start by making sure everything is numeric
correlation_data$Gender <- as.numeric(data$Gender == "Male")  
correlation_data$Type <- as.numeric(as.factor(data$Type))   
correlation_data$Transfusion <- as.numeric(data$Transfusion) 
correlation_data$ALIVE_30DAYS_YN <- as.numeric(data$ALIVE_30DAYS_YN == "Y")  
correlation_data$ALIVE_90DAYS_YN <- as.numeric(data$ALIVE_90DAYS_YN == "Y")  
correlation_data$ALIVE_12MTHS_YN <- as.numeric(data$ALIVE_12MTHS_YN == "Y") 


#' Define groups of variables
group1 <- correlation_data %>%
  select(Age, Gender, Weight, Height, BMI, Type)

group2 <- correlation_data %>%
  select(Transfusion, ICU_LOS, HOSPITAL_LOS, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN)

#' Compute correlation matrix between group1 and group2
cor_matrix <- cor(group1, group2, use = "pairwise.complete.obs")

#' Plot the correlation heatmap
corrplot(cor_matrix, method = "color", is.corr = TRUE, 
         tl.cex = 0.8, number.cex = 0.7,
         title = "Correlation Plot Between Predictors and Outcomes",
         mar = c(0, 0, 1, 0))


#' We will now create histograms for the two columns we will impute.
#' This will help inform the imputation method we will use.

#' Histogram for LAS_score
ggplot(data, aes(x = LAS_score)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Distribution of LAS Score", x = "LAS Score", y = "Frequency") +
  theme_minimal()
#' Right skewed - slightly normal

#' Histogram for Pre_PTT
ggplot(data, aes(x = Pre_PTT)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Pre PTT", x = "Pre PTT", y = "Frequency") +
  theme_minimal()
#' Right skewed


###
### Tailor EDA to answer first part of first question
### Characteristics of patients that require transfusions.
###

#' Additional cleaning and Imputation

#' We will start by removing an unnecessary column that is 
#' redundant with another column for icu stay.
data <- data %>% select(-Duration_of_ICU_Stay__days_)

#' Next, we are going to remove this column "Pre_Fibrinogen" because
#' it has missingness greater than 30%.
data <- data %>% select(-Pre_Fibrinogen)

#' List of column names to replace NAs with 0
placeholder <- c(
  "RBC_0-24hrs", "RBC_24-48hrs", "RBC_48-72hrs", "RBC_72hr_Total",
  "FFP_0-24hrs", "FFP_24-48hrs", "FFP_48-72hrs", "FFP_72hr_Total",
  "Plt_0-24hrs", "Plt_24-48hrs", "Plt_48-72hrs", "Plt_72hr_Total",
  "Cryo_0-24hrs", "Cryo_24-48hrs", "Cryo_48-72hrs", "Cryo_72hr_Total"
)

# Replace NAs with 0 in the specified columns
data[placeholder] <- lapply(data[placeholder], function(x) {
  x[is.na(x)] <- 0
  return(x)
})

#' Some additional cleaning: 
#' Removing redundant columns:
data <- data %>% select(-First_Lung_Transplant)

#' Creating a new column with the type of life support
#' Combining 3 columns into one.
#' Set everything in the new column as none.
data$ECLS_Type <- "None"

#' Assign "ECMO" where ECLS_ECMO is TRUE and ECLS_CPB is FALSE
data$ECLS_Type[data$ECLS_ECMO == TRUE & data$ECLS_CPB == FALSE] <- "ECMO"

#' Assign "CPB" where ECLS_CPB is TRUE and ECLS_ECMO is FALSE
data$ECLS_Type[data$ECLS_CPB == TRUE & data$ECLS_ECMO == FALSE] <- "CPB"

#' Convert ECLS_Type to a factor with specified levels
data$ECLS_Type <- factor(data$ECLS_Type, levels = c("None", "ECMO", "CPB"))

#' Verify the distribution
table(data$ECLS_Type)

#' Now that we have solved the redundancy, we will
#' remove the unnecessary columns.
data <- data %>% select(-Intraoperative_ECLS)
data <- data %>% select(-ECLS_ECMO)
data <- data %>% select(-ECLS_CPB)


data <- data %>% mutate_if(is.character, as.factor)
colnames(data) <- gsub("[#-]", "_", colnames(data))

#' Imputation


#' Converting character variables to factors
data <- data %>% mutate_if(is.character, as.factor)


#' Generate default methods vector
methods <- make.method(data)

# Set all methods to "" (no imputation)
methods[] <- ""

# Set methods for variables to impute
methods["LAS_score"] <- "pmm"
methods["Pre_PTT"] <- "pmm"

# Predictors for LAS_score
predictors_LAS <- c("Age", "Gender", "BMI", "COPD", "alpha1_Antitrypsin_Deficiency", 
                    "Cystic_Fibrosis", "Idiopathic_Pulmonary_Hypertension",
                    "Interstitial_Lung_Disease", "Pulm_Other", "Type")

# Predictors for Pre_PTT
predictors_PTT <- c("Pre_Hb", "Pre_Hct", "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_Creatinine")

# Initialize predictor matrix with zeros
pred_matrix <- make.predictorMatrix(data)
pred_matrix[,] <- 0  # Set all entries to 0

# Set predictors for LAS_score
pred_matrix["LAS_score", predictors_LAS] <- 1

# Set predictors for Pre_PTT
pred_matrix["Pre_PTT", predictors_PTT] <- 1

# Perform single imputation
imputed111 <- mice(
  data,
  method = methods,
  predictorMatrix = pred_matrix,
  m = 1,
  maxit = 5,
  seed = 123
)

# Complete the data
data_imputed <- complete(imputed111)

# Update the original data
data$LAS_score <- data_imputed$LAS_score
data$Pre_PTT <- data_imputed$Pre_PTT



#' The following plots are used to visualize the imputed 
#' data. 
xyplot(imputed111, LAS_score ~ Gender)
xyplot(imputed111, Pre_PTT ~ Pre_Hct)

#Diagnostic tests - dsitrbution of complete and imputed data needs to be similar
stripplot(imputed111, pch = c(21, 20), cex = c(1, 1.5))

#' Analysis 
#' Question 1

library(pROC)
library(tree)
library(knitr)

#Need for transfusion 

#TRANSFUSION

#Assess and compares the performance of the methods (lasso classification vs. CART) using a fraction 
#of the original data that was not used for training/tuning.
#The best model (highest AUC score) will be used for further analysis.

#LASSO CLASSIFICATION 

#Set the seed
set.seed(111)

#' Next we are going to identify the predictors that 
#' we will be using in the Lasso classification model. 
x <- c(
  "Type", "Gender", "Height", "Weight", "Age", "BMI", "COPD",
  "alpha1_Antitrypsin_Deficiency", "Cystic_Fibrosis",
  "Idiopathic_Pulmonary_Hypertension", "Interstitial_Lung_Disease",
  "Pulm_Other", "Redo_Lung_Transplant", "ExVIVO_Lung_Perfusion",
  "Preoperative_ECLS", "LAS_score", "Pre_Hb", "Pre_Hct",
  "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_PTT", "Pre_Creatinine"
)

#' Subsetting the model data
model1data <- data[, c(x, "Transfusion")]

#Create an empty list to store the final predictors
lasso_classi_predictor <- list()

#Create an empty vector to store the final AUC output
lasso_classi_auc <- c()

# Define 27 custom hex colors for plotting
colours <- c(
  "#195d90", "#297022", "#b91c16", "#cc6600", "#52267d", "#8c4a20", "#7ca6c2",
  "#8eb072", "#c47272", "#cc9933", "#9b94ac", "#cccc66", "#6da395", "#cccc80",
  "#958094", "#cc665b", "#6686a4", "#cc8a4e", "#8aa24f", "#cc9ab5", "#a1a1a1",
  "#9c66a1", "#a2cca0", "#ccba59", "#52907e", "#cc7250", "#6e80a4"
)

#LASSO CLASSIFER

#For loop to look at the average AUC for the Lasso classifer
for (i in 1:5) {
  
  #The training and testing set will be a standard 80/20 training/testing split
  #80% of the data will be for training and 20% will be for testing
  #Create a vector of row indices that corresponds to the trianing set
  #The for loop will create 5 unique training sets
  training <- sample(nrow(model1data), round(nrow(model1data) * 0.8))
  
  #Create a dummy variable for categorical variables and keeping the continuous variables the same 
  x_classification <- model.matrix(Transfusion ~., model1data)[training, -1]
  
  #Create a vector with the response values
  y_classification <- model1data$Transfusion[training]

  #Train a model
  lasso_model_classification <- glmnet(x_classification, y_classification, family = "binomial")
  
  # Plot Lasso plot
  plot(
    lasso_model_classification, xvar = "lambda", label = TRUE, col = colours, 
    lwd = 1,
    main = paste("Log(lambda) vs. Coefficients for Lasso Iteration of", i),
    xlab = "log(Lambda)", ylab = "Coefficients"
  )
  
  # Create a legend
  legend(
    "topright", legend = rownames(modelxx1$beta),
    col = colours,  
    lty = 1, lwd = 1, cex = 0.6, ncol = 2, title = "Predictors"
  )
  
  #Perform cross-validation to select the lambda that maximizes AUC 
  cv_classification <- cv.glmnet(x_classification, y_classification, family = "binomial", type.measure = "auc")
  
  #Plot the curve
  plot(cv_classification)
  title(paste("Cross-Validation Plot for Lasso Classifier of", i))
  
  #Optimal lambda that maximizes the AUC
  optimal_lambda <- cv_classification$lambda.min
  
  #Train a model using the optimal lambda
  lasso_model_classification_final <- glmnet(x_classification, y_classification, family = "binomial", lambda = optimal_lambda)
  
  #Look at the value of the features that stay in the model when using the optimal lambda
  coef_min_classification <- coef(cv_classification, s = "lambda.min")
  
  #List the selected predictors that stayed in the model
  lasso_classi_predictor[[i]] <- rownames(coef_min_classification)[coef_min_classification[, 1] != 0][-1]
  
  #Test the model on the testing data set and get the predicted probability 
  lasso_class_predict <- as.numeric(predict(lasso_model_classification_final, newx = model.matrix(Transfusion ~., model1data)[-training,-1], 
                                            s = optimal_lambda, type = "response"))
  
  #Generate the ROC curve for the testing data set 
  roc_class <- roc(model1data$Transfusion[-training], lasso_class_predict)
  
  #Plot ROC curve 
  plot(roc_class)
  title(paste("AUC-ROC curve (Lasso) of", i))
  
  #List the resulting AUC value from each loop's model
  lasso_classi_auc[i] <- roc_class$auc
  
}

#Convert the predictors list to a data frame for better visualization (table)
lasso_class_predictors_table <- data.frame(Iteration = 1:5, Predictors = sapply(lasso_classi_predictor, toString))
kable(
  lasso_class_predictors_table, format = "html", digits = 2, 
  col.names = c("Iteration", "Predictors"),
  caption = "Lasso Classifiers - Selected Predictors for Each Iteration"
) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE, position = "center") 

#Convert the AUC values to a data frame (table)
lasso_classi_auc_table <- data.frame(Iteration = 1:5, AUC = lasso_classi_auc)

kable(
  lasso_classi_auc_table, format = "html", digits = 2, 
  col.names = c("Iteration", "AUC"),
  caption = "Lasso Classifiers - AUC for Each Iteration"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE, position = "center")

#Calculate the average AUC
lasso_classi_auc_average <- round(mean(lasso_classi_auc), digits = 3)


###CART (full and pruned)

model1data$Transfusion <- as.factor(model1data$Transfusion)

#Create an empty list to store the variables from the CART model - full tree
tree_classi_predictors <- list()

#Create an empty vector to store the AUC output - full tree 
tree_classi_auc <- c()

#Create an empty list to store the variables from the CART model - full tree
prune_classi_predictors <- list()

#Create an empty vector to store the AUC output - full tree 
prune_classi_auc <- c()

#For loop to look at the CART model for each iteration of the data
for (i in 1:5) {
  
  #The training and testing set will be a standard 80/20 training/testing split
  #80% of the data will be for training and 20% will be for testing
  #Create a vector of row indices that corresponds to the training set
  #The for loop will create 5 unique training sets
  training <- sample(nrow(model1data), round(nrow(model1data) * 0.8))
  
  #Training CART
  tree_model <- tree(Transfusion ~ ., data = model1data, subset = training)
  plot(tree_model)
  title(paste("Full Tree Model of", i))
  text(tree_model, pretty=0)
  
  #Perform cross-validation to prune the tree using deviance 
  cv_tree <- cv.tree(tree_model, FUN = prune.tree, method = "deviance")
  
  #Find the best size of the tree for pruning associated with the lowest deviance 
  best_size <- cv_tree$size[which.min(cv_tree$dev)]
  
  #Prune the tree to the best size
  #Best size is set to 2 to avoid a "stump"
  pruned_tree <- prune.tree(tree = tree_model, best = 2)
  
  #Plot the tree
  plot(pruned_tree)
  title(paste("Pruned Tree Model of", i))
  text(pruned_tree, pretty=0)
  
  #Extract the variables that were used in the final model
  variable_prune <- unique(pruned_tree$frame$var[pruned_tree$frame$var != "<leaf>"])
  prune_classi_predictors[[i]] <- variable_prune
  
  #Predict using the pruned tree model
  tree_prune_predict <- predict(pruned_tree, newdata = model1data[-training,], type = "vector")
  
  #Extract the predicted probability 
  pred_probs_prune_tree <- as.numeric(tree_prune_predict[,2])
  
  #Generate the ROC curve for testing 
  roc_prune <-  roc(model1data$Transfusion[-training] ~ pred_probs_prune_tree)
  plot(roc_prune)
  title(paste("AUC-ROC Curve (Prune) of", i))
  prune_classi_auc[i] <- roc_prune$auc
  
  #Let's do the same evaluation fro the original tree without pruning 
  #Extract the variables that were used in the tree model without pruning
  variable_tree <- unique(tree_model$frame$var[tree_model$frame$var != "<leaf>"])
  tree_classi_predictors[[i]] <- variable_tree
  
  #Predict using the full tree model
  tree_predict <- predict(tree_model, newdata = model1data[-training,], type = "vector")
  
  #Extract the predicted probability 
  pred_probs_tree <- as.numeric(tree_predict[,2])
  
  #Generate the ROC curve for testing 
  roc_tree <-  roc(model1data$Transfusion[-training] ~ pred_probs_tree)
  plot(roc_tree)
  title(paste("AUC-ROC Curve (Full) of", i))
  tree_classi_auc[i] <- roc_tree$auc
  
}

#Convert the predictors list to a data frame for better visualization (table) - full tree
tree_classi_predictors_table <- data.frame(Iteration = 1:5, Variables = sapply(tree_classi_predictors, toString))

kable(
  tree_classi_predictors_table, format = "html", digits = 2, 
  col.names = c("Iteration", "Predictors"),
  caption = "CART Tree - Selected Predictors for Each Iteration"
) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE, position = "center") 

#Convert the AUC values to a data frame (table) - full tree
tree_auc_table <- data.frame(Iteration = 1:5, AUC = tree_classi_auc)

kable(
  tree_auc_table, format = "html", digits = 2, 
  col.names = c("Iteration", "AUC"),
  caption = "CART Tree - AUC for Each Iteration"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE, position = "center")


#Calculate the average AUC - full tree 
tree_auc_average <- round(mean(tree_classi_auc), digits = 3)

#Convert the predictors list to a data frame for better visualization (table) - prune tree
prune_predictors_table <- data.frame(Iteration = 1:5, Variables = sapply(prune_classi_predictors, toString))

kable(
  prune_predictors_table, format = "html", digits = 2, 
  col.names = c("Iteration", "Predictors"),
  caption = "CART Pruned Tree - Selected Predictors for Each Iteration"
) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE, position = "center")

#Convert the AUC values to a data frame (table) - prune tree
prune_auc_table <- data.frame(Iteration = 1:5, AUC = prune_classi_auc)

kable(
  prune_auc_table, format = "html", digits = 2, 
  col.names = c("Iteration", "AUC"),
  caption = "CART Pruned Tree - AUC for Each Iteration"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE, position = "center")

#Calculate the average AUC - prune tree 
prune_auc_average <- round(mean(prune_classi_auc), digits = 3)

#Combine AUC values into a single data frame
auc_comparison_table <- data.frame(
  Model = c("LASSO Classification", "CART Tree", "CART Pruned Tree"),
  Average_AUC = c(
    lasso_classi_auc_average, 
    round(mean(tree_classi_auc), digits = 3), 
    prune_auc_average
  )
)

kable(
  auc_comparison_table, format = "html", digits = 3, 
  col.names = c("Model", "Average AUC"),
  caption = "Comparison of Average AUC Across Models"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE,
    position = "center"
  )

#The lasso model had a higher average AUC - proceeding with lasso classification
#for further analysis

################# LASSO CLASSIFICATION

#Let's fit a lasso classification model with all of the data.

#Using the same predictors and subset data as above, we will
#make the matrix for the predictors, with dummy variables.
x <- model.matrix(Transfusion ~ ., data = model1data)

y <- as.numeric(model1data$Transfusion) - 1

#' Train the model
modelxx1 <- glmnet(x, y, family = "binomial")

# Plot the Lasso paths 
plot(
  modelxx1, xvar = "lambda", label = TRUE, col = colours, 
  lwd = 1,
  main = "Lasso Paths",
  xlab = "log(Lambda)", ylab = "Coefficients"
)

# Create a legend
legend(
  "topright", legend = rownames(modelxx1$beta),
  col = colours,  
  lty = 1, lwd = 1, cex = 0.6, ncol = 2, title = "Predictors"
)

#' Setting seed for reproducibility and doing cross-validation.
set.seed(123)
cv.lasso <- cv.glmnet(x, y, nfolds = 5, alpha = 1,family = "binomial", type.measure = "auc")

#' Plotting MSE vs log lambda
plot(cv.lasso)

#' Optimal lambda value that maximizes AUC 
optimal_lambda <- cv.lasso$lambda.min
# AUC corresponding to optimal lambda
optimal_auc <- cv.lasso$cvm[cv.lasso$lambda == optimal_lambda]

#' Coefficients at optimal lambda
optimal_coefs <- coef(cv.lasso, s = "lambda.min")

#Extract non-zero coefficients and create a data frame
optimal_coefs_table <- as.data.frame(as.matrix(optimal_coefs))
optimal_coefs_table <- data.frame(
  Predictor = rownames(optimal_coefs_table),
  Coefficient = optimal_coefs_table[, 1]
)
#Remove row names
rownames(optimal_coefs_table) <- NULL 

# Filter to display only non-zero coefficients
optimal_coefs_table <- optimal_coefs_table[optimal_coefs_table$Coefficient != 0, ]

kable(
  optimal_coefs_table, format = "html", digits = 4,
  col.names = c("Predictor", "Coefficient"),
  caption = "Optimal Coefficients from Lasso Model"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE,
    position = "center"
  )

################### Model 2: Continuous outcome.
#' Identical process to the one used above, just  
#' different family.

#TOTAL RBC
set.seed(123)

#LASSO REGRESSION

#' Identical process to the one used above, just 
#' different family.

# Define predictors and outcome with updated variable names
predictors22 <- c(
  "Type", "Gender", "Height", "Weight", "Age", "BMI", "COPD",
  "alpha1_Antitrypsin_Deficiency", "Cystic_Fibrosis",
  "Idiopathic_Pulmonary_Hypertension", "Interstitial_Lung_Disease",
  "Pulm_Other", "Redo_Lung_Transplant", "ExVIVO_Lung_Perfusion",
  "Preoperative_ECLS", "LAS_score", "Pre_Hb", "Pre_Hct",
  "Pre_Platelets", "Pre_PT", "Pre_INR", "Pre_PTT", "Pre_Creatinine"
)

# Subset data for the new model
model22data <- data[, c(predictors22, "Total_24hr_RBC")]

# Create design matrix for predictors and outcome with updated variable names
x22 <- model.matrix(Total_24hr_RBC ~ ., data = model22data)[, -1]
y22 <- model22data$Total_24hr_RBC

# Train Lasso model
lasso_model22 <- glmnet(x22, y22, family = "gaussian")

# Plot coefficients vs log(lambda)
plot(
  lasso_model22, xvar = "lambda", label = TRUE, col = colours, 
  lwd = 1,
  main = "Lasso Paths",
  xlab = "log(Lambda)", ylab = "Coefficients"
)

# Create a legend
legend(
  "bottomright", legend = rownames(lasso_model22$beta),
  col = colours,  
  lty = 1, lwd = 1, cex = 0.6, ncol = 2, title = "Predictors"
)

# Cross-validation to find optimal lambda
set.seed(123)
cv_lasso22 <- cv.glmnet(x22, y22, family = "gaussian")

# Plot cross-validation curve
plot(cv_lasso22)

# Extract optimal lambda
optimal_lambda22 <- cv_lasso22$lambda.min
print(paste("Optimal Lambda:", optimal_lambda22))

# Coefficients at optimal lambda
optimal_coefs22 <- coef(cv_lasso22, s = "lambda.min")

#Extract non-zero coefficients and create a data frame
optimal_coefs22_table <- as.data.frame(as.matrix(optimal_coefs22))
optimal_coefs22_table <- data.frame(
  Predictor = rownames(optimal_coefs22_table),
  Coefficient = optimal_coefs22_table[, 1]
)

# Remove row names
rownames(optimal_coefs22_table) <- NULL  

# Filter to display only non-zero coefficients
optimal_coefs22_table <- optimal_coefs22_table[optimal_coefs22_table$Coefficient != 0, ]

# Use kable to display the table
library(knitr)
library(kableExtra)

kable(
  optimal_coefs22_table, format = "html", digits = 4,
  col.names = c("Predictor", "Coefficient"),
  caption = "Optimal Coefficients from Second Lasso Model"
) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE,
    position = "center"
  )

###
### Be ready to answer the question: 
### Why Lasso, why not tree, why not logistic/linear regression?
###

######################### q2 baby

# Ensure date columns are in Date format
data$OR_Date <- as.Date(data$OR_Date)

data$DEATH_DATE <- as.Date(data$DEATH_DATE, format = "%d-%b-%Y")

###
### Is it safe to assume that the last day of follow up is the last recorded date of death?
### Some patients have date of death recorded after their 12 month follow up.
###

# Define a cutoff date for censoring (e.g., date of last follow-up)
# Here, we'll use the latest date available in the dataset or a specified date
last_follow_up_date <- as.Date("2020-01-22")  # This date here is the latest one available.

#' We will now calculate Survival_Time.
#' If the patient's date of death is set, then their survival
#' is the number of days from their surgery to death.
#' If the patient's death date was not recorded, then we assumed
#' that they were alive until the last follow up date (which is
#' the last recorded date of death). 
#' The status column: 1 = patient has died. 
#' 0 = Patient has not experienced the event, censored. 
data <- data %>%
  mutate(
    Survival_Time = ifelse(
      !is.na(DEATH_DATE),
      as.numeric(DEATH_DATE - OR_Date),
      as.numeric(last_follow_up_date - OR_Date)
    ),
    status = ifelse(!is.na(DEATH_DATE), 1, 0)
  )

# Ensure Survival_Time is non-negative
data$Survival_Time <- pmax(data$Survival_Time, 0)

# Check the data
head(data[, c("OR_Date", "DEATH_DATE", "Survival_Time", "status")])


########## KM for Massive_Transfusion!!!
# Convert Massive_Transfusion to factor if not already
data$Massive_Transfusion <- as.factor(data$Massive_Transfusion)

# Fit KM model
km_fit_mt <- survfit(Surv(Survival_Time, status) ~ Massive_Transfusion, data = data)

# Kaplan-Meier and Log-Rank Test for Massive Transfusion
plot(km_fit_mt, xlab = "Time (days)", ylab = "Survival Probability",
     col = 1:2, lwd = 2, main = "Kaplan-Meier Survival Curves by Massive Transfusion")
legend("bottom", legend = levels(data$Massive_Transfusion), col = 1:2, lwd = 2)


# Plot KM curves
ggsurvplot(
  km_fit_mt,
  data = data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  title = "Survival by Massive Transfusion",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  legend.title = "Massive Transfusion"
)

# Log-rank test for Massive Transfusion
log_rank_massive <- survdiff(Surv(Survival_Time, status) ~ Massive_Transfusion, data = data)
print(log_rank_massive)

########### KM for ECLS_TYPE
# Ensure ECLS_Type is a factor
data$ECLS_Type <- as.factor(data$ECLS_Type)

# Fit KM model
km_fit_ecls <- survfit(Surv(Survival_Time, status) ~ ECLS_Type, data = data)

# Kaplan-Meier and Log-Rank Test for ECLS Type
plot(km_fit_ecls, xlab = "Time (days)", ylab = "Survival Probability",
     col = 1:3, lwd = 2, main = "Kaplan-Meier Survival Curves by ECLS Type")
legend("bottom", legend = levels(data$ECLS_Type), col = 1:3, lwd = 2)


# Plot KM curves
ggsurvplot(
  km_fit_ecls,
  data = data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  title = "Survival by ECLS Type",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  legend.title = "ECLS Type"
)

# Log-rank test for ECLS Type
log_rank_ecls <- survdiff(Surv(Survival_Time, status) ~ ECLS_Type, data = data)
print(log_rank_ecls)

######### KM for Transfusion 
# Ensure Transfusion is a factor
data$Transfusion <- as.factor(data$Transfusion)

# Fit KM model
km_fit_transfusion <- survfit(Surv(Survival_Time, status) ~ Transfusion, data = data)

# Kaplan-Meier and Log-Rank Test for Transfusion
plot(km_fit_transfusion, xlab = "Time (days)", ylab = "Survival Probability",
     col = 1:2, lwd = 2, main = "Kaplan-Meier Survival Curves by Transfusion")
legend("bottom", legend = levels(data$Transfusion), col = 1:2, lwd = 2)

# Plot KM curves
ggsurvplot(
  km_fit_transfusion,
  data = data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  title = "Survival by Transfusion",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  legend.title = "Transfusion"
)

# Log-rank test for Transfusion
log_rank_transfusion <- survdiff(Surv(Survival_Time, status) ~ Transfusion, data = data)
print(log_rank_transfusion)



############ Cox PH Model
# Define the predictors to be used in the model
predictors_cox <- c(
   "ICU_LOS", "BMI", "Age", "Preoperative_ECLS", "LAS_score",
    "Massive_Transfusion", "COPD", "Total_24hr_RBC"
)

predictors_cox <- c(
  "Type", "COPD", "Total_24hr_RBC"
)

# Create the formula for the Cox model
coxf <- as.formula(paste("Surv(Survival_Time, status) ~", paste(predictors_cox, collapse = " + ")))

# Fit the Cox Proportional Hazards model
coxmodel <- coxph(coxf, data = data)

# Print the summary of the model
summary(coxmodel)

# Check proportional hazards assumptions
phtest <- cox.zph(coxmodel)
print(phtest)

