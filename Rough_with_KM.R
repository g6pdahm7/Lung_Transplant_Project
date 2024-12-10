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
library(pastecs)
library(tidyverse)
library(kableExtra)
library(gt)
library(pROC)
library(tree)
library(knitr)

#' Uploading the data
data <- read_excel("raw_data.xlsx")

#' Remove unnecessary columns
columns_to_remove <- c(
  "TX DB ID", "Coronary Artery Disease", "Hypertension",
  "Diabetes (insulin)", "Diabetes (diet/OHGs)", "GERD/PUD", "Renal Failure",
  "Stroke/CVA", "Liver Disease", "Thyroid Disease", "DCD vs DBD",
  "Protamine (Y=1 N=0)", "Intra_Albumin 5% (mL)", "Intra_Crystalloid (mL)",
  "Intra_Cell Saver returned (mL)", "Intra_PCC/Octaplex", "Blood Loss",
  "Urine Output", "Fluid Balance", "Tranexamic Acid Used",
  "ICU Admission Date/Time", "ICU Discharge Date/Time", "Date of Extubation",
  "Duration of Ventilation", "PostImmediate_PTT", "PostImmediate_Fibrinogen",
  "PostImmediate_Creatinine", "PostDay1_Hb", "PostDay1_Hct",
  "PostDay1_Platelets", "PostDay1_PT", "PostDay1_INR", "PostDay1_PTT",
  "PostDay1_Fibrinogen", "PostDay1_Creatinine",
  "Need for reoperation for bleeding within 24h"
)

#' Remove the columns
data <- data %>% select(-all_of(columns_to_remove))

#' Preliminary view of the data
#View(data)
status(data)

# Create a table with the variable categories and its respective variables
variable_categories <- data.frame(
  Variable_Category = c(
    "Patient Demographic Data", 
    "Underlying Respiratory Diagnosis and Intraoperative Descriptions", 
    "Life Support", 
    "Blood Product Transfusion Data", 
    "Survival and ICU Length of Stay"
  ),
  Extracted_Variables = c(
    "1. Gender\n2. Height\n3. Weight\n4. Age\n5. Body Mass Index (BMI)",
    "1. Chronic obstructive pulmonary disease (COPD)\n2. alpha1-Antitrypsin Deficiency\n3. Cystic Fibrosis\n4. Idiopathic Pulmonary Hypertension\n5. Interstitial Lung Disease\n6. Other Pulmonary Disease\n7. First Lung Transplant\n8. Redo Lung Transplant\n9. ExVIVO Lung Perfusion\n10. Preoperative Extracorporeal Life Support (ECLS)\n11. Lung Allocation Score (LAS score)\n12. Intraoperative ECLS",
    "1. ECLS Extracorporeal Membrane Oxygenation (ECMO)\n2. ECLS Cardiopulmonary Bypass (CPB)",
    "1. Intra Fresh Frozen Plasma\n2. Intra Packed Cells\n3. Intra Platelets\n4. Intra Cryoprecipitate\n5. Red Blood Cell (RBC) 0-24hrs\n6. RBC 24-48hrs\n7. RBC 48-72hrs\n8. RBC 72hr Total\n9. Fresh Frozen Plasma (FFP) 0-24hrs\n10. FFP 24-48hrs\n11. FFP 48-72hrs\n12. FFP 72hr Total\n13. Platelet (Plt) 0-24hrs\n14. Plt 24-48hrs\n15. Plt 48-72hrs\n16. Plt 72hr Total\n17. Cryoprecipitate (Cryo) 0-24hrs\n18. Cryo 24-48hrs\n19. Cryo 48-72hrs\n20. Cryo 72hr Total\n21. Total 24hr RBC\n22. Massive Transfusion",
    "1. Duration of ICU Stay (days)\n2. Death Date\n3. Alive by 30 days\n4. Alive by 90 days\n5. Alive by 12 months\n6. ICU Length of Stay (LOS)\n7. Hospital LOS"
  )
)

# Create table using gt package
variable_categories %>%
  gt() %>%
  tab_header(
    title = "Variable Categories and Extracted Variables"
  ) %>%
  cols_label(
    Variable_Category = "Variable Category",
    Extracted_Variables = "Extracted Variables"
  ) %>%
  tab_options(
    table.width = pct(100),  # Adjust table width as needed
    table.font.size = "small"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  )

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
#' 
####################
### Cleaning/EDA ###

#' Next, we will perform a simple EDA prior to 
#' handling the any missingness.

#' Before that, I want to create a logical column
#' that states whether or not someone had a transplant.
data <- data %>%
  mutate(Transfusion = ifelse(Total_24hr_RBC > 0, TRUE, FALSE))

#' Summary table of numeric variables
desc.data <- stat.desc(dplyr::select_if(data, is.numeric))

desc.data <- desc.data[-c(1,2,3,7,10,11),-c(1,39)] #remove rows and cols with irrelevant descriptive stats

# Render table
kable(t(desc.data), format = "html", digits = 2, 
      caption = "Summary of Numeric Variables in Lung Transplant Patients Dataset") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE, position = "center") 

#' Summary table of categorical data
#subset categorical variables
data$Massive_Transfusion <- as.factor(data$Massive_Transfusion)
character.data <- data %>% select_if(negate(is.numeric))
character.data <- character.data[,-c(1,17)] #remove OR date, death date

character.data <- character.data %>%
  mutate(across(everything(), as.character)) #ensure all cols are characters

# Function to summarize categorical variables
summarize_character_data_combined <- function(df) {
  # Summarize each column
  summary_list <- lapply(names(df), function(var) {
    df %>%
      count(!!sym(var), name = "Count") %>%
      mutate(
        Variable = var,
        Proportion = round((Count / sum(Count)) * 100, 2)
      ) %>%
      rename(Category = !!sym(var)) %>%
      select(Variable, Category, Count, Proportion)
  })
  
  # Combine all summaries into one dataframe
  summary_df <- bind_rows(summary_list)
  
  # Combine Variable and Category into a single column
  summary_df <- summary_df %>%
    mutate(Combined = paste(Variable, Category, sep = " - ")) %>%
    select(Combined, Count, Proportion)
  
  return(summary_df)
}

summary_table <- summarize_character_data_combined(character.data)

#table of Categorical data
kable(summary_table, format = "html", caption = "Summary of Categorical Variables", 
      col.names = c("Variable", "Count","Proportion (%)")) %>%
      kable_styling(bootstrap_options = "condensed") %>%
      row_spec(c(1:3, 6:7, 10:11, 14:15, 18:19, 22:23, 26:27, 32:37, 40:41), background = "#f2f2f2") 


#' Bar plot for the number of people with and without transfusions
ggplot(data, aes(x = as.factor(Transfusion))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(
    title = "Number of People Who Had Transfusions",
    x = "Transfusion (TRUE = Yes, FALSE = No)",
    y = "Count"
  ) +
  theme_minimal()

#' Bar plot for Type of transplant
ggplot(data, aes(x = Type)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Transplant Types", x = "Type", y = "Count") + 
  theme_minimal()

#' Histogram of `Age`
ggplot(data, aes(x = Total_24hr_RBC)) +
  geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Frequency") + 
  theme_minimal()

#' Relationship between BMI and ICU Length of Stay
ggplot(data, aes(x = BMI, y = ICU_LOS)) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm", color = "black") +
  labs(title = "BMI vs ICU Length of Stay", x = "BMI", y = "ICU Stay Duration (days)") + 
  theme_minimal()

#' Stacked bar chart for Gender by Type
ggplot(data, aes(x = Gender, fill = Type)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("lightblue", "#B0C4DE", "#D3D3D3")) +
  labs(title = "Proportion of Transplant Types by Gender", x = "Gender", y = "Proportion") +
  theme_minimal()

#' Boxplot of total RBC transfusion based on 30-day survival
ggplot(data, aes(x = ALIVE_30DAYS_YN, y = Total_24hr_RBC)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Total RBC Transfusion by 30-Day Survival", x = "Survived 30 Days (Y/N)", y = "Total RBC Units") + 
  theme_minimal()

#histogram length of hospital stay 
ggplot(data, aes(x = HOSPITAL_LOS)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Hospital Stay Length", x = "Hospital Stay (days)", y = "Frequency") + 
  theme_minimal()

#histogram of icu stay length
ggplot(data, aes(x = ICU_LOS)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of ICU Stay Length", x = "ICU Stay (days)", y = "Frequency") + 
  theme_minimal()

######################################################
### Exploring Transfusions/Massive Transfusion Pts ###

#subset data for transfusion and massive transfusion
data.transfusion <- data %>%
  filter(Transfusion == "TRUE") 
data.transfusion$Transfusion <- as.factor(data.transfusion$Transfusion)

data.massive <- data %>%
  filter(Massive_Transfusion == 1)
data.massive$Massive_Transfusion <- as.factor(data.massive$Massive_Transfusion)
data.massive$Transfusion <- as.factor(data.massive$Transfusion)

#' Summary table of numeric variables for pts with transfusions
desc.transfusion <- stat.desc(dplyr::select_if(data.transfusion, is.numeric))

desc.transfusion <- desc.transfusion[-c(1,2,3,7,10,11),-c(1)] #remove rows and cols with irrelevant descriptive stats

# Render table
kable(t(desc.transfusion), format = "html", digits = 2, 
      caption = "Summary of Numeric Data in Lung Transplant Patients who received Transfusion") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                full_width = FALSE, position = "center") 


#Summarize categorical transfusion data
char.transfusion <- data.transfusion[, !sapply(data.transfusion, is.numeric), drop = FALSE]
char.transfusion <- char.transfusion[,-c(1,17,22)] #remove OR date, death date, transfusion status
char.transfusion <- char.transfusion %>%
  mutate(across(everything(), as.character)) #ensure all cols are characters

#summarize columns
summarize_transfusion_data_combined <- function(df) {
  # Summarize each column
  summary_list <- lapply(names(df), function(var) {
    df %>%
      count(!!sym(var), name = "Count") %>%
      mutate(
        Variable = var,
        Proportion = round((Count / sum(Count)) * 100, 2)
      ) %>%
      rename(Category = !!sym(var)) %>%
      select(Variable, Category, Count, Proportion)
  })
  
  # Combine all summaries into one dataframe
  summary_df <- bind_rows(summary_list)
  
  # Combine Variable and Category into a single column
  summary_df <- summary_df %>%
    mutate(Combined = paste(Variable, Category, sep = " - ")) %>%
    select(Combined, Count, Proportion)
  
  return(summary_df)
}

transfusion_summary_table <- summarize_transfusion_data_combined(char.transfusion)

#Table of transfusion pt characterisitcs
kable(transfusion_summary_table, format = "html", caption = "Characterisitcs of Patients with Transfusions (Categorical Variables)", 
      col.names = c("Variable", "Count","Proportion (%)")) %>%
  kable_styling(bootstrap_options = "condensed") %>%
  row_spec(c(1:3, 6:7, 10:11, 14:15, 18:19, 22:23, 26:27, 32:37), background = "#f2f2f2") #highlight to group variables



#######################################
### Testing collinearity of variables ## 

#' Next, I want to create a simple correlation plot to view if 
#' variables are correlated with each other

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
  select(Age, Gender, Weight, Height, BMI, Type,Transfusion, ICU_LOS, HOSPITAL_LOS, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN,
         Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, Pre_Fibrinogen, Pre_Creatinine)

group2 <- correlation_data %>%
  select(Age, Gender, Weight, Height, BMI, Type,Transfusion, ICU_LOS, HOSPITAL_LOS, ALIVE_30DAYS_YN, ALIVE_90DAYS_YN, ALIVE_12MTHS_YN,
         Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_INR, Pre_PTT, Pre_Fibrinogen, Pre_Creatinine)

#' Compute correlation matrix between group1 and group2
cor_matrix <- cor(group1, group2, use = "pairwise.complete.obs")

#' Plot the correlation heatmap
corrplot(cor_matrix, method = "color", is.corr = TRUE, 
         tl.cex = 0.8, number.cex = 0.7,
         title = "Correlation of Predcitors ",
         mar = c(0, 0, 1, 0))

# Define the threshold for high correlation 
threshold <- 0.7

# Find variable pairs with absolute correlation above the threshold
high_corr_pairs <- which(abs(cor_matrix) > threshold & lower.tri(cor_matrix), arr.ind = TRUE)

# Display the variable names and their correlation values
high_corr_variables <- data.frame(
  Variable1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Variable2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

print(high_corr_variables)

#thus, we will choose to remove weight, hospital_los, Pre_Hb, Pre_Hct, Pre_Platelets, Pre_PT, Pre_PTT, Pre_Fibrinogen and Pre_Creatinine from our analysis.
# We will use BMI, icu_los and Pre_INR

#######################################
#' Additional cleaning and Imputation #

#' We will now create histograms for the two columns we will impute.
#' This will help inform the imputation method we will use.

#' Histogram for LAS_score
ggplot(data, aes(x = LAS_score)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of LAS Score", x = "LAS Score", y = "Frequency") +
  theme_minimal()
#' Right skewed - slightly normal

#' Histogram for Pre_PTT
ggplot(data, aes(x = Pre_PTT)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Pre PTT", x = "Pre PTT", y = "Frequency") +
  theme_minimal()
#' Right skewed

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

#' Imputation ##
#' Converting character variables to factors
data <- data %>% mutate_if(is.character, as.factor)


#' Generate default methods vector
methods <- make.method(data)

# Set all methods to "" (no imputation)
methods[] <- ""

# Set methods for variables to impute
methods["LAS_score"] <- "pmm"

# Predictors for LAS_score
predictors_LAS <- c("Age", "Gender", "BMI", "COPD", "alpha1_Antitrypsin_Deficiency", 
                    "Cystic_Fibrosis", "Idiopathic_Pulmonary_Hypertension",
                    "Interstitial_Lung_Disease", "Pulm_Other", "Type")

# Initialize predictor matrix with zeros
pred_matrix <- make.predictorMatrix(data)
pred_matrix[,] <- 0  # Set all entries to 0

# Set predictors for LAS_score
pred_matrix["LAS_score", predictors_LAS] <- 1

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


#' The following plots are used to visualize the imputed 
#' data. 
xyplot(imputed111, LAS_score ~ Gender)

#Diagnostic tests - distribution of complete and imputed data needs to be similar
stripplot(imputed111, pch = c(21, 20), cex = c(1, 1.5))

# Analysis 
# Objective: Identify predictors that influence the need for transfusions 

#Assess and compares the performance of the methods (lasso classification vs. CART) using a fraction 
#of the original data that was not used for training/tuning.
#The best model (highest AUC score) will be used for further analysis.

#Let's check the performance of LASSO classification model
#LASSO CLASSIFICATION 

#Set the seed
set.seed(111)

#' Next we are going to identify the predictors that 
#' we will be using in the Lasso classification model. 

x <- c(
  "Type", "Gender", "Height", "Age", "BMI", "COPD",
  "alpha1_Antitrypsin_Deficiency", "Cystic_Fibrosis",
  "Idiopathic_Pulmonary_Hypertension", "Interstitial_Lung_Disease",
  "Pulm_Other", "Redo_Lung_Transplant", "ExVIVO_Lung_Perfusion",
  "Preoperative_ECLS", "LAS_score", "Pre_INR"
)

# Sub-setting the model data to include the predictors and "Transfusion" variable 
model1data <- data[, c(x, "Transfusion")]

#Ensure "Transfusion" variable is factorized 
model1data$Transfusion <- as.factor(model1data$Transfusion)

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
  #Create a vector of row indices that corresponds to the training set
  #The for loop will create 5 unique training sets
  training <- sample(nrow(model1data), round(nrow(model1data) * 0.8))
  
  #Create a dummy variable for categorical variables and keeping the continuous variables the same 
  x_classification <- model.matrix(Transfusion ~., model1data)[training, -1]
  
  #Create a vector with the response values
  y_classification <- model1data$Transfusion[training]

  #Train a model
  lasso_model_classification <- glmnet(x_classification, y_classification, family = "binomial")
  
  #Plot Lasso plot
  plot(
    lasso_model_classification, xvar = "lambda", label = TRUE, col = colours, 
    lwd = 1,
    main = paste("Log(lambda) vs. Coefficients for Lasso Iteration of", i),
    xlab = "log(Lambda)", ylab = "Coefficients"
  )
  
  #Create a legend
  legend(
    "topright", legend = rownames(lasso_model_classification$beta),
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

# Create the table with gt
lasso_class_predictors_table %>%
  gt() %>%
  tab_header(
    title = "Lasso Classifiers - Selected Predictors for Each Iteration"
  ) %>%
  cols_label(
    Iteration = "Iteration",
    Predictors = "Predictors"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(75)
  )

#Check how common each predictors appears 

#Flatten the list of predictors and count the frequency of each predictor
predictor_list <- unlist(lasso_classi_predictor)
predictor_freq <- table(predictor_list)

#Sort the frequencies in descending order
predictor_freq_sorted <- sort(predictor_freq, decreasing = TRUE)

#View the sorted frequency table
predictor_freq_sorted

#Convert the AUC values to a data frame (table)
lasso_classi_auc_table <- data.frame(Iteration = 1:5, AUC = lasso_classi_auc)

# Create the table with gt
lasso_classi_auc_table %>%
  gt() %>%
  tab_header(
    title = "Lasso Classifiers - AUC for Each Iteration"
  ) %>%
  cols_label(
    Iteration = "Iteration",
    AUC = "AUC"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  fmt_number(
    columns = c(AUC),
    decimals = 2
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(50)
  )


#Calculate the average AUC
lasso_classi_auc_average <- round(mean(lasso_classi_auc), digits = 3)

#Let's check the performance of CART model (full and pruned tree)
###CART (full and pruned)
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
  
  #Generate the ROC curve for testing - pruned tree
  roc_prune <-  roc(model1data$Transfusion[-training] ~ pred_probs_prune_tree)
  plot(roc_prune)
  title(paste("AUC-ROC Curve (Prune) of", i))
  prune_classi_auc[i] <- roc_prune$auc
  
  #Let's do the same evaluation for the original tree without pruning 
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

# Create the table with gt
tree_classi_predictors_table %>%
  gt() %>%
  tab_header(
    title = "CART Tree - Selected Predictors for Each Iteration"
  ) %>%
  cols_label(
    Iteration = "Iteration",
    Variables = "Predictors"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(75)
  )

#Check how common each predictors appears 

#Flatten the list of predictors and count the frequency of each predictor
all_tree_predictors <- unlist(tree_classi_predictors)
tree_predictor_freq <- table(all_tree_predictors)

#Sort the frequencies in descending order
tree_predictor_freq_sorted <- sort(tree_predictor_freq, decreasing = TRUE)

#Display the sorted frequency of predictors
tree_predictor_freq_sorted

#Convert the AUC values to a data frame (table) - full tree
tree_auc_table <- data.frame(Iteration = 1:5, AUC = tree_classi_auc)

# Create the table with gt
tree_auc_table %>%
  gt() %>%
  tab_header(
    title = "CART Tree - AUC for Each Iteration"
  ) %>%
  cols_label(
    Iteration = "Iteration",
    AUC = "AUC"
  ) %>%
  fmt_number(
    columns = AUC,
    decimals = 2
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(75)
  )

#Calculate the average AUC - full tree 
tree_auc_average <- round(mean(tree_classi_auc), digits = 3)

#Convert the predictors list to a data frame for better visualization (table) - prune tree
prune_predictors_table <- data.frame(Iteration = 1:5, Variables = sapply(prune_classi_predictors, toString))

# Create the table with gt
prune_predictors_table %>%
  gt() %>%
  tab_header(
    title = "CART Pruned Tree - Selected Predictors for Each Iteration"
  ) %>%
  cols_label(
    Iteration = "Iteration",
    Variables = "Predictors"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(75)
  )

#Convert the AUC values to a data frame (table) - prune tree
prune_auc_table <- data.frame(Iteration = 1:5, AUC = prune_classi_auc)

prune_auc_table %>%
  gt() %>%
  tab_header(
    title = "CART Pruned Tree - AUC for Each Iteration"
  ) %>%
  cols_label(
    Iteration = "Iteration",
    AUC = "AUC"
  ) %>%
  fmt_number(
    columns = AUC,
    decimals = 2
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(75)
  )

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

# Create the table with gt
auc_comparison_table %>%
  gt() %>%
  tab_header(
    title = "Comparison of Average AUC Across Models"
  ) %>%
  cols_label(
    Model = "Model",
    Average_AUC = "Average AUC"
  ) %>%
  fmt_number(
    columns = vars(Average_AUC),
    decimals = 3
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(75)
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
title("Log(lambda) vs. Coefficients for LASSO Classifier")

#' Optimal lambda value that maximizes AUC 
optimal_lambda <- cv.lasso$lambda.min
# AUC corresponding to optimal lambda
optimal_auc <- cv.lasso$cvm[cv.lasso$lambda == optimal_lambda]

#' Coefficients at optimal lambda
optimal_coefs <- coef(cv.lasso, s = "lambda.min")

#Extract non-zero coefficients that stayed in the mode when using optimal lambda
optimal_lasso_predictors <- rownames(optimal_coefs)[optimal_coefs[, 1] != 0][-1]

#Convert the predictors list to a data frame for better visualization (table)
final_lasso_class_predictors_table <- data.frame(Predictors = optimal_lasso_predictors)

# Create the table with gt
final_lasso_class_predictors_table %>%
  gt() %>%
  tab_header(
    title = "Lasso Classifiers - Final Selected Predictors"
  ) %>%
  cols_label(
    Predictors = "Predictors"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(50)  # Adjust width as needed
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
  "Type", "Gender", "Height", "Age", "BMI", "COPD",
  "alpha1_Antitrypsin_Deficiency", "Cystic_Fibrosis",
  "Idiopathic_Pulmonary_Hypertension", "Interstitial_Lung_Disease",
  "Pulm_Other", "Redo_Lung_Transplant", "ExVIVO_Lung_Perfusion",
  "Preoperative_ECLS", "LAS_score", "Pre_INR"
)


# Subset data for the new model
model22data <- data[, c(predictors22, "Total_24hr_RBC")]

# Create design matrix for predictors and outcome with updated variable names
x22 <- model.matrix(Total_24hr_RBC ~ ., data = model22data)[, -1]
y22 <- model22data$Total_24hr_RBC

#Train Lasso model
lasso_model22 <- glmnet(x22, y22, family = "gaussian")

#Plot coefficients vs log(lambda) - Lasso Path
plot(
  lasso_model22, xvar = "lambda", label = TRUE, col = colours, 
  lwd = 1,
  main = "Lasso Paths",
  xlab = "log(Lambda)", ylab = "Coefficients"
)

#Create a legend
legend(
  "topright", legend = rownames(lasso_model22$beta),
  col = colours,  
  lty = 1, lwd = 1, cex = 0.6, ncol = 2, title = "Predictors"
)

#Cross-validation to find optimal lambda that minimizes MSE
set.seed(123)
cv_lasso22 <- cv.glmnet(x22, y22, family = "gaussian")

#Plot cross-validation curve
plot(cv_lasso22)
title(main = "Cross-Validation Plot for Lasso Regression")

#Extract optimal lambda
optimal_lambda22 <- cv_lasso22$lambda.min
print(paste("optimal lambda:", optimal_lambda22))

#Coefficients at optimal lambda
optimal_coefs22 <- coef(cv_lasso22, s = "lambda.min")

#Extract non-zero coefficients that stayed in the mode when using optimal lambda
optimal_reg_predictors <- rownames(optimal_coefs22)[optimal_coefs22[, 1] != 0][-1]

#Convert the predictors list to a data frame for better visualization (table)
lasso_reg_predictors_table <- data.frame(Predictors = optimal_reg_predictors)

# Create table using gt package
lasso_reg_predictors_table %>%
  gt() %>%
  tab_header(
    title = "Lasso Regression - Final Selected Predictors"
  ) %>%
  cols_label(
    Predictors = "Predictors"
  ) %>%
  tab_options(
    table.width = pct(100),  # Adjust table width as needed
    table.font.size = "small"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  )
######################### Q2

#################################################################### Setting up Survival time

#' The first thing we will do is ensure all the date columns
#' are in the correct format

data$OR_Date <- as.Date(data$OR_Date)
data$DEATH_DATE <- as.Date(data$DEATH_DATE, format = "%d-%b-%Y")

#' Next we will use the last death date as the last date of follow
#' up. See the results section in the report for more elaboration.
last_follow_up_date <- as.Date("2020-01-22")

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

#' This step is just to make sure there are no 
#' negative values.
data$Survival_Time <- pmax(data$Survival_Time, 0)

######################################################################### KM for Transfusion 
#' Setting the data in the correct type
data$Transfusion <- as.factor(data$Transfusion)

#' Fitting the KM model and viewing its summary
km_fit_transfusion <- survfit(Surv(Survival_Time, status) ~ Transfusion, data = data)
summary(km_fit_transfusion)

# Plotting the KM curve in a similar way to the tutorials
plot(km_fit_transfusion, xlab = "Time (days)", ylab = "Survival Probability",
     col = 1:2, lwd = 2, main = "Kaplan-Meier Survival Curves by Transfusion")
legend("bottom", legend = levels(data$Transfusion), col = 1:2, lwd = 2)

#' Using a new function that was found in order to provide a more
#' elaborate graph with the n.risk table.
ggsurvplot(
  km_fit_transfusion,
  data = data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = TRUE,
  title = "Survival by Transfusion",
  xlab = "Time (days)",
  ylab = "Survival Probability",
  legend.title = "Transfusion",
  ggtheme = theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  
    axis.title = element_text(face = "bold"),              
    axis.text = element_text(face = "bold")                
  ),
)

#' Now we are going to do the log rank test
log_rank_transfusion <- survdiff(Surv(Survival_Time, status) ~ Transfusion, data = data)
print(log_rank_transfusion)

#' Next, I want to view the distribution of time points at which patients were deceased.
event.survival <- data %>% filter(status == 1) %>% pull(Survival_Time)
hist(
  event.survival,
  breaks = 20,  # Adjust number of bins as needed
  main = "Histogram of Survival Times (Days) for Deceased Patients",
  xlab = "Survival Time (Days)",
  ylab = "Frequency",
  col = "lightblue",
  border = "black"
)

######################################################################## Cox PH Model
#' We will start by definiting the predictors. 
#' We will use transfusion, in addition to all the significant predictors from the earlier
#' Lasso classification model.
predictors_cox <- c(
   "Type", "Height", "BMI", "Interstitial_Lung_Disease", "COPD", "Pulm_Other",
   "Transfusion", "Preoperative_ECLS", "LAS_score", 
   "Pre_INR"
)

#' Creating the Cox Proportion Hazards model. We will continue to censor for survival time
#' using the same assumption.
coxf <- as.formula(paste("Surv(Survival_Time, status) ~", paste(predictors_cox, collapse = " + ")))
coxmodel <- coxph(coxf, data = data)
summary(coxmodel)

#' Checking assumptions
phtest <- cox.zph(coxmodel)
print(phtest)



#' We are going to start by creating a results table, with all the things we 
#' want to include, with the appropriate rounding.
cox_summary <- summary(coxmodel)
result_table <- data.frame(
  Variable = rownames(cox_summary$coefficients),
  Hazard_Ratio = round(cox_summary$coefficients[, "exp(coef)"], 2),
  CI = paste0("(", 
              round(cox_summary$conf.int[, "lower .95"], 2), 
              ", ", 
              round(cox_summary$conf.int[, "upper .95"], 2), 
              ")"),
  P_Value = round(cox_summary$coefficients[, "Pr(>|z|)"], 3) # Rounded to 3 decimals
)

#' Creating the table
gt_table <- result_table %>%
  gt() %>%
  tab_header(
    title = md("**Cox Proportional Hazards Model Results**"),
    subtitle = md("**Hazard Ratios, 95% CI, and P-Values**")
  ) %>%
  fmt_number(
    columns = c(Hazard_Ratio),
    decimals = 2
  ) %>%
  fmt_number(
    columns = P_Value,
    decimals = 3
  ) %>%
  cols_label(
    Variable = md("**Predictor**"),
    Hazard_Ratio = md("**Hazard Ratio**"),
    CI = md("**95% CI**"),
    P_Value = md("**P-Value**")
  )
gt_table

####################################################### Wilcoxon

#' The histogram below shows that ICU lengths of stay are heavily right skewed.
#' A non-parametric hypothesis test required.
hist(data$ICU_LOS,
     breaks = 300,
     main = "Distribution of ICU Length of Stay",
     xlab = "ICU Length of Stay (days)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black" , 
     xlim = c(1, 100))

#' Wilcoxon test conducted to see if there is a significant difference between 
#' those with and without transfusions.
wilcox.icu <- wilcox.test(ICU_LOS ~ Transfusion, data = data)
icu.p <- wilcox.icu$p.value
wilcox.icu

#' Boxplot created to visualize.
boxplot(ICU_LOS ~ Transfusion, data = data,
        main = "ICU Length of Stay by Transfusion",
        xlab = "Transfusion",
        ylab = "ICU Length of Stay (days)",
        col = c("lightblue", "tomato"),
        ylim = c(0, 25))




#' The following are redos of the Lasso tables to include coefficients
optimal_coefs <- coef(cv.lasso, s = "lambda.min")

non_zero_coefs <- optimal_coefs[optimal_coefs[, 1] != 0, , drop = FALSE]
non_zero_coefs <- non_zero_coefs[-1, , drop = FALSE]  

lasso_class_table <- data.frame(
  Predictors = rownames(non_zero_coefs),
  Coefficient = as.numeric(non_zero_coefs)
)

lasso_class_table %>%
  gt() %>%
  tab_header(
    title = "Lasso Classifiers - Final Selected Predictors with Coefficients"
  ) %>%
  cols_label(
    Predictors = "Predictors",
    Coefficient = "Coefficient"
  ) %>%
  fmt_number(
    columns = c(Coefficient),
    decimals = 4
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(50) 
  )




optimal_coefs22 <- coef(cv_lasso22, s = "lambda.min")

non_zero_coefs22 <- optimal_coefs22[optimal_coefs22[, 1] != 0, , drop = FALSE]
non_zero_coefs22 <- non_zero_coefs22[-1, , drop = FALSE]  

lasso_reg_table <- data.frame(
  Predictors = rownames(non_zero_coefs22),
  Coefficient = as.numeric(non_zero_coefs22)
)

lasso_reg_table %>%
  gt() %>%
  tab_header(
    title = "Lasso Regression - Final Selected Predictors with Coefficients"
  ) %>%
  cols_label(
    Predictors = "Predictors",
    Coefficient = "Coefficient"
  ) %>%
  fmt_number(
    columns = c(Coefficient),
    decimals = 4
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    table.font.size = "small",
    table.width = pct(100)  
  )

data$Transfusion_numeric <- as.numeric(data$Transfusion) - 1
# Calculate correlation
correlation <- cor(data$Transfusion_numeric, data$Total_24hr_RBC, method = "pearson")

# Print the correlation
print(correlation)


