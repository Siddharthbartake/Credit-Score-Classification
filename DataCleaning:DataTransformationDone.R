rm(list=ls()); gc()
set.seed(1) 
library(class)
library(fastDummies)
library(dplyr)
library(ggplot2)
library(Amelia)
library(stringr)
library(caret)
library(MASS)
library(rpart)
library(rpart.plot)




# It's best to start by setting stringsAsFactors to FALSE globally for this session.
options(stringsAsFactors = FALSE)

# Improved data loading with checks for file existence and proper handling
traindata.df <- read.csv(file = "/Users/csuftitan/Documents/574Project.csv", header = TRUE, sep = ",") # nolint
#View(traindata.df)
#dat = read.csv('/Users/csuftitan/Documents/574Project.csv', stringsAsFactors=T, head=T,na.strings='')
traindata.df <- read.csv(file = '/Users/csuftitan/Documents/574Project.csv', header = TRUE, sep = ",", na.strings = c('', 'NA', '_______', '_'))

# More advanced data cleaning function, that integrates all cleaning steps for numeric columns.
clean_numeric_columns <- function(data, columns) {
  for (col_name in columns) {
    # Using dplyr for a more readable syntax
    data <- data %>% 
      mutate(!!col_name := as.numeric(gsub("[^0-9.-]", "", get(col_name))))
  }
  data
}
##removing negatives and replacing them with NA
traindata.df[c("Age", "Num_Bank_Accounts", "Num_of_Loan", "Monthly_Balance")] <- lapply(traindata.df[c("Age", "Num_Bank_Accounts", "Num_of_Loan", "Monthly_Balance")], function(x) ifelse(x < 0, NA, x))

#class(traindata.df$Annual_Income)

# List of columns that need cleaning
columns_need_cleaning <- c('Age', 'Annual_Income', 'Num_of_Loan', 'Num_of_Delayed_Payment',
                           'Changed_Credit_Limit', 'Outstanding_Debt', 'Amount_invested_monthly', 'Monthly_Balance')

# Clean the data
traindata.df <- clean_numeric_columns(traindata.df, columns_need_cleaning)

age_parts <- strsplit(as.character(traindata.df$Credit_History_Age), " Years and | Months")
age_parts <- lapply(age_parts, as.integer)
##View(traindata.df)

month_order <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')
traindata.df$Credit_History_Age<-round(sapply(age_parts, function(x) x[1] + x[2]/12),2)
##View(traindata.df)
original_order <- order(traindata.df$ID)

# A custom function for parsing and computing credit history in years.
traindata.df <- traindata.df %>%
  arrange(Customer_ID, match(Month, month_order)) %>%
  mutate(Credit_History_Age = ifelse(is.na(Credit_History_Age), 
                                     ifelse(!is.na(lag(Credit_History_Age)), 
                                            lag(Credit_History_Age) + 1/12, 
                                            lead(Credit_History_Age) - 1/12),
                                     Credit_History_Age))
##View(traindata.df)


traindata.df <- traindata.df[original_order, ]
##traindata.df$Credit_History_Age <- sapply(traindata.df$Credit_History_Age, compute_credit_history)



##View(traindata.df)

##traindata.df$Credit_History_Age <- sapply(traindata.df$Credit_History_Age, compute_credit_history)

# A function to clean categorical variables, to replace specific placeholder strings with NA
clean_categorical_variables <- function(data, columns) {
  for (col_name in columns) {
    data[[col_name]] <- ifelse(data[[col_name]] %in% c('_______', '_', 'NM', '!@9#%8', 'NA'), NA, data[[col_name]])
  }
  data
}
##View(traindata.df)
# Apply the cleaning function to the categorical variables
categorical_vars <- c('Occupation', 'Credit_Mix', 'Payment_of_Min_Amount', 'Payment_Behaviour')
traindata.df <- clean_categorical_variables(traindata.df, categorical_vars)

##matrix.na = is.na(traindata.df)
##pmiss = colMeans(matrix.na)
##missmap(traindata.df)

traindata.df <- traindata.df %>% mutate(Age = ifelse(Age < 0 | Age > 94, NA, as.numeric(Age)))

categorical_columns <- c("Num_Bank_Accounts", "Num_Credit_Card", "Num_of_Loan", "Delay_from_due_date", "Num_Credit_Inquiries")

for (var in categorical_columns) {
  print(paste("Frequency Table for", var))
  print(table(traindata.df[[var]], useNA = "ifany"))
  cat("\n")  # Add a newline for better separation
}


# Replacing outliers with NA using a more compact approach
handle_outliers <- function(data, column_names) {
  for (column_name in column_names) {
    # Calculate the IQR for each column
    Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    # Define the upper and lower bounds
    upper <- Q3 + 1.5 * IQR
    lower <- Q1 - 1.5 * IQR
    # Replace outliers with NA
    data[[column_name]] <- ifelse(data[[column_name]] > upper | data[[column_name]] < lower, NA, data[[column_name]])
  }
  return(data)
}


# Apply the outlier handling to the necessary columns
columns_with_outliers <- c('Annual_Income', 'Monthly_Inhand_Salary', 'Num_Bank_Accounts', 'Num_Credit_Card', 'Interest_Rate', 'Num_of_Loan', 'Delay_from_due_date', 'Num_of_Delayed_Payment',
                             'Changed_Credit_Limit', 'Num_Credit_Inquiries', 'Credit_Utilization_Ratio', 'Credit_History_Age', 'Total_EMI_per_month', 'Amount_invested_monthly', 'Monthly_Balance')
#traindata.df <- handle_outliers(traindata.df, columns_with_outliers)
for (col_name in columns_with_outliers) {
  traindata.df <- handle_outliers(traindata.df, col_name)
}


##View(traindata.df)


# Replacing NAs with mean or mode, refactored to use dplyr and to handle the operation more efficiently.
replace_missing_values <- function(dat, column_name, method = "mean") {
  unique_ids <- unique(dat[, 'Customer_ID'])
  for (id in unique_ids) {
    values_for_id <- dat[dat[, 'Customer_ID'] == id, column_name]
    if (method == "mean") {
      value_to_fill <- round(mean(values_for_id, na.rm = TRUE), 2)
    } else if (method == "mode") {
      tab <- table(values_for_id)
      value_to_fill <- names(tab)[tab == max(tab)][1]
      if (length(value_to_fill) == 0) {
        value_to_fill <- NA
      }
    }
    dat[dat[, 'Customer_ID'] == id & is.na(dat[, column_name]), column_name] <- value_to_fill
  }
  return(dat)
}

# Apply the NA replacement for numeric columns using mean
numeric_columns <- c('Age', 'Monthly_Inhand_Salary', 'Interest_Rate', 'Outstanding_Debt', 'Amount_invested_monthly', 'Monthly_Balance')
for (col_name in numeric_columns) {
  traindata.df <- replace_missing_values(traindata.df, col_name, method = "mean")
}

# Apply the NA replacement for categorical columns using mode
categorical_columns <- c('Name', 'Type_of_Loan', 'Num_Credit_Inquiries', 'Occupation', 'Num_Bank_Accounts', 'Num_Credit_Card', 'Num_of_Loan', 'Credit_Mix', 'Payment_of_Min_Amount', 'Total_EMI_per_month')

for (col_name in categorical_columns) {
  traindata.df <- replace_missing_values(traindata.df, col_name, method = "mode")
}

# Remove the unnecessary columns
traindata.df <- dplyr::select(traindata.df, -ID, -Customer_ID, -Name, -SSN, -Type_of_Loan, -Monthly_Inhand_Salary)

##na_count <- colSums(is.na(traindata.df))



# Print column name and NA count for every column
##for (col_name in names(traindata.df)) {
##  cat("Column:", col_name, "- NA count:", na_count[col_name], "\n")
##}

traindata.df <- na.omit(traindata.df)


#combining three levels of outcome variable to two levels
traindata.df$Credit_Score <- as.factor(ifelse(traindata.df$Credit_Score %in% c('Good','Standard'), 'Good', 'Poor'))
traindata.df$Credit_Score <- relevel(traindata.df$Credit_Score, ref = 'Poor')

#changing Total_EMI_per_month to numeric
traindata.df$Total_EMI_per_month <- as.numeric(traindata.df$Total_EMI_per_month)

#Making the data balanced to contain equal number of Good and Poor instances
min_instances <- min(table(traindata.df$Credit_Score))
balanced_data <- traindata.df %>%
  group_by(Credit_Score) %>%
  slice_sample(n = min_instances)%>%
  ungroup()

KNN_DATA <- balanced_data

write.csv(balanced_data, file = "/Users/manasgarg123/Desktop/balanced.csv", row.names = FALSE)

########################### Data Tranformation ########################################

# Convert factors to numeric
balanced_data$Num_Bank_Accounts <- as.numeric(as.character(balanced_data$Num_Bank_Accounts))
balanced_data$Num_Credit_Card <- as.numeric(as.character(balanced_data$Num_Credit_Card))

# Function to categorize the number of bank accounts
categorize_bank_accounts <- function(num_accounts) {
  if (is.na(num_accounts)) {
    return("Unknown")
  } else if (num_accounts <= 3) {
    return("Low")
  } else if (num_accounts <= 6) {
    return("High")
  } else {
    return("Very High")
  }
}

# Function to categorize the number of credit cards
categorize_credit_cards <- function(num_cards) {
  if (is.na(num_cards)) {
    return("Unknown")
  } else if (num_cards <= 2) {
    return("Low")
  } else if (num_cards <= 7) {
    return("Medium")
  } else {
    return("High")
  }
}

# Apply the categorization functions to the data
balanced_data$Num_Bank_Accounts <- sapply(balanced_data$Num_Bank_Accounts, categorize_bank_accounts)
balanced_data$Num_Credit_Card <- sapply(balanced_data$Num_Credit_Card, categorize_credit_cards)

# Transforming Num_Credit_Inquiries into ordered factors after checking the distribution
table(balanced_data$Num_Credit_Inquiries)
ggplot(balanced_data, aes(x = reorder(factor(Num_Credit_Inquiries), as.numeric(Num_Credit_Inquiries)), fill = Credit_Score)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Bar Plot of Num_Credit_Inquiries by Credit_Score")

# Function to categorize the number of credit inquiries
categorize_credit_inquiries <- function(num_inquiries) {
  if (is.na(num_inquiries)) {
    return("Unknown")
  } else if (num_inquiries <= 3) {
    return("Low")
  } else if (num_inquiries <= 7) {
    return("Moderate")
  } else {
    return("High")
  }
}

# Apply the categorization function to the data
balanced_data$Num_Credit_Inquiries <- sapply(balanced_data$Num_Credit_Inquiries, categorize_credit_inquiries)

# Convert Num_of_Loan to a factor with 'Low' and 'High' levels
table(balanced_data$Num_of_Loan)
ggplot(balanced_data, aes(x = Num_of_Loan, fill = Credit_Score)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Bar Plot of Num_of_Loan by Credit_Score")

# Function to categorize the number of loans
categorize_loans <- function(num_loans) {
  if (is.na(num_loans)) {
    return("Unknown")
  } else if (num_loans <= 5) {
    return("Low")
  } else {
    return("High")
  }
}

# Apply the categorization function to the data
balanced_data$Num_of_Loan <- sapply(balanced_data$Num_of_Loan, categorize_loans)

# Convert to factor with specified levels
balanced_data$Num_of_Loan <- factor(balanced_data$Num_of_Loan, levels = c('Low', 'High'))

write.csv(balanced_data, file = "/Users/manasgarg123/Desktop/balanced.csv", row.names = FALSE)

####################################MODEL SETTINGS#################################################

# Function to evaluate performance and setting cutoffs
performance = function(ytest, ypred, ct,method) {
  measures = c(
    Method=method,
    Cutoff = ct,
    ErrorRate = mean(ytest != ypred),
    Sensitivity = mean(ytest[ytest == 1] == ypred[ytest == 1]),
    Specificity = mean(ytest[ytest == 0] == ypred[ytest == 0]),
    Accuracy = mean(ytest == ypred)
  )
  return(measures)
}
#Setting threshold
cut_offs = c(0.6, 0.7, 0.8)
# Splitting data to training and test
train = round(nrow(balanced_data) * 0.7, 0)
train = sample(nrow(balanced_data), train)
df_train = balanced_data[train, ]
df_test = balanced_data[-train, ]


##Logistic Regression#########

library(MASS)

# Logistic Regression with Forward Selection
logit_fwd <- step(glm(Credit_Score ~ 1, data = df_train, family = binomial),
                  scope = ~ . + Credit_Score, direction = "forward")

# Logistic Regression with Backward Elimination
logit_bwd <- step(glm(Credit_Score ~ ., data = df_train, family = binomial),
                  direction = "backward")

# Logistic Regression with Stepwise Selection
logit_step <- step(glm(Credit_Score ~ 1, data = df_train, family = binomial),
                   scope = ~ . + Credit_Score, direction = "both")



# Forward Selection
result_log_fwd <- lapply(cut_offs, function(ct) {
  ypred_fwd <- ifelse(predict(logit_fwd, newdata = df_test, type = "response") > ct, 1, 0)
  performance(df_test$Credit_Score, ypred_fwd, ct, "Forward Selection")
})
result_log_fwd <- do.call(rbind, result_log_fwd)

# Backward Elimination
result_log_bwd <- lapply(cut_offs, function(ct) {
  ypred_bwd <- ifelse(predict(logit_bwd, newdata = df_test, type = "response") > ct, 1, 0)
  performance(df_test$Credit_Score, ypred_bwd, ct, "Backward Elimination")
})
result_log_bwd <- do.call(rbind, result_log_bwd)

# Stepwise Selection
result_log_step <- lapply(cut_offs, function(ct) {
  ypred_step <- ifelse(predict(logit_step, newdata = df_test, type = "response") > ct, 1, 0)
  performance(df_test$Credit_Score, ypred_step, ct, "Stepwise Selection")
})
result_log_step <- do.call(rbind, result_log_step)

# Combine results
result_log <- rbind(result_log_fwd, result_log_bwd, result_log_step)
result_log <- as.data.frame(result_log)

print(result_log)


