#Initial commit
if (!require(pacman))
  install.packages(pacman)

pacman::p_load(tidyverse,
               ggplot2,
               e1071)

# Reading the dataset
stroke_data <- read_csv("data/stroke_data.csv")

# View structure of the data
str(stroke_data)

# View summary statistics of the dataset
summary(stroke_data)

# Count of non-missing values for each column.
colSums(!is.na(stroke_data))

# List the data types of each column.
sapply(stroke_data, class)

# Finding the count NA (missing) values in a 'sex' column.
missing_count <- sum(is.na(stroke_data$sex))
missing_count

# There are 3 rows that are missing a value in the "sex" column - because of the negligible count, we can arbitrarily set them to be female, i.e. 0.
stroke_data$sex[is.na(stroke_data$sex)] <- 0

# Checking if the any NA values exist
missing_count <- sum(is.na(stroke_data$sex))
missing_count #output is 0

# Check if there are any missing values in the dataset
any(is.na(stroke_data))

# Check for duplicate rows in the dataset
sum(duplicated(stroke_data))

stroke_data_new <- stroke_data

# Convert 'sex' column
stroke_data_new$sex <- ifelse(stroke_data_new$sex == 0, "Female", "Male")

# Convert 'hypertension' column
stroke_data_new$hypertension <- ifelse(stroke_data_new$hypertension == 0, "Not had hypertension", "Had hypertension")

# Convert 'heart_disease' column
stroke_data_new$heart_disease <- ifelse(stroke_data_new$heart_disease == 0, "Not had heart disease", "Had heart disease")

# Convert 'ever_married' column
stroke_data_new$ever_married <- ifelse(stroke_data_new$ever_married == 0, "No", "Yes")

# Convert 'work_type' column
stroke_data_new$work_type <- ifelse(stroke_data_new$work_type == 0, "Never worked",
                         ifelse(stroke_data_new$work_type == 1, "Children",
                                ifelse(stroke_data_new$work_type == 2, "Govt job",
                                       ifelse(stroke_data_new$work_type == 3, "Self-employed", "Private"))))

# Convert 'Residence_type' column
stroke_data_new$Residence_type <- ifelse(stroke_data_new$Residence_type == 0, "Urban", "Rural")

# Convert 'smoking_status' column
stroke_data_new$smoking_status <- ifelse(stroke_data_new$smoking_status == 0, "Never smoked", "Smokes")

# Convert 'stroke' column
stroke_data_new$stroke <- ifelse(stroke_data_new$stroke == 0, "No", "Yes")

stroke_data_new

# Function to calculate count of unique values in each column
unique_values <- sapply(stroke_data_new, function(x) n_distinct(x))

# Create a dataframe with counts of unique values
unique_values_df <- data.frame('unique value count' = unique_values)

# Display the counts of unique values for each column
print(unique_values_df)

# The "work_type" column will need One Hot Encoder to process in nominal categorical data.
# "age", "avg_glucose_level" and "bmi" columns will be processed using Binning or Scaling.


# Filter out rows with negative age values in 'stroke_data_new'
stroke_data_new <- stroke_data_new[stroke_data_new$age >= 0, ]

# Check the number of rows in the updated dataframe
nrow(stroke_data_new) #58 rows were droppped

plot <- ggplot(stroke_data_new, aes(x = work_type, fill = stroke)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5)+
  labs(title = "Stroke distribution by Work Type") +
  scale_fill_manual(values = c('deepskyblue', 'orange')) +
  theme_minimal()

plot

# Only 1.3 percent of the data are divided into the values "children", "never worked". In addition, neither of 
# these two values had a stroke. This data can cause biases, so we can drop these records.




# Filter rows where 'work_type' is greater than 1
stroke_data_new <- subset(stroke_data_new, work_type > 1)

# Create a countplot using ggplot2
plot <- ggplot(stroke_data_new, aes(x = factor(work_type), fill = stroke)) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = paste0(sprintf("%.1f", (..count.. / sum(..count..)) * 100), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Stroke distribution by Work Type") +
  scale_fill_manual(values = c('deepskyblue', 'orange')) +
  theme_minimal()

plot
# The "work_type" feature is of the Nominal Variable type. Therefore One Hot Encoder must be activated here.

stroke_data_new <- stroke_data_new %>%
  mutate(work_type = case_when(
    work_type == 2 ~ "Govt job",
    work_type == 3 ~ "Self-employed",
    work_type == 4 ~ "Private",
    TRUE ~ as.character(work_type)  # Keep other values as is
  ))

# Encode 'work_type' column as a factor
stroke_data_new$work_type <- as.factor(stroke_data_new$work_type)

# Perform one-hot encoding using tidyr's 'pivot_wider()'
stroke_data_new <- stroke_data_new %>%
  pivot_wider(names_from = work_type, values_from = work_type, values_fn = length, values_fill = 0)

# Display the modified dataframe
head(stroke_data_new)

# Checking Skewness
library(moments)
library(e1071)
data_skew <- stroke_data_new[, c('age', 'avg_glucose_level', 'bmi')]

# Calculate skewness for the selected columns
skewness <- sapply(data_skew, skewness)

# Create a dataframe with skewness values
skew <- data.frame(skew = skewness)

# Add a column to identify columns with skewness greater than 0.75
skew$too_skewed <- skew$skew > 0.75

# Rename row names to column names
rownames(skew) <- c('age', 'avg_glucose_level', 'bmi')

# Display the skewness values and identification of too skewed columns
skew

# There are 2 columns with high Skewness. Therefore, we will normalize them.

# Transforming Data Using Box-Cox Transformation
