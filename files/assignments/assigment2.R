rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                             # Clear the Console

#install haven
#install tidyverse
#install MASS
library(haven)
  
# Set correct file path (use double backslashes or a forward slash)
file_path <- "C:/Users/annni/Documents/1_Spring_2025/1_EPPS6323/Assign_2/TEDS_2016.dta"

# Load the Stata file
TEDS_2016 <- read_stata(file_path)

# View the first few rows of the data
head(TEDS_2016)

#checking to see if NA values in any of the considered columns
colSums(is.na(TEDS_2016)) #checks misssing values for all columns n gives the sum
colSums(is.na(TEDS_2016[, c("Tondu", "female", "DPP", "age", "income", "edu", "Taiwanese", "Econ_worse")]))

#edu has 10 missing values
hist(TEDS_2016$edu, main = "Distribution of Education", col = "blue", breaks = 10)#to see if they are normally distributed
TEDS_2016$edu[is.na(TEDS_2016$edu)] <- median(TEDS_2016$edu, na.rm = TRUE)

#filter out NA missing values from edu column
library(dplyr)
# Remove rows where edu is NA
TEDS_2016 <- TEDS_2016 %>% filter(!is.na(edu))

# Convert categorical variables to factors
TEDS_2016$female <- factor(TEDS_2016$female)
TEDS_2016$DPP <- factor(TEDS_2016$DPP)
TEDS_2016$Taiwanese <- factor(TEDS_2016$Taiwanese)

# Convert Tondu to an ordered factor
TEDS_2016$Tondu <- factor(TEDS_2016$Tondu, ordered = TRUE)

#Tondu is ordinal, we use Spearman’s correlation for numeric predictors (age, income, edu, Econ_worse)
cor.test(as.numeric(TEDS_2016$Tondu), TEDS_2016$age, method = "spearman")
cor.test(as.numeric(TEDS_2016$Tondu), TEDS_2016$income, method = "spearman")
cor.test(as.numeric(TEDS_2016$Tondu), TEDS_2016$edu, method = "spearman")
cor.test(as.numeric(TEDS_2016$Tondu), TEDS_2016$Econ_worse, method = "spearman")

#Chi-square test to check associations between Tondu and categorical variables (female, DPP, Taiwanese)
chisq.test(TEDS_2016$Tondu, TEDS_2016$female)
chisq.test(TEDS_2016$Tondu, TEDS_2016$DPP)
chisq.test(TEDS_2016$Tondu, TEDS_2016$Taiwanese)

#Using Ordinal Logistic Regression to examine how female, DPP, age, income, edu, Econ_worse affect Tondu

# Load library
library(MASS)

# Run Ordinal Logistic Regression
model <- polr(Tondu ~ female + DPP + age + income + edu + Taiwanese + Econ_worse, data = TEDS_2016, Hess = TRUE)

# Display model summary
summary(model)

# Calculate Odds Ratios
exp(coef(model))

library(ggplot2)

# Bar plot of Tondu distribution
ggplot(TEDS_2016, aes(x = Tondu)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Tondu Scores", x = "Tondu (Unification - Independence)", y = "Count")

#Distribution of votesai votes
table(TEDS_2016$votetsai)  # Frequency count of people who voted for Tsai
prop.table(table(TEDS_2016$votetsai))  # Proportion of votes

#plot for votesai votes
ggplot(TEDS_2016, aes(x = factor(votetsai))) +
  geom_bar(fill = "green") +
  theme_minimal() +
  labs(title = "Distribution of Votes for Tsai Ing-wen",
       x = "Voted for Tsai (0 = No, 1 = Yes)",
       y = "Count")

#Q7.Generate a frequency table and bar chart for the Tondu variable
#assign meaningful labels to its  values 

# Convert Tondu into a factor with labels
TEDS_2016$Tondu <- factor(TEDS_2016$Tondu, 
                          levels = c(1, 2, 3, 4, 5, 6, 9), 
                          labels = c("Unification now", 
                                     "Status quo, unif. in future", 
                                     "Status quo, decide later", 
                                     "Status quo forever", 
                                     "Status quo, indep. in future", 
                                     "Independence now", 
                                     "No response"))

#generate a table of counts:
table(TEDS_2016$Tondu)  # Frequency counts of each category
prop.table(table(TEDS_2016$Tondu))  # Proportion of each category

#generate a table visually pleasing 
library(knitr)

# Create frequency and proportion tables
freq_table <- table(TEDS_2016$Tondu)
prop_table <- prop.table(freq_table)

# Combine into a data frame
tondu_summary <- data.frame(
  "Tondu Category" = names(freq_table),
  "Count" = as.numeric(freq_table),
  "Proportion (%)" = round(100 * as.numeric(prop_table), 2)
)

# Print nicely formatted table
kable(tondu_summary, format = "markdown", caption = "Distribution of Tondu Preferences")


# generate bar chart
ggplot(TEDS_2016, aes(x = Tondu)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Distribution of Tondu Preferences", 
       x = "Tondu (Unification - Independence)", 
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotates x-axis labels