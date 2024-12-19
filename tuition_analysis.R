#take the csv file
data <- read.csv("fordhamTuition.csv")
#load lmtest, car (for VIF values),
#glmnet (ridge regression)
#and stats (autocorrelation) packages
library(lmtest)
library(car)
library(glmnet)
library(stats)
library(scales)
options(scipen=5)
#create scatterplots to check for linear relationships
#(needed for linear regression tests)
#Scatterplot for Financial Aid vs. Tuition Revenue
#plot(data$Tuition_Revenue, data$Financial_Aid,
#xlab = "Tuition Revenue",
#ylab = "Financial Aid",
#main = "Scatterplot: Tuition Revenue vs. Financial Aid")
#line of best fit
#abline(lm(Financial_Aid ~ Tuition_Revenue, data=data), col="red")

#Scatterplot for Operating Expenses vs. Tuition
plot(data$fordham_tuition_fees_charge, data$Tuition_Revenue,
     xlab = "Tuition Charge (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Total Tuition Revenue vs. Tuition Charge")
abline(lm(Tuition_Revenue ~ 
            fordham_tuition_fees_charge, data = data), col = "red")

#Scatterplot for Operating Expenses vs. Tuition
plot(data$Operating_Expenses, data$Tuition_Revenue,
     xlab = "Operating Expenses (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Total Operating Expenses vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Operating_Expenses, data = data), col = "red")

#Scatterplot for Institutional Admin vs. Tuition Revenue
plot(data$Institutional_Administration, data$Tuition_Revenue,
     xlab = "Institutional Administration (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Institutional Administration vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Institutional_Administration,
          data = data), col = "red")

#Scatterplot for Inflation vs. Tuition Revenue
plot(data$Inflation_Rate, data$Tuition_Revenue,
     xlab = "Inflation Rate (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Inflation Rate vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Inflation_Rate, data = data), col = "red")

#Scatterplot for Academic Support vs. Tuition Revenue
plot(data$Academic_Support, data$Tuition_Revenue,
     xlab = "Academic Support (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Academic Support vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Academic_Support, data = data), col = "red")

#Scatterplot for Instruction and Research vs. Tuition Revenue
plot(data$Instruction_and_research, data$Tuition_Revenue,
     xlab = "Instruction and Research (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Instruction and Research vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Instruction_and_research, data = data), col = "red")

#Scatterplot for Student Services vs. Tuition Revenue
plot(data$Student_Services, data$Tuition_Revenue,
     xlab = "Student Services (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Student Services vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Student_Services, data = data), col = "red")

#Scatterplot for Public Service vs. Tuition Revenue
plot(data$Public_Service, data$Tuition_Revenue,
     xlab = "Public Service (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Public Service vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Public_Service, data = data), col = "red")

#Scatterplot for Saleries and Wages vs. Tuition Revenue
plot(data$Salaries_and_wages, data$Tuition_Revenue,
     xlab = "Salaries and Wages (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Saleries vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Salaries_and_wages, data = data), col = "red")

#Scatterplot for Auxiliary Services vs. Tuition Revenue
plot(data$Auxiliary_Services, data$Tuition_Revenue,
     xlab = "Auxiliary Services (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Auxiliary Services vs. Tuition Revenue")
abline(lm(Tuition_Revenue ~ Auxiliary_Services, data = data), col = "red")

#Scatterplot for Operating Expenses vs. Tuition Charge
plot(data$Operating_Expenses, data$tuition_charge,
     xlab = "Operating Expenses (in thousands)",
     ylab = "Tuition Charge (in thousands)",
     main = "Scatterplot: Operating Expenses vs. Tuition Charge")
abline(lm(tuition_charge ~ Operating_Expenses, data = data), col = "red")

#Scatterplot for Institutional Admin vs. Tuition Charge
plot(data$Institutional_Administration, data$tuition_charge,
     xlab = "Institutional Administration (in thousands)",
     ylab = "Tuition Charge (in thousands)",
     main = "Scatterplot: Institutional Administration vs. Tuition Charge")
abline(lm(tuition_charge ~ Institutional_Administration,
          data = data), col = "red")

#Scatterplot for Inflation vs. Tuition Charge
plot(data$Inflation_Rate, data$tuition_charge,
     xlab = "Inflation Rate (percent)",
     ylab = "Tuition Charge (in thousands)",
     main = "Scatterplot: Inflation Rate vs. Tuition Charge")
abline(lm(tuition_charge ~ Inflation_Rate, data = data), col = "red")

#Scatterplot for Academic Support vs. Tuition Charge
plot(data$Academic_Support, data$tuition_charge,
     xlab = "Academic Support (in thousands)",
     ylab = "Tuition Charge (in thousands)",
     main = "Scatterplot: Academic Support vs. Tuition Charge")
abline(lm(tuition_charge ~ Academic_Support, data = data), col = "red")

#Scatterplot for Instruction and Research vs. Tuition Charge
plot(data$Instruction_and_research, data$tuition_charge,
     xlab = "Instruction and Research (in thousands)",
     ylab = "Tuition Revenue (in thousands)",
     main = "Scatterplot: Instruction and Research vs. Tuition Revenue")
abline(lm(tuition_charge ~ Instruction_and_research, data = data), col = "red")

#Scatterplot for Institutional Admin vs. Tuition Charge
plot(data$Student_Services, data$tuition_charge,
     xlab = "Student Services (in thousands)",
     ylab = "Tuition Charge (in thousands)",
     main = "Scatterplot: Student Services vs. Tuition Charge")
abline(lm(tuition_charge ~ Student_Services, data = data), col = "red")

#Scatterplot for Institutional Admin vs. Tuition Charge
plot(data$Public_Service, data$tuition_charge,
     xlab = "Public Service (in thousands)",
     ylab = "Tuition Charge (in thousands)",
     main = "Scatterplot: Public Service vs. Tuition Charge")
abline(lm(tuition_charge ~ Public_Service, data = data), col = "red")

#Scatterplot for Institutional Admin vs. Tuition Charge
plot(data$Salaries_and_wages, data$tuition_charge,
     xlab = "Salaries and Wages (in thousands)",
     ylab = "Tuition Charge (in thousands)",
     main = "Scatterplot: Saleries vs. Tuition Charge")
abline(lm(tuition_charge ~ Salaries_and_wages, data = data), col = "red")

#Scatterplot for Institutional Admin vs. Tuition Charge
plot(data$Auxiliary_Services, data$tuition_charge,
     xlab = "Auxiliary Services (in thousands)",
     ylab = "Tuition Charge (in thousands)",
     main = "Scatterplot: Auxiliary Services vs. Tuition Charge")
abline(lm(tuition_charge ~ Auxiliary_Services, data = data), col = "red")

#Shapiro-Wilk test for normality (p > 0.05 indicates normality)
#shapiro.test(data$Financial_Aid)
#shapiro.test(data$Operating_Expenses)
#shapiro.test(data$Tuition_Revenue)
#shapiro.test(data$Inflation_Rate)
#shapiro.test(data$Instruction_and_research)
#shapiro.test(data$Academic_Support)
#shapiro.test(data$Institutional_Administration)
#shapiro.test(data$Public_Service)
#shapiro.test(data$Student_Services)
#shapiro.test(data$Salaries_and_wages)

#choosing specific variables to include in the correlation matrix
#selected_vars <- data[complete.cases(data[c("Tuition_Revenue", "Operating_Expenses", "Instruction_and_research", "Institutional_Administration", "Auxiliary_Services", "Inflation_Rate")]), ]

# Calculate the correlation matrix (using Pearson correlation coefficient)
#correlation_matrix <- round(cor(selected_vars, use="pairwise.complete.obs"), 2)

# Print the correlation matrix
#print(correlation_matrix)

# Perform hypothesis tests for correlation coefficients
#cor_test_result <- cor.test(data$Tuition_Revenue, data$Operating_Expenses)
#print(cor_test_result)
#we can do it for the other variables too

#multiple linear regression model
print("linear regression")
lm_model <- lm(Tuition_Revenue ~
                 Instruction_and_research +
                   Institutional_Administration +
                   Public_Service +
                   Student_Services +
                   Auxiliary_Services, data = data)
# Calculate VIF values (for multicollinearity)
vif_values <- car::vif(lm_model)
# Print VIF values (they're too high)
print(vif_values)
#results
summary(lm_model)
#testing assumptions for linear regression
#test for heterodasticity
print("heterodasticity test (variance of residuals)")
bptest(lm_model)
# get list of residuals
print("residuals: ")
res <- resid(lm_model)
#test normality of residuals (errors)
print("testing normality of residuals H0: residuals are normal")
shapiro.test(res)
# produce residual vs. fitted (predicted values) plot
plot(fitted(lm_model), res,
     xlab = "predicted Tuition Revenue",
     ylab = "residuals",
     main = "Residuals Plot")
abline(0, 0)
# create Q-Q plot for residuals
qqnorm(res)
# add a straight diagonal line to the plot
qqline(res)
#bell curve
plot(density(res))
# Durbin-Watson test (tests autocorrelation independence of observations/errors
print("testing independence of errors H0: no autocorrelation")
dw_test <- dwtest(lm_model)
print(dw_test)

# Perform Ridge Regression with glmnet package
#multicollinearity is VERY high in the linear regression 
#(The VIF values are really high)
#ridge regression is robust against multicollinearity
#define response variable
subset_data <- data[2:17, ]
y <- subset_data$Tuition_Revenue
#define matrix of predictor variables
x <- data.matrix(subset_data[, c("Instruction_and_research",
                                 "Student_Services",
                                 "Institutional_Administration",
                                 "Auxiliary_Services")])
#fit ridge regression model
model <- glmnet(x, y, alpha = 0)
#view summary of model
summary(model)
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)
#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
#produce plot of test MSE by lambda value
plot(cv_model)
#find coefficients of best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)
#produce Ridge trace plot
plot(model, xvar = "lambda")
#use fitted best model to make predictions
y_predicted <- predict(model, s = best_lambda, newx = x)
#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
#find R-Squared
print("R^2")
rsq <- 1 - sse / sst
rsq
#testing assumptions
#Test for linearity (visual inspection)
#use scatterplot of y vs. each predictor variable
# Calculate residuals using predict with type="working"
residuals <- y - y_predicted
print(residuals)
#Test for homoscedasticity
plot(y_predicted, residuals, main = "Residuals vs Predicted Values",
     xlab = "Predicted Tuition Revenue",
     ylab = "Residuals")
abline(0, 0)
#Test for autocorrelation (acf = autocorrelation function)
#the point is for there not to be spikes
#at lags that go beyond the confidence interval
acf(residuals, main = "ACF Plot of Residuals")
abline(h = 0, col = "blue", lty = 2)
# Test for normality
shapiro.test(residuals)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
plot(density(residuals))

#Subsetting data to rows 10-20
#tuition_charge doesn't have a value in every row
#so only taking the rows it does have a value for
subset_data <- data[6:17, ]
# Define response variable
y <- subset_data$fordham_tuition_fees_charge
# Define matrix of predictor variables
x <- data.matrix(subset_data[, c("Instruction_and_research",
                                 "Student_Services",
                                 "Institutional_Administration",
                                 "Auxiliary_Services")])
# Scale predictor variables
# Fit ridge regression model
model <- glmnet(x, y, alpha = 0)
# View summary of model
summary(model)
# Perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)
# Find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
# Produce plot of test MSE by lambda value
plot(cv_model)
# Find coefficients of best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)
# Produce Ridge trace plot
plot(model, xvar = "lambda")
# Use fitted best model to make predictions
y_predicted <- predict(model, s = best_lambda, newx = x)
# Find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
# Find R-Squared
print("R^2")
rsq <- 1 - sse / sst
rsq
# Testing assumptions
# Test for linearity (visual inspection)
# scatterplot of y vs. each predictor variable
# Calculate residuals
residuals <- y - y_predicted
# Test for homoscedasticity
#Pretty sure this plot indicates heteroscedasticity
plot(y_predicted, residuals, main = "Residuals vs Predicted Values",
     xlab = "Predicted Tuition Charge", ylab = "Residuals")
abline(0, 0)
#Test for autocorrelation (acf = autocorrelation function)
#the point is for there not to be spikes
#at lags that go beyond the confidence interval
acf(residuals, main = "ACF Plot of Residuals")
abline(h = 0, col = "blue", lty = 2)
# Test for normality
shapiro.test(residuals)
hist(residuals)
qqnorm(residuals)
qqline(residuals)
plot(density(residuals))

# Plot Tuition over time
plot(data$Year, data$Tuition_Revenue, type = "l",
     xlab = "Year",
     ylab = "Tuition Revenue (in thousands)",
     col = "red",
     main = "Tuition Revenue Over Time")
plot(data$Year, data$Operating_Expenses, type = "l",
     xlab = "Year",
     ylab = "Operating Expenses (in thousands)",
     col = "red",
     main = "Expenses Over Time")
plot(data$Year, data$fordham_tuition_fees_charge, type = "l",
     xlab = "Year",
     ylab = "Tuition Charge (in thousands)",
     col = "red",
     main = "Tuition Over Time")

# Filter the data to include only the desired range of years
filtered_data <- data[data$Year >= 2007 & data$Year <= 2023, ]
# Find the maximum and minimum values for all variables
max_val <- max(max(filtered_data$fordham_tuition_fees_rate),
               max(filtered_data$columbia_tuition_fees_rate),
               max(filtered_data$Inflation_Rate),
               max(filtered_data$salary_rate_change))
min_val <- min(min(filtered_data$fordham_tuition_fees_rate),
               min(filtered_data$columbia_tuition_fees_rate),
               min(filtered_data$Inflation_Rate),
               min(filtered_data$salary_rate_change))

# Plot multiple time series variables on the same plot with adjusted y-axis
plot(filtered_data$Year, filtered_data$fordham_tuition_fees_rate,
     type = "l",
     xlab = "Year",
     ylab = "Percent",
     col = "red",
     main = "Tuition Rate Change and Inflation Over Time",
     ylim = c(min_val, max_val),
     yaxt = "n")  # Remove y-axis
lines(filtered_data$Year, filtered_data$columbia_tuition_fees_rate,
      col = "blue")
lines(filtered_data$Year, filtered_data$Inflation_Rate, col = "purple")
# Customize y-axis labels
axis(2, at = pretty(c(min_val, max_val)), labels =
       format(pretty(c(min_val, max_val)), scientific = FALSE))
legend("topleft", legend =
         c("Fordham Change", "Columbia Change", "Inflation Rate"),
       col = c("red", "blue", "purple"), lty = 1)

plot(filtered_data$Year, filtered_data$fordham_tuition_fees_rate,
     type = "l",
     xlab = "Year",
     ylab = "Percent Change",
     col = "red",
     main = "Tuition Rate Change and Salary Rate Change Over Time",
     ylim = c(min_val, max_val),
     yaxt = "n")  # Remove y-axis
lines(filtered_data$Year, filtered_data$salary_rate_change,
      col = "blue")
# Customize y-axis labels
axis(2, at = pretty(c(min_val, max_val)), labels =
       format(pretty(c(min_val, max_val)), scientific = FALSE))
legend("topleft", legend =
         c("Fordham Increase", "Salary Change"),
       col = c("red", "blue"), lty = 1)

var.test(data$fordham_tuition_fees_rate,
         data$pace_increase, alternative = "two.sided")
var.test(data$fordham_tuition_fees_rate,
         data$columbia_tuition_fees_rate, alternative = "two.sided")
var.test(data$fordham_tuition_fees_rate,
         data$Inflation_Rate, alternative = "two.sided")
#unequal variances
var.test(data$fordham_tuition_fees_rate,
         data$salary_rate_change, alternative = "two.sided")

shapiro.test(data$fordham_tuition_fees_rate)
plot(density(filtered_data$fordham_tuition_fees_rate))

shapiro.test(data$columbia_tuition_fees_rate)
plot(density(filtered_data$columbia_tuition_fees_rate))

plot(density(data$Inflation_Rate))
shapiro.test(data$Inflation_Rate)

plot(density(filtered_data$salary_rate_change))
shapiro.test((data$salary_rate_change))

# Contingency table of fordham_increase and columbia_increase
contingency_table <- table(data$fordham_increase, data$columbia_increase)
# Perform Fisher's exact test
fisher_result <- fisher.test(contingency_table)
# View the result
print(fisher_result)

# Contingency table of fordham_increase and inflation rate
contingency_table <- table(data$fordham_increase, data$Inflation_Rate)
# Perform Fisher's exact test
fisher_result <- fisher.test(contingency_table)
# View the result
print(fisher_result)

# Contingency table of fordham_increase and salary_rate_change
contingency_table <- table(data$fordham_increase, data$salary_rate_change)
# Perform Fisher's exact test
fisher_result <- fisher.test(contingency_table)
# View the result
print(fisher_result)

#the populations arent normal so we cant use T test


#We can use Mann-Whitney U Test
#used to compare differences between two independent groups
#when the dependent variable is continuous
#but not normally distributed

# Perform Mann-Whitney U test on fordham vs columbia rate
mannwhitneytest_result <- wilcox.test(data$fordham_tuition_fees_rate,
                                      data$columbia_tuition_fees_rate)
print(mannwhitneytest_result)
# Print the test results
summary(mannwhitneytest_result)

# Perform Mann-Whitney U test on tuition change vs inflation
mannwhitneytest_result <- wilcox.test(data$fordham_tuition_fees_rate,
                                      data$Inflation_Rate)
print(mannwhitneytest_result)
# Print the test results
summary(mannwhitneytest_result)

# Perform Mann-Whitney U test on tuition change vs salary rate change
mannwhitneytest_result <- wilcox.test(data$fordham_tuition_fees_rate,
                                      data$salary_rate_change)
print(mannwhitneytest_result)
# Print the test results
summary(mannwhitneytest_result)
