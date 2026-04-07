#9.3 Dummy Variable Regression Model (chick Data)
getwd()
setwd("C:/Users/user/Documents/R_DA")

# Load required libraries
library(readxl)
library(car)
library(corrplot)

# Load dataset
data <- read_excel("table9.3_chick.xlsx") 
data

# Inspect data structure and summary
str(data)
head(data)
summary(data)

# Convert gender to factor
data$gender <- factor(data$gender) 

# Split dataset by gender
data_male <- subset(data, gender == "m")
data_female <- subset(data, gender == "f")

# Descriptive statistics
library(dplyr)

summary_stats <- data %>%
  group_by(gender) %>%
  summarise(
    N = n(),
    mean_growth = mean(growth),
    sd_growth = sd(growth),
    mean_vitamin = mean(vitamin),
    sd_vitamin = sd(vitamin),
    min_growth = min(growth),
    max_growth = max(growth)
  )

print(summary_stats)


# Histogram plots
par(mfrow=c(1, 2)) 

hist(data$growth,
     main = "Growth",
     xlab = "Growth (Y)",
     ylab = "Frequency",
     col = "lightblue")

hist(data$vitamin,
     main = "Vitamin Dose",
     xlab = "Vitamin Dose (X)",
     ylab = "Frequency",
     col = "lightgreen")

# Boxplots
boxplot(growth ~ gender, data = data,
        main = "Growth by Gender",
        xlab = "Gender",
        ylab = "Growth (Y)",
        col = c("red", "blue"), 
        names = c("Female", "Male"))

boxplot(vitamin ~ gender, data = data,
        main = "Vitamin Dose by Gender",
        xlab = "Gender",
        ylab = "Vitamin Dose (X)",
        col = c("red", "blue"),
        names = c("Female", "Male"))

par(mfrow=c(1, 1)) 


# (a) Plot dose-response relationship by gender
x_range <- range(data$vitamin)
y_range <- range(data$growth)

plot(data$vitamin, data$growth,
     type = "n",
     xlab = "vitamin",
     ylab = "growth",
     main = "dose-response",
     xlim = x_range, 
     ylim = y_range
)

# Male data points
points(data_male$vitamin, data_male$growth, 
       pch = 2, 
       col = "blue")

# Female data points
points(data_female$vitamin, data_female$growth, 
       pch = 1, 
       col = "red")

# Male regression line
model_male <- lm(growth ~ vitamin, data = data_male)
abline(model_male, col = "blue", lwd = 2, lty = 1) 
print(coef(model_male))

# Female regression line
model_female <- lm(growth ~ vitamin, data = data_female)
abline(model_female, col = "red", lwd = 2, lty = 2) 
print(coef(model_female))

# Add legend
legend("topleft", 
       legend = c("Male", "Female"), 
       col = c("blue", "red"), 
       pch = c(2, 1), 
       lwd = 2, 
       lty = c(1, 2), 
       bty = "n")


# Full model with interaction (M1)
data$gender_dummy <- as.numeric(data$gender) - 1 # Male=1, Female=0
model_full <- lm(growth ~ vitamin * gender_dummy, data = data)
print(summary(model_full))

# Extract coefficients
c0 <- model_full$coefficients[[1]]         
c1 <- model_full$coefficients[[2]]         
c2 <- model_full$coefficients[[3]]     
c3 <- model_full$coefficients[[4]]   

# Generate fitted lines
M1_male_line <- (c0 + c2) + (c1 + c3) * grid
M1_female_line <- c0 + c1 * grid

# Plot data with fitted lines
plot(data$vitamin, data$growth,
     pch = data$gender_dummy + 1,
     col = ifelse(data$gender_dummy == 1, "blue", "red"),
     xlab = "vitamin",
     ylab = "growth",
     sub = "Male(=1), Female(=0)")

legend("topleft",
       c("Male", "Female"),
       pch = c(2, 1),
       col = c("blue", "red"),
       lty = c(1, 2),
       bty = "n")

lines(grid, M1_male_line, col = "blue", lty = 1, lwd = 2)
lines(grid, M1_female_line, col = "red", lty = 2, lwd = 2)


# (b) Test for equality of slopes (parallelism test at 5% level)
p_value_interaction <- summary(model_full)$coefficients["vitamin:gender_dummy", "Pr(>|t|)"]
cat(paste("\nParallelism test (interaction term) P-value:", round(p_value_interaction, 4), "\n"))


# Parallel model without interaction (M2)
model_parallel <- lm(growth ~ vitamin + gender_dummy, data = data)
print(summary(model_parallel))

# Extract coefficients
b0_parallel <- model_parallel$coefficients[[1]]          
b1_parallel <- model_parallel$coefficients[[2]]          
b2_parallel <- model_parallel$coefficients[[3]] 

# Create grid for plotting
grid <- seq(min(data$vitamin), max(data$vitamin), length.out = 100)

# Compute fitted lines
M2_male_line <- (b0_parallel + b2_parallel) + b1_parallel * grid
M2_female_line <- b0_parallel + b1_parallel * grid

# Plot results
plot(data$vitamin, data$growth,
     pch = data$gender_dummy + 1,
     col = ifelse(data$gender_dummy == 1, "blue", "red"),
     xlab = "vitamin",
     ylab = "growth",
     sub = "Male(=1), Female(=0)")

legend("topleft",
       c("Male", "Female"),
       pch = c(2, 1),
       col = c("blue", "red"),
       lty = c(1, 2),
       bty = "n")

lines(grid, M2_male_line, col = "blue", lty = 1, lwd = 2)
lines(grid, M2_female_line, col = "red", lty = 2, lwd = 2)


# Pooled model without gender (M3)
model_pooled <- lm(growth ~ vitamin, data = data)
print(summary(model_pooled))


# Compare M2 vs M3
anova_intercept <- anova(model_parallel, model_pooled)
print(anova_intercept)


# Final model comparison
models <- list(
  M1_Full = model_full,
  M2_Parallel = model_parallel,
  M3_Pooled = model_pooled
)

AIC_values <- sapply(models, AIC)
BIC_values <- sapply(models, BIC)
R2_values <- sapply(models, function(m) summary(m)$r.squared)
Adj_R2_values <- sapply(models, function(m) summary(m)$adj.r.squared)

comparison_table <- data.frame(
  Model = names(models),
  R2 = round(R2_values, 4),
  Adj_R2 = round(Adj_R2_values, 4),
  AIC = round(AIC_values, 2),
  BIC = round(BIC_values, 2)
)

print(comparison_table)


#=================================================
#9.4 Dummy Variable Regression Model (Turkey Data)

getwd()
setwd("C:/Users/user/Documents/R_DA")

# Load required libraries
library(readxl)
library(car)
library(corrplot)
library(dplyr)

# Load dataset
data <- read_excel("table9.4_turkey.xlsx") 
data

# Inspect data structure and summary
str(data)
head(data)
summary(data)

# Convert Origin to factor with specified order
data$Origin <- factor(data$Origin, levels = c("W", "G", "V"))

# Descriptive statistics by Origin
summary_stats_turkey <- data %>%
  group_by(Origin) %>%
  summarise(
    N = n(),
    mean_Y = mean(y),
    sd_Y = sd(y),     
    mean_X = mean(x), 
    sd_X = sd(x)     
  )

print(summary_stats_turkey)


# Histogram plots
par(mfrow=c(1, 2))

hist(data$y,
     main = "Weight (Y)",
     xlab = "Weight (pound)",
     ylab = "Frequency",
     col = "lightblue")

hist(data$x,
     main = "Age (X)",
     xlab = "Age (weeks)",
     ylab = "Frequency",
     col = "lightgreen")


# Boxplots by Origin
par(mfrow=c(1, 2))

boxplot(y ~ Origin, data = data,
        main = "Weight by Origin",
        xlab = "Origin",
        ylab = "Weight (Y)",
        col = c("lightgrey", "red", "blue")) # W, G, V order

boxplot(x ~ Origin, data = data,
        main = "Age by Origin",
        xlab = "Origin",
        ylab = "Age (X)",
        col = c("lightgrey", "red", "blue"))

par(mfrow=c(1, 1))


# ---------------------------------------------------------
# M1: Full Model (Different Slopes)
# ---------------------------------------------------------

model_full_turkey <- lm(y ~ x * Origin, data = data)
print(summary(model_full_turkey))

# Create prediction grid
new_data <- expand.grid(
  x = seq(min(data$x), max(data$x), length.out = 100),
  Origin = levels(data$Origin)
)

# Predicted values for M1
new_data$pred_M1 <- predict(model_full_turkey, newdata = new_data)

# Plot observed data
plot(data$x, data$y,
     pch = c(17, 16, 15)[as.numeric(data$Origin)],
     col = c("gray50", "red", "blue")[as.numeric(data$Origin)],
     xlab = "Age (X, weeks)",
     ylab = "Weight (Y, pound)",
     main = "M1: Full Model (Different Slopes)",
     ylim = range(data$y),
     xlim = range(data$x))

# Add fitted lines
for (origin_level in levels(data$Origin)) {
  sub_data <- subset(new_data, Origin == origin_level)
  color_index <- as.numeric(factor(origin_level, levels = levels(data$Origin)))
  
  lines(sub_data$x, sub_data$pred_M1, 
        col = c("gray50", "red", "blue")[color_index], 
        lty = 1, lwd = 2)
}

# Add legend
legend("topleft",
       legend = levels(data$Origin),
       col = c("gray50", "red", "blue"),
       pch = c(17, 16, 15),
       lwd = 2,
       lty = 1,
       bty = "n",
       cex = 0.8)


# ---------------------------------------------------------
# M2: Parallel Model (Equal Slopes)
# ---------------------------------------------------------

model_parallel_turkey <- lm(y ~ x + Origin, data = data)
print(summary(model_parallel_turkey))

# Predicted values for M2
new_data$pred_M2 <- predict(model_parallel_turkey, newdata = new_data)

# Plot observed data
plot(data$x, data$y,
     pch = c(17, 16, 15)[as.numeric(data$Origin)],
     col = c("gray50", "red", "blue")[as.numeric(data$Origin)],
     xlab = "Age (X, weeks)",
     ylab = "Weight (Y, pound)",
     main = "M2: Parallel Model (Equal Slopes)",
     ylim = range(data$y),
     xlim = range(data$x))

# Add fitted lines
for (origin_level in levels(data$Origin)) {
  sub_data <- subset(new_data, Origin == origin_level)
  color_index <- as.numeric(factor(origin_level, levels = levels(data$Origin)))
  
  lines(sub_data$x, sub_data$pred_M2, 
        col = c("gray50", "red", "blue")[color_index],
        lty = 2, lwd = 2)
}

# Add legend
legend("topleft",
       legend = levels(data$Origin),
       col = c("gray50", "red", "blue"),
       pch = c(17, 16, 15),
       lwd = 2,
       lty = 2,
       bty = "n",
       cex = 0.8)


# ---------------------------------------------------------
# M3: Pooled Model (No Group Effect)
# ---------------------------------------------------------

model_pooled_turkey <- lm(y ~ x, data = data)
print(summary(model_pooled_turkey))


# ---------------------------------------------------------
# Model Comparison
# ---------------------------------------------------------

# M1 vs M2 (Test for interaction / slope differences)
anova(model_full_turkey, model_parallel_turkey)

# M2 vs M3 (Test for group effect / intercept differences)
anova(model_parallel_turkey, model_pooled_turkey)


# Diagnostic plots for selected model
par(mfrow=c(2, 2))
plot(model_parallel_turkey)