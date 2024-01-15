# Creating a contingency table
observed_data <- matrix(c(24, 37, 20, 10, 24, 106, 40, 24), ncol = 2)
rownames(observed_data) <- c("Red", "White", "Black", "Blue")
colnames(observed_data) <- c("Exceeds Limit", "Obeys Limit")

# Performing chi-squared test
chi_squared_test <- chisq.test(observed_data)
chi_squared_test

##
# Fit a linear regression model
model <- lm(log.radius ~ log.parent.radius, data = moon_size)

# Check regression results
summary(model)
#####
# Load the data
lf_data <- read.csv("L_fictitious.csv")

# Logistic regression for survival
survival_model <- glm(survived ~ generations, data = lf_data, family = "binomial")

# Display results
summary(survival_model)


####
# Predict survival probability for a pair with 5 generations in captivity
new_data <- data.frame(generations = 5)
predicted_survival <- predict(survival_model, newdata = new_data, type = "response")

# Display the prediction
predicted_survival


####
# Subset the data for pairs that survived
survived_data <- subset(lf_data, survived == 1)

# Logistic regression for breeding success
breeding_model <- glm(bred ~ generations, data = survived_data, family = "binomial")

# Display results
summary(breeding_model)


####
# Fit linear model
model_eggshell <- lm(thickness ~ PBDE + population, data = eggshell)

# Display results
summary(model_eggshell)

##
# Independent t-test
t_test_result <- t.test(thickness ~ population, data = eggshell)

# Display results
t_test_result

####
# Fit linear model with interaction term
model_interaction <- lm(thickness ~ PBDE * population, data = eggshell)

# Display results
summary(model_interaction)


####
# Predict eggshell thickness for a given PBDE concentration in each population
new_data <- data.frame(PBDE = 5.0, population = c("NY", "VT"))
predicted_thickness <- predict(model_eggshell, new_data, interval = "confidence")

# Display predicted thickness
predicted_thickness




