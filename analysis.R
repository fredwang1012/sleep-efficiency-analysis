#Loading Libraries
library(lubridate)
library(leaps)
# library(rsq)
library(car)

#Data preprocessing
dataset = read.table('Sleep_Efficiency.csv', TRUE, ',')
dataset = na.omit(dataset)

dataset$Smoking.status = as.factor(dataset$Smoking.status)
dataset$Gender = as.factor(dataset$Gender)
dataset$Bedtime = as.POSIXlt(dataset$Bedtime)
dataset$Wakeup.time = as.POSIXlt(dataset$Wakeup.time)

#Turn bedtime into number of hours after 9:00pm
hours = as.numeric(hour(dataset$Bedtime))
hours = as.numeric(hours + minute(dataset$Bedtime)/60)
hours = ifelse(21-hours <= 0, hours - 21, hours + 3)
dataset$Bedtime = hours
dataset$Bedtime

#Turn wake up time into hours since 3:00am
hours = as.numeric(hour(dataset$Wakeup.time))
hours = as.numeric(hours + minute(dataset$Wakeup.time)/60)
hours = hours - 3
dataset$Wakeup.time = hours
dataset$Wakeup.time



#Main model selection
plot(Sleep.efficiency ~ Age, data = dataset, xlab = 'Age', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Gender, data = dataset, xlab = 'Gender', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Sleep.duration, data = dataset, xlab = 'Sleep Duration', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ REM.sleep.percentage, data = dataset, xlab = 'REM Sleep Percentage', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Deep.sleep.percentage, data = dataset, xlab = 'Deep Sleep Percentage', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Light.sleep.percentage, data = dataset, xlab = 'Light Sleep Percentage', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Awakenings, data = dataset, xlab = 'Awakenings', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Caffeine.consumption, data = dataset, xlab = 'Caffeine Consumption', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Alcohol.consumption, data = dataset, xlab = 'Alcohol Consumption', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Smoking.status, data = dataset, xlab = 'Smoking Status', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Exercise.frequency, data = dataset, xlab = 'Exercise Frequency', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Bedtime, data = dataset, xlab = 'Bedtime', ylab = 'Sleep Efficiency')
plot(Sleep.efficiency ~ Wakeup.time, data = dataset, xlab = 'Wakeup Time', ylab = 'Sleep Efficiency')


full_model = lm(Sleep.efficiency ~ Age + Gender + Sleep.duration + REM.sleep.percentage + Deep.sleep.percentage + Light.sleep.percentage + Awakenings + Caffeine.consumption +
                 Alcohol.consumption + Smoking.status + Exercise.frequency + Bedtime + Wakeup.time, data = dataset)
summary(full_model)

resids = full_model$residuals
fitted = full_model$fitted.values

resids_plot = plot(fitted, resids, xlab = 'Fitted Sleep Efficiency', ylab = 'Residuals')
title('Plot of Residuals vs Fitted Sleep Efficiency for Full Model')

# Partial R squared and VIF values for the initial full model
vif_full_dataset <- dataset[-7][-1]
age_pr2_full <- summary(lm(Age ~ ., data = vif_full_dataset))$r.squared
bedtime_pr2_full <- summary(lm(Bedtime ~ ., data = vif_full_dataset))$r.squared
wakeuptime_pr2_full <- summary(lm(Wakeup.time ~ ., data = vif_full_dataset))$r.squared
duration_pr2_full <- summary(lm(Sleep.duration ~ ., data = vif_full_dataset))$r.squared
REM_pr2_full <- summary(lm(REM.sleep.percentage ~ ., data = vif_full_dataset))$r.squared
deep_pr2_full <- summary(lm(Deep.sleep.percentage ~ ., data = vif_full_dataset))$r.squared
light_pr2_full <- summary(lm(Light.sleep.percentage ~ ., data = vif_full_dataset))$r.squared
awakening_pr2_full <- summary(lm(Awakenings ~ ., data = vif_full_dataset))$r.squared
caffeine_pr2_full <- summary(lm(Caffeine.consumption ~ ., data = vif_full_dataset))$r.squared
alcohol_pr2_full <- summary(lm(Alcohol.consumption ~ ., data = vif_full_dataset))$r.squared
exercise_pr2_full <- summary(lm(Exercise.frequency ~ ., data = vif_full_dataset))$r.squared
pr2_full_model <- c(age_pr2_full, bedtime_pr2_full, wakeuptime_pr2_full, duration_pr2_full, REM_pr2_full,
                    deep_pr2_full, light_pr2_full, awakening_pr2_full, caffeine_pr2_full, alcohol_pr2_full,
                    exercise_pr2_full)
vif_full_model <- 1 / (1 - pr2_full_model)


#Backwards Selection
ver2 = lm(Sleep.efficiency ~ Age + Sleep.duration + REM.sleep.percentage + Deep.sleep.percentage + Light.sleep.percentage + Awakenings + Caffeine.consumption +
            Alcohol.consumption + Smoking.status + Exercise.frequency + Bedtime + Wakeup.time, data = dataset)
summary(ver2)

ver3 = lm(Sleep.efficiency ~ Age + Sleep.duration + REM.sleep.percentage + Deep.sleep.percentage + Light.sleep.percentage + Awakenings + Caffeine.consumption +
            Alcohol.consumption + Smoking.status + Exercise.frequency + Wakeup.time, data = dataset)
summary(ver3)

ver4 = lm(Sleep.efficiency ~ Age + Sleep.duration + REM.sleep.percentage + Deep.sleep.percentage + Light.sleep.percentage + Awakenings + Caffeine.consumption +
            Alcohol.consumption + Smoking.status + Exercise.frequency, data = dataset)
summary(ver4)

ver5 = lm(Sleep.efficiency ~ Age + REM.sleep.percentage + Deep.sleep.percentage + Light.sleep.percentage + Awakenings + Caffeine.consumption +
            Alcohol.consumption + Smoking.status + Exercise.frequency, data = dataset)
summary(ver5)


#To choose from REM, deep, light sleep percentage
mod_sub = regsubsets(Sleep.efficiency ~., data = dataset, method = 'exhaustive')
ss = summary(mod_sub)
ss$which
ss$adjr2
ss$cp
ss$rsq

plot(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9), ss$adjr2, xlab = 'Number of Parameters', ylab = "Adjusted R2")
title("Adjusted R2 for Model Selection")

plot(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9), ss$cp, xlab = 'Number of Parameters', ylab = "Mallow's Cp")
abline(0, 1)
title("Mallow's Cp Plot for Model Selection")


optimized = lm(Sleep.efficiency ~ Age + Light.sleep.percentage + Awakenings + Caffeine.consumption +
            Alcohol.consumption + Smoking.status + Exercise.frequency, data = dataset)
summary(optimized)

optimized_resids = optimized$residuals
optimized_fitted = optimized$fitted.values

resids_plot = plot(optimized_fitted, optimized_resids, xlab = 'Fitted Sleep Efficiency', ylab = 'Residuals')
title('Plot of Residuals vs Fitted Sleep Efficiency for Optimized Model')


qq_plot = qqnorm(optimized_resids, ylab = 'Optimized Model Residuals Sample Quantiles')
qqline(optimized_resids)



# Partial R squared and VIF values for the intermediate model without interaction
vif_int_dataset <- dataset[-9][-8][-7][-6][-5][-4][-3][-1]
age_pr2_int <- summary(lm(Age ~ ., data = vif_int_dataset))$r.squared
light_pr2_int <- summary(lm(Light.sleep.percentage ~ ., data = vif_int_dataset))$r.squared
awakening_pr2_int <- summary(lm(Awakenings ~ ., data = vif_int_dataset))$r.squared
caffeine_pr2_int <- summary(lm(Caffeine.consumption ~ ., data = vif_int_dataset))$r.squared
alcohol_pr2_int <- summary(lm(Alcohol.consumption ~ ., data = vif_int_dataset))$r.squared
exercise_pr2_int <- summary(lm(Exercise.frequency ~ ., data = vif_int_dataset))$r.squared
pr2_int_model <- c(age_pr2_int, light_pr2_int, awakening_pr2_int, caffeine_pr2_int,
                   alcohol_pr2_int, exercise_pr2_int)
vif_int_model <- 1 / (1 - pr2_int_model)

# #Sample calculation for partial R squared value of r^2 y,age:x'
# mod_noage = lm(Sleep.efficiency ~ Light.sleep.percentage + Awakenings + Caffeine.consumption +
#                  Alcohol.consumption + Smoking.status + Exercise.frequency, data = dataset)
# optimized_ageprsq = (summary(optimized)$r.squared - summary(mod_noage)$r.squared) / (1 - summary(mod_noage)$r.squared)

#Sample calculation for VIF of age
mod_justage = lm(Age ~ Light.sleep.percentage + Awakenings + Caffeine.consumption +
                   Alcohol.consumption + Smoking.status + Exercise.frequency, data = dataset)
optimized_agevif = 1 / (1 - summary(mod_justage)$r.squared)



## Optimized model including interaction terms
optimized_int = lm(Sleep.efficiency ~ Age + Light.sleep.percentage + Awakenings + Caffeine.consumption +
                     Alcohol.consumption + Smoking.status + Exercise.frequency + 
                     Awakenings:Alcohol.consumption + Age:Light.sleep.percentage, data = dataset)
summary(optimized_int)

optimized_int_resids = optimized_int$residuals
optimized_int_fitted = optimized_int$fitted.values

resids_int_plot = plot(optimized_int_fitted, optimized_int_resids, xlab = 'Fitted Sleep Efficiency', ylab = 'Residuals')
title('Plot of Residuals vs Fitted Sleep Efficiency for Optimized Model with Interactions')

qq_int_plot = qqnorm(optimized_int_resids, ylab = 'Optimized Model With Interactions Residuals Sample Quantiles')
qqline(optimized_int_resids)


#Leverage and Outlier Checks
leverages <- as.data.frame(hatvalues(optimized_int))
cookdstes <- as.data.frame(cooks.distance(optimized_int))
standard_res <- rstandard(optimized_int)


