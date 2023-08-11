#Loading Libraries
library(lubridate)
library(leaps)

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



alcohol_drinkers <- subset(dataset, Alcohol.consumption > 0)
non_alcohol_drinkers <- subset(dataset, Alcohol.consumption == 0)

mean(alcohol_drinkers$Sleep.efficiency)
mean(non_alcohol_drinkers$Sleep.efficiency)

nrow(subset(dataset, Caffeine.consumption <= 50)) / nrow(dataset)

mean(dataset$Exercise.frequency)

proportions(table(alcohol_drinkers$Smoking.status))
proportions(table(non_alcohol_drinkers$Smoking.status))




# #Other interesting models
# age_vs_efficiency = lm(Sleep.efficiency ~ Age, data = dataset)
# summary(age_vs_efficiency)
# plot(dataset$Age, dataset$Sleep.efficiency)
# plot(age_vs_efficiency$fitted.values, age_vs_efficiency$residuals)
# 
# age_vs_duration = lm(Sleep.duration ~ Age, data = dataset)
# summary(age_vs_duration)
# plot(dataset$Age, dataset$Sleep.duration)
# plot(age_vs_duration$fitted.values, age_vs_duration$residuals)
# 
# duration_vs_efficiency = lm(Sleep.efficiency ~ Sleep.duration, data = dataset)
# summary(duration_vs_efficiency)
# plot(dataset$Sleep.duration, dataset$Sleep.efficiency)
# plot(duration_vs_efficiency$fitted.values, duration_vs_efficiency$residuals)
