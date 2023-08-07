#Loading Libraries
library(lubridate)


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
ifelse(21-hours <= 0, hours - 21, hours + 3)
dataset$Bedtime = hours
dataset$Bedtime

#Turn wake up time into hours since 3:00am
hours = as.numeric(hour(dataset$Wakeup.time))
hours = as.numeric(hours + minute(dataset$Wakeup.time)/60)
hours = hours - 3
dataset$Wakeup.time = hours
dataset$Wakeup.time



#Main model selection
full_model = lm(Sleep.efficiency ~ Age + Gender + Sleep.duration + REM.sleep.percentage + Deep.sleep.percentage + Light.sleep.percentage + Awakenings + Caffeine.consumption +
                 Alcohol.consumption + Smoking.status + Exercise.frequency + Bedtime + Wakeup.time, data = dataset)
summary(full_model)

resids = full_model$residuals
fitted = full_model$fitted.values

resids_plot = plot(fitted, resids, xlab = 'Fitted Sleep Efficiency', ylab = 'Residuals')
title('Plot of Residuals vs Fitted Sleep Efficiency for Full Model')



optimized = lm(Sleep.efficiency ~ Age + REM.sleep.percentage + Deep.sleep.percentage + Awakenings + Caffeine.consumption +
            Alcohol.consumption + Smoking.status + Exercise.frequency, data = dataset)
summary(optimized)

optimized_resids = optimized$residuals
optimized_fitted = optimized$fitted.values

resids_plot = plot(optimized_fitted, optimized_resids, xlab = 'Fitted Sleep Efficiency', ylab = 'Residuals')
title('Plot of Residuals vs Fitted Sleep Efficiency for Optimized Model')


qq_plot = qqnorm(optimized_resids, ylab = 'Optimized Model Residuals Sample Quantiles')
qqline(optimized_resids)


#Other relevant models
age_vs_efficiency = lm(Sleep.efficiency ~ Age, data = dataset)
summary(age_vs_efficiency)
plot(dataset$Age, dataset$Sleep.efficiency)
plot(age_vs_efficiency$fitted.values, age_vs_efficiency$residuals)

age_vs_duration = lm(Sleep.duration ~ Age, data = dataset)
summary(age_vs_duration)
plot(dataset$Age, dataset$Sleep.duration)
plot(age_vs_duration$fitted.values, age_vs_duration$residuals)

duration_vs_efficiency = lm(Sleep.efficiency ~ Sleep.duration, data = dataset)
summary(duration_vs_efficiency)
plot(dataset$Sleep.duration, dataset$Sleep.efficiency)
plot(duration_vs_efficiency$fitted.values, duration_vs_efficiency$residuals)