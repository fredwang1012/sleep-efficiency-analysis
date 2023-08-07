
#Data preprocessing
dataset = read.table('Sleep_Efficiency.csv', TRUE, ',')
dataset = na.omit(dataset)

dataset$Smoking.status = as.factor(dataset$Smoking.status)
dataset$Gender = as.factor(dataset$Gender)

dataset


#Main model selection
full_model = lm(Sleep.efficiency ~ Age + Gender + Sleep.duration + REM.sleep.percentage + Deep.sleep.percentage + Light.sleep.percentage + Awakenings + Caffeine.consumption +
                 Alcohol.consumption + Smoking.status + Exercise.frequency, data = dataset)
summary(full_model)

resids = full_model$residuals
fitted = full_model$fitted.values

resids_plot = plot(fitted, resids)



optimized = lm(Sleep.efficiency ~ Age + REM.sleep.percentage + Deep.sleep.percentage + Awakenings + Caffeine.consumption +
            Alcohol.consumption + Smoking.status + Exercise.frequency, data = dataset)
summary(optimized)

resids = optimized$residuals
fitted = optimized$fitted.values

resids_plot = plot(fitted, resids)

qq_plot = qqnorm(resids)
qqline(resids)


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