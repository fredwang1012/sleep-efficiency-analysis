dataset = read.table('Sleep_Efficiency.csv', TRUE, ',')
dataset = na.omit(dataset)

dataset$Smoking.status = as.factor(dataset$Smoking.status)
dataset$Gender = as.factor(dataset$Gender)

dataset

scuffed = lm(Sleep.efficiency ~ Age + Caffeine.consumption + 
               Alcohol.consumption + Smoking.status + Exercise.frequency + Awakenings + 
               Deep.sleep.percentage, data = dataset)
summary(scuffed)

resids = scuffed$residuals
fitted = scuffed$fitted.values

resids_plot = plot(fitted, resids)

qq_plot = qqnorm(resids)
qqline(resids)


age_vs_efficiency = lm(Sleep.efficiency ~ Age, data = dataset)
summary(age_vs_efficiency)

age_vs_duration = lm(Sleep.duration ~ Age, data = dataset)
summary(age_vs_duration)


duration_vs_efficiency = lm(Sleep.efficiency ~ Sleep.duration, data = dataset)
summary(duration_vs_efficiency)