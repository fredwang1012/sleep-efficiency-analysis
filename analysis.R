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