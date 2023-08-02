dataset = read.table('Sleep_Efficiency.csv', TRUE, ',')
dataset = na.omit(dataset)

dataset$Smoking.status = as.factor(dataset$Smoking.status)
dataset$Gender = as.factor(dataset$Gender)
dataset$Awakenings = as.factor(dataset$Awakenings)
dataset$Exercise.frequency = as.factor(dataset$Exercise.frequency)

dataset

scuffed = lm(Sleep.efficiency ~ Age + Caffeine.consumption + 
               Alcohol.consumption + Smoking.status + Exercise.frequency + Awakenings + 
               Deep.sleep.percentage, data = dataset)
summary(scuffed)

resids = scuffed$residuals

resids_plot = plot(dataset$Sleep.efficiency, resids)

qq_plot = qqnorm(resids)