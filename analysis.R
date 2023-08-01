dataset = read.table('Sleep_Efficiency.csv', TRUE, ',')
dataset = na.omit(dataset)
dataset

dataset$Smoking.status = as.factor(dataset$Smoking.status)
dataset

scuffed = lm(Sleep.efficiency ~ ., data = dataset)
summary(scuffed)