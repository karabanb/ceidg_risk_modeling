
library(h2o)

write.csv(cleaned_data, 'data/100_CleanedData.csv', row.names = FALSE)

h2o.init()

cleaned_data <- h2o.importFile('data/100_CleanedData.csv')

h2o.describe(cleaned_data)

to_factors <- c('PKDMainDivision', 'PKDMainGroup', 'PKDMainClass', 'QuarterOfStartingOfTheBusiness')
cleaned_data[, to_factors] <- as.factor(cleaned_data[ , to_factors])

df_split <- h2o.splitFrame(cleaned_data, ratios = 0.8, seed = 42)

train <- df_split[[1]]
test <- df_split[[2]]

target = 'Target'

aml <- h2o.automl(y = target,
                  training_frame = train,
                  validation_frame = test,
                  max_runtime_secs = 60, 
                  seed = 42)


