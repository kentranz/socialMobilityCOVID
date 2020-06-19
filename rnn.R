
setwd('/Users/kentran/Documents/PhD/senMovement')
source('socialMobilityCOVID/000.utils.R')
source('socialMobilityCOVID/0.import.R')
source('socialMobilityCOVID/000.olsCoeff95CI.R')
source('socialMobilityCOVID/1.dataPrep.R')

library(keras)
library(tensorflow)

trainAll <- data %>% 
  filter(date >= as.Date('2020-02-15') 
         &  date <= as.Date('2020-04-30')) %>%
  select(driving, walking, transit, holidayWeekend, casesTminus1, casesTminus2) %>%
  as.matrix()

trainAllLabels <- data %>% 
  filter(date >= as.Date('2020-02-15') 
         &  date <= as.Date('2020-04-30')) %>%
  select(newCases) %>%
  as.matrix()

testAll <- data %>% 
  filter(date >= as.Date('2020-05-01') & date <= as.Date('2020-05-16')) %>%
  select(driving, walking, transit, holidayWeekend, casesTminus1, casesTminus2) %>%
  as.matrix()
testAllLabels <- data %>% 
  filter(date >= as.Date('2020-05-01') & date <= as.Date('2020-05-16')) %>%
  select(newCases) %>%
  as.matrix()


model <- keras_model_sequential() %>% 
  layer_gru(units = 2
            , dropout = 0.2
            , recurrent_dropout = 0.2
            , input_shape = list(NULL, dim(trainAll)[[-1]])
            ) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

model %>% fit(
  trainAll
  , trainAllLabels
  , epochs = 10
  , batch_size = 32
  , validation_data = list(testAll, testAllLabels)
)



