

setwd('/Users/kentran/Documents/PhD/senMovement')
source('socialMobilityCOVID/000.utils.R')
#source('socialMobilityCOVID/000.stockholm.R') # API call to pull stockholm data
source('socialMobilityCOVID/0.import.R')
source('socialMobilityCOVID/000.olsCoeff95CI.R')
source('socialMobilityCOVID/1.dataPrep.R')


temp <- data %>% group_by(city) %>%
  summarize(#nCity = n()
            population = unique(Population))

lag = c('7', '8', '9', '10', '14', '78', '89', '910', '714')

SM_Only_Vars <- c(
   'drivingMinus7 + walkingMinus7 + transitMinus7'
  , 'drivingMinus8 + walkingMinus8 + transitMinus8'
  , 'drivingMinus9 + walkingMinus9 + transitMinus9'
  , 'drivingMinus10 + walkingMinus10 + transitMinus10'
  , 'drivingMinus14 + walkingMinus14 + transitMinus14'
  , 'drivingMinus7 + walkingMinus7 + transitMinus7 + drivingMinus8 + walkingMinus8 + transitMinus8'
  , 'drivingMinus8 + walkingMinus8 + transitMinus8 + drivingMinus9 + walkingMinus9 + transitMinus9'
  , 'drivingMinus9 + walkingMinus9 + transitMinus9 + drivingMinus10 + walkingMinus10 + transitMinus10'
  , 'sumDrivingMinus7_14 + sumWalkingMinus7_14 + sumTransitMinus7_14'
)

FULL_Vars <-  c(
   'drivingMinus7 + walkingMinus7 + transitMinus7 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
  , 'drivingMinus8 + walkingMinus8 + transitMinus8 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
  , 'drivingMinus9 + walkingMinus9 + transitMinus9 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
  , 'drivingMinus10 + walkingMinus10 + transitMinus10 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
  , 'drivingMinus14 + walkingMinus14 + transitMinus14 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
  , 'drivingMinus7 + walkingMinus7 + transitMinus7 + drivingMinus8 + walkingMinus8 + transitMinus8 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
  , 'drivingMinus8 + walkingMinus8 + transitMinus8 + drivingMinus9 + walkingMinus9 + transitMinus9 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
  , 'drivingMinus9 + walkingMinus9 + transitMinus9 + drivingMinus10 + walkingMinus10 + transitMinus10 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
  , 'sumDrivingMinus7_14 + sumWalkingMinus7_14 + sumTransitMinus7_14 + anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'
)

###################################
# Train + Test Split
###################################

startDate <- '2020-03-01'
endDate <- '2020-11-15'
trainSplitDate <- '2020-11-01'

trainAll <- data %>% 
  filter(date >= as.Date('2020-03-01') 
  #       &  date <= as.Date('2020-04-30'))
          & date <= (as.Date(trainSplitDate)-1))
  # group_by(city) %>%
  # mutate(t = row_number()) %>%
  # ungroup() %>%
  # filter(t <= 60) # HOW MANY DAYS TO CUT

trainAll %>% group_by(city) %>% summarize(firstday = min(date)) %>% as.data.frame()
trainAll %>% group_by(city) %>% summarize(lastday = max(date)) %>% as.data.frame()

testAll <- data %>% 
  #filter(date >= as.Date('2020-05-01') & date <= as.Date('2020-05-16'))
  filter(date >= as.Date(trainSplitDate) & date <= as.Date(endDate))

write.csv(all <- data %>% filter(date >= as.Date(startDate) 
                                 & date <= as.Date(endDate))
          , file = "socialMobilityCOVID/data/all.csv"
          , row.names = F)
write.csv(trainAll
          , file = "socialMobilityCOVID/data/trainAll.csv"
          , row.names = F)
write.csv(testAll
          , file = "socialMobilityCOVID/data/testAll.csv"
          , row.names = F)



for(j in 1:length(lag))
{

  dir.create(paste0('results/'
                    , 'appleMobilityLag'
             , lag[j]))

for (i in 1:length(unique(data$city)))
{
  indexCity <- unique(data$city)[i]
  
  print('################################')
  print(indexCity)
  
  train <- trainAll %>% filter(city == indexCity)
  test <- testAll %>% filter(city == indexCity)
  
  print(paste0('Train: ', as.character(min(train$date))
               , ' to ', as.character(max(train$date))))
  print(paste0('Test: ', as.character(min(test$date))
               , ' to ', as.character(max(test$date))))
  
  ################### APPLE DATA ONLY
  SM_Only <- fittedOLS('newCases'
              , input = SM_Only_Vars[j]
              , fitData = train)
                       
  
  
  round(cor(test$newCases, predict(SM_Only$model, newdata = test))^2, 3)
  
  SM_Only$coeffs[nrow(SM_Only$coeffs)+1, 'Vars'] <- 'R2 Test'
  SM_Only$coeffs[nrow(SM_Only$coeffs), 'Estimate'] <- as.character(round(cor(test$newCases, predict(SM_Only$model, newdata = test))^2, 3))
  
  SM_Only$coeffs[nrow(SM_Only$coeffs)+1, 'Vars'] <- 'RMSE Train'
  SM_Only$coeffs[nrow(SM_Only$coeffs), 'Estimate'] <- 
    as.character(round(RMSE(predict(SM_Only$model), train$newCases), 3))
  
  SM_Only$coeffs[nrow(SM_Only$coeffs)+1, 'Vars'] <- 'RMSE Test'
  SM_Only$coeffs[nrow(SM_Only$coeffs), 'Estimate'] <- as.character(round(RMSE(predict(SM_Only$model, newdata = test), test$newCases), 3))
  
  SM_Only$coeffs[nrow(SM_Only$coeffs)+1, 'Vars'] <- 'RMSPE Train'
  SM_Only$coeffs[nrow(SM_Only$coeffs), 'Estimate'] <- as.character(round( MLmetrics::RMSPE(predict(SM_Only$model), train$newCases), 3))
  
  SM_Only$coeffs[nrow(SM_Only$coeffs)+1, 'Vars'] <- 'RMSPE Test'
  SM_Only$coeffs[nrow(SM_Only$coeffs), 'Estimate'] <- as.character(round( MLmetrics::RMSPE(predict(SM_Only$model, newdata = test), test$newCases), 3))
  
  write.csv(
    SM_Only$coeffs
    , file = paste0('results/'
                    , 'appleMobilityLag'
                    , lag[j]
                    , '/SM_onlyOLS', indexCity, '.csv')
    , row.names = F
  )
  
  print(SM_Only$coeffs)
  print('')
  
  ################### FULL MODEL
  Full <- fittedOLS('newCases'
                       , input = FULL_Vars[j]
                       , fitData = train)
  
  
  
  round(cor(test$newCases, predict(Full$model, newdata = test))^2, 3)
  
  Full$coeffs[nrow(Full$coeffs)+1, 'Vars'] <- 'R2 Test'
  Full$coeffs[nrow(Full$coeffs), 'Estimate'] <- 
    as.character(round(cor(test$newCases, predict(Full$model, newdata = test))^2, 3))
  
  Full$coeffs[nrow(Full$coeffs)+1, 'Vars'] <- 'RMSE Train'
  Full$coeffs[nrow(Full$coeffs), 'Estimate'] <- 
    as.character(round(RMSE(predict(Full$model), train$newCases), 3))
  
  Full$coeffs[nrow(Full$coeffs)+1, 'Vars'] <- 'RMSE Test'
  Full$coeffs[nrow(Full$coeffs), 'Estimate'] <- as.character(round(RMSE(predict(Full$model, newdata = test), test$newCases), 3))
  
  Full$coeffs[nrow(Full$coeffs)+1, 'Vars'] <- 'RMSPE Train'
  Full$coeffs[nrow(Full$coeffs), 'Estimate'] <- as.character(round( MLmetrics::RMSPE(predict(Full$model), train$newCases), 3))
  
  Full$coeffs[nrow(Full$coeffs)+1, 'Vars'] <- 'RMSPE Test'
  Full$coeffs[nrow(Full$coeffs), 'Estimate'] <- as.character(round( MLmetrics::RMSPE(predict(Full$model, newdata = test), test$newCases), 3))
  
  write.csv(
    Full$coeffs
    , file = paste0('results/'
                    , 'appleMobilityLag'
                    , lag[j]
                    , '/FullOLS', indexCity, '.csv')
    , row.names = F
  )
  
  print(Full$coeffs)
  print('')
  
  #################### PLOT

  plotDF <- data.frame(c(train$date, test$date)) %>%
    rename(date = everything()) %>%
    cbind(c(train$newCases, test$newCases)) %>%
    rename(Actual = names(.)[2]) %>%
    
    cbind(c(predict(SM_Only$model, newdata = train)
            , predict(SM_Only$model, newdata = test))) %>%
    rename(SM_Only = names(.)[3]) %>%
    
    cbind(c(predict(Full$model, newdata = train)
            , predict(Full$model, newdata = test))) %>%
    rename(Full = names(.)[4]) %>%
    
    melt(id = 'date') %>%
    mutate(forecast = case_when(
      date >= as.Date(trainSplitDate) & variable != 'Actual' ~ 1
      , TRUE ~ 0)
      )
  


  ggplot(plotDF, aes(date, value, group = variable, colour = variable)) + 
    geom_line(data = filter(plotDF, forecast == 0), linetype = "solid") + 
    geom_line(data = filter(plotDF, forecast == 1), linetype = "dashed") +
    scale_color_manual(values=c("gray", "red", "green")) +  
    ylab('Daily Cases') +
    xlab('date') +
    ggtitle(indexCity) +
    theme_classic() +
    theme(legend.title=element_blank()) 
  
 
  
  ggsave(paste0('results/'
                , 'appleMobilityLag'
                , lag[j]
                ,'/'
                , indexCity, ".png")
         , width=8, height=4)
          
  
}

# source('socialMobilityCOVID/2.usVsOthers.R')




}