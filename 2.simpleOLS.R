

setwd('/Users/kentran/Documents/PhD/senMovement')
source('socialMobilityCOVID/000.utils.R')
source('socialMobilityCOVID/0.import.R')
source('socialMobilityCOVID/000.olsCoeff95CI.R')
source('socialMobilityCOVID/1.dataPrep.R')


###################################
# Train + Test Split
###################################

trainAll <- data %>% 
  filter(date >= as.Date('2020-02-15') 
         &  date <= as.Date('2020-04-30'))
  # group_by(city) %>%
  # mutate(t = row_number()) %>%
  # ungroup() %>%
  # filter(t <= 60) # HOW MANY DAYS TO CUT

trainAll %>% group_by(city) %>% summarize(lastday = max(date))

testAll <- data %>% 
  filter(date >= as.Date('2020-05-01') & date <= as.Date('2020-05-16'))
  

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
              , input =#'driving + walking + transit + holidayWeekend + marchBreak + casesTminus1 + casesTminus2'
                'driving + walking + transit'
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
    , file = paste0('results/SM_onlyOLS', indexCity, '.csv')
    , row.names = F
  )
  
  print(SM_Only$coeffs)
  print('')
  
  ################### FULL MODEL
  Full <- fittedOLS('newCases'
                       , input = 'driving + walking + transit + holidayWeekend + marchBreak + casesTminus1 + casesTminus2'
                       , fitData = train)
  
  
  
  round(cor(test$newCases, predict(Full$model, newdata = test))^2, 3)
  
  Full$coeffs[nrow(Full$coeffs)+1, 'Vars'] <- 'R2 Test'
  Full$coeffs[nrow(Full$coeffs), 'Estimate'] <- as.character(round(cor(test$newCases, predict(Full$model, newdata = test))^2, 3))
  
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
    , file = paste0('results/FullOLS', indexCity, '.csv')
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
      date >= as.Date('2020-05-01') & variable != 'Actual' ~ 1
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
  
 
  
  ggsave(paste0('results/',indexCity, ".png")
         , width=8, height=4)
          
  
}





