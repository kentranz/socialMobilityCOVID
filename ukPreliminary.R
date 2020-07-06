


setwd('/Users/kentran/Documents/PhD/senMovement')
source('socialMobilityCOVID/000.utils.R')
source('socialMobilityCOVID/0.import.R')
source('socialMobilityCOVID/000.olsCoeff95CI.R')

google <- read.csv('data/Global_Mobility_Report.csv')
uk <- read.csv('data/UKCovid.csv') %>%
  select(Area.name, Specimen.date, Daily.lab.confirmed.cases) %>%
  rename(date = Specimen.date
         , newCases = Daily.lab.confirmed.cases
         ) %>%
  mutate(date = as.Date(date)) %>%
  filter(date <= as.Date('2020-06-20')) %>%
  arrange(Area.name, date) %>%
  
  group_by(Area.name) %>%
  mutate(casesTminus1 = lag(newCases)
         , casesTminus2 = lag(casesTminus1)) %>%
  ungroup() %>%
  
  replace_na(list(newCases = 0
                  , casesTminus1 = 0
                  , casesTminus2 = 0)) %>%
  # ADD HOLIDAY
  mutate(anomalousWeekend = 
           case_when(
             date %in% as.Date(c('2020-03-12', '2020-03-13'
                                 , '2020-03-14', '2020-03-15'
             )) ~ 1
             , TRUE ~ 0
           )
         
         , longWeekend = 
           case_when( date %in% as.Date(c(
               '2020-04-10', '2020-04-11', '2020-04-12', '2020-04-13'
               , '2020-05-08', '2020-05-09', '2020-05-10'
               , '2020-05-23', '2020-05-24', '2020-05-25'
             ))
             ~ 1
             
             , TRUE ~ 0
             
           )

         , weekend = case_when(
           lubridate::wday(date, label = TRUE) %in% c('Sat', 'Sun') ~ 1
           , TRUE ~ 0
         )
  )


###################################
# Train + Test Split
###################################

trainAll <- uk %>% 
  filter(date >= as.Date('2020-03-01') 
         #       &  date <= as.Date('2020-04-30'))
         & date <= as.Date('2020-06-05'))
# group_by(city) %>%
# mutate(t = row_number()) %>%
# ungroup() %>%
# filter(t <= 60) # HOW MANY DAYS TO CUT

trainAll %>% group_by(Area.name) %>% summarize(lastday = max(date))

testAll <- uk %>% 
  #filter(date >= as.Date('2020-05-01') & date <= as.Date('2020-05-16'))
  filter(date >= as.Date('2020-06-06') & date <= as.Date('2020-06-20'))

write.csv(all <- uk %>% filter(date >= as.Date('2020-03-01') 
                                 & date <= as.Date('2020-06-20'))
          , file = "socialMobilityCOVID/data/ukPrelim/all.csv"
          , row.names = F)
write.csv(trainAll
          , file = "socialMobilityCOVID/data/ukPrelim/trainAll.csv"
          , row.names = F)
write.csv(testAll
          , file = "socialMobilityCOVID/data/ukPrelim/testAll.csv"
          , row.names = F)




regions <- c('South West', 'South East', 'London', 'East of England', 
             'West Midlands', 'East Midlands', 'Yorkshire and The Humber'
             , 'North West', 'North East')


FULL_Vars <- 'anomalousWeekend + longWeekend + weekend + casesTminus1 + casesTminus2'

for (i in 1:length(unique(regions)))
{
  indexCity <- unique(regions)[i]
  
  print('################################')
  print(indexCity)
  
  train <- trainAll %>% filter(Area.name == indexCity)
  test <- testAll %>% filter(Area.name == indexCity)
  
  print(paste0('Train: ', as.character(min(train$date))
               , ' to ', as.character(max(train$date))))
  print(paste0('Test: ', as.character(min(test$date))
               , ' to ', as.character(max(test$date))))
  
  ################### FULL MODEL
  Full <- fittedOLS('newCases'
                    , input = FULL_Vars
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
                    , 'ukPrelim'
                    , '/FullOLS'
                    , gsub('\\s+', '', indexCity)
                    , '.csv')
    , row.names = F
  )
  
  print(Full$coeffs)
  print('')
  
  #################### PLOT
  
  plotDF <- data.frame(c(train$date, test$date)) %>%
    rename(date = everything()) %>%
    cbind(c(train$newCases, test$newCases)) %>%
    rename(Actual = names(.)[2]) %>%
    
    # cbind(c(predict(SM_Only$model, newdata = train)
    #         , predict(SM_Only$model, newdata = test))) %>%
    # rename(SM_Only = names(.)[3]) %>%
    
    cbind(c(predict(Full$model, newdata = train)
            , predict(Full$model, newdata = test))) %>%
    rename(Full = names(.)[3]) %>%
    
    melt(id = 'date') %>%
    mutate(forecast = case_when(
      date >= as.Date('2020-06-06') & variable != 'Actual' ~ 1
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
                , 'ukPrelim'
                , '/FullOLS'
                , gsub('\\s+', '', indexCity)
                , ".png")
         , width=8, height=4)
  
  
}

#####################################################
# UK + GOOGLE transit, retail, work, and residential 
#####################################################

google %<>% filter(country_region_code == 'GB')


uk %<>% filter(Area.name == 'England')
  




