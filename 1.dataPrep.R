###################################
# PREP DATAFRAME TO USE FOR MODEL DEVELOPMENT
###################################


###################################
# WHICH CITIES ARE INCLUDED?
###################################

cities <- c('New York', 'Chicago'
            ,'Seattle', 'Tampa'
            , 'Baltimore', 'Cleveland'
            , 'Sacramento', 'Indianapolis'
            , 'New Orleans', 'Oklahoma' 
            , 'Los Angeles', 'Denver', 'Boston', 'Pittsburgh', 'Memphis'
            , 'Toronto', 'Montreal'
            , 'Stockholm'
            , 'London'
            )

###################################
# PREPING DATA FOR MODEL BUIDLING
###################################
# SUBSET APPLE DATA
mobility <- apple %>% 
  mutate(region = str_replace(region, ' City', '')) %>%
  filter(geo_type == 'city'
         & region %in% cities) %>%
  select(region, transportation_type, starts_with('X')) %>%
  melt(id.vars = c('region', 'transportation_type')
  ) %>%
  rename(date = variable) %>%
  dcast(region + date ~ transportation_type) %>%
  mutate(date = str_replace(date, 'X', '')
         , date = str_replace_all(date, '\\.', '-')
         , date = as.Date(date)
         , dateMinus7 = date + 7) %>% # MOBILITY OF t-n days
  
  group_by(region) %>%
  mutate(driving = imputeTS::na_seadec(driving, "interpolation")
         , transit = imputeTS::na_seadec(transit, "interpolation")
         , walking = imputeTS::na_seadec(walking, "interpolation")) %>%
  ungroup() %>%
  
    # ADD HOLIDAY
  mutate(anomalousWeekend = 
           case_when(
             date %in% as.Date(c('2020-03-12', '2020-03-13', '2020-03-14', '2020-03-15'
             )) ~ 1
             , TRUE ~ 0
           )
    
    , longWeekend = 
           case_when(
             !region %in% c('Toronto', 'Montreal'
                            , 'Stockholm', 'London') == T 
             & date %in% as.Date(c(
               '2020-02-15', '2020-02-16', '2020-02-17'
               , '2020-04-10', '2020-04-11', '2020-04-12', '2020-04-13'
               , '2020-05-23', '2020-05-24', '2020-05-25'))
             ~ 1
             
              , region %in% c('Toronto') == T
              & date %in% as.Date(c(
                        '2020-02-15', '2020-02-16', '2020-02-17'
                        , '2020-05-16', '2020-05-17', '2020-05-18' 
                        ))
              ~ 1
             , region %in% c('Montreal') == T
             & date %in% as.Date(c(
             '2020-04-10', '2020-04-11', '2020-04-12', '2020-04-13'
             , '2020-05-16', '2020-05-17', '2020-05-18'
             ))
             ~ 1
             
             , region %in% c('Stockholm') == T
             & date %in% as.Date(c(
               '2020-04-10', '2020-04-11', '2020-04-12', '2020-04-13'
               , '2020-05-01', '2020-05-02', '2020-05-03'
               , '2020-05-21'
               , '2020-06-01'
             ))
             ~ 1
             
             , region %in% c('London') == T
             & date %in% as.Date(c(
               '2020-04-10', '2020-04-11', '2020-04-12', '2020-04-13'
               , '2020-05-08', '2020-05-09', '2020-05-10'
               , '2020-05-23', '2020-05-24', '2020-05-25'
             ))
             ~ 1

             , TRUE ~ 0
             
           )
         # , marchBreak = case_when(
         #   region %in% c('Toronto') == T
         #   & date %in% as.Date(c(
         #    '2020-03-16', '2020-03-17', '2020-03-18', '2020-03-19', '2020-03-20'
         #   ))
         #   ~ 1
         #   , TRUE ~ 0
         # )
    , weekend = case_when(
      lubridate::wday(date, label = TRUE) %in% c('Sat', 'Sun') ~ 1
      , TRUE ~ 0
    )
  )





# DATA FRAME USED FOR MODEL BUILDING
data <- cityCovid %>% 
  mutate(date = as.Date(date)
         , city = str_replace_all(display_name, '-.+', '')
         , city = str_replace(city, ',.+', '')
         , city = str_replace(city, ' City', '')) %>%
  
  # SAME NAME BUT NOT WHAT WE WANT
  filter(! display_name %in% c('Cleveland, MS', 'Cleveland, TN'
                               , 'London, KY', 'Norwich-New London, CT')
         ) %>%
  
  filter(city %in% cities) %>%
  arrange(city, date) %>% 
  
  group_by(city) %>%
  mutate(newCases = 
           case_when(
             row_number() == 1 ~ all_cases # if first day, then cumulative cases == daily cases
             , TRUE ~ all_cases - lag(all_cases)
           )
  ) %>%
  select(city, date, newCases) %>%
  ungroup() %>%
  
  bind_rows(montrealCovid, torontoCovid
            , stockholmCovid
            , londonCovid) %>%
  arrange(city, date) %>% 
  
  group_by(city) %>%
  mutate(casesTminus1 = lag(newCases)
         , casesTminus2 = lag(casesTminus1)) %>%
  ungroup() %>%
  
  replace_na(list(newCases = 0
                  , casesTminus1 = 0
                  , casesTminus2 = 0)) %>%
 
  
  # BRING IN APPLE DATA
  left_join(mobility %>% select(region, dateMinus7, driving, transit, walking)
            , by = c('city' = 'region', 'date' = 'dateMinus7')) %>%
  rename(drivingMinus7 = driving
         , walkingMinus7 = walking
         , transitMinus7 = transit
         ) %>%
  
  left_join(mobility %>% 
              mutate(dateMinus8 = date + 8) %>%
              select(region, dateMinus8, driving, transit, walking)
            , by = c('city' = 'region', 'date' = 'dateMinus8')) %>%
  rename(drivingMinus8 = driving
         , walkingMinus8 = walking
         , transitMinus8 = transit
         ) %>%
  
  left_join(mobility %>% 
              mutate(dateMinus9 = date + 9) %>%
              select(region, dateMinus9, driving, transit, walking)
            , by = c('city' = 'region', 'date' = 'dateMinus9')) %>%
  rename(drivingMinus9 = driving
         , walkingMinus9 = walking
         , transitMinus9 = transit) %>%
  
  left_join(mobility %>% 
              mutate(dateMinus10 = date + 10) %>%
              select(region, dateMinus10, driving, transit, walking)
            , by = c('city' = 'region', 'date' = 'dateMinus10')) %>%
  rename(drivingMinus10 = driving
         , walkingMinus10 = walking
         , transitMinus10 = transit) %>%
  
  left_join(mobility %>% 
              mutate(dateMinus11 = date + 11) %>%
              select(region, dateMinus11, driving, transit, walking)
            , by = c('city' = 'region', 'date' = 'dateMinus11')) %>%
  rename(drivingMinus11 = driving
         , walkingMinus11 = walking
         , transitMinus11 = transit) %>%  
 
  left_join(mobility %>% 
              mutate(dateMinus12 = date + 12) %>%
              select(region, dateMinus12, driving, transit, walking)
            , by = c('city' = 'region', 'date' = 'dateMinus12')) %>%
  rename(drivingMinus12 = driving
         , walkingMinus12 = walking
         , transitMinus12 = transit) %>%   
  
  left_join(mobility %>% 
              mutate(dateMinus13 = date + 13) %>%
              select(region, dateMinus13, driving, transit, walking)
            , by = c('city' = 'region', 'date' = 'dateMinus13')) %>%
  rename(drivingMinus13 = driving
         , walkingMinus13 = walking
         , transitMinus13 = transit) %>%  
  
  left_join(mobility %>% 
              mutate(dateMinus14 = date + 14) %>%
              select(region, dateMinus14, driving, transit, walking)
            , by = c('city' = 'region', 'date' = 'dateMinus14')) %>%
  rename(drivingMinus14 = driving
         , walkingMinus14 = walking
         , transitMinus14 = transit) %>%  
  
  left_join(mobility %>% 
              select(region, date, driving, walking, transit
                     , anomalousWeekend, longWeekend, weekend)
            , by = c('city' = 'region', 'date' = 'date')) %>%
  
  
  mutate(sumDrivingMinus7_14 = rowSums(select(.,contains('drivingMinus')))/7
         , sumTransitMinus7_14 = rowSums(select(.,contains('transitMinus')))/7
         , sumWalkingMinus7_14 = rowSums(select(.,contains('walkingMinus')))/7
         ) %>%
  
  left_join(topCity %>% select(-X2019.rank)
            , by = c('city' = 'City')) %>%
  
  arrange(city, date) %>%
  
  
  
  
  # ONLY UPTO JUN 4
  filter(!date %in% as.Date(c(#'2020-05-18', '2020-05-19'
                               '2020-06-05', '2020-06-06')) ) 

###################################
# AT A GLANCE
###################################
# EARLIEST APPLE DATA FOR EACH CITY
data %>% group_by(city) %>% summarize(minDate = min(date))



ggplot(data, aes(date, driving, group = city, colour = city))+
  geom_line() +
  xlab('date') +
  ggtitle('Driving') +
  theme_classic() +
  theme(legend.title=element_blank()) 


ggsave(paste0('results/Driving', ".png")
       , width=8, height=8)




ggplot(data, aes(date, transit, group = city, colour = city))+
  geom_line() +
  xlab('date') +
  ggtitle('Transit') +
  theme_classic() +
  theme(legend.title=element_blank()) 


ggsave(paste0('results/Transit', ".png")
       , width=8, height=8)


ggplot(data, aes(date, walking, group = city, colour = city))+
  geom_line() +
  xlab('date') +
  ggtitle('Walking') +
  theme_classic() +
  theme(legend.title=element_blank()) 


ggsave(paste0('results/Walking', ".png")
       , width=8, height=8)