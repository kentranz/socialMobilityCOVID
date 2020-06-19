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
  # ADD MEMORIAL DAY HOLIDAY
  mutate(holidayWeekend = 
           case_when(
             !region %in% c('Toronto', 'Montreal'
                            , 'Stockholm', 'London') == T 
             & date %in% as.Date(c('2020-05-23', '2020-05-24', '2020-05-25'))
             ~ 1
             
              , region %in% c('Toronto') == T
              & date %in% as.Date(c(
                        '2020-02-15', '2020-02-16', '2020-02-17'
                        , '2020-04-10', '2020-04-11', '2020-04-12', '2020-04-13'
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
         , marchBreak = case_when(
           region %in% c('Toronto') == T
           & date %in% as.Date(c(
            '2020-03-16', '2020-03-17', '2020-03-18', '2020-03-19', '2020-03-20'
           ))
           ~ 1
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
  left_join(mobility
            , by = c('city' = 'region', 'date' = 'dateMinus7')) %>%
  select(city, date
         , newCases, casesTminus1, casesTminus2
         , holidayWeekend, marchBreak
         , driving, walking, transit
         #, population
         ) %>%
  arrange(city, date) %>%
  
  # FILTER UNWATED DATE. MAY 11,12 MISSING PER APPLE, ONLY UPTO JUN 4
  filter(!date %in% as.Date(c('2020-05-18', '2020-05-19', '2020-06-05', '2020-06-06')) )

###################################
# AT A GLANCE
###################################
# EARLIEST APPLE DATA FOR EACH CITY
data %>% group_by(city) %>% summarize(minDate = min(date))

