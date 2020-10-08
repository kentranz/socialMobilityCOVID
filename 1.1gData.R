

gMobility <- google %>% 
  filter(country_region_code == 'GB' 
         & sub_region_1 == 'Greater London'
         & sub_region_2 == '') %>%
  mutate(City = 'London') %>%
  rbind(
    google %>% filter(country_region_code == 'CA' 
                      & sub_region_2 == 'Toronto Division') %>%
      mutate(City = "Toronto")
    ) %>% 
  rbind(
    google %>% filter(country_region_code == 'CA' 
                      & sub_region_2 == 'Montreal') %>%
      mutate(City = "Montreal")
  ) %>% 
  rbind(
    google %>% filter(country_region_code == 'SE' 
                      & sub_region_1 == 'Stockholm County'
                      & sub_region_2 == '') %>%
      mutate(City = "Stockholm")
  ) %>%
  rename(
    g_retail = retail_and_recreation_percent_change_from_baseline
    , g_grocery = grocery_and_pharmacy_percent_change_from_baseline
    , g_parks = parks_percent_change_from_baseline
    , g_transit = transit_stations_percent_change_from_baseline
    , g_workplaces = workplaces_percent_change_from_baseline
    , g_residential = residential_percent_change_from_baseline
  ) %>%
  select(-country_region_code
         , -country_region
         , -sub_region_1
         , -sub_region_2
         , -metro_area
         , -iso_3166_2_code
         , -census_fips_code) %>%
  mutate(date = as.Date(date))



gMobility %<>%
  left_join(gMobility %>%
              mutate(dateMinus7 = date + 7) %>%
              rename(g_retailMinus7 = g_retail
                     , g_groceryMinus7 = g_grocery
                     , g_parksMinus7 = g_parks
                     , g_transitMinus7 = g_transit
                     , g_workplacesMinus7 = g_workplaces
                     , g_residentialMinus7 = g_residential
              )
            , by = c('City' = 'City'
                     , 'date' = 'dateMinus7')
       
  ) %>%
  select(-date.y) %>%
  
  
  left_join(gMobility %>%
              mutate(dateMinus8 = date + 8) %>%
              rename(g_retailMinus8 = g_retail
                     , g_groceryMinus8 = g_grocery
                     , g_parksMinus8 = g_parks
                     , g_transitMinus8 = g_transit
                     , g_workplacesMinus8 = g_workplaces
                     , g_residentialMinus8 = g_residential
              )
            , by = c('City' = 'City'
                     , 'date' = 'dateMinus8')
            
  ) %>%
  select(-date.y) %>%

  left_join(gMobility %>%
              mutate(dateMinus9 = date + 9) %>%
              rename(g_retailMinus9 = g_retail
                     , g_groceryMinus9 = g_grocery
                     , g_parksMinus9 = g_parks
                     , g_transitMinus9 = g_transit
                     , g_workplacesMinus9 = g_workplaces
                     , g_residentialMinus9 = g_residential
              )
            , by = c('City' = 'City'
                     , 'date' = 'dateMinus9')
            
  ) %>%
  select(-date.y) %>%


  left_join(gMobility %>%
              mutate(dateMinus10 = date + 10) %>%
              rename(g_retailMinus10 = g_retail
                     , g_groceryMinus10 = g_grocery
                     , g_parksMinus10 = g_parks
                     , g_transitMinus10 = g_transit
                     , g_workplacesMinus10 = g_workplaces
                     , g_residentialMinus10 = g_residential
              )
            , by = c('City' = 'City'
                     , 'date' = 'dateMinus10')
            
  ) %>%
  select(-date.y) %>%

  left_join(gMobility %>%
              mutate(dateMinus11 = date + 11) %>%
              rename(g_retailMinus11 = g_retail
                     , g_groceryMinus11 = g_grocery
                     , g_parksMinus11 = g_parks
                     , g_transitMinus11 = g_transit
                     , g_workplacesMinus11 = g_workplaces
                     , g_residentialMinus11 = g_residential
              )
            , by = c('City' = 'City'
                     , 'date' = 'dateMinus11')
            
  ) %>%
  select(-date.y) %>%


  left_join(gMobility %>%
              mutate(dateMinus12 = date + 12) %>%
              rename(g_retailMinus12 = g_retail
                     , g_groceryMinus12 = g_grocery
                     , g_parksMinus12 = g_parks
                     , g_transitMinus12 = g_transit
                     , g_workplacesMinus12 = g_workplaces
                     , g_residentialMinus12 = g_residential
              )
            , by = c('City' = 'City'
                     , 'date' = 'dateMinus12')
            
  ) %>%
  select(-date.y) %>%

  left_join(gMobility %>%
              mutate(dateMinus13 = date + 13) %>%
              rename(g_retailMinus13 = g_retail
                     , g_groceryMinus13 = g_grocery
                     , g_parksMinus13 = g_parks
                     , g_transitMinus13 = g_transit
                     , g_workplacesMinus13 = g_workplaces
                     , g_residentialMinus13 = g_residential
              )
            , by = c('City' = 'City'
                     , 'date' = 'dateMinus13')
            
  ) %>%
  select(-date.y) %>%

  left_join(gMobility %>%
              mutate(dateMinus14 = date + 14) %>%
              rename(g_retailMinus14 = g_retail
                     , g_groceryMinus14 = g_grocery
                     , g_parksMinus14 = g_parks
                     , g_transitMinus14 = g_transit
                     , g_workplacesMinus14 = g_workplaces
                     , g_residentialMinus14 = g_residential
              )
            , by = c('City' = 'City'
                     , 'date' = 'dateMinus14')
            
  ) %>%
  select(-date.y) %>%
  arrange(City, date) 



colToRename <- data %>% 
  select(starts_with('driving')
         , starts_with('walking')
         , starts_with('transit')) %>% 
  colnames()


gMobility %<>%
  left_join(data %>% 
              rename_at(colToRename, list( ~ paste0('a_', .)))
            , by = c('City' = 'city', 'date' = 'date')
    ) %>%
  select(City, date, newCases, casesTminus1, casesTminus2, everything()) %>%
  filter(date >= as.Date('2020-03-01'))


write.csv(gMobility
          , file = 'socialMobilityCOVID/data/gMobility.csv')