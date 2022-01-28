####################################################
# Construct weekly cases
####################################################

start <- as.Date('2020-03-01') # starts on Sunday
end <- as.Date('2020-12-12') # ends on Saturday


weeklyCases <- all %>%
  group_by(city) %>%
  mutate(weekday = weekdays(as.Date(date))
         , casesRollSum7 = RcppRoll::roll_sum(newCases,7, fill=NA, align="left")
         ) %>%
  
# TEST
#weeklyCases %>% select(date, newCases, casesRollSum7) %>% print(n=300)
  
  filter(as.Date(date) >= start 
         & as.Date(date) <= end
         & weekday == 'Sunday'
         ) %>%
  mutate(weekNum = row_number()
         , case.rate =  round(casesRollSum7 * 100000 / Population, 2)
         , casesTminus1.rate = lag(case.rate, 1)
         , casesTminus2.rate = lag(case.rate, 2)
         ) %>%
  ungroup() %>%
  select(city, date, weekNum, case.rate, casesTminus1.rate, casesTminus2.rate) %>% 
  left_join(googleTrends %>% select(Week, testingNearMe, city), by = c('date' = 'Week', 'city' = 'city')) %>%
  mutate(testingNearMeTminus1 = lag(testingNearMe, 1))





# EXPORT
write.csv(weeklyCases
          , file = "socialMobilityCOVID/data/weeklyCases.csv"
          , row.names = F)
