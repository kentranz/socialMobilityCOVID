

#####
# COPY JSON RESPONSE FROM
# https://covid19sweden.azurewebsites.net/api/GraphPerDayRegion?lang=en&regionId=15
# FOUND ON PAGE https://www.covid19insweden.com/en/region.html?regionId=15


temp<-jsonlite::fromJSON("https://covid19sweden.azurewebsites.net/api/GraphPerDayRegion?lang=en&regionId=15")


stockholmCovid <- data.frame(date = as.Date(str_replace(string = temp$labels
                                                , pattern = "T00\\:00\\:00"
                                                , replacement = "")    
                                            )
                             , newCases = round(unlist(temp$datasets$data[2]),0)
                             ) %>%
                  mutate(city = 'Stockholm')

write.csv(stockholmCovid
          , file = "socialMobilityCOVID/data/raw/stockholm.csv"
          , row.names = F)
