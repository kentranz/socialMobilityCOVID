
UScities <- c(
  'New York-Newark-Jersey City, NY-NJ-PA'
  , 'Chicago-Naperville-Elgin, IL-IN-WI'
  , 'Seattle-Tacoma-Bellevue, WA'
  , 'Tampa-St. Petersburg-Clearwater, FL'
  , 'Baltimore-Columbia-Towson, MD'
  , 'Cleveland-Elyria, OH'
  , 'Sacramento-Roseville-Folsom, CA'
  , 'Indianapolis-Carmel-Anderson, IN'
  , 'Phoenix-Mesa-Chandler, AZ'
  , 'Houston-The Woodlands-Sugar Land, TX'
  , 'Oklahoma City, OK'
  , 'Los Angeles-Long Beach-Anaheim, CA'
  , 'Denver-Aurora-Lakewood, CO'
  , 'Boston-Cambridge-Newton, MA-NH'
  , 'Pittsburgh, PA'
  , 'Memphis, TN-MS-AR'
  , 'Atlanta-Sandy Springs-Alpharetta, GA'
  , 'Miami-Fort Lauderdale-Pompano Beach, FL'
  , 'Dallas-Fort Worth-Arlington, TX'
  , 'Portland-Vancouver-Hillsboro, OR-WA'
)


#################################
# FIPS Look up for US cities
# FOR EACH CBSA/MSA:
# Look up the FIPS CODES to see which COUNTIES ARE PART OF THE CBSA/MSA
# DAILY CASES FOR EACH CBSA/MSA == SUM OF ALL FIPS COUNTY
#################################

FIPS <- read.csv('data/CBSAtoFIPS.csv'
                 , colClasses=c( "FIPS.State.Code"="character"
                               , "FIPS.County.Code"="character")
                 ) %>% 
  mutate(fips = paste0(FIPS.State.Code, FIPS.County.Code)) %>%
  filter(CBSA.Title %in% UScities) %>%
  mutate(city = str_replace_all(CBSA.Title , '-.+', '')
         , city = str_replace(city, ',.+', '')
         , city = str_replace(city, ' City', '')) 

FIPS %>% distinct(city) %>% pull() %>% length()


cityCovid <- read.csv('data/us-counties.csv'
                      , colClasses=c( "fips"="character")
                      ) %>%
  filter(fips %in% FIPS$fips)  %>%
  left_join(FIPS %>% select(fips, city)
            , by = c('fips' = 'fips')) %>%
  group_by(city, date) %>% 
  summarize(sumCasesAllCounty = sum(cases)) %>%
  ungroup() %>%
  mutate(date = as.Date(date)) %>%
  arrange(city, date) %>%
  rename(all_cases = sumCasesAllCounty)



torontoCovid <- read.csv('data/TorontoCovid.csv') %>%
  mutate(date = as.Date(Episode.Date) 
         , city = 'Toronto'
         , newCases = Case.Count) %>%
  select(city, date, newCases) 

montrealCovid <- read.csv('data/MontrealCovid.csv'
                          , sep = ';'
) %>%
  mutate(date = as.Date(Date)
         , city = 'Montreal') %>%
  rename(newCases = Nouveaux.cas) %>%
  select(city, date, newCases)


stockholmCovid <- read.csv('data/stockholm.csv') %>%
  mutate(date = as.Date(Date)
         , city = 'Stockholm') %>%
  select(city, date, newCases)


londonCovid <- read.csv('data/phe_cases_london_boroughs.csv') %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarize(newCases = sum(new_cases)) %>%
  ungroup() %>%
  mutate(city = 'London') %>%
  select(city, date, newCases)



#################################
# Apple data
#################################
apple <- read.csv('data/applemobilitytrends-2020-08-09.csv')


topCity <- read.csv('data/top50USCitiesPop.csv') 
