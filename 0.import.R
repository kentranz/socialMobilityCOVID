
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
  
  , 'Louisville/Jefferson County, KY-IN'
  , 'Albuquerque, NM'
  , 'San Francisco-Oakland-Berkeley, CA'
  , 'Detroit-Warren-Dearborn, MI'
  , 'Charlotte-Concord-Gastonia, NC-SC'
)


#################################
# FIPS Look up for US cities
# FOR EACH CBSA/MSA:
# Look up the FIPS CODES to see which COUNTIES ARE PART OF THE CBSA/MSA
# DAILY CASES FOR EACH CBSA/MSA == SUM OF ALL FIPS COUNTY
#################################

FIPS <- read.csv('socialMobilityCOVID/data/raw/CBSAtoFIPS.csv'
                 , colClasses=c( "FIPS.State.Code"="character"
                               , "FIPS.County.Code"="character")
                 ) %>% 
  mutate(fips = paste0(FIPS.State.Code, FIPS.County.Code)) %>%
  filter(CBSA.Title %in% UScities) %>%
  mutate(city = str_replace_all(CBSA.Title , '-.+', '')
         , city = str_replace(city, ',.+', '')
         , city = str_replace(city, ' City', '')) 

FIPS %>% distinct(city) %>% pull() %>% length()

# rawNYT <- read.csv('socialMobilityCOVID/data/raw/us-counties.csv'
#                    , colClasses=c( "fips"="character")
#                     )

cityCovid <- read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
                      , colClasses=c( "fips"="character")
                      ) %>%
  filter(fips %in% FIPS$fips)  %>%
  left_join(FIPS %>% select(fips, city)
            , by = c('fips' = 'fips')) %>%
  group_by(city, date) %>% 
  summarize(sumCasesAllCounty = sum(cases)) %>%
  ungroup() %>%
  mutate(date = as.Date(date)
         , city = case_when(city == 'Louisville/Jefferson County' ~ 'Louisville'
                            , TRUE ~ city)
         ) %>%
  arrange(city, date) %>%
  rename(all_cases = sumCasesAllCounty)



torontoCovid <- read.csv('socialMobilityCOVID/data/raw/TorontoCovid.csv') %>%
  mutate(date = as.Date(Episode.Date, '%m/%d/%y') 
         , city = 'Toronto'
         , newCases = Case.Count) %>%
  select(city, date, newCases) 

montrealCovid <- read.csv('socialMobilityCOVID/data/raw/MontrealCovid.csv'
                          , sep = ','
                          ) %>%
  mutate(date = as.Date(DATE, '%m/%d/%y')
         , city = 'Montreal') %>%
  rename(newCases = New.cases) %>%
  select(city, date, newCases)


# stockholmCovid <- read.csv('data/stockholm.csv') %>%
#   mutate(date = as.Date(Date, '%m/%d/%y')
#          , city = 'Stockholm') %>%
#   select(city, date, newCases)

source(file = 'socialMobilityCOVID/000.stockholm.R')


londonCovid <- read.csv('socialMobilityCOVID/data/raw/phe_cases_london_boroughs.csv') %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarize(newCases = sum(new_cases)) %>%
  ungroup() %>%
  mutate(city = 'London') %>%
  select(city, date, newCases)



apple <- read.csv('socialMobilityCOVID/data/raw/applemobilitytrends.csv')


topCity <- read.csv('socialMobilityCOVID/data/raw/top50USCitiesPop.csv') 


#google <- read.csv('socialMobilityCOVID/data/raw/Global_Mobility_Report.csv')


googleTrends <- read.csv('/Users/kentran/Documents/PhD/senMovement/socialMobilityCOVID/data/raw/google trends.csv') %>%
  select(Week, contains('covid.testing')) %>% 
  melt(id = 'Week') %>%
  mutate(city = str_extract(string = variable, pattern = '\\.\\.\\..+?\\.' )
         , city = str_replace_all(string = city, pattern = '\\.', replacement = '')
         , city = case_when(city == 'Los' ~ 'Los Angeles'
                            , city == 'New' ~ 'New York'
                            , city == 'San' ~ 'San Francisco'
                            , TRUE ~ city
                            ) 
         , Week = as.Date(Week, '%m/%d/%y')
         ) %>%
  rename(testingNearMe = value)



