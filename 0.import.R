


apple <- read.csv('data/applemobilitytrends-2020-06-14.csv')
cityCovid <- read.csv('data/coviddat_us.csv')
topCity <- read.csv('data/top50USCitiesPop.csv')

torontoCovid <- read.csv('data/TorontoCovid.csv') %>%
  mutate(date = as.Date(Episode.Date) 
         , city = 'Toronto'
         , newCases = Case.Count) %>%
  select(city, date, newCases)

montrealCovid <- read.csv('data/MontrealCovid.csv') %>%
  mutate(date = as.Date(date)
         , city = 'Montreal') %>%
  select(city, date, newCases)

stockholmCovid <- read.csv('data/stockholm.csv') %>%
  mutate(date = as.Date(Date)
         , city = 'Stockholm') %>%
  select(city, date, newCases)
