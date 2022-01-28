---
title: "weeklyForecast"
output: 
  #rmarkdown::github_document
  html_document:
    toc: true
    number_sections: true
    keep_md: true
---









# Mar-Aug

```r
weeklyCases <- read.csv('https://raw.githubusercontent.com/kentranz/socialMobilityCOVID/master/data/weeklyCases.csv')

cutOff <- as.Date("2020-11-29")

train <- weeklyCases %>% 
  filter(weekNum > 3 & as.Date(date) < cutOff)


test7 <- weeklyCases %>% 
   filter(weekNum > 3 & as.Date(date) >= cutOff & weekNum < 41)

  
test14 <- weeklyCases %>% 
  filter(weekNum > 3 & as.Date(date) >= cutOff) %>%
  
  # retained lag case rates for first week only, zero out the rest
  mutate(
    casesTminus2.rate = case_when(weekNum == 41 ~ 0, TRUE ~ casesTminus2.rate)
  )


nrow(train)
```

```
## [1] 900
```
