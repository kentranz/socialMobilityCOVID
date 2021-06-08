---
title: "covid19-lmm-forecast-draft"
output: 
  #rmarkdown::github_document
  html_document:
    toc: true
    number_sections: true
    keep_md: true
---





# Downloading and getting data in shape

<br />

## Initial work

- First need to read in all.csv off Ken's GitHub page; to do this, load RCurl package to use getURL function w/in read.csv, as the csv file is embedded within an html file (typical for GitHub).


```r
alldata <-
  read.csv(text=getURL("https://raw.githubusercontent.com/kentranz/socialMobilityCOVID/master/data/all.csv"), 
           header=T)
## the above file has already been updated for standardized 
##    Apple mobility data, and various indicator variables for
##    dates of interest, as well as lagged cases variables, etc.
##    --> it has longitudinal data for 24 cities,
##          20 in US, and Toronto, Montreal, London, and Stockholm

str(alldata)
```

```
## 'data.frame':	7225 obs. of  43 variables:
##  $ city                   : Factor w/ 25 levels "Albuquerque",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ date                   : Factor w/ 289 levels "2020-03-01","2020-03-02",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ newCases               : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
##  $ casesTminus1           : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
##  $ casesTminus2           : num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
##  $ drivingMinus7          : num  10.63 17.33 8.43 10 13.26 ...
##  $ transitMinus7          : num  4.82 7.39 -16.17 2.54 9.04 ...
##  $ walkingMinus7          : num  -2.09 14.1 0.84 4.02 12.07 ...
##  $ drivingMinus8          : num  8.61 10.63 17.33 8.43 10 ...
##  $ transitMinus8          : num  1.18 4.82 7.39 -16.17 2.54 ...
##  $ walkingMinus8          : num  11.64 -2.09 14.1 0.84 4.02 ...
##  $ drivingMinus9          : num  11.38 8.61 10.63 17.33 8.43 ...
##  $ transitMinus9          : num  11.47 1.18 4.82 7.39 -16.17 ...
##  $ walkingMinus9          : num  1.44 11.64 -2.09 14.1 0.84 ...
##  $ drivingMinus10         : num  10 11.38 8.61 10.63 17.33 ...
##  $ transitMinus10         : num  19.53 11.47 1.18 4.82 7.39 ...
##  $ walkingMinus10         : num  10.96 1.44 11.64 -2.09 14.1 ...
##  $ drivingMinus11         : num  4.87 10 11.38 8.61 10.63 ...
##  $ transitMinus11         : num  3.02 19.53 11.47 1.18 4.82 ...
##  $ walkingMinus11         : num  2.22 10.96 1.44 11.64 -2.09 ...
##  $ drivingMinus12         : num  7.53 4.87 10 11.38 8.61 ...
##  $ transitMinus12         : num  4.45 3.02 19.53 11.47 1.18 ...
##  $ walkingMinus12         : num  8.9 2.22 10.96 1.44 11.64 ...
##  $ drivingMinus13         : num  9.69 7.53 4.87 10 11.38 ...
##  $ transitMinus13         : num  12.16 4.45 3.02 19.53 11.47 ...
##  $ walkingMinus13         : num  17.65 8.9 2.22 10.96 1.44 ...
##  $ drivingMinus14         : num  21.58 9.69 7.53 4.87 10 ...
##  $ transitMinus14         : num  12.38 12.16 4.45 3.02 19.53 ...
##  $ walkingMinus14         : num  21.86 17.65 8.9 2.22 10.96 ...
##  $ driving                : num  19.75 11.94 13.82 8.62 13.86 ...
##  $ walking                : num  7.8 15.76 15.1 5.41 14.15 ...
##  $ transit                : num  -1.43 5.46 0 0.32 19.43 ...
##  $ anomalousWeekend       : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ longWeekend            : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ weekend                : int  1 0 0 0 0 0 1 1 0 0 ...
##  $ sumDrivingMinus7_14    : num  12 11.4 11.3 11.6 12.8 ...
##  $ sumTransitMinus7_14    : num  9.86 9.15 5.1 4.83 5.69 ...
##  $ sumWalkingMinus7_14    : num  10.37 9.26 6.86 6.16 7.57 ...
##  $ Population             : int  915927 915927 915927 915927 915927 915927 915927 915927 915927 915927 ...
##  $ LandAreakm             : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ Poverty.rate           : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ Median.age             : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ Median.household.income: int  NA NA NA NA NA NA NA NA NA NA ...
```

- Code to read in data and output from `str` call hidden from this output.  Most R code below for data manipulation will also be hidden from output, and only selective dataset elements will be displayed. Alternatively, R code for data analyses and resulting output will more generally be displayed.

- Will output variable names though:


```
##  [1] "city"                    "date"                   
##  [3] "newCases"                "casesTminus1"           
##  [5] "casesTminus2"            "drivingMinus7"          
##  [7] "transitMinus7"           "walkingMinus7"          
##  [9] "drivingMinus8"           "transitMinus8"          
## [11] "walkingMinus8"           "drivingMinus9"          
## [13] "transitMinus9"           "walkingMinus9"          
## [15] "drivingMinus10"          "transitMinus10"         
## [17] "walkingMinus10"          "drivingMinus11"         
## [19] "transitMinus11"          "walkingMinus11"         
## [21] "drivingMinus12"          "transitMinus12"         
## [23] "walkingMinus12"          "drivingMinus13"         
## [25] "transitMinus13"          "walkingMinus13"         
## [27] "drivingMinus14"          "transitMinus14"         
## [29] "walkingMinus14"          "driving"                
## [31] "walking"                 "transit"                
## [33] "anomalousWeekend"        "longWeekend"            
## [35] "weekend"                 "sumDrivingMinus7_14"    
## [37] "sumTransitMinus7_14"     "sumWalkingMinus7_14"    
## [39] "Population"              "LandAreakm"             
## [41] "Poverty.rate"            "Median.age"             
## [43] "Median.household.income"
```

- Here, *newCases* will be converted into a rate, and this, and the first two days of lagged case rate values, will serve as the primary response and two predictors, respectively.

- The time variable will be a converted version of *date*.  This, and the previous bullet point will be worked on in the next subsection.


<br />

## Additional data manipulation

- Creating a US city indicator variable, i.e., 1 if US city, 0 for non-US city.



### Play with dates

- First, note that not all 24 cities had counts available to us starting at earlier date in our dataset of March 1, 2019, while all cities had observations starting March 13th.  As of the November 14th end dataset (the one being worked on here), these earlier dates in March, for cities where they were not originally collected, were entered in original dataset as 0's.  This seems a reasonable choice given the very low or non-existent case counts in many cities at that time.  In addition, we will not be use data earlier than March 13th in our analyses, so those 0's are irrelevant for our work here.

- Dates are in character form, and need to change to numeric for modeling.

- Need to align all new day variable to min of observed dates, so day 0 will 2020-03-01, but will have a second new day variable that will be about 2 days after 2020-03-13 (make this the midpoint of March, i.e., March 16th), to allow for two days of lagged cases as predictors; the lagged 7-day mobility data is actually available since near Feb 1, if we ever need it.

- Aside from March 16th start date, we will make one for April 1st, and one that is based on a French study (Prague et al., 2020), which is dynamic based on specifically starting on the first day of cases for which it is followed by
at least 3 consecutive days of cases as well; this took longer to program.

- Will use *chron* package in R to help with dates and specifying initial origin at March 1st (day 0). As suggested on *chron* help page, use `options` argument to set default first day --- could do this twice to help create two sets of two columns, one where 03-01-20 is default and one for 03-16-20;
a 2nd option is to subset data when modeling such that all dates are
at least 03-16-20 when we want to have the later start/default date, which
will only affect the intercept interpretation; will take the 2nd approach
where day 15 (16th day), i.e., 03-16-20, will be first day w/ all complete data that accounts for two lagged cases days.



- Here is where French start day will be defined, which will be the 1st day that at least 4 consecutive days of cases start.  Code will identify sequences in a vector.  Output prints first day of French method in each city.


```
##          city.FM startdate.FM
## 1    Albuquerque     03/01/20
## 2        Atlanta     03/01/20
## 3      Baltimore     03/01/20
## 4         Boston     03/01/20
## 5      Charlotte     03/01/20
## 6        Chicago     03/01/20
## 7      Cleveland     03/01/20
## 8         Dallas     03/01/20
## 9         Denver     03/01/20
## 10       Detroit     03/01/20
## 11       Houston     03/01/20
## 12  Indianapolis     03/01/20
## 13   Los Angeles     03/01/20
## 14    Louisville     03/01/20
## 15       Memphis     03/01/20
## 16         Miami     03/01/20
## 17      New York     03/01/20
## 18      Oklahoma     03/01/20
## 19       Phoenix     03/01/20
## 20    Pittsburgh     03/01/20
## 21      Portland     03/01/20
## 22    Sacramento     03/01/20
## 23 San Francisco     03/01/20
## 24       Seattle     03/01/20
## 25         Tampa     03/01/20
```

- Dec 17 2020:  The above all provided, it should be mentioned that the use of the French method and start dates other than 03-16-20 will be put off for potential future use.


### Creating case rates

- Making another column in dataset, which is a rate, i.e.,
    number of cases per 100000 people in population.  This will
    be the longitudinal response variable in our graphing and modeling.
    To do this, taking *newCases*, divide by Population (in city), and      multiplying by 100000; will do this for *caseTminus1* and *caseTminus2*         variables as well:





# Start longitudinal work

<br />

## Initial graphing

- Start with using *nlme* library, created so-called groupedData objects, which benefit both graphing and modeling using the `lme` function (and other modeling functions) within *nlme*.



- Create plots from groupedData objects; nlme generic plotting on these objects uses Trellis graphs here.

![](lme_files/figure-html/plot_group-1.png)<!-- -->![](lme_files/figure-html/plot_group-2.png)<!-- -->







# Mar-Aug

## Data partition

```r
cutOff <- as.Date("2020-08-31")

testRange <- 14
```

## LME

```r
modelName = 'lmeAug_' 

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend,
    random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city,
    data = train
    )
summary(model)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   25991.58 26042.36 -12987.79
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev     Corr  
## casesTminus1.rate 0.09814567 cssT1.
## casesTminus2.rate 0.08061118 -0.948
## Residual          5.20123468       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend 
##                        Value  Std.Error   DF   t-value p-value
## (Intercept)        1.6411579 0.12535393 4197 13.092193       0
## casesTminus1.rate  0.5370228 0.02655276 4197 20.224741       0
## casesTminus2.rate  0.3498719 0.02340705 4197 14.947289       0
## weekend           -1.7815348 0.17832357 4197 -9.990462       0
##  Correlation: 
##                   (Intr) cssT1. cssT2.
## casesTminus1.rate -0.149              
## casesTminus2.rate -0.094 -0.898       
## weekend           -0.348 -0.016 -0.013
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -9.3474576 -0.3529970 -0.1114364  0.2555198 12.5509615 
## 
## Number of Observations: 4225
## Number of Groups: 25
```

```r
# test to hold all date data points up to testRange
test <- alldata.groupedR %>% 
filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
         
         


# Predict one new observation at a time
# For each new 7-day period, refit the model with actual observed Y
for (i in 1:testRange)
{
  
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  #print(i)
  #print(model)
  
  # test to check correct training window
  # print(i)
  # print(max(as.Date(train$date)))
  
  
  tempTest <- test %>% 
    filter(as.Date(date) == cutOff + i)
  
  tempTest$pred <- predict(model, newdata = tempTest)

  
  # test to check for correct test window
  # print(max(as.Date(tempTest$date)))
  
  # create design matrix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], tempTest)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  #results7$SE <- sqrt(predvar) 
  tempTest$SE <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  tempTest$lowerCI <- tempTest$pred - 1.96*tempTest$SE
  tempTest$upperCI <- tempTest$pred + 1.96*tempTest$SE
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
  
  # clean up extra columns after join 
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
}



# pull all metrics into one dataframe called dailyMetrics
metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))

dailyMetrics <- metrics
```


## LM

```r
modelName <- 'lmAug_'

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lm(case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend,
    data = train
    )
summary(model)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate + 
##     weekend, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -48.773  -1.772  -0.527   1.351  65.494 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        1.40273    0.11840   11.85   <2e-16 ***
## casesTminus1.rate  0.55618    0.01421   39.15   <2e-16 ***
## casesTminus2.rate  0.36400    0.01418   25.67   <2e-16 ***
## weekend           -1.81137    0.17959  -10.09   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.251 on 4221 degrees of freedom
## Multiple R-squared:  0.7935,	Adjusted R-squared:  0.7934 
## F-statistic:  5407 on 3 and 4221 DF,  p-value: < 2.2e-16
```

```r
test <- alldata.groupedR %>% 
filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
         
  
for (i in 1:testRange)
{
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)

  pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
  tempTest$pred <- pred$fit
  tempTest$lowerCI <- pred$lwr
  tempTest$upperCI <- pred$upr
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
  
}

metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))


dailyMetrics %<>% 
  left_join(metrics, by = "city")
```



## LME + Walking

```r
modelName = 'lmeWalkingAug_' 

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend + walkingMinus7,
    random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city,
    data = train
    )
summary(model)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   25999.09 26056.21 -12990.54
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev    Corr  
## casesTminus1.rate 0.1022726 cssT1.
## casesTminus2.rate 0.0789893 -0.93 
## Residual          5.1960527       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend +      walkingMinus7 
##                        Value  Std.Error   DF   t-value p-value
## (Intercept)        1.7432729 0.13007956 4196 13.401590    0.00
## casesTminus1.rate  0.5310574 0.02723713 4196 19.497551    0.00
## casesTminus2.rate  0.3446061 0.02317438 4196 14.870131    0.00
## weekend           -1.7108632 0.18041007 4196 -9.483191    0.00
## walkingMinus7      0.0051257 0.00220191 4196  2.327856    0.02
##  Correlation: 
##                   (Intr) cssT1. cssT2. weeknd
## casesTminus1.rate -0.161                     
## casesTminus2.rate -0.111 -0.875              
## weekend           -0.292 -0.023 -0.024       
## walkingMinus7      0.235 -0.040 -0.070  0.157
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -9.3567497 -0.3565535 -0.1082410  0.2525661 12.5241901 
## 
## Number of Observations: 4225
## Number of Groups: 25
```

```r
# test to hold all date data points up to testRange
test <- alldata.groupedR %>% 
filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate, walkingMinus7) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
         
         


# Predict one new observation at a time
# For each new 7-day period, refit the model with actual observed Y
for (i in 1:testRange)
{
  
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  #print(i)
  #print(model)
  
  # test to check correct training window
  # print(i)
  # print(max(as.Date(train$date)))
  
  
  tempTest <- test %>% 
    filter(as.Date(date) == cutOff + i)
  
  tempTest$pred <- predict(model, newdata = tempTest)

  
  # test to check for correct test window
  # print(max(as.Date(tempTest$date)))
  
  # create design matrix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], tempTest)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  #results7$SE <- sqrt(predvar) 
  tempTest$SE <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  tempTest$lowerCI <- tempTest$pred - 1.96*tempTest$SE
  tempTest$upperCI <- tempTest$pred + 1.96*tempTest$SE
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
  
  # clean up extra columns after join 
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
}



# pull all metrics into one dataframe called dailyMetrics
metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))

dailyMetrics %<>% 
  left_join(metrics, by = "city")
```


## LM + Walking

```r
modelName <- 'lmWalkingAug_'

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lm(case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend + walkingMinus7,
    data = train
    )
summary(model)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate + 
##     weekend + walkingMinus7, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -48.744  -1.761  -0.512   1.328  65.445 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        1.413551   0.119788  11.800   <2e-16 ***
## casesTminus1.rate  0.555981   0.014210  39.125   <2e-16 ***
## casesTminus2.rate  0.363481   0.014209  25.582   <2e-16 ***
## weekend           -1.797202   0.181160  -9.921   <2e-16 ***
## walkingMinus7      0.001185   0.001980   0.598     0.55    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.251 on 4220 degrees of freedom
## Multiple R-squared:  0.7935,	Adjusted R-squared:  0.7933 
## F-statistic:  4055 on 4 and 4220 DF,  p-value: < 2.2e-16
```

```r
test <- alldata.groupedR %>% 
filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate, walkingMinus7) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
         
  
for (i in 1:testRange)
{
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)

  pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
  tempTest$pred <- pred$fit
  tempTest$lowerCI <- pred$lwr
  tempTest$upperCI <- pred$upr
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
  
}

metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))


dailyMetrics %<>% 
  left_join(metrics, by = "city")
```






## LME-Log

```r
modelName = 'lmeLogAug_' 

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))


model <- lme(fixed = log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + weekend,
    random = ~ log(casesTminus1.rate) + log(casesTminus2.rate) - 1 | city,
    data = train
    , control = lmeControl(maxIter = 1e8, opt='optim') 
    )
summary(model)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   7137.503 7188.286 -3560.751
## 
## Random effects:
##  Formula: ~log(casesTminus1.rate) + log(casesTminus2.rate) - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                        StdDev     Corr  
## log(casesTminus1.rate) 0.05373854 l(T1.)
## log(casesTminus2.rate) 0.03306970 -0.939
## Residual               0.55863296       
## 
## Fixed effects: log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) +      weekend 
##                             Value  Std.Error   DF   t-value p-value
## (Intercept)             0.3170175 0.01589327 4197  19.94665       0
## log(casesTminus1.rate)  0.4916141 0.01789620 4197  27.47030       0
## log(casesTminus2.rate)  0.3795013 0.01535762 4197  24.71095       0
## weekend                -0.2347221 0.01917068 4197 -12.24381       0
##  Correlation: 
##                        (Intr) l(T1.) l(T2.)
## log(casesTminus1.rate) -0.223              
## log(casesTminus2.rate) -0.113 -0.863       
## weekend                -0.265 -0.004 -0.043
## 
## Standardized Within-Group Residuals:
##         Min          Q1         Med          Q3         Max 
## -11.4346821  -0.4059338   0.0481264   0.4911078   9.1394059 
## 
## Number of Observations: 4225
## Number of Groups: 25
```

```r
# test to hold all date data points up to testRange
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )

for (i in 1:testRange)
{
  
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  # test to check correct training window
  # print(i)
  # print(max(as.Date(train$date)))
  
  # filter test set to calculate pred for only 1 day at a time
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)
  tempTest$pred <- predict(model, newdata = tempTest)
  
  # test to check for correct test window
  # print(max(as.Date(tempTest$date)))
  
  # create design matrix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], tempTest)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  #results7$SE <- sqrt(predvar) 
  tempTest$SE <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  tempTest$lowerCI <- tempTest$pred - 1.96*tempTest$SE
  tempTest$upperCI <- tempTest$pred + 1.96*tempTest$SE
  
  tempTest %<>% 
    mutate(pred = exp(pred)
           , lowerCI = exp(lowerCI)
           , upperCI = exp(upperCI)
           )
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
}



# pull all metrics into one dataframe called dailyMetrics
metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))

dailyMetrics %<>% 
  left_join(metrics, by = "city")
```

## LM-Log

```r
modelName <- 'lmLogAug_'

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lm(log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + weekend,
    data = train
    )
summary(model)
```

```
## 
## Call:
## lm(formula = log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + 
##     weekend, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4582 -0.2293  0.0286  0.2763  5.1127 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             0.30669    0.01575   19.48   <2e-16 ***
## log(casesTminus1.rate)  0.48658    0.01378   35.31   <2e-16 ***
## log(casesTminus2.rate)  0.39466    0.01335   29.56   <2e-16 ***
## weekend                -0.23668    0.01925  -12.30   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5614 on 4221 degrees of freedom
## Multiple R-squared:  0.7788,	Adjusted R-squared:  0.7786 
## F-statistic:  4953 on 3 and 4221 DF,  p-value: < 2.2e-16
```

```r
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
  
for (i in 1:testRange)
{
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)

  pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
  tempTest$pred <- exp(pred$fit)
  tempTest$lowerCI <- exp(pred$lwr)
  tempTest$upperCI <- exp(pred$upr)
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }

  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )  
}

metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))


dailyMetrics %<>% 
  left_join(metrics, by = "city")
```



## LME-Log + Walking

```r
modelName = 'lmeLogWalkingAug_' 

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))


model <- lme(fixed = log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + weekend + walkingMinus7,
    random = ~ log(casesTminus1.rate) + log(casesTminus2.rate) - 1 | city,
    data = train
    , control = lmeControl(maxIter = 1e8, opt='optim') 
    )
summary(model)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   7136.557 7193.685 -3559.279
## 
## Random effects:
##  Formula: ~log(casesTminus1.rate) + log(casesTminus2.rate) - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                        StdDev     Corr  
## log(casesTminus1.rate) 0.05958363 l(T1.)
## log(casesTminus2.rate) 0.03049040 -0.915
## Residual               0.55697329       
## 
## Fixed effects: log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) +      weekend + walkingMinus7 
##                             Value   Std.Error   DF   t-value p-value
## (Intercept)             0.3375113 0.016405274 4196  20.57334       0
## log(casesTminus1.rate)  0.4887454 0.018543709 4196  26.35640       0
## log(casesTminus2.rate)  0.3710105 0.015139048 4196  24.50686       0
## weekend                -0.2197504 0.019391740 4196 -11.33216       0
## walkingMinus7           0.0010818 0.000245456 4196   4.40738       0
##  Correlation: 
##                        (Intr) l(T1.) l(T2.) weeknd
## log(casesTminus1.rate) -0.213                     
## log(casesTminus2.rate) -0.141 -0.830              
## weekend                -0.210 -0.007 -0.062       
## walkingMinus7           0.245 -0.016 -0.113  0.169
## 
## Standardized Within-Group Residuals:
##          Min           Q1          Med           Q3          Max 
## -11.38927874  -0.41116947   0.04455902   0.48945010   9.16827727 
## 
## Number of Observations: 4225
## Number of Groups: 25
```

```r
# test to hold all date data points up to testRange
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate, walkingMinus7) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )

for (i in 1:testRange)
{
  
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  # test to check correct training window
  # print(i)
  # print(max(as.Date(train$date)))
  
  # filter test set to calculate pred for only 1 day at a time
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)
  tempTest$pred <- predict(model, newdata = tempTest)
  
  # test to check for correct test window
  # print(max(as.Date(tempTest$date)))
  
  # create design matrix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], tempTest)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  #results7$SE <- sqrt(predvar) 
  tempTest$SE <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  tempTest$lowerCI <- tempTest$pred - 1.96*tempTest$SE
  tempTest$upperCI <- tempTest$pred + 1.96*tempTest$SE
  
  tempTest %<>% 
    mutate(pred = exp(pred)
           , lowerCI = exp(lowerCI)
           , upperCI = exp(upperCI)
           )
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
}



# pull all metrics into one dataframe called dailyMetrics
metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))

dailyMetrics %<>% 
  left_join(metrics, by = "city")
```

## LM-Log + Walking

```r
modelName <- 'lmLogWalkingAug_'

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lm(log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + weekend + walkingMinus7,
    data = train
    )
summary(model)
```

```
## 
## Call:
## lm(formula = log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + 
##     weekend + walkingMinus7, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -6.4526 -0.2337  0.0303  0.2766  5.0993 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             0.3154223  0.0161030  19.588   <2e-16 ***
## log(casesTminus1.rate)  0.4846967  0.0137906  35.147   <2e-16 ***
## log(casesTminus2.rate)  0.3923449  0.0133747  29.335   <2e-16 ***
## weekend                -0.2296043  0.0194343 -11.814   <2e-16 ***
## walkingMinus7           0.0005475  0.0002141   2.557   0.0106 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.561 on 4220 degrees of freedom
## Multiple R-squared:  0.7791,	Adjusted R-squared:  0.7789 
## F-statistic:  3721 on 4 and 4220 DF,  p-value: < 2.2e-16
```

```r
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate, walkingMinus7) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
  
for (i in 1:testRange)
{
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)

  pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
  tempTest$pred <- exp(pred$fit)
  tempTest$lowerCI <- exp(pred$lwr)
  tempTest$upperCI <- exp(pred$upr)
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }

  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )  
}

metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))


dailyMetrics %<>% 
  left_join(metrics, by = "city")
```







# Mar-Nov

## Data partition

```r
cutOff <- as.Date("2020-11-30")

testRange <- 14
```

## LME

```r
modelName = 'lmeNov_' 

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend,
    random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city,
    data = train
    )
summary(model)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##       AIC      BIC   logLik
##   47269.6 47323.83 -23626.8
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev    Corr  
## casesTminus1.rate 0.1438038 cssT1.
## casesTminus2.rate 0.1326577 -0.889
## Residual          9.1016707       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend 
##                        Value  Std.Error   DF   t-value p-value
## (Intercept)        2.6592531 0.17071128 6472 15.577489       0
## casesTminus1.rate  0.5058146 0.03304937 6472 15.304818       0
## casesTminus2.rate  0.3650749 0.03113294 6472 11.726320       0
## weekend           -2.3750293 0.25150319 6472 -9.443337       0
##  Correlation: 
##                   (Intr) cssT1. cssT2.
## casesTminus1.rate -0.087              
## casesTminus2.rate -0.080 -0.879       
## weekend           -0.372 -0.006 -0.013
## 
## Standardized Within-Group Residuals:
##         Min          Q1         Med          Q3         Max 
## -10.1206393  -0.2994794  -0.1122960   0.1646515  21.0437133 
## 
## Number of Observations: 6500
## Number of Groups: 25
```

```r
# test to hold all date data points up to testRange
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )


# Predict one new observation at a time
# For each new test, refit the model with actual observed Y
for (i in 1:testRange)
{
  
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  # test to check correct training window
  # print(i)
  # print(max(as.Date(train$date)))
  
  # filter test set to calculate pred for only 1 day at a time
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)
  tempTest$pred <- predict(model, newdata = tempTest)
  
  # test to check for correct test window
  # print(max(as.Date(tempTest$date)))
  
  # create design matrix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], tempTest)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  #results7$SE <- sqrt(predvar) 
  tempTest$SE <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  tempTest$lowerCI <- tempTest$pred - 1.96*tempTest$SE
  tempTest$upperCI <- tempTest$pred + 1.96*tempTest$SE
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )  
}



# pull all metrics into one dataframe called dailyMetrics
metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))

dailyMetrics %<>% 
  left_join(metrics, by = "city")
```


## LM

```r
modelName <- 'lmNov_'

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lm(case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend,
    data = train
    )
summary(model)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate + 
##     weekend, data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -103.429   -2.656   -0.960    1.579  189.875 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        2.40346    0.16714  14.380   <2e-16 ***
## casesTminus1.rate  0.47279    0.01135  41.672   <2e-16 ***
## casesTminus2.rate  0.42342    0.01141  37.103   <2e-16 ***
## weekend           -2.42871    0.25596  -9.488   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.29 on 6496 degrees of freedom
## Multiple R-squared:  0.703,	Adjusted R-squared:  0.7029 
## F-statistic:  5125 on 3 and 6496 DF,  p-value: < 2.2e-16
```

```r
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
  
for (i in 1:testRange)
{
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)

  pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
  tempTest$pred <- pred$fit
  tempTest$lowerCI <- pred$lwr
  tempTest$upperCI <- pred$upr
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }

  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
  
}

metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))


dailyMetrics %<>% 
  left_join(metrics, by = "city")
```



## LME + walking

```r
modelName = 'lmeWalkingNov_' 

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend + walkingMinus7,
    random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city,
    data = train
    )
summary(model)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC   logLik
##   47247.01 47308.02 -23614.5
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev    Corr  
## casesTminus1.rate 0.1440756 cssT1.
## casesTminus2.rate 0.1316762 -0.87 
## Residual          9.0762261       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend +      walkingMinus7 
##                        Value  Std.Error   DF   t-value p-value
## (Intercept)        2.6608748 0.17047447 6471 15.608641       0
## casesTminus1.rate  0.5013509 0.03307696 6471 15.157102       0
## casesTminus2.rate  0.3624610 0.03094095 6471 11.714605       0
## weekend           -2.2951586 0.25116824 6471 -9.137933       0
## walkingMinus7      0.0171578 0.00291157 6471  5.892992       0
##  Correlation: 
##                   (Intr) cssT1. cssT2. weeknd
## casesTminus1.rate -0.088                     
## casesTminus2.rate -0.081 -0.864              
## weekend           -0.371 -0.007 -0.014       
## walkingMinus7     -0.005 -0.020 -0.013  0.054
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -9.8839751 -0.3042167 -0.1053123  0.1641670 21.0738633 
## 
## Number of Observations: 6500
## Number of Groups: 25
```

```r
# test to hold all date data points up to testRange
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate, walkingMinus7) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )


# Predict one new observation at a time
# For each new test, refit the model with actual observed Y
for (i in 1:testRange)
{
  
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  # test to check correct training window
  # print(i)
  # print(max(as.Date(train$date)))
  
  # filter test set to calculate pred for only 1 day at a time
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)
  tempTest$pred <- predict(model, newdata = tempTest)
  
  # test to check for correct test window
  # print(max(as.Date(tempTest$date)))
  
  # create design matrix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], tempTest)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  #results7$SE <- sqrt(predvar) 
  tempTest$SE <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  tempTest$lowerCI <- tempTest$pred - 1.96*tempTest$SE
  tempTest$upperCI <- tempTest$pred + 1.96*tempTest$SE
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )  
}



# pull all metrics into one dataframe called dailyMetrics
metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))

dailyMetrics %<>% 
  left_join(metrics, by = "city")
```


## LM

```r
modelName <- 'lmWalkingNov_'

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lm(case.rate ~ casesTminus1.rate + casesTminus2.rate + weekend + walkingMinus7,
    data = train
    )
summary(model)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate + 
##     weekend + walkingMinus7, data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -102.891   -2.640   -0.925    1.583  189.605 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        2.40333    0.16689  14.401  < 2e-16 ***
## casesTminus1.rate  0.46918    0.01136  41.310  < 2e-16 ***
## casesTminus2.rate  0.42121    0.01141  36.929  < 2e-16 ***
## weekend           -2.36962    0.25593  -9.259  < 2e-16 ***
## walkingMinus7      0.01208    0.00269   4.492 7.19e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9.276 on 6495 degrees of freedom
## Multiple R-squared:  0.7039,	Adjusted R-squared:  0.7037 
## F-statistic:  3860 on 4 and 6495 DF,  p-value: < 2.2e-16
```

```r
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate, walkingMinus7) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
  
for (i in 1:testRange)
{
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)

  pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
  tempTest$pred <- pred$fit
  tempTest$lowerCI <- pred$lwr
  tempTest$upperCI <- pred$upr
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }

  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
  
}

metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))


dailyMetrics %<>% 
  left_join(metrics, by = "city")
```




## LME-Log

```r
modelName = 'lmeLogNov_' 

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))


model <- lme(fixed = log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + weekend,
    random = ~ log(casesTminus1.rate) + log(casesTminus2.rate) - 1 | city,
    data = train
    , control = lmeControl(maxIter = 1e8, opt='optim') 
    )
summary(model)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   13607.74 13661.97 -6795.868
## 
## Random effects:
##  Formula: ~log(casesTminus1.rate) + log(casesTminus2.rate) - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                        StdDev      Corr  
## log(casesTminus1.rate) 0.026555697 l(T1.)
## log(casesTminus2.rate) 0.002522454 -0.081
## Residual               0.685580867       
## 
## Fixed effects: log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) +      weekend 
##                             Value  Std.Error   DF   t-value p-value
## (Intercept)             0.4059964 0.01752368 6472  23.16844       0
## log(casesTminus1.rate)  0.4435309 0.01239582 6472  35.78068       0
## log(casesTminus2.rate)  0.4069272 0.01094748 6472  37.17084       0
## weekend                -0.2590744 0.01893239 6472 -13.68419       0
##  Correlation: 
##                        (Intr) l(T1.) l(T2.)
## log(casesTminus1.rate) -0.286              
## log(casesTminus2.rate) -0.225 -0.705       
## weekend                -0.231 -0.012 -0.047
## 
## Standardized Within-Group Residuals:
##          Min           Q1          Med           Q3          Max 
## -12.57058816  -0.34748240   0.04585214   0.40528734   7.12116219 
## 
## Number of Observations: 6500
## Number of Groups: 25
```

```r
# test to hold all date data points up to testRange
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )


# Predict one new observation at a time
# For each new test, refit the model with actual observed Y
for (i in 1:testRange)
{
  
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  # test to check correct training window
  # print(i)
  # print(max(as.Date(train$date)))
  
  # filter test set to calculate pred for only 1 day at a time
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)
  tempTest$pred <- predict(model, newdata = tempTest)
  
  # test to check for correct test window
  # print(max(as.Date(tempTest$date)))
  
  # create design matrix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], tempTest)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  #results7$SE <- sqrt(predvar) 
  tempTest$SE <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  tempTest$lowerCI <- tempTest$pred - 1.96*tempTest$SE
  tempTest$upperCI <- tempTest$pred + 1.96*tempTest$SE
  
  tempTest %<>% 
    mutate(pred = exp(pred)
           , lowerCI = exp(lowerCI)
           , upperCI = exp(upperCI)
           )
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
}



# pull all metrics into one dataframe called dailyMetrics
metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))

dailyMetrics %<>% 
  left_join(metrics, by = "city")
```

## LM-Log

```r
modelName <- 'lmLogNov_'

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lm(log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + weekend,
    data = train
    )
summary(model)
```

```
## 
## Call:
## lm(formula = log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + 
##     weekend, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.6135 -0.2336  0.0293  0.2817  4.9461 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             0.40044    0.01740   23.01   <2e-16 ***
## log(casesTminus1.rate)  0.43846    0.01109   39.53   <2e-16 ***
## log(casesTminus2.rate)  0.41730    0.01087   38.37   <2e-16 ***
## weekend                -0.26077    0.01900  -13.72   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6883 on 6496 degrees of freedom
## Multiple R-squared:  0.686,	Adjusted R-squared:  0.6858 
## F-statistic:  4730 on 3 and 6496 DF,  p-value: < 2.2e-16
```

```r
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
  
for (i in 1:testRange)
{
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)

  pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
  tempTest$pred <- exp(pred$fit)
  tempTest$lowerCI <- exp(pred$lwr)
  tempTest$upperCI <- exp(pred$upr)
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }

  test %<>% 
  mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
         , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
         )
  
}

metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))


dailyMetrics %<>% 
  left_join(metrics, by = "city")
```



## LME-Log + Walking

```r
modelName = 'lmeLogWalkingNov_' 

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))


model <- lme(fixed = log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + weekend + walkingMinus7,
    random = ~ log(casesTminus1.rate) + log(casesTminus2.rate) - 1 | city,
    data = train
    , control = lmeControl(maxIter = 1e8, opt='optim') 
    )
summary(model)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   13582.33 13643.33 -6782.163
## 
## Random effects:
##  Formula: ~log(casesTminus1.rate) + log(casesTminus2.rate) - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                        StdDev     Corr  
## log(casesTminus1.rate) 0.03169980 l(T1.)
## log(casesTminus2.rate) 0.01545177 -0.175
## Residual               0.68293537       
## 
## Fixed effects: log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) +      weekend + walkingMinus7 
##                             Value   Std.Error   DF   t-value p-value
## (Intercept)             0.4246955 0.017691544 6471  24.00556       0
## log(casesTminus1.rate)  0.4350088 0.013086938 6471  33.23992       0
## log(casesTminus2.rate)  0.4009117 0.011603926 6471  34.54966       0
## weekend                -0.2496305 0.018914775 6471 -13.19764       0
## walkingMinus7           0.0015439 0.000233687 6471   6.60672       0
##  Correlation: 
##                        (Intr) l(T1.) l(T2.) weeknd
## log(casesTminus1.rate) -0.281                     
## log(casesTminus2.rate) -0.223 -0.673              
## weekend                -0.215 -0.017 -0.052       
## walkingMinus7           0.151 -0.081 -0.087  0.073
## 
## Standardized Within-Group Residuals:
##          Min           Q1          Med           Q3          Max 
## -12.65536864  -0.34666301   0.04309963   0.40650993   7.14210648 
## 
## Number of Observations: 6500
## Number of Groups: 25
```

```r
# test to hold all date data points up to testRange
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate, walkingMinus7) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )


# Predict one new observation at a time
# For each new test, refit the model with actual observed Y
for (i in 1:testRange)
{
  
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  # test to check correct training window
  # print(i)
  # print(max(as.Date(train$date)))
  
  # filter test set to calculate pred for only 1 day at a time
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)
  tempTest$pred <- predict(model, newdata = tempTest)
  
  # test to check for correct test window
  # print(max(as.Date(tempTest$date)))
  
  # create design matrix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], tempTest)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  #results7$SE <- sqrt(predvar) 
  tempTest$SE <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  tempTest$lowerCI <- tempTest$pred - 1.96*tempTest$SE
  tempTest$upperCI <- tempTest$pred + 1.96*tempTest$SE
  
  tempTest %<>% 
    mutate(pred = exp(pred)
           , lowerCI = exp(lowerCI)
           , upperCI = exp(upperCI)
           )
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }
  
  # bring forward predictions
  test %<>% 
    mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
           , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
           )
}



# pull all metrics into one dataframe called dailyMetrics
metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))

dailyMetrics %<>% 
  left_join(metrics, by = "city")
```

## LM-Log + Walking

```r
modelName <- 'lmLogWalkingNov_'

train <- alldata.groupedR %>% 
  filter(day031620 >= 0 & as.Date(date) <= (cutOff))

model <- lm(log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + weekend + walkingMinus7,
    data = train
    )
summary(model)
```

```
## 
## Call:
## lm(formula = log(case.rate) ~ log(casesTminus1.rate) + log(casesTminus2.rate) + 
##     weekend + walkingMinus7, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.5866 -0.2335  0.0290  0.2861  4.9162 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             0.4147665  0.0175605  23.619  < 2e-16 ***
## log(casesTminus1.rate)  0.4322953  0.0111264  38.853  < 2e-16 ***
## log(casesTminus2.rate)  0.4125567  0.0108857  37.899  < 2e-16 ***
## weekend                -0.2540836  0.0190004 -13.373  < 2e-16 ***
## walkingMinus7           0.0011066  0.0002035   5.437 5.61e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6868 on 6495 degrees of freedom
## Multiple R-squared:  0.6874,	Adjusted R-squared:  0.6872 
## F-statistic:  3571 on 4 and 6495 DF,  p-value: < 2.2e-16
```

```r
test <- alldata.groupedR %>% 
  filter(day031620 >= 0 
       & as.Date(date) > cutOff
       & as.Date(date) <= (cutOff + testRange)
       ) %>%
  select(city, day031620, date, weekend, case.rate, casesTminus1.rate, casesTminus2.rate, walkingMinus7) %>%
  mutate(
    casesTminus1.rate = case_when( mod(as.numeric(as.Date(date) - cutOff), 7) == 1 ~ casesTminus1.rate
         , TRUE ~ as.numeric(NA))
    , casesTminus2.rate = 
           case_when( mod(as.numeric(as.Date(date) - cutOff), 7) %in% c(1,2) ~ casesTminus2.rate
         , TRUE ~ as.numeric(NA))
         )
  
for (i in 1:testRange)
{
  if(i >1 & i %% 7 == 1) # update only when predicting on 8th, 15th, etc. days
  {
    train <- alldata.groupedR %>% 
      filter(day031620 >= 0 & as.Date(date) <= (cutOff - 1 + i))

    model <- update(model, data = train)
  }
  
  
  tempTest <- test %>% filter(as.Date(date) == cutOff + i)

  pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
  tempTest$pred <- exp(pred$fit)
  tempTest$lowerCI <- exp(pred$lwr)
  tempTest$upperCI <- exp(pred$upr)
  
  test %<>%
    left_join(tempTest %>% select(city, date, pred, lowerCI, upperCI)
              , by = c("date" = "date", "city" = "city"))
   
  if(i > 1)
  {
    test %<>% 
      mutate(pred = coalesce(pred.x, pred.y)
             , lowerCI = coalesce(lowerCI.x, lowerCI.y)
             , upperCI = coalesce(upperCI.x, upperCI.y)
             ) %>%
      select(-pred.x, -pred.y
             , -lowerCI.x, -lowerCI.y
             , -upperCI.x, -upperCI.y)
  }

  test %<>% 
  mutate(casesTminus1.rate =  case_when(is.na(casesTminus1.rate) ~ lag(as.numeric(pred)), TRUE ~ casesTminus1.rate)
         , casesTminus2.rate =  case_when(is.na(casesTminus2.rate) ~ lag(as.numeric(pred), 2), TRUE ~ casesTminus2.rate)
         )
  
}

metrics <- test %>% 
    filter(as.Date(date) <= cutOff + 7) %>%
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( test %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) %>%
    rename_at(vars(-city), ~ paste0(modelName,.)) %>%
    arrange(as.character(city))


dailyMetrics %<>% 
  left_join(metrics, by = "city")
```






# Metrics

```r
print(dailyMetrics %>% as.data.frame())
```

```
##             city lmeAug_MAE_7d lmeAug_MAPE_7d lmeAug_CP_7d lmeAug_MAE_14d
## 1    Albuquerque          2.04           1.26         1.00           1.85
## 2        Atlanta          5.13           0.48         1.00           4.34
## 3      Baltimore          2.85           0.23         1.00           3.42
## 4         Boston          2.23          81.49         1.00           1.97
## 5      Charlotte          2.93           0.27         1.00           2.96
## 6        Chicago          6.11           0.40         0.86           3.75
## 7      Cleveland          1.87           0.32         1.00           2.06
## 8         Dallas          4.63           1.28         1.00           5.32
## 9         Denver          1.64           0.36         1.00           1.45
## 10       Detroit          2.90          65.00         1.00           2.91
## 11       Houston          3.18           1.62         0.86           6.87
## 12  Indianapolis          1.68           0.20         1.00           3.35
## 13   Los Angeles          2.56           0.32         1.00           2.14
## 14    Louisville         19.65           1.20         0.71          14.79
## 15       Memphis          4.81           0.55         0.86           5.10
## 16         Miami          8.83           0.31         0.86           5.15
## 17      New York          2.95           1.28         1.00           2.99
## 18      Oklahoma          4.95           0.24         0.86           5.95
## 19       Phoenix          4.37           1.16         0.86           3.33
## 20    Pittsburgh          1.40           0.36         1.00           1.72
## 21      Portland          2.20           0.65         1.00           1.79
## 22    Sacramento          8.71           1.36         0.71           7.31
## 23 San Francisco          1.78           0.48         1.00           1.46
## 24       Seattle          2.22           0.67         1.00           1.53
## 25         Tampa          3.04           0.28         1.00           2.32
##    lmeAug_MAPE_14d lmeAug_CP_14d lmAug_MAE_7d lmAug_MAPE_7d lmAug_CP_7d
## 1             1.04          1.00         2.01          1.25        1.00
## 2             0.38          1.00         8.65          0.63        0.71
## 3             0.41          1.00         7.20          0.60        1.00
## 4            40.99          1.00         2.21         57.01        1.00
## 5             0.29          1.00         9.53          0.63        0.57
## 6             0.26          0.93         9.30          0.59        0.71
## 7             0.46          1.00         2.10          0.27        1.00
## 8             0.86          0.86         6.02          0.79        0.86
## 9             0.32          1.00         1.53          0.27        1.00
## 10           77.31          1.00         4.17         51.56        1.00
## 11            1.09          0.64         9.62          1.07        0.71
## 12            0.30          0.93         6.27          0.55        1.00
## 13            0.29          1.00         5.75          0.52        1.00
## 14            1.08          0.64        22.62          0.70        0.43
## 15            0.65          0.93         9.70          0.64        0.71
## 16            0.25          0.93        14.39          0.64        0.57
## 17            1.31          1.00         2.22          0.96        1.00
## 18            0.29          0.86        12.17          0.69        0.43
## 19            1.10          0.93         4.82          0.99        0.86
## 20           17.15          1.00         1.62          0.31        1.00
## 21            0.52          1.00         1.36          0.34        1.00
## 22            1.35          0.86         5.85          0.57        0.86
## 23            0.32          1.00         5.17          0.59        1.00
## 24            0.45          1.00         0.94          0.30        1.00
## 25            0.24          1.00         6.15          0.50        0.86
##    lmAug_MAE_14d lmAug_MAPE_14d lmAug_CP_14d lmeWalkingAug_MAE_7d
## 1           1.73           0.98         1.00                 2.79
## 2           7.87           0.62         0.79                 4.74
## 3           5.85           0.56         1.00                 2.21
## 4           1.82          28.63         1.00                 2.52
## 5           8.77           0.64         0.71                 2.98
## 6           7.94           0.61         0.86                 6.27
## 7           1.55           0.23         1.00                 2.28
## 8           6.69           0.68         0.79                 4.93
## 9           1.53           0.26         1.00                 2.01
## 10          4.98          47.67         1.00                 2.46
## 11         11.73           0.92         0.50                 3.41
## 12          6.95           0.55         0.93                 1.78
## 13          4.78           0.49         1.00                 2.55
## 14         16.57           0.74         0.50                19.71
## 15          9.92           0.64         0.57                 5.10
## 16         10.67           0.63         0.79                 8.95
## 17          1.90           0.83         1.00                 2.70
## 18         13.80           0.74         0.29                 4.92
## 19          3.74           0.92         0.93                 4.31
## 20          1.81          12.08         1.00                 1.74
## 21          1.30           0.30         1.00                 2.54
## 22          5.49           0.60         0.86                 8.60
## 23          4.33           0.53         1.00                 1.78
## 24          0.83           0.24         1.00                 2.65
## 25          5.36           0.52         0.93                 3.02
##    lmeWalkingAug_MAPE_7d lmeWalkingAug_CP_7d lmeWalkingAug_MAE_14d
## 1                   1.69                1.00                  2.65
## 2                   0.47                1.00                  3.86
## 3                   0.18                1.00                  3.44
## 4                  85.47                1.00                  2.37
## 5                   0.29                1.00                  3.10
## 6                   0.43                0.86                  3.91
## 7                   0.43                1.00                  2.68
## 8                   1.39                0.86                  5.24
## 9                   0.46                1.00                  1.77
## 10                 85.01                1.00                  2.77
## 11                  1.70                0.86                  6.57
## 12                  0.22                1.00                  3.35
## 13                  0.35                1.00                  2.11
## 14                  1.22                0.71                 14.50
## 15                  0.57                0.86                  5.31
## 16                  0.33                0.86                  5.32
## 17                  1.17                1.00                  2.73
## 18                  0.24                0.86                  5.91
## 19                  1.25                1.00                  3.28
## 20                  0.46                1.00                  1.96
## 21                  0.73                1.00                  2.14
## 22                  1.35                0.71                  7.26
## 23                  0.45                1.00                  1.39
## 24                  0.79                1.00                  2.02
## 25                  0.29                1.00                  2.43
##    lmeWalkingAug_MAPE_14d lmeWalkingAug_CP_14d lmWalkingAug_MAE_7d
## 1                    1.44                 1.00                2.23
## 2                    0.36                 1.00                8.52
## 3                    0.42                 1.00                6.96
## 4                   43.05                 1.00                2.18
## 5                    0.32                 1.00                9.29
## 6                    0.30                 0.93                9.06
## 7                    0.61                 1.00                2.09
## 8                    0.91                 0.79                5.89
## 9                    0.40                 1.00                1.54
## 10                  96.79                 1.00                4.03
## 11                   1.11                 0.71                9.49
## 12                   0.32                 0.93                6.04
## 13                   0.31                 1.00                5.62
## 14                   1.09                 0.71               22.50
## 15                   0.69                 0.86                9.46
## 16                   0.27                 0.93               14.15
## 17                   1.19                 1.00                2.46
## 18                   0.29                 0.86               11.94
## 19                   1.18                 1.00                4.79
## 20                  21.18                 1.00                1.57
## 21                   0.61                 1.00                1.52
## 22                   1.36                 0.86                5.70
## 23                   0.30                 1.00                5.03
## 24                   0.58                 1.00                1.07
## 25                   0.26                 1.00                5.92
##    lmWalkingAug_MAPE_7d lmWalkingAug_CP_7d lmWalkingAug_MAE_14d
## 1                  1.37               1.00                 1.93
## 2                  0.63               0.71                 7.72
## 3                  0.58               1.00                 5.64
## 4                 58.76               1.00                 1.77
## 5                  0.61               0.57                 8.56
## 6                  0.57               0.71                 7.73
## 7                  0.28               1.00                 1.54
## 8                  0.80               0.86                 6.54
## 9                  0.28               1.00                 1.45
## 10                55.66               1.00                 4.85
## 11                 1.10               0.71                11.57
## 12                 0.52               1.00                 6.74
## 13                 0.52               1.00                 4.62
## 14                 0.70               0.43                16.43
## 15                 0.63               0.71                 9.75
## 16                 0.62               0.71                10.46
## 17                 1.06               1.00                 2.11
## 18                 0.67               0.43                13.59
## 19                 1.04               0.86                 3.68
## 20                 0.31               1.00                 1.76
## 21                 0.39               1.00                 1.35
## 22                 0.56               0.86                 5.36
## 23                 0.60               1.00                 4.16
## 24                 0.35               1.00                 0.90
## 25                 0.48               0.86                 5.15
##    lmWalkingAug_MAPE_14d lmWalkingAug_CP_14d lmeLogAug_MAE_7d lmeLogAug_MAPE_7d
## 1                   1.09                1.00             1.18              0.68
## 2                   0.61                0.79             5.64              0.48
## 3                   0.54                1.00             3.03              0.25
## 4                  29.50                1.00             2.08             68.92
## 5                   0.63                0.71             3.57              0.29
## 6                   0.59                0.86             6.00              0.39
## 7                   0.23                1.00             1.58              0.21
## 8                   0.68                0.79             4.26              1.04
## 9                   0.25                1.00             1.20              0.25
## 10                 51.33                1.00             3.61             54.65
## 11                  0.93                0.50             3.21              1.37
## 12                  0.53                0.93             1.77              0.19
## 13                  0.48                1.00             2.47              0.30
## 14                  0.74                0.50            19.87              0.77
## 15                  0.63                0.57             4.57              0.53
## 16                  0.61                0.86             8.53              0.27
## 17                  0.92                1.00             0.97              0.44
## 18                  0.73                0.29             5.25              0.24
## 19                  0.96                0.93             4.96              0.94
## 20                 12.96                1.00             1.76              0.34
## 21                  0.32                1.00             1.20              0.33
## 22                  0.60                0.86             7.06              1.02
## 23                  0.51                1.00             1.90              0.43
## 24                  0.27                1.00             1.16              0.35
## 25                  0.49                0.93             3.11              0.26
##    lmeLogAug_CP_7d lmeLogAug_MAE_14d lmeLogAug_MAPE_14d lmeLogAug_CP_14d
## 1             0.86              0.87               0.45             0.93
## 2             1.00              5.07               0.43             1.00
## 3             1.00              3.53               0.42             1.00
## 4             0.86              1.84              34.63             0.93
## 5             1.00              3.49               0.32             1.00
## 6             1.00              3.69               0.26             1.00
## 7             1.00              1.44               0.28             1.00
## 8             0.86              5.99               0.82             0.71
## 9             1.00              1.27               0.25             1.00
## 10            0.86              5.38              41.45             0.43
## 11            0.86              8.50               1.07             0.50
## 12            1.00              3.51               0.29             0.93
## 13            1.00              2.24               0.29             1.00
## 14            0.71             15.38               0.87             0.50
## 15            0.86              5.06               0.59             0.86
## 16            0.86              4.90               0.21             0.93
## 17            1.00              0.92               0.42             1.00
## 18            0.86              6.62               0.32             0.93
## 19            0.71              4.14               0.87             0.79
## 20            1.00              1.99              13.69             0.93
## 21            1.00              1.32               0.33             1.00
## 22            0.86              6.12               1.07             0.86
## 23            0.86              1.42               0.28             0.93
## 24            1.00              0.98               0.26             1.00
## 25            1.00              2.31               0.22             1.00
##    lmLogAug_MAE_7d lmLogAug_MAPE_7d lmLogAug_CP_7d lmLogAug_MAE_14d
## 1             1.33             0.78           0.86             0.96
## 2             9.51             0.66           0.14             9.16
## 3             8.45             0.71           0.14             7.34
## 4             2.18            40.72           0.86             2.46
## 5            10.78             0.73           0.14            10.25
## 6            10.56             0.70           0.57             9.42
## 7             3.17             0.43           0.86             2.72
## 8             6.85             0.77           0.43             7.96
## 9             2.19             0.35           1.00             2.72
## 10            5.17            40.58           0.71             6.15
## 11           10.46             0.97           0.00            13.00
## 12            7.53             0.67           0.14             8.43
## 13            6.77             0.61           0.43             6.14
## 14           23.45             0.69           0.43            17.70
## 15           10.96             0.74           0.14            11.35
## 16           15.65             0.74           0.14            12.15
## 17            0.96             0.43           1.00             0.70
## 18           13.43             0.77           0.14            15.29
## 19            5.40             0.85           0.71             4.46
## 20            2.25             0.36           1.00             2.81
## 21            1.36             0.27           1.00             1.94
## 22            6.76             0.61           0.57             6.57
## 23            6.17             0.64           0.57             5.68
## 24            1.18             0.28           1.00             1.64
## 25            7.40             0.64           0.57             6.84
##    lmLogAug_MAPE_14d lmLogAug_CP_14d lmeLogWalkingAug_MAE_7d
## 1               0.51            0.93                    1.55
## 2               0.72            0.14                    5.11
## 3               0.72            0.07                    1.64
## 4              20.61            0.86                    2.08
## 5               0.77            0.07                    2.92
## 6               0.74            0.29                    6.42
## 7               0.45            0.93                    1.54
## 8               0.76            0.36                    4.66
## 9               0.47            0.93                    1.66
## 10             34.58            0.43                    2.80
## 11              0.92            0.00                    3.08
## 12              0.71            0.21                    1.97
## 13              0.65            0.36                    2.40
## 14              0.73            0.29                   19.94
## 15              0.75            0.14                    5.77
## 16              0.76            0.14                    8.72
## 17              0.31            1.00                    0.87
## 18              0.83            0.07                    5.25
## 19              0.78            0.71                    4.92
## 20              8.04            0.79                    1.64
## 21              0.37            0.93                    1.34
## 22              0.64            0.50                    7.48
## 23              0.67            0.43                    2.00
## 24              0.37            1.00                    1.52
## 25              0.69            0.36                    3.12
##    lmeLogWalkingAug_MAPE_7d lmeLogWalkingAug_CP_7d lmeLogWalkingAug_MAE_14d
## 1                      0.93                   0.86                     1.11
## 2                      0.48                   1.00                     4.45
## 3                      0.14                   1.00                     3.58
## 4                     73.52                   0.86                     1.87
## 5                      0.28                   1.00                     2.97
## 6                      0.48                   1.00                     3.97
## 7                      0.24                   1.00                     1.55
## 8                      1.27                   0.86                     5.92
## 9                      0.37                   1.00                     1.32
## 10                    72.51                   0.86                     4.78
## 11                     1.51                   0.86                     8.17
## 12                     0.23                   1.00                     3.37
## 13                     0.35                   1.00                     2.03
## 14                     0.81                   0.71                    15.25
## 15                     0.63                   0.86                     5.68
## 16                     0.31                   0.86                     4.98
## 17                     0.39                   1.00                     0.81
## 18                     0.24                   0.86                     6.65
## 19                     1.02                   0.71                     4.07
## 20                     0.36                   1.00                     1.88
## 21                     0.37                   1.00                     1.35
## 22                     1.11                   0.86                     6.43
## 23                     0.43                   0.86                     1.48
## 24                     0.48                   1.00                     1.13
## 25                     0.28                   1.00                     2.26
##    lmeLogWalkingAug_MAPE_14d lmeLogWalkingAug_CP_14d lmLogWalkingAug_MAE_7d
## 1                       0.62                    0.93                   1.61
## 2                       0.39                    1.00                   9.29
## 3                       0.45                    1.00                   8.07
## 4                      36.97                    0.93                   2.16
## 5                       0.31                    1.00                  10.40
## 6                       0.32                    1.00                  10.17
## 7                       0.33                    1.00                   2.80
## 8                       0.91                    0.71                   6.66
## 9                       0.30                    1.00                   1.96
## 10                     55.29                    0.57                   4.93
## 11                      1.12                    0.50                  10.26
## 12                      0.31                    0.93                   7.15
## 13                      0.30                    1.00                   6.38
## 14                      0.89                    0.57                  23.26
## 15                      0.71                    0.86                  10.57
## 16                      0.23                    0.93                  15.26
## 17                      0.37                    1.00                   1.34
## 18                      0.32                    0.93                  13.05
## 19                      0.92                    0.79                   5.34
## 20                     16.33                    0.93                   2.08
## 21                      0.34                    1.00                   1.25
## 22                      1.17                    0.86                   6.52
## 23                      0.27                    0.93                   5.93
## 24                      0.33                    1.00                   1.14
## 25                      0.22                    1.00                   7.02
##    lmLogWalkingAug_MAPE_7d lmLogWalkingAug_CP_7d lmLogWalkingAug_MAE_14d
## 1                     0.97                  0.86                    1.13
## 2                     0.65                  0.43                    8.94
## 3                     0.68                  0.29                    7.04
## 4                    43.12                  0.86                    2.34
## 5                     0.70                  0.29                    9.96
## 6                     0.67                  0.57                    9.12
## 7                     0.37                  1.00                    2.43
## 8                     0.79                  0.57                    7.76
## 9                     0.32                  1.00                    2.49
## 10                   46.67                  0.71                    5.97
## 11                    1.03                  0.00                   12.80
## 12                    0.63                  0.29                    8.13
## 13                    0.56                  0.57                    5.84
## 14                    0.70                  0.43                   17.51
## 15                    0.71                  0.14                   11.05
## 16                    0.71                  0.29                   11.86
## 17                    0.60                  1.00                    0.88
## 18                    0.74                  0.14                   14.99
## 19                    0.93                  0.71                    4.38
## 20                    0.34                  1.00                    2.66
## 21                    0.26                  1.00                    1.78
## 22                    0.61                  0.71                    6.38
## 23                    0.65                  0.71                    5.45
## 24                    0.29                  1.00                    1.51
## 25                    0.60                  0.57                    6.54
##    lmLogWalkingAug_MAPE_14d lmLogWalkingAug_CP_14d lmeNov_MAE_7d lmeNov_MAPE_7d
## 1                      0.63                   0.93         20.32           0.22
## 2                      0.71                   0.29         32.14           0.70
## 3                      0.69                   0.29         12.74           0.29
## 4                     21.78                   0.93         43.25           0.63
## 5                      0.74                   0.14         22.98           0.40
## 6                      0.72                   0.36         31.90           0.41
## 7                      0.40                   1.00         15.24           0.20
## 8                      0.76                   0.43         16.32           0.25
## 9                      0.43                   1.00         17.04           0.22
## 10                    39.44                   0.43         28.03           2.96
## 11                     0.94                   0.00         17.66           0.52
## 12                     0.68                   0.29         28.74           0.26
## 13                     0.61                   0.50         35.82           0.47
## 14                     0.73                   0.29         30.25           0.39
## 15                     0.73                   0.14         12.73           0.24
## 16                     0.73                   0.21         13.60           0.22
## 17                     0.39                   1.00          9.03           0.26
## 18                     0.81                   0.07         49.10           0.52
## 19                     0.83                   0.71         58.65           0.78
## 20                     9.02                   0.93         40.51           0.52
## 21                     0.34                   0.93          5.89           0.20
## 22                     0.64                   0.57         24.05           0.42
## 23                     0.65                   0.64         14.02           0.39
## 24                     0.35                   1.00         14.56           0.54
## 25                     0.65                   0.43         13.28           0.39
##    lmeNov_CP_7d lmeNov_MAE_14d lmeNov_MAPE_14d lmeNov_CP_14d lmNov_MAE_7d
## 1          0.57          14.78            0.18          0.71        27.40
## 2          0.14          31.33            0.63          0.21        16.55
## 3          0.71          12.02            0.27          0.79        22.01
## 4          0.14          37.23            0.53          0.14        17.47
## 5          0.43          18.80            0.31          0.57        14.38
## 6          0.00          19.10            0.25          0.50        15.75
## 7          0.86          24.31            0.21          0.71        16.11
## 8          0.57          23.43            0.30          0.64        10.55
## 9          0.71          16.08            0.29          0.64        18.41
## 10         0.14          24.04            3.54          0.29        17.56
## 11         0.43          17.50            0.46          0.57        27.15
## 12         0.14          21.62            0.20          0.43        40.58
## 13         0.14          32.15            0.37          0.29        17.83
## 14         0.14          29.14            0.38          0.29        14.15
## 15         0.71          23.28            0.30          0.57        18.40
## 16         0.71          11.21            0.18          0.86         5.07
## 17         1.00           7.87            0.23          1.00        29.41
## 18         0.14          43.55            0.48          0.29        31.69
## 19         0.29          74.69            0.77          0.21        36.65
## 20         0.14          39.96            0.45          0.29        22.20
## 21         0.86           5.28            0.17          0.93        26.47
## 22         0.43          23.98            0.67          0.43        20.62
## 23         0.57          13.55            0.36          0.71        31.51
## 24         0.86          10.94            0.38          0.93        38.12
## 25         0.86          14.91            0.37          0.71        28.97
##    lmNov_MAPE_7d lmNov_CP_7d lmNov_MAE_14d lmNov_MAPE_14d lmNov_CP_14d
## 1           0.27        0.43         20.93           0.23         0.57
## 2           0.46        0.57         14.50           0.36         0.64
## 3           0.62        0.57         22.91           0.62         0.50
## 4           0.30        0.71         12.86           0.21         0.86
## 5           0.33        0.57         11.91           0.25         0.79
## 6           0.19        0.71         12.06           0.16         0.86
## 7           0.22        0.86         28.29           0.26         0.64
## 8           0.18        1.00         19.14           0.29         0.86
## 9           0.24        0.57         15.89           0.27         0.64
## 10          5.19        0.71         18.99           7.14         0.64
## 11          0.95        0.29         29.28           1.00         0.21
## 12          0.38        0.14         35.61           0.34         0.21
## 13          0.24        0.43         25.61           0.27         0.36
## 14          0.19        0.57         14.94           0.22         0.64
## 15          0.42        0.57         23.63           0.41         0.50
## 16          0.08        1.00          6.40           0.11         1.00
## 17          0.95        0.14         31.19           0.99         0.07
## 18          0.39        0.43         27.01           0.32         0.50
## 19          1.52        0.43         45.93           1.25         0.36
## 20          0.30        0.43         26.41           0.30         0.43
## 21          0.81        0.14         29.59           0.92         0.07
## 22          0.61        0.57         20.40           0.91         0.57
## 23          1.45        0.14         31.71           1.24         0.14
## 24          1.93        0.00         33.64           1.38         0.07
## 25          0.93        0.14         26.03           0.79         0.29
##    lmeWalkingNov_MAE_7d lmeWalkingNov_MAPE_7d lmeWalkingNov_CP_7d
## 1                 21.23                  0.23                0.57
## 2                 32.16                  0.70                0.14
## 3                 13.67                  0.31                0.71
## 4                 43.50                  0.64                0.14
## 5                 24.18                  0.42                0.43
## 6                 32.81                  0.42                0.00
## 7                 16.29                  0.21                0.71
## 8                 17.45                  0.26                0.57
## 9                 18.40                  0.24                0.71
## 10                28.84                  2.83                0.14
## 11                17.96                  0.53                0.29
## 12                30.81                  0.28                0.14
## 13                35.96                  0.47                0.14
## 14                31.73                  0.42                0.14
## 15                12.79                  0.23                0.71
## 16                12.90                  0.21                0.71
## 17                 8.94                  0.26                1.00
## 18                49.73                  0.52                0.14
## 19                58.58                  0.77                0.29
## 20                42.04                  0.54                0.14
## 21                 6.01                  0.20                0.86
## 22                25.19                  0.44                0.43
## 23                14.48                  0.40                0.57
## 24                14.42                  0.52                0.86
## 25                13.44                  0.40                0.86
##    lmeWalkingNov_MAE_14d lmeWalkingNov_MAPE_14d lmeWalkingNov_CP_14d
## 1                  15.21                   0.18                 0.71
## 2                  31.36                   0.63                 0.14
## 3                  12.60                   0.28                 0.71
## 4                  37.09                   0.53                 0.14
## 5                  19.72                   0.32                 0.57
## 6                  19.54                   0.26                 0.50
## 7                  24.95                   0.22                 0.64
## 8                  24.11                   0.31                 0.64
## 9                  16.62                   0.29                 0.64
## 10                 24.46                   3.45                 0.29
## 11                 17.51                   0.46                 0.50
## 12                 22.74                   0.22                 0.36
## 13                 31.92                   0.36                 0.29
## 14                 29.96                   0.40                 0.29
## 15                 23.46                   0.30                 0.57
## 16                 10.42                   0.17                 0.86
## 17                  7.72                   0.22                 1.00
## 18                 44.31                   0.49                 0.29
## 19                 74.51                   0.77                 0.21
## 20                 41.19                   0.47                 0.29
## 21                  5.33                   0.17                 0.93
## 22                 24.88                   0.67                 0.43
## 23                 13.76                   0.36                 0.71
## 24                 10.93                   0.37                 0.93
## 25                 14.93                   0.37                 0.71
##    lmWalkingNov_MAE_7d lmWalkingNov_MAPE_7d lmWalkingNov_CP_7d
## 1                28.11                 0.28               0.29
## 2                15.85                 0.44               0.57
## 3                21.31                 0.61               0.57
## 4                17.86                 0.31               0.71
## 5                14.49                 0.33               0.57
## 6                16.46                 0.20               0.71
## 7                16.52                 0.22               0.86
## 8                10.73                 0.18               1.00
## 9                19.12                 0.25               0.57
## 10               17.34                 5.08               0.71
## 11               26.45                 0.93               0.43
## 12               41.28                 0.39               0.14
## 13               18.50                 0.24               0.43
## 14               14.57                 0.19               0.57
## 15               18.19                 0.41               0.43
## 16                5.64                 0.09               1.00
## 17               28.71                 0.93               0.14
## 18               31.78                 0.39               0.43
## 19               36.76                 1.50               0.43
## 20               22.84                 0.31               0.29
## 21               25.77                 0.78               0.29
## 22               20.51                 0.61               0.57
## 23               30.81                 1.43               0.29
## 24               37.70                 1.89               0.00
## 25               28.27                 0.91               0.14
##    lmWalkingNov_MAE_14d lmWalkingNov_MAPE_14d lmWalkingNov_CP_14d
## 1                 21.20                  0.23                0.50
## 2                 14.24                  0.36                0.64
## 3                 22.66                  0.61                0.50
## 4                 13.01                  0.21                0.86
## 5                 11.94                  0.25                0.79
## 6                 12.40                  0.17                0.86
## 7                 28.40                  0.26                0.64
## 8                 19.24                  0.29                0.86
## 9                 16.23                  0.27                0.64
## 10                18.97                  7.10                0.57
## 11                28.97                  0.99                0.29
## 12                35.87                  0.34                0.21
## 13                25.85                  0.28                0.36
## 14                15.11                  0.22                0.64
## 15                23.50                  0.40                0.43
## 16                 6.77                  0.12                1.00
## 17                30.93                  0.98                0.07
## 18                26.98                  0.32                0.50
## 19                45.92                  1.24                0.36
## 20                26.64                  0.30                0.36
## 21                29.33                  0.91                0.14
## 22                20.36                  0.91                0.57
## 23                31.41                  1.23                0.21
## 24                33.51                  1.36                0.07
## 25                25.77                  0.78                0.29
##    lmeLogNov_MAE_7d lmeLogNov_MAPE_7d lmeLogNov_CP_7d lmeLogNov_MAE_14d
## 1             43.60              0.48            1.00             36.78
## 2             29.07              0.63            1.00             28.50
## 3             15.62              0.36            1.00             15.21
## 4             48.62              0.72            0.43             45.75
## 5             26.98              0.46            1.00             25.43
## 6             40.12              0.52            1.00             28.07
## 7             41.53              0.57            0.71             54.47
## 8             22.58              0.35            1.00             28.14
## 9             33.66              0.42            1.00             23.22
## 10            48.56              1.25            0.14             42.42
## 11            13.00              0.41            1.00             13.11
## 12            54.72              0.52            1.00             46.89
## 13            40.95              0.53            0.86             44.28
## 14            41.79              0.56            1.00             38.77
## 15            14.68              0.23            1.00             27.81
## 16            29.20              0.48            1.00             26.91
## 17            13.21              0.39            1.00             12.32
## 18            54.47              0.58            0.57             50.35
## 19            60.53              0.68            0.29             76.29
## 20            51.02              0.67            0.71             54.80
## 21            12.79              0.36            1.00             11.96
## 22            27.57              0.49            0.71             28.77
## 23            15.38              0.41            0.86             15.28
## 24            13.61              0.31            0.86             12.89
## 25            17.45              0.52            1.00             20.05
##    lmeLogNov_MAPE_14d lmeLogNov_CP_14d lmLogNov_MAE_7d lmLogNov_MAPE_7d
## 1                0.43             1.00           47.07             0.52
## 2                0.57             1.00           10.63             0.23
## 3                0.35             1.00           16.50             0.44
## 4                0.67             0.64           29.02             0.43
## 5                0.41             1.00           22.25             0.42
## 6                0.38             1.00           35.89             0.47
## 7                0.59             0.79           29.17             0.38
## 8                0.35             0.93           23.43             0.37
## 9                0.31             1.00           35.15             0.45
## 10               1.44             0.21           24.62             2.53
## 11               0.35             1.00           13.81             0.54
## 12               0.45             1.00           61.57             0.59
## 13               0.50             0.93           33.12             0.42
## 14               0.52             1.00           33.13             0.45
## 15               0.34             0.93           16.04             0.25
## 16               0.45             1.00           19.93             0.33
## 17               0.37             1.00           13.54             0.43
## 18               0.56             0.71           44.30             0.49
## 19               0.74             0.21           40.45             0.81
## 20               0.64             0.64           35.81             0.45
## 21               0.35             1.00           12.61             0.40
## 22               0.59             0.79           20.51             0.44
## 23               0.37             0.93           22.94             1.01
## 24               0.32             0.93           24.88             0.94
## 25               0.51             1.00           14.06             0.45
##    lmLogNov_CP_7d lmLogNov_MAE_14d lmLogNov_MAPE_14d lmLogNov_CP_14d
## 1            1.00            40.17              0.47            1.00
## 2            1.00            11.81              0.24            1.00
## 3            1.00            14.86              0.38            1.00
## 4            1.00            28.04              0.41            1.00
## 5            1.00            21.80              0.37            1.00
## 6            1.00            28.38              0.40            1.00
## 7            0.86            45.85              0.46            0.93
## 8            1.00            26.97              0.33            1.00
## 9            1.00            25.41              0.35            1.00
## 10           0.86            19.44              3.38            0.86
## 11           1.00            15.67              0.53            1.00
## 12           0.86            57.14              0.56            0.93
## 13           1.00            44.28              0.48            0.93
## 14           1.00            32.39              0.45            1.00
## 15           1.00            26.54              0.33            0.93
## 16           1.00            18.28              0.31            1.00
## 17           1.00            14.30              0.45            1.00
## 18           0.86            40.13              0.45            0.93
## 19           1.00            52.23              0.74            0.93
## 20           1.00            41.13              0.46            1.00
## 21           1.00            12.35              0.38            1.00
## 22           1.00            23.53              0.61            0.93
## 23           1.00            21.79              0.79            1.00
## 24           1.00            18.00              0.63            1.00
## 25           1.00            14.53              0.43            1.00
##    lmeLogWalkingNov_MAE_7d lmeLogWalkingNov_MAPE_7d lmeLogWalkingNov_CP_7d
## 1                    48.03                     0.53                   0.86
## 2                    29.39                     0.63                   1.00
## 3                    16.90                     0.40                   1.00
## 4                    49.74                     0.74                   0.43
## 5                    27.92                     0.48                   0.86
## 6                    42.89                     0.56                   1.00
## 7                    45.20                     0.62                   0.57
## 8                    24.32                     0.38                   1.00
## 9                    37.15                     0.47                   0.86
## 10                   49.27                     1.18                   0.14
## 11                   13.53                     0.43                   1.00
## 12                   59.02                     0.56                   0.86
## 13                   41.45                     0.54                   0.86
## 14                   44.64                     0.60                   0.86
## 15                   14.86                     0.23                   1.00
## 16                   27.15                     0.45                   1.00
## 17                   13.62                     0.41                   1.00
## 18                   56.95                     0.62                   0.57
## 19                   60.53                     0.68                   0.29
## 20                   53.77                     0.71                   0.29
## 21                   14.93                     0.42                   1.00
## 22                   29.46                     0.53                   0.71
## 23                   16.40                     0.45                   0.86
## 24                   13.45                     0.28                   0.86
## 25                   17.77                     0.53                   1.00
##    lmeLogWalkingNov_MAE_14d lmeLogWalkingNov_MAPE_14d lmeLogWalkingNov_CP_14d
## 1                     39.26                      0.46                    0.93
## 2                     28.61                      0.57                    1.00
## 3                     15.95                      0.37                    1.00
## 4                     46.92                      0.68                    0.57
## 5                     25.59                      0.42                    0.93
## 6                     30.06                      0.41                    1.00
## 7                     58.03                      0.63                    0.57
## 8                     28.79                      0.36                    0.93
## 9                     24.83                      0.34                    0.93
## 10                    42.95                      1.37                    0.21
## 11                    12.96                      0.35                    1.00
## 12                    49.98                      0.48                    0.93
## 13                    44.33                      0.50                    0.93
## 14                    40.56                      0.54                    0.93
## 15                    27.18                      0.33                    0.93
## 16                    24.66                      0.41                    1.00
## 17                    12.69                      0.38                    1.00
## 18                    53.15                      0.60                    0.64
## 19                    76.13                      0.74                    0.21
## 20                    57.80                      0.68                    0.43
## 21                    13.48                      0.39                    1.00
## 22                    30.28                      0.61                    0.79
## 23                    15.91                      0.39                    0.86
## 24                    12.96                      0.31                    0.93
## 25                    20.27                      0.51                    1.00
##    lmLogWalkingNov_MAE_7d lmLogWalkingNov_MAPE_7d lmLogWalkingNov_CP_7d
## 1                   48.41                    0.54                  0.86
## 2                   11.68                    0.24                  1.00
## 3                   16.99                    0.44                  1.00
## 4                   30.28                    0.45                  1.00
## 5                   23.77                    0.45                  1.00
## 6                   37.23                    0.49                  1.00
## 7                   30.77                    0.40                  0.86
## 8                   24.70                    0.39                  1.00
## 9                   36.74                    0.47                  0.86
## 10                  25.42                    2.39                  0.86
## 11                  14.31                    0.56                  1.00
## 12                  62.91                    0.60                  0.86
## 13                  34.71                    0.44                  0.86
## 14                  34.48                    0.47                  1.00
## 15                  17.05                    0.27                  1.00
## 16                  21.20                    0.35                  1.00
## 17                  13.13                    0.42                  1.00
## 18                  44.61                    0.48                  0.86
## 19                  40.97                    0.78                  1.00
## 20                  37.33                    0.47                  0.86
## 21                  12.54                    0.39                  1.00
## 22                  21.57                    0.46                  1.00
## 23                  22.87                    1.00                  1.00
## 24                  23.88                    0.87                  1.00
## 25                  13.99                    0.45                  1.00
##    lmLogWalkingNov_MAE_14d lmLogWalkingNov_MAPE_14d lmLogWalkingNov_CP_14d
## 1                    40.42                     0.48                   0.93
## 2                    11.94                     0.24                   1.00
## 3                    15.01                     0.38                   1.00
## 4                    28.25                     0.41                   1.00
## 5                    22.21                     0.38                   1.00
## 6                    28.69                     0.40                   1.00
## 7                    46.16                     0.47                   0.93
## 8                    27.34                     0.34                   1.00
## 9                    25.84                     0.36                   0.93
## 10                   19.77                     3.36                   0.86
## 11                   15.88                     0.54                   1.00
## 12                   57.32                     0.56                   0.93
## 13                   44.59                     0.48                   0.86
## 14                   32.63                     0.45                   1.00
## 15                   26.74                     0.34                   0.93
## 16                   18.56                     0.31                   1.00
## 17                   14.11                     0.44                   1.00
## 18                   39.85                     0.44                   0.93
## 19                   52.16                     0.72                   0.93
## 20                   41.40                     0.46                   0.93
## 21                   12.43                     0.38                   1.00
## 22                   23.85                     0.63                   0.93
## 23                   21.87                     0.79                   1.00
## 24                   17.73                     0.60                   1.00
## 25                   14.13                     0.42                   1.00
```























































