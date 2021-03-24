---
title: "weeklyForecast"
output: 
  #rmarkdown::github_document
  html_document:
    toc: true
    number_sections: true
    keep_md: true
---






# Automate extracting 95% CI for prediction band, metrics, and plotting

Predict one day at a time, feedforward the prediction as lag case.rate. 


```r
forecastLME <- function(model, test) # provide model obj and test set
{
  for (i in 1:nrow(test))
  {
    
    pred <- predict(model, newdata = test[i,])
    test[i, 'pred'] <- pred
    
    # if second day or later, use pred case.rate as lag-1
    if ( i <= (nrow(test)-1) & as.Date(test[i+1,'date']) > cutOff+1)
    {
      test[i+1, 'casesTminus1.rate'] <- pred
    }
    
    # if third day or later, use pred case.rate as lag-2
    if (i <= (nrow(test)-2) & as.Date(test[i+2,'date']) > (cutOff + 2))
    {
      test[i+2, 'casesTminus2.rate'] <- pred
    }
  
  }
  
  return(test)
}

#forecast(model = case.rate.lme20201031
#         , test = test14
#         )
```


Extract 95% CI for prediction band. References from the web:

- https://stackoverflow.com/questions/14358811/extract-prediction-band-from-lme-fit and 
- http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#predictions-andor-confidence-or-prediction-intervals-on-predictions

SE: This approach takes into account the uncertainty of the random effect in the prediction. 

SE2: This takes into account both random effect uncertainty and within-group error




```r
#model <- case.rate.lme20201031

metricsFuncLME <- function(model)
{
  results7 <- forecastLME(model, test = test7)
  
  # create design mateix
  # [-2] drops response from formula
  Designmat <- model.matrix(formula(model)[-2], results7)
  
  # compute XVX′ to get the variance-covariance matrix of the predictions
  # extract the diagonal of this matrix to get variances of predictions
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  results7$SE <- sqrt(predvar) 
  results7$SE2 <- sqrt(predvar + model$sigma^2) # sigma is the estimated within-group error standard deviation
  
  
  results14 <- forecastLME(model, test = test14)
  
  Designmat <- model.matrix(formula(model)[-2], results14)
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  results14$SE <- sqrt(predvar) 
  results14$SE2 <- sqrt(predvar + model$sigma^2)


  # CALCULATE MAE, MAPE, CP
  perform7 <- results7 %>% 
  group_by(city) %>% 
  summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
            , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
            , CP = round(sum(case.rate >= (pred - 1.96*SE2) & case.rate <= (pred + 1.96*SE2) ) / n() , 2)
            ) #%>%
  #as.data.frame() %>%
  #print()


  perform14 <- results14 %>% 
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= (pred - 1.96*SE2) & case.rate <= (pred + 1.96*SE2) ) / n() , 2)
              ) #%>%
    #as.data.frame() %>%
    #print()
  
  metrics <- perform7 %>%
    left_join(perform14 
              , by = "city"
              , suffix = c("_7d", "_14d")
              ) #%>%
    #as.data.frame() %>%
    #print()
  
  return(list(performMetrics = metrics
              , results7 = results7
              , results14 = results14))
}




plotFuncLME <- function(model
                     , forecastLength # only 7 or 14 as numeric arg
                     )
{
  
  if(forecastLength == 7)
  { results = output$results7
    
    perform <- output$performMetrics %>% 
      select(city, contains("7d"))
  }
  
  if(forecastLength == 14)
  { results = output$results14
    
    perform <- output$performMetrics %>% 
      select(city, contains("14d")) 
  }
  
  perform <- setNames(perform, gsub("_.*", "", names(perform)))
  
  perform <- perform %>%
    mutate(MAPE = paste0("MAPE = ", MAPE)
                      , MAE = paste0("MAE = ", MAE)
                      , CP = paste0("CP = ", CP)
                      )
  
  
  Designmat <- model.matrix(formula(model)[-2], train)
  predvar <- diag(Designmat %*% vcov(model) %*% t(Designmat)) 
  
  plotDF <- train %>% 
    select(city, weekNum, case.rate) %>%
    mutate(city = as.character(city)
           , SE2 = sqrt(predvar + model$sigma^2)
           , upperCI.fitted = case.rate + 1.96*SE2
           , lowerCI.fitted = case.rate - 1.96*SE2
           , fitted = predict(model, data = train)
           ) %>%
    reshape::melt(id = c('city', 'weekNum')) %>%
    
    # bring into forecasted case rates
    rbind(results %>% 
            select(city, weekNum, case.rate, pred, SE2) %>% 
            #rename(case.rate = pred) %>% 
            mutate(upperCI.pred = pred + 1.96*SE2
                   , lowerCI.pred = pred - 1.96*SE2
                   ) %>%
            reshape2::melt(id = c('city', 'weekNum'))
          ) 
      
  
  p <- ggplot(plotDF 
              , aes(weekNum, value, group = variable, colour = variable)) +
    
    geom_line(data = filter(plotDF, variable == 'case.rate'), linetype = "solid", color = 'black', alpha = 0.6) + 
    geom_line(data = filter(plotDF, variable == 'fitted'), linetype = "solid", color = 'red') +
    geom_line(data = filter(plotDF, variable == 'pred'), linetype = "solid", color = 'blue') +
    
    # uncertainty band for fitted model
    geom_ribbon(data = filter(plotDF, variable == 'lowerCI.fitted')
      , aes(ymin = filter(plotDF, variable == 'lowerCI.fitted')$value
                    , ymax =  filter(plotDF, variable == 'upperCI.fitted')$value
                    )
                , alpha = 0.3
                , color = NA
                , fill = "red") +
    
    # prediction band for forecast
    geom_ribbon(data = filter(plotDF, variable == 'lowerCI.pred')
      , aes(ymin = filter(plotDF, variable == 'lowerCI.pred')$value
                    , ymax =  filter(plotDF, variable == 'upperCI.pred')$value
                    )
                , alpha = 0.3
                , color = NA
                , fill = "blue") +
    
    facet_wrap(. ~ city, ncol = 2) +
    ggtitle(paste0('Forecast ', forecastLength, ' Days')) +
    ylab('Case Rate per 100,000 People') +
    xlab('Days since March 16, 2020') +
    #ylim(-15,100) +
    
    # add performance metrics
    geom_label(data = perform 
               , aes(label = MAPE), 
              x = 0, y = max(abs(filter(plotDF, variable == 'case.rate')$value)), hjust="inward", vjust="inward",
              inherit.aes = FALSE) +
    geom_label(data = perform 
               , aes(label = MAE), 
              x = 0, y = max(abs(filter(plotDF, variable == 'case.rate')$value))*4/5, hjust="inward", vjust="inward",
              inherit.aes = FALSE) +
    geom_label(data = perform 
                , aes(label = CP), 
               x = 0, y = max(abs(filter(plotDF, variable == 'case.rate')$value))*2/5, hjust="inward", vjust="inward",
               inherit.aes = FALSE) +
     
    theme_classic() +
    theme(legend.position = 'none') 
    
  p
}
```


# Naive Multiple Regression

Automate some functions specific to LM object

```r
forecastLM <- function(model, test) # provide model obj and test set
{
  for (i in 1:nrow(test))
  {
    
    pred <- predict(model, newdata = test[i,], interval = "prediction", level = 0.95) %>% as.data.frame()
    test[i, 'pred'] <- pred$fit
    test[i, 'lowerCI'] <- pred$lwr
    test[i, 'upperCI'] <- pred$upr
    
    
    # if second day or later, use pred case.rate as lag-1
    if ( i <= (nrow(test)-1) & as.Date(test[i+1,'date']) > cutOff+1)
    {
      test[i+1, 'casesTminus1.rate'] <- pred$fit
    }
    
    # if third day or later, use pred case.rate as lag-2
    if (i <= (nrow(test)-2) & as.Date(test[i+2,'date']) > (cutOff + 2))
    {
      test[i+2, 'casesTminus2.rate'] <- pred$fit
    }
  
  }
  
  return(test)
}

# 
# 
# temp <- forecastLM(model = case.rate.lm
#         , test = test14
#         )


metricsFuncLM <- function(model)
{
  results7 <- forecastLM(model, test = test7)
  
  results14 <- forecastLM(model, test = test14)
  
  metrics <- results7 %>% 
    group_by(city) %>% 
    summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
              , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
              , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
              ) %>%
    left_join( results14 %>% 
                 group_by(city) %>% 
                 summarize(MAE = round(mean(abs(case.rate - pred)) , 2)
                           , MAPE = round(mean(abs(case.rate - pred)/case.rate) , 2)
                           , CP = round(sum(case.rate >= lowerCI & case.rate <= upperCI ) / n() , 2)
                           )
               , by = "city"
               , suffix = c("_7d", "_14d")
               ) 
  
  return(list(performMetrics = metrics
              , results7 = results7
              , results14 = results14))
}




plotFuncLM <- function(model, forecastLength)
{
  
  
  if(forecastLength == 7)
  { results = output$results7
    
    metrics <- output$performMetrics %>% 
      select(city, contains("7d"))
  }
  
  if(forecastLength == 14)
  { results = output$results14
    
    metrics <- output$performMetrics %>% 
      select(city, contains("14d")) 
  }
  
  metrics <- setNames(metrics, gsub("_.*", "", names(metrics)))

  pred <- predict(model, newdata = train, interval = "prediction", level = 0.95)

  plotDF <- train %>% 
    select(city, day031620, case.rate) %>%
    mutate(city = as.character(city)) %>%
    cbind(pred) %>%
    rename(fitted = fit
           , lowerCI.fitted = lwr
           , upperCI.fitted = upr
           ) %>%
    reshape::melt(id = c('city', 'day031620')) %>%
    
    # bring into forecasted case rates
    rbind(results %>% 
            select(city, day031620, case.rate, pred, upperCI, lowerCI) %>% 
            rename(upperCI.pred = upperCI
                   , lowerCI.pred = lowerCI
                   ) %>%
            reshape2::melt(id = c('city', 'day031620'))
          ) 
      
  
  p <- ggplot(plotDF 
              , aes(day031620, value, group = variable, colour = variable)) +
    
    geom_line(data = filter(plotDF, variable == 'case.rate'), linetype = "solid", color = 'black', alpha = 0.6) + 
    geom_line(data = filter(plotDF, variable == 'fitted'), linetype = "solid", color = 'red') +
    geom_line(data = filter(plotDF, variable == 'pred'), linetype = "solid", color = 'blue') +
    
    # uncertainty band for fitted model
    geom_ribbon(data = filter(plotDF, variable == 'lowerCI.fitted')
      , aes(ymin = filter(plotDF, variable == 'lowerCI.fitted')$value
                    , ymax =  filter(plotDF, variable == 'upperCI.fitted')$value
                    )
                , alpha = 0.3, color = NA
                , fill = "red") +
    
    # prediction band for forecast
    geom_ribbon(data = filter(plotDF, variable == 'lowerCI.pred')
      , aes(ymin = filter(plotDF, variable == 'lowerCI.pred')$value
                    , ymax =  filter(plotDF, variable == 'upperCI.pred')$value
                    )
                , alpha = 0.3, color = NA
                , fill = "blue") +
    
    facet_wrap(. ~ city, ncol = 2) +
    ggtitle('Forecast 7 Days') +
    ylab('Case Rate per 100,000 People') +
    xlab('Days since March 16, 2020') +
    #ylim(-15,100) +
    
    # add performance metrics
    geom_label(data = metrics %>% mutate(MAPE = paste0("MAPE = ", MAPE))
               , aes(label = MAPE), 
              x = 0, y = max(abs(filter(plotDF, variable == 'case.rate')$value)), hjust="inward", vjust="inward",
              inherit.aes = FALSE) +
    geom_label(data = metrics %>% mutate(MAE = paste0("MAE = ", MAE))
               , aes(label = MAE),
              x = 0, y = max(abs(filter(plotDF, variable == 'case.rate')$value))*4/5, hjust="inward", vjust="inward",
              inherit.aes = FALSE) +
    geom_label(data = metrics %>% mutate(CP = paste0("CP = ", CP))
                , aes(label = CP),
               x = 0, y = max(abs(filter(plotDF, variable == 'case.rate')$value))*3/5, hjust="inward", vjust="inward",
               inherit.aes = FALSE) +

    theme_classic() +
    theme(legend.position = 'none') 
  
  
  p
}
```


# Mar-Nov

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

```r
nrow(test7)
```

```
## [1] 25
```

```r
nrow(test14)
```

```
## [1] 50
```



## LME

```r
lme <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate,
      random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city,
      data = train
      )

summary(lme)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   9060.505 9094.098 -4523.253
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev     Corr  
## casesTminus1.rate  0.3120481 cssT1.
## casesTminus2.rate  0.3631265 -0.981
## Residual          35.6081200       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate 
##                       Value Std.Error  DF   t-value p-value
## (Intercept)        9.248131 1.7734786 873  5.214684   0e+00
## casesTminus1.rate  1.268141 0.0749967 873 16.909282   0e+00
## casesTminus2.rate -0.298704 0.0876861 873 -3.406520   7e-04
##  Correlation: 
##                   (Intr) cssT1.
## casesTminus1.rate -0.020       
## casesTminus2.rate -0.128 -0.966
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -5.3343309 -0.3681018 -0.1209072  0.2962405  8.5364801 
## 
## Number of Observations: 900
## Number of Groups: 25
```



```r
output <- metricsFuncLME(model = lme)

allMetrics <- output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lme")})
```

## LME weighted


```r
lmeWt <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      , weights = varPower(form = ~ fitted(.))
      , control = lmeControl(maxIter = 1e4) 
      )

summary(lmeWt)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   8236.536 8274.928 -4110.268
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev    Corr  
## casesTminus1.rate 0.2265664 cssT1.
## casesTminus2.rate 0.2566874 -1    
## Residual          0.9941763       
## 
## Variance function:
##  Structure: Power of variance covariate
##  Formula: ~fitted(.) 
##  Parameter estimates:
##     power 
## 0.7429083 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate 
##                       Value Std.Error  DF   t-value p-value
## (Intercept)        6.410474 0.6572098 873  9.754075       0
## casesTminus1.rate  1.329633 0.0608294 873 21.858397       0
## casesTminus2.rate -0.337995 0.0654113 873 -5.167227       0
##  Correlation: 
##                   (Intr) cssT1.
## casesTminus1.rate -0.105       
## casesTminus2.rate -0.052 -0.968
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -2.7825579 -0.5932031 -0.1557770  0.4906187  7.6874159 
## 
## Number of Observations: 900
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeWt)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeWt")})
    , by = "city"
    )
```



## Multiple Regression

```r
lm <- 
  lm( case.rate ~ casesTminus1.rate + casesTminus2.rate
       , data = train
       )

summary(lm)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate, 
##     data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -211.859  -13.197   -4.711    9.761  313.868 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        9.22956    1.77194   5.209 2.36e-07 ***
## casesTminus1.rate  1.26128    0.03359  37.548  < 2e-16 ***
## casesTminus2.rate -0.29155    0.03862  -7.548 1.08e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37.77 on 897 degrees of freedom
## Multiple R-squared:  0.8747,	Adjusted R-squared:  0.8744 
## F-statistic:  3130 on 2 and 897 DF,  p-value: < 2.2e-16
```

```r
output <- metricsFuncLM(model = lm)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lm")})
    , by = "city"
    )
```

## LME + Testing Near Me


```r
lmeTest <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      )

summary(lmeTest)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   8861.567 8899.951 -4422.784
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev     Corr  
## casesTminus1.rate  0.2680892 cssT1.
## casesTminus2.rate  0.2847061 -0.955
## Residual          31.6384030       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe 
##                        Value Std.Error  DF   t-value p-value
## (Intercept)       -0.9399498 1.7281151 872 -0.543916  0.5866
## casesTminus1.rate  0.9221705 0.0685397 872 13.454552  0.0000
## casesTminus2.rate -0.0929929 0.0723222 872 -1.285814  0.1988
## testingNearMe      0.9672018 0.0631609 872 15.313297  0.0000
##  Correlation: 
##                   (Intr) cssT1. cssT2.
## casesTminus1.rate  0.089              
## casesTminus2.rate -0.183 -0.936       
## testingNearMe     -0.389 -0.315  0.170
## 
## Standardized Within-Group Residuals:
##         Min          Q1         Med          Q3         Max 
## -5.53719699 -0.43475232 -0.06377232  0.29449255  9.19400018 
## 
## Number of Observations: 900
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeTest")})
    , by = "city"
    )
```

## Multiple Regression + Testing Near Me

```r
lmTest <- 
  lm( case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
       , data = train
       )

summary(lmTest)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate + 
##     testingNearMe, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -178.85  -13.66   -1.84    9.06  339.63 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)       -2.31797    1.78031  -1.302    0.193    
## casesTminus1.rate  1.00775    0.03487  28.898  < 2e-16 ***
## casesTminus2.rate -0.14802    0.03612  -4.098 4.54e-05 ***
## testingNearMe      0.91846    0.06304  14.569  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 33.98 on 896 degrees of freedom
## Multiple R-squared:  0.8987,	Adjusted R-squared:  0.8983 
## F-statistic:  2649 on 3 and 896 DF,  p-value: < 2.2e-16
```

```r
output <- metricsFuncLM(model = lmTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmTest")})
    , by = "city"
    )
```


## Log-transformed


```r
train <- train %>% 
  mutate(case.rate = log(case.rate)
         , casesTminus1.rate = case_when(casesTminus1.rate != 0 ~ log(casesTminus1.rate)
                                         , TRUE ~ 0.000000001)
         , casesTminus2.rate = case_when(casesTminus2.rate != 0 ~ log(casesTminus2.rate)
                                         , TRUE ~ 0.000000001)
         )

test7 <- test7  %>% 
  mutate(case.rate = log(case.rate)
         , casesTminus1.rate = case_when(casesTminus1.rate != 0 ~ log(casesTminus1.rate)
                                         , TRUE ~ 0.000000001)
         , casesTminus2.rate = case_when(casesTminus2.rate != 0 ~ log(casesTminus2.rate)
                                         , TRUE ~ 0.000000001)
         )
         

test14 <- test14  %>% 
  mutate(case.rate = log(case.rate)
         , casesTminus1.rate = case_when(casesTminus1.rate != 0 ~ log(casesTminus1.rate)
                                         , TRUE ~ 0.000000001)
         , casesTminus2.rate = case_when(casesTminus2.rate != 0 ~ log(casesTminus2.rate)
                                         , TRUE ~ 0.000000001)
         )
```
## LME-Log
NOT CONVERGING - TBD

<!-- ```{r } -->
<!-- lmeLog <- -->
<!--   lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate -->
<!--       , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city -->
<!--       , data = train -->
<!--       , control = lmeControl(maxIter = 1e8) -->
<!--       ) -->

<!-- summary(lmeLog) -->

<!-- output <- metricsFuncLME(model = lmeLog) -->

<!-- allMetrics <- allMetrics %>% left_join( -->
<!--     output$performMetrics %>% -->
<!--       rename_at(vars(-city), function(x) {paste0(x, "_lmeLog")}) -->
<!--     , by = "city" -->
<!--     ) -->

<!-- ``` -->


## LME-Log weighted residual

```r
lmeLogWt <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      , weights = varPower(form = ~ fitted(.))
      , control = lmeControl(maxIter = 1e4) 
      )

summary(lmeLogWt)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##       AIC      BIC   logLik
##   512.384 550.7764 -248.192
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev    Corr  
## casesTminus1.rate 0.1051108 cssT1.
## casesTminus2.rate 0.1077224 -1    
## Residual          0.6721486       
## 
## Variance function:
##  Structure: Power of variance covariate
##  Formula: ~fitted(.) 
##  Parameter estimates:
##      power 
## -0.5473487 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate 
##                        Value  Std.Error  DF   t-value p-value
## (Intercept)        0.3783120 0.04828216 873   7.83544       0
## casesTminus1.rate  1.2435351 0.03425502 873  36.30227       0
## casesTminus2.rate -0.3224945 0.03167578 873 -10.18111       0
##  Correlation: 
##                   (Intr) cssT1.
## casesTminus1.rate -0.459       
## casesTminus2.rate  0.164 -0.949
## 
## Standardized Within-Group Residuals:
##         Min          Q1         Med          Q3         Max 
## -6.48752997 -0.55333639 -0.01987342  0.61746702  4.34188714 
## 
## Number of Observations: 900
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeLogWt)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeLogWt")})
    , by = "city"
    )
```


## Multiple Regression

```r
lmLog <- 
  lm( case.rate ~ casesTminus1.rate + casesTminus2.rate
       , data = train
       )

summary(lmLog)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate, 
##     data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.03599 -0.18616 -0.00141  0.18711  1.58096 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        0.45184    0.04545   9.941   <2e-16 ***
## casesTminus1.rate  1.25860    0.02567  49.035   <2e-16 ***
## casesTminus2.rate -0.35609    0.02087 -17.065   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.324 on 897 degrees of freedom
## Multiple R-squared:  0.8893,	Adjusted R-squared:  0.889 
## F-statistic:  3601 on 2 and 897 DF,  p-value: < 2.2e-16
```

```r
output <- metricsFuncLM(model = lmLog)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmLog")})
    , by = "city"
    )
```

## LME-Log + Testing Near Me


```r
lmeLogTest <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      )

summary(lmeLogTest)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC     BIC    logLik
##   450.7374 489.121 -217.3687
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev     Corr  
## casesTminus1.rate 0.07908711 cssT1.
## casesTminus2.rate 0.08058749 -0.995
## Residual          0.29934306       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe 
##                        Value  Std.Error  DF   t-value p-value
## (Intercept)        0.6875025 0.04742666 872  14.49612       0
## casesTminus1.rate  1.1139739 0.03191280 872  34.90680       0
## casesTminus2.rate -0.3061494 0.02615564 872 -11.70491       0
## testingNearMe      0.0062046 0.00054254 872  11.43620       0
##  Correlation: 
##                   (Intr) cssT1. cssT2.
## casesTminus1.rate -0.627              
## casesTminus2.rate  0.295 -0.918       
## testingNearMe      0.418 -0.402  0.182
## 
## Standardized Within-Group Residuals:
##         Min          Q1         Med          Q3         Max 
## -5.63741009 -0.53227064 -0.04522627  0.55649054  5.19163152 
## 
## Number of Observations: 900
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeLogTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeLogTest")})
    , by = "city"
    )
```



## LME-Log weighted residual + Testing Near Me

```r
lmeLogWtTest <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      , weights = varPower(form = ~ fitted(.))
      , control = lmeControl(maxIter = 1e4) 
      )

summary(lmeLogWtTest)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##       AIC      BIC   logLik
##   381.062 424.2434 -181.531
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev    Corr  
## casesTminus1.rate 0.1306451 cssT1.
## casesTminus2.rate 0.1348650 -1    
## Residual          0.8889737       
## 
## Variance function:
##  Structure: Power of variance covariate
##  Formula: ~fitted(.) 
##  Parameter estimates:
##      power 
## -0.8083805 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe 
##                        Value  Std.Error  DF   t-value p-value
## (Intercept)        0.6385735 0.05053307 872 12.636746       0
## casesTminus1.rate  1.0593550 0.03903017 872 27.141951       0
## casesTminus2.rate -0.2335460 0.03564459 872 -6.552074       0
## testingNearMe      0.0057040 0.00044968 872 12.684572       0
##  Correlation: 
##                   (Intr) cssT1. cssT2.
## casesTminus1.rate -0.467              
## casesTminus2.rate  0.166 -0.945       
## testingNearMe      0.445 -0.355  0.162
## 
## Standardized Within-Group Residuals:
##         Min          Q1         Med          Q3         Max 
## -6.42076048 -0.54769374 -0.07010398  0.61404729  4.65162658 
## 
## Number of Observations: 900
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeLogWtTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeLogWtTest")})
    , by = "city"
    )
```

## Multiple Regression + Testing Near Me

```r
lmLogTest <- 
  lm( case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
       , data = train
       )

summary(lmLogTest)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate + 
##     testingNearMe, data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.81605 -0.16661 -0.01669  0.16986  1.58246 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        0.6593969  0.0465775   14.16   <2e-16 ***
## casesTminus1.rate  1.1229508  0.0270137   41.57   <2e-16 ***
## casesTminus2.rate -0.3067750  0.0200782  -15.28   <2e-16 ***
## testingNearMe      0.0060190  0.0005429   11.09   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.304 on 896 degrees of freedom
## Multiple R-squared:  0.9026,	Adjusted R-squared:  0.9023 
## F-statistic:  2768 on 3 and 896 DF,  p-value: < 2.2e-16
```

```r
output <- metricsFuncLM(model = lmLogTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmLogTest")})
    , by = "city"
    )
```

## Metrics for Mar-Nov

```r
allMetrics %>% select(city, starts_with("MAPE_7d")) %>%
  arrange(as.character(city)) %>%
  as.data.frame() %>%
  format(scientific = F) %>%
  print()
```

```
##             city MAPE_7d_lme MAPE_7d_lmeWt MAPE_7d_lm MAPE_7d_lmeTest
## 1    Albuquerque        0.33          0.10       0.12            0.23
## 2        Atlanta        0.30          0.31       0.37            0.25
## 3      Baltimore        0.24          0.21       0.17            0.15
## 4         Boston        0.41          0.38       0.39            0.31
## 5      Charlotte        0.24          0.23       0.23            0.16
## 6        Chicago        0.18          0.16       0.10            0.19
## 7      Cleveland        0.49          0.43       0.34            0.37
## 8         Dallas        0.46          0.43       0.48            0.42
## 9         Denver        0.18          0.03       0.00            0.14
## 10       Detroit        0.00          0.05       0.01            0.06
## 11       Houston        0.40          0.38       0.39            0.38
## 12  Indianapolis        0.20          0.18       0.18            0.18
## 13   Los Angeles        0.32          0.31       0.32            0.29
## 14    Louisville        0.00          0.04       0.09            0.06
## 15       Memphis        0.19          0.19       0.23            0.08
## 16         Miami        0.23          0.18       0.20            0.18
## 17      New York        0.26          0.22       0.22            0.06
## 18      Oklahoma        0.19          0.11       0.08            0.12
## 19       Phoenix        0.43          0.40       0.41            0.38
## 20    Pittsburgh        0.11          0.11       0.13            0.12
## 21      Portland        0.09          0.09       0.11            0.05
## 22    Sacramento        0.18          0.16       0.15            0.10
## 23 San Francisco        0.29          0.27       0.27            0.10
## 24       Seattle        0.14          0.16       0.14            0.11
## 25         Tampa        0.15          0.11       0.10            0.03
##    MAPE_7d_lmTest MAPE_7d_lmeLogWt MAPE_7d_lmLog MAPE_7d_lmeLogTest
## 1            0.05             0.00          0.01               0.02
## 2            0.28             0.08          0.10               0.07
## 3            0.10             0.05          0.05               0.04
## 4            0.30             0.09          0.10               0.06
## 5            0.13             0.06          0.06               0.02
## 6            0.12             0.04          0.04               0.05
## 7            0.26             0.04          0.03               0.04
## 8            0.43             0.11          0.12               0.11
## 9            0.03             0.02          0.02               0.03
## 10           0.04             0.01          0.02               0.03
## 11           0.36             0.10          0.10               0.10
## 12           0.19             0.05          0.05               0.04
## 13           0.30             0.07          0.08               0.06
## 14           0.11             0.03          0.04               0.03
## 15           0.10             0.06          0.06               0.00
## 16           0.18             0.05          0.06               0.04
## 17           0.04             0.05          0.06               0.02
## 18           0.05             0.00          0.01               0.00
## 19           0.36             0.10          0.10               0.07
## 20           0.11             0.04          0.04               0.02
## 21           0.02             0.03          0.03               0.00
## 22           0.02             0.04          0.04               0.00
## 23           0.04             0.07          0.07               0.03
## 24           0.22             0.02          0.01               0.02
## 25           0.05             0.03          0.03               0.00
##    MAPE_7d_lmeLogWtTest MAPE_7d_lmLogTest
## 1                  0.02              0.02
## 2                  0.06              0.08
## 3                  0.04              0.04
## 4                  0.06              0.06
## 5                  0.02              0.02
## 6                  0.05              0.05
## 7                  0.04              0.04
## 8                  0.10              0.11
## 9                  0.03              0.03
## 10                 0.03              0.03
## 11                 0.10              0.10
## 12                 0.04              0.04
## 13                 0.06              0.06
## 14                 0.02              0.03
## 15                 0.00              0.01
## 16                 0.04              0.05
## 17                 0.02              0.01
## 18                 0.00              0.00
## 19                 0.07              0.07
## 20                 0.02              0.02
## 21                 0.00              0.01
## 22                 0.00              0.01
## 23                 0.03              0.03
## 24                 0.03              0.03
## 25                 0.00              0.00
```


```r
allMetrics %>% select(city, starts_with("MAE_7d")) %>%
  arrange(as.character(city)) %>% 
  as.data.frame() %>%
  print()
```

```
##             city MAE_7d_lme MAE_7d_lmeWt MAE_7d_lm MAE_7d_lmeTest MAE_7d_lmTest
## 1    Albuquerque     204.51        59.97     73.65         141.62         33.02
## 2        Atlanta      86.14        89.67    105.84          72.79         78.96
## 3      Baltimore      63.40        57.38     46.55          40.34         25.87
## 4         Boston     166.47       153.23    158.27         125.47        121.55
## 5      Charlotte      78.21        73.98     75.90          50.68         41.87
## 6        Chicago      91.25        81.50     52.59          93.02         58.12
## 7      Cleveland     223.77       193.62    156.68         170.25        120.01
## 8         Dallas     211.40       199.06    218.70         191.69        197.53
## 9         Denver      86.39        15.93      0.08          68.25         14.78
## 10       Detroit       1.02        20.75      4.06          24.55         15.98
## 11       Houston      99.89        95.09     97.41          95.48         90.49
## 12  Indianapolis     132.90       123.65    123.47         118.49        126.63
## 13   Los Angeles     136.81       131.46    135.25         123.34        128.62
## 14    Louisville       1.81        19.45     45.72          28.56         55.25
## 15       Memphis      72.34        72.12     89.11          29.84         36.23
## 16         Miami      96.86        76.62     82.88          76.14         76.29
## 17      New York      56.35        47.94     47.21          12.49          7.78
## 18      Oklahoma      98.84        60.49     42.14          64.96         24.12
## 19       Phoenix     232.57       213.52    217.96         205.87        193.41
## 20    Pittsburgh      49.01        49.54     61.53          55.64         51.79
## 21      Portland      26.06        24.53     28.86          13.31          4.28
## 22    Sacramento      52.52        46.48     44.05          29.04          7.09
## 23 San Francisco      48.41        45.52     45.24          16.87          6.24
## 24       Seattle      27.49        31.82     26.71          21.16         43.11
## 25         Tampa      31.73        23.86     21.97           5.80         11.44
##    MAE_7d_lmeLogWt MAE_7d_lmLog MAE_7d_lmeLogTest MAE_7d_lmeLogWtTest
## 1             0.02         0.06              0.16                0.12
## 2             0.46         0.54              0.40                0.34
## 3             0.28         0.29              0.22                0.21
## 4             0.56         0.59              0.36                0.36
## 5             0.33         0.36              0.14                0.14
## 6             0.24         0.25              0.30                0.30
## 7             0.23         0.18              0.21                0.23
## 8             0.70         0.75              0.66                0.61
## 9             0.12         0.15              0.21                0.19
## 10            0.06         0.12              0.21                0.15
## 11            0.54         0.56              0.56                0.53
## 12            0.32         0.35              0.24                0.24
## 13            0.44         0.46              0.35                0.37
## 14            0.17         0.23              0.19                0.15
## 15            0.33         0.38              0.03                0.03
## 16            0.32         0.34              0.27                0.27
## 17            0.29         0.31              0.12                0.10
## 18            0.02         0.07              0.00                0.03
## 19            0.60         0.63              0.45                0.44
## 20            0.22         0.25              0.13                0.10
## 21            0.16         0.19              0.00                0.03
## 22            0.22         0.23              0.00                0.00
## 23            0.36         0.37              0.18                0.16
## 24            0.08         0.06              0.13                0.16
## 25            0.16         0.19              0.01                0.00
##    MAE_7d_lmLogTest
## 1              0.13
## 2              0.43
## 3              0.20
## 4              0.35
## 5              0.14
## 6              0.30
## 7              0.23
## 8              0.68
## 9              0.20
## 10             0.19
## 11             0.56
## 12             0.23
## 13             0.38
## 14             0.20
## 15             0.05
## 16             0.29
## 17             0.08
## 18             0.00
## 19             0.45
## 20             0.09
## 21             0.04
## 22             0.03
## 23             0.15
## 24             0.16
## 25             0.00
```


```r
allMetrics %>% select(city, starts_with("CP_7d")) %>%
  arrange(as.character(city)) %>%
  as.data.frame() %>%
  print()
```

```
##             city CP_7d_lme CP_7d_lmeWt CP_7d_lm CP_7d_lmeTest CP_7d_lmTest
## 1    Albuquerque         0           0        1             0            1
## 2        Atlanta         0           0        0             0            0
## 3      Baltimore         1           0        1             1            1
## 4         Boston         0           0        0             0            0
## 5      Charlotte         0           0        0             1            1
## 6        Chicago         0           0        1             0            1
## 7      Cleveland         0           0        0             0            0
## 8         Dallas         0           0        0             0            0
## 9         Denver         0           1        1             1            1
## 10       Detroit         1           0        1             1            1
## 11       Houston         0           0        0             0            0
## 12  Indianapolis         0           0        0             0            0
## 13   Los Angeles         0           0        0             0            0
## 14    Louisville         1           0        1             1            1
## 15       Memphis         0           0        0             1            1
## 16         Miami         0           0        0             0            0
## 17      New York         1           0        1             1            1
## 18      Oklahoma         0           0        1             1            1
## 19       Phoenix         0           0        0             0            0
## 20    Pittsburgh         1           0        1             1            1
## 21      Portland         1           0        1             1            1
## 22    Sacramento         1           0        1             1            1
## 23 San Francisco         1           0        1             1            1
## 24       Seattle         1           0        1             1            1
## 25         Tampa         1           0        1             1            1
##    CP_7d_lmeLogWt CP_7d_lmLog CP_7d_lmeLogTest CP_7d_lmeLogWtTest
## 1               1           1                1                  1
## 2               1           1                1                  1
## 3               1           1                1                  1
## 4               1           1                1                  1
## 5               1           1                1                  1
## 6               1           1                1                  1
## 7               1           1                1                  1
## 8               1           0                0                  1
## 9               1           1                1                  1
## 10              1           1                1                  1
## 11              1           1                1                  1
## 12              1           1                1                  1
## 13              1           1                1                  1
## 14              1           1                1                  1
## 15              1           1                1                  1
## 16              1           1                1                  1
## 17              1           1                1                  1
## 18              1           1                1                  1
## 19              1           1                1                  1
## 20              1           1                1                  1
## 21              1           1                1                  1
## 22              1           1                1                  1
## 23              1           1                1                  1
## 24              1           1                1                  1
## 25              1           1                1                  1
##    CP_7d_lmLogTest
## 1                1
## 2                1
## 3                1
## 4                1
## 5                1
## 6                1
## 7                1
## 8                0
## 9                1
## 10               1
## 11               1
## 12               1
## 13               1
## 14               1
## 15               1
## 16               1
## 17               1
## 18               1
## 19               1
## 20               1
## 21               1
## 22               1
## 23               1
## 24               1
## 25               1
```




```r
allMetrics %>% select(city, starts_with("MAPE_14d")) %>%
  arrange(as.character(city)) %>%
  as.data.frame() %>%
  format(scientific = F) %>%
  print()
```

```
##             city MAPE_14d_lme MAPE_14d_lmeWt MAPE_14d_lm MAPE_14d_lmeTest
## 1    Albuquerque         0.47           0.48        0.35             0.36
## 2        Atlanta         0.19           0.45        0.71             0.14
## 3      Baltimore         0.37           0.19        0.09             0.20
## 4         Boston         0.42           0.41        0.45             0.32
## 5      Charlotte         0.35           0.33        0.33             0.23
## 6        Chicago         0.10           0.12        0.10             0.15
## 7      Cleveland         0.29           0.23        0.25             0.23
## 8         Dallas         0.59           0.53        0.62             0.43
## 9         Denver         0.18           0.14        0.11             0.11
## 10       Detroit         0.04           0.16        0.07             0.05
## 11       Houston         0.43           0.52        0.53             0.34
## 12  Indianapolis         0.16           0.15        0.11             0.11
## 13   Los Angeles         0.54           0.51        0.52             0.39
## 14    Louisville         0.02           0.02        0.09             0.07
## 15       Memphis         0.22           0.24        0.35             0.12
## 16         Miami         0.23           0.14        0.14             0.16
## 17      New York         0.51           0.53        0.35             0.19
## 18      Oklahoma         0.13           0.17        0.22             0.12
## 19       Phoenix         0.60           0.50        0.52             0.41
## 20    Pittsburgh         0.08           0.13        0.20             0.22
## 21      Portland         0.32           0.25        0.17             0.05
## 22    Sacramento         0.29           0.25        0.24             0.16
## 23 San Francisco         0.48           0.50        0.47             0.15
## 24       Seattle         0.08           0.14        0.09             0.14
## 25         Tampa         0.37           0.33        0.26             0.15
##    MAPE_14d_lmTest MAPE_14d_lmeLogWt MAPE_14d_lmLog MAPE_14d_lmeLogTest
## 1             0.14              0.20           0.17                0.15
## 2             0.41              0.15           0.22                0.14
## 3             0.08              0.03           0.03                0.02
## 4             0.32              0.11           0.12                0.07
## 5             0.20              0.08           0.09                0.03
## 6             0.09              0.02           0.03                0.05
## 7             0.25              0.04           0.04                0.03
## 8             0.49              0.15           0.16                0.13
## 9             0.02              0.02           0.02                0.03
## 10            0.07              0.01           0.02                0.04
## 11            0.41              0.12           0.13                0.12
## 12            0.18              0.03           0.03                0.03
## 13            0.43              0.13           0.14                0.10
## 14            0.09              0.02           0.02                0.03
## 15            0.16              0.08           0.09                0.02
## 16            0.16              0.05           0.05                0.04
## 17            0.05              0.09           0.09                0.04
## 18            0.08              0.03           0.03                0.01
## 19            0.37              0.13           0.14                0.07
## 20            0.21              0.05           0.05                0.05
## 21            0.07              0.04           0.05                0.00
## 22            0.09              0.06           0.06                0.00
## 23            0.12              0.12           0.13                0.07
## 24            0.12              0.01           0.01                0.01
## 25            0.11              0.07           0.08                0.03
##    MAPE_14d_lmeLogWtTest MAPE_14d_lmLogTest
## 1                   0.16               0.14
## 2                   0.08               0.16
## 3                   0.04               0.02
## 4                   0.06               0.07
## 5                   0.04               0.04
## 6                   0.05               0.05
## 7                   0.03               0.03
## 8                   0.12               0.14
## 9                   0.03               0.03
## 10                  0.02               0.03
## 11                  0.10               0.12
## 12                  0.02               0.02
## 13                  0.09               0.11
## 14                  0.03               0.03
## 15                  0.02               0.03
## 16                  0.04               0.05
## 17                  0.04               0.03
## 18                  0.00               0.01
## 19                  0.07               0.08
## 20                  0.04               0.04
## 21                  0.01               0.01
## 22                  0.00               0.00
## 23                  0.06               0.06
## 24                  0.02               0.02
## 25                  0.03               0.03
```


```r
allMetrics %>% select(city, starts_with("MAE_14d")) %>%
  arrange(as.character(city)) %>% 
  as.data.frame() %>%
  print()
```

```
##             city MAE_14d_lme MAE_14d_lmeWt MAE_14d_lm MAE_14d_lmeTest
## 1    Albuquerque      266.79        267.43     196.59          206.56
## 2        Atlanta       58.64        151.02     239.10           40.25
## 3      Baltimore      105.62         53.18      23.34           56.63
## 4         Boston      182.79        179.79     195.56          141.31
## 5      Charlotte      141.80        133.56     132.26           94.60
## 6        Chicago       49.47         60.07      46.36           72.43
## 7      Cleveland      143.68        112.19     133.72          118.35
## 8         Dallas      291.31        262.75     307.86          213.07
## 9         Denver       88.31         65.51      52.52           51.81
## 10       Detroit       14.05         57.47      22.68           20.42
## 11       Houston      110.19        134.49     136.73           87.63
## 12  Indianapolis      108.34         98.30      71.01           71.87
## 13   Los Angeles      324.39        302.07     309.20          228.47
## 14    Louisville       11.08         11.58      44.84           33.70
## 15       Memphis       86.54         96.88     140.87           49.11
## 16         Miami       93.52         58.66      58.69           65.99
## 17      New York      115.31        120.56      79.38           43.42
## 18      Oklahoma       70.87         90.25     113.97           61.40
## 19       Phoenix      332.12        278.04     288.09          228.36
## 20    Pittsburgh       40.97         73.65     111.11          124.32
## 21      Portland       81.39         62.18      43.42           12.60
## 22    Sacramento      102.66         90.04      86.78           56.77
## 23 San Francisco      110.66        114.89     106.69           34.23
## 24       Seattle       16.82         33.36      19.51           33.83
## 25         Tampa      100.86         91.00      69.95           41.85
##    MAE_14d_lmTest MAE_14d_lmeLogWt MAE_14d_lmLog MAE_14d_lmeLogTest
## 1           77.52             1.24          1.10               0.95
## 2          137.47             0.89          1.27               0.81
## 3           22.17             0.20          0.18               0.12
## 4          140.48             0.67          0.75               0.41
## 5           81.46             0.50          0.51               0.20
## 6           41.80             0.13          0.18               0.33
## 7          146.59             0.27          0.28               0.22
## 8          243.16             0.92          1.00               0.83
## 9           10.24             0.13          0.12               0.18
## 10          24.26             0.04          0.13               0.23
## 11         105.58             0.69          0.74               0.67
## 12         121.23             0.22          0.18               0.20
## 13         251.45             0.85          0.89               0.64
## 14          44.02             0.12          0.14               0.17
## 15          64.81             0.48          0.57               0.15
## 16          64.74             0.27          0.29               0.26
## 17          10.41             0.49          0.47               0.23
## 18          39.22             0.16          0.19               0.05
## 19         206.06             0.80          0.87               0.47
## 20         121.61             0.30          0.34               0.29
## 21          17.23             0.22          0.27               0.00
## 22          33.62             0.37          0.37               0.03
## 23          29.08             0.68          0.71               0.37
## 24          23.42             0.05          0.06               0.08
## 25          28.35             0.40          0.44               0.17
##    MAE_14d_lmeLogWtTest MAE_14d_lmLogTest
## 1                  1.03              0.86
## 2                  0.47              0.93
## 3                  0.22              0.12
## 4                  0.38              0.41
## 5                  0.21              0.21
## 6                  0.32              0.33
## 7                  0.22              0.22
## 8                  0.73              0.87
## 9                  0.19              0.18
## 10                 0.14              0.20
## 11                 0.58              0.68
## 12                 0.14              0.14
## 13                 0.60              0.68
## 14                 0.16              0.17
## 15                 0.13              0.19
## 16                 0.27              0.28
## 17                 0.21              0.14
## 18                 0.02              0.07
## 19                 0.43              0.48
## 20                 0.25              0.23
## 21                 0.06              0.04
## 22                 0.03              0.02
## 23                 0.31              0.34
## 24                 0.08              0.10
## 25                 0.14              0.17
```


```r
allMetrics %>% select(city, starts_with("CP_14d")) %>%
  arrange(as.character(city)) %>%
  as.data.frame() %>%
  print()
```

```
##             city CP_14d_lme CP_14d_lmeWt CP_14d_lm CP_14d_lmeTest CP_14d_lmTest
## 1    Albuquerque        0.0          0.0       0.5            0.0           0.5
## 2        Atlanta        0.5          0.0       0.0            0.5           0.0
## 3      Baltimore        0.5          0.0       1.0            0.5           1.0
## 4         Boston        0.0          0.0       0.0            0.0           0.0
## 5      Charlotte        0.0          0.0       0.0            0.5           0.5
## 6        Chicago        0.5          0.0       1.0            0.5           1.0
## 7      Cleveland        0.5          0.0       0.0            0.5           0.0
## 8         Dallas        0.0          0.0       0.0            0.0           0.0
## 9         Denver        0.0          0.5       0.5            1.0           1.0
## 10       Detroit        1.0          0.0       1.0            1.0           1.0
## 11       Houston        0.0          0.0       0.0            0.0           0.0
## 12  Indianapolis        0.5          0.0       0.5            0.5           0.0
## 13   Los Angeles        0.0          0.0       0.0            0.0           0.0
## 14    Louisville        1.0          0.5       1.0            1.0           1.0
## 15       Memphis        0.0          0.0       0.0            0.5           0.5
## 16         Miami        0.0          0.0       0.5            0.5           0.5
## 17      New York        0.5          0.0       0.5            0.5           1.0
## 18      Oklahoma        0.5          0.0       0.5            1.0           1.0
## 19       Phoenix        0.0          0.0       0.0            0.0           0.0
## 20    Pittsburgh        1.0          0.0       0.5            0.5           0.5
## 21      Portland        0.5          0.0       1.0            1.0           1.0
## 22    Sacramento        0.5          0.0       0.5            0.5           1.0
## 23 San Francisco        0.5          0.0       0.5            1.0           1.0
## 24       Seattle        1.0          0.0       1.0            1.0           1.0
## 25         Tampa        0.5          0.0       0.5            0.5           1.0
##    CP_14d_lmeLogWt CP_14d_lmLog CP_14d_lmeLogTest CP_14d_lmeLogWtTest
## 1              0.5          0.5               0.5                 0.5
## 2              1.0          0.5               0.5                 1.0
## 3              1.0          1.0               1.0                 1.0
## 4              1.0          0.5               1.0                 1.0
## 5              1.0          0.5               1.0                 1.0
## 6              1.0          1.0               1.0                 1.0
## 7              1.0          1.0               1.0                 1.0
## 8              1.0          0.0               0.0                 1.0
## 9              1.0          1.0               1.0                 1.0
## 10             1.0          1.0               1.0                 1.0
## 11             1.0          0.5               0.5                 1.0
## 12             1.0          1.0               1.0                 1.0
## 13             1.0          0.5               0.5                 1.0
## 14             1.0          1.0               1.0                 1.0
## 15             1.0          0.5               1.0                 1.0
## 16             1.0          1.0               1.0                 1.0
## 17             1.0          0.5               1.0                 1.0
## 18             1.0          1.0               1.0                 1.0
## 19             1.0          0.5               1.0                 1.0
## 20             1.0          1.0               1.0                 1.0
## 21             1.0          1.0               1.0                 1.0
## 22             1.0          1.0               1.0                 1.0
## 23             1.0          0.5               1.0                 1.0
## 24             1.0          1.0               1.0                 1.0
## 25             1.0          0.5               1.0                 1.0
##    CP_14d_lmLogTest
## 1               0.5
## 2               0.5
## 3               1.0
## 4               1.0
## 5               1.0
## 6               1.0
## 7               1.0
## 8               0.0
## 9               1.0
## 10              1.0
## 11              0.5
## 12              1.0
## 13              0.5
## 14              1.0
## 15              1.0
## 16              1.0
## 17              1.0
## 18              1.0
## 19              1.0
## 20              1.0
## 21              1.0
## 22              1.0
## 23              1.0
## 24              1.0
## 25              1.0
```








# Mar-Aug

```r
cutOff <- as.Date("2020-08-31")

train <- weeklyCases %>% 
  filter(weekNum > 3 & as.Date(date) < cutOff)


test7 <- weeklyCases %>% 
   filter(weekNum > 3 & as.Date(date) >= cutOff & weekNum <= 29)

  
test14 <- weeklyCases %>% 
  filter(weekNum > 3 & as.Date(date) >= cutOff) %>%
  
  # retained lag case rates for first week only, zero out the rest
  mutate(
    casesTminus2.rate = case_when(weekNum == 29 ~ 0, TRUE ~ casesTminus2.rate)
  )


nrow(train)
```

```
## [1] 600
```

```r
nrow(test7)
```

```
## [1] 50
```

```r
nrow(test14)
```

```
## [1] 350
```



## LME

```r
lme <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate,
      random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city,
      data = train
      )

summary(lme)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   5671.022 5701.765 -2828.511
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev     Corr  
## casesTminus1.rate  0.2961015 cssT1.
## casesTminus2.rate  0.3075262 -0.998
## Residual          26.2328610       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate 
##                       Value Std.Error  DF   t-value p-value
## (Intercept)       10.122326 1.5185237 573  6.665899   0.000
## casesTminus1.rate  1.141818 0.0793370 573 14.391995   0.000
## casesTminus2.rate -0.251997 0.0812915 573 -3.099918   0.002
##  Correlation: 
##                   (Intr) cssT1.
## casesTminus1.rate -0.133       
## casesTminus2.rate -0.005 -0.979
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -6.4491654 -0.4069951 -0.1625910  0.2563971  5.7537120 
## 
## Number of Observations: 600
## Number of Groups: 25
```



```r
output <- metricsFuncLME(model = lme)

allMetrics <- output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lme")})
```

## LME weighted


```r
lmeWt <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      , weights = varPower(form = ~ fitted(.))
      , control = lmeControl(maxIter = 1e4) 
      )

summary(lmeWt)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   5256.509 5291.644 -2620.254
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev    Corr  
## casesTminus1.rate 0.1982295 cssT1.
## casesTminus2.rate 0.2194537 -1    
## Residual          1.2746000       
## 
## Variance function:
##  Structure: Power of variance covariate
##  Formula: ~fitted(.) 
##  Parameter estimates:
##     power 
## 0.6790522 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate 
##                       Value Std.Error  DF   t-value p-value
## (Intercept)        7.232783 0.7443353 573  9.717104       0
## casesTminus1.rate  1.244602 0.0643151 573 19.351622       0
## casesTminus2.rate -0.317214 0.0646672 573 -4.905329       0
##  Correlation: 
##                   (Intr) cssT1.
## casesTminus1.rate -0.203       
## casesTminus2.rate  0.019 -0.956
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -2.8496465 -0.5654065 -0.1963527  0.3956993  6.6362973 
## 
## Number of Observations: 600
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeWt)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeWt")})
    , by = "city"
    )
```



## Multiple Regression

```r
lm <- 
  lm( case.rate ~ casesTminus1.rate + casesTminus2.rate
       , data = train
       )

summary(lm)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate, 
##     data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -171.221  -10.697   -4.202    6.949  146.102 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        9.59869    1.54881   6.197 1.07e-09 ***
## casesTminus1.rate  1.20578    0.03928  30.695  < 2e-16 ***
## casesTminus2.rate -0.31294    0.03881  -8.062 4.11e-15 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 27.56 on 597 degrees of freedom
## Multiple R-squared:  0.8652,	Adjusted R-squared:  0.8648 
## F-statistic:  1916 on 2 and 597 DF,  p-value: < 2.2e-16
```

```r
output <- metricsFuncLM(model = lm)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lm")})
    , by = "city"
    )
```

## LME + Testing Near Me


```r
lmeTest <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      )

summary(lmeTest)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##       AIC      BIC    logLik
##   5569.55 5604.672 -2776.775
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev     Corr  
## casesTminus1.rate  0.3144160 cssT1.
## casesTminus2.rate  0.3139215 -0.997
## Residual          23.8780311       
## 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe 
##                       Value Std.Error  DF   t-value p-value
## (Intercept)        3.562611 1.5483767 572  2.300868  0.0218
## casesTminus1.rate  0.918322 0.0827817 572 11.093298  0.0000
## casesTminus2.rate -0.109964 0.0809404 572 -1.358585  0.1748
## testingNearMe      0.757691 0.0693028 572 10.933053  0.0000
##  Correlation: 
##                   (Intr) cssT1. cssT2.
## casesTminus1.rate -0.034              
## casesTminus2.rate -0.052 -0.978       
## testingNearMe     -0.413 -0.234  0.156
## 
## Standardized Within-Group Residuals:
##        Min         Q1        Med         Q3        Max 
## -6.7891125 -0.4235384 -0.1281970  0.3341600  5.4944125 
## 
## Number of Observations: 600
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeTest")})
    , by = "city"
    )
```

## Multiple Regression + Testing Near Me

```r
lmTest <- 
  lm( case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
       , data = train
       )

summary(lmTest)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate + 
##     testingNearMe, data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -157.554  -10.416   -3.051    7.654  119.038 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        2.90023    1.57878   1.837   0.0667 .  
## casesTminus1.rate  1.03142    0.04023  25.638  < 2e-16 ***
## casesTminus2.rate -0.20429    0.03748  -5.451 7.35e-08 ***
## testingNearMe      0.70156    0.06951  10.093  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 25.49 on 596 degrees of freedom
## Multiple R-squared:  0.8849,	Adjusted R-squared:  0.8843 
## F-statistic:  1527 on 3 and 596 DF,  p-value: < 2.2e-16
```

```r
output <- metricsFuncLM(model = lmTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmTest")})
    , by = "city"
    )
```


## Log-transformed


```r
train <- train %>% 
  mutate(case.rate = log(case.rate)
         , casesTminus1.rate = case_when(casesTminus1.rate != 0 ~ log(casesTminus1.rate)
                                         , TRUE ~ 0.000000001)
         , casesTminus2.rate = case_when(casesTminus2.rate != 0 ~ log(casesTminus2.rate)
                                         , TRUE ~ 0.000000001)
         )

test7 <- test7  %>% 
  mutate(case.rate = log(case.rate)
         , casesTminus1.rate = case_when(casesTminus1.rate != 0 ~ log(casesTminus1.rate)
                                         , TRUE ~ 0.000000001)
         , casesTminus2.rate = case_when(casesTminus2.rate != 0 ~ log(casesTminus2.rate)
                                         , TRUE ~ 0.000000001)
         )
         

test14 <- test14  %>% 
  mutate(case.rate = log(case.rate)
         , casesTminus1.rate = case_when(casesTminus1.rate != 0 ~ log(casesTminus1.rate)
                                         , TRUE ~ 0.000000001)
         , casesTminus2.rate = case_when(casesTminus2.rate != 0 ~ log(casesTminus2.rate)
                                         , TRUE ~ 0.000000001)
         )
```
## LME-Log
NOT CONVERGING - TBD

<!-- ```{r } -->
<!-- lmeLog <- -->
<!--   lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate -->
<!--       , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city -->
<!--       , data = train -->
<!--       , control = lmeControl(maxIter = 1e8) -->
<!--       ) -->

<!-- summary(lmeLog) -->

<!-- output <- metricsFuncLME(model = lmeLog) -->

<!-- allMetrics <- allMetrics %>% left_join( -->
<!--     output$performMetrics %>% -->
<!--       rename_at(vars(-city), function(x) {paste0(x, "_lmeLog")}) -->
<!--     , by = "city" -->
<!--     ) -->

<!-- ``` -->


## LME-Log weighted residual

```r
lmeLogWt <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      , weights = varPower(form = ~ fitted(.))
      , control = lmeControl(maxIter = 1e4) 
      )

summary(lmeLogWt)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC      BIC    logLik
##   387.1919 422.3272 -185.5959
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev     Corr  
## casesTminus1.rate 0.07112405 cssT1.
## casesTminus2.rate 0.06851983 -1    
## Residual          0.72123928       
## 
## Variance function:
##  Structure: Power of variance covariate
##  Formula: ~fitted(.) 
##  Parameter estimates:
##      power 
## -0.6051554 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate 
##                        Value  Std.Error  DF   t-value p-value
## (Intercept)        0.4780339 0.05909971 573   8.08860       0
## casesTminus1.rate  1.2241265 0.03520980 573  34.76664       0
## casesTminus2.rate -0.3394812 0.02867655 573 -11.83829       0
##  Correlation: 
##                   (Intr) cssT1.
## casesTminus1.rate -0.643       
## casesTminus2.rate  0.320 -0.926
## 
## Standardized Within-Group Residuals:
##         Min          Q1         Med          Q3         Max 
## -6.42506364 -0.55142489 -0.03322022  0.54826487  4.26144800 
## 
## Number of Observations: 600
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeLogWt)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeLogWt")})
    , by = "city"
    )
```


## Multiple Regression

```r
lmLog <- 
  lm( case.rate ~ casesTminus1.rate + casesTminus2.rate
       , data = train
       )

summary(lmLog)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate, 
##     data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.97944 -0.18304 -0.00116  0.17664  1.59773 
## 
## Coefficients:
##                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        0.54679    0.05524   9.899   <2e-16 ***
## casesTminus1.rate  1.23146    0.03088  39.873   <2e-16 ***
## casesTminus2.rate -0.36501    0.02337 -15.621   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3347 on 597 degrees of freedom
## Multiple R-squared:  0.8718,	Adjusted R-squared:  0.8714 
## F-statistic:  2030 on 2 and 597 DF,  p-value: < 2.2e-16
```

```r
output <- metricsFuncLM(model = lmLog)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmLog")})
    , by = "city"
    )
```

## LME-Log + Testing Near Me
NOT CONVERGING

<!-- ```{r } -->
<!-- lmeLogTest <-  -->
<!--   lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe -->
<!--       , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city -->
<!--       , data = train -->
<!--       ) -->

<!-- summary(lmeLogTest) -->

<!-- output <- metricsFuncLME(model = lmeLogTest) -->

<!-- allMetrics <- allMetrics %>% left_join( -->
<!--     output$performMetrics %>% -->
<!--       rename_at(vars(-city), function(x) {paste0(x, "_lmeLogTest")}) -->
<!--     , by = "city" -->
<!--     ) -->

<!-- ``` -->



## LME-Log weighted residual + Testing Near Me

```r
lmeLogWtTest <- 
  lme(fixed = case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
      , random = ~ casesTminus1.rate + casesTminus2.rate - 1 | city
      , data = train
      , weights = varPower(form = ~ fitted(.))
      , control = lmeControl(maxIter = 1e4) 
      )

summary(lmeLogWtTest)
```

```
## Linear mixed-effects model fit by REML
##  Data: train 
##        AIC     BIC    logLik
##   342.9908 382.503 -162.4954
## 
## Random effects:
##  Formula: ~casesTminus1.rate + casesTminus2.rate - 1 | city
##  Structure: General positive-definite, Log-Cholesky parametrization
##                   StdDev     Corr  
## casesTminus1.rate 0.08669358 cssT1.
## casesTminus2.rate 0.08517334 -1    
## Residual          0.81870885       
## 
## Variance function:
##  Structure: Power of variance covariate
##  Formula: ~fitted(.) 
##  Parameter estimates:
##      power 
## -0.7386615 
## Fixed effects: case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe 
##                        Value  Std.Error  DF   t-value p-value
## (Intercept)        0.5935856 0.05894098 572 10.070847       0
## casesTminus1.rate  1.1268506 0.03761394 572 29.958327       0
## casesTminus2.rate -0.2946232 0.03041735 572 -9.686025       0
## testingNearMe      0.0055849 0.00070373 572  7.936149       0
##  Correlation: 
##                   (Intr) cssT1. cssT2.
## casesTminus1.rate -0.634              
## casesTminus2.rate  0.315 -0.925       
## testingNearMe      0.275 -0.324  0.167
## 
## Standardized Within-Group Residuals:
##         Min          Q1         Med          Q3         Max 
## -6.40214502 -0.55174498 -0.05655174  0.55065243  4.34174498 
## 
## Number of Observations: 600
## Number of Groups: 25
```

```r
output <- metricsFuncLME(model = lmeLogWtTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmeLogWtTest")})
    , by = "city"
    )
```

## Multiple Regression + Testing Near Me

```r
lmLogTest <- 
  lm( case.rate ~ casesTminus1.rate + casesTminus2.rate + testingNearMe
       , data = train
       )

summary(lmLogTest)
```

```
## 
## Call:
## lm(formula = case.rate ~ casesTminus1.rate + casesTminus2.rate + 
##     testingNearMe, data = train)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.85502 -0.16889 -0.01744  0.17866  1.61566 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        0.6347868  0.0545904  11.628  < 2e-16 ***
## casesTminus1.rate  1.1595225  0.0314256  36.897  < 2e-16 ***
## casesTminus2.rate -0.3414725  0.0227242 -15.027  < 2e-16 ***
## testingNearMe      0.0059612  0.0008493   7.019  6.1e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.322 on 596 degrees of freedom
## Multiple R-squared:  0.8816,	Adjusted R-squared:  0.881 
## F-statistic:  1479 on 3 and 596 DF,  p-value: < 2.2e-16
```

```r
output <- metricsFuncLM(model = lmLogTest)

allMetrics <- allMetrics %>% left_join(
    output$performMetrics %>%
      rename_at(vars(-city), function(x) {paste0(x, "_lmLogTest")})
    , by = "city"
    )
```

## Metrics for Mar-Aug

```r
allMetrics %>% select(city, starts_with("MAPE_7d")) %>%
  arrange(as.character(city)) %>%
  as.data.frame() %>%
  format(scientific = F) %>%
  print()
```

```
##             city MAPE_7d_lme MAPE_7d_lmeWt MAPE_7d_lm MAPE_7d_lmeTest
## 1    Albuquerque        0.59          0.33       0.58            0.37
## 2        Atlanta        0.39          0.53       0.43            0.46
## 3      Baltimore        0.11          0.17       0.10            0.15
## 4         Boston        0.90          0.63       0.82            1.33
## 5      Charlotte        0.08          0.18       0.12            0.11
## 6        Chicago        0.06          0.04       0.04            0.38
## 7      Cleveland        1.69          1.53       1.65            2.06
## 8         Dallas        0.20          0.18       0.20            0.28
## 9         Denver        1.08          1.09       1.05            1.13
## 10       Detroit        0.77          0.86       0.76            0.64
## 11       Houston        0.23          0.18       0.24            0.32
## 12  Indianapolis        0.31          0.39       0.28            0.18
## 13   Los Angeles        0.62          0.72       0.58            0.31
## 14    Louisville        0.14          0.14       0.14            0.23
## 15       Memphis        0.18          0.11       0.24            0.26
## 16         Miami        0.20          0.30       0.09            0.10
## 17      New York        4.46          4.78       4.20            4.63
## 18      Oklahoma        0.37          0.35       0.40            0.26
## 19       Phoenix        1.21          1.30       1.11            1.51
## 20    Pittsburgh        1.62          1.70       1.57            1.78
## 21      Portland        1.94          2.02       1.91            1.92
## 22    Sacramento        0.52          0.61       0.46            0.33
## 23 San Francisco        0.89          1.05       0.80            0.82
## 24       Seattle        2.32          2.58       2.21            2.38
## 25         Tampa        0.39          0.47       0.38            0.60
##    MAPE_7d_lmTest MAPE_7d_lmeLogWt MAPE_7d_lmLog MAPE_7d_lmeLogWtTest
## 1            0.29             0.08          0.08                 0.08
## 2            0.48             0.22          0.22                 0.22
## 3            0.14             0.11          0.10                 0.08
## 4            1.13             0.06          0.07                 0.11
## 5            0.07             0.11          0.11                 0.08
## 6            0.25             0.07          0.07                 0.03
## 7            1.86             0.19          0.18                 0.21
## 8            0.22             0.05          0.06                 0.04
## 9            1.09             0.10          0.09                 0.10
## 10           0.66             0.05          0.06                 0.06
## 11           0.31             0.14          0.14                 0.15
## 12           0.18             0.04          0.04                 0.05
## 13           0.29             0.03          0.02                 0.02
## 14           0.28             0.09          0.10                 0.13
## 15           0.29             0.13          0.14                 0.14
## 16           0.03             0.05          0.07                 0.06
## 17           4.19             0.47          0.44                 0.47
## 18           0.27             0.17          0.18                 0.14
## 19           1.56             0.11          0.09                 0.15
## 20           1.87             0.17          0.15                 0.19
## 21           1.99             0.21          0.19                 0.21
## 22           0.30             0.01          0.02                 0.02
## 23           0.77             0.05          0.05                 0.05
## 24           2.40             0.24          0.23                 0.25
## 25           0.63             0.02          0.02                 0.02
##    MAPE_7d_lmLogTest
## 1               0.08
## 2               0.22
## 3               0.08
## 4               0.11
## 5               0.08
## 6               0.04
## 7               0.19
## 8               0.05
## 9               0.08
## 10              0.06
## 11              0.16
## 12              0.06
## 13              0.03
## 14              0.15
## 15              0.16
## 16              0.08
## 17              0.44
## 18              0.15
## 19              0.13
## 20              0.17
## 21              0.19
## 22              0.04
## 23              0.03
## 24              0.24
## 25              0.02
```


```r
allMetrics %>% select(city, starts_with("MAE_7d")) %>%
  arrange(as.character(city)) %>% 
  as.data.frame() %>%
  print()
```

```
##             city MAE_7d_lme MAE_7d_lmeWt MAE_7d_lm MAE_7d_lmeTest MAE_7d_lmTest
## 1    Albuquerque      10.06         5.21      9.91           5.39          4.16
## 2        Atlanta      30.71        40.95     33.16          35.58         37.45
## 3      Baltimore       6.43        10.71      6.40           9.20          8.55
## 4         Boston      33.84        23.67     30.69          50.15         42.37
## 5      Charlotte       7.15        15.33      9.76           9.12          5.63
## 6        Chicago       4.70         3.06      3.10          29.43         19.37
## 7      Cleveland      52.87        47.63     51.45          64.38         58.27
## 8         Dallas      13.01        12.14     13.18          18.61         14.66
## 9         Denver      42.43        42.73     40.92          43.69         42.18
## 10       Detroit      35.92        40.31     35.36          29.30         30.05
## 11       Houston      31.28        25.24     33.10          42.07         40.77
## 12  Indianapolis      19.50        24.77     17.51          11.00         11.05
## 13   Los Angeles      35.00        40.57     32.35          17.11         16.07
## 14    Louisville      13.15        12.71     13.87          23.01         28.26
## 15       Memphis      23.36        15.31     31.00          33.08         36.16
## 16         Miami      16.40        24.24      7.61           7.83          2.62
## 17      New York      76.52        82.02     72.16          79.73         72.18
## 18      Oklahoma      60.20        56.72     64.76          43.73         44.67
## 19       Phoenix      48.04        51.97     43.99          60.37         62.84
## 20    Pittsburgh      56.35        59.04     54.53          61.47         64.90
## 21      Portland      59.73        62.14     58.76          59.00         61.28
## 22    Sacramento      31.38        36.86     27.98          19.65         18.05
## 23 San Francisco      44.08        51.98     39.91          40.53         37.89
## 24       Seattle      64.84        72.12     61.65          66.33         67.13
## 25         Tampa      24.39        29.52     23.94          38.17         40.59
##    MAE_7d_lmeLogWt MAE_7d_lmLog MAE_7d_lmeLogWtTest MAE_7d_lmLogTest
## 1             0.21         0.21                0.22             0.22
## 2             0.98         0.95                0.94             0.95
## 3             0.46         0.43                0.33             0.34
## 4             0.20         0.24                0.39             0.38
## 5             0.50         0.47                0.34             0.36
## 6             0.31         0.32                0.12             0.17
## 7             0.64         0.62                0.73             0.66
## 8             0.22         0.25                0.16             0.23
## 9             0.34         0.30                0.35             0.30
## 10            0.20         0.21                0.22             0.22
## 11            0.68         0.69                0.73             0.79
## 12            0.15         0.16                0.20             0.26
## 13            0.10         0.06                0.07             0.10
## 14            0.42         0.47                0.60             0.68
## 15            0.64         0.69                0.69             0.75
## 16            0.24         0.31                0.28             0.35
## 17            1.34         1.25                1.32             1.24
## 18            0.84         0.93                0.70             0.75
## 19            0.39         0.31                0.53             0.48
## 20            0.62         0.54                0.68             0.62
## 21            0.73         0.67                0.72             0.66
## 22            0.06         0.07                0.08             0.15
## 23            0.21         0.19                0.19             0.13
## 24            0.79         0.77                0.84             0.80
## 25            0.10         0.10                0.09             0.08
```


```r
allMetrics %>% select(city, starts_with("CP_7d")) %>%
  arrange(as.character(city)) %>%
  as.data.frame() %>%
  print()
```

```
##             city CP_7d_lme CP_7d_lmeWt CP_7d_lm CP_7d_lmeTest CP_7d_lmTest
## 1    Albuquerque       1.0         0.0      1.0           1.0          1.0
## 2        Atlanta       1.0         0.0      1.0           1.0          1.0
## 3      Baltimore       1.0         0.0      1.0           1.0          1.0
## 4         Boston       1.0         0.0      1.0           0.0          1.0
## 5      Charlotte       1.0         0.0      1.0           1.0          1.0
## 6        Chicago       1.0         0.5      1.0           1.0          1.0
## 7      Cleveland       0.0         0.0      1.0           0.0          0.0
## 8         Dallas       1.0         0.0      1.0           1.0          1.0
## 9         Denver       0.5         0.0      1.0           0.5          0.5
## 10       Detroit       1.0         0.0      1.0           1.0          1.0
## 11       Houston       0.5         0.5      0.5           0.5          0.5
## 12  Indianapolis       1.0         0.0      1.0           1.0          1.0
## 13   Los Angeles       1.0         0.0      1.0           1.0          1.0
## 14    Louisville       1.0         0.0      1.0           1.0          1.0
## 15       Memphis       1.0         0.5      1.0           1.0          1.0
## 16         Miami       1.0         0.0      1.0           1.0          1.0
## 17      New York       0.0         0.0      0.0           0.0          0.0
## 18      Oklahoma       0.5         0.0      0.5           0.5          0.5
## 19       Phoenix       0.5         0.0      0.5           0.5          0.5
## 20    Pittsburgh       0.0         0.0      0.5           0.0          0.0
## 21      Portland       0.0         0.0      0.0           0.0          0.0
## 22    Sacramento       1.0         0.0      1.0           1.0          1.0
## 23 San Francisco       1.0         0.0      1.0           1.0          1.0
## 24       Seattle       0.0         0.0      0.0           0.0          0.0
## 25         Tampa       1.0         0.0      1.0           1.0          1.0
##    CP_7d_lmeLogWt CP_7d_lmLog CP_7d_lmeLogWtTest CP_7d_lmLogTest
## 1               1         1.0                  1             1.0
## 2               1         0.0                  1             0.0
## 3               1         1.0                  1             1.0
## 4               1         1.0                  1             1.0
## 5               1         1.0                  1             1.0
## 6               1         1.0                  1             1.0
## 7               1         1.0                  1             0.0
## 8               1         1.0                  1             1.0
## 9               1         1.0                  1             1.0
## 10              1         1.0                  1             1.0
## 11              1         0.5                  1             0.5
## 12              1         1.0                  1             1.0
## 13              1         1.0                  1             1.0
## 14              1         1.0                  1             0.5
## 15              1         0.5                  1             0.0
## 16              1         1.0                  1             1.0
## 17              1         0.0                  1             0.0
## 18              1         0.0                  1             0.5
## 19              1         1.0                  1             0.5
## 20              1         1.0                  1             0.5
## 21              1         0.0                  1             0.0
## 22              1         1.0                  1             1.0
## 23              1         1.0                  1             1.0
## 24              1         0.0                  1             0.0
## 25              1         1.0                  1             1.0
```




```r
allMetrics %>% select(city, starts_with("MAPE_14d")) %>%
  arrange(as.character(city)) %>%
  as.data.frame() %>%
  format(scientific = F) %>%
  print()
```

```
##             city MAPE_14d_lme MAPE_14d_lmeWt MAPE_14d_lm MAPE_14d_lmeTest
## 1    Albuquerque         0.67           0.63        0.66             0.43
## 2        Atlanta         0.39           0.38        0.39             0.46
## 3      Baltimore         0.48           0.50        0.49             0.74
## 4         Boston         0.64           0.67        0.66             1.44
## 5      Charlotte         0.37           0.35        0.38             0.68
## 6        Chicago         0.53           0.55        0.52             0.72
## 7      Cleveland         1.03           1.01        1.06             2.05
## 8         Dallas         0.44           0.40        0.46             0.63
## 9         Denver         0.66           0.72        0.64             0.89
## 10       Detroit         0.64           0.69        0.64             1.05
## 11       Houston         0.33           0.37        0.33             0.49
## 12  Indianapolis         0.54           0.57        0.52             0.55
## 13   Los Angeles         0.47           0.48        0.47             1.10
## 14    Louisville         0.50           0.45        0.55             0.38
## 15       Memphis         0.42           0.39        0.50             0.44
## 16         Miami         0.47           0.52        0.43             0.74
## 17      New York         1.42           1.49        1.46             3.20
## 18      Oklahoma         0.63           0.64        0.62             0.47
## 19       Phoenix         0.76           0.80        0.72             1.14
## 20    Pittsburgh         0.75           0.77        0.78             1.87
## 21      Portland         0.79           0.76        0.81             1.94
## 22    Sacramento         0.75           0.90        0.69             1.62
## 23 San Francisco         1.05           1.43        0.90             2.50
## 24       Seattle         0.93           1.06        0.89             2.24
## 25         Tampa         0.39           0.40        0.38             0.83
##    MAPE_14d_lmTest MAPE_14d_lmeLogWt MAPE_14d_lmLog MAPE_14d_lmeLogWtTest
## 1             0.41              0.23           0.25                  0.15
## 2             0.49              0.15           0.16                  0.10
## 3             0.86              0.11           0.12                  0.09
## 4             1.48              0.16           0.16                  0.14
## 5             0.61              0.17           0.18                  0.06
## 6             0.78              0.21           0.23                  0.10
## 7             1.98              0.23           0.22                  0.21
## 8             0.63              0.19           0.20                  0.11
## 9             0.96              0.22           0.22                  0.13
## 10            1.15              0.18           0.17                  0.12
## 11            0.41              0.14           0.15                  0.07
## 12            0.58              0.19           0.21                  0.09
## 13            0.98              0.12           0.13                  0.16
## 14            0.37              0.23           0.24                  0.06
## 15            0.39              0.21           0.22                  0.06
## 16            0.78              0.15           0.18                  0.12
## 17            3.32              0.22           0.20                  0.28
## 18            0.35              0.25           0.26                  0.09
## 19            1.52              0.17           0.17                  0.16
## 20            1.75              0.18           0.18                  0.17
## 21            2.06              0.15           0.15                  0.18
## 22            1.40              0.12           0.12                  0.14
## 23            2.77              0.12           0.12                  0.26
## 24            2.49              0.15           0.15                  0.22
## 25            1.12              0.11           0.12                  0.11
##    MAPE_14d_lmLogTest
## 1                0.15
## 2                0.10
## 3                0.08
## 4                0.13
## 5                0.06
## 6                0.11
## 7                0.19
## 8                0.12
## 9                0.13
## 10               0.10
## 11               0.08
## 12               0.10
## 13               0.15
## 14               0.07
## 15               0.06
## 16               0.12
## 17               0.26
## 18               0.09
## 19               0.16
## 20               0.17
## 21               0.17
## 22               0.12
## 23               0.25
## 24               0.20
## 25               0.10
```


```r
allMetrics %>% select(city, starts_with("MAE_14d")) %>%
  arrange(as.character(city)) %>% 
  as.data.frame() %>%
  print()
```

```
##             city MAE_14d_lme MAE_14d_lmeWt MAE_14d_lm MAE_14d_lmeTest
## 1    Albuquerque      229.05        230.18     229.13          164.23
## 2        Atlanta       81.40         79.85      81.19           60.18
## 3      Baltimore       76.02         76.31      75.72           53.83
## 4         Boston      101.68         96.15     100.66           89.94
## 5      Charlotte       88.63         82.97      89.55           80.59
## 6        Chicago      229.68        231.63     226.93          147.72
## 7      Cleveland      184.12        182.23     184.71          160.40
## 8         Dallas      125.16        114.79     128.37          101.04
## 9         Denver      210.56        208.11     210.44          150.55
## 10       Detroit      146.11        138.10     145.32          107.09
## 11       Houston       57.82         55.11      59.16           56.06
## 12  Indianapolis      212.82        214.83     211.55          149.72
## 13   Los Angeles      105.91        103.76     107.53          125.34
## 14    Louisville      174.17        160.12     186.86           96.15
## 15       Memphis      120.93        109.82     136.67           71.86
## 16         Miami      114.72        114.11     116.18           91.70
## 17      New York       65.09         68.54      64.11          111.11
## 18      Oklahoma      213.26        213.80     213.90          134.91
## 19       Phoenix      133.31        133.15     129.88           96.55
## 20    Pittsburgh      131.46        134.05     131.48          127.17
## 21      Portland       72.93         70.54      73.68           86.92
## 22    Sacramento       77.62         77.59      77.77           94.44
## 23 San Francisco       56.72         70.07      51.81          121.80
## 24       Seattle       69.46         73.81      68.04          101.27
## 25         Tampa       57.55         59.10      54.91           61.69
##    MAE_14d_lmTest MAE_14d_lmeLogWt MAE_14d_lmLog MAE_14d_lmeLogWtTest
## 1          159.47             1.24          1.28                 0.73
## 2           60.84             0.77          0.84                 0.48
## 3           61.80             0.60          0.65                 0.39
## 4           94.76             0.82          0.81                 0.57
## 5           73.88             0.91          0.95                 0.28
## 6          164.19             1.25          1.35                 0.51
## 7          156.51             1.15          1.14                 0.85
## 8           97.65             1.03          1.08                 0.54
## 9          152.28             1.25          1.28                 0.63
## 10         107.77             1.02          0.98                 0.53
## 11          50.40             0.73          0.74                 0.37
## 12         152.42             1.14          1.21                 0.50
## 13         120.09             0.67          0.70                 0.77
## 14          95.59             1.32          1.35                 0.35
## 15          66.18             1.16          1.21                 0.30
## 16         104.74             0.83          0.96                 0.59
## 17         107.44             0.82          0.79                 0.96
## 18         115.73             1.44          1.49                 0.48
## 19         121.31             0.87          0.92                 0.71
## 20         120.32             0.93          0.95                 0.69
## 21          93.18             0.71          0.70                 0.69
## 22          79.17             0.63          0.62                 0.59
## 23         136.91             0.51          0.52                 0.99
## 24         113.00             0.69          0.70                 0.81
## 25          85.21             0.58          0.61                 0.46
##    MAE_14d_lmLogTest
## 1               0.73
## 2               0.50
## 3               0.36
## 4               0.51
## 5               0.27
## 6               0.55
## 7               0.78
## 8               0.59
## 9               0.65
## 10              0.42
## 11              0.42
## 12              0.54
## 13              0.74
## 14              0.36
## 15              0.32
## 16              0.62
## 17              0.85
## 18              0.52
## 19              0.70
## 20              0.68
## 21              0.64
## 22              0.51
## 23              0.98
## 24              0.76
## 25              0.42
```


```r
allMetrics %>% select(city, starts_with("CP_14d")) %>%
  arrange(as.character(city)) %>%
  as.data.frame() %>%
  print()
```

```
##             city CP_14d_lme CP_14d_lmeWt CP_14d_lm CP_14d_lmeTest CP_14d_lmTest
## 1    Albuquerque       0.36         0.07      0.36           0.36          0.36
## 2        Atlanta       0.50         0.00      0.50           0.43          0.43
## 3      Baltimore       0.64         0.00      0.64           0.43          0.43
## 4         Boston       0.57         0.00      0.57           0.21          0.29
## 5      Charlotte       0.50         0.07      0.50           0.43          0.43
## 6        Chicago       0.36         0.07      0.36           0.29          0.21
## 7      Cleveland       0.29         0.00      0.29           0.14          0.14
## 8         Dallas       0.50         0.00      0.50           0.36          0.50
## 9         Denver       0.36         0.07      0.36           0.21          0.21
## 10       Detroit       0.50         0.00      0.50           0.14          0.14
## 11       Houston       0.64         0.07      0.57           0.36          0.57
## 12  Indianapolis       0.43         0.07      0.43           0.29          0.36
## 13   Los Angeles       0.64         0.00      0.64           0.29          0.29
## 14    Louisville       0.36         0.07      0.29           0.29          0.36
## 15       Memphis       0.43         0.00      0.36           0.36          0.43
## 16         Miami       0.50         0.07      0.50           0.29          0.21
## 17      New York       0.36         0.00      0.36           0.00          0.00
## 18      Oklahoma       0.07         0.00      0.07           0.14          0.36
## 19       Phoenix       0.43         0.07      0.43           0.36          0.21
## 20    Pittsburgh       0.50         0.07      0.50           0.21          0.21
## 21      Portland       0.50         0.00      0.50           0.36          0.36
## 22    Sacramento       0.50         0.00      0.64           0.21          0.36
## 23 San Francisco       0.29         0.07      0.71           0.07          0.07
## 24       Seattle       0.43         0.00      0.43           0.14          0.07
## 25         Tampa       0.57         0.00      0.71           0.57          0.43
##    CP_14d_lmeLogWt CP_14d_lmLog CP_14d_lmeLogWtTest CP_14d_lmLogTest
## 1             0.64         0.21                1.00             0.57
## 2             0.86         0.43                1.00             0.64
## 3             0.86         0.57                1.00             0.79
## 4             0.86         0.50                0.93             0.79
## 5             0.86         0.36                1.00             0.86
## 6             0.57         0.36                1.00             0.71
## 7             0.64         0.36                0.86             0.36
## 8             0.64         0.21                1.00             0.50
## 9             0.50         0.36                0.93             0.50
## 10            0.57         0.43                1.00             0.71
## 11            0.86         0.50                1.00             0.79
## 12            0.57         0.36                1.00             0.57
## 13            0.79         0.64                1.00             0.43
## 14            0.50         0.14                1.00             1.00
## 15            0.64         0.21                1.00             0.86
## 16            0.71         0.43                1.00             0.50
## 17            0.93         0.43                0.79             0.57
## 18            0.57         0.00                1.00             0.64
## 19            0.71         0.50                0.93             0.50
## 20            0.71         0.57                0.86             0.64
## 21            0.86         0.43                0.86             0.64
## 22            0.86         0.64                1.00             0.64
## 23            0.93         0.71                0.93             0.29
## 24            0.93         0.43                0.86             0.64
## 25            0.93         0.57                1.00             0.64
```
