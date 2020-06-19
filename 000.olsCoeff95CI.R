

fittedOLS <- function(output, input, fitData, rounding = 3)
{
  formula <- as.formula(paste0(output, '~', input))
  
  model <- lm(formula, data = fitData)
  # return(model)
  #print(model)

  coeffs <- round(summary(model)$coeff,rounding) %>%
    as.data.frame() %>%
    rownames_to_column('Vars') %>%
    rename(p = 'Pr(>|t|)'
           , stdError = 'Std. Error') %>%
     select(-'t value') %>%
    mutate(Vars = case_when(Vars == '(Intercept)' ~ 'Intercept'
                            , TRUE ~ Vars)
           , p = case_when( p < 0.001 ~ paste0('<0.001 *')
                        , p < 0.05 ~ paste0(p, ' *')
                        , TRUE ~ as.character(p))
           ) %>%
  #   cbind(round(confint(model), rounding)) %>%
  #   rename(lowerCI = '2.5 %', upperCI = '97.5 %') %>%
  #   mutate(Estimate_95CI = paste0(Estimate, ' [', lowerCI, ', ', upperCI, ']')) %>%
  #   select(-Estimate, -lowerCI, -upperCI) %>%
     select(Vars, Estimate, everything()) %>%
     as.data.frame()
   #   
   coeffs[nrow(coeffs)+1, 'Vars'] <- 'R2 Adjusted Training'
   coeffs[nrow(coeffs), 'Estimate'] <- paste0(round(summary(model)$r.squared, rounding)
                                                  # , ' ['
                                                  # , round(psychometric::CI.Rsqlm(model)$LCL, rounding)
                                                  # , ', '
                                                  # , round(psychometric::CI.Rsqlm(model)$UCL, rounding)
                                                  # , ']'
                                                  )

  out <- list('model' = model, 'coeffs' = coeffs)
  return(out)
}
