
R295 <- function(vector1, vector2, rounding = 3, type = 'pearson')
{
  paste0(
  round(
      cor(vector1, vector2, use = 'pairwise.complete.obs', method = type)^2
    , rounding)
    , ' ['
    , round(cor.test(vector1, vector2, use = 'pairwise.complete.obs', method = type)$conf.int[2]^2
            , rounding)
    , ', '
    , round(cor.test(vector1, vector2, use = 'pairwise.complete.obs', method = type)$conf.int[1]^2
            , rounding)
    , ']'

  )
}

#R295(c(1,2,3,452), c(34,397,32,34))

#cor.test(c(1,2,3,543), c(324,397,32,34))$conf.int



