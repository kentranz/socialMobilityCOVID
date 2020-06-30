library(imputeTS)
library(stringr)
apple.raw <- apple # CHANGE TO MATCH KEN'S NAMESPACE
# rm(apple)

mobility <- data.frame()

for (indexCity in 1:length(cities))
{
  

print(cities[indexCity])

#Replace "Toronto" with the name of city you want to separate
City <- which(apple.raw$region == cities[indexCity])

apple.first <- as.Date(gsub('\\.', '-', substr(names(apple.raw)[7], start = 2, 11)))
apple.last <- as.Date(gsub('\\.', '-', substr(names(apple.raw)[ncol(apple.raw)], start = 2, 11)))
apple.days <- seq(apple.first, apple.last, by = "day")
apple.city <- data.frame(date = apple.days, 
                        driving = rep(NA, length(apple.days)),
                        transit = rep(NA, length(apple.days)),
                        walking = rep(NA, length(apple.days)))
for (i in 1:length(City))
{
  apple.city[,i+1]<-round(na_seadec(ts(as.numeric(apple.raw[City[i],7:ncol(apple.raw)])), "interpolation"),2)
}
#### Standardization
## Labeling each week, starting January 13
wk.dat2 <- read.csv("data/week2.csv")
week.2 <- numeric()
for(ind in 1:nrow(apple.city))
{
  for(ind2 in 1:length(wk.dat2))
  {
    if(apple.city$date[ind]%in%as.Date(wk.dat2[,ind2]))
      week.2[ind] <- ind2
  }
}
apple.city$week.number <- week.2
day <- rep(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), ncol(wk.dat2))
day <- day[1:nrow(apple.city)]
apple.city$day<-day
base.weeks <- c(1,2,3)
attr.names <- names(apple.city)[2:4]
for (attr in attr.names)
{
  #print(attr)
  for (x in unique(apple.city$day))
  {
    #print(x)
    each.day <- which(apple.city[,"day"]==x)
    med <- (median(apple.city[,attr][each.day][base.weeks]))/100
    #print(apple.city[,attr][each.day])
    #print(med)
    apple.city[,attr][each.day]<-apple.city[,attr][each.day]/med
    #print(apple.city[,attr][each.day])
  }
}
apple.city[,2:4] <- sapply(round(apple.city[,2:4]-100,2),as.numeric)
# apple.city[,2:4] <- unclass(round(apple.city[,2:4]-100,2))
apple.city[,2:4]


apple.city %<>% mutate(region = cities[indexCity])

mobility <- rbind(mobility, apple.city)
}




