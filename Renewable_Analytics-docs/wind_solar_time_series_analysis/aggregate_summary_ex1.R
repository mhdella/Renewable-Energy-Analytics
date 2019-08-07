##### https://ro-che.info/articles/2017-02-22-group_by_month_r
##### https://stackoverflow.com/questions/57344042/why-did-this-aggregation-r-code-give-the-total-sum-rather-than-the-monthly-aggre
library(dplyr)
library(lubridate)
set.seed(2017)
options(digits=4)

(expenses <- data_frame(
  date=seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by=1),
  amount=rgamma(length(date), shape = 2, scale = 20)))

Monthly<-expenses %>% group_by(month=floor_date(date, "month")) %>%
  summarize(amount=sum(amount))

monthly2<-expenses %>% mutate(Mon=month(date), Day=day(date)) %>%
  group_by(Mon,Day) %>% 
  summarize(amount=sum(amount))

