library(dplyr)
library(lubridate)

set.seed(2017)
options(digits=4)

(expenses <- data_frame(
  date=seq(as.Date("2016-01-01"), as.Date("2016-12-31"), by=1),
  amount=rgamma(length(date), shape = 2, scale = 20)))


head(expenses)

expenses %>% 
  group_by(yr = year(date), mon = month(date)) %>% 
  summarise(mn_amt = mean(amount))