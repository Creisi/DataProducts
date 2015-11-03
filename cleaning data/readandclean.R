require(dplyr)
require(lubridate)
## for this project I want is summarised per state and candidate 

#reading data 
donations <- read.csv(unz( "data/P00000001-ALL.zip", "P00000001-ALL.csv"), header = F, stringsAsFactors = F)
spend <-  read.csv(unz( "data/P00000001D-ALL.zip", "P00000001D-ALL.csv"), header = F, stringsAsFactors = F)

#get rid of the header and useless data 
donations <- donations[c(-1),c(3,6,10,11)]
names(donations) <- c("Candidate", "State", "Amount", "Date")
spend <- spend[c(-1), c(3,5,6,8)]
names(spend) <- c("Candidate",  "Amount", "Date", "State")

donations <- donations %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(Amount = as.numeric(Amount)) %>% 
  mutate(Candidate = as.factor(Candidate)) %>% 
  mutate(State = as.factor(State))

spend <- spend %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(Amount = as.numeric(Amount)) %>% 
  mutate(Candidate = as.factor(Candidate)) %>% 
  mutate(State = as.factor(State))
