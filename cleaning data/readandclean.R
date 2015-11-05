require(dplyr)
require(lubridate)
require(ggplot2)

#reading data 
donations <- read.csv(unz( "data/P00000001-ALL.zip", "P00000001-ALL.csv"), header = F, stringsAsFactors = F)
spent <-  read.csv(unz( "data/P00000001D-ALL.zip", "P00000001D-ALL.csv"), header = F, stringsAsFactors = F)
states <- read.csv("data/state_table.csv")

#get rid of the header and useless data 
donations <- donations[c(-1),c(3,6,10,11)]
names(donations) <- c("Candidate", "State", "Amount", "Date")
spent <- spent[c(-1), c(3,5,6,8)]
names(spent) <- c("Candidate",  "Amount", "Date", "State")

donations <- donations %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(Amount = as.numeric(Amount)) %>% 
  mutate(Candidate = as.factor(Candidate)) %>% 
  filter(State %in% states$abbreviation ) %>%
  mutate(State = as.factor(State))

spent <- spent %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(Amount = as.numeric(Amount)) %>% 
  mutate(Amount = -Amount) %>% 
  mutate(Candidate = as.factor(Candidate)) %>% 
  filter(State %in% states$abbreviation ) %>%
  mutate(State = factor(State, levels = levels(donations$State)))


test<-  bind_rows(donations, spent) 

