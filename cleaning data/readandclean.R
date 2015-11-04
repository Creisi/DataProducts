require(dplyr)
require(lubridate)
require(ggplot2)

#reading data 
donations <- read.csv(unz( "data/P00000001-ALL.zip", "P00000001-ALL.csv"), header = F, stringsAsFactors = F)
spent <-  read.csv(unz( "data/P00000001D-ALL.zip", "P00000001D-ALL.csv"), header = F, stringsAsFactors = F)

#get rid of the header and useless data 
donations <- donations[c(-1),c(3,6,10,11)]
names(donations) <- c("Candidate", "State", "Amount", "Date")
spent <- spent[c(-1), c(3,5,6,8)]
names(spent) <- c("Candidate",  "Amount", "Date", "State")

donations <- donations %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(Amount = as.numeric(Amount)) %>% 
  mutate(Candidate = as.factor(Candidate)) %>% 
  mutate(State = as.factor(State))
  
donationsPerCandidateAndState <- donations %>% 
  select(Candidate, State, Amount) %>% 
  group_by(Candidate, State) %>% 
  summarise(total = sum(Amount))


donationsPerCandidate <- donations %>% 
  select(Candidate, Amount) %>% 
  group_by(Candidate) %>% 
  summarise(TotalDonations = sum(Amount))

spent <- spent %>% 
  mutate(Date = dmy(Date)) %>% 
  mutate(Amount = as.numeric(Amount)) %>% 
  mutate(Candidate = as.factor(Candidate)) %>% 
  mutate(State = as.factor(State))

spentPerCandidateAndState <- spent %>% 
  select(Candidate, State, Amount) %>% 
  group_by(Candidate, State) %>% 
  summarise(Total = sum(Amount))

spentPerCandidate <- spent %>% 
  select(Candidate,  Amount) %>% 
  group_by(Candidate) %>% 
  summarise(TotalSpent = sum(Amount))

canditates <- full_join(spentPerCandidate, donationsPerCandidate)

ggplot(data = canditates, aes(
  x = TotalSpent, 
  y = TotalDonations, 
  label = Candidate,
  guide = F,
  size = TotalDonations - TotalSpent)) + geom_point(colour="white", fill="red", shape=21) +
  geom_text(size = 4) + 
  scale_x_continuous(name="$ spent per candidate", limits = c(0, 1e6)) +
  scale_y_continuous(name="$ Recieved in donations per candidate", limits = c(0, 1e6)) 
  
