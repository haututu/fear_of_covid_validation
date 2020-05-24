library(eRm)
library(difR)



rasch_3 <- covid_dat %>% 
  filter(lockdown == "lvl3") %>%
  select(starts_with("Fof")) %>% 
  RSM()

rasch_4 <- covid_dat %>% 
  filter(lockdown == "lvl4") %>%
  select(starts_with("Fof")) %>% 
  RSM()

rasch

plotPImap(rasch, cex.gen = .55)

summary(rasch_4)

plotPWmap(rasch_4, pmap = TRUE, imap = TRUE)

test <- covid_dat %>% 
  filter(lockdown == "lvl4" & Gender != "other") %>%
  select(starts_with("Fof"), Gender) %>%
  #mutate(Gender = ifelse(Gender == "male", 0, 1)) %>%
  mutate_if(is.numeric, ~ifelse(. > 3, 1, 0))

difMH(test, group = 8, focal.name = "male")

itemfit(person.parameter(rasch_4))
