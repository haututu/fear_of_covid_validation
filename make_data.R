library(tidyverse)
library(lavaan)
library(haven)
library(brms)

covid_dat <- bind_rows(
  read_sav("data/L3Clean.sav"),
  read_sav("data/L4Clean.sav")
  ) %>%
  select(ResponseId, Gender, Age, starts_with("FofCOVID")) %>%
  mutate(Gender = case_when(
    Gender == 1 ~ "male", 
    Gender == 2 ~ "female"
  )
  ) %>%
  na.omit()

#sapply(covid_dat, function(x) attributes(x)$label) %>% 
#  data.frame(var = names(.), description = .) %>% 
#  mutate(description = gsub("\n", "", description)) %>%
#  mutate(description = gsub("Please indicate how much you agree with each of the following statements. Respond withhow you feel right now. - ", "", description)) %>%
#  mutate(description = gsub("Please indicatehow much you agree with each of the following statements. Respond withhow you feel right now.", "", description)) %>%
#  write_csv("question_labels.csv")

covid_fear <- cfa(
  paste("fear =~", paste0(covid_cols[grepl("FofCOVID", covid_cols)], collapse = " + ")),
  missing = "ML",
  data = covid_dat
)

covid_dat <- bind_cols(
  select(covid_dat, ResponseId, Age, Gender, Political_Beliefs, National, Labour), 
  data.frame(predict(covid_authority)),
  data.frame(predict(covid_fear))
) %>%
  mutate_if(is.numeric, function(x) {as.numeric(scale(x))})