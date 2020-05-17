library(tidyverse)
library(lavaan)
library(haven)
library(brms)
library(eRm)

# Load data
covid_dat <- bind_rows(
  read_sav("data/L3Clean.sav") %>% select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_")),
  read_sav("data/L4Clean.sav") %>% select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"))
  ) %>%
  mutate(Gender = case_when(
    Gender == 1 ~ "male", 
    Gender == 2 ~ "female",
    TRUE ~ "other"
  ),
  Ethnicity = case_when(
    Ethnicity == 1 ~ "european",
    Ethnicity %in% 2:3 ~ "maori_poly",
    Ethnicity == 4 ~ "asian",
    TRUE ~ "other"
  )
  ) %>%
  na.omit()

# Demographic information

covid_dat %>%
  summarise(female_percent = mean(Gender == "female"),
            age_mean = mean(Age),
            age_sd = sd(Age),
            european_percent = mean(Ethnicity == "european"),
            maori_poly_percent = mean(Ethnicity == "maori_poly"),
            asian_percent = mean(Ethnicity == "asian"),
            other_percent = mean(Ethnicity == "other")
            ) %>%
  gather(key, value)

covid_cols <- colnames(covid_dat)[grepl("FofCOVID", colnames(covid_dat))]

# CFA
covid_fear <- cfa(
  paste("fear =~", paste0(covid_cols, collapse = " + ")),
  missing = "ML",
  data = covid_dat
)

summary(covid_fear, fit=TRUE, standardized=TRUE)

# Cronback alpha
ltm::cronbach.alpha(covid_dat %>% select(covid_cols), standardized = TRUE)

# Rasch model
rsm_results <- RSM(covid_dat %>% select(covid_cols))
itemfit(covid_dat %>% select(covid_cols))

covid_dat <- bind_cols(
  select(covid_dat, ResponseId, Age, Gender, Political_Beliefs, National, Labour), 
  data.frame(predict(covid_authority)),
  data.frame(predict(covid_fear))
) %>%
  mutate_if(is.numeric, function(x) {as.numeric(scale(x))})