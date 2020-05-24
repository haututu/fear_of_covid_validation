library(tidyverse)
library(lavaan)
library(haven)
library(brms)
library(eRm)

# Load data
covid_dat <- bind_rows(
  read_sav("data/L3Clean.sav") %>% select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("Lockdown"), starts_with("Behaviour_C"), Political_Beliefs, FearCovid) %>% mutate(lockdown = "lvl3"),
  read_sav("data/L4Clean.sav") %>% select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("Lockdown"), starts_with("Behaviour_C"), Political_Beliefs, FearCovid = FearofCovid) %>% mutate(lockdown = "lvl4")
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
  filter(!is.na(Gender) & !is.na(Age) & !is.na(FearCovid)) %>%
  na.omit() %>%
  mutate(id = row_number())

covid_lvl3 <- read_sav("data/L3Clean.sav") %>% 
  select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("WEMWBS"), FearCovid) %>% 
  mutate(lockdown = "lvl3") %>%
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
  filter(!is.na(Gender) & !is.na(Age) & !is.na(FearCovid)) %>%
  na.omit() %>%
  mutate(id = row_number())

# Demographic information

covid_dat %>%
  group_by(lockdown) %>%
  summarise(female_percent = mean(Gender == "female"),
            age_mean = mean(Age),
            age_sd = sd(Age),
            age_max = max(Age),
            european_percent = mean(Ethnicity == "european"),
            maori_poly_percent = mean(Ethnicity == "maori_poly"),
            asian_percent = mean(Ethnicity == "asian"),
            other_percent = mean(Ethnicity == "other")
            ) %>%
  gather(key, value, -1) %>%
  spread(lockdown, value) %>%
  knitr::kable() %>%
  kableExtra::kable_styling()

# Fear of covid ############################################
covid_cols <- colnames(covid_dat)[grepl("FofCOVID", colnames(covid_dat))]

covid_fear_lvl3 <- cfa(
  paste("fear =~", paste0(covid_cols, collapse = " + ")),
  data = covid_dat %>% filter(lockdown == "lvl3")
)

covid_fear_lvl4 <- cfa(
  paste("fear =~", paste0(covid_cols, collapse = " + ")),
  data = covid_dat %>% filter(lockdown == "lvl4")
)

summary(covid_fear_lvl3, fit=TRUE, standardized=TRUE)
summary(covid_fear_lvl4, fit=TRUE, standardized=TRUE)


# PVD ##################################################
infect_cols <- colnames(covid_dat)[grepl("PVD_", colnames(covid_dat)) & grepl("_[2568]|1[024]", colnames(covid_dat))]
germ_cols <- colnames(covid_dat)[grepl("PVD_", colnames(covid_dat)) & !grepl("_[2568]|1[024]", colnames(covid_dat))]

covid_pvd_lvl3 <- cfa(
  paste(
    paste("infect =~", paste0(infect_cols, collapse = " + ")),
    paste("germ =~", paste0(germ_cols, collapse = " + ")),
    sep="\n"
  ),
  data = covid_dat %>% filter(lockdown == "lvl3")
)

covid_pvd_lvl4 <- cfa(
  paste(
    paste("infect =~", paste0(infect_cols, collapse = " + ")),
    paste("germ =~", paste0(germ_cols, collapse = " + ")),
    sep="\n"
  ),
  data = covid_dat %>% filter(lockdown == "lvl4")
)

# Mental wellbeing ############################################
mwb_cols <- colnames(covid_lvl3)[grepl("WEMWBS", colnames(covid_lvl3))]

covid_mwb <- cfa(
  paste("MWB =~", paste0(mwb_cols, collapse = " + ")),
  data = covid_lvl3
)

covid_fear_for_mwb <- cfa(
  paste("fear =~", paste0(covid_cols, collapse = " + ")),
  data = covid_lvl3
)

summary(covid_fear, fit=TRUE, standardized=TRUE)

summary(covid_mwb, fit=TRUE, standardized=TRUE)

######################################### Comparisons ################################################

# Infect to fear
cor.test(predict(covid_pvd_lvl3)[,1], predict(covid_fear_lvl3))
cor.test(predict(covid_pvd_lvl4)[,1], predict(covid_fear_lvl4))

# Germ to fear
cor.test(predict(covid_pvd_lvl3)[,2], predict(covid_fear_lvl3))
cor.test(predict(covid_pvd_lvl4)[,2], predict(covid_fear_lvl4))

# MWB to fear
cor.test(predict(covid_mwb), predict(covid_fear_for_mwb))

cor.test(filter(covid_dat, lockdown == "lvl4")$Political_Beliefs, predict(covid_fear_lvl4))
cor.test(filter(covid_dat, lockdown == "lvl3")$Political_Beliefs, predict(covid_fear_lvl3))

# Cronback alpha
covid_dat %>% filter(lockdown == "lvl3") %>% select(covid_cols) %>% na.omit() %>% ltm::cronbach.alpha(standardized = TRUE)
covid_dat %>% filter(lockdown == "lvl4") %>% select(covid_cols) %>% na.omit() %>% ltm::cronbach.alpha(standardized = TRUE)

covid_dat %>% filter(lockdown == "lvl3") %>% select(infect_cols) %>% na.omit() %>% ltm::cronbach.alpha(standardized = TRUE)
covid_dat %>% filter(lockdown == "lvl4") %>% select(infect_cols) %>% na.omit() %>% ltm::cronbach.alpha(standardized = TRUE)

covid_dat %>% filter(lockdown == "lvl3") %>% select(germ_cols) %>% na.omit() %>% ltm::cronbach.alpha(standardized = TRUE)
covid_dat %>% filter(lockdown == "lvl4") %>% select(germ_cols) %>% na.omit() %>% ltm::cronbach.alpha(standardized = TRUE)

covid_lvl3 %>% filter(lockdown == "lvl3") %>% select(mwb_cols) %>% na.omit() %>% ltm::cronbach.alpha(standardized = TRUE)


