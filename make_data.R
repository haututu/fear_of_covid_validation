library(tidyverse)
library(lavaan)
library(haven)
library(brms)
library(eRm)

# Load data
covid_dat <- bind_rows(
  read_sav("data/L3Clean.sav") %>% select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("WEMWBS")),
  read_sav("data/L4Clean.sav") %>% select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("WEMWBS"))
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
  )

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

# Fear of covid ############################################
covid_cols <- colnames(covid_dat)[grepl("FofCOVID", colnames(covid_dat))]

covid_fear <- cfa(
  paste("fear =~", paste0(covid_cols, collapse = " + ")),
  data = covid_dat
)

summary(covid_fear, fit=TRUE, standardized=TRUE)

# PVD ##################################################
infect_cols <- colnames(covid_dat)[grepl("PVD_", colnames(covid_dat)) & grepl("_[2568]|1[024]", colnames(covid_dat))]
germ_cols <- colnames(covid_dat)[grepl("PVD_", colnames(covid_dat)) & !grepl("_[2568]|1[024]", colnames(covid_dat))]

covid_pvd <- cfa(
  paste(
    paste("infect =~", paste0(infect_cols, collapse = " + ")),
    paste("germ =~", paste0(germ_cols, collapse = " + ")),
    sep="\n"
  ),
  data = covid_dat
)

# Mental wellbeing ############################################
covid_cols <- colnames(covid_dat)[grepl("WEMWBS", colnames(covid_dat))]

covid_mwb <- cfa(
  paste("MWB =~", paste0(covid_cols, collapse = " + ")),
  data = covid_dat
)

summary(covid_fear, fit=TRUE, standardized=TRUE)

summary(covid_pvd, fit=TRUE, standardized=TRUE)

summary(covid_mwb, fit=TRUE, standardized=TRUE)

######################################### Comparisons ################################################

# Infect to fear
cor.test(predict(covid_pvd)[,1], predict(covid_fear))

# Germ to fear
cor.test(predict(covid_pvd)[,2], predict(covid_fear))

# MWB to fear
cor.test(predict(covid_mwb), predict(covid_fear))

# Cronback alpha
ltm::cronbach.alpha(covid_dat %>% select(covid_cols), standardized = TRUE)
ltm::cronbach.alpha(covid_dat %>% select(pvd_cols), standardized = TRUE)
ltm::cronbach.alpha(covid_dat %>% select(covid_cols), standardized = TRUE)


