# Load data
read_sav("data/L4Clean.sav") %>% 
  select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("Lockdown"), starts_with("Behaviour_C"), Political_Beliefs, FearCovid = FearofCovid) %>%
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
  mutate(id = row_number()) %>%
  select(starts_with("FofC"), Gender, Age) %>%
  write_csv("study_one.csv")

read_sav("data/L3Clean.sav") %>% 
  select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("Lockdown"), starts_with("Behaviour_C"), Political_Beliefs, FearCovid) %>%
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
  mutate(id = row_number()) %>%
  select(starts_with("FofC"), Gender, Age) %>%
  write_csv("study_two.csv")
