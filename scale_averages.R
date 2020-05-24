# covid_dat <- bind_rows(
#   read_sav("data/L3Clean.sav") %>% select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("WEMWBS")) %>% mutate(lockdown = "lvl3"),
#   read_sav("data/L4Clean.sav") %>% select(Gender, Age, Ethnicity, starts_with("FofCOVID_"), starts_with("PVD_"), starts_with("WEMWBS")) %>% mutate(lockdown = "lvl4")
# ) %>%
#   mutate(Gender = case_when(
#     Gender == 1 ~ "male", 
#     Gender == 2 ~ "female",
#     TRUE ~ "other"
#   ),
#   Ethnicity = case_when(
#     Ethnicity == 1 ~ "european",
#     Ethnicity %in% 2:3 ~ "maori_poly",
#     Ethnicity == 4 ~ "asian",
#     TRUE ~ "other"
#   )
#   )

############### Averages for each measure
covid_dat %>%
  select(lockdown, id, contains("FofC")) %>%
  na.omit() %>%
  gather(var, value, -id, -lockdown) %>%
  group_by(lockdown, id) %>%
  summarise(foc = sum(value),
            nas = sum(is.na(value))
            ) %>%
  group_by(lockdown) %>%
  summarise(m = mean(foc),
            sd = sd(foc))

covid_dat %>%
  select(lockdown, id, contains("PVD")) %>%
  na.omit() %>%
  gather(var, value, -id, -lockdown) %>%
  group_by(lockdown, id) %>%
  summarise(pvd = sum(value),
            nas = sum(is.na(value))
  ) %>%
  group_by(lockdown) %>%
  summarise(m = mean(pvd),
            sd = sd(pvd))

covid_lvl3 %>%
  select(lockdown, id, contains("WEMWBS")) %>%
  na.omit() %>%
  gather(var, value, -id, -lockdown) %>%
  group_by(lockdown, id) %>%
  summarise(WEMWBS = sum(value),
            nas = sum(is.na(value))
  ) %>%
  group_by(lockdown) %>%
  summarise(m = mean(WEMWBS),
            sd = sd(WEMWBS))

covid_dat %>%
  group_by(lockdown) %>%
  summarise(m = mean(Political_Beliefs),
            sd = sd(Political_Beliefs))
#####################################

############## Getting stats on FCOV
covid_dat %>%
  mutate(id = row_number()) %>%
  select(lockdown, id, contains("FofC")) %>%
  na.omit() %>%
  gather(var, value, -id, -lockdown) %>%
  group_by(lockdown, id) %>%
  summarise(foc = sum(value)) %>%
  group_by(lockdown) %>%
  summarise(m = mean(foc),
            sd = sd(foc),
            min = min(foc),
            max = max(foc),
            kurt = e1071::kurtosis(foc),
            skew = e1071::skewness(foc)
            ) %>%
  t() %>%
  knitr::kable() %>%
  kableExtra::kable_styling()

#######################################

sjPlot::tab_itemscale(covid_dat %>%
                        filter(lockdown == "lvl3") %>%
                         mutate(id = row_number()) %>%
                         select(contains("FofC")) %>%
                         na.omit(),
                      show.kurtosis = TRUE)

sjPlot::tab_itemscale(covid_dat %>%
                        filter(lockdown == "lvl4") %>%
                        mutate(id = row_number()) %>%
                        select(contains("FofC")) %>%
                        na.omit(),
                      show.kurtosis = TRUE)


########################## CORRELATIONS

fear <- covid_dat %>%
  select(lockdown, id, contains("FofC")) %>%
  na.omit() %>%
  gather(var, value, -id, -lockdown) %>%
  group_by(lockdown, id) %>%
  summarise(foc = sum(value),
            nas = sum(is.na(value))
  )

infect <- covid_dat %>%
  select(lockdown, id, infect_cols) %>%
  na.omit() %>%
  gather(var, value, -id, -lockdown) %>%
  group_by(lockdown, id) %>%
  summarise(infect = sum(value))

avert <- covid_dat %>%
  select(lockdown, id, germ_cols) %>%
  na.omit() %>%
  gather(var, value, -id, -lockdown) %>%
  group_by(lockdown, id) %>%
  summarise(avert = sum(value))

mwb <- covid_lvl3 %>%
  select(lockdown, id, mwb_cols) %>%
  na.omit() %>%
  gather(var, value, -id, -lockdown) %>%
  group_by(lockdown, id) %>%
  summarise(mwb = sum(value))

total <- fear %>%
  left_join(infect, by=c("lockdown", "id")) %>%
  left_join(avert, by=c("lockdown", "id")) %>%
  left_join(mwb, by=c("lockdown", "id"))

cor.test(total$foc, total$infect)
cor.test(total$foc, total$avert)



cor.test(filter(covid_dat, lockdown == "lvl4")$Political_Beliefs, predict(covid_fear_lvl4), method = "spearman")
cor.test(filter(covid_dat, lockdown == "lvl3")$Political_Beliefs, predict(covid_fear_lvl3), method = "spearman")

covid_dat %>%
  group_by(lockdown) %>%
  summarise(Political_Beliefs_m = mean(Political_Beliefs),
            Political_Beliefs_sd = sd(Political_Beliefs)
  ) %>%
  gather(measure, value, -lockdown) %>%
  mutate(func = ifelse(grepl("m", measure), "m", "sd"),
         measure = gsub("_m|_sd", "", measure)) %>%
  spread(func, value)

covid_dat %>%
  group_by(lockdown) %>%
  summarise(Lockdown_Law1_m = mean(Lockdown_Law1),
            Lockdown_Law2_m = mean(Lockdown_Law2),
            Lockdown_Law3_m = mean(Lockdown_Law3),
            Lockdown_Law4_m = mean(Lockdown_Law4),
            Lockdown_Law5_m = mean(Lockdown_Law5),
            Lockdown_Law1_sd = sd(Lockdown_Law1),
            Lockdown_Law2_sd = sd(Lockdown_Law2),
            Lockdown_Law3_sd = sd(Lockdown_Law3),
            Lockdown_Law4_sd = sd(Lockdown_Law4),
            Lockdown_Law5_sd = sd(Lockdown_Law5)
            ) %>%
  gather(measure, value, -lockdown) %>%
  mutate(func = ifelse(grepl("m", measure), "m", "sd"),
         measure = gsub("_m|_sd", "", measure)) %>%
  spread(func, value)

covid_dat %>%
  group_by(lockdown) %>%
  summarise(Behaviour_Change1_m = mean(Behaviour_Change1),
            Behaviour_Change2_m = mean(Behaviour_Change2),
            Behaviour_Change3_m = mean(Behaviour_Change3),
            Behaviour_Change4_m = mean(Behaviour_Change4),
            Behaviour_Change5_m = mean(Behaviour_Change5),
            Behaviour_Change6_m = mean(Behaviour_Change6),
            Behaviour_Change7_m = mean(Behaviour_Change7),
            Behaviour_Change8_m = mean(Behaviour_Change8),
            Behaviour_Change1_sd = sd(Behaviour_Change1),
            Behaviour_Change2_sd = sd(Behaviour_Change2),
            Behaviour_Change3_sd = sd(Behaviour_Change3),
            Behaviour_Change4_sd = sd(Behaviour_Change4),
            Behaviour_Change5_sd = sd(Behaviour_Change5),
            Behaviour_Change6_sd = sd(Behaviour_Change6),
            Behaviour_Change7_sd = sd(Behaviour_Change7),
            Behaviour_Change8_sd = sd(Behaviour_Change8)
  ) %>%
  gather(measure, value, -lockdown) %>%
  mutate(func = ifelse(grepl("m", measure), "m", "sd"),
         measure = gsub("_m|_sd", "", measure)) %>%
  spread(func, value)


##################### Lockdown laws
data.frame(
  "lvl3" = c(
    "Lockdown_Law1" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law1, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Lockdown_Law2" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law2, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Lockdown_Law3" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law3, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Lockdown_Law4" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law4, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Lockdown_Law5" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law5, predict(covid_fear_lvl3), method = "spearman")$estimate
    ),
  
  "lvl4" = c(
    "Lockdown_Law1" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law1, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Lockdown_Law2" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law2, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Lockdown_Law3" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law3, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Lockdown_Law4" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law4, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Lockdown_Law5" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law5, predict(covid_fear_lvl4), method = "spearman")$estimate
    )
  )

data.frame(
  "lvl3" = c(
    "Lockdown_Law1" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law1, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Lockdown_Law2" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law2, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Lockdown_Law3" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law3, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Lockdown_Law4" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law4, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Lockdown_Law5" = cor.test(filter(covid_dat, lockdown == "lvl3")$Lockdown_Law5, predict(covid_fear_lvl3), method = "spearman")$p.value
  ),
  
  "lvl4" = c(
    "Lockdown_Law1" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law1, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Lockdown_Law2" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law2, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Lockdown_Law3" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law3, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Lockdown_Law4" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law4, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Lockdown_Law5" = cor.test(filter(covid_dat, lockdown == "lvl4")$Lockdown_Law5, predict(covid_fear_lvl4), method = "spearman")$p.value
  )
)

##################### behaviour change
data.frame(
  "lvl3" = c(
    "Behaviour_Change1" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change1, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Behaviour_Change2" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change2, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Behaviour_Change3" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change3, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Behaviour_Change4" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change4, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Behaviour_Change5" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change5, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Behaviour_Change6" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change6, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Behaviour_Change7" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change7, predict(covid_fear_lvl3), method = "spearman")$estimate,
    "Behaviour_Change8" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change8, predict(covid_fear_lvl3), method = "spearman")$estimate
  ),
  
  "lvl4" = c(
    "Behaviour_Change1" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change1, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Behaviour_Change2" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change2, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Behaviour_Change3" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change3, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Behaviour_Change4" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change4, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Behaviour_Change5" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change5, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Behaviour_Change6" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change6, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Behaviour_Change7" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change7, predict(covid_fear_lvl4), method = "spearman")$estimate,
    "Behaviour_Change8" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change8, predict(covid_fear_lvl4), method = "spearman")$estimate
  )
)

data.frame(
  "lvl3" = c(
    "Behaviour_Change1" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change1, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Behaviour_Change2" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change2, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Behaviour_Change3" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change3, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Behaviour_Change4" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change4, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Behaviour_Change5" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change5, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Behaviour_Change6" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change6, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Behaviour_Change7" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change7, predict(covid_fear_lvl3), method = "spearman")$p.value,
    "Behaviour_Change8" = cor.test(filter(covid_dat, lockdown == "lvl3")$Behaviour_Change8, predict(covid_fear_lvl3), method = "spearman")$p.value
  ),
  
  "lvl4" = c(
    "Behaviour_Change1" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change1, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Behaviour_Change2" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change2, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Behaviour_Change3" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change3, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Behaviour_Change4" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change4, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Behaviour_Change5" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change5, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Behaviour_Change6" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change6, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Behaviour_Change7" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change7, predict(covid_fear_lvl4), method = "spearman")$p.value,
    "Behaviour_Change8" = cor.test(filter(covid_dat, lockdown == "lvl4")$Behaviour_Change8, predict(covid_fear_lvl4), method = "spearman")$p.value
  )
)
