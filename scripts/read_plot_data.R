# load packages
pacman::p_load(tidyverse, dplyr, ggplot2, magrittr, ggthemr)

# set the data directory
path <- here::here("references", "SDG_3_data")

# Read data
# maternal mortality
maternal <- rio::import(paste0(path, "/1_maternal_mortality_ratio.xlsx"), skip=8) %>%
  linelist::clean_variable_names() %>%
  mutate_at(c("year", "value"), as.numeric) %>%
  filter(!is.na(value), year>=2009)

# neonatal mortality
neon <- rio::import(paste0(path, "/2_neonatal_under5_mortality_rate.csv")) %>%
  linelist::clean_variable_names() %>%
  mutate(
    country = sub(".*: ", "", ref_area_geographic_area),
    indicator = sub(".*: ", "", indicator_indicator)
  )%>% 
  select(country, indicator, time_period_time_period, obs_value_observation_value, unit_measure_unit_of_measure)
colnames(neon) <- c("country", "indicator", "year", "value", "unit") 

neon%<>%
  filter(!is.na(value), year>=2009)

# chronic disease mortality
chronic <- rio::import(paste0(path, "/3_chronic_mortality.csv")) %>%
  filter(metric=="Rate") %>%
  group_by(location, year) %>%
  summarize(
    value = sum(val)
  )%>%
  filter(!is.na(value))

# malaria, tuberculosis
maltub <- rio::import(paste0(path, "/4_malaria_tuberculosis.csv")) %>%
  filter(metric=="Rate", !is.na(val))

# health worker density
hcw <- rio::import(paste0(path, "/5_physician_density.csv"), skip = 4, header = T) %>%
  linelist::clean_variable_names() %>%
  gather(key = "year", value = "value", -country_name, -country_code, -indicator_name, -indicator_code)%>%
  mutate(cat = "Physician")

nurse <- rio::import(paste0(path, "/5_nurses_midwives_density.csv"), skip = 4, header = T) %>%
  linelist::clean_variable_names() %>%
  gather(key = "year", value = "value", -country_name, -country_code, -indicator_name, -indicator_code)%>%
  mutate(cat = "Nurses and midwives")


chw <- rio::import(paste0(path, "/5_comm_hc_density.csv"), skip = 4, header = T) %>%
  linelist::clean_variable_names() %>%
  gather(key = "year", value = "value", -country_name, -country_code, -indicator_name, -indicator_code)%>%
  mutate(cat = "Community health worker")

hcw %<>% rbind(nurse, chw) %>%
  mutate_at("year", as.numeric) %>%
  mutate(country_name = ifelse(country_name=="Vietnam", "Viet Nam", country_name)) %>%
  filter(year >=2000, !is.na(value))

# uhc
uhc <- rio::import(paste0(path, "/6_uhc.CSV")) %>%
  filter(indicator_name=="UHC effective coverage index")
