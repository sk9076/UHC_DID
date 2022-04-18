pacman::p_load(tidyverse, ggplot2, dplyr, magrittr, ggthemr, linelist, openxlsx,latex2exp, gtsummary)
ggthemr("fresh")

# load the UHC data
uhc <- rio::import(here::here("data", "WHO_agg_data.xlsx")) %>% 
  clean_data() 

uhc_2019 <- rio::import(here::here("data", "uhc_2019.xlsx")) %>% 
  filter(year_id==2019, indicator_name=="UHC effective coverage index") %>%
  select(location_name, val) %>%
  clean_data()

colnames(uhc_2019) <- c("location_name", "uhc_2019")

uhc %<>% left_join(uhc_2019, c("country" = "location_name")) %>%
  mutate(
    country = case_when(
      country== "united_kingdom_of_great_britain_and_northern_ireland" ~ "united_kingdom",
      country== "united_states_of_america" ~ "united_states",
      country == "republic_of_north_macedonia " ~ "north_macedonia",
      TRUE ~ country
    )
  )

uhc %>% ggplot(aes(uhc_2019)) + geom_histogram()+
  xlab("UHC SCI 2019 (Range 0 - 100)") + ylab("Count") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  geom_vline(xintercept = 80, linetype = "dotted", color = "black")

summary(uhc$uhc_2019) +
  
  
  uhc %<>% mutate(
    uhc_cat = as.factor(ntile(uhc_2019, 4)),
    uhc_cat2 = ifelse(uhc_cat == "4", "Top quantile", "Rest"),
    uhc_cat3 = ifelse(uhc_cat %in% c("4", "3"), "Top half", "Bottom half"),
    uhc_cat4 = ifelse(uhc_cat == "1", "Bottom quantile", "Rest"),
    uhc_cat5 = ifelse(uhc_2019>=80, "UHC Index >=80", "UHC Index <80"),
    uhc_cat6 = ifelse(uhc_2019<50, "UHC Index <50", "UHC Index >=50"),
    uhc_cat7 = ifelse(uhc_2019 <50, "UHC Index <50", 
                      ifelse(uhc_2019>=80, "UHC Index >=80", NA))
  ) 

# load immunization data
ipv_path <- here::here("data", "wuenic2020rev_data_2021-07-28.csv")

ipv_dat <- rio::import(ipv_path) %>% clean_data()

# merge with the uhc data
dat_merged <- ipv_dat %>% left_join(uhc, by = c("name" = "country")) %>%
  mutate(
    vaccine = toupper(vaccine),
    prepost = ifelse(year<=2019, 0, 1),
    wb_income = factor(wb_income, levels = c("low", "lm", "um", "high")),
    who_region = factor(who_region, levels = c("americas", "europe", "western_pacific",
                                               "eastern_mediterranean", "south_east_asia",
                                               "africa"))
  ) %>%
  filter(!is.na(uhc_2019),
         !vaccine %in% c("IPV1", "YFV"))

# load GHSI data
ghs <- rio::import(here::here("data", "ghsi_2019.xlsx"))%>% clean_data()

dat_merged %<>% left_join(ghs, c("name" = "country2"))