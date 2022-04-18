# UHC box plot

# load packages
pacman::p_load(tidyverse, dplyr, ggplot2, magrittr, ggthemr)

# set the data directory
path <- here::here("references", "SDG_3_data")

# load uhc data
uhc <- rio::import(paste0(path, "/6_uhc.CSV")) %>%
  filter(indicator_name=="UHC effective coverage index", year_id==2019)

who <- rio::import(here::here("WHO_agg_data.xlsx"))

# mark the countries of our interest
uhc %<>% mutate(
  vietnam = ifelse(location_name=="Viet Nam", 1, 0),
  rwanda = ifelse(location_name=="Rwanda", 1, 0),
  peru = ifelse(location_name=="Peru", 1, 0),
  cuba = ifelse(location_name=="Cuba", 1, 0),
  nigeria = ifelse(location_name=="Nigeria", 1, 0),
  oman = ifelse(location_name=="Oman", 1, 0)
)

# Merge
uhc_merged <- left_join(uhc, who, by = c("location_name" = "Country")) %>%
  filter(!is.na(WHO_Region)) %>%
  mutate(
    WB_income = factor(WB_income, levels = c("High", "UM", "LM", "Low"))
  )

# plot
ggthemr("fresh")

plot_merged <-
ggplot(uhc_merged,
       aes(x=val, y=log(WHO_Cases_cumulative))) +
  geom_point(aes(color = WB_income,
                 size = WHO_Cases_cumulative_per_100k)) +
  xlab("UHC Effective Coverage Score (0-100), 2019") +
  ylab("Log (Cumulative COVID-19 Cases)")+
  scale_x_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20))+
  scale_color_discrete(name = "Income Group") +
  scale_size_continuous(name = "Cumulative COVID-19\n cases per 100K")+
  facet_grid(.~ WHO_Region) +
  theme(panel.spacing = unit(1.5, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        strip.text = element_text(size = 9, face = "bold"),
        strip.background = element_rect(color = "black", size = 1)
        ) 

save_plot_sep(plot_merged,
              paste0("UHC_COVID_merged_by_region.png"),
              width = 13,
              height = 5)

