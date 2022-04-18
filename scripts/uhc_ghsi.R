pacman::p_load(tidyverse, ggplot2, magrittr, dplyr, linelist)

ghs <- rio::import(here::here("data", "ghsi_2019.xlsx"))%>% clean_data()

uhc_2019 <- rio::import(here::here("data", "uhc_2019.xlsx")) %>% 
  filter(year_id==2019, indicator_name=="UHC effective coverage index") %>%
  select(location_name, val) %>%
  clean_data()

ghs %<>% left_join(uhc_2019, c("country2" = "location_name")) %>%
  mutate(
    country2 = case_when(
      country2== "united_kingdom_of_great_britain_and_northern_ireland" ~ "united_kingdom",
      country2== "united_states_of_america" ~ "united_states",
      country2 == "republic_of_north_macedonia " ~ "north_macedonia",
      TRUE ~ country
    )
  ) %>%
  mutate(
      vietnam = ifelse(country2=="vietnam", "Viet Nam", NA),
      rwanda = ifelse(country2=="rwanda", "Rwanda", NA),
      peru = ifelse(country2=="peru", "Peru", NA),
      cuba = ifelse(country2=="cuba", "Cuba", NA),
      nigeria = ifelse(country2=="nigeria", "Nigeria", NA),
      oman = ifelse(country2=="oman", "Oman", NA)
    ) %>%
  mutate(
    flag = ifelse(country2 %in% c("vietnam", "rwanda", "peru", "nigeria", "oman", "cuba"), 1, 0)
  ) %>%
  mutate(
    income_grp = case_when(
      country2 %in% c("vietnam", "nigeria") ~ "Lower-Middle",
      country2 %in% c("rwanda") ~ "Low",
      country2 %in% c("peru", "cuba") ~ "Upper-Middle",
      country2 == "oman" ~ "High",
      TRUE ~ "."
    )
  )
      


ggthemr::ggthemr("fresh")

p <- ggplot(ghs, aes(x=val, y=ghsi)) + 
  geom_point(aes(color=income_grp,
                 size = as.factor(flag)))+
 scale_color_manual(
   name = "World Bank Income Group", 
   values= c(
              "High" = "#99b3ff",
              "Upper-Middle" = "#db93ff",
              "Lower-Middle" = "#ffeb63",
              "Low" = "#ffb399",
                "." = "#e0e0eb"),
   breaks = c("High", "Upper-Middle", "Lower-Middle", "Low")
  ) +
  scale_size_manual(
    values = c(1, 5),
    guide = FALSE
  )+
  xlab("UHC SCI 2019") +
  ylab("GHSI 2019") +
  theme(legend.position = c(0.3, 0.8),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
  geom_text(
            aes(72.6, 33.7, label="Cuba"),                 , 
            size=4)+
  geom_text(
    aes(38.3, 35.8, label="Nigeria"),                 , 
    size=4)+
  geom_text(
    aes(71.2,41.1, label="Oman"),                 , 
    size=4)+
  geom_text(
    aes(75.7, 47.2, label="Peru"),                 , 
    size=4)+
  geom_text(
    aes(59.3, 33.2, label="Rwanda"),                 , 
    size=4) +
  geom_text(
    aes(59.7, 47.1, label="Viet Nam"),                 , 
    size=4)

ggsave(path =here::here("results"),
       filename = "uhc_ghsi_plot",
       plot = p,
       width = 4.50,
       height = 4.20,
       dpi = 100)

