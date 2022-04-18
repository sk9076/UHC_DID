# load packages
pacman::p_load(tidyverse, dplyr, ggplot2, magrittr, ggthemr)

# load data
sds <- rio::import(here::here("references/countrydata.csv")) %>%
  mutate(
    date1 = as.Date(as.character(Date), "%Y%m%d"),
    CountryName = ifelse(CountryName=="Vietnam", "Viet Nam", CountryName)
  ) %>%
  filter(Jurisdiction == "NAT_TOTAL") 

# set theme
ggthemr("fresh")

# all country
p_oxf_tot <-
ggplot(sds, aes(x=date1, y=StringencyIndex)) + 
  geom_point(aes(shape = "Individual Country"), alpha=0.01) + 
  geom_smooth(aes(color = "Smoothed Trend")) +
  scale_color_manual(values = c("Smoothed Trend" = "#325a99"))+
  scale_x_date(date_labels = format("%b\n%Y"))+
  labs(x='Date', y = 'Oxford Stringency Index (0-100)') +
  guides(color = guide_legend(override.aes = list(fill = "white")),
         shape = guide_legend(override.aes = list(alpha = 1)))+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(c(0.5,1,0.5,0.5), unit = "cm")) +
  ggtitle('All countries',
          subtitle = 'Oxford Stringency Index, 01 January 2020 to present')

save_plot_sep(p_oxf_tot,
              paste0("Oxford_SI_all.png"),
              width = 9,
              height = 5)

# for each country

countries <- c("Viet Nam", "Rwanda", "Nigeria", "Oman", "Peru", "Cuba")

for(country in countries){
  p <-
    ggplot() + 
    geom_point(data = sds %>% filter(CountryName==country),
               aes(x=date1, y = StringencyIndex,
                   shape = country), alpha=0.3) + 
    geom_smooth(data=sds, 
                aes(x=date1,
                    y=StringencyIndex,
                    color = "Smoothed Trend (All Countries)")) +
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
    scale_color_manual(values = c("Smoothed Trend (All Countries)" = "#325a99"))+
    labs(x='Date', y = 'Oxford Stringency Index (0-100)') +
    guides(color = guide_legend(override.aes = list(fill = "white")),
           shape = guide_legend(override.aes = list(alpha = 1)))+
    theme(legend.title = element_blank(),
         legend.position = "bottom",
         plot.margin = margin(c(0.5,1,0.5,0.5), unit = "cm")) +
    ggtitle(country,
            subtitle = 'Oxford Stringency Index, 01 January 2020 to present')
  
  save_plot_sep(p,
                paste0("Oxford_SI_", country, ".png"),
                width = 9,
                height = 5)
}

