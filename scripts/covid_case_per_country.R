# COVID-19 cases per country

# load packages
pacman::p_load(tidyverse, dplyr, ggplot2, magrittr, ggthemr)

# set the data directory
path <- here::here("references")

# load data
covid <- rio::import(paste0(path, "/WHO-COVID-19-global-data.csv")) %>%
  linelist::clean_variable_names() %>%
  mutate(
    fatality = new_deaths/new_cases*100
  )

str(covid)

# country names
countries <- c("Cuba", "Viet Nam", "Peru", "Rwanda", "Nigeria", "Oman")
countries %in% unique(covid$country) 

# set theme
ggthemr("fresh")

# plot
# for entire period (til Jul 2021)
for(cnt in countries){
  temp <- covid %>% filter(country == cnt,
                           !is.na(new_cases))
                          
  ymax <- max(temp$new_cases, na.rm=T)/100 %>% ceiling()*100
  
  plot <-
    ggplot(data = temp,
         aes(x=date_reported,
             y=new_cases)) +
    # New case bar plot
    geom_bar(stat="identity", width=1,
             aes(fill = "New cases")) +
    # % death dots and smoothed curve
    geom_smooth(data = temp,
               aes(x=date_reported,
                   y=(fatality)*ymax/100,
                   color = "% Death among new cases"), 
               size=.5,
               method = "loess") +
      geom_point(data = temp,
                  aes(x=date_reported,
                      y=(fatality)*ymax/100,
                      color = "% Death among new cases"), 
                  size=.5)+
    # Y-axis adjustment and adding second axis (%)
    scale_y_continuous(
      limits = c(0, ymax),
      expand = c(0, 0.1),
      sec.axis = sec_axis(
        ~./ymax*100,
        name = "% Death among new cases",
        breaks = seq(0, 100, 20),
        labels = paste0(seq(0, 100, 20), rep("%", 6))
      )
    )+
    # X-axis adjustment
    scale_x_date(
      date_breaks = "3 months",
      #date_minor_breaks = "1 month",
      limits = c(as.Date("2020-01-01"), as.Date("2021-07-31")),
      date_labels =  format("%b-%d\n %Y"),
      expand = c(0, 0.1)
    )+
    # Color change
    scale_color_manual(
      values = c("% Death among new cases" = "#2f4a75")
    )+
    # Theme set
    theme(
      axis.text.x=element_text(size = 9),
      legend.position = c(0.2, 0.85),
      legend.title = element_blank(),
      plot.caption = element_text(size = 8)
    )+
    guides(color = guide_legend(override.aes = list(fill = "white")))+
    # Label axis titles
    xlab("Date of Report") +
    ylab("Number of Cases") +
    # Label plot title
    ggtitle(cnt,
            subtitle = "New COVID-19 Cases And % of Deaths Over Time")+
    labs(caption = "(Source: WHO COVID-19 Dashboard as of Jul 8, 2021)")
  
  save_plot_sep(plot,
                paste0(cnt, "_covid_trend.png"),
                width = 7,
                height = 4.2)
  
}

# For 2020 only
for(cnt in countries){
  temp <- covid %>% filter(country == cnt,
                           !is.na(new_cases),
                           date_reported<=as.Date("2020-12-31"))
  ymax <- max(temp$new_cases, na.rm=T)/100 %>% ceiling()*100
  
  plot <-
    ggplot(data = temp,
           aes(x=date_reported,
               y=new_cases)) +
    # New case bar plot
    geom_bar(stat="identity", width=1,
             aes(fill = "New cases")) +
    # % death dots and smoothed curve
    geom_smooth(data = temp,
                aes(x=date_reported,
                    y=(fatality)*ymax/100,
                    color = "% Death among new cases"), 
                size=.5,
                method = "loess") +
    geom_point(data = temp,
               aes(x=date_reported,
                   y=(fatality)*ymax/100,
                   color = "% Death among new cases"), 
               size=.5)+
    # Y-axis adjustment and adding second axis (%)
    scale_y_continuous(
      limits = c(0, ymax),
      expand = c(0, 0.1),
      sec.axis = sec_axis(
        ~./ymax*100,
        name = "% Death among new cases",
        breaks = seq(0, 100, 20),
        labels = paste0(seq(0, 100, 20), rep("%", 6))
      )
    )+
    # X-axis adjustment
    scale_x_date(
      date_breaks = "1 month",
      #date_minor_breaks = "1 month",
      limits = c(as.Date("2020-01-01"), as.Date("2020-12-31")),
      date_labels =  format("%b-%d\n %Y"),
      expand = c(0, 0.1)
    )+
    # Color change
    scale_color_manual(
      values = c("% Death among new cases" = "#2f4a75")
    )+
    # Theme set
    theme(
      axis.text.x=element_text(size = 9),
      legend.position = c(0.2, 0.85),
      legend.title = element_blank(),
      plot.caption = element_text(size = 8)
      )+
    guides(color = guide_legend(override.aes = list(fill = "white")))+
    # Label axis titles
    xlab("Date of Report") +
    ylab("Number of Cases") +
    # Label plot title
    ggtitle(cnt,
            subtitle = "New COVID-19 Cases And % of Deaths Over Time (Jan - Dec 2020)")+
    labs(caption = "(Source: WHO COVID-19 Dashboard)")
  
  save_plot_sep(plot,
                paste0(cnt, "_covid_trend_2020.png"),
                width = 7,
                height = 4.2)
  
}
