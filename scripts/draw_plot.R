# define country names
country <- c("Cuba", "Viet Nam", "Peru", "Rwanda", "Nigeria", "Oman")

# set base theme
ggthemr("fresh")

# set palette
set.seed(283781)
random_colours <- sample(colors()[-c(1, 253, 361)], 20)

ugly <- define_palette(
  swatch = random_colours,
  gradient = c(lower = random_colours[1], upper = random_colours[2])
)

ggthemr(ugly)

# Saving plots
plots <- list()

for(pais in country){
  # Get the y-axis max
  ymax <- max(
    max(maternal$value[which(maternal$country==pais)], na.rm=T),
    max(neon$value[which(neon$country==pais)], na.rm=T),
    max(maltub$val[which(maltub$location==pais)], na.rm=T),
    max(hcw$value[which(hcw$country_name==pais)], na.rm=T),
    max(chronic$value[which(chronic$location==pais)], na.rm=T),
    na.rm=T
  )
  
  ymax_new <- round(ymax/100,0)*100+100
  
  # Build plot 
  plots[[pais]]<-
    ggplot()+
    
    # Set plot aesthetics
    scale_x_continuous(limits = c(2009, 2021),
                       breaks = seq(2009, 2021, 2)) +
    scale_y_continuous(limits = c(0, ymax_new),
                       sec.axis = sec_axis(~ ./(ymax_new/100),
                                           name= "UHC Effective Coverage Index (0-100)")) +
    xlab("Year") +
    ylab("Rate/Ratio/Density")+
    ggtitle(pais,
            subtitle = "Trend of Selected SDG3 Indicators before COVID-19 Pandemic (2009-2019)") +
    theme(legend.position="right",
          legend.title=element_blank())+
    guides(color = guide_legend(nrow = 11)) +
    
    # Add UHC index
    geom_area(data=uhc %>% filter(location_name==pais),
              aes(x=year_id, y=val*(ymax_new/100), fill = "UHC effective coverage index (0 - 100)"), alpha = 0.2) +
    
    # Add MMR
    geom_point(data = maternal%>%filter(country==pais), 
               aes(x=year, y=value, color = "Maternal mortality ratio (per 100,000 live births)")) + 
    geom_line(data = maternal%>%filter(country==pais), 
               aes(x=year, y=value, color = "Maternal mortality ratio (per 100,000 live births)")) + 
    
    # Add neonatal mortality and under 5 mortality
    geom_point(data=neon %>% filter(country==pais),
               aes(x=year, y=value, color=paste0(indicator, " (per 1000 live births)"))) + 
    geom_line(data=neon %>% filter(country==pais),
              aes(x=year, y=value, color = paste0(indicator, " (per 1000 live births)"))) +
    
    # Add chronic disease mortality
    geom_point(data=chronic %>% filter(location==pais),
               aes(x=year, y=value, 
                   color = "Mortality rate attributed to cardiovascular disease, cancer, \ndiabetes, or chronic respiratory diseases (per 100,000)")) +
    geom_line(data=chronic %>% filter(location==pais),
               aes(x=year, y=value, 
                   color = "Mortality rate attributed to cardiovascular disease, cancer, \ndiabetes, or chronic respiratory diseases (per 100,000)")) +
    # Add tuberculosis and malaria incidence
    geom_point(data=maltub %>% filter(location==pais),
               aes(x=year, y=val, color = paste0(cause, " incidence (per 100,000)"))) +
    geom_line(data=maltub %>% filter(location==pais),
                 aes(x=year, y=val, color = paste0(cause, " incidence (per 100,000)"))) +
  
    # Add HCW density
    geom_point(data=hcw %>% filter(country_name==pais),
               aes(x=year, y=value, color = paste0(cat, " density (per 1000)")))+
    geom_line(data=hcw %>% filter(country_name==pais),
               aes(x=year, y=value, color = paste0(cat, " density (per 1000)")))
  
    save_plot(plots[[pais]],
              paste0(pais, "_sdg3.png"))
}

plots
