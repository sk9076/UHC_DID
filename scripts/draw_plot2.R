## define country names
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
  
  # Get the y-axis max for each plot
  ymax1 <- round(max(
    max(maternal$value[which(maternal$country==pais)], na.rm=T),
    max(neon$value[which(neon$country==pais)], na.rm=T),
    na.rm=T
  )/100, 0)*100
  ymax2 <- round(max(chronic$value[which(chronic$location==pais)], na.rm=T)/100, 0)*100
  ymax3 <- round(max(maltub$val[which(maltub$location==pais)], na.rm=T)/100, 0)*100
  ymax4 <- round(max(hcw$value[which(hcw$country_name==pais)], na.rm=T)/100, 0)*100
  
  ymax1 <- ifelse(ymax1<100, ymax1+20, ymax1+100)
  ymax2 <- ifelse(ymax2<100, ymax2+20, ymax2+100)
  ymax3 <- ifelse(ymax3<100, ymax3+20, ymax3+100)
  ymax4 <- ifelse(ymax4<100, ymax4+20, ymax4+100)
  
  # Build plot 
  p1 <-
    ggplot()+
    
    # Set plot aesthetics
    scale_x_continuous(limits = c(2009, 2021),
                       breaks = seq(2009, 2021, 2)) +
    scale_y_continuous(limits = c(0, ymax1),
                       sec.axis = sec_axis(~ ./(ymax1/100),
                                           name= "UHC Effective Coverage Index (0-100)")) +
    xlab("Year") +
    ylab("Mortality Rate/Ratio")+
    ggtitle("Maternal and Child Mortality") +
    theme(legend.position="bottom",
          legend.title=element_blank())+
    guides(color = guide_legend(nrow=4)) +
    
    # Add UHC index
    geom_area(data=uhc %>% filter(location_name==pais),
              aes(x=year_id, y=val*(ymax1/100), fill = "UHC effective coverage index (0 - 100)"), alpha = 0.2) +
    
    # Add MMR
    geom_point(data = maternal%>%filter(country==pais), 
               aes(x=year, y=value, color = "Maternal mortality ratio (per 100,000 live births)")) + 
    geom_line(data = maternal%>%filter(country==pais), 
              aes(x=year, y=value, color = "Maternal mortality ratio (per 100,000 live births)")) + 
    
    # Add neonatal mortality and under 5 mortality
    geom_point(data=neon %>% filter(country==pais),
               aes(x=year, y=value, color=paste0(indicator, " (per 1000 live births)"))) + 
    geom_line(data=neon %>% filter(country==pais),
              aes(x=year, y=value, color = paste0(indicator, " (per 1000 live births)")))
    
    
  p2 <-
    ggplot()+
    
    # Set plot aesthetics
    scale_x_continuous(limits = c(2009, 2021),
                       breaks = seq(2009, 2021, 2)) +
    scale_y_continuous(limits = c(0, ymax2),
                       sec.axis = sec_axis(~ ./(ymax2/100),
                                           name= "UHC Effective Coverage Index (0-100)")) +
    xlab("Year") +
    ylab("Mortality Rate")+
    ggtitle("Mortality Attributed to Non-Communicable Diseases",
            subtitle = "(Cardiovascular Diseases, Cancer, Diabetes, Chronic Respiratory Illness)") +
    theme(legend.position="bottom",
          legend.title=element_blank())+
    guides(color = guide_legend(nrow = 3)) +
    
    # Add UHC index
    geom_area(data=uhc %>% filter(location_name==pais),
              aes(x=year_id, y=val*(ymax2/100), fill = "UHC effective coverage index (0 - 100)"), alpha = 0.2) +
    # Add chronic disease mortality
    geom_point(data=chronic %>% filter(location==pais),
               aes(x=year, y=value, 
                   color = "Mortality rate attributed to cardiovascular diseases, cancer, \ndiabetes, or chronic respiratory diseases (per 100,000)")) +
    geom_line(data=chronic %>% filter(location==pais),
              aes(x=year, y=value, 
                  color = "Mortality rate attributed to cardiovascular diseases, cancer, \ndiabetes, or chronic respiratory diseases (per 100,000)")) 
    
  
  p3 <-
    ggplot()+
    
    # Set plot aesthetics
    scale_x_continuous(limits = c(2009, 2021),
                       breaks = seq(2009, 2021, 2)) +
    scale_y_continuous(limits = c(0, ymax3),
                       sec.axis = sec_axis(~ ./(ymax3/100),
                                           name= "UHC Effective Coverage Index (0-100)")) +
    xlab("Year") +
    ylab("Incidence rate")+
    ggtitle("Incidence of Malaria and Tuberculosis") +
    theme(legend.position="bottom",
          legend.title=element_blank())+
    guides(color = guide_legend(nrow = 3)) +
    
    # Add UHC index
    geom_area(data=uhc %>% filter(location_name==pais),
              aes(x=year_id, y=val*(ymax3/100), fill = "UHC effective coverage index (0 - 100)"), alpha = 0.2) +
    
    # Add tuberculosis and malaria incidence
    geom_point(data=maltub %>% filter(location==pais),
               aes(x=year, y=val, color = paste0(cause, " incidence (per 100,000)"))) +
    geom_line(data=maltub %>% filter(location==pais),
              aes(x=year, y=val, color = paste0(cause, " incidence (per 100,000)")))
    
  p4 <-
    ggplot()+
    
    # Set plot aesthetics
    scale_x_continuous(limits = c(2009, 2021),
                       breaks = seq(2009, 2021, 2)) +
    scale_y_continuous(limits = c(0, ymax4),
                       sec.axis = sec_axis(~ ./(ymax4/100),
                                           name= "UHC Effective Coverage Index (0-100)")) +
    xlab("Year") +
    ylab("Density")+
    ggtitle("Density of Healthcare Workers") +
    theme(legend.position="bottom",
          legend.title=element_blank())+
    guides(color = guide_legend(nrow = 3)) +
    
    # Add UHC index
    geom_area(data=uhc %>% filter(location_name==pais),
              aes(x=year_id, y=val*(ymax4/100), fill = "UHC effective coverage index (0 - 100)"), alpha = 0.2) +
    
    # Add HCW density
    geom_point(data=hcw %>% filter(country_name==pais),
               aes(x=year, y=value, color = paste0(cat, " density (per 1000)")))+
    geom_line(data=hcw %>% filter(country_name==pais),
              aes(x=year, y=value, color = paste0(cat, " density (per 1000)")))
  
  plots[[pais]] <- ggpubr::ggarrange(p1, p2, p3, p4) %>% 
    ggpubr::annotate_figure(top = ggpubr::text_grob(pais, face = "bold", size = 20))
  
  save_plot_sep(plots[[pais]],
            paste0(pais, "_sdg3_sep.png"))
}

plots
