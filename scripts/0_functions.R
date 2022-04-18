pacman::p_load(tidyverse, ggplot2, dplyr, magrittr, ggthemr, linelist, openxlsx,latex2exp, gtsummary)
ggthemr("fresh")
# load the vaccination coverage and merge with UHC file
load_ipv <- function(path, sheet, uhc){
  dat <- rio::import(path,
                     which = sheet) %>%
    clean_data() %>%
    left_join(uhc, by = c("cname" = "country")) %>%
    filter(!is.na(uhc_2017))
  
  return(dat)
}

# convert data into long form and filter out the years
convert_long <- function(data){
  core_var <- colnames(data)[!colnames(data) %in% as.character(c(1980:2019))]
  dat_long <- data %>% 
    gather("year", "coverage", -any_of(core_var)) %>%
    filter(year >= 1990, year <=2011) %>%
    mutate(
      year = as.numeric(year),
      pre_post = ifelse(year == 2009, 1, 0),
      pre_post2 = ifelse(year == 2010, 1, 0),
      #pre_post3 = ifelse(year <2011, 0, 1),
      uhc_cat_did = ifelse(uhc_cat2 == "UHC", 1, 0)
    )
  
  return(dat_long)
}

save_plot <- function(plot, filename){
  ggsave(plot=plot,
         filename=filename,
         path = here::here("results"),
         units = "in",
         dpi = 100,
         width = 11,
         height = 5)
}

save_plot_sep <- function(plot, filename, width = 15, height = 11){
  ggsave(plot=plot,
         filename=filename,
         path = here::here("results"),
         units = "in",
         dpi = 100,
         width = width,
         height = height)
}

do_did <- function(dat, cutoff, from=1997){
  
  print(sprintf("Using %s variable for stratification", cutoff))
  dat$cat <- dat[,cutoff]
  
  subt <- ifelse(cutoff=="uhc_cat5",
                 "Countries with UHC Index >= 80 vs. the rest",
                 ifelse(cutoff=="uhc_cat4",
                        "Countries with bottom quantile of UHC Index vs. the rest",
                        ifelse(cutoff=="uhc_cat3",
                               "Bottom half vs. Top half of UHC Index",
                               ifelse(cutoff=="uhc_cat2", "Countries in top quantile of UHC Index vs. the rest",
                                      ifelse(cutoff=="uhc_cat6", 
                                             "Countries with UHC Index <50 vs. the rest", "Countirs with UHC Index >=80 vs. <50")))))
  
  dat%<>% filter(year>=from)
  
  print("*****OVERALL********")
  # DiD
  p <- ggplot(dat, aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    #facet_wrap(.~vaccine) + 
    geom_vline(xintercept = 2019, linetype = "dashed")+
    theme(legend.title = element_blank()) +
    scale_x_continuous(limits = c(from, 2020), breaks = seq(from, 2020, 2))+
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%)")+
    ggtitle("Immunization coverage over time - All vaccines",
            subtitle = subt)
  
  
  did_overall <- glm(coverage ~ year + prepost + cat +
                      prepost*cat +
                      wb_income+who_region,
                    data = dat
  )
  
  # interrupted time series with control group
  #did_overall <- glm(coverage ~ year + prepost + cat +
  #                    prepost*year +
  #                    prepost*cat +
  #                    year*cat +
  #                    prepost*year*cat+
  #                    
  #                    wb_income+who_region,
  #                  data = dat
  #)
  
  print(summary(did_overall))
  
  print("*****BCG********")
  
  # BCG (I excluded 2015 datapoint bc it looked awkward)
  p_bcg <- ggplot(dat %>% filter(vaccine=="BCG", year!=2015), aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 6),
          axis.title = element_text(size =8)) +
    scale_x_continuous(limits = c(from, 2020), breaks = seq(from, 2020, 2))+
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%)")+
    #facet_wrap(.~vaccine) + 
    geom_vline(xintercept = 2019, linetype = "dashed")+
    ggtitle("BCG")
  
  
  did_bcg <- glm(coverage ~ year + prepost + cat +
                   prepost*cat +
                  wb_income+who_region,
                data = dat %>% filter(vaccine=="BCG", year!=2015) 
  )
  
  summary(did_bcg)%>% print()
  
  print("*****DTP3********")
  
  # DTP3
  p_dtp <- ggplot(dat %>% filter(vaccine=="DTP3"), aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 6),
          axis.title = element_text(size =8)) +
    scale_x_continuous(limits = c(from, 2020), breaks = seq(from, 2020, 2))+
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%)")+
    #facet_wrap(.~vaccine) + 
    geom_vline(xintercept = 2019, linetype = "dashed")+
    ggtitle("DTP3")
  
  
  did_dtp3 <- glm(coverage ~ year + prepost + cat +
                    prepost*cat +
                   wb_income+who_region,
                 data = dat %>% filter(vaccine=="DTP3") 
  )
  
  summary(did_dtp3)%>% print()
  
  print("*****MCV1********")
  
  # MCV1
  p_mcv1 <- ggplot(dat %>% filter(vaccine=="MCV1"), aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 6),
          axis.title = element_text(size =8)) +
    scale_x_continuous(limits = c(from, 2020), breaks = seq(from, 2020, 2))+
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%)")+
    #facet_wrap(.~vaccine) + 
    geom_vline(xintercept = 2019, linetype = "dashed")+
    ggtitle("MCV1")
  
  
  did_mcv1 <- glm(coverage ~ year + prepost + cat +
                    prepost*cat +
                   wb_income+who_region,
                 data = dat %>% filter(vaccine=="MCV1") 
  )
  
  summary(did_mcv1)%>% print()
  
  print("*****MCV2********")
  
  # MCV2
  p_mcv2 <- ggplot(dat %>% filter(vaccine=="MCV2"), aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 6),
          axis.title = element_text(size =8)) +
    scale_x_continuous(limits = c(from, 2020), breaks = seq(from, 2020, 2))+
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%)")+
    #facet_wrap(.~vaccine) + 
    geom_vline(xintercept = 2019, linetype = "dashed")+
    ggtitle("MCV2")
  
  
  did_mcv2 <- glm(coverage ~ year + prepost + cat +
                    prepost*cat +
                   wb_income+who_region,
                 data = dat %>% filter(vaccine=="MCV2") 
  )
  
  summary(did_mcv2)%>% print()
  
  p_merged <- ggpubr::ggarrange(p,
                                ggpubr::ggarrange(p_bcg, p_dtp, p_mcv1, p_mcv2,
                                                  common.legend=T),
                                ncol = 1,
                                common.legend=T)
  print(p_merged)
  
}

gen_plot <- function(dat, cat){
  
  dat$cat <- dat[,cat]
  
  p_all <- ggplot(dat, aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    facet_wrap(.~vaccine) + 
    geom_vline(xintercept = 2019, linetype = "dashed")+
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%, Range 0-100)") +
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 8))
  
  p_tot <- ggplot(dat, aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    geom_vline(xintercept = 2019, linetype = "dashed")+
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%, Range 0-100)") +
    ggtitle("Overall")+
    theme(legend.title = element_blank())
  
  p_comb <- ggpubr::ggarrange(
    p_tot, p_all, 
    labels = c("(A)", "(B)"),
    ncol = 1, 
    common.legend=T,
    legend = "bottom")
  
  return(p_comb)
}


gen_fig1 <- function(dat, cat){
  
  dat$cat <- dat[,cat]
  
  p_all <- ggplot(dat, aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    facet_wrap(.~vaccine) + 
    geom_vline(xintercept = 2019, linetype = "dashed")+
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%, Range 0-100)") +
    scale_color_discrete(labels = c('UHC Index >=80' = expression(paste('UHC SCI ', "">=80)),
                                    'UHC Index <80' = expression(paste('UHC SCI ', ""<80))))+
    
    theme(legend.title = element_blank(),
          axis.text = element_text(size = 8))
  
  p_tot <- ggplot(dat, aes(year, coverage)) + 
    stat_summary(geom = "line", fun.y=mean, aes(color = cat)) +
    geom_vline(xintercept = 2019, linetype = "dashed")+
    scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, 20))+
    xlab("Year") +
    ylab("Coverage (%, Range 0-100)") +
    scale_color_discrete(labels = c('UHC Index >=80' = expression(paste('UHC SCI ', "">=80)),
                                    'UHC Index <80' = expression(paste('UHC SCI ', ""<80))))+
    ggtitle("Overall")+
    theme(legend.title = element_blank())
  
  p_comb <- ggpubr::ggarrange(
    p_tot, p_all, 
    labels = c("(A)", "(B)"),
    ncol = 1, 
    common.legend=T,
    legend = "bottom")
  
  return(p_comb)
}

add_ci <- function(dat){
  coef <- dat$coefficients
  ci <- sprintf("(%.2f, %.2f)",
              coef[,1] - 1.96*coef[,2],
              coef[,1] + 1.96*coef[,2])
  res <- data.frame(beta = coef[,1] %>% round(2), 
             ci = ci,
             pval = coef[,4] %>% round(3))
  return(res)
}
