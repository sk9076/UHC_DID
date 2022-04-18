pacman::p_load(tidyverse, ggplot2, dplyr, magrittr, ggthemr, linelist, openxlsx,latex2exp)
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
  
# maybe filter only the vaccines that are considered essential? 

# visual inspection & Figure 1 and 2

fig1<-gen_fig1(dat_merged, "uhc_cat5")
fig2<-gen_plot(dat_merged, "uhc_cat6")

ggsave(here::here("results", "figure_1.png"),
       fig1,
       width = 8,
       height = 9,
       dpi=100)

ggsave(here::here("results", "figure_s1_1.png"),
       fig2,
       width = 8,
       height = 9,
       dpi=100)


#do_did(dat_merged, "uhc_cat5")
#do_did(dat_merged, "uhc_cat6")
#do_did(dat_merged, "uhc_cat7")

#do_did(dat_merged, "uhc_cat5", from = 2010)
#do_did(dat_merged, "uhc_cat6", from = 2010)
#do_did(dat_merged, "uhc_cat7", from = 2010)

vaccines <- c("All", dat_merged$vaccine %>% unique())
res_list <- list()


# For supplementary material

reg_pre <- lm(coverage ~ year + year*uhc_cat5, data = dat_merged %>% filter(prepost==0))
gtsummary::tbl_regression(reg_pre)

for(vac in vaccines){
  if(vac=="All"){
    base_temp1 <- glm(coverage ~ year + 
                        wb_income+who_region +
                        prepost + uhc_cat5 + vaccine + ghsi,
                      data = dat_merged %>% filter(year >=2010)) %>% summary()
    
    base_temp2 <-  glm(coverage ~ year  +
                         wb_income+who_region+ 
                         prepost + uhc_cat6 + vaccine+ ghsi,
                       data = dat_merged %>% filter(year >=2010)) %>% summary()
    
    did_temp1 <- glm(coverage ~ year + 
                       wb_income+who_region +
                       prepost + uhc_cat5 + vaccine+ ghsi+
                       prepost*uhc_cat5 ,
                     data = dat_merged %>% filter(year >=2010)) %>% summary()
    
    did_temp2 <-  glm(coverage ~ year  +
                        wb_income+who_region+ 
                        prepost + uhc_cat6 + vaccine+ ghsi+
                        prepost*uhc_cat6,
                      data = dat_merged %>% filter(year >=2010)) %>% summary()
  }else{
  base_temp1 <- glm(coverage ~ year + 
                     wb_income+who_region + ghsi+
                     prepost + uhc_cat5,
                   data = dat_merged %>% filter(vaccine==vac, year >=2010)) %>% summary()
  
  base_temp2 <-  glm(coverage ~ year  +
                      wb_income+who_region+  ghsi+
                      prepost + uhc_cat6,
                    data = dat_merged %>% filter(vaccine==vac, year >=2010)) %>% summary()
  
  did_temp1 <- glm(coverage ~ year + 
                     wb_income+who_region + ghsi+
                     prepost + uhc_cat5 +
                     prepost*uhc_cat5,
                   data = dat_merged %>% filter(vaccine==vac, year >=2010)) %>% summary()
  
  did_temp2 <-  glm(coverage ~ year  +
                      wb_income+who_region+  ghsi+
                      prepost + uhc_cat6 +
                      prepost*uhc_cat6,
                    data = dat_merged %>% filter(vaccine==vac, year >=2010)) %>% summary()
  }
  res <- base_temp1$coefficients[,c(1,2,4)] %>% data.frame() %>% add_row()
  res %<>% cbind(did_temp1$coefficients[,c(1,2,4)] %>% data.frame(),
                 base_temp2$coefficients[,c(1,2,4)] %>% data.frame() %>% add_row(),
                 did_temp2$coefficients[,c(1,2,4)] %>% data.frame())
  
  res_list[[vac]] <- res %>% round(digits=2)
}

# Save
wb <- createWorkbook()
header <- c(NA, 
            "Base model with UHC SCI cutoff >=80",
            NA, NA,
            "DiD model with UHC SCI cutoff >=80",
            NA, NA,
            "Base model with UHC SCI cutoff <50",
            NA, NA,
            "DiD model with UHC SCI cutoff <50")%>% t()

for(i in 1:length(res_list)){
  addWorksheet(
    wb,
    sheetName = vaccines[i],
    
  )
  writeData(wb,
            sheet = vaccines[i],
            startRow = 1,
            header, 
            colNames=F)
  
  writeData(wb, 
            sheet = vaccines[i],
            startRow = 2,
            res_list[[vaccines[i]]], 
            rowNames=T)
}
saveWorkbook(overwrite = T,
             wb, 
             here::here("results", "regression_tabs_v2.xlsx")
            )  


