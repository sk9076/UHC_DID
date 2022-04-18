### you can SKIP script #1 and #2 if you use below line to load the merged data
#dat_merged <- readRDS(here::here("data", "data_merged.rds"))

# Table S1-2
reg_pre <- lm(coverage ~ year + year*uhc_cat5, data = dat_merged %>% filter(prepost==0))
gtsummary::tbl_regression(reg_pre,
                          intercept = T,
                          estimate_fun = purrr::partial(style_ratio, digits = 2),
                          pvalue_fun = purrr::partial(style_sigfig, digits = 2))

# Table S2-1
lm(coverage ~ uhc_cat5 + prepost + year, dat_merged %>% filter(year>=2010)) %>% 
  gtsummary::tbl_regression(intercept = T,
                            estimate_fun = purrr::partial(style_ratio, digits = 2),
                            pvalue_fun = purrr::partial(style_sigfig, digits = 2))

lm(coverage ~ uhc_cat5 + prepost + year +
     wb_income+who_region +
     vaccine+ ghsi, dat_merged) %>% 
  gtsummary::tbl_regression(intercept = T,
                            estimate_fun = purrr::partial(style_ratio, digits = 2),
                            pvalue_fun = purrr::partial(style_sigfig, digits = 2))

# Tables S2-2 ~ 14
vaccines <- c("All", dat_merged$vaccine %>% unique())
res_list <- list()
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
  

  res <- add_ci(base_temp1) %>% add_row()
  res %<>% cbind(add_ci(did_temp1),
                 add_ci(base_temp2) %>% add_row(),
                 add_ci(did_temp2))
  
  res_list[[vac]] <- res 
}

# Save
wb <- createWorkbook()
header <- c(NA, 
            "Base model with UHC SCI cutoff >=80 (Adjusted)",
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
             here::here("results", "regression_tabs_20220418.xlsx")
            )  

# Adding unadjusted base model to the tables in S2
res_list_unadj <- list()
for(vac in vaccines){
  if(vac=="All"){
    base_temp1 <- glm(coverage ~ year + 
                        prepost + uhc_cat5,
                      data = dat_merged %>% filter(year >=2010)) %>% summary()
    did_temp1 <- glm(coverage ~ year + 
                       prepost + uhc_cat5 + prepost*uhc_cat5,
                     data = dat_merged %>% filter(year >=2010)) %>% summary()
    
    base_temp2 <-  glm(coverage ~ year + 
                         prepost + uhc_cat6,
                       data = dat_merged %>% filter(year >=2010)) %>% summary()
    
    did_temp2 <- glm(coverage ~ year + 
                       prepost + uhc_cat6 + prepost*uhc_cat6,
                     data = dat_merged %>% filter(year >=2010)) %>% summary()
  }else{
    base_temp1 <- glm(coverage ~ year + 
                        prepost + uhc_cat5,
                      data = dat_merged %>% filter(vaccine==vac, year >=2010)) %>% summary()
    
    did_temp1 <- glm(coverage ~ year + 
                       prepost + uhc_cat5 + prepost*uhc_cat5,
                     data = dat_merged %>% filter(vaccine==vac, year >=2010)) %>% summary()
    
    base_temp2 <-  glm(coverage ~ year  +
                         prepost + uhc_cat6,
                       data = dat_merged %>% filter(vaccine==vac, year >=2010)) %>% summary()
    did_temp1 <- glm(coverage ~ year + 
                       prepost + uhc_cat6 + prepost*uhc_cat6,
                     data = dat_merged %>% filter(vaccine==vac, year >=2010)) %>% summary()
  }
  
  res <- add_ci(base_temp1) %>% add_row()
  res %<>% cbind(add_ci(did_temp1),
                 add_ci(base_temp2) %>% add_row(),
                 add_ci(did_temp2))
  
  res_list_unadj[[vac]] <- res 
}
wb <- createWorkbook()
header <- c(NA, 
            "Unadjusted Base model with UHC SCI cutoff >=80",
            NA,NA,
            "Unadjusted DiD model with UHC SCI cutoff >=80",
            NA,NA,
            "Unadjusted Base model with UHC SCI cutoff <50",
            NA,NA,
            "Unadjusted DiD model with UHC SCI cutoff >=50"
            )%>% t()

for(i in 1:length(res_list_unadj)){
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
            res_list_unadj[[vaccines[i]]], 
            rowNames=T)
}
saveWorkbook(overwrite = T,
             wb, 
             here::here("results", "regression_tabs_unadjusted.xlsx")
)  
