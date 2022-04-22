## These are additional codes to respond to reviewers' comments

# Table S1-2 and 3
tab_country <- dat_merged[!duplicated(dat_merged$country),]
# categorizaton based on UHC index cut-off 80
table1::table1(~ uhc_2019 + factor(wb_income) + factor(who_region) + ghsi|uhc_cat5,
               data = tab_country)
# categorizaton based on UHC index cut-off 50
table1::table1(~ uhc_2019 + factor(wb_income) + factor(who_region) + ghsi|uhc_cat6,
               data = tab_country)

# DiD analysis using the time period 2015 - 2020
## Threshold 80
base_u <-
  glm(coverage ~ prepost + uhc_cat5 ,
      data = dat_merged %>% filter(year >=2015)) %>% summary()

did_u <-
  glm(coverage ~ 
      prepost + uhc_cat5 +
      prepost*uhc_cat5 ,
    data = dat_merged %>% filter(year >=2015)) %>% summary()

base_a <-
  glm(coverage ~ year + 
        wb_income+who_region +
        prepost + uhc_cat5 + vaccine+ ghsi,
      data = dat_merged %>% filter(year >=2015)) %>% summary()

did_a <-
  glm(coverage ~ year + 
        wb_income+who_region +
        prepost + uhc_cat5 + vaccine+ ghsi+
        prepost*uhc_cat5 ,
      data = dat_merged %>% filter(year >=2015)) %>% summary()

## Threshold 50
base_u2 <-
  glm(coverage ~ prepost + uhc_cat6 ,
      data = dat_merged %>% filter(year >=2015)) %>% summary()

did_u2 <-
  glm(coverage ~ 
        prepost + uhc_cat6 +
        prepost*uhc_cat6 ,
      data = dat_merged %>% filter(year >=2015)) %>% summary()

base_a2 <-
  glm(coverage ~ year + 
        wb_income+who_region +
        prepost + uhc_cat6 + vaccine+ ghsi,
      data = dat_merged %>% filter(year >=2015)) %>% summary()

did_a2 <-
  glm(coverage ~ year + 
        wb_income+who_region +
        prepost + uhc_cat6 + vaccine+ ghsi+
        prepost*uhc_cat6 ,
      data = dat_merged %>% filter(year >=2015)) %>% summary()

res_u <- cbind(add_ci(base_u)%>%add_row(),
             add_ci(did_u))
res_a <- cbind(add_ci(base_a)%>%add_row(),
               add_ci(did_a))
res_u2 <- cbind(add_ci(base_u2)%>%add_row(),
               add_ci(did_u2))
res_a2 <- cbind(add_ci(base_a2)%>%add_row(),
               add_ci(did_a2))

## Save
# Save
wb <- createWorkbook()
header1 <- c(NA, 
           "Base model with UHC SCI cutoff >=80",
            NA, NA,
           "DID model with UHC SCI cutoff >=80")%>% t()
header2 <- c(NA, 
             "Base model with UHC SCI cutoff <50",
             NA, NA,
             "DID model with UHC SCI cutoff <50")%>% t()

addWorksheet(
    wb,
    sheetName = "unadj_80",
)
writeData(wb,
          sheet = "unadj_80",
          startRow = 1,
          header1, 
          colNames=F)
writeData(wb, 
          sheet = "unadj_80",
          startRow = 2,
          res_u, 
          rowNames=T)

addWorksheet(
  wb,
  sheetName = "adj_80",
)
writeData(wb,
          sheet = "adj_80",
          startRow = 1,
          header1, 
          colNames=F)
writeData(wb, 
          sheet = "adj_80",
          startRow = 2,
          res_a, 
          rowNames=T)

addWorksheet(
  wb,
  sheetName = "unadj_50",
)
writeData(wb,
          sheet = "unadj_50",
          startRow = 1,
          header2, 
          colNames=F)
writeData(wb, 
          sheet = "unadj_50",
          startRow = 2,
          res_u2, 
          rowNames=T)

addWorksheet(
  wb,
  sheetName = "adj_50",
)
writeData(wb,
          sheet = "adj_50",
          startRow = 1,
          header2, 
          colNames=F)
writeData(wb, 
          sheet = "adj_50",
          startRow = 2,
          res_a2, 
          rowNames=T)

saveWorkbook(overwrite = T,
             wb, 
             here::here("results", "DID_2015_2020_20220418.xlsx")
)  
