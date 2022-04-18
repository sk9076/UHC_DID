# Table S1-2 and 3
tab_country <- dat_merged[!duplicated(dat_merged$country),]
# categorizaton based on UHC index cut-off 80
table1::table1(~ uhc_2019 + factor(wb_income) + factor(who_region) + ghsi|uhc_cat5,
               data = tab_country)
# categorizaton based on UHC index cut-off 50
table1::table1(~ uhc_2019 + factor(wb_income) + factor(who_region) + ghsi|uhc_cat6,
               data = tab_country)