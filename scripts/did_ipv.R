pacman::p_load(tidyverse, ggplot2, dplyr, magrittr, ggthemr, linelist)
ggthemr("fresh")

# load the UHC data
uhc <- rio::import(here::here("WHO_agg_data.xlsx")) %>% 
  clean_data() %>%
  mutate(
    uhc_cat = as.factor(ntile(uhc_2017, 4)),
    uhc_cat2 = ifelse(uhc_cat == "4", "UHC", "Not UHC")
  ) %>% 
  filter(uhc_cat!="1")

uhc %>% ggplot(aes(uhc_2017)) + geom_histogram()
summary(uhc$uhc_2017)

# load immunization data (BCG, DTP1, DTP3, MCV1, MCV2)
ipv_path <- here::here("coverage_estimates_series.xls")

bcg <- load_ipv(ipv_path, "BCG", uhc)
dtp1 <- load_ipv(ipv_path, "DTP1", uhc)
dtp3 <- load_ipv(ipv_path, "DTP3", uhc)
mcv1 <- load_ipv(ipv_path, "MCV1", uhc)
mcv2 <- load_ipv(ipv_path, "MCV2", uhc)

# make the long-format data 
bcg_long <- convert_long(bcg)
dtp1_long <- convert_long(dtp1)
dtp3_long <- convert_long(dtp3)
mcv1_long <- convert_long(mcv1)
mcv2_long <- convert_long(mcv2)

# plot
ggplot(bcg_long, aes(year, coverage, color = uhc_cat2, group = uhc_cat2)) + stat_summary(geom = "line", fun.y = mean) +
  geom_vline(xintercept = 2009, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") + ggtitle("BCG")

ggplot(dtp1_long, aes(year, coverage, color = uhc_cat2, group = uhc_cat2)) + stat_summary(geom = "line", fun.y = mean) +
  geom_vline(xintercept = 2009, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") + ggtitle ("DTP1")

ggplot(dtp3_long, aes(year, coverage, color = uhc_cat2, group = uhc_cat2)) + stat_summary(geom = "line", fun.y = mean) +
  geom_vline(xintercept = 2009, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") + ggtitle("DTP3")

ggplot(mcv1_long, aes(year, coverage, color = uhc_cat2, group = uhc_cat2)) + stat_summary(geom = "line", fun.y = mean) +
  geom_vline(xintercept = 2009, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") + ggtitle ("MCV1")

ggplot(mcv2_long, aes(year, coverage, color = uhc_cat2, group = uhc_cat2)) + stat_summary(geom = "line", fun.y = mean) +
  geom_vline(xintercept = 2009, linetype = "dashed") +
  geom_vline(xintercept = 2010, linetype = "dashed") + ggtitle ("MCV2")

# DiD
did_bcg <- lm(coverage ~ year + pre_post + uhc_cat_did +
                pre_post * uhc_cat_did+
                pre_post2 *uhc_cat_did,
              data = bcg_long 
)
summary(did_bcg)

did_dtp1 <- lm(coverage ~ year + pre_post + uhc_cat_did +
                pre_post * uhc_cat_did+
                pre_post2 *uhc_cat_did,
              data = dtp1_long 
)
summary(did_dtp1)

did_dtp3 <- lm(coverage ~ year + pre_post + uhc_cat_did +
                 pre_post * uhc_cat_did+
                 pre_post2 *uhc_cat_did,
               data = dtp3_long 
)
summary(did_dtp3)

did_mcv1 <- lm(coverage ~ year + pre_post + uhc_cat_did +
                 pre_post * uhc_cat_did+
                 pre_post2 *uhc_cat_did,
               data = mcv1_long 
)
summary(did_mcv1)

did_mcv2 <- lm(coverage ~ year + pre_post + uhc_cat_did +
                 pre_post * uhc_cat_did+
                 pre_post2 *uhc_cat_did,
               data = mcv2_long 
)
summary(did_mcv2)
