# sensitivity analysis with sliding cutoff scale
cutoffs <- seq(50, 80, by=5)
dat_sen <- dat_merged

res_sen <- data.frame(
  cutoff = cutoffs,
  beta = rep(NA, length(cutoffs)),
  ci_low =rep(NA, length(cutoffs)),
  ci_high =rep(NA, length(cutoffs)),
  pval = rep(NA, length(cutoffs))
)

for(cutoff in cutoffs){
 
  dat_sen %<>% mutate(
    temp_cat = ifelse(uhc_2019>=cutoff, 1, 0)
  )  
  did_temp <- glm(coverage ~ year + 
                     wb_income+who_region +
                     prepost + temp_cat + vaccine+ ghsi+
                     prepost*temp_cat ,
                   data = dat_sen %>% filter(year >=2010)) %>% summary()
  
  
  res <- did_temp$coefficients[which(rownames(did_temp$coefficients)=="prepost:temp_cat"),]

  res_sen[which(res_sen$cutoff == cutoff), c("beta", "ci_low", "ci_high", "pval")] <-
    c(res[1],
      res[1] - 1.96*res[2],
      res[1] + 1.96*res[2],
      res[4])
}

p_sen <- res_sen %>% ggplot() + 
  geom_errorbar(aes(cutoff, ymin = ci_low, ymax = ci_high, color = "95% CI")) +
  geom_point(aes(cutoff, beta, color = "DiD coefficient"), size=3) +
  scale_y_continuous(limits = c(-5, 10)) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dotted")+
  ylab("Difference-in-Difference Coefficient") +
  xlab("UHC SCI 2019 Cut-Off Value")+
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "top")

res_sen[,2:4] <- apply(res_sen[,2:4], 2, function(x) round(x, 3))
res_sen$pval %<>% round(4)

rio::export(res_sen, here::here("results", "sliding_cutoff_table.xlsx"))
ggsave(p_sen,
      filename= here::here("results", "sliding_cutoff.png"),
       units = "in",
       dpi = 300,
       width = 5,
       height = 4)
