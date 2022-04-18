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