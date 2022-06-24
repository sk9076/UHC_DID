### you can SKIP script #1 and #2 if you use below line to load the merged data
#dat_merged <- readRDS(here::here("data", "data_merged.rds"))

# visual inspection & Figure 1 and 2

fig1<-gen_fig1(dat_merged, "uhc_cat5")
fig2<-gen_plot(dat_merged, "uhc_cat6")

abbreviations <- 
  paste0(
  c(
    "Abbreviations: BCG = Bacille Calmette-Guérin",
  "DTP1 = diphtheria, tetanus toxoid, and pertussis containing vaccine – first dose",
  "DTP3 = diphtheria, tetanus toxoid, and pertussis containing vaccine – third dose",
  "HEPB3 = third dose; HEPBB=hepatitis B vaccine – birth dose",
  "HIB3 = Haemophilus influenzae type B containing vaccine; MCV1=measles containing vaccine – first dose",
  "MCV2 = measles containing vaccine – third dose; PCV3=pneumococcal conjugate vaccine – third dose",
  "POL3 = polio containing vaccine – third dose",
  "RCV1 = rubella containing vaccine – first dose",
  "ROTAC = rotavirus vaccine – second or third dose",
  "UHC SCI = UHC Service Coverage Index"),
  collapse = "; ")

annotate_figure(fig1,
                bottom = text_grob( abbreviations,
                                    just = "right",
                                    hjust = NULL,
                                    vjust = NULL,
                                    rot = 0,
                                    color = "black",
                                    face = "plain",
                                    size = 8,
                                    lineheight = NULL))

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
