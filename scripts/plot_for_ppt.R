pacman::p_load(ggplot2, dplyr, tidyverse)
ggthemr::ggthemr("fresh")

dat <- data.frame(
  did = c(-3.31, -3.31+2.93),
  low = c(-4.17, -4.17+0.78),
  high = c(-2.45, -2.45+5.08),
  group = c("Countries with \nUHC SCI 2019 <80", 
            "Countries with \nUHC SCI 2019 \u2265 80")
)

fig2 <- ggplot(dat,
       aes(x=group, y=did, fill = group)) + geom_bar(stat="identity") +
  geom_errorbar(aes(x=group, ymin = low, ymax=high),
                width = 0.3,
                color = "black",
                size = 0.5) +
  geom_hline(yintercept = 0, color="black")+
  xlab("")+
  ylab("Drop in childhood immunization coverage (%) \nduring the COVID-19 pandemic") +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14))

ggsave(filename = "/Users/sookim/Documents/Projects/UHC_COVID/results/figure2.png",
       plot = fig2,
       width = 8,
       height = 5,
       dpi = 300)
