pacman::p_load(ggplot2, dplyr, tidyverse)
ggthemr::ggthemr("fresh")

dat <- data.frame(
  did = c(-3.11, -3.11+2.7),
  low = c(-3.93, -3.93+0.75),
  high = c(-2.29, -2.29+4.65),
  group = c("Countries with \nUHC SCI 2019 <80", 
            "Countries with \nUHC SCI 2019 \u2265 80")
)

fig2 <- ggplot(dat,
       aes(x=group, y=did, fill = group)) + geom_bar(stat="identity") +
  geom_errorbar(aes(x=group, ymin = low, ymax=high, 
                    color = "95% confidence interval"),
                width = 0.3,
                size = 0.5) +
  geom_hline(yintercept = 0, color="black")+
  scale_color_manual(name = "",
                     values = "black")+
  scale_fill_discrete(guide = "none")+
  xlab("")+
  ylab("Drop in childhood immunization \ncoverage (%) during 2020") +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 14))

ggsave(filename = here::here("results", "figure2.png"),
       plot = fig2,
       width = 8,
       height = 5,
       dpi = 300)
