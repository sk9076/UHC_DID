# UHC box plot

# load packages
pacman::p_load(tidyverse, dplyr, ggplot2, magrittr, ggthemr)

# set the data directory
path <- here::here("references", "SDG_3_data")

# load uhc data
uhc <- rio::import(paste0(path, "/6_uhc.CSV")) %>%
  filter(indicator_name=="UHC effective coverage index", year_id==2019)

# mark the countries of our interest
uhc %<>% mutate(
  vietnam = ifelse(location_name=="Viet Nam", 1, 0),
  rwanda = ifelse(location_name=="Rwanda", 1, 0),
  peru = ifelse(location_name=="Peru", 1, 0),
  cuba = ifelse(location_name=="Cuba", 1, 0),
  nigeria = ifelse(location_name=="Nigeria", 1, 0),
  oman = ifelse(location_name=="Oman", 1, 0)
)

ggthemr("fresh")

# Viet nam
plot <- 
  ggplot(uhc, aes(y=val)) + geom_boxplot(fill="grey", alpha = 0.5) +
  geom_point(aes(runif(nrow(uhc), -0.1, 0.1), y=val), shape=1) +
  geom_point(data=uhc %>% filter(vietnam==1), 
             aes(x=0, y=val, color = "Viet Nam"),
             size = 10,
             shape = 18
             ) +
  scale_color_manual(values = c("Viet Nam" = "#0981CF"),
                     labels = c(sprintf("Viet Nam (UHC effective coverage score = %.1f)", uhc$val[which(uhc$vietnam==1)]))) +
  xlab("")+
  ylab("UHC Effective Coverage Score (0-100)") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10)) +
  scale_x_continuous(labels = rep("", 5))+
  theme(legend.title = element_blank(),
        legend.position = c(0.45, -0.07),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold", hjust=0)) +
  ggtitle("Progress Towards Universal Health Coverage (UHC), 2019")+
  align_title_left()

save_plot_sep(plot,
              paste0("Viet_Nam", "_uhc_2019.png"),
              width = 5,
              height = 4.7)

# Oman
plot <- 
  ggplot(uhc, aes(y=val)) + geom_boxplot(fill="grey", alpha = 0.5) +
  geom_point(aes(runif(nrow(uhc), -0.1, 0.1), y=val), shape=1) +
  geom_point(data=uhc %>% filter(oman==1), 
             aes(x=0, y=val, color = "Oman"),
             size = 10,
             shape = 18
  ) +
  scale_color_manual(values = c("Oman" = "#0981CF"),
                     labels = c(sprintf("Oman (UHC effective coverage score = %.1f)", uhc$val[which(uhc$oman==1)]))) +
  xlab("")+
  ylab("UHC Effective Coverage Score (0-100)") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10)) +
  scale_x_continuous(labels = rep("", 5))+
  theme(legend.title = element_blank(),
        legend.position = c(0.45, -0.07),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold", hjust=0)) +
  ggtitle("Progress Towards Universal Health Coverage (UHC), 2019")+
  align_title_left()

save_plot_sep(plot,
              paste0("Oman", "_uhc_2019.png"),
              width = 5,
              height = 4.7)
# Rwanda
plot <- 
  ggplot(uhc, aes(y=val)) + geom_boxplot(fill="grey", alpha = 0.5) +
  geom_point(aes(runif(nrow(uhc), -0.1, 0.1), y=val), shape=1) +
  geom_point(data=uhc %>% filter(rwanda==1), 
             aes(x=0, y=val, color = "Rwanda"),
             size = 10,
             shape = 18
  ) +
  scale_color_manual(values = c("Rwanda" = "#0981CF"),
                     labels = c(sprintf("Rwanda (UHC effective coverage score = %.1f)", uhc$val[which(uhc$rwanda==1)]))) +
  xlab("")+
  ylab("UHC Effective Coverage Score (0-100)") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10)) +
  scale_x_continuous(labels = rep("", 5))+
  theme(legend.title = element_blank(),
        legend.position = c(0.45, -0.07),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold", hjust=0)) +
  ggtitle("Progress Towards Universal Health Coverage (UHC), 2019")+
  align_title_left()

save_plot_sep(plot,
              paste0("Rwanda", "_uhc_2019.png"),
              width = 5,
              height = 4.7)

# Nigeria
plot <- 
  ggplot(uhc, aes(y=val)) + geom_boxplot(fill="grey", alpha = 0.5) +
  geom_point(aes(runif(nrow(uhc), -0.1, 0.1), y=val), shape=1) +
  geom_point(data=uhc %>% filter(nigeria==1), 
             aes(x=0, y=val, color = "Nigeria"),
             size = 10,
             shape = 18
  ) +
  scale_color_manual(values = c("Nigeria" = "#0981CF"),
                     labels = c(sprintf("Nigeria (UHC effective coverage score = %.1f)", uhc$val[which(uhc$nigeria==1)]))) +
  xlab("")+
  ylab("UHC Effective Coverage Score (0-100)") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10)) +
  scale_x_continuous(labels = rep("", 5))+
  theme(legend.title = element_blank(),
        legend.position = c(0.45, -0.07),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold", hjust=0)) +
  ggtitle("Progress Towards Universal Health Coverage (UHC), 2019")+
  align_title_left()

save_plot_sep(plot,
              paste0("Nigeria", "_uhc_2019.png"),
              width = 5,
              height = 4.7)

# Cuba
plot <- 
  ggplot(uhc, aes(y=val)) + geom_boxplot(fill="grey", alpha = 0.5) +
  geom_point(aes(runif(nrow(uhc), -0.1, 0.1), y=val), shape=1) +
  geom_point(data=uhc %>% filter(cuba==1), 
             aes(x=0, y=val, color = "Cuba"),
             size = 10,
             shape = 18
  ) +
  scale_color_manual(values = c("Cuba" = "#0981CF"),
                     labels = c(sprintf("Cuba (UHC effective coverage score = %.1f)", uhc$val[which(uhc$cuba==1)]))) +
  xlab("")+
  ylab("UHC Effective Coverage Score (0-100)") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10)) +
  scale_x_continuous(labels = rep("", 5))+
  theme(legend.title = element_blank(),
        legend.position = c(0.45, -0.07),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold", hjust=0)) +
  ggtitle("Progress Towards Universal Health Coverage (UHC), 2019")+
  align_title_left()

save_plot_sep(plot,
              paste0("Cuba", "_uhc_2019.png"),
              width = 5,
              height = 4.7)

# Peru
# Viet nam
plot <- 
  ggplot(uhc, aes(y=val)) + geom_boxplot(fill="grey", alpha = 0.5) +
  geom_point(aes(runif(nrow(uhc), -0.1, 0.1), y=val), shape=1) +
  geom_point(data=uhc %>% filter(peru==1), 
             aes(x=0, y=val, color = "Peru"),
             size = 10,
             shape = 18
  ) +
  scale_color_manual(values = c("Peru" = "#0981CF"),
                     labels = c(sprintf("Peru (UHC effective coverage score = %.1f)", uhc$val[which(uhc$peru==1)]))) +
  xlab("")+
  ylab("UHC Effective Coverage Score (0-100)") +
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 10)) +
  scale_x_continuous(labels = rep("", 5))+
  theme(legend.title = element_blank(),
        legend.position = c(0.45, -0.07),
        legend.text = element_text(size = 9),
        plot.title = element_text(size = 10, face = "bold", hjust=0)) +
  ggtitle("Progress Towards Universal Health Coverage (UHC), 2019")+
  align_title_left()

save_plot_sep(plot,
              paste0("Peru", "_uhc_2019.png"),
              width = 5,
              height = 4.7)
