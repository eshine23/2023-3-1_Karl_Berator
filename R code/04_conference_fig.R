# load libs
pacman::p_load(readxl,dplyr,tidyverse,tidygraph,patchwork,
               ggrepel,sysfonts,showtext,latex2exp)


#load in clean data ----
karl_data <- read.csv("data/Karl_data_clean_2023-3-1.csv") %>%
  mutate(across(where(is.character), factor))




# add times new roman font
font_add(
  family = "times",
  regular = here::here("Figures",
    "fonts","times.ttf"
  ),
  bold = here::here(
    "fonts","timesbd.ttf"
  )
)


#create graph
karl_wild_cell <- karl_data %>%
  filter(cell_line=="Wild_type")


karl_101_cell <- karl_data %>%
  filter(cell_line=="Cell_type_101")


showtext_auto()

p_1 <- ggplot(karl_wild_cell,aes(concentration,gene_expression)) +
  geom_point(colour="black",shape=21, stroke = 0.6, size = 3, aes(fill=treatment))+
  theme_bw()+ theme(text = element_text(family = "times",size=40)) +
  scale_fill_manual("Treatment",labels = c("Activating factor 42", "Placebo"),values = c("#78a8d1", "#d5bf98")) +
  labs(title = "Wild-type", tag = "A",x=TeX("$\\mu$g/ml"), y="Gene Expression") +
  scale_x_continuous(breaks = seq(0, 10, by = 1),minor_breaks = seq(0, 12, 0.5)) +
  scale_y_continuous(breaks = seq(0, 40, by = 10),minor_breaks = seq(0, 40, 5)) +
  geom_label_repel(data = subset(karl_wild_cell, concentration==10),aes(label=gene_line,fill=treatment),
                   size = 15,family="times",nudge_y = 1.75,nudge_x = 1.5,show.legend = FALSE)


p_2 <- ggplot(karl_101_cell,aes(concentration,gene_expression)) +
  geom_point(colour="black",shape=21, size = 3, stroke = 0.6, aes(fill=treatment))+
  scale_fill_manual("Treatment",labels = c("Activating factor 42", "Placebo"),values = c("#78a8d1", "#d5bf98")) +
  theme_bw()+ theme(text = element_text(family = "times",size=40)) +
  scale_x_continuous(breaks = seq(0, 10, by = 1),minor_breaks = seq(0, 12, 0.5)) +
  scale_y_continuous(breaks = seq(0, 50, by = 10),minor_breaks = seq(0, 50, 5)) +
  labs(title = "Cell-type 101",tag = "B",x=TeX("$\\mu$g/ml"),y="Gene Expression") +
  geom_label_repel(data = subset(karl_101_cell, concentration==10),aes(label=gene_line,fill=treatment),size = 15,nudge_y = 1.75,nudge_x = 1.5,
                   family="times",show.legend = FALSE)


plot <- p_1+ p_2 + plot_layout(guides = "collect") & theme(legend.position="bottom",legend.spacing.x = unit(0.3, 'cm'))

plot

ggsave(filename=here::here("Figures/conference_plot_2023-4-3.tiff"),
       plot = plot,
       width = 9,
       height =6,
       units = c("in")
)


showtext_auto(FALSE)



# fill=treatment
harrypotter::scale_fill_hp("Ravenclaw", discrete = TRUE)