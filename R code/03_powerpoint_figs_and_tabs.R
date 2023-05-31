

pacman::p_load(readxl,dplyr,tidyverse,gt)


#load in clean data ----
karl_data <- read.csv("data/Karl_data_clean_2023-3-1.csv") %>%
    mutate(across(where(is.character), factor))


# create plot of gene expression vs concentration for the wild cell line (slide 2) ----
karl_wild_cell <- karl_data%>%
  filter(cell_line=="Wild_type")

ggplot(karl_wild_cell,aes(concentration,gene_expression,colour=treatment)) +
  geom_point() + geom_smooth(method="lm") +  theme_bw()+
  scale_color_manual("Treatment (50mg)",labels = c("Factor 42", "Saline (Placebo)"), values = c("#56B4E9", "#E69F00"))+
  labs(title = "Graph 1: Gene expression of Wild cell line for both treatments" ,x="Concentration of Growth factor (mg/ml)",
       y="Gene  Expression",caption="The gene expression for the Wild cell line for varying concentrations of growth factor and 50mg of treatment. \n The Factor 42 treatment increased the gene expression for all concentrations.")


ggsave("wild_cell_2023-3-1.jpg",path=here::here("Figures"),width=23,height=16,units="cm")


# create plot of gene expression vs concentration for the 101 cell line (slide 3) ----
karl_101_cell <- karl_data %>%
  filter(cell_line=="Cell_type_101")

ggplot(karl_101_cell,aes(concentration,gene_expression,colour=treatment)) +
  geom_point() + geom_smooth(method="lm") +  theme_bw()+
  scale_color_manual('Treatment (50mg)',labels = c("Factor 42", "Saline (Placebo)"), values = c("#CC79A7", "#009E73"))+
  labs(title = "Graph 2 : Gene Expression of 101 cell line for both treatments ",x="Concentration of growth factor (mg/ml)",
       y="Gene  Expression",caption="The gene expression for the 101 cell line for varying concentrations of growth factor and 50mg of treatment. \n The Factor 42 treatment increased the gene expression for all concentrations.")

ggsave("101_cell_2023-3-1.jpg",path=here::here("Figures"),width=23,height=16,units="cm")



# create tables containing sample mean and sample standard deviation for all cell line and treatment combinations------

# mean ----
# get mean for each cell line, treatment and concentration
samp_mean <- aggregate(gene_expression~cell_line+treatment+concentration, mean, data=karl_data) %>%
  dplyr::rename(sample_mean=gene_expression) %>%
  dplyr::mutate(cell_line = dplyr::recode(cell_line, "Cell_type_101"="101","Wild_type"="Wild "),
                treatment = dplyr::recode(treatment, "Factor_42"="Factor 42","Placebo"="Saline (Placebo)"))

# pivot into format where concentrations form the rows
sample_mean_table <- pivot_wider(samp_mean, names_from=concentration, values_from = c(sample_mean))

#round to 3 decimal places
sample_mean_table <- sample_mean_table %>%
  mutate_if(is.numeric,round,digits=3)


mean_tab <- sample_mean_table |>
  gt(caption="The sample mean for each cell line with 50mg of treatment and varying concentrations of growth factor. \n The sample mean at all concentrations was lower for the saline treatment than the Factor 42 Treatment.")|>
  tab_spanner(
    label = md('**Concentration of Growth Factor (mg/ml)**'),
    columns = 3:13
  ) |>
  cols_label(cell_line = "Cell line", treatment = "Treatment (50mg)") |>
  tab_header(
    title ="The sample mean of gene expression for each cell line and treatment" )


gtsave(mean_tab, "sample_mean_tab_2023-3-1.png",path=here::here("Tables"))


#sd----
sample_sd <- aggregate(gene_expression~cell_line+treatment+concentration, sd, data=karl_data) %>%
  dplyr::rename(sample_sd=gene_expression)%>%
  dplyr::mutate(cell_line = dplyr::recode(cell_line, "Cell_type_101"="101","Wild_type"="Wild "),
                treatment = dplyr::recode(treatment, "Factor_42"="Factor 42","Placebo"="Saline (Placebo)"))
# pivot into format where concentrations form the rows
sample_sd_table <- pivot_wider(sample_sd, names_from=concentration, values_from = c(sample_sd))

# round digits so table looks good
sample_sd_table <- sample_sd_table %>%
  mutate_if(is.numeric,round,digits=3)

# create a table
sd_tab <- sample_sd_table |>
  gt(caption="The sample standard deviation for each cell line with 50mg of treatment at varying concentrations of growth factor. \n The sample standard deviation is higher for the factor 42 treatment than the saline treatment.\n We can not calculate a sample SD for the wild cell line treated with factor 42 and a growth factor concentration of 5mg/ml as there we only have a single observation. ")|>
  tab_spanner(
    label = md('**Concentration of Growth Factor (mg/ml)**'),
    columns = 3:13
  ) |>
  cols_label(cell_line = "Cell line", treatment = "Treatment (50mg)") |>
  tab_header(
    title ="The sample standard deviation of gene expression for each cell line and treatment" )

gtsave(sd_tab, "sample_sd_tab_2023-3-1.png",path=here::here("Tables"))







