#this code cleans the gene expression data provided in march 2023 by removing missing
# data

pacman::p_load(readxl,dplyr,tidyverse,skimr)

#load in raw data
karl_data <- read_excel(here::here("Raw data/Karl_data_2023-3-1.xlsx"))


#set cell type, treatment and gene_line as factors
karl_data <- karl_data %>%
  mutate(across(where(is.character), factor))


#perform EDA on data --------------
#EDA
skimr::skim_without_charts(karl_data)

#check distribution of outcome variable
karl_data %>% ggplot(aes(gene_expression)) + geom_histogram()

# outlier was raised with karl, is a missing obseravtion so is removed 

# remove missing observation 
karl_data <- karl_data %>% filter(gene_expression>0)

#save clean data 
write.csv(karl_data,here::here("data/Karl_data_clean_2023-3-1.csv"),row.names=FALSE)

