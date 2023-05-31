
pacman::p_load(readxl,dplyr,tidyverse,gglm)

#load in clean data ----
karl_data <- read.csv("data/Karl_data_clean_2023-3-1.csv") %>%
  mutate(across(where(is.character), factor))


# Fit MLR ----

# choose best linear regression model using step wise selection
#start with null model
null <- lm(gene_expression ~ 1, data = karl_data)
lm <- step(null, scope = gene_expression ~ (concentration + treatment + cell_line)^3, direction = "both")


#test assumptions
#doesn't pass normality test and doesn't have homoscadity
gglm::gglm(lm)

# transform data to fix normality 
# we will try log first as the aim is to interprete the relationship
# between gene expression, treatment and concentration and a box cox transformation
# would make this more challenging 


# Transform data (log)----

#check relationship between log of gene expression and each variable


# choose best linear regression model using step wise selection
# start with null model
null <- lm(log(gene_expression) ~ 1, data = karl_data)
lm_log <- step(null, scope = log(gene_expression) ~ (concentration + treatment + cell_line)^3, direction = "both")



#test assumptions
#all looks good
gglm::gglm(lm_log)


## check res vs each predictor

#get student res for each obs
log_res <-
  broom::augment(lm_log) %>%
  #adds studentized residuals
  add_column(pear_res = residuals(lm_log, type = "pearson"),gene_line = karl_data$gene_line)

#check res vs fitted for catergorial
#looks good
log_res %>%
  dplyr::select(where(is.factor), pear_res) %>%
  pivot_longer(-pear_res) %>%
  ggplot(aes(value, pear_res)) +
  geom_point() +
  facet_wrap(~name, scale = "free")


#check res for concentration
#looks curved :( so we add squared term for concentration to fix this 
log_res %>%
  ggplot(aes(concentration, pear_res)) +
  geom_point() + geom_smooth()



# log transformation and sqred concentration term ----

# choose best linear regression model using step wise selection
#start with null model
null <- lm(log(gene_expression) ~ 1, data = karl_data)
lm_log_con2 <- step(null, scope = log(gene_expression) ~ (concentration + treatment + cell_line+I(concentration^2))^3 , direction = "both")

#test assumptions
#all looks good
gglm::gglm(lm_log_con2)


## check res vs each predictor

#get student res for each obs
log_con2_res <-
  broom::augment(lm_log_con2) %>%
  #adds studentized residuals
  add_column(pear_res = residuals(lm_log_con2, type = "pearson"),gene_line = karl_data$gene_line) #obs_num=karl_data_1$obs_num)



#check res vs fitted for catergorial
#looks good apart from gene_line 
log_con2_res %>%
  dplyr::select(where(is.factor), pear_res) %>%
  pivot_longer(-pear_res) %>%
  ggplot(aes(value, pear_res)) +
  geom_boxplot() +
  facet_wrap(~name, scale = "free")


#check res for concentration
#looks good
log_con2_res %>%
  ggplot(aes(concentration, pear_res)) +
  geom_point() + geom_smooth()


# im just assuming the error terms are independent as my life is too short
# to deal with that, and I don't know how to deal with it


# significance of treatment on gradient

# h0: concentration:treatment all equal zero
# ha: at least one treatment:concentration not zero

anova(lm_log_con2)










