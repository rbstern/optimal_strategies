library(magrittr)
library(randomForest)
library(tidyverse)

#############
# Read data #
#############

data <- read_rds("./data/data_final.rds")
control_vars = data$control_vars
response_vars = data$response_vars
covars_ant = data$covars_ant
covars_post = data$covars_post
data = data$data

##########################################
# Data cleaning for use in random forest #
##########################################

# 1. Remove data without response variable
data %<>% filter(!is.na(utility))

# 2. Main response is utility
response <- data$utility

# 3. Covariates include:
# - covars_ant: covariates available before first legal decision
# - control_variables: causal variables of interest
covariates <- data %>% select(c(control_vars, covars_ant))

# Variables which are not used as covariates
setdiff(names(data), names(covariates))

# 4. Add moral_dmg and patrimonial_dmg as factor covariates.
#    Remove tipo_dano
covariates %<>% 
  mutate(moral_dmg = ifelse(stringr::str_detect(tipo_dano, "dano moral"), "sim", "não"),
         patrimonial_dmg = ifelse(stringr::str_detect(tipo_dano, "dano material"), "sim", "não")) %>% 
  select(-tipo_dano) %>% 
  mutate_if(is.character, as.factor)

# 5. Remove requested amount of money
covariates %<>% select(-valor_acao)

# 6. Remove missing covariates
response <- response[complete.cases(covariates)]
covariates <- covariates[complete.cases(covariates), ]

#####################
# Fit random forest #
#####################
set.seed(1)
data_split <- sample(c("Train", "Validation"),
                     size = nrow(covariates),
                     prob = c(0.8, 0.2),
                     replace = TRUE)

fitRF <- randomForest(x = as.data.frame(covariates[data_split == "Train",]),
                      y = response[data_split == "Train"])
varImpPlot(fitRF)

pred <- predict(fitRF, as.data.frame(covariates[data_split == "Validation", ]))
plot(pred, response[data_split=="Validation"], cex=0.4)
abline(a=0,b=1)

plot(pred, response[data_split=="Validation"], xlim=c(0,100), ylim=c(0,100), cex=0.4)
abline(a=0,b=1)
