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

####################
# Utility function #
####################

legal_fee = 0.01
adv_win = 0.2
adv_loss = 0.15

data %<>% mutate(tipo_comum = (tipo_vara == "Comum com advogado"),
                 tipo_jec_com = (tipo_vara == "JEC com advogado"),
                 tipo_jec_sem = (tipo_vara == "JEC sem advogado"),
                 res_acordo = (resultado == "ACORDO"),
                 res_impr = (resultado == "IMPROCEDENTE"),
                 res_parcial = (resultado == "PARCIALMENTE"),
                 res_proc = (resultado == "PROCEDENTE"),
                 utility = 
                   tipo_jec_sem*resultado_vl +
                   tipo_jec_com*res_proc*(1-adv_win)*resultado_vl +
                   tipo_jec_com*res_parcial*(1-adv_win)*resultado_vl +
                   tipo_jec_com*res_impr*resultado_vl +
                   tipo_comum*res_proc*(1-adv_win)*resultado_vl +
                   tipo_comum*res_impr*((-legal_fee - adv_loss)*valor_acao + resultado_vl) +
                   tipo_comum*res_parcial*(1 - 0.5*legal_fee - adv_win)*resultado_vl
)
data$utility = ifelse(data$res_acordo, NA, data$utility)
response_vars = c(response_vars, "utility")

#################
# Save the data #
#################

data = list(data = data,
            response_vars = response_vars,
            control_vars = control_vars,
            covars_ant = covars_ant,
            covars_post = covars_post,
            covars_text = covars_text)
saveRDS(data, "./data/data_final.rds")
