library(magrittr)
library(tidyverse)

data <- readRDS("./data/data_with_txt.rds")

#######################
# Variable extraction #
#######################
response_vars = c("resultado", "resultado_vl", "tempo")
control_vars = c("adv_reu", "tipo_vara")
covars_ant = c("foro", "empresa", "tipo_dano", "valor_acao", 
                  "terceiro", "comarca")
covars_post = c("magistrado", "vara")
covars_text = c("txt_limpo")

# 1. Covariates obtained from legal type

data %<>% mutate(banking = grepl("Bancário|Contratos Bancários", assunto), 
                 credit_card = grepl("Cartão de Crédito", assunto),
                 health_insurance = grepl("Planos de Saúde", assunto),
                 pvt_social_sec = grepl("Previdência privada", assunto),
                 telco = grepl("Telefonia", assunto))
covars_ant = c(covars_ant, "banking", "credit_card", "health_insurance",
               "pvt_social_sec", "telco")

# 2. Covariates obtained from decision text

# Anticipated decision
data %<>% mutate(antecipate = 
                   grepl("tutelas? antecipada|antecipacao tutela|liminar", txt_limpo))
covars_ant = c(covars_ant, "antecipate")

# Credit bureau
data %<>% mutate(debt_bureau = 
                   grepl("serasa|scpc|cadastros? maus? pagador|cadastros? devedor|orgaos? protecao credito|cadastros? inadimplente|nomes? negativ[[:alpha:]]|negativ[[:alpha:]]+ nome|negativ", txt_limpo))
covars_ant = c(covars_ant, "debt_bureau")

# Service canceled
data %<>% mutate(cancel_service = 
                   grepl("cancela[[:alpha:]]+ servico|suspen[[:alpha:]]+ servico|restabelec[[:alpha:]]+ servico|servicos? cancel|servicos? suspen|servicos? restabelec", txt_limpo))
covars_ant = c(covars_ant, "cancel_service")

# Customer law
data %<>% mutate(customer_law = 
                   grepl("relac[[:alpha:]]+ consumo|codigo defesa consumidor| cdc |legislacao consumerista", txt_limpo))
covars_ant = c(covars_ant, "customer_law")

# Onus of proof
data %<>% mutate(onus_proof = 
                   grepl("onus prova", txt_limpo))
covars_ant = c(covars_ant, "onus_proof")

# Wrongful debt collection
# REF: Civil code art. 940
data %<>% mutate(wrongful_debt = 
                   grepl("cobra[[:alpha:]]+ indevid|indevid[[:alpha:]]+ cobrad|devol[[:alpha:]]+ dobro|repeti[[:alpha:]]+ indebito|indebitos? dobro|devolver dobro|restitui[[:alpha:]]+ dobro|ilegalidade cobranca|cobrancas? ilega|cobrancas? abusiva|cobra[[:alpha:]]+ ilegitima|ilegal[[:alpha:]]+ cobra", txt_limpo))
covars_ant = c(covars_ant, "wrongful_debt")

# Abusive interest
data %<>% mutate(abusive_interest = 
                   grepl("juros? abusivo|altos juros|juros altos", txt_limpo))
covars_ant = c(covars_ant, "abusive_interest")

# Illegal bank taxes
data %<>% mutate(tac_tec = 
                   grepl("tac | tec |taxa abertura credito|tarifa emissao carne|tarifa emissao boleto|tarifas? cadastr|servico terceiro", txt_limpo))
covars_ant = c(covars_ant, "tac_tec")

# Tax reversal
data %<>% mutate(tax_reversal = 
                   grepl("estorn[[:alpha:]]+ parcela|estorn[[:alpha:]]+ prestac|estorn[[:alpha:]]+ tarifa", txt_limpo))
covars_ant = c(covars_ant, "tax_reversal")

# Contract revision
data %<>% mutate(contract_revision = 
                   grepl("revisa[[:alpha:]]+ contrat", txt_limpo))
covars_ant = c(covars_ant, "contract_revision")

# Remove text variables
data %<>% select(-txt, -txt_limpo) 

data = list(data = data,
            response_vars = response_vars,
            control_vars = control_vars,
            covars_ant = covars_ant,
            covars_post = covars_post)

saveRDS(data, "./data/data_final.rds")

# Remove large BD with texts which won't be used again
do.call(file.remove, list("./data/data_with_txt.rds"))
