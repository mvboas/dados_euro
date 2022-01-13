#Rotina para coletar séries do eurostat
#Feito por: Marcelo Vilas Boas de Castro
#última atualização: 10/11/2020

#Definindo diretórios a serem utilizados
getwd()
setwd("C:/Users/User/Documents")

#Carregando pacotes que serão utilizados
library(dplyr)
library(devtools)
#install_github("ropengov/eurostat")
library(eurostat)

#1)PIB EURO
pib = get_eurostat("namq_10_gdp")
pib =  pib[order(pib$time),]
pib_AE = pib %>% filter(geo=="EA") %>% filter(unit=="CLV_I10") %>% filter(na_item=="B1GQ") %>% filter(s_adj=="SCA")
pib_AE = subset(pib_AE, select = c(time, values))
colnames(pib_AE) = c('data', "Euro area (EA11-2000, EA12-2006, EA13-2007, EA15-2008, EA16-2010, EA17-2013, EA18-2014, EA19)")

write.csv2(pib_AE,"01-pib_AE.csv", row.names = F)
export(pib_AE, "Dados Euro(fonte).xlsx", sheetName = "pib_AE")

#2)PIB países
codigo_paises = c("DE", "EL", "FR", "IT", "UK", "PT", "ES")
nome_paises = c("Alemanha", "Grécia", "França", "Itália", "Reino Unido", "Portugal", "Espanha")
for (i in 1:length(codigo_paises)){
  dados = pib %>% filter(geo==codigo_paises[i]) %>% filter(unit=="CLV_I10") %>% filter(na_item=="B1GQ") %>% filter(s_adj=="SCA")
  dados = subset(dados, select = c(time, values))
    variacao=apply(dados[,-1],2,function(x){
      variacao_YoY=rep(NA,4)
      for(i in 13:dim(dados)[1])
        variacao_YoY[i]=((x[i]/x[i-4])-1)*100
      return(variacao_YoY)
    })
  dados = cbind(dados, variacao)
  colnames(dados) = c('data', nome_paises[i], paste("Variação YoY PIB", nome_paises[i], sep = " "))
  if(i==1)
    pib_paises = dados #Primeira repetição cria o dataframe
  else
    pib_paises = merge(pib_paises, dados, by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
  print(paste(i, length(codigo_paises), sep = '/')) #Printa o progresso da repetição
}

write.csv2(pib_paises,"02-pib_paises.csv", row.names = F)
export(pib_paises, "Dados Euro(fonte).xlsx", which = "pib_paises")

#3) Tabelas var. PIB
coleta_tabelas = function(codigo_itens, unidade, codigo_saz, nome_itens){
  for (i in 1:length(codigo_itens)){
    dados = pib %>% filter(geo=="EA") %>% filter(unit==unidade) %>% filter(na_item==codigo_itens[i]) %>% filter(s_adj==codigo_saz)
    dados = subset(dados, select = c(time, values))
    colnames(dados) = c('data', nome_itens[i])
    if(i==1)
      tabela = dados #Primeira repetição cria o dataframe
    else
      tabela = merge(tabela, dados, by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(codigo_itens), sep = '/')) #Printa o progresso da repetição
  }
  return(tabela)
}

codigo_itens1 = c("B1GQ", "P31_S14_S15", "P51G", "P52", "P3_S13", "B11")
nome_itens1 = c("PIB", "Consumo das famílias", "FBKF", "Variação de estoques", "Gastos do Governo", "Exportações Líquidas de Bens e Serviços")

#Zona do Euro - PIB - Contribuição para a Variação Trimestral,  ante ao período anterior, com ajuste sazonal
tabela1 = coleta_tabelas(codigo_itens1, "CON_PPCH_PRE", "SCA", nome_itens1)
write.csv2(tabela1,"03-tabela1.csv", row.names = F)
export(tabela1, "Dados Euro(fonte).xlsx", which = "tabela1")

#Zona do Euro - PIB - Contribuição para a variação ante igual período do ano anterior, sem ajuste sazonal  (%)
tabela2 = coleta_tabelas(codigo_itens1, "CON_PPCH_SM", "NSA", nome_itens1)
write.csv2(tabela2,"03-tabela2.csv", row.names = F)
export(tabela2, "Dados Euro(fonte).xlsx", which = "tabela2")


codigo_itens2 = c("B1GQ", "P31_S14_S15", "P51G", "P3_S13", "P6", "P7")
nome_itens2 = c("PIB", "Consumo das famílias", "FBKF", "Gastos do Governo", "Exportações", "Importações")

#Zona do Euro - PIB - Variação Trimestral,  ante ao período anterior, com ajuste sazonal  (%)
tabela3 = coleta_tabelas(codigo_itens2, "CLV_PCH_PRE", "SCA", nome_itens2)
write.csv2(tabela3,"03-tabela3.csv", row.names = F)
export(tabela3, "Dados Euro(fonte).xlsx", which = "tabela3")

#Zona do Euro - PIB - Variação Trimestral,  ante igual período do ano anterior, sem ajuste sazonal  (%)
tabela4 = coleta_tabelas(codigo_itens2, "CLV_PCH_SM", "NSA", nome_itens2)
write.csv2(tabela4,"03-tabela4.csv", row.names = F)
export(tabela4, "Dados Euro(fonte).xlsx", which = "tabela4")

#4) Inflação
inflacao = get_eurostat("prc_hicp_manr")
inflacao =  inflacao[order(inflacao$time),]

#Inflação total
inflacao_tot = inflacao %>% filter(geo=="EA") %>% filter(unit=="RCH_A") %>% filter(coicop=="CP00")
inflacao_tot = subset(inflacao_tot, select = c(time, values))
colnames(inflacao_tot) = c('data', "All-items HICP")

#Inflação núcleo
inflacao_nucleo = inflacao %>% filter(geo=="EA") %>% filter(unit=="RCH_A") %>% filter(coicop=="TOT_X_NRG_FOOD")
inflacao_nucleo = subset(inflacao_nucleo, select = c(time, values))
colnames(inflacao_nucleo) = c('data', "Overall index excluding energy, food, alcohol and tobacco")

#Juntando os dois num arquivo só
inflacao_merge = merge(inflacao_tot, inflacao_nucleo, by = "data", all = T)

write.csv2(inflacao_merge,"04-inflacao.csv", row.names = F)
export(inflacao_merge, "Dados Euro(fonte).xlsx", which = "inflacao")

#5) Produção industrial mensal
producao_industrial_mensal = get_eurostat("sts_inpr_m")
producao_industrial_mensal =  producao_industrial_mensal[order(producao_industrial_mensal$time),]

#Producao industrial mensal total
producao_industrial_mensal_total = producao_industrial_mensal %>% filter(indic_bt=="PROD") %>% filter(nace_r2=="B-D") %>% filter(s_adj=="SCA") %>% filter(unit=="I15") %>% filter(geo=="EA19")
producao_industrial_mensal_total = subset(producao_industrial_mensal_total, select = c(time, values))
colnames(producao_industrial_mensal_total) = c('data', "Mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply")

#Producao industrial mensal manufatura
producao_industrial_mensal_manufatura = producao_industrial_mensal %>% filter(indic_bt=="PROD") %>% filter(nace_r2=="C") %>% filter(s_adj=="SCA") %>% filter(unit=="I15") %>% filter(geo=="EA19")
producao_industrial_mensal_manufatura = subset(producao_industrial_mensal_manufatura, select = c(time, values))
colnames(producao_industrial_mensal_manufatura) = c('data', "Manufacturing")

#Producao industrial mensal eletricidade
producao_industrial_mensal_eletricidade = producao_industrial_mensal %>% filter(indic_bt=="PROD") %>% filter(nace_r2=="D") %>% filter(s_adj=="SCA") %>% filter(unit=="I15") %>% filter(geo=="EA19")
producao_industrial_mensal_eletricidade = subset(producao_industrial_mensal_eletricidade, select = c(time, values))
colnames(producao_industrial_mensal_eletricidade) = c('data', "Electricity, gas, steam and air conditioning supply")

#Juntando os tres num arquivo só
producao_industrial_mensal_merge = merge(producao_industrial_mensal_total, producao_industrial_mensal_manufatura, by = "data", all = T)
producao_industrial_mensal_merge = merge(producao_industrial_mensal_merge, producao_industrial_mensal_eletricidade, by = "data", all = T)

write.csv2(producao_industrial_mensal_merge,"05-producao_industrial_mensal.csv", row.names = F)
export(producao_industrial_mensal_merge, "Dados Euro(fonte).xlsx", which = "producao industrial mensal")


#6) Produção industrial trimestral
producao_industrial_trimestral = get_eurostat("sts_inpr_q")
producao_industrial_trimestral =  producao_industrial_trimestral[order(producao_industrial_trimestral$time),]

#Producao industrial mensal total
producao_industrial_trimestral_total = producao_industrial_trimestral %>% filter(indic_bt=="PROD") %>% filter(nace_r2=="B-D") %>% filter(s_adj=="SCA") %>% filter(unit=="I15") %>% filter(geo=="EA19")
producao_industrial_trimestral_total = subset(producao_industrial_trimestral_total, select = c(time, values))
colnames(producao_industrial_trimestral_total) = c('data', "Mining and quarrying; manufacturing; electricity, gas, steam and air conditioning supply")

#Producao industrial mensal manufatura
producao_industrial_trimestral_manufatura = producao_industrial_trimestral %>% filter(indic_bt=="PROD") %>% filter(nace_r2=="C") %>% filter(s_adj=="SCA") %>% filter(unit=="I15") %>% filter(geo=="EA19")
producao_industrial_trimestral_manufatura = subset(producao_industrial_trimestral_manufatura, select = c(time, values))
colnames(producao_industrial_trimestral_manufatura) = c('data', "Manufacturing")

#Producao industrial mensal eletricidade
producao_industrial_trimestral_eletricidade = producao_industrial_trimestral %>% filter(indic_bt=="PROD") %>% filter(nace_r2=="D") %>% filter(s_adj=="SCA") %>% filter(unit=="I15") %>% filter(geo=="EA19")
producao_industrial_trimestral_eletricidade = subset(producao_industrial_trimestral_eletricidade, select = c(time, values))
colnames(producao_industrial_trimestral_eletricidade) = c('data', "Electricity, gas, steam and air conditioning supply")

#Juntando os tres num arquivo só
producao_industrial_trimestral_merge = merge(producao_industrial_trimestral_total, producao_industrial_trimestral_manufatura, by = "data", all = T)
producao_industrial_trimestral_merge = merge(producao_industrial_trimestral_merge, producao_industrial_trimestral_eletricidade, by = "data", all = T)

write.csv2(producao_industrial_trimestral_merge,"06-producao_industrial_trimestral.csv", row.names = F)
export(producao_industrial_trimestral_merge, "Dados Euro(fonte).xlsx", which = "producao industrial trimestral")


#6)Vendas no varejo
vendas_varejo = get_eurostat("sts_trtu_m")
vendas_varejo = vendas_varejo[order(vendas_varejo$time),]
vendas_varejo_filtrado = vendas_varejo %>% filter(indic_bt=="TOVV") %>% filter(nace_r2=="G47") %>% filter(s_adj=="SCA") %>% filter(unit=="I15") %>% filter(geo=="EA19")
vendas_varejo_filtrado = subset(vendas_varejo_filtrado, select = c(time, values))
colnames(vendas_varejo_filtrado) = c('data', "Retail trade, except of motor vehicles and motorcycles")

write.csv2(vendas_varejo_filtrado,"07-vendas_varejo.csv", row.names = F)
export(vendas_varejo_filtrado, "Dados Euro(fonte).xlsx", which = "vendas_varejo")