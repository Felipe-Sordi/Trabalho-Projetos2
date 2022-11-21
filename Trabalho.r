######Projetos 2 


##Felipe De Sordi e Giordano Bruno Boff


#Limpando o environment e gráficos:

rm(list = ls())
graphics.off()

#Instalando e/ou carregando pacotes necessários + verificacção de conflitos:

load.lib <- c("dplyr", "tidyverse", "readxl", "ggplot2", "rbcb", "xts", "lubridate", "rbcb",
              "ipeadatar","eFRED","mFilter")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)

###Importando as bases direto do site do Bacen e do IPEA:

df_ipca <- rbcb::get_series(433)
ipca_meta <- rbcb::get_series(13521)
df_selic <- ipeadatar::ipeadata("PAN_TJOVER")
df_EMBI_br <- ipeadatar::ipeadata("JPM366_EMBI366")
df_PIB <- ipeadatar::ipeadata("PAN_PIBPMG")
df_txcambio <- ipeadatar::ipeadata("GM366_ERC366")


#importando dados do FRED, para isso é necessário acessar o site do FRED e obter uma apiKey:

api_key <- "8efb1f71cf280c2653a63ab17e618a2e"
set_fred_key(api_key)
df_FED <- fred(series_id = "RIFSPFFNA")


#######################TRATAMENTO#DAS#BASES##################################################

###IPCA

#Filtrando o IPCA para o período de interesse:

df_ipca <- filter(df_ipca, date >= as.Date("1999-01-01") & date < as.Date("2020-01-01"))

##Cálculo do IPCA acumulado durante o ano:

df_ipca <- df_ipca %>%
  mutate(date = as.Date(date),
         year = year(date)) %>%
  arrange(date)
df_ipcaA <- df_ipca %>% 
  group_by(year) %>% 
  mutate(
    tx_acum = cumprod((`433`/100)+1)-1
  )

##Criando um novo df com o IPCA acumulado por ano, ou seja, apenas o ipca acumulado no mês 12:
ipca_anual <- subset(df_ipcaA, format(date, "%m") %in% c("12") & format(date, "%Y") %in% 
                   c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006",
                     "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
                     "2015", "2016", "2017", "2018", "2019"))
ipca_anual <- ipca_anual[,-c(1,2)]

###Meta IPCA anual

#Transformando a data para ano, renomeando as colunas + transformando os valores para a mesma unidade de medida do IPCA

ipca_meta <- ipca_meta %>% 
  mutate(date = as.Date(date),
         year = year(date)) %>%
  arrange(date)
colnames(ipca_meta)[2] <- "meta"
ipca_meta <- ipca_meta[,-1]
ipca_meta$meta <- ipca_meta$meta/100

#filtrando a base para o período de interesse:

ipca_meta <- filter(ipca_meta, year <= 2019)

##criando limites min e max:

ipca_meta$lim_min <- ipca_meta$meta-0.015
ipca_meta$lim_max <- ipca_meta$meta+0.015

#Criando um df_ipca com meta, lim_min, lim_max, tx_acum tudo em porcentagem

year <- ipca_anual$year
tx_acum <- ipca_anual$tx_acum*100
meta <- ipca_meta$meta*100
lim_min <- ipca_meta$lim_min*100
lim_max <- ipca_meta$lim_max*100
df_ipca <- data.frame(year, tx_acum, meta, lim_max, lim_min)

#gráfico IPCA e meta IPCA + limtes + separação por governo:

plot_ipca <- ggplot() + 
  geom_line(data = df_ipca, aes(x=year,y=tx_acum, col ="IPCA")) + 
  geom_line(data = df_ipca, aes(x=year,y=meta, col ="Meta")) + 
  geom_ribbon(data = df_ipca, aes(x=year, ymin = lim_min, ymax = lim_max, col = "IC"),
              alpha = 0.2, fill = "#BDE3FF",
              linetype = 2 , size = 0.3) +
  geom_vline(xintercept = 2003, linetype="dashed", 
                                                      color = "#E71D36", size=0.5)+
  geom_vline(xintercept = 2011, linetype="dashed", 
             color = "#E71D36", size=0.5)+
  geom_vline(xintercept = 2016, linetype="dashed", 
             color = "#E71D36", size=0.5)+
  geom_vline(xintercept = 2019, linetype="dashed", 
             color = "#E71D36", size=0.5)+
  labs(title="IPCA x Meta IPCA", 
       subtitle="anual", 
       caption="Fontes:IBGE; BCB-Depec", y="IPCA (%)") +
  scale_color_manual(name="Legenda:", 
                     values = c("IPCA"="#FF66D8", "Meta"="#0496FF")) +
  theme_bw()

plot_ipca

###Taxa Selic over anual:

#filtrando a base para o período de interesse:

df_selic <- filter(df_selic, date >= as.Date("1999-01-01") & date < as.Date("2020-01-01"))

#transformando a data em ano + excluindo colunas desnecessárias:

df_selic$year <- year(df_selic$date)
df_selic <- df_selic[, -c(1,2,4,5)]
colnames(df_selic)[1] <- "selic"

#Gráfico Taxa de Juros Nominal - Over/ Selic

plot_selic <- ggplot()+
  geom_line(data = df_selic, aes(x=year, y=selic, col = "Selic")) +
  labs(title="Taxa de Juros Nominal - Over/Selic", 
       subtitle="", 
       caption="Fonte: Banco Central do Brasil", y="(%)") +
  scale_color_manual(name="Legenda:", 
                     values = c("Selic"="#0496FF")) +
  theme_bw()
plot_selic

#Gráfico Taxa de Juros Nominal - Over/Selic vs IPCA

plot_ipca_selic <- ggplot() + 
  geom_line(data = df_selic, aes(x=year, y=selic, col = "Selic")) +
  geom_line(data = df_ipca, aes(x=year,y=tx_acum, col ="IPCA")) +
  labs(title="Selic Nominal- Over x IPCA", 
       subtitle="", 
       caption="Fontes: Banco Central do Brasil, IBGE", y="(%)") +
  scale_color_manual(name="Legenda:", 
                     values = c("Selic"="#0496FF","IPCA"="#FF66D8")) +
  theme_bw()
plot_ipca_selic

####Taxa de Juros Natural

#EMBI+BR - risco país:

#Filtrando a base para o período de interesse:

df_EMBI_br <- filter(df_EMBI_br, date >= as.Date("1999-01-01") & date <= ("2021-01-01"))

#Excluindo colunas desnecessárias:

df_EMBI_br <- df_EMBI_br[,-c(1,4,5)]

#Transformando o índice para unidade percentual
df_EMBI_br$value <- df_EMBI_br$value/100

#transformando data em ano:

df_EMBI_br$year <- year(df_EMBI_br$date)

#média EMBI+BR anual:

df_EMBI_br <-  df_EMBI_br %>% 
  group_by(year) %>%
  summarise_at(vars(value), list(name = mean))

#alterando nome das colunas + excluindo colunas desnecessárias:

colnames(df_EMBI_br)[2] <- "risco soberano"
df_EMBI_br <- df_EMBI_br[-29,]

#Gráfico EMBI+BR, separado por governos:

plot_EMBI <- ggplot() +
  geom_line(data = df_EMBI_br, aes(x=year, y=`risco soberano`, col = "EMBI+ Risco Brasil")) +
  labs(title="EMBI+ Risco Brasil", 
       subtitle="Média Anual", 
       caption="Fonte: JP Morgan", y="(%)") +
  scale_color_manual(name="Legenda", 
                     values = c("EMBI+ Risco Brasil"="#4BB3FD")) +
  geom_vline(xintercept = 2003, linetype="dashed", 
             color = "#E71D36", size=0.5)+
  geom_vline(xintercept = 2011, linetype="dashed", 
             color = "#E71D36", size=0.5)+
  geom_vline(xintercept = 2016, linetype="dashed", 
             color = "#E71D36", size=0.5)+
  geom_vline(xintercept = 2019, linetype="dashed", 
             color = "#E71D36", size=0.5)+
  theme_bw()

plot_EMBI

###FED Funds Effective Rate:

#Filtrando período de interesse + transformação da data em ano:

df_FED <- filter(df_FED, date >= as.Date("1999-01-01") & date <= as.Date("2021-01-01"))
df_FED$year <- year(df_FED$date)

#Eliminado colunas desnecessárias:
df_FED <- df_FED[,-1]
df_FED <- df_FED[-23,]

###Risco Cambial:

#Filtrando período de interesse:

df_txcambio <- filter(df_txcambio, date >= as.Date("1999-01-01") & date <= as.Date("2021-01-01"))

#Eliminando colunas desnecessárias + tranformação da data em ano:

df_txcambio <- df_txcambio[,-c(1,4,5)]
df_txcambio$year <- year(df_txcambio$date)

#Calculando o desvio padrão da taxa de câmbio durante um ano:

df_txcambio <-  df_txcambio %>% 
  group_by(year) %>%
  summarise_at(vars(value), list(name = sd))

#Nomeando colunas#

colnames(df_txcambio)[2] <- "currency risk"



###DF taxa natural de juros:

#criando df natural, com as variáveis necessárias para calcular a taxa natural de juros:

df_natural <- data.frame(df_EMBI_br$year, df_EMBI_br$`risco soberano`, df_FED$series_id, df_txcambio$`currency risk`)


#calculando taxa natural com o uso do HP:

df_natural$r <- df_natural$df_EMBI_br..risco.soberano.+df_natural$df_FED.series_id +(df_natural$df_txcambio..currency.risk.)

hp_r = hpfilter(df_natural$r, freq = 1)
df_natural= cbind(df_natural, hp_r$cycle, hp_r$trend)
df_natural <- df_natural[-22,]

#Nomeando colunas:

colnames(df_natural)[1] <- "Year"
colnames(df_natural)[7] <- "selic natural"

#Gráfico taxa natural:

plot_txnatural <- ggplot()+
  geom_line(data = df_natural, aes(x=Year, y=r, col = "R"))+
  geom_line(data = df_natural, aes(x=Year, y=`selic natural`, col = "Taxa Natural de Juros"))+
  labs(title="Taxa de Juros natural", 
       subtitle="EMBI+BR + Risco Cambial + FED Funds effective Rates", 
       caption="Fonte: JP Morgan, Banco Central do Brasil", y="(%)") +
  scale_color_manual(name="Legenda:", 
                     values = c("R"="#0496FF", "Taxa Natural de Juros"="#FF66D8"))+
  theme_bw()

plot_txnatural
  
  
###GDP:

#filtrando preríodo de interesse:

df_PIB <- filter(df_PIB, date >= as.Date("1999-01-01") & date< as.Date("2020-01-01"))

#Passando o filtro HP - calcular o PIB potêncial:

hp_gdp <- hpfilter(df_PIB$value, freq = 1)
df_PIB <- cbind(df_PIB, hp_gdp$cycle, hp_gdp$trend)
df_PIB$Year <- year(df_PIB$date)


#Gráfico GDP:

gdp_polt <- ggplot()+
  geom_line(data = df_PIB, aes(x=Year, y=value, col="PIB Real"))+
  geom_line(data = df_PIB, aes(x=Year, y=`hp_gdp$trend`, col="PIB Potencial"))+
  labs(title = "PIB Real x PIB Potencial",caption="Fonte: IBGE", y="(%)")+
  scale_color_manual(name="Legenda", 
                     values = c("PIB Real"="#FF66D8", "PIB Potencial"= "#23B5D3"))+
  theme_bw()
gdp_polt

#Variação percentual do hiato do produto:

Y <- (100*(df_PIB$value-df_PIB$`hp_gdp$trend`)/df_PIB$`hp_gdp$trend`)

############################Data#frame#FINAL######################################################################

df_taylor <- data.frame(year, ipca_anual$tx_acum, ipca_meta$meta, df_selic$selic, Y, df_natural$`selic natural`)
colnames(df_taylor)[2] <- "IPCA"
colnames(df_taylor)[3] <- "IPCA_Meta"
colnames(df_taylor)[4] <- "Selic_over_nominal"
colnames(df_taylor)[6] <- "Selic_natural"

##Plots:

##Gráfico risco cambial x selic:

colnames(df_natural)[4] <- "Risco_cambial"
plot_riscoxselic <- ggplot()+
  geom_line(data = df_taylor, aes(x=year, y=Selic_over_nominal, col="Selic"))+
  geom_line(data = df_natural, aes(x=Year, y=Risco_cambial*100, col="Currency Risk"))+
  labs(title = "SELIC x Currency Risk",caption="Banco Central", y="(%)")+
  scale_color_manual(name="Legenda", 
                     values = c("Selic"="#FF66D8", "Currency Risk"= "#23B5D3"))+
  theme_bw()
plot_riscoxselic 


#Plot selic Real x taxa natural:

plot_selic_realxselicn <- ggplot()+
  geom_line(data = df_taylor, aes(x=year, y=((1+Selic_over_nominal)/(1+df_taylor$IPCA))-1, col="Selic real"))+
  geom_line(data = df_natural, aes(x=Year, y=`selic natural`, col="Selic Natural"))+
  labs(title = "SELIC Real x Selic natural",caption="Banco Central, IBGE", y="(%)")+
  scale_color_manual(name="Legenda", 
                     values = c("Selic real"="#FF66D8", "Selic Natural"= "#23B5D3"))+
  theme_bw()
plot_selic_realxselicn

#tabelas para o trabalho escrito:

stargazer(df_taylor)

colnames(df_natural)[2] <- "risco_soberano"
colnames(df_natural)[3] <- "fed"
colnames(df_natural)[4] <- "risco cambial"
df_natural <- df_natural[,-c(1,6)]

stargazer(df_natural)

