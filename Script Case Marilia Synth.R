# Importação de bibliotecas
library(haven)
library(tidyverse)
library(texreg)
library(reshape2)
library(Synth)
library(dplyr)
library(SCtools)

# Importação de dados
data <- read_dta("lab02-decadaperdida.dta")
data <- as.data.frame(data)


########### Rodando os modelos de Synthetic Control

# Synth 1 (imf31 - Usa inflação em forma de índice)
set.seed(123)
dataprep.out1 <- dataprep(
  # a. define the dataframe
  foo = data
  # b. define the basic predictor
  ,predictors = c("ifo26","wdi866", "wdi567", "wdi466", "wdi59", "imf31", "wdi762", "wdi1293", "wdi354", "wdi547", "ifo28", "ifo27") 
  
  ,predictors.op = "mean" ## to be averaged
  ,time.predictors.prior = 1995:2002
  # c. Special predictors -> Skipped
  # d. dependent variable
  ,dependent = "wdi421"
  # unit variable: cid
  ,unit.variable = "cid"
  # time variable: year
  ,time.variable = "year"
  # treatment unit: Brazil
  ,treatment.identifier = 19
  # control variables
  ,controls.identifier = c(21,29,30,31,58,59,60,76,79,85,99,104,105,106,119,132,140,144,149)
  # years to minimize RMSPE
  ,time.optimize.ssr = 1995:2002
  # unit.names.variable
  , unit.names.variable = "CountryName"
  # time series for plotting purposes
  ,time.plot = 1995:2012
)

synth.out1 <- synth(data.prep.obj = dataprep.out1
                    ,optimxmethod="BFGS")

synth.tables1 <- synth.tab(dataprep.res = dataprep.out1
                           ,synth.res = synth.out1
                           ,round.digit = 3)

# Synth 2 (imf32 - Usa inflação em forma de variação)
set.seed(123)
dataprep.out2 <- dataprep(
  # a. define the dataframe
  foo = data
  # b. define the basic predictor
  ,predictors = c("wdi866", "wdi567", "wdi466", "wdi59", "imf32", "wdi762", "wdi1293", "wdi354", "wdi547", "ifo28", "ifo27", "ifo26") 
  
  ,predictors.op = "mean" ## to be averaged
  ,time.predictors.prior = 1995:2002
  # c. Special predictors -> Skipped
  # d. dependent variable
  ,dependent = "wdi421"
  # unit variable: cid
  ,unit.variable = "cid"
  # time variable: year
  ,time.variable = "year"
  # treatment unit: Brazil
  ,treatment.identifier = 19
  # control variables
  ,controls.identifier = c(21,29,30,31,58,59,60,76,79,85,99,104,105,106,119,132,140,144,149)
  # years to minimize RMSPE
  ,time.optimize.ssr = 1995:2002
  # unit.names.variable
  , unit.names.variable = "CountryName"
  # time series for plotting purposes
  ,time.plot = 1995:2012
)

synth.out2 <- synth(data.prep.obj = dataprep.out2
                    ,optimxmethod="BFGS")

synth.tables2 <- synth.tab(dataprep.res = dataprep.out2
                           ,synth.res = synth.out2
                           ,round.digit = 3)

########### Dados para a tabela

synth.tables1 # Tabelas do Synth 1

synth.tables2 # Tabelas do Synth 2


# Média Geral do Brazil Vs. dos Demais países:
data_medias <- data[,c("cid","year","CountryName", "wdi421", "wdi866", "wdi567", "wdi466", "wdi59", "imf31", "imf32", "wdi762", "wdi1293", "wdi354", "wdi547", "ifo28", "ifo27", "ifo26")]
data_medias$grupo <- ifelse(data_medias$cid == 19, "Brasil", "Outros")

summary(data_medias[data_medias$grupo=="Brasil",])
summary(data_medias[data_medias$grupo=="Outros",])

########### Cálcula o RMSPE pré-tratamento
synth.out1$loss.v # Retorna o MSPE do Synth 1
synth.out2$loss.v # Retorna o MSPE do Synth 2 

########### Path Plot com o Brasil e os dois synths

time <- dataprep.out1$tag$time.plot
actual <- dataprep.out1$Y1plot
synthetic1 <- dataprep.out1$Y0plot %*% synth.out1$solution.w
synthetic2 <- dataprep.out2$Y0plot %*% synth.out2$solution.w

plot(time, actual, type = "l", col = "black", lwd = 2,
     ylab = "GDPpc in 2005 U.S dollars", xlab = "Year", 
     main = "Path Plot", ylim = range(c(actual, synthetic1, synthetic2)))
lines(time, synthetic1, col = "orange", lwd = 2, lty = 4)
lines(time, synthetic2, col = "purple", lwd = 2, lty = 2)

# Add vertical line
abline(v = 2003, col = "blue", lty = 2)

# Add legend
legend("bottomright", legend = c("Brazil", "Synthetic Control 1", "Synthetic Control 2"),
       col = c("black", "orange", "purple"), lty = c(1, 2, 3), lwd = 2)


########### In-time placebo usando SCtools no Synth 1 (imf31)

set.seed(123)
dataprep.out_intime <- dataprep(
  # a. define the dataframe
  foo = data
  # b. define the basic predictor
  ,predictors = c("wdi866", "wdi567", "wdi466", "wdi59", "imf31", "wdi762", "wdi1293", "wdi354", "wdi547", "ifo28", "ifo27", "ifo26") 
  
  ,predictors.op = "mean" ## to be averaged
  ,time.predictors.prior = 1995:2000
  # c. Special predictors -> Skipped
  # d. dependent variable
  ,dependent = "wdi421"
  # unit variable: cid
  ,unit.variable = "cid"
  # time variable: year
  ,time.variable = "year"
  # treatment unit: Brazil
  ,treatment.identifier = 19
  # control variables
  ,controls.identifier = c(21,29,30,31,58,59,60,76,79,85,99,104,105,106,119,132,140,144,149)
  # years to minimize RMSPE
  ,time.optimize.ssr = 1995:2000
  # unit.names.variable
  , unit.names.variable = "CountryName"
  # time series for plotting purposes
  ,time.plot = 1995:2002
)
synth.out_intime <- synth(data.prep.obj = dataprep.out_intime
                          ,optimxmethod="BFGS")

synth.tables_intime <- synth.tab(dataprep.res = dataprep.out_intime
                                 ,synth.res = synth.out_intime
                                 ,round.digit = 3)
synth.tables_intime

# Synth 1
gaps.plot(synth.res = synth.out_intime
          ,dataprep.res = dataprep.out_intime
          ,Ylab = "GDPpc in 2005 U.S dollars"
          ,Xlab = "Year"
          ,Ylim = c(-620, 620)
          ,Main = "in-time Placebo")

# add extra line
abline(v=2001, col="red", lty=2)

# Criação da função que calcula a raiz quadrada, para calcular o RMSPE
rmspe <- function(x){sqrt(mean(x^2))}

# RMSPE In-time Placebo
gap_intime <- data.frame(dataprep.out_intime$Y1plot- (dataprep.out_intime$Y0plot %*% synth.out_intime$solution.w))
gap_intime$year <- rownames(gap_intime)
rownames(gap_intime) <- NULL
pre_intime <- rmspe(gap_intime[gap_intime$year < 2001, 1])
post_intime <- rmspe(gap_intime[gap_intime$year >= 2001, 1])
post_intime/pre_intime


########### In-space placebo usando SCtools no Synth 1 (imf31)

# Gera os placebos in-space
set.seed(567)
space_placebos <- generate.placebos(dataprep.out1, synth.out1)

# Cria o gráfico
plot_placebos(space_placebos
              ,discard.extreme = T
              ,ylab = "GDPpc in 2005 U.S dollars)"
              ,xlab = "Year"
              ,title = "in-time Placebo")

mspe.plot(space_placebos, discard.extreme = T)

mspe.test(space_placebos,discard.extreme=T)

# p-valor implícito é calculado dividindo o número de casos cujo valor do erro 
# pós-tratamento dividido pelo pré-tratamento é maior do que o do Brasil.
# 9/16 = 0.5625
# SOuth Africa, Ukraine