# clean Environment
rm(list=ls()) 

#Import libraries
library(tidyverse)
library(estimatr)
library(texreg)
library(car)
library(ggplot2)

#_______________________________________________________________________________
# Limpeza e criação da base de dados
#_______________________________________________________________________________

# Importando os dados
setwd("C:/Users/Joao arthur/OneDrive - Fundacao Getulio Vargas - FGV/2/Statistics IB/Stats Lab/Stats Lab 01")
data_wide <-  read.csv("lab01-2023-DATAfourperiod.csv")

# Transformando do formato wide pro formato long
data <- pivot_longer(data_wide,c(ideb_2005,ideb_2007,ideb_2009,ideb_2011
                             ,mtm_2005,mtm_2007,mtm_2009,mtm_2011
                             ,lp_2005,lp_2007,lp_2009,lp_2011
                             ,pibpc_2005,pibpc_2007,pibpc_2009,pibpc_2011)
                   ,names_to = c(".value", "Ano")
                   ,names_pattern = "(.*)(_\\d{4})$"
                   ,values_transform = list(measurement = as.double))

# Filtra apenas os municípios que ficam em regiões fronteirissas
data <- subset(data,div_regiao==1)

# Transformas as variáveis para facilitar análises posteriores
data$ibgecode <- factor(data$ibgecode)
data$mun <- factor(data$mun)
data$div_regiao <- factor(data$div_regiao)
data$Ano <- as.integer(gsub("_","",data$Ano))
data$ln_pibpc <- log(data$pibpc)

# Transforma o PIB per capito em log, como no exemplo da monitoria.
data$ln_pibpc <- log(data$pibpc)

#_______________________________________________________________________________
# Criação das variáveis pro DD
#_______________________________________________________________________________

# Cria a variável que identifica se a observação foi ou não tratada (Grupo de Tratamento em período pós tratamento)
data$TREAT <- ifelse(data$uf=="CE" & data$Ano>=2008,1,0)

# Cria a variável que identifica se a observação é do período pré ou pós tratamento
data$POST <- ifelse(data$Ano>=2008,1,0)

# Cria a variável que identica a que grupo pertencem os municípios (tratamento ou controle)
data$GROUP <- factor(ifelse(data$uf=="CE",1,0)
                   ,levels=c(0,1)
                   ,labels=c("Control Group","Treatment Group"))

data$Ano <- factor(data$Ano)

#_______________________________________________________________________________
# Item 3 Basic Full DiD (four periods)
#_______________________________________________________________________________

# a. Full DiD Model
m1a <- lm_robust(ideb~ GROUP*Ano
                 #   ,clusters = mun
                 ,se_type = "stata"
                 ,data=data)
# b. 1.a + fixed effects
m1b <- lm_robust(ideb~ GROUP*Ano
                 ,fixed_effects = ~mun
                 ,clusters = mun
                 ,se_type = "stata"
                 ,data=data)
# c. 1.b + controles (variáveis que variam ao longo do tempo)
m1c <- lm_robust(ideb~ GROUP*Ano
                 + ln_pibpc
                 ,fixed_effects = ~mun
                 ,clusters = mun
                 ,se_type = "stata"
                 ,data=data)

# Table 1 - Modelos do item 3
screenreg(list(m1a,m1b,m1c)
          ,custom.model.names=c("1.a","1.b","1.c")
          ,custom.coef.names=c("Intercept","Group","2007","2009","2011"
                               ,"Group*2007","Group*2009","Group*2011"
                               ,"GDPpc (ln)")
          ,custom.gof.rows=list("Fixed-effects (unit)"=c(F,T,T),
                                "Clustering (by mun)"=c(F,T,T))
          ,digits=2
          ,groups = list("Interaction"=6:8, "Control"=9)
          ,column.spacing = 0
          ,center=T)

#_______________________________________________________________________________
# Item 5 - Typical DiD model (grouped)
#_______________________________________________________________________________
# a. Basic DiD Model
m2a <- lm_robust(ideb~ GROUP + POST + TREAT
                 ,clusters = mun
                 ,se_type = "stata"
                 ,data=data)
# b. 2.a + time fixed effects
m2b <- lm_robust(ideb~ GROUP + TREAT
                 ,fixed_effects = ~Ano
                 ,clusters = mun
                 ,se_type = "stata"
                 ,data=data)
# c. 2.a + unit fixed effects
m2c <- lm_robust(ideb~ POST + TREAT
                 ,fixed_effects = ~mun
                 ,clusters = mun
                 ,se_type = "stata"
                 ,data=data)
# d. 2.a + time fixed effects + unit fixed effects
m2d <- lm_robust(ideb~ TREAT + Ano
                 ,fixed_effects = ~mun
                 ,clusters = mun
                 ,se_type = "stata"
                 ,data=data)
# e. 2.a + time fixed effects + unit fixed effects + controles (variáveis que variam ao longo do tempo)
m2e <- lm_robust(ideb~ TREAT + ln_pibpc + Ano
                 ,fixed_effects = ~mun
                 ,clusters = mun 
                 ,se_type = "stata"
                 ,data=data)

# Table 2 - Modelos do item 5
screenreg(list(m2a,m2b,m2c,m2d,m2e)
          ,custom.model.names=c("2.a","2.b","2.c","2.d","2.e")
          ,custom.coef.names=c("Intercept","Group","Post","Treatment"
                               ,"GDPpc (ln)")
          ,custom.gof.rows=list("Fixed-effects (unit)"=c(F,F,T,T,T)
                                ,"Fixed-effects (time)"=c(F,T,F,T,T)
                                ,"Clustering (by mun)"=c(T,T,T,T,T))
          ,digits=2
          ,groups = list("Interaction"=4:4, "Control"=5)
          ,omit = c("Ano")
          ,column.spacing = 0
          ,center=T)

#_______________________________________________________________________________
# Item 7 - Plot que resume bem os dados
#_______________________________________________________________________________
aggregate(ideb~Ano+GROUP,data=data, FUN=mean) %>% 
  ggplot(aes(x=Ano,y=ideb,group=GROUP,color=GROUP))+
  geom_line(size=.8)+
  theme_bw()+
  labs(x="Year",y="Ideb")
aggregate(ideb~Ano+GROUP,data=data, FUN=mean)


