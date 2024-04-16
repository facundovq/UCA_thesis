##############################
###        Tesis           ###
##############################
 
setwd = "C:\Users\cufa9\Desktop\UCA\Tesis\R"
dir()

#Cargo los paquetes pertinentes para el análisis de las series de tiempo

install.packages('BigVAR')

suppressPackageStartupMessages({
  library(vars)
  library(BigVAR)
  library(MTS)
  library(tsbox)
  library(dplyr)
  library(corrplot)
  library(urca)
  library(lmtest)
  library(forecast)
  library(ggplot2)
  library(reshape2)
  library(tseries)
  library(tidyverse)
  library(forecast)
  library(astsa)
  library(quantmod)
  library(Quandl)
  library(xts)
  library(fpp)
  library(PerformanceAnalytics)
  library(dygraphs)
  library(pastecs)
  library(gridExtra)
  library(moments)
  library(nortest)
  library(AER)
  library(dynlm)
  library(readxl)
  library(stargazer)
  library(scales)
  library(reshape2)
  library(car)
  library(lmtest)
  library(MLmetrics)
  library(mvtnorm)
  library(mvnormtest)
  library(jtools)
  library(QuantPsyc)
  library(xtable)
})

# Importo data sets y los pongo como time series (ts).

data <- read_excel("data_unszn.xlsx")

data_anual <- read_excel("data_var_interanual.xlsx")

dummyTCR <- read_excel("dummy_TCR.xlsx")

# Cambio los datos a números 

chars <- sapply(data, is.character)

data[ ,chars] <- as.data.frame(apply(data[ , chars], 2, as.numeric))

sapply(data, class) #Lost the dates but idgaf

sapply(data_anual, class)

#Creo las series de tiempo. Todas fueron desestacionalizadas excepto GDP BRA, US que ya lo estaban.

gdp_arg <- ts(data$PBI_ARG_unsz, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_arg_var_tri <- ts(data$PBI_ARG_unsz_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_arg_var_yoy <- ts(data_anual$PBI_ARG, start = c(1998,1), end = c(2021,2), frequency = 4)

gdp_arg_tt <- ts(data$PBI_ARG_TT_unsz, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_arg_tt_var_yoy <- ts(data_anual$PBI_ARG_TT, start = c(1998,1), end = c(2021,2), frequency = 4)

gdp_arg_tt_var_tri <- ts(data$PBI_ARG_TT_unsz_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_arg_te <- ts(data$PBI_ARG_TE_unsz, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_arg_te_var_tri <- ts(data$PBI_ARG_TE_unsz_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_arg_te_var_yoy <- ts(data_anual$PBI_ARG_TE, start = c(1998,1), end = c(2021,2), frequency = 4)

gdp_arg_nt <- ts(data$PBI_ARG_NT_unsz, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_arg_nt_var_tri <-ts(data$PBI_ARG_NT_unsz_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_arg_nt_var_yoy <- ts(data_anual$PBI_ARG_NT, start = c(1998,1), end = c(2021,2), frequency = 4)

gdp_bra <- ts(data$GDP_Brasil_unsz, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_bra_var_tri <- ts(data$GDP_Brasil_unsz_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_bra_var_yoy <- ts(data_anual$GDP_BRA, start = c(1998,1), end = c(2021,2), frequency = 4)

gdp_chi <- ts(data$GDP_China_unszn, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_chi_var_tri <- ts(data$GDP_China_unszn_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_chi_var_yoy <- ts(data_anual$GDP_CHI, start = c(1998,1), end = c(2021,2), frequency = 4)

gdp_us <- ts(data$GDP_US_unszn, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_us_var_tri <- ts(data$GDP_US_unszn_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

gdp_us_var_yoy <- ts(data_anual$GDP_US, start = c(1998,1), end = c(2021,2), frequency = 4)

cpi_arg <- ts(data$CPI_ARG, start = c(1997,3), end = c(2021,2), frequency = 4)

cpi_arg_var_tri <- ts(data$CPI_ARG_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

cpi_arg_var_yoy <- ts(data_anual$CPI_ARG, start = c(1998,1), end = c(2021,2), frequency = 4)

tcr <- ts(data$TCR_US, start = c(1997,3), end = c(2021,2), frequency = 4)

tcr_var_tri <- ts(data$TCR_US_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

tcr_var_yoy <- ts(data_anual$TCR_US, start = c(1998,1), end = c(2021,2), frequency = 4)

tot <- ts(data$TOT, start = c(1997,3), end = c(2021,2), frequency = 4)

tot_var_tri <- ts(data$TOT_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

tot_var_yoy <- ts(data_anual$TOT, start = c(1998,1), end = c(2021,2), frequency = 4)

agrindex <- ts(data$IPMP_Agro, start = c(1997,3), end = c(2021,2), frequency = 4)

agrindex_var_tri <- ts(data$IPMP_Agro_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

agrindex_var_yoy <- ts(data_anual$IPMP_Agro, start = c(1998,1), end = c(2021,2), frequency = 4)

fullindex <- ts(data$IPMP_Total, start = c(1997,3), end = c(2021,2), frequency = 4)

fullindex_var_tri <- ts(data$IPMP_Total_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

fullindex_var_yoy <- ts(data_anual$IPMP_Total, start = c(1998,1), end = c(2021,2), frequency = 4)

ff <- ts(data$Fed_Funds, start = c(1997,3), end = c(2021,2), frequency = 4)

ff_var_tri <- ts(data$Fed_Funds_var_tri, start = c(1997,3), end = c(2021,2), frequency = 4)

ff_var_yoy <- ts(data_anual$Fed_Funds, start = c(1998,1), end = c(2021,2), frequency = 4)


# Creo logaritmos de los PBIs en niveles de Argentina no desestacionalizados. las variables de GDP ARG que tienen _04 es que tienen la estacionalidad.


gdp_arg_04 <- ts(data$PBI_ARG_base_04, start = c(1997,3), end = c(2021,2), frequency = 4) #Logaritmo del PBI ARG base 2004 en niveles, no desestacionalizado
l_gdp_arg_04 <- log(gdp_arg_04)

gdp_arg_tt_04 <- ts(data$PBI_ARG_TT_base_04, start = c(1997,3), end = c(2021,2), frequency = 4) #Logaritmo del PBI ARG base 2004 en niveles, no desestacionalizado
l_gdp_arg_tt_04 <- log(gdp_arg_tt_04)


gdp_arg_te_04 <- ts(data$PBI_ARG_TE_base_04, start = c(1997,3), end = c(2021,2), frequency = 4) #Logaritmo del PBI ARG base 2004 en niveles, no desestacionalizado
l_gdp_arg_te_04 <- log(gdp_arg_te_04)


gdp_arg_nt_04 <- ts(data$PBI_ARG_NT_base_04, start = c(1997,3), end = c(2021,2), frequency = 4) #Logaritmo del PBI ARG base 2004 en niveles, no desestacionalizado
l_gdp_arg_nt_04 <- log(gdp_arg_nt_04)


# Regresiones a datos sin modificar, agregando cada vez más variables

reg1 <- lm(gdp_arg ~ agrindex + cpi_arg + tcr + ff, data = data)
summary(reg1)

reg2 <- lm(gdp_arg_var_tri ~ agrindex_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg2)

reg3 <- lm(gdp_arg_nt_var_yoy ~ agrindex_var_yoy + cpi_arg_var_yoy+ tcr_var_yoy + ff_var_yoy, data = data)
summary(reg3)

# Aplico logaritmos a los otros datos

l_gdp_bra <- log(gdp_bra)

l_gdp_chi <- log(gdp_chi)

l_gdp_us <- log(gdp_us)

l_cpi_arg <- log(cpi_arg)

l_ff <- log(ff)

l_tcr <- log(tcr)

l_agrindex <- log(agrindex)

l_fullindex <- log(fullindex)

l_tot <- log(tot)



####################### TESTING CORRELATION BETWEEN VARIABLES ##################################

# Agrindex, GDP Brasil, GDP US, GDP China, TCR, FF, CPI.

 
# Variaciones interanuales 


data_df_yoy <- as.data.frame(data_anual[2:14])

corrplot_yoy <- corrplot(cor(data_df_yoy), method = "number", title = "Correlaciones de variaciones interanuales")

stargazer(cor(data_df_yoy), title="Correlation Matrix YoY variations")


# Variables en niveles con estacionalidad

data_lvls <- data %>%
  select(PBI_ARG_base_04, PBI_ARG_TT_base_04, PBI_ARG_NT_base_04, PBI_ARG_TE_base_04, TCR_US, CPI_ARG, Fed_Funds, IPMP_Agro, IPMP_Total,TOT, GDP_Brasil_unsz, GDP_China_unszn, GDP_US_unszn)

data_lvls_df <- as.data.frame(data_lvls)

corrplot_lvls <- corrplot(cor(data_lvls_df), method = "number", title = "Correlaciones de variables en niveles")

stargazer(cor(data_lvls_df), title="Correlation Matrix of variables in levels")

# Variables desestacionalizadas


data_unsz <- data %>%
  select(PBI_ARG_unsz, PBI_ARG_TT_unsz, PBI_ARG_NT_unsz, PBI_ARG_TE_unsz, TCR_US, CPI_ARG, Fed_Funds, IPMP_Agro, IPMP_Total, TOT, GDP_Brasil_unsz, GDP_China_unszn, GDP_US_unszn)

data_unsz_df <- as.data.frame(data_unsz)
  
corrplot_sz <- corrplot(cor(data_unsz_df), method ="number", title ="Correlaciones de variables desestacionalizadas")
stargazer(cor(data_unsz_df), title = "Correlation Matrix of unseasoned variables (in levels)")


# Variaciones trimestrales (desestacionalizadas)

data_unsz_var_tri <- data %>%
  select(PBI_ARG_unsz_var_tri, PBI_ARG_TT_unsz_var_tri, PBI_ARG_NT_unsz_var_tri, PBI_ARG_TE_unsz_var_tri, TCR_US_var_tri, CPI_ARG_var_tri, Fed_Funds_var_tri, IPMP_Agro_var_tri, IPMP_Total_var_tri, TOT_var_tri, GDP_Brasil_unsz_var_tri, GDP_China_unszn_var_tri, GDP_US_unszn_var_tri)

data_unsz_var_tri_df <- as.data.frame(data_unsz_var_tri)

corrplot_sz <- corrplot(cor(data_unsz_var_tri_df), method ="number", title ="Correlaciones de Variaciones trimestrales")
stargazer(cor(data_unsz_df), title = "Correlation Matrix of Quaterly variations")

## Conclusions: Las variable agrindex (IPMP Agro) es la menos correlacionada con el PBI de Argentina, comparado con TOT y IPMP Total. La frecuencia de datos con menos correlación es interanual.


######################################################################################################################################################################################

###################
### Regresiones ###
###################

## PBI ARG ##

# I. Variables en niveles (PBI ARG)

reg1.1.1 <- lm(gdp_arg ~ agrindex + gdp_chi + cpi_arg + tcr + ff, data = data)
summary(reg1.1.1)

reg1.1.2 <- lm(gdp_arg ~ agrindex + gdp_chi + gdp_bra + cpi_arg + tcr + ff, data = data)
summary(reg1.1.2)

reg1.1.3 <- lm(gdp_arg ~ agrindex + gdp_bra + cpi_arg + tcr + ff, data = data)
summary(reg1.1.3)

reg1.1.4 <- lm(gdp_arg ~ agrindex + gdp_us + gdp_bra + gdp_chi + cpi_arg + tcr + ff, data = data)
summary(reg1.1.4)

reg1.1.5 <- lm(gdp_arg ~ agrindex + cpi_arg + tcr + ff, data = data)
summary(reg1.1.5)

stargazer(reg1.1.1, reg1.1.2, reg1.1.3, reg1.1.4, reg1.1.5, title="Regresiones para GDP Arg desestacionalizado", align=TRUE)

# II. Variables en logaritmos (PBI ARG)

reg1.2.1 <- lm(l_gdp_arg_04 ~ l_agrindex + l_gdp_chi + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg1.2.1)

reg1.2.2 <- lm(l_gdp_arg_04 ~ l_agrindex + l_gdp_chi + l_gdp_bra + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg1.2.2)

reg1.2.3 <- lm(l_gdp_arg_04 ~ l_agrindex + l_gdp_bra + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg1.2.3)

reg1.2.4 <- lm(l_gdp_arg_04 ~ l_agrindex + l_gdp_us + l_gdp_bra + l_gdp_chi + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg1.2.4)

reg1.2.5 <- lm(l_gdp_arg_04 ~ l_agrindex + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg1.2.5)

stargazer(reg1.2.1, reg1.2.2, reg1.2.3, reg1.2.4, reg1.2.5, title="Regresiones para el logaritmo de GDP Arg", align=TRUE)



# III. Variables de variaciones trimestrales (PBI ARG)

reg1.3.1 <- lm(gdp_arg_var_tri ~ agrindex_var_tri + gdp_chi_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg1.3.1)

reg1.3.2 <- lm(gdp_arg_var_tri ~ agrindex_var_tri + gdp_chi_var_tri + gdp_bra_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg1.3.2)

reg1.3.3 <- lm(gdp_arg_var_tri ~ agrindex_var_tri + gdp_bra_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg1.3.3)

reg1.3.4 <- lm(gdp_arg_var_tri ~ agrindex_var_tri + gdp_us_var_tri + gdp_bra_var_tri + gdp_chi_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg1.3.4)

reg1.3.5 <- lm(gdp_arg_var_tri ~ agrindex_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg1.3.5)

stargazer(reg1.3.1, reg1.3.2, reg1.3.3, reg1.3.4, reg1.3.5, title="Regresiones para GDP Arg variaciones trimestrales (desestacionalizado)", align=TRUE)

# IV. Variables de variaciones interanuales (PBI ARG)

reg1.4.1 <- lm(gdp_arg_var_yoy ~ agrindex_var_yoy + gdp_chi_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg1.4.1)

reg1.4.2 <- lm(gdp_arg_var_yoy ~ agrindex_var_yoy + gdp_chi_var_yoy + gdp_bra_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg1.4.2)

reg1.4.3 <- lm(gdp_arg_var_yoy ~ agrindex_var_yoy + gdp_bra_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg1.4.3)

reg1.4.4 <- lm(gdp_arg_var_yoy ~ agrindex_var_yoy + gdp_us_var_yoy + gdp_bra_var_yoy + gdp_chi_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg1.4.4)

reg1.4.5 <- lm(gdp_arg_var_yoy ~ agrindex_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg1.4.5)

stargazer(reg1.4.1, reg1.4.2, reg1.4.3, reg1.4.4, reg1.4.5, title="Regresiones para GDP Arg variaciones interanuales", align=TRUE)



## PBI ARG TT ##


# I. Variables en niveles (PBI ARG TT)

reg2.1.1 <- lm(gdp_arg_tt ~ agrindex + gdp_chi + cpi_arg + tcr + ff, data = data)
summary(reg2.1.1)

reg2.1.2 <- lm(gdp_arg_tt ~ agrindex + gdp_chi + gdp_bra + cpi_arg + tcr + ff, data = data)
summary(reg2.1.2)

reg2.1.3 <- lm(gdp_arg_tt ~ agrindex + gdp_bra + cpi_arg + tcr + ff, data = data)
summary(reg2.1.3)

reg2.1.4 <- lm(gdp_arg_tt ~ agrindex + gdp_us + gdp_bra + gdp_chi + cpi_arg + tcr + ff, data = data)
summary(reg2.1.4)

reg2.1.5 <- lm(gdp_arg_tt ~ agrindex + cpi_arg + tcr + ff, data = data)
summary(reg2.1.5)

stargazer(reg2.1.1, reg2.1.2, reg2.1.3, reg2.1.4, reg2.1.5, title="Regresiones para GDP Arg TT desestacionalizado", align=TRUE)


# II. Variables en logaritmos (PBI ARG TT)

reg2.2.1 <- lm(l_gdp_arg_tt_04 ~ l_agrindex + l_gdp_chi + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg2.2.1)

reg2.2.2 <- lm(l_gdp_arg_tt_04 ~ l_agrindex + l_gdp_chi + l_gdp_bra + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg2.2.2)

reg2.2.3 <- lm(l_gdp_arg_tt_04 ~ l_agrindex + l_gdp_bra + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg2.2.3)

reg2.2.4 <- lm(l_gdp_arg_tt_04 ~ l_agrindex + l_gdp_us + l_gdp_bra + l_gdp_chi + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg2.2.4)

reg2.2.5 <- lm(l_gdp_arg_tt_04 ~ l_agrindex + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg2.2.5)

stargazer(reg2.2.1, reg2.2.2, reg2.2.3, reg2.2.4, reg2.2.5, title="Regresiones para el logaritmo de GDP Arg TT sin desestacionalizar", align=TRUE)


# III. Variables de variaciones trimestrales (PBI ARG TT)

reg2.3.1 <- lm(gdp_arg_tt_var_tri ~ agrindex_var_tri + gdp_chi_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg2.3.1)

reg2.3.2 <- lm(gdp_arg_tt_var_tri ~ agrindex_var_tri + gdp_chi_var_tri + gdp_bra_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg2.3.2)

reg2.3.3 <- lm(gdp_arg_tt_var_tri ~ agrindex_var_tri + gdp_bra_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg2.3.3)

reg2.3.4 <- lm(gdp_arg_tt_var_tri ~ agrindex_var_tri + gdp_us_var_tri + gdp_bra_var_tri + gdp_chi_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg2.3.4)

reg2.3.5 <- lm(gdp_arg_tt_var_tri ~ agrindex_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg2.3.5)

stargazer(reg2.3.1, reg2.3.2, reg2.3.3, reg2.3.4, reg2.3.5, title="Regresiones para GDP Arg TT variaciones trimestrales (desestacionalizado)", align=TRUE)


# IV. Variables de variaciones interanuales (PBI ARG TT)

reg2.4.1 <- lm(gdp_arg_tt_var_yoy ~ agrindex_var_yoy + gdp_chi_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg2.4.1)

reg2.4.2 <- lm(gdp_arg_tt_var_yoy ~ agrindex_var_yoy + gdp_chi_var_yoy + gdp_bra_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg2.4.2)

reg2.4.3 <- lm(gdp_arg_tt_var_yoy ~ agrindex_var_yoy + gdp_bra_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg2.4.3)

reg2.4.4 <- lm(gdp_arg_tt_var_yoy ~ agrindex_var_yoy + gdp_us_var_yoy + gdp_bra_var_yoy + gdp_chi_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg2.4.4)

reg2.4.5 <- lm(gdp_arg_tt_var_yoy ~ agrindex_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg2.4.5)

stargazer(reg2.4.1, reg2.4.2, reg2.4.3, reg2.4.4, reg2.4.5, title="Regresiones para GDP Arg TT variaciones interanuales", align=TRUE)



## PBI ARG NT ##


# I. Variables en niveles (PBI ARG NT)

reg3.1.1 <- lm(gdp_arg_nt ~ agrindex + gdp_chi + cpi_arg + tcr + ff, data = data)
summary(reg3.1.1)

reg3.1.2 <- lm(gdp_arg_nt ~ agrindex + gdp_chi + gdp_bra + cpi_arg + tcr + ff, data = data)
summary(reg3.1.2)

reg3.1.3 <- lm(gdp_arg_nt ~ agrindex + gdp_bra + cpi_arg + tcr + ff, data = data)
summary(reg3.1.3)

reg3.1.4 <- lm(gdp_arg_nt ~ agrindex + gdp_us + gdp_bra + gdp_chi + cpi_arg + tcr + ff, data = data)
summary(reg3.1.4)

reg3.1.5 <- lm(gdp_arg_nt ~ agrindex + cpi_arg + tcr + ff, data = data)
summary(reg3.1.5)

stargazer(reg3.1.1, reg3.1.2, reg3.1.3, reg3.1.4, reg3.1.5, title="Regresiones para GDP Arg NT desestacionalizado", align=TRUE)


# II. Variables en logaritmos (PBI ARG NT)

reg3.2.1 <- lm(l_gdp_arg_nt_04 ~ l_agrindex + l_gdp_chi + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg3.2.1)

reg3.2.2 <- lm(l_gdp_arg_nt_04 ~ l_agrindex + l_gdp_chi + l_gdp_bra + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg3.2.2)

reg3.2.3 <- lm(l_gdp_arg_nt_04 ~ l_agrindex + l_gdp_bra + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg3.2.3)

reg3.2.4 <- lm(l_gdp_arg_nt_04 ~ l_agrindex + l_gdp_us + l_gdp_bra + l_gdp_chi + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg3.2.4)

reg3.2.5 <- lm(l_gdp_arg_nt_04 ~ l_agrindex + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg3.2.5)

stargazer(reg3.2.1, reg3.2.2, reg3.2.3, reg3.2.4, reg3.2.5, title="Regresiones para el logaritmo de GDP Arg NT sin desestacionalizar", align=TRUE)



# III. Variables de variaciones trimestrales (PBI ARG NT)


reg3.3.1 <- lm(gdp_arg_nt_var_tri ~ agrindex_var_tri + gdp_chi_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg3.3.1)

reg3.3.2 <- lm(gdp_arg_nt_var_tri ~ agrindex_var_tri + gdp_chi_var_tri + gdp_bra_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg3.3.2)

reg3.3.3 <- lm(gdp_arg_nt_var_tri ~ agrindex_var_tri + gdp_bra_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg3.3.3)

reg3.3.4 <- lm(gdp_arg_nt_var_tri ~ agrindex_var_tri + gdp_us_var_tri + gdp_bra_var_tri + gdp_chi_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg3.3.4)

reg3.3.5 <- lm(gdp_arg_nt_var_tri ~ agrindex_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg3.3.5)

stargazer(reg3.3.1, reg3.3.2, reg3.3.3, reg3.3.4, reg3.3.5, title="Regresiones para GDP Arg NT variaciones trimestrales (desestacionalizado)", align=TRUE)



# IV. Variables de variaciones interanuales (PBI ARG NT)

reg3.4.1 <- lm(gdp_arg_nt_var_yoy ~ agrindex_var_yoy + gdp_chi_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg3.4.1)

reg3.4.2 <- lm(gdp_arg_nt_var_yoy ~ agrindex_var_yoy + gdp_chi_var_yoy + gdp_bra_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg3.4.2)

reg3.4.3 <- lm(gdp_arg_nt_var_yoy ~ agrindex_var_yoy + gdp_bra_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg3.4.3)

reg3.4.4 <- lm(gdp_arg_nt_var_yoy ~ agrindex_var_yoy + gdp_us_var_yoy + gdp_bra_var_yoy + gdp_chi_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg3.4.4)

reg3.4.5 <- lm(gdp_arg_nt_var_yoy ~ agrindex_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg3.4.5)

stargazer(reg3.4.1, reg3.4.2, reg3.4.3, reg3.4.4, reg3.4.5, title="Regresiones para GDP Arg NT variaciones interanuales", align=TRUE)



## PBI ARG TE ##


# I. Variables en niveles (PBI ARG TE)

reg4.1.1 <- lm(gdp_arg_te ~ agrindex + gdp_chi + cpi_arg + tcr + ff, data = data)
summary(reg4.1.1)

reg4.1.2 <- lm(gdp_arg_te ~ agrindex + gdp_chi + gdp_bra + cpi_arg + tcr + ff, data = data)
summary(reg4.1.2)

reg4.1.3 <- lm(gdp_arg_te ~ agrindex + gdp_bra + cpi_arg + tcr + ff, data = data)
summary(reg4.1.3)

reg4.1.4 <- lm(gdp_arg_te ~ agrindex + gdp_us + gdp_bra + gdp_chi + cpi_arg + tcr + ff, data = data)
summary(reg4.1.4)

reg4.1.5 <- lm(gdp_arg_te ~ agrindex + cpi_arg + tcr + ff, data = data)
summary(reg4.1.5)

stargazer(reg4.1.1, reg4.1.2, reg4.1.3, reg4.1.4, reg4.1.5, title="Regresiones para GDP Arg TE desestacionalizado", align=TRUE)


# II. Variables en logaritmos (PBI ARG TE)

reg4.2.1 <- lm(l_gdp_arg_te_04 ~ l_agrindex + l_gdp_chi + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg4.2.1)

reg4.2.2 <- lm(l_gdp_arg_te_04 ~ l_agrindex + l_gdp_chi + l_gdp_bra + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg4.2.2)

reg4.2.3 <- lm(l_gdp_arg_te_04 ~ l_agrindex + l_gdp_bra + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg4.2.3)

reg4.2.4 <- lm(l_gdp_arg_te_04 ~ l_agrindex + l_gdp_us + l_gdp_bra + l_gdp_chi + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg4.2.4)

reg4.2.5 <- lm(l_gdp_arg_te_04 ~ l_agrindex + l_cpi_arg + l_tcr + l_ff, data = data)
summary(reg4.2.5)

stargazer(reg4.2.1, reg4.2.2, reg4.2.3, reg4.2.4, reg4.2.5, title="Regresiones para el logaritmo de GDP Arg TE sin desestacionalizar", align=TRUE)


# III. Variables de variaciones trimestrales (PBI ARG TE)

reg4.3.1 <- lm(gdp_arg_te_var_tri ~ agrindex_var_tri + gdp_chi_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg4.3.1)

reg4.3.2 <- lm(gdp_arg_te_var_tri ~ agrindex_var_tri + gdp_chi_var_tri + gdp_bra_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg4.3.2)

reg4.3.3 <- lm(gdp_arg_te_var_tri ~ agrindex_var_tri + gdp_bra_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg4.3.3)

reg4.3.4 <- lm(gdp_arg_te_var_tri ~ agrindex_var_tri + gdp_us_var_tri + gdp_bra_var_tri + gdp_chi_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg4.3.4)

reg4.3.5 <- lm(gdp_arg_te_var_tri ~ agrindex_var_tri + cpi_arg_var_tri + tcr_var_tri + ff_var_tri, data = data)
summary(reg4.3.5)

stargazer(reg4.3.1, reg4.3.2, reg4.3.3, reg4.3.4, reg4.3.5, title="Regresiones para GDP Arg TE variaciones trimestrales (desestacionalizado)", align=TRUE)


# IV. Variables de variaciones interanuales (PBI ARG TE)

reg4.4.1 <- lm(gdp_arg_te_var_yoy ~ agrindex_var_yoy + gdp_chi_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg4.4.1)

reg4.4.2 <- lm(gdp_arg_te_var_yoy ~ agrindex_var_yoy + gdp_chi_var_yoy + gdp_bra_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg4.4.2)

reg4.4.3 <- lm(gdp_arg_te_var_yoy ~ agrindex_var_yoy + gdp_bra_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg4.4.3)

reg4.4.4 <- lm(gdp_arg_te_var_yoy ~ agrindex_var_yoy + gdp_us_var_yoy + gdp_bra_var_yoy + gdp_chi_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg4.4.4)

reg4.4.5 <- lm(gdp_arg_te_var_yoy ~ agrindex_var_yoy + cpi_arg_var_yoy + tcr_var_yoy + ff_var_yoy, data = data)
summary(reg4.4.5)

stargazer(reg4.4.1, reg4.4.2, reg4.4.3, reg4.4.4, reg4.4.5, title="Regresiones para GDP Arg TE variaciones interanuales", align=TRUE)


##########################################################################################################################################

######################
# Estimación del VAR #
######################

# I. Estacionariedad de las series:

# A. Test Dickey - Fuller: sin constante ni tendencia  H1 es que no tiene, H0 es que si tiene.
#####################################################

# GDP ARG 

adf1.1 <- summary(ur.df(l_gdp_arg_04, lags = 1))
adf1.1 # No rechazo H0, p value = 0.1966, la regresión tiene raíz unitaria.


# GDP ARG TE

adf1.2 <- summary(ur.df(l_gdp_arg_te_04, lags = 1))
adf1.2 # Rechazo H0, la serie no tiene una raíz unitaria. (p value <0.05)

# GDP ARG TT

adf1.3 <- summary(ur.df(l_gdp_arg_tt_04, lags = 1))
adf1.3 # No rechazo H0, la serie tiene una raíz unitaria (p-value = 0.537).

# GDP ARG NT

adf1.4 <- summary(ur.df(l_gdp_arg_nt_04, lags = 1))
adf1.4 # no rechazo H0, la serie tiene raíz unitaria (p-value = 537)

# Agrindex

adf1.5 <- summary(ur.df(l_agrindex, lags = 1))
adf1.5 #No rechazo H0, la serie tiene raíz unitaria

# GDP China

adf1.6 <- summary(ur.df(l_gdp_chi, lags = 1))
adf1.6 #Rechazo H0, la serie no tiene raíz unitaria (p-value = 6.6 e-07)

# CPI ARG


adf1.7 <- summary(ur.df(l_cpi_arg, lags = 1))
adf1.7 #Rechazo H0, la serie no tiene raíz unitaria (p-value = 2.2e-16)

# TCR

adf1.8 <- summary(ur.df(l_tcr, lags = 1))
adf1.8 #No rechazo H0, la serie tiene raíz unitaria

# Fed Funds

adf1.9 <- summary(ur.df(l_ff, lags = 1))
adf1.9 #No rechazo H0, la serie tiene raíz unitaria


# B. Test Dickey - Fuller: con constante, o con deriva
#########################################################

# GDP ARG 

adf2.1 <- summary(ur.df(l_gdp_arg_04, type = "drift", lags = 12))
adf2.1 #No rechazo H0, la serie tiene raíz unitaria, p value = 0.1966, ni constante ni deriva.


# GDP ARG TE

adf2.2 <- summary(ur.df(l_gdp_arg_te_04, type ="drift", lags = 12))
adf2.2 # Rechazo H0, la serie no tiene una raíz unitaria con constante o con tendencia (p value <0.05) y constante o deriva

# GDP ARG TT

adf2.3 <- summary(ur.df(l_gdp_arg_tt_04, type = "drift", lags = 12))
adf2.3 # Rechazo H0, la serie no tiene una raíz unitaria con constante o con tendencia (p value <0.05) y constante o deriva

# GDP ARG NT

adf2.4 <- summary(ur.df(l_gdp_arg_nt_04, type = "drift", lags = 12))
adf2.4 # Rechazo H0, la serie no tiene raíz unitaria con constante o con tendencia (p value <0.05) y constante o deriva


# Agrindex

adf2.5 <- summary(ur.df(l_agrindex, type = "drift", lags = 12))
adf2.5 #No rechazo H0, la serie tiene raíz unitaria

# GDP China

adf2.6 <- summary(ur.df(l_gdp_chi, type = "drift", lags = 12))
adf2.6 #Rechazo H0, la serie no tiene raíz unitaria (p-value = 2.2 e-07) con constante o con tendencia (p value <0.05) y constante o deriva

# CPI ARG

adf2.7 <- summary(ur.df(l_cpi_arg, type = "drift", lags = 12))
adf2.7 #Rechazo H0, la serie no tiene raíz unitaria (p-value = 8.2e-12) con constante o con tendencia (p value <0.05) y constante o deriva

# TCR

adf2.8 <- summary(ur.df(l_tcr, type = "drift", lags = 12))
adf2.8 #No/ o SI rechazo H0, la serie no tiene raíz unitaria (p-value = 0.0945)

# Fed Funds

adf2.9 <- summary(ur.df(l_ff, type = "drift", lags = 12))
adf2.9 #No rechazo H0, la serie tiene raíz unitaria


# C. Test Dickey - Fuller: con tendencia
########################################################


# GDP ARG 

adf3.1 <- summary(ur.df(l_gdp_arg_04, type = "trend", lags = 1))
adf3.1 #No rechazo H0, la serie tiene raíz unitaria.

# GDP ARG TE

adf3.2 <- summary(ur.df(l_gdp_arg_te_04, type ="trend", lags = 1))
adf3.2 # Rechazo H0, la serie no tiene una raíz unitaria y tendencia (p value <0.05)

# GDP ARG TT

adf3.3<- summary(ur.df(l_gdp_arg_tt_04, type = "trend", lags = 1))
adf3.3 # Rechazo H0, la serie no tiene una raíz unitaria. (p-value = 0.052) y tendencia

# GDP ARG NT

adf3.4 <- summary(ur.df(l_gdp_arg_nt_04, type = "trend", lags = 1))
adf3.4 # Rechazo H0, la serie no tiene raíz unitaria (p-value = 0.052) y tendencia

# Agrindex

adf3.5 <- summary(ur.df(l_agrindex, type = "trend", lags = 1))
adf3.5 # Rechazo H0, la serie no tiene raíz unitaria y ni tendencia.

# GDP China

adf3.6 <- summary(ur.df(l_gdp_chi, type = "trend", lags = 1))
adf3.6 #Rechazo H0, la serie no tiene raíz unitaria (p-value = 2.2 e-07) con constante o con tendencia (p value <0.05) y tendencia. 
# CPI ARG

adf3.7 <- summary(ur.df(l_cpi_arg, type = "trend", lags = 1))
adf3.7 #Rechazo H0, la serie no tiene raíz unitaria (p-value = 8.2e-12) con constante o con tendencia (p value <0.05) y tendencia.

# TCR

adf3.8 <- summary(ur.df(l_tcr, type = "trend", lags = 1))
adf3.8 #Rechazo H0, la serie no tiene raíz unitaria (p-value = 0.0945) y tendencia.

# Fed Funds

adf3.9 <- summary(ur.df(l_ff, type = "trend", lags = 1))
adf3.9 #No rechazo H0, la serie tiene raíz unitaria ni tendencia.
 
# Summarize results in stargazer

stargazer(adf1.1, adf1.2, adf1.3, adf1.4, adf1.5, adf1.6, adf1.7, adf1.8, adf1.9, title = "Tests Dickey-Fuller: variables en logaritmos sin diferencias")

# D. Diferenciación if needed
#############################

# GDP ARG 

ndiffs(l_gdp_arg_04) # -> 1 
d_l_gdp_arg_04 <- diff(l_gdp_arg_04)
d2_l_gdp_arg_04 <- diff(l_gdp_arg_04,2)


# GDP ARG TE

ndiffs(l_gdp_arg_te_04) # -> 1
d_l_gdp_arg_te_04 <- diff(l_gdp_arg_te_04)
d2_l_gdp_arg_te_04 <- diff(l_gdp_arg_te_04,2)

# GDP ARG TT

ndiffs(l_gdp_arg_tt_04) # -> 1
d_l_gdp_arg_tt_04 <- diff(l_gdp_arg_tt_04)
d2_l_gdp_arg_tt_04 <- diff(l_gdp_arg_tt_04,2)

# GDP ARG NT

ndiffs(l_gdp_arg_nt_04) # -> 1
d_l_gdp_arg_nt_04 <- diff(l_gdp_arg_nt_04)
d2_l_gdp_arg_nt_04 <- diff(l_gdp_arg_nt_04,2)

# Agrindex

ndiffs(l_agrindex) # -> 1
d_l_agrindex <- diff(l_agrindex)
d2_l_agrindex <- diff(l_agrindex,2)

# GDP China

ndiffs(l_gdp_chi) # -> 1
d_l_gdp_chi <- diff(l_gdp_chi)
d2_l_gdp_chi <- diff(l_gdp_chi,2)

# CPI Arg

ndiffs(l_cpi_arg) # -> 2
d_l_cpi_arg <- diff(l_cpi_arg)
d2_l_cpi_arg <- diff(l_cpi_arg,2)

# TCR

ndiffs(l_tcr) # -> 2
d_l_tcr <- diff(l_tcr)
d2_l_tcr <- diff(l_tcr,2)

# Fed Funds

ndiffs(l_ff) # -> 1
d_l_ff <- diff(l_ff)
d2_l_ff <- diff(l_ff,2)


# Re check de estacionariedad #

#################
# 1er diferencia#
#################

# A. Test Dickey - Fuller: sin constante ni tendencia. H1 es que no tiene, H0 es que si tiene.
#####################################################

# GDP ARG 

adf1.1.1 <- summary(ur.df(d_l_gdp_arg_04, lags = 1))
adf1.1.1 # Rechazo H0, p value = 2.238e-11, la regresión no tiene raíz unitaria.


# GDP ARG TE

adf1.2.1 <- summary(ur.df(d_l_gdp_arg_te_04, lags = 1))
adf1.2.1 # Rechazo H0, la serie no tiene una raíz unitaria.

# GDP ARG TT

adf1.3.1 <- summary(ur.df(d_l_gdp_arg_tt_04, lags = 1))
adf1.3.1 #Rechazo H0, la serie no tiene una raíz unitaria.

# GDP ARG NT

adf1.4.1 <- summary(ur.df(d_l_gdp_arg_nt_04, lags = 1))
adf1.4.1 #Rechazo H0, la serie  no tiene una raíz unitaria.

# Agrindex

adf1.5.1 <- summary(ur.df(d_l_agrindex, lags = 1))
adf1.5.1 #Rechazo H0, la serie no tiene raíz unitaria

# GDP China

adf1.6.1 <- summary(ur.df(d_l_gdp_chi, lags = 1))
adf1.6.1 #Rechazo H0, la serie no tiene raíz unitaria.

# CPI ARG

adf1.7.1 <- summary(ur.df(d_l_cpi_arg, lags = 1))
adf1.7.1 #No Rechazo H0, la serie tiene raíz unitaria (p-value = 0.1389)

# TCR

adf1.8.1 <- summary(ur.df(d_l_tcr, lags = 1))
adf1.8.1 #Rechazo H0, la serie no tiene raíz unitaria

# Fed Funds

adf1.9.1 <- summary(ur.df(d_l_ff, lags = 1))
adf1.9.1 #Rechazo H0, la serie no tiene raíz unitaria


# B. Test Dickey - Fuller: con constante, o con deriva
#########################################################

# GDP ARG 

adf2.1.1 <- summary(ur.df(d_l_gdp_arg_04, type = "drift", lags = 12))
adf2.1.1 #Rechazo H0, la serie no tiene raíz unitaria, ni constante ni deriva.


# GDP ARG TE

adf2.2.1 <- summary(ur.df(d_l_gdp_arg_te_04, type ="drift", lags = 12))
adf2.2.1 # Rechazo H0, la serie no tiene una raíz unitaria con constante o con tendencia (p value <0.05) y constante o deriva

# GDP ARG TT

adf2.3.1 <- summary(ur.df(d_l_gdp_arg_tt_04, type = "drift", lags = 12))
adf2.3.1 # Rechazo H0, la serie no tiene una raíz unitaria con constante o con tendencia (p value <0.05) y constante o deriva

# GDP ARG NT

adf2.4.1 <- summary(ur.df(d_l_gdp_arg_nt_04, type = "drift", lags = 12))
adf2.4.1 # Rechazo H0, la serie no tiene raíz unitaria con constante o con tendencia (p value <0.05) y constante o deriva


# Agrindex

adf2.5.1 <- summary(ur.df(d_l_agrindex, type = "drift", lags = 12))
adf2.5.1 # Rechazo H0, la serie no tiene raíz unitaria

# GDP China

adf2.6.1 <- summary(ur.df(d_l_gdp_chi, type = "drift", lags = 12))
adf2.6.1 # Rechazo H0, la serie no tiene raíz unitaria con constante o con tendencia (p value <0.05) y constante o deriva

# CPI ARG

adf2.7.1 <- summary(ur.df(d_l_cpi_arg, type = "drift", lags = 12))
adf2.7.1 # Rechazo H0, la serie no tiene raíz unitaria con constante o deriva

# TCR

adf2.8.1 <- summary(ur.df(d_l_tcr, type = "drift", lags = 12))
adf2.8.1 #Rechazo H0, la serie no tiene raíz unitaria (p-value = 0.001)

# Fed Funds

adf2.9.1 <- summary(ur.df(d_l_ff, type = "drift", lags = 12))
adf2.9.1 # Rechazo H0, la serie no tiene raíz unitaria.


# C. Test Dickey - Fuller: con tendencia
########################################################


# GDP ARG 

adf3.1.1 <- summary(ur.df(d_l_gdp_arg_04, type = "trend", lags = 1))
adf3.1.1 # Rechazo H0, la serie no tiene raíz unitaria.

# GDP ARG TE

adf3.2.1 <- summary(ur.df(d_l_gdp_arg_te_04, type ="trend", lags = 1))
adf3.2.1 # Rechazo H0, la serie no tiene una raíz unitaria y tendencia (p value <0.05)

# GDP ARG TT

adf3.3.1 <- summary(ur.df(d_l_gdp_arg_tt_04, type = "trend", lags = 1))
adf3.3.1 # Rechazo H0, la serie no tiene una raíz unitaria. (p-value = 0.052) y tendencia

# GDP ARG NT

adf3.4.1 <- summary(ur.df(d_l_gdp_arg_nt_04, type = "trend", lags = 1))
adf3.4.1 # Rechazo H0, la serie no tiene raíz unitaria (p-value = 0.052) y tendencia

# Agrindex

adf3.5.1 <- summary(ur.df(d_l_agrindex, type = "trend", lags = 1))
adf3.5.1 # Rechazo H0, la serie no tiene raíz unitaria y ni tendencia.

# GDP China

adf3.6.1 <- summary(ur.df(d_l_gdp_chi, type = "trend", lags = 1))
adf3.6.1 # Rechazo H0, la serie no tiene raíz unitaria con constante o con tendencia (p value <0.05) y tendencia. 

# CPI ARG

adf3.7.1 <- summary(ur.df(d_l_cpi_arg, type = "trend", lags = 1))
adf3.7.1 # Rechazo H0, la serie no tiene raíz unitaria con tendencia (p value <0.05).

# TCR

adf3.8.1 <- summary(ur.df(d_l_tcr, type = "trend", lags = 1))
adf3.8.1 # Rechazo H0, la serie no tiene raíz unitaria y tendencia.

# Fed Funds

adf3.9.1 <- summary(ur.df(d_l_ff, type = "trend", lags = 1))
adf3.9.1 # Rechazo H0, la serie no tiene raíz unitaria ni tendencia.


# Después de aplicar la primera diferencia, solo la variable de CPI Arg no logra rechazar la H0 de presencia de raíces unitarias (p-value 0.1389) con drift. Aplicamos entonces una segunda diferencia.

##################
# 2da diferencia #
##################

# A. Test Dickey - Fuller: sin constante ni tendencia 
#####################################################

# GDP ARG 

adf1.1.2 <- summary(ur.df(d2_l_gdp_arg_04, lags = 1))
adf1.1.2 # Rechazo H0, la regresión no tiene raíz unitaria.
stargazer(adf1.1.2)


# GDP ARG TE

adf1.2.2 <- summary(ur.df(d2_l_gdp_arg_te_04, lags = 1))
adf1.2.2 # Rechazo H0, la serie no tiene una raíz unitaria.

# GDP ARG TT

adf1.3.2 <- summary(ur.df(d2_l_gdp_arg_tt_04, lags = 1))
adf1.3.2 # Rechazo H0, la serie no tiene una raíz unitaria

# GDP ARG NT

adf1.4.2 <- summary(ur.df(d2_l_gdp_arg_nt_04, lags = 1))
adf1.4.2 # Rechazo H0, la serie no tiene raíz unitaria

# Agrindex

adf1.5.2 <- summary(ur.df(d2_l_agrindex, lags = 1))
adf1.5.2 #Rechazo H0, la serie no tiene raíz unitaria

# GDP China

adf1.6.2 <- summary(ur.df(d2_l_gdp_chi, lags = 1))
adf1.6.2 #Rechazo H0, la serie no tiene raíz unitaria

# CPI ARG

adf1.7.2 <- summary(ur.df(d2_l_cpi_arg, lags = 1))
adf1.7.2 #No Rechazo H0, la serie tiene raíz unitaria (p-value = 0.1448)

# TCR

adf1.8.2 <- summary(ur.df(d2_l_tcr, lags = 1))
adf1.8.2 #Rechazo H0, la serie no tiene raíz unitaria

# Fed Funds

adf1.9.2 <- summary(ur.df(d2_l_ff, lags = 1))
adf1.9.2 #Rechazo H0, la serie no tiene raíz unitaria


# B. Test Dickey - Fuller: con constante, o con deriva
#########################################################

# GDP ARG 

adf2.1.2 <- summary(ur.df(d2_l_gdp_arg_04, type = "drift", lags = 12))
adf2.1.2 #Rechazo H0, la serie no tiene raíz unitaria

# GDP ARG TE

adf2.2.2 <- summary(ur.df(d2_l_gdp_arg_te_04, type ="drift", lags = 12))
adf2.2.2 #Rechazo H0, la serie no tiene raíz unitaria

# GDP ARG TT

adf2.3.2 <- summary(ur.df(d2_l_gdp_arg_tt_04, type = "drift", lags = 12))
adf2.3.2 #Rechazo H0, la serie no tiene raíz unitaria

# GDP ARG NT

adf2.4.2 <- summary(ur.df(d2_l_gdp_arg_nt_04, type = "drift", lags = 12))
adf2.4.2 #Rechazo H0, la serie no tiene raíz unitaria

# Agrindex

adf2.5.2 <- summary(ur.df(d2_l_agrindex, type = "drift", lags = 12))
adf2.5.2 #Rechazo H0, la serie no tiene raíz unitaria

# GDP China

adf2.6.2 <- summary(ur.df(d2_l_gdp_chi, type = "drift", lags = 12))
adf2.6.2 #Rechazo H0, la serie no tiene raíz unitaria

# CPI ARG

adf2.7.2 <- summary(ur.df(d2_l_cpi_arg, type = "drift", lags = 12))
adf2.7.2 #Rechazo H0, la serie no tiene raíz unitaria con constante o con drift

# TCR

adf2.8.2 <- summary(ur.df(d2_l_tcr, type = "drift", lags = 12))
adf2.8.2 #Rechazo H0, la serie no tiene raíz unitaria

# Fed Funds

adf2.9.2 <- summary(ur.df(d2_l_ff, type = "drift", lags = 12))
adf2.9.2 #Rechazo H0, la serie no tiene raíz unitaria


# C. Test Dickey - Fuller: con tendencia
########################################################


# GDP ARG 

adf3.1.2 <- summary(ur.df(d2_l_gdp_arg_04, type = "trend", lags = 1))
adf3.1.2 #Rechazo H0, la serie no tiene raíz unitaria

# GDP ARG TE

adf3.2.2 <- summary(ur.df(d2_l_gdp_arg_te_04, type ="trend", lags = 1))
adf3.2.2 #Rechazo H0, la serie no tiene raíz unitaria

# GDP ARG TT

adf3.3<- summary(ur.df(d2_l_gdp_arg_tt_04, type = "trend", lags = 1))
adf3.3 #Rechazo H0, la serie no tiene raíz unitaria

# GDP ARG NT

adf3.4.2 <- summary(ur.df(d2_l_gdp_arg_nt_04, type = "trend", lags = 1))
adf3.4.2 #Rechazo H0, la serie no tiene raíz unitaria

# Agrindex

adf3.5.2 <- summary(ur.df(d2_l_agrindex, type = "trend", lags = 1))
adf3.5.2 #Rechazo H0, la serie no tiene raíz unitaria

# GDP China

adf3.6.2 <- summary(ur.df(d2_l_gdp_chi, type = "trend", lags = 1))
adf3.6.2 #Rechazo H0, la serie no tiene raíz unitaria

# CPI ARG

adf3.7.2 <- summary(ur.df(d2_l_cpi_arg, type = "trend", lags = 1))
adf3.7.2 #Rechazo H0, la serie no tiene raíz unitaria o tendencia

# TCR

adf3.8.2 <- summary(ur.df(d2_l_tcr, type = "trend", lags = 1))
adf3.8.2 #Rechazo H0, la serie no tiene raíz unitaria

# Fed Funds

adf3.9.2 <- summary(ur.df(d2_l_ff, type = "trend", lags = 1))
adf3.9.2 #Rechazo H0, la serie no tiene raíz unitaria


# Con la segunda diferencia, la variable CPI ARG se volvió estacionaria

# II. Orden de causalidad: Granger test? maybe con PBI ARG y Agrindex (????)
#####################################################################



# Agrindex

grangertest (d2_l_agrindex ~ d2_l_gdp_arg_04, order = 1) # No rechaza (i.e. no causa en sentido de Granger)
grangertest (d2_l_agrindex ~ d2_l_gdp_arg_te_04, order = 1) # No rechaza
grangertest (d2_l_agrindex ~ d2_l_gdp_arg_tt_04, order = 1) # No rechaza
grangertest (d2_l_agrindex ~ d2_l_gdp_arg_nt_04, order = 1) # No rechaza

grangertest (d2_l_agrindex ~ d2_l_cpi_arg, order = 1) # No rechaza

grangertest (d2_l_agrindex ~ d2_l_tcr, order = 1) # Rechaza H0 con p value 0.09077 (i.e. causa en sentido de Granger). Podemos decir que no rechaza al tomar p-value significativo de 5%.


# GDP China

grangertest (d2_l_gdp_chi ~ d2_l_gdp_arg_04, order = 1) # No rechaza
grangertest (d2_l_gdp_chi ~ d2_l_gdp_arg_te_04, order = 1) # Rechaza con p-value = 2.2e-16
grangertest (d2_l_gdp_chi ~ d2_l_gdp_arg_tt_04, order = 1) # Rechaza con p-value = 1.729e-5
grangertest (d2_l_gdp_chi ~ d2_l_gdp_arg_nt_04, order = 1) # Rechaza con p-value = 1.729e-5

grangertest (d2_l_gdp_chi ~ d2_l_cpi_arg, order = 1) # No rechaza

grangertest (d2_l_gdp_chi ~ d2_l_tcr, order = 1) # No rechaza

# FF

grangertest (d2_l_ff ~ d2_l_gdp_arg_04, order = 1) # No rechaza
grangertest (d2_l_ff ~ d2_l_gdp_arg_te_04, order = 1) # No rechaza
grangertest (d2_l_ff ~ d2_l_gdp_arg_tt_04, order = 1) # No rechaza
grangertest (d2_l_ff ~ d2_l_gdp_arg_nt_04, order = 1) # No rechaza

grangertest (d2_l_ff ~ d2_l_cpi_arg, order = 1) # No rechaza

grangertest (d2_l_ff ~ d2_l_tcr, order = 1) # No rechaza

# TCR VAR Dummy

grangertest (VARdummyTCR ~ d2_l_gdp_arg_04, order = 1) # No rechaza
grangertest (VARdummyTCR ~ d2_l_gdp_arg_te_04, order = 1) # No rechaza
grangertest (VARdummyTCR ~ d2_l_gdp_arg_tt_04, order = 1) # No rechaza
grangertest (VARdummyTCR ~ d2_l_gdp_arg_nt_04, order = 1) # No rechaza

grangertest (VARdummyTCR ~ d2_l_cpi_arg, order = 1) # No rechaza

grangertest (VARdummyTCR ~ d2_l_tcr, order = 1) # No rechaza


# III. DUMMY DE DEVALUACION #
#############################

VARdummyTCR <- ts(dummyTCR$devaluacion, start = c(1998,1), end = c(2021,2), frequency = 4)

#esta variable tiene menos datos porque sus compañeras de dos diferencias tienen solo 94 observaciones. 

# IV. Pre seteo de variables.
#############################

# Seteo de las variables como un data frame.


# GDP ARG: 

# A. Variables endógenas

gdp_arg_end_df <- data.frame(d2_l_gdp_arg_04, d2_l_tcr, d2_l_cpi_arg)

# B. Variables exógenas

gdp_arg_exo_df <- data.frame(d2_l_agrindex, d2_l_gdp_chi, d2_l_ff, VARdummyTCR)


# GDP ARG TE

# A. Variables endógenas

gdp_arg_te_end_df <- data.frame(d2_l_gdp_arg_te_04, d2_l_tcr, d2_l_cpi_arg)

# B. Variables exógenas

gdp_arg_te_exo_df <- data.frame(d2_l_agrindex, d2_l_gdp_chi, d2_l_ff, VARdummyTCR)

# GDP ARG TT

# A. Variables endógenas

gdp_arg_tt_end_df <- data.frame(d2_l_gdp_arg_tt_04, d2_l_tcr, d2_l_cpi_arg)

# B. Variables exógenas

gdp_arg_tt_exo_df <- data.frame(d2_l_agrindex, d2_l_gdp_chi, d2_l_ff, VARdummyTCR)

# GDP ARG NT

# A. Variables endógenas

gdp_arg_nt_end_df <- data.frame(d2_l_gdp_arg_nt_04, d2_l_tcr, d2_l_cpi_arg)

# B. Variables exógenas

gdp_arg_nt_exo_df <- data.frame(d2_l_agrindex, d2_l_gdp_chi, d2_l_ff, VARdummyTCR)


# V. Estimación de los VARX
#######################

##############################################################################################################################################################################################################################################

# VARX
#######

## I. GDP ARG ##

VARselect(gdp_arg_end_df, lag.max = 12) # AIC Criteria = 6 lags add a lag

VARX1order <- VARXorder(gdp_arg_end_df, gdp_arg_exo_df, output=TRUE) #BIC Criteria 2 for endogen, 2 for exogen.

VAR1 <- VARX(gdp_arg_end_df, p = 3, xt = gdp_arg_exo_df, m = 3) # 3 lags for endogenous variables and 3 lags for exogenous.

x11()

# Análisis de residuos

residuosVARX1 <- VAR1$residuals


# - Residuos GDP

res_VARX1_gdp <- residuosVARX1[,1]

# - Residuos TCR

res_VARX1_tcr <- residuosVARX1[,2]

# - Residuos IPC

res_VARX1_ipc <- residuosVARX1[,3]

# Normality tests

shapiro.test(res_VARX1_gdp) # No rechazo H0, residuos son normales.
shapiro.test(res_VARX1_tcr) # Rechazo H0, resiudos no son normales.
shapiro.test(res_VARX1_ipc) # Rechazo H0, residuos no son normales.

layout(matrix(1:4, 2, 2))
hist(res_VARX1_gdp, breaks = 30, col ="dark green", main = "Residuos del PBI Argentino del VARX 1")
hist(res_VARX1_tcr, breaks = 30, col ="steel blue", main = "Residuos del tipo de cambio del VARX 1")
hist(res_VARX1_ipc, breaks = 30, col ="dark red", main = "Residuos del IPC del VARX 1")

# Autocorrelations

acf(VAR1$residuals)

pacf(VAR1$residuals)


# IRF

VARXirf(VAR1,lag=12,orth=TRUE) # First plot is RF, second plot is cummulative impact.

# Magnitud de la IRF =  1 desviación estándar. = sd()

# SD de Agrindex

sd1 <- sd(gdp_arg_exo_df$l_agrindex)

sd1

VAR1$se

# SD De PBI China

sd2 <- sd(gdp_arg_exo_df$l_gdp_chi)
sd2

# SD de FF

sd3 <- sd(gdp_arg_exo_df$l_ff)
sd3

# Predicción

VARXpred(VAR1, gdp_arg_exo_df, hstep = 8, orig=0) # -> Predicción

plot(VARXpred(VAR1, gdp_arg_exo_df, hstep = 8, orig=0)

     
     
#############################
# ANALISIS CON SPLITTED PBI #
#############################
     
### II. GDP ARG TT ##


VARselect(gdp_arg_tt_end_df, lag.max = 12) # AIC Criteria = 6 lags add a lag

VARX2order <- VARXorder(gdp_arg_tt_end_df, gdp_arg_exo_df, output=TRUE) #BIC Criteria 2 for endogen, 2 for exogen.

VAR2 <- VARX(gdp_arg_tt_end_df, p = 3, xt = gdp_arg_exo_df, m = 3) # 3 lags for endogenous variables and 3 lags for exogenous.

x11()

# Análisis de residuos

residuosVARX2 <- VAR2$residuals

# - Residuos GDP

res_VARX2_gdp <- residuosVARX2[,1]

# - Residuos TCR

res_VARX2_tcr <- residuosVARX2[,2]

# - Residuos IPC

res_VARX2_ipc <- residuosVARX2[,3]

# Normality tests

shapiro.test(res_VARX2_gdp) # No rechazo H0, residuos son normales.
shapiro.test(res_VARX2_tcr) # Rechazo H0, resiudos no son normales.
shapiro.test(res_VARX2_ipc) # Rechazo H0, residuos no son normales.

layout(matrix(1:4, 2, 2))
hist(res_VARX2_gdp, breaks = 30, col ="dark green", main = "Residuos del PBI TT Argentino del VARX 2")
hist(res_VARX2_tcr, breaks = 30, col ="steel blue", main = "Residuos del tipo de cambio del VARX 2")
hist(res_VARX2_ipc, breaks = 30, col ="dark red", main = "Residuos del IPC del VARX 2")


# Autocorrelations

acf(VAR2$residuals)

pacf(VAR2$residuals)


# IRF

VARXirf(VAR2,lag=12,orth=TRUE) # First plot is RF, second plot is cummulative impact.


# Predicción

VARXpred(VAR2, gdp_arg_exo_df, hstep = 8, orig=0) # -> Predicción

plot(VARXpred(VAR2, gdp_arg_exo_df, hstep = 8, orig=0)
     
     
### III. GDP ARG TE ##
     
     
VARselect(gdp_arg_te_end_df, lag.max = 12) # AIC Criteria = 6 lags add a lag
     
VARX3order <- VARXorder(gdp_arg_te_end_df, gdp_arg_exo_df, output=TRUE) #BIC Criteria 2 for endogen, 2 for exogen.
     
VAR3 <- VARX(gdp_arg_te_end_df, p = 3, xt = gdp_arg_exo_df, m = 3) # 3 lags for endogenous variables and 3 lags for exogenous.
     
x11()
     
# Análisis de residuos
     
residuosVARX3 <- VAR3$residuals
     
# - Residuos GDP
     
res_VARX3_gdp <- residuosVARX3[,1]
     
# - Residuos TCR
     
res_VARX3_tcr <- residuosVARX3[,2]
     
# - Residuos IPC
     
res_VARX3_ipc <- residuosVARX3[,3]
     
# Normality tests
     
shapiro.test(res_VARX3_gdp) # Rechazo, residuos no son normales.
shapiro.test(res_VARX3_tcr) # Rechazo H0, resiudos no son normales.
shapiro.test(res_VARX3_ipc) # Rechazo H0, residuos no son normales.


layout(matrix(1:4, 2, 2))
hist(res_VARX3_gdp, breaks = 30, col ="dark green", main = "Residuos del PBI TE Argentino del VARX 3")
hist(res_VARX3_tcr, breaks = 30, col ="steel blue", main = "Residuos del tipo de cambio del VARX 3")
hist(res_VARX3_ipc, breaks = 30, col ="dark red", main = "Residuos del IPC del VARX 3")
     

     
# Autocorrelations
   
acf(VAR3$residuals)
     
pacf(VAR3$residuals)
     
     
# IRF
     
VARXirf(VAR3,lag=12,orth=TRUE) # First plot is RF, second plot is cummulative impact.
     
     
# Predicción
   
VARXpred(VAR3, gdp_arg_exo_df, hstep = 8, orig=0) # -> Predicción
     
plot(VARXpred(VAR3, gdp_arg_exo_df, hstep = 8, orig=0)

          
### IV. GDP ARG NT ##


VARselect(gdp_arg_nt_end_df, lag.max = 12) # AIC Criteria = 6 lags add a lag

VARX4order <- VARXorder(gdp_arg_nt_end_df, gdp_arg_exo_df, output=TRUE) #BIC Criteria 2 for endogen, 2 for exogen.

VAR4 <- VARX(gdp_arg_nt_end_df, p = 2, xt = gdp_arg_exo_df, m = 3) # 2 lags for endogenous variables and 3 lags for exogenous.

x11()

# Análisis de residuos

residuosVARX4 <- VAR4$residuals

# - Residuos GDP

res_VARX4_gdp <- residuosVARX4[,1]

# - Residuos TCR

res_VARX4_tcr <- residuosVARX4[,2]

# - Residuos IPC

res_VARX4_ipc <- residuosVARX4[,3]

# Normality tests

shapiro.test(res_VARX4_gdp) # Rechazo H0, residuos son normales.
shapiro.test(res_VARX4_tcr) # Rechazo H0, resiudos no son normales.
shapiro.test(res_VARX4_ipc) # No Rechazo H0, residuos  son normales.

layout(matrix(1:4, 2, 2))
hist(res_VARX4_gdp, breaks = 30, col ="dark green", main = "Residuos del PBI NT Argentino del VARX 4")
hist(res_VARX4_tcr, breaks = 30, col ="steel blue", main = "Residuos del tipo de cambio del VARX 4")
hist(res_VARX4_ipc, breaks = 30, col ="dark red", main = "Residuos del IPC del VARX 4")

# Autocorrelations

acf(VAR4$residuals)

pacf(VAR4$residuals)


# IRF

VARXirf(VAR4,lag=12,orth=TRUE) # First plot is RF, second plot is cummulative impact.


# Predicción

VARXpred(VAR4, gdp_arg_exo_df, hstep = 8, orig=0) # -> Predicción

plot(VARXpred(VAR4, gdp_arg_exo_df, hstep = 8, orig=0)
     
     
     
## Figuring out Shock's standard deviations ##

VAR1$varresult$l_gdp_arg_04$model




# The end. #