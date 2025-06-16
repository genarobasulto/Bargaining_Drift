# Data Cleaining Routine.
#September 2023
#Itza Tlaloc Quetzalcoatl Curiel Cabral
#Genaro Basulto
#Sonia Di Giannatale

#  This is a routine to explore "Compustat Snapshot North America"
# S&P  Capital IQ
# Copyright © 2012 by Standard & Poor‟s Financial Services LLC. All rights reserved



#####  libraries ####
#install.packages("tidyverse")
library(tidyverse)
library(dplyr)
library(stargazer)
library(sandwich)
library(plm) 
library(xtable)
library(ggplot2)
library("purrr") 


### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica



####  RegFullData ----
rm(list=ls())
data_all_1999_to_2022_filtered <- read.csv("Data_all_1999_to_2022.csv") #Here we read the data transformed from the python script
#View(data_all_1999_to_2022_filtered)
#summary(data_all_1999_to_2022_filtered)  # 172 605

### Eliminar variables que no son necesarias, se crearon al importar la base y manipular por primera vez.#####
data_all_1999_to_2022_filtered$X.1 <- NULL
data_all_1999_to_2022_filtered$X <- NULL
data_all_1999_to_2022_filtered$X_1 <- NULL
data_all_1999_to_2022_filtered$Y <- NULL
data_all_1999_to_2022_filtered$Y_1 <- NULL
data_all_1999_to_2022_filtered$maxebit <- NULL
data_all_1999_to_2022_filtered$maxpi <- NULL
data_all_1999_to_2022_filtered$minebit<- NULL
data_all_1999_to_2022_filtered$minpi <- NULL
data_all_1999_to_2022_filtered$ind <- NULL
data_all_1999_to_2022_filtered$ind_1<- NULL  # nos quedamos con 28 variables

summary(data_all_1999_to_2022_filtered)  # 172 605


###########  missing value, cleaning varibles & Data Frame ########### 
is.na(data_all_1999_to_2022_filtered)

sum(is.na(data_all_1999_to_2022_filtered$execid  )) 
sum(is.na(data_all_1999_to_2022_filtered$coname  ))
sum(is.na(data_all_1999_to_2022_filtered$year  ))
sum(is.na(data_all_1999_to_2022_filtered$salary  ))
sum(is.na(data_all_1999_to_2022_filtered$tdc1  ))
sum(is.na(data_all_1999_to_2022_filtered$tdc2  ))
sum(is.na(data_all_1999_to_2022_filtered$total_curr ))
sum(is.na(data_all_1999_to_2022_filtered$ceoann  ))
sum(is.na(data_all_1999_to_2022_filtered$titleann  ))
sum(is.na(data_all_1999_to_2022_filtered$gvkey  ))
sum(is.na(data_all_1999_to_2022_filtered$conm  ))
sum(is.na(data_all_1999_to_2022_filtered$ebit  )) 
sum(is.na(data_all_1999_to_2022_filtered$pi  )) 
sum(is.na(data_all_1999_to_2022_filtered$addzip  )) 
sum(is.na(data_all_1999_to_2022_filtered$city  )) 
sum(is.na(data_all_1999_to_2022_filtered$naics  )) 
sum(is.na(data_all_1999_to_2022_filtered$fyear  )) 
sum(is.na(data_all_1999_to_2022_filtered$state  )) 
sum(is.na(data_all_1999_to_2022_filtered$sale  )) 
sum(is.na(data_all_1999_to_2022_filtered$at  )) 
sum(is.na(data_all_1999_to_2022_filtered$inflation_rate  )) 
sum(is.na(data_all_1999_to_2022_filtered$inflation_multiplier  )) 
sum(is.na(data_all_1999_to_2022_filtered$size_industry  )) 

sum(is.na(data_all_1999_to_2022_filtered$age))  # 28,240
sum(is.na(data_all_1999_to_2022_filtered$joined_co)) # 134,803
sum(is.na(data_all_1999_to_2022_filtered$becameceo)) # 119,708
sum(is.na(data_all_1999_to_2022_filtered$mkvalt)) #10561


sum(is.na(data_all_1999_to_2022_filtered$ebitda  )) # 7, 684

##### limpiza de la base según los datos faltes de EBITDA  ####
data_all_1999_to_2022_filtered <- data_all_1999_to_2022_filtered[!is.na(data_all_1999_to_2022_filtered$ebitda),]
sum(is.na(data_all_1999_to_2022_filtered$ebitda))


# Después de la limpiza de EBITDA las otras variables quedaron
sum(is.na(data_all_1999_to_2022_filtered$age)) # 27, 524
sum(is.na(data_all_1999_to_2022_filtered$joined_co)) #128, 296
sum(is.na(data_all_1999_to_2022_filtered$becameceo)) #114, 404
sum(is.na(data_all_1999_to_2022_filtered$mkvalt)) #7836


### creación de Data Frame
df_compensation<- data.frame(data_all_1999_to_2022_filtered) # 164,921 obs. of 27 variables.



### Creamos algunas de las variables deseadas

df_compensation<-mutate(df_compensation, Firm_Size = log(at))

sector2 <-factor(substr(df_compensation$naics, 
                        start = 1, stop = 2) )

df_compensation <- cbind(df_compensation, sector2)


catalogo2 <- tibble(Code=c(11, 
                           21, 22, 23, 
                           31, 32, 33, 
                           42, 
                           44, 45, 
                           48, 49, 51, 52, 53, 54, 55, 56, 61, 62, 71, 72, 81, 92),   
                    Sector=c("Ag., Forestry, Fishing and Hunting",
                             "Mining",  
                             "Utilities",  
                             "Construction", 
                             "Manufacturing", 
                             "Manufacturing", 
                             "Manufacturing", 
                             "Wholesale Trade",
                             "Retail Trade", 
                             "Retail Trade", 
                             "Transp. and Warehousing", 
                             "Transp. and Warehousing",
                             "Information", 
                             "Finance and Insurance",
                             "Real Estate Rental and Leasing",
                             "Pro., Sci., and Technical Svcs",  
                             "Mgmt. of Companies and Enterprises",
                             "Admin. and Support and Waste Svcs.", 
                             "Educational Svcs", 
                             "Health Care and Social Asst.", 
                             "Arts, Entmt., and Rec.", 
                             "Accomm. and Food Svcs.", 
                             "Other Svcs. (except Public Adm.)", 
                             "Public Adm."))

df_compensation <-  
  df_compensation %>% 
  mutate(sector_2 = sector2 %>% as.character() %>% as.numeric()) %>% 
  left_join(catalogo2,by = c("sector_2"="Code")) %>% 
  glimpse()


summary(df_compensation) # 164,921
View(df_compensation)

#Market Capitalization Category

df_compensation <- df_compensation[!is.na(df_compensation$mkvalt),]

df_compensation$cap_level <- cut(df_compensation$mkvalt, 
                                 breaks = c(min(df_compensation$mkvalt), 2000, 
                                            10000, 
                                            max(df_compensation$mkvalt)),
                                 labels = c("Small Capitalization",
                                            "Medium Capitalization", 
                                            "Large Capitalization"))



###############  Exploring data ###############

###   Earnings Before Interest and Taxes ####
plot(df_compensation$ebit)

df_compensation %>% 
  ggplot() +
  geom_smooth(aes(x= year,
                  y=ebit))

boxplot(df_compensation$ebit,
        varwidth = TRUE,
        horizontal=TRUE, 
        las = 1,
        main="Box Plot Ebit ")

hist(df_compensation$ebit)

# Limpieza
boxplot_ebit <- boxplot(df_compensation$ebit,
                        varwidth = TRUE,
                        horizontal=TRUE, 
                        las = 1,
                        main="Box Plot ")
boxplot_ebit$out
df_compensation_ebit <- df_compensation[!(df_compensation$ebit %in% boxplot_ebit$out),]  
# 143,687
boxplot_ebit <- boxplot(df_compensation_ebit$ebit,
                        varwidth = TRUE,
                        horizontal=TRUE, 
                        las = 1,
                        main="Box Plot ")

hist(df_compensation_ebit$ebit)
plot(df_compensation_ebit$ebit)
summary(df_compensation_ebit$ebit)

###   Earnings Before Interest and Taxes Depreciation and Amortization ####
plot(df_compensation$ebitda)

df_compensation %>% 
  ggplot() +
  geom_smooth(aes(x= year,
                  y=ebitda))

boxplot(df_compensation$ebit,
        varwidth = TRUE,
        horizontal=TRUE, 
        las = 1,
        main="Box Plot EBITDA ")

hist(df_compensation$ebitda)

# Limpieza
boxplot_ebitda <- boxplot(df_compensation$ebitda,
                          varwidth = TRUE,
                          horizontal=TRUE, 
                          las = 1,
                          main="Box Plot EBITDA")
boxplot_ebitda$out
df_compensation_ebitda <- df_compensation[!(df_compensation$ebitda %in% boxplot_ebitda$out),]  
# 143,214
boxplot_ebitda <- boxplot(df_compensation_ebit$ebitda,
                          varwidth = TRUE,
                          horizontal=TRUE, 
                          las = 1,
                          main="Box Plot EBITDA")

hist(df_compensation_ebit$ebitda)
plot(df_compensation_ebit$ebitda)


summary(df_compensation_ebit$ebitda)


###   Pretax Income ####
plot(df_compensation$pi)

df_compensation %>% 
  ggplot() +
  geom_smooth(aes(x= year,
                  y=pi))

boxplot(df_compensation$pi,
        varwidth = TRUE,
        horizontal=TRUE, 
        las = 1,
        main="Box Plot Pretax Income ")

hist(df_compensation$pi)

# Limpieza
boxplot_pi <- boxplot(df_compensation$pi,
                      varwidth = TRUE,
                      horizontal=TRUE, 
                      las = 1,
                      main="Box Plot Pretax Income")
boxplot_pi$out
df_compensation_pi <- df_compensation[!(df_compensation$pi %in% boxplot_pi$out),]  
# 143,824
boxplot_pi <- boxplot(df_compensation_pi$pi,
                      varwidth = TRUE,
                      horizontal=TRUE, 
                      las = 1,
                      main="Box Plot Pretax Income")

hist(df_compensation_pi$pi)
plot(df_compensation_pi$pi)


###   Salary ####
plot(df_compensation$salary)

df_compensation %>% 
  ggplot() +
  geom_smooth(aes(x= year,
                  y=salary))

boxplot(df_compensation$salary,
        varwidth = TRUE,
        horizontal=TRUE, 
        las = 1,
        main="Box Plot Salary ")

hist(df_compensation$salary)

# Limpieza
boxplot_salary <- boxplot(df_compensation$salary,
                          varwidth = TRUE,
                          horizontal=TRUE, 
                          las = 1,
                          main="Box Plot Salary ")
boxplot_salary$out

df_compensation_salary <- df_compensation[!(df_compensation$salary %in% boxplot_salary$out),]  
# 158,400
boxplot_salary <- boxplot(df_compensation_salary$salary,
                          varwidth = TRUE,
                          horizontal=TRUE, 
                          las = 1,
                          main="Box Plot Salary")

hist(df_compensation_salary$salary)
plot(df_compensation_salary$salary)


###   Total Compensation (Salary + Bonus + Other Annual + Restricted Stock Grants + LTIP Payouts + All other + Value of Option Grants) ####
plot(df_compensation$tdc1)

df_compensation %>% 
  ggplot() +
  geom_smooth(aes(x= year,
                  y=tdc1))

boxplot(df_compensation$tdc1,
        varwidth = TRUE,
        horizontal=TRUE, 
        las = 1,
        main="Box Plot Total Compensation +++ Value of Option Grants")

hist(df_compensation$tdc1)

# Limpieza
boxplot_tdc1 <- boxplot(df_compensation$tdc1,
                        varwidth = TRUE,
                        horizontal=TRUE, 
                        las = 1,
                        main="Box Plot Total Compensation +++ Value of Option Grants ")
boxplot_tdc1$out

df_compensation_tdc1 <- df_compensation[!(df_compensation$tdc1 %in% boxplot_tdc1$out),]  
# 149,967
boxplot_tdc1 <- boxplot(df_compensation_tdc1$tdc1,
                        varwidth = TRUE,
                        horizontal=TRUE, 
                        las = 1,
                        main="Box Plot  Total Compensation +++ Value of Option Grants ")

hist(df_compensation_tdc1$tdc1)
plot(df_compensation_tdc1$tdc1)


###   Total Compensation (Salary + Bonus + Other Annual + Restricted Stock Grants + LTIP Payouts + All other + Value of Option Exercised) ####
plot(df_compensation$tdc2)

df_compensation %>% 
  ggplot() +
  geom_smooth(aes(x= year,
                  y=tdc2))

boxplot(df_compensation$tdc2,
        varwidth = TRUE,
        horizontal=TRUE, 
        las = 1,
        main="Box Plot Total Compensation +++ Value of Option Exercised")

hist(df_compensation$tdc2)

# Limpieza
boxplot_tdc2 <- boxplot(df_compensation$tdc2,
                        varwidth = TRUE,
                        horizontal=TRUE, 
                        las = 1,
                        main="Box Plot Total Compensation +++ Value of Option Exercised")
boxplot_tdc2$out

df_compensation_tdc2 <- df_compensation[!(df_compensation$tdc2 %in% boxplot_tdc2$out),]  
# 148,556
boxplot_tdc2 <- boxplot(df_compensation_tdc2$tdc2,
                        varwidth = TRUE,
                        horizontal=TRUE, 
                        las = 1,
                        main="Box PlotTotal Compensation +++ Value of Option Exercised")

hist(df_compensation_tdc1$tdc2)
plot(df_compensation_tdc1$tdc2)

###   Total Current Compensation (Salary + Bonus) ####

plot(df_compensation$total_curr)

df_compensation %>% 
  ggplot() +
  geom_smooth(aes(x= year,
                  y=total_curr))

boxplot(df_compensation$total_curr,
        varwidth = TRUE,
        horizontal=TRUE, 
        las = 1,
        main="Box Plot Total Current Compensation (Salary + Bonus)")

hist(df_compensation$total_curr)

# Limpieza
boxplot_total_curr <- boxplot(df_compensation$total_curr,
                              varwidth = TRUE,
                              horizontal=TRUE, 
                              las = 1,
                              main="Box Plot Total Current Compensation (Salary + Bonus)")
boxplot_total_curr$out

df_compensation_total_curr <- df_compensation[!(df_compensation$total_curr %in% boxplot_total_curr$out),]  
# 151,914
boxplot_total_curr <- boxplot(df_compensation_total_curr$total_curr,
                              varwidth = TRUE,
                              horizontal=TRUE, 
                              las = 1,
                              main="Box Plot Total Current Compensation (Salary + Bonus)")

hist(df_compensation_tdc1$total_curr)
plot(df_compensation_tdc1$total_curr)

##############################
###### LIMPIEZA GENERAL ######
##############################

df_compensation_General<-df_compensation

boxplot_salary <- boxplot(df_compensation_General$salary,
                          varwidth = TRUE,
                          horizontal=TRUE, 
                          las = 1,
                          main="Box Plot Salary ")
boxplot_salary$out

df_compensation_General <- df_compensation_General[!(df_compensation_General$salary %in% boxplot_salary$out),]  #158 400


boxplot_total_curr <- boxplot(df_compensation_General$total_curr,
                              varwidth = TRUE,
                              horizontal=TRUE, 
                              las = 1,
                              main="Box Plot total_curr ")
boxplot_total_curr$out


df_compensation_General <- df_compensation_General[!(df_compensation_General$total_curr%in% boxplot_total_curr$out),]  #146,434

boxplot_ebit <- boxplot(df_compensation_General$ebit,
                        varwidth = TRUE,
                        horizontal=TRUE, 
                        las = 1,
                        main="Box Plot EBIT ")
boxplot_ebit$out

df_compensation_General <- df_compensation_General[!( df_compensation_General$ebit %in% boxplot_ebit$out),]  #128,494

boxplot_pi <- boxplot(df_compensation_General$pi,
                      varwidth = TRUE,
                      horizontal=TRUE, 
                      las = 1,
                      main="Box Plot Pretax Income ")
boxplot_pi$out

df_compensation_General <- df_compensation_General[!( df_compensation_General$pi %in% boxplot_pi$out),]  

# 118 165




#Eliminar el valor más alto de tdc2
df_compensation_General<-df_compensation_General[!(df_compensation_General$tdc2 > 177000),]

#Eliminar los 4 valores más altos de tdc1
df_compensation_General<-df_compensation_General[!(df_compensation_General$tdc1 > 161),]

# 118 165 observations  and   31 variables

getwd() 

##### Saving Data Frame ####


write.csv(df_compensation_General, file = "df_compensation_final.csv")  # 118 165

write.csv(df_compensation_General22, "df_compensation_General.csv", row.names=FALSE)
