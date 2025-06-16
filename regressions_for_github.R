#################
#   Cargar librerias
#################


library(tidyverse) 
library(dplyr)
library(stargazer) #Tablas que se exportan a latex
library(sandwich)
library(plm) 
library(xtable)


############################
### NO CORRER ESTA PARTE ###
############################

#Read Data Base for Regression
rm(list=ls())

compensations <- read.csv("df_compensation_Final2.csv")
names(compensations)


#########################
### CORRER ESTA PARTE ###
#########################


        #################
        #   Definición de Variables
        #################


# Aquí se puede poner:  
#  ebit,  pi, mkvalt

    
#################
#   Correr para el caso de mkvalt
#################


#find max output by firm 
max_mkval <- compensations %>%
  group_by(gvkey) %>%
  summarise(max_mkval = max(mkvalt, na.rm=TRUE),  # Aquí se puede cambiar
            min_mkval = min(mkvalt, na.rm=TRUE))  # Aquí se puede cambiar

compensations <- left_join(compensations, max_mkval,  # Aqui se puede cambiar
                           by = join_by(gvkey == gvkey))

#Get indicator of high or low output
compensations <- compensations %>%
  mutate(I = if_else(max_mkval - mkvalt > mkvalt - min_mkval, 1,-1)) #Aquí se puede cambiar





#########################
### CORRER ESTA PARTE ###
#########################


#################
#   Definición de f2
#################

# Aquí se puede poner:  
# Define f(w) con  0.5, 0.4, 0.3, 0.2, 0.1, 0.05 

r = 0.5 #Cambiar aqui. 

f<- function(x){
  return(x**(r)) # Aqui se puede cambiar
}

#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Create Regression Variables X= (I*(yt/yH))*f(yH), Y = f(w_t+1) - f(w_t)
#################

# Aqui se puede poner: 
    #tdc1,  
    #tdc2, 
    #total_curr, 
    #salary

compensations <- compensations %>%
  mutate(f_w = f(total_curr)) %>%      #Aqui se puede cambiar
  mutate(f_yh = f(max_mkval)) %>%
  group_by(execid) %>% 
  mutate(Y = c(diff(f_w),0)) %>%
  mutate(X = (I*(mkvalt/max_mkval))*f_yh)      #Aqui se puede cambiar

#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Correr regresiones aquí:
#################


#MCO Model ebit and total current
mod_mco <- lm(Y~X,data=compensations)

#MCO Model ebit and total current whit controls
mod_mco_ctr <- lm(Y~X
                  #+#Esto es para ebit
                  #+X*year+X*age+X*cap_level+X*Sector,data=compensations) 
                  #Esto es para mkval
                  +X*year+X*age+X*Sector,data=compensations)  

#Fixed Effects Model
mod_within <- plm(Y~X, index = c("execid", "year"),
                  model="within", data=compensations)

#Fixed Effects Model with controls
mod_within_ctr <- plm(Y~X
                      #Esto es para ebit
                      #+X*age+X*cap_level+X*Sector, index = c("execid", "year"),
                      #Esto es para mkval
                      +X*age+X*Sector, index = c("execid", "year"),
                      model="within", data=compensations)


#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Mostrar los resultados de las regresiones
#################


# Mostrar resultados en tabla
stargazer(mod_mco, 
          mod_mco_ctr, 
          mod_within,
          mod_within_ctr, type = 'text') 
#Aqui se puede cambiar a type = 'text' para verlo impreso
#Aqui se puede cambiar a type = 'latex' para código para tabla




#########################
### CORRER ESTA PARTE ###
#########################

compensations_large <- subset(compensations, cap_level=="Large Capitalization")

compensations_medium <- subset(compensations, cap_level=="Medium Capitalization")

compensations_small <- subset(compensations, cap_level=="Small Capitalization")

###########
#### LARGE
###########

#find max output by firm 
max_mkval <- compensations_large %>%
  group_by(gvkey) %>%
  summarise(max_mkval = max(mkvalt, na.rm=TRUE),  
            min_mkval = min(mkvalt, na.rm=TRUE))  

compensations_large <- left_join(compensations_large, max_mkval,  
                                 by = join_by(gvkey == gvkey))

#Get indicator of high or low output
compensations_large <- compensations_large %>%
  mutate(I = if_else(max_mkval - mkvalt > mkvalt - min_mkval, 1,-1)) 


#################
#   Definición de f
#################

# Aquí se puede poner:  
# Define f(w) con  0.5, 0.4, 0.3, 0.2, 0.1, 0.05 

f<- function(x){
  return(x**(0.5)) # Aqui se puede cambiar
}


#################
#   Create Regression Variables X= (I*(yt/yH))*f(yH), Y = f(w_t+1) - f(w_t)
#################


# Aqui se puede poner: 
#  tdc1, 
#  tdc2, 
#  total_curr, 
#  salary

compensations_large <- compensations_large %>%
  mutate(f_w = f(salary)) %>%      #Aqui se puede cambiar
  mutate(f_yh = f(max_mkval)) %>% 
  group_by(execid) %>% 
  mutate(Y = c(diff(f_w),0)) %>%
  mutate(X = (I*(mkvalt/max_mkval))*f_yh)       


#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Correr regresiones aquí:
#################


#MCO Model ebit and total current
mod_mco <- lm(Y~X,
              data=compensations_large)

#MCO Model ebit and total current whit controls
mod_mco_ctr <- lm(Y~X
                  +X*year+X*age+X*Sector,
                  data=compensations_large)  

#Fixed Effects Model
mod_within <- plm(Y~X, 
                  index = c("execid", "year"),
                  model="within", 
                  data=compensations_large)

#Fixed Effects Model with controls
mod_within_ctr <- plm(Y~X+X*age+X*Sector, 
                      index = c("execid", "year"),
                      model="within", 
                      data=compensations_large)


#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Mostrar los resultados de las regresiones por grupos segun capitalización
#################


# Mostrar resultados en tabla
stargazer(mod_mco, 
          mod_mco_ctr, 
          mod_within,
          mod_within_ctr, type = 'latex') 
#Aqui se puede cambiar a type = 'text' para verlo impreso
#Aqui se puede cambiar a type = 'latex' para código para tabla








############
#### MEDIUM
############

#find max output by firm 
max_mkval <- compensations_medium %>%
  group_by(gvkey) %>%
  summarise(max_mkval = max(mkvalt, na.rm=TRUE),  
            min_mkval = min(mkvalt, na.rm=TRUE))  

compensations_medium <- left_join(compensations_medium, max_mkval,  
                                 by = join_by(gvkey == gvkey))

#Get indicator of high or low output
compensations_medium <- compensations_medium %>%
  mutate(I = if_else(max_mkval - mkvalt > mkvalt - min_mkval, 1,-1)) 




#################
#   Definición de f
#################

# Aquí se puede poner:  
# Define f(w) con  0.5, 0.4, 0.3, 0.2, 0.1, 0.05 

f<- function(x){
  return(x**(0.5)) # Aqui se puede cambiar
}


#################
#   Create Regression Variables X= (I*(yt/yH))*f(yH), Y = f(w_t+1) - f(w_t)
#################


# Aqui se puede poner: 
#  tdc1, 
#  tdc2, 
#  total_curr, 
#  salary

compensations_medium <- compensations_medium %>%
  mutate(f_w = f(salary)) %>%      #Aqui se puede cambiar
  mutate(f_yh = f(max_mkval)) %>% 
  group_by(execid) %>% 
  mutate(Y = c(diff(f_w),0)) %>%
  mutate(X = (I*(mkvalt/max_mkval))*f_yh)       




#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Correr regresiones aquí:
#################


#MCO Model ebit and total current
mod_mco <- lm(Y~X,
              data=compensations_medium)

#MCO Model ebit and total current whit controls
mod_mco_ctr <- lm(Y~X
                  +X*year+X*age+X*Sector,
                  data=compensations_medium)  

#Fixed Effects Model
mod_within <- plm(Y~X, 
                  index = c("execid", "year"),
                  model="within", 
                  data=compensations_medium)

#Fixed Effects Model with controls
mod_within_ctr <- plm(Y~X+X*age+X*Sector, 
                      index = c("execid", "year"),
                      model="within", 
                      data=compensations_medium)


#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Mostrar los resultados de las regresiones por grupos segun capitalización
#################


# Mostrar resultados en tabla
stargazer(mod_mco, 
          mod_mco_ctr, 
          mod_within,
          mod_within_ctr, type = 'latex') 
#Aqui se puede cambiar a type = 'text' para verlo impreso
#Aqui se puede cambiar a type = 'latex' para código para tabla










##########
### SMALL
##########

#find max output by firm 
max_mkval <- compensations_small %>%
  group_by(gvkey) %>%
  summarise(max_mkval = max(mkvalt, na.rm=TRUE),  
            min_mkval = min(mkvalt, na.rm=TRUE))  

compensations_small <- left_join(compensations_small, max_mkval,  
                                 by = join_by(gvkey == gvkey))

#Get indicator of high or low output
compensations_small <- compensations_small %>%
  mutate(I = if_else(max_mkval - mkvalt > mkvalt - min_mkval, 1,-1)) 




#################
#   Definición de f
#################

# Aquí se puede poner:  
# Define f(w) con  0.5, 0.4, 0.3, 0.2, 0.1, 0.05 

f<- function(x){
  return(x**(0.5)) # Aqui se puede cambiar
}


#################
#   Create Regression Variables X= (I*(yt/yH))*f(yH), Y = f(w_t+1) - f(w_t)
#################


# Aqui se puede poner: 
    #  tdc1, 
    #  tdc2, 
    #  total_curr, 
    #  salary

compensations_small <- compensations_small %>%
  mutate(f_w = f(salary)) %>%      #Aqui se puede cambiar
  mutate(f_yh = f(max_mkval)) %>% 
  group_by(execid) %>% 
  mutate(Y = c(diff(f_w),0)) %>%
  mutate(X = (I*(mkvalt/max_mkval))*f_yh)       




#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Correr regresiones aquí:
#################


#MCO Model ebit and total current
mod_mco <- lm(Y~X,
              data=compensations_small)

#MCO Model ebit and total current whit controls
mod_mco_ctr <- lm(Y~X
                  +X*year+X*age+X*Sector,
                  data=compensations_small)  

#Fixed Effects Model
mod_within <- plm(Y~X, 
                  index = c("execid", "year"),
                  model="within", 
                  data=compensations_small)

#Fixed Effects Model with controls
mod_within_ctr <- plm(Y~X+X*age+X*Sector, 
                      index = c("execid", "year"),
                      model="within", 
                      data=compensations_small)


#########################
### CORRER ESTA PARTE ###
#########################

#################
#   Mostrar los resultados de las regresiones por grupos segun capitalización
#################


# Mostrar resultados en tabla
stargazer(mod_mco, 
          mod_mco_ctr, 
          mod_within,
          mod_within_ctr, type = 'latex') 
#Aqui se puede cambiar a type = 'text' para verlo impreso
#Aqui se puede cambiar a type = 'latex' para código para tabla



























#########################
### CORRER ESTA PARTE ###
#########################


#################
#    Usar el modelo para predecir salarios futuros
################

#Tomar epsilon del modelo 'full' - Panel con controles
eps_hat = unname(mod_within_ctr$coefficients[1])

#################
#    Ejercicio con un ejecutivo 
################


#Encontrar un ejecutivo con mas de 20 observaciones 
compensations %>% 
  group_by(execid) %>%
  count() %>% 
  filter(n >= 20) 

#Seleccionar uno 
example_exec =  4065  #Aqui se puede cambiar

#Filtrar sus observaciones

example_comp <- compensations %>%
  filter(execid == example_exec)

example_comp


#################
#    Graficar compensaciones 
################


example_comp %>% 
  ggplot(aes(x= year,
             y=total_curr)) +
  geom_line()

#Modelo dice w_{t+1} = f^{-1}(f(w_t) + eps*I*(y_t/y_H)*f(y_H)+u) 

sim_comp = c(example_comp$total_curr[1])

for(k in 1:23){
  sim_comp[k+1] = (f(sim_comp[k]) + eps_hat*example_comp$I[k]*(example_comp$mkvalt[k]/example_comp$max_mkval[1])*example_comp$f_yh[1])^(1/0.5)
}

example_comp['sim_comp'] <- sim_comp


example_comp %>% 
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr), color = 'green') +
  geom_line(aes(x= year,
                y=sim_comp))
example_comp %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = mkvalt), color = 'green') +
  geom_line(aes(x= year,
                y=I))


######################
# Simulate all comps.#
######################

#Filter only executives with at least 10 years in the database

compensations <- merge(compensations, count(group_by(compensations, execid, gvkey)), by = c('execid', 'gvkey'))

comp_to_sim <- compensations %>%
  ungroup() %>%
  filter(n >= 10) %>%
  select(c("execid", "year","cap_level",  "total_curr", "mkvalt","max_mkval", "I", 
           "f_yh","gvkey", "n")) %>%
  group_by(execid) %>%
  mutate(nc = length(unique(gvkey))) %>%
  filter(nc == 1) %>%
  ungroup() %>% 
  arrange(execid, year)

#Check how many executives we have
length(unique(comp_to_sim$execid))


#For loop to simulate 
Sim_Comps <- c()
for(data in group_split(comp_to_sim, execid)){
  sim_comp = c(data$total_curr[1])
  ny = data$n[1]
  yh = data$max_mkval[1]
  for(k in 1:(ny-1)){
    sim_comp[k+1] = (max(0,f(sim_comp[k]) + eps_hat*data$I[k]*(data$mkvalt[k]/yh)*f(yh)))^(1/r)
  }
  #print(data$execid[1])
  Sim_Comps <- c(Sim_Comps, sim_comp)
}

comp_to_sim$simulated_total_curr <- Sim_Comps

comp_to_sim%>%
  mutate(sq_err = (simulated_total_curr - total_curr)^2)%>%
  summarise(mse = mean(sq_err))
  
comp_to_sim%>%
  summarize(mean_tc = mean(total_curr),
            max_tc = max(total_curr),
            mean_tc_sim = mean(simulated_total_curr),
            max_tc_sim = max(simulated_total_curr)) 

comp_to_sim %>%
  filter(execid == 4065) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr), color = 'green') +
  geom_line(aes(x= year,
                y=simulated_total_curr))

########################
## Sim. Median Salary  #
########################


#Como definimos el 'median salary' ? 

#Opcion 1 - Mediana de toda la muestra:

#Take median
median(comp_to_sim$total_curr)
#filter
comp_to_sim %>%
  filter(total_curr >= 0.3424) %>%
  filter(total_curr <= 0.3425)
#There are many guys - take one for comparison 
comp_to_sim %>%
  filter(execid == 4932) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr), color = 'green') +
  geom_line(aes(x= year,
                y=simulated_total_curr))

comp_to_sim %>%
  filter(execid == 4932) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr - simulated_total_curr), color = 'green')
#Nuestro modelo predice con margen de error de $100k. 
#Aqui podemos ver el error 
comp_to_sim %>%
  filter(total_curr >= 0.3424) %>%
  filter(total_curr <= 0.3425) %>% 
  mutate(sq_err = (simulated_total_curr - total_curr)^2)%>%
  summarise(mse = mean(sq_err))
#En promedio se equivoca en $20k dolares 
#(cuando las compensaciones son al rededor de $400k). 

#######
# Ahora mediana por market cap - 

#MSE by marke cap level 
comp_to_sim %>%
  group_by(cap_level) %>%
  mutate(sq_err = (simulated_total_curr - total_curr)^2)%>%
  summarise(med_tc = median(total_curr), var_tc = var(total_curr), 
            med_tc_sim = median(simulated_total_curr),
            var_tc_sim = var(simulated_total_curr),mse = mean(sq_err))
#Parece que nos equivocamos mas en el sector grande 
#Es congruente con la teoria???

#Small Cap -----

#filter
comp_to_sim %>%
  filter(cap_level == 'Small Capitalization') %>%
  filter(total_curr >= 0.319) %>%
  filter(total_curr <= 0.320)

#There are many guys - take one for comparison 
comp_to_sim %>%
  filter(execid == 15970) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr), color = 'green') +
  geom_line(aes(x= year,
                y=simulated_total_curr))

comp_to_sim %>%
  filter(execid == 15970) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr - simulated_total_curr), color = 'green')
#Nuestro modelo predice con margen de error de $100k. 
#Aqui podemos ver el error 
comp_to_sim %>%
  filter(total_curr >= 0.319) %>%
  filter(total_curr <= 0.320) %>% 
  mutate(sq_err = (simulated_total_curr - total_curr)^2)%>%
  summarise(mse = mean(sq_err))

#Medium Cap -----

#filter
comp_to_sim %>%
  filter(cap_level == 'Medium Capitalization') %>%
  filter(total_curr >= 0.39) %>%
  filter(total_curr <= 0.40)

#There are many guys - take one for comparison 
comp_to_sim %>%
  filter(execid == 1018) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr), color = 'green') +
  geom_line(aes(x= year,
                y=simulated_total_curr))

comp_to_sim %>%
  filter(execid == 1018) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr - simulated_total_curr), color = 'green')
#Nuestro modelo predice con margen de error de $100k. 
#Aqui podemos ver el error 
comp_to_sim %>%
  filter(cap_level == 'Medium Capitalization') %>%
  filter(total_curr >= 0.39) %>%
  filter(total_curr <= 0.40) %>% 
  mutate(sq_err = (simulated_total_curr - total_curr)^2)%>%
  summarise(mse = mean(sq_err))

#Large Cap -----

#filter
comp_to_sim %>%
  filter(cap_level == 'Large Capitalization') %>%
  filter(total_curr >= 0.40) %>%
  filter(total_curr <= 0.401)

#There are many guys - take one for comparison 
comp_to_sim %>%
  filter(execid == 29892) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr), color = 'green') +
  geom_line(aes(x= year,
                y=simulated_total_curr))

comp_to_sim %>%
  filter(execid == 29892) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr - simulated_total_curr), color = 'green')
#Nuestro modelo predice con margen de error de $100k. 
#Aqui podemos ver el error 
comp_to_sim %>%
  filter(cap_level == 'Large Capitalization') %>%
  filter(total_curr >= 0.40) %>%
  filter(total_curr <= 0.41) %>% 
  mutate(sq_err = (simulated_total_curr - total_curr)^2)%>%
  summarise(mse = mean(sq_err))





