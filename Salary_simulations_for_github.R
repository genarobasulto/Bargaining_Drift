
#########################
### CORRER ESTA PARTE ###
#########################


#################
#   Cargar librerias
#################


library(tidyverse) 
library(dplyr)
library(stargazer) #Tablas que se exportan a latex
library(sandwich)
library(plm) 
library(xtable)
#################
#   Cargar base de datos
#################

#Read Data Base for Regression
rm(list=ls())


#Genaro compu
compensations <- read.csv(file = 'df_compensation_Final2.csv')
names(compensations)

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
#salary, 
#total_curr, 
#tdc2

compensations <- compensations %>%
  mutate(f_w = f(total_curr)) %>%      #Aqui se puede cambiar
  mutate(f_yh = f(max_mkval)) %>%
  group_by(execid) %>% 
  mutate(Y = c(diff(f_w),0)) %>%
  mutate(X = (I*(mkvalt/max_mkval))*f_yh) %>%      #Aqui se puede cambiar
  filter(cap_level == "Small Capitalization")

unique(compensations$cap_level)
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
#stargazer(mod_mco, 
 #         mod_mco_ctr, 
  #        mod_within,
   #       mod_within_ctr, type = 'text') 
#Aqui se puede cambiar a type = 'text' para verlo impreso
#Aqui se puede cambiar a type = 'latex' para código para tabla

#################
#    Usar el modelo para predecir salarios futuros
################

#Tomar epsilon del modelo 'full' - Panel con controles
eps_hat = unname(mod_within_ctr$coefficients[1])

######################
# Simulate all comps.#
######################

#Filter only executives with at least 10 years in the database

compensations <- merge(compensations, count(group_by(compensations, execid, gvkey)), by = c('execid', 'gvkey'))

comp_to_sim <- compensations %>%
  ungroup() %>%
  filter(n >= 5) %>%
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

mean(comp_to_sim$total_curr)

mean_exec <- comp_to_sim %>%
  group_by(execid) %>%
  summarize(sq_err = sum((simulated_total_curr - total_curr)^2), 
            total_curr_m = mean(total_curr)) %>%
  filter(total_curr_m >= mean(comp_to_sim$total_curr)*0.97) %>%
  filter(total_curr_m <= mean(comp_to_sim$total_curr)*1.03) %>%
  arrange(sq_err) 

mean_exec

comp_to_sim %>%
  filter(execid == as.numeric(mean_exec[1,1]))  %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = total_curr), color = 'blue') +
  geom_line(aes(x= year,
                y=simulated_total_curr)) + 
  ylab('total_curr') + 
  geom_text(aes(x = max(year)-0.5, y = 0.95*total_curr[length(total_curr)]) ,label = "Real", color = 'blue', size = 6) +
  geom_text(aes(x = max(year)-1, y = 1.05*simulated_total_curr[length(total_curr)]) ,label = "Simulated", color = 'black', size = 6)+
  theme_bw()

#ggsave('Simulation_Figs/total_curr_05_small_medium_cap.png', width = 21, height = 13, units = "cm")


comp_to_sim %>%
  ggplot() +
  geom_histogram(aes(x = total_curr, y = ..density..), fill = "blue", alpha = 0.5, bins = 25) +
  geom_density(aes(x = total_curr), color = "blue", alpha = 0.8) +
  geom_histogram(aes(x = simulated_total_curr, y = ..density..), alpha = 0.5, bins = 25) +
  geom_density(aes(x = simulated_total_curr), alpha = 0.5) +
  theme_bw()
ggsave('Simulation_Figs/histogram_total_curr_05_small.png', width = 21, height = 13, units = "cm")

#MSE by market cap level 
comp_to_sim %>%
  group_by(cap_level) %>%
  mutate(sq_err = (simulated_total_curr - total_curr)^2)%>%
  summarise(med_tc = mean(total_curr), var_tc = var(total_curr), 
            med_tc_sim = mean(simulated_total_curr),
            var_tc_sim = var(simulated_total_curr),
            mse = mean(sq_err), 
            t_test = t.test(total_curr, simulated_total_curr)$p.value, 
            obs. = n()) %>%
  xtable(digits = 4)

comp_to_sim %>%
  group_by(year, cap_level) %>%
  filter(n()>1) %>%
  summarise(t_test = t.test(total_curr, simulated_total_curr)$p.value)%>%
  ungroup() %>%
  filter(year>1999) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = t_test, color = cap_level), show.legend = TRUE) +
  geom_hline(yintercept = 0.05)+ ylab("p-value")+
  theme_bw()+labs(color = "Capitalization")


kstest_res <- ad.test(total_curr ~ simulated_total_curr, data = comp_to_sim[1:50,], method = "asymptotic")



ggsave('Simulation_Figs/ttest_f05_tdc2.pdf', width = 20, height = 10, units = "cm")


########################
## Sim. Median Salary  #
########################


#Como definimos el 'median salary' ? 

#Opcion 1 - Mediana de toda la muestra:

#Take median
median(comp_to_sim$tdc2)
#filter
comp_to_sim %>%
  filter(tdc2 >= 0.3424) %>%
  filter(tdc2 <= 0.3425)
#There are many guys - take one for comparison 
pdf("tdc2_05.pdf")
comp_to_sim %>%
  filter(execid == 4932) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = tdc2), color = "blue", show.legend = TRUE) +
  geom_line(aes(x= year,
                y=simulated_tdc2), color = 'black', show.legend =  TRUE) + 
  geom_text(aes(x = 2015, y = 0.34) ,label = "Real", color = 'blue', size = 6) +
  geom_text(aes(x = 2014, y = 0.45) ,label = "Simulated", color = 'black', size = 6) +
  theme_bw()
dev.off()
comp_to_sim %>%
  filter(execid == 4932) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = tdc2 - simulated_tdc2), color = 'green')
#Nuestro modelo predice con margen de error de $100k. 
#Aqui podemos ver el error 
comp_to_sim %>%
  filter(tdc2 >= 0.3424) %>%
  filter(tdc2 <= 0.3425) %>% 
  mutate(sq_err = (simulated_tdc2 - tdc2)^2)%>%
  summarise(mse = mean(sq_err))
#En promedio se equivoca en $20k dolares 
#(cuando las compensaciones son al rededor de $400k). 

#######
# Ahora mediana por market cap - 

#MSE by market cap level 
comp_to_sim %>%
  group_by(cap_level) %>%
  mutate(sq_err = (simulated_tdc2 - tdc2)^2)%>%
  summarise(med_tc = mean(tdc2), var_tc = var(tdc2), 
            med_tc_sim = mean(simulated_tdc2),
            var_tc_sim = var(simulated_tdc2),
            mse = mean(sq_err), 
            t_test = t.test(tdc2, simulated_tdc2)$p.value, 
            obs. = n()) %>%
  xtable(digits = 4)
comp_to_sim %>%
  mutate(sq_err = (simulated_salary - salary)^2)%>%
  summarise(med_tc = median(salary), var_tc = var(salary), 
            med_tc_sim = median(simulated_salary),
            var_tc_sim = var(simulated_salary),
            mse = mean(sq_err), 
            t_test = t.test(salary, simulated_salary)$p.value) %>%
  xtable(digits = 4)
#Observamos que los test de diferencia de medias no son significativos 

#Necesitamos normalidad? 

comps_ks <- comp_to_sim[!duplicated(comp_to_sim$salary), ]

ks.test(comps_ks$salary, 'pnorm', mean(comps_ks$salary), 
        var(comps_ks$salary), alternative= 'two.sided')
comps_ks <- comps_ks[!duplicated(comps_ks$simulated_salary), ]
ks.test(comps_ks$salary, comps_ks$simulated_salary, 
        alternative= 'two.sided')


#By year? 

large <- comp_to_sim %>%
  filter(cap_level == 'Large Capitalization') %>%
  filter(year == 2015)
t.test(large$total_curr, large$simulated_total_curr,
        alternative = "two.sided")




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





#Read Data Base for Regression
rm(list=ls())

#Genaro compu
compensations <- read.csv(file = 'df_compensation_Final2.csv')
names(compensations)

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
  mutate(f_w = f(salary)) %>%      #Aqui se puede cambiar
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

#################
#    Usar el modelo para predecir salarios futuros
################

#Tomar epsilon del modelo 'full' - Panel con controles
eps_hat = unname(mod_within_ctr$coefficients[1])

######################
# Simulate all comps.#
######################

#Filter only executives with at least 10 years in the database

compensations <- merge(compensations, count(group_by(compensations, execid, gvkey)), by = c('execid', 'gvkey'))

comp_to_sim <- compensations %>%
  ungroup() %>%
  filter(n >= 10) %>%
  select(c("execid", "year","cap_level",  "salary", "mkvalt","max_mkval", "I", 
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
  sim_comp = c(data$salary[1])
  ny = data$n[1]
  yh = data$max_mkval[1]
  for(k in 1:(ny-1)){
    sim_comp[k+1] = (max(0,f(sim_comp[k]) + eps_hat*data$I[k]*(data$mkvalt[k]/yh)*f(yh)))^(1/r)
  }
  #print(data$execid[1])
  Sim_Comps <- c(Sim_Comps, sim_comp)
}

comp_to_sim$simulated_salary <- Sim_Comps

comp_to_sim%>%
  mutate(sq_err = (simulated_salary - salary)^2)%>%
  summarise(mse = mean(sq_err))

comp_to_sim%>%
  summarize(mean_salary = mean(salary),
            max_salary = max(salary),
            mean_salary_sim = mean(simulated_salary),
            max_salary_sim = max(simulated_salary)) 

comp_to_sim %>%
  filter(execid == 4065) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = salary), color = 'green') +
  geom_line(aes(x= year,
                y=simulated_salary))


comp_to_sim %>%
  group_by(year, cap_level) %>%
  filter(n()>1) %>%
  summarise(t_test = t.test(salary, simulated_salary)$p.value)%>%
  ungroup() %>%
  filter(year>1999) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = t_test, color = cap_level), show.legend = TRUE) +
  geom_hline(yintercept = 0.05)+ 
  ggtitle("Mean difference test. f(w)=w^(0.1)") + ylab("p-value")


ggsave('ttest_f01_salary.pdf', width = 20, height = 10, units = "cm")



########################
## Sim. Median salary  #
########################


#Como definimos el 'median salary' ? 

#Opcion 1 - Mediana de toda la muestra:

#Take median
median(comp_to_sim$salary)
#filter
comp_to_sim %>%
  filter(salary >= 0.29) %>%
  filter(salary <= 0.30)
#There are many guys - take one for comparison 
pdf("salary_01.pdf")
comp_to_sim %>%
  filter(execid == 1018) %>%
  ggplot() +
  geom_line(aes(x = year , 
                y = salary), color = "blue", show.legend = TRUE) +
  geom_line(aes(x= year,
                y=simulated_salary), color = 'black', show.legend =  TRUE) + 
  scale_color_discrete(labels = c("Simulated" ,"Real")) + 
  geom_text(aes(x = 2012, y = 0.3) ,label = "Real", color = 'blue', size = 6) +
  geom_text(aes(x = 2012, y = 0.25) ,label = "Simulated", color = 'black', size = 6) + 
  theme_bw()
dev.off()
#Aqui podemos ver el error 
comp_to_sim %>%
  filter(salary >= 0.00029) %>%
  filter(salary <= 0.0003) %>% 
  mutate(sq_err = (simulated_salary - salary)^2)%>%
  summarise(mse = mean(sq_err))
#En promedio se equivoca en $20k dolares 
#(cuando las compensaciones son al rededor de $400k). 

#######
# Ahora mediana por market cap - 

#MSE by market cap level 
comp_to_sim %>%
  group_by(cap_level) %>%
  mutate(sq_err = (simulated_salary - salary)^2)%>%
  summarise(med_tc = median(salary), var_tc = var(salary), 
            med_tc_sim = median(simulated_salary),
            var_tc_sim = var(simulated_salary),
            mse = mean(sq_err), 
            t_test = t.test(salary, simulated_salary)$p.value) #%>%
  #xtable(digits = 4)
comp_to_sim %>%
  mutate(sq_err = (simulated_salary - salary)^2)%>%
  summarise(med_tc = median(salary), var_tc = var(salary), 
            med_tc_sim = median(simulated_salary),
            var_tc_sim = var(simulated_salary),
            mse = mean(sq_err), 
            t_test = t.test(salary, simulated_salary)$p.value)# %>%
  #xtable(digits = 4)

