library(tidyverse)
library(janitor)

dt <- read_csv("~/Documents/Datos/esi-2020---personas.csv")

dt2 <- dt %>% select(sexo,d1_monto) %>% drop_na


#10% trimmed boxplot 
dt2 %>% filter(d1_monto > 120157 & d1_monto < 1.6e6) %>% ggplot() + geom_boxplot(aes(x=d1_monto,colour=factor(sexo)))

## sample with 30 for each sex
set.seed(123)
dt2 %>% filter(d1_monto > 120157 & d1_monto < 1.6e6) %>% 
  slice_sample(n = 60, weight_by = ifelse(.$sexo == 1, 0.5,0.5)) -> dt_sample 

dt_sample %>% round() %>% group_by(sexo) %>% 
  summarise(
    n=n(),
    q1=quantile(d1_monto,0.25, type = 1),
    q2=quantile(d1_monto,0.5, type = 1),
    q3=quantile(d1_monto,0.75, type = 1)
  )

#dt_sample %>% round() %>% write_csv("ine_sueldo_sexo_n60.csv")

## count unique values (tabyl as an alternative to as.table)
dt_sample %>% 
  round() %>% 
  filter(sexo == 1) %>% 
  tabyl(d1_monto) %>%
  mutate(cum_n = cumsum(n), cum_p = cumsum(percent)) %>%
  round(., digits = 3) %>% 
  select(d1_monto, n, cum_n, percent, cum_p) -> sueldo_h

#write_csv(sueldo_h, "sueldo_hombre_n32.csv")

dt_sample %>% 
  round() %>% 
  filter(sexo == 2) %>% 
  tabyl(d1_monto) %>%
  mutate(cum_n = cumsum(n), cum_p = cumsum(percent)) %>%
  round(., digits = 3) %>% 
  select(d1_monto, n, cum_n, percent, cum_p) -> sueldo_m

#write_csv(sueldo_m, "sueldo_mujer_n28.csv")

## plot horas trabajadas (semanales) vs sueldo (mensual)
dt %>% 
  select(d1_monto,c2_1_3) %>% 
  drop_na() %>%
  filter(c2_1_3 < 75) %>%
  ggplot(data=.,aes(c2_1_3,d1_monto)) + geom_point(alpha=0.05,size=3) +
  geom_smooth()

## dividir d1_monto en 100 grupos (percentiles)
## despues, graficar por grupos el monto y horas trabajadas

limites_sueldo <- quantile(
    dt$d1_monto,probs=seq(from=0.02,to=1,by=.02),na.rm=TRUE,type=1,names = FALSE
)

dt %>% select(d1_monto,c2_1_3) %>% drop_na() %>% filter(c2_1_3 < 168) %>% 
  mutate(cat_monto=cut_number(d1_monto,n=10)) %>% group_by(cat_monto) %>% 
  summarise(sueldo = mean(d1_monto), horas = mean(c2_1_3))


