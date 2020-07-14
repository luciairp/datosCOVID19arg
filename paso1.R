library(tidyverse)
library(skimr)
library(lubridate)

guess_encoding("Covid19Casos.csv", n_max = 1500)

# readr::read_csv() no lee multibyte encoding... así que lo leo desde base

datos <- read.delim("Covid19Casos.csv",
           sep = ",",
           stringsAsFactors = FALSE,
           fileEncoding = 'UTF-16LE') %>% 
  as_tibble()

datos
skim(datos)

# problemas con variables que considera cadenas de caracteres y son otra cosa,
# y con supuestos número que son niveles de un factor.

# problema con meses, si menor a 1 año el valor en "edad" es meses y no años... 
# recodifico variable edad a años = 0 para menores de 1 año

datos <- mutate(datos, EDAD = ifelse(edad_años_meses == "Meses",0,edad))

# check-if-ok:
filter(datos,edad!=EDAD) %>% 
  select(edad, EDAD, edad_años_meses) %>% 
  view(datos)



# datos limpios -------------------------------------------------------------

limpia <- datos %>%
  mutate (ID = id_evento_caso, 
          SEXO = factor(sexo, unique(sexo)),
          EDAD = ifelse(edad_años_meses == "Meses",0,edad),
          SINTf = ymd(fecha_inicio_sintomas),
          INTf = ymd(fecha_internacion),
          CUIf = ymd(fecha_cui_intensivo),
          FALLf = ymd(fecha_fallecimiento), 
          cui = factor(cuidado_intensivo, unique(cuidado_intensivo)),
          fall = factor(fallecido, unique(fallecido)),
          arm = factor(asistencia_respiratoria_mecanica, unique(asistencia_respiratoria_mecanica)),
          PAISr = factor(residencia_pais_nombre,unique(residencia_pais_nombre)),
          PCIAr = factor(residencia_provincia_nombre, unique(residencia_provincia_nombre)),
          DPTOr = factor(residencia_departamento_nombre, unique(residencia_departamento_nombre)),
          PCIAcarga = factor(carga_provincia_nombre, unique(carga_provincia_nombre)),
          CLASIF = factor(CLASIFICACION, unique(CLASIFICACION)),
          CLASIFres = factor(clasificacion_resumen,unique(clasificacion_resumen)))%>% 
  select (ID, SEXO, EDAD, SINTf, INTf, CUIf, FALLf,cui,fall,arm,PAISr,PCIAr,PCIAcarga,DPTOr, CLASIF, CLASIFres)

skim(limpia)


# PROP ASINTOMÁTICOS ------------------------------------------------------

# en limpia hay casos descartados y confirmados
# me interesa evaluar cuántos de los confirmados fueron asintomáticos: proporción de asintomáticos

prop_asintom <- limpia %>% 
  filter(CLASIFres == 'Confirmado') %>% 
  mutate (sinSINTOMAS = is.na(SINTf)) %>% 
  group_by(PCIAcarga) %>% 
  summarise( n = n(),
             sinSINT = sum(sinSINTOMAS),
             prop_asint = sinSINT/n) 

filter(prop_asintom, n > 500) %>% 
ggplot(aes(prop_asint,fct_reorder(PCIAcarga,prop_asint),label = n))+
  geom_point()+
  geom_text(nudge_x = .07)+
  coord_cartesian(xlim = c(0,1))

# de los casos confirmados, 
# esta es la proporción de casos asintomáticos por provincia
# filtro para n xq pcias con muy pocos casos estudiados


# Período PREHOSPITALIZACIÓN ----------------------------------------------

# quiero ver el tiempo que le lleva a alguien sintomático la htalización
# base solo para casos CONHOSPITALIZACIÓN
# miro la diferencia entre la fecha inicio síntomas SINTf (declarada paciente)
# y fecha de hospitalización INTf con calculo de períodos de lubridate y división entera
# para obtener en unidad de días

tiempo_a_htal <- limpia %>% 
  filter(CLASIFres == 'Confirmado') %>% 
  filter (!is.na(INTf) & !is.na(SINTf)) %>% 
  mutate(t_int = (SINTf %--% INTf) %/% days(1)) %>% 
  filter (t_int < 15)

orden_pcias_n <- tiempo_a_htal %>% 
  group_by(PCIAcarga) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

give.n <- function(x){
  return(c(y = median(x)+0.5, label = length(x))) 
  # primera parte es posición, segunda es justamente n
}

ggplot(tiempo_a_htal, aes(t_int,fct_reorder(PCIAcarga,t_int)))+
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom="text")


# Período HOSP FALLECIDO --------------------------------------------------

tiempo_htal_fall <- limpia %>% 
  filter(CLASIFres == 'Confirmado') %>% 
  filter (!is.na(INTf) & !is.na(FALLf)) %>% 
  mutate(t_fall = (INTf %--% FALLf) %/% days(1))

ggplot(tiempo_htal_fall, aes(t_fall,fct_reorder(PCIAcarga,t_fall)))+
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom="text")
