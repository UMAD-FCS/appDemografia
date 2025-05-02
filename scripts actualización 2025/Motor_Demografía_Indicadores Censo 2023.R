
## -------------------------------------------------------------------------  ##
##                         Observatorio Uruguay                               ##
##                          Módulo demografía                                 ##
##                        Indicadores Censo 2023                              ##
## -------------------------------------------------------------------------  ##

library(rio)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Carga de bases de datos
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

#dfv <- rio::import("Bases/viviendas_ext_26_02.csv")
#dfh <- rio::import("Bases/hogares_ext_26_02.csv")


#dfp <- rio::import("C:/Users/Shari/Dropbox/UMAD/DESCA/Convenio 2025/Procesamiento Censo/Bases/personas_ampliada_ext_26_02.csv")
dfp <- rio::import("Bases/personas_ampliada_ext_26_02.csv")
#df2011 <- rio::import("Bases/población con actividad_2011.dta") # Para chequeos
motor <- rio::import("bases/Base_Motor_Demografica_rev2025.xlsx")
rraa <- rio::import("Demografía/Actualización_Registros Administrativos_2025.xlsx")
codigopais <- rio::import("Bases/Paises_Censo2023.xlsx") 
proydptos <- rio::import("Bases/proy_dptos.xlsx")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Objetos auxiliares para construcción de base motor
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

PAIS   <- "Uruguay"
FECHA  <- "2023"
FUENTE <- "INE Censos de Población"
RESPONSABLE <- "J.Pandolfi"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Transformaciones a la base
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Edad quinquenal

dfp <- dfp %>% dplyr::mutate(quinq =    case_when(PERNA01 >= 0 & PERNA01 <= 4 ~ "0-4 años",
                                                  PERNA01 >= 5 & PERNA01 <= 9 ~ "5-9 años",
                                                  PERNA01 >= 10 & PERNA01 <= 14 ~ "10-14 años",
                                                  PERNA01 >= 15 & PERNA01 <= 19 ~ "15-19 años",
                                                  PERNA01 >= 20 & PERNA01 <= 24 ~ "20-24 años",
                                                  PERNA01 >= 25 & PERNA01 <= 29 ~ "25-29 años",
                                                  PERNA01 >= 30 & PERNA01 <= 34 ~ "30-34 años",
                                                  PERNA01 >= 35 & PERNA01 <= 39 ~ "35-39 años",
                                                  PERNA01 >= 40 & PERNA01 <= 44 ~ "40-44 años",
                                                  PERNA01 >= 45 & PERNA01 <= 49 ~ "45-49 años",
                                                  PERNA01 >= 50 & PERNA01 <= 54 ~ "50-54 años",
                                                  PERNA01 >= 55 & PERNA01 <= 59 ~ "55-59 años",
                                                  PERNA01 >= 60 & PERNA01 <= 64 ~ "60-64 años",
                                                  PERNA01 >= 65 & PERNA01 <= 69 ~ "65-69 años",
                                                  PERNA01 >= 70 & PERNA01 <= 74 ~ "70-74 años",
                                                  PERNA01 >= 75 & PERNA01 <= 79 ~ "75-79 años",
                                                  PERNA01 >= 80 & PERNA01 <= 84 ~ "80-84 años",
                                                  PERNA01 >= 85 & PERNA01 <= 89 ~ "85-89 años",
                                                  PERNA01 >= 90 & PERNA01 <= 94 ~ "90-94 años",
                                                  PERNA01 >= 95 & PERNA01 <= 99 ~ "95-99 años",
                                                  PERNA01 >= 100 & PERNA01 <= 104 ~ "100-104 años",
                                                  PERNA01 >= 105 & PERNA01 <= 109 ~ "105-109 años"))



# Código identificador del hogar

dfp <- dfp %>%
  mutate(NUMERO = paste(DIRECCION_ID, VIVID, HOGID))


# Cantidad de personas en el hogar sin servicio doméstico

dfp <- dfp %>%
  group_by(NUMERO) %>% 
  mutate(HT19 = sum(!(PERPA01 %in% c(14, 15)))) %>% 
  ungroup() 

#dfp <- dfp %>% 
#  dplyr::mutate(prueba = HOGPR01_CON_RRAA - HT19)

#(dfp$prueba, dfp$PERPA01)
#table(dfp$prueba, dfp$VIVVO00)
#table(dfp$prueba[!(dfp$VIVVO00 %in% 2)])

#prueba <- dfp %>%
#  filter(VIVVO00 == 1 & prueba != 0) %>%
#  arrange(NUMERO, PERPA01) %>%
#  select(NUMERO, PERPA01, HT19, HOGPR01_CON_RRAA) # confirmo que HOGPR01_CON_RRAA incluye al servicio doméstico

                
# NBI - Hacinamiento
dfp <- dfp %>% 
  dplyr::mutate(aux1 = HT19/HOGHD00,  # habitaciones residenciales
                aux2 = HT19/HOGHD01, # habitaciones para domir
                bd_hacinamiento1  = ifelse(aux1>2, 1, 0),
                bd_hacinamiento2  = ifelse(aux2>2, 1, 0))


# NBI - Materialidad vivienda
dfp <- dfp %>% 
  dplyr::mutate(NBI_materialidad11 = ifelse(VIVDV01 == 6 | VIVDV02 == 5 | VIVDV03 == 4, 1, 0))


# NBI - Espacio apropiado para cocinar
dfp <- dfp %>% 
  dplyr::mutate(NBI_cocina11 = ifelse(HOGSC01 == 3, 1, 0))


# NBI - Abastecimiento de agua potable
dfp <- dfp %>% 
  dplyr::mutate(bd_d11 = case_when(VIVDV05 == 1 ~ 1, 
                                   VIVDV05==2 | VIVDV05==3 ~ 2, 
                                   VIVDV05>=4 & VIVDV05<=7 ~ 3))

dfp <- dfp %>% 
  dplyr::mutate(bd_d12 = case_when(VIVDV06 == 1 ~ 1, 
                                   VIVDV06 == 2 | VIVDV06 == 3 ~ 2, 
                                   VIVDV06 == 4 ~ 3))

dfp <- dfp %>% 
  dplyr::mutate(NBI_agua11 = ifelse((bd_d12 == 2 | 
                                     bd_d12 == 3) | 
                                     bd_d11 == 3, 1, 0))



# NBI - Servicio higiénico de calidad

dfp <- dfp %>% 
  dplyr::mutate(bd_d16 = as.numeric(case_when(HOGSH03 == 0 ~ 0,
                                              HOGSH03 == 1 ~ 1, 
                                              HOGSH03 == 2 ~ 2, 
                                              HOGSH03 >= 3 & HOGSH03 <= 4 ~ 3)))

dfp <- dfp %>% 
  dplyr::mutate(NBI_servhigien11 = case_when(
    HOGSH01 == 3 | 
      ((HOGSH01 == 1 | HOGSH01 == 2) & HOGSH02 == 2) | 
       (HOGSH03 == 3 | HOGSH03 == 4) ~ 1,
    HOGSH02 == 1 & (HOGSH01 == 1 | HOGSH01 == 2) & (HOGSH03 == 1 | HOGSH03 == 2) ~ 0, 
    TRUE ~ 99 ))
          

# NBI - Energía eléctrica (No relevado)


# NBI - Artefactos básicos de cofnort

# Calefón y calentador de agua se relevan en una única variable

dfp <- dfp %>% 
  dplyr::mutate(
    NBI_artefactos11 = ifelse(HOGCA01 == 9 | HOGCE27 == 2 | HOGCE03 == 2, 1, 0))


# NBI - Educación

dfp <- dfp %>% dplyr::mutate(niños_na = case_when(
  PERNA01 <  4 | PERNA01 >  17 ~ 0,
  PERNA01 >= 4 & PERNA01 <= 17 & (PERED00 >=1 & PERED00<= 3) ~ 0,
  PERNA01 >= 4 & PERNA01 <= 17 & (PERED01 >=1 & PERED01<= 2) ~ 0,
  
  PERNA01 >= 4 & PERNA01 <= 17 & (PERED01 == 3 | PERED01 == 4) & 
    (PERED03_1 == 14 | PERED04 == 1) ~ 0,  
  
  PERNA01 >= 4 & PERNA01 <= 17 & (PERED01 == 3 | PERED01 == 4) & PERED00 >=4 &
    ((PERED03_1 == 14 & PERED04 != 1) | PERED03_1 != 14) ~ 1))
  
 

base_niños_na <- dfp[,c("NUMERO","niños_na")]
base_niños_na <- base_niños_na %>% dplyr::mutate(NBI_educacion11 = niños_na)
base_niños_na <- base_niños_na[order(base_niños_na$NUMERO, 
                                     base_niños_na$NBI_educacion11, 
                                     decreasing = TRUE), ]
base_niños_na <- dplyr::distinct(base_niños_na, NUMERO, .keep_all = TRUE)
base_niños_na <- base_niños_na[,c("NUMERO","NBI_educacion11")]

dfp <- merge(dfp, base_niños_na, by = "NUMERO")



# NBI Total
dfp <- dfp %>% 
  dplyr::mutate(NBI_vivienda11= ifelse(bd_hacinamiento1 == 1 | 
                                         NBI_materialidad11 == 1 | 
                                         NBI_cocina11 == 1, 1, 0))
dfp <- dfp %>% 
  dplyr::mutate(NBI_2011= ifelse(NBI_vivienda11 == 1 | 
                                   NBI_agua11 == 1 | 
                                   NBI_servhigien11 == 1 | 
                                   NBI_artefactos11 == 1 | 
                                   NBI_educacion11 == 1, 1, 0))

dfp <- dfp %>% 
  dplyr::mutate(NBI_cant = NBI_vivienda11 + 
                  NBI_agua11 + 
                  NBI_servhigien11 + 
                  NBI_artefactos11 + 
                  NBI_educacion11)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Procesamiento de indicadores
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# ~~~ Población total censada (en miles de personas) (ver comentarios Martin Koolhaas)

VALOR <- table(dfp$FUENTE_EXT)
VALOR <- VALOR[1]

CODIND <- "p72"
NOMINDICADOR <- "Población total censada"
EDAD <- "Total"
SEXO <- "Total"
DEPARTAMENTO_UY <- "NA"

i01 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA, VALOR, RESPONSABLE, FUENTE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Población por edades quinquenales

# Para chequear que la serie se calcula con población estimada:
# df2011 <- df2011 %>% dplyr::mutate(quinq = case_when(perna01 >= 0 & perna01 <= 4 & perph02 == 2 & ma == 0 ~ 1))

# Hay que tomar decisión a partir de la "anonimización" de los datos publicados. 
# Hay dos variables: 1) Edad simple (PERNA01), solo disponible para quienes residen 
# en localidades de más de 10.000 habitantes; o 2) Edad quinquenal (PERNA01_TRAMO), se trunca en +80. 
# Ninguna de las dos permite réplica en relación a la serie. Se opta por trabajar con (1). 

i02 <- as.matrix(
  as.data.frame(
    table(dfp$quinq)))

colnames(i02) <- c("EDAD","VALOR")


CODIND <- "p5"
NOMINDICADOR <- "Población por edades quinquenales"
SEXO <- "Total"
DEPARTAMENTO_UY <- "Total"

i02 <- cbind(CODIND, NOMINDICADOR, SEXO, PAIS, DEPARTAMENTO_UY, FECHA, i02, RESPONSABLE, FUENTE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Población por edades quinquenales y sexo

crear_tabla <- function(dfp, perph_value, sexo_label) {
  VALOR <- dfp %>%
    filter(PERPH02 == perph_value) %>%
    .$quinq %>%
    table()
  
  SEXO <- rep(sexo_label, length(VALOR))
  EDAD <- names(VALOR)
  
  VALOR <- cbind(VALOR, SEXO, EDAD)
  return(VALOR)
}

i03_m <- crear_tabla(dfp, 2, "Mujeres")
i03_v <- crear_tabla(dfp, 1, "Varones")

i03 <- rbind(i03_m, i03_v)

CODIND <- "p5"
NOMINDICADOR <- "Población por edades quinquenales"
DEPARTAMENTO_UY <- "Total"

i03 <- cbind(CODIND, NOMINDICADOR, PAIS, DEPARTAMENTO_UY, FECHA, i03, RESPONSABLE, FUENTE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Población departamental (censos) 

i04 <- as.matrix(
  as.data.frame(
    table(dfp$DEPARTAMENTO)))

colnames(i04) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p3"
NOMINDICADOR <- "Población departamental"
EDAD <- "Total"
SEXO <- "Total"

i04 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i04, RESPONSABLE, FUENTE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Población departamental - porcentaje (censos) 

i05 <- dfp %>%
  count(DEPARTAMENTO) %>%                    
  mutate(VALOR = prop.table(n) * 100) %>%
  mutate(VALOR = format(VALOR, decimal.mark = ",")) %>%
  select(DEPARTAMENTO, VALOR) %>%        
  as.matrix()
colnames(i05) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p4"
NOMINDICADOR <- "Porcentaje de población departamental sobre total de población"
EDAD <- "Total"
SEXO <- "Total"

i05 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i05, RESPONSABLE, FUENTE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Población  departamental censada por edades quinquenales y sexo

crear_tabla <- function(dfp, perph_value, dep_value, sexo_label) {
  tabla <- dfp %>%
    filter(PERPH02 == perph_value & DEPARTAMENTO == dep_value) %>%
    .$quinq %>%
    table()
  
  SEXO <- rep(sexo_label, length(tabla))
  DEPARTAMENTO_UY <- rep(dep_value, length(tabla))
  
  tabla <- as.data.frame(tabla) %>%
    as.matrix() %>%
    cbind(SEXO, DEPARTAMENTO_UY)
  
  return(tabla)
}

resultados <- list()

for (dep in 1:19) {
  i06_m <- crear_tabla(dfp, 2, dep, "Mujeres")
  i06_v <- crear_tabla(dfp, 1, dep, "Varones")
  
  resultados[[paste0("i06_m_", dep)]] <- i06_m
  resultados[[paste0("i06_v_", dep)]] <- i06_v
}

i06 <- do.call(rbind, resultados)
colnames(i06) <- c("EDAD","VALOR","SEXO","DEPARTAMENTO_UY")


CODIND <- "p5"
NOMINDICADOR <- "Población  departamental censada por edades quinquenales y sexo"

i06 <- cbind(CODIND, NOMINDICADOR, PAIS, FECHA, i06, RESPONSABLE, FUENTE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas afrodescendientes con al menos una NBI

i07 <- dfp %>%
  filter(as.numeric(PERER01_1) == 1) %>%  
  mutate(NBI_2011 = as.numeric(NBI_2011)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_2011, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i07 <- i07 %>% select(-mean_NBI)
colnames(i07) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p40"
NOMINDICADOR <- "Porcentaje de personas afrodescendientes con al menos una NBI"
EDAD <- "NA"
SEXO <- "NA"

i07 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i07, RESPONSABLE, FUENTE)

i07t <- dfp %>%
  filter(as.numeric(PERER01_1) == 1) %>%  
  mutate(NBI_2011 = as.numeric(NBI_2011)) %>%  
  summarise(mean_NBI = mean(NBI_2011, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i07t <- i07t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"

i07t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i07t, RESPONSABLE, FUENTE)

i07 <- rbind(i07,i07t)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas no afrodescendientes con al menos una NBI


i08 <- dfp %>%
  filter(as.numeric(PERER01_1) == 2) %>%  
  mutate(NBI_2011 = as.numeric(NBI_2011)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_2011, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i08 <- i08 %>% select(-mean_NBI)
colnames(i08) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p41"
NOMINDICADOR <- "Porcentaje de personas no afrodescendientes con al menos una NBI"
EDAD <- "NA"
SEXO <- "NA"

i08 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i08, RESPONSABLE, FUENTE)

i08t <- dfp %>%
  filter(as.numeric(PERER01_1) == 2) %>%  
  mutate(NBI_2011 = as.numeric(NBI_2011)) %>%  
  summarise(mean_NBI = mean(NBI_2011, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i08t <- i08t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"

i08t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i08t, RESPONSABLE, FUENTE)

i08 <- rbind(i08,i08t)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas entre 0 y 14 años con al menos una NBI


i14 <- dfp %>%
  filter(PERNA01>= 0 & PERNA01 <= 14) %>%  
  mutate(NBI_2011 = as.numeric(NBI_2011)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_2011, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i14 <- i14 %>% select(-mean_NBI)
colnames(i14) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p42"
NOMINDICADOR <- "Porcentaje de personas entre 0 y 14 años con al menos una NBI"
EDAD <- "NA"
SEXO <- "NA"

i14 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i14, RESPONSABLE, FUENTE)

i14t <- dfp %>%
  filter(PERNA01>= 0 & PERNA01 <= 14) %>%  
  mutate(NBI_2011 = as.numeric(NBI_2011)) %>%  
  summarise(mean_NBI = mean(NBI_2011, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i14t <- i14t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i14t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i14t, RESPONSABLE, FUENTE)

i14 <- rbind(i14,i14t)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas con NBI en Vivienda decorosa


i15 <- dfp %>%
  mutate(NBI_vivienda11 = as.numeric(NBI_vivienda11)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_vivienda11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i15 <- i15 %>% select(-mean_NBI)
colnames(i15) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p43"
NOMINDICADOR <- "Porcentaje de personas con NBI en Vivienda decorosa"
EDAD <- "NA"
SEXO <- "NA"

i15 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i15, RESPONSABLE, FUENTE)

i15t <- dfp %>%
  mutate(NBI_vivienda11 = as.numeric(NBI_vivienda11)) %>%  
  summarise(mean_NBI = mean(NBI_vivienda11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i15t <- i15t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i15t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i15t, RESPONSABLE, FUENTE)

i15 <- rbind(i15,i15t)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas con NBI en abastecimiento de agua potable


i16 <- dfp %>%
  mutate(NBI_agua11 = as.numeric(NBI_agua11)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_agua11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i16 <- i16 %>% select(-mean_NBI)
colnames(i16) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p44"
NOMINDICADOR <- "Porcentaje de personas con NBI en abastecimiento de agua potable"
EDAD <- "NA"
SEXO <- "NA"

i16 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i16, RESPONSABLE, FUENTE)

i16t <- dfp %>%
  mutate(NBI_agua11 = as.numeric(NBI_agua11)) %>%  
  summarise(mean_NBI = mean(NBI_agua11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i16t <- i16t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i16t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i16t, RESPONSABLE, FUENTE)

i16 <- rbind(i16,i16t)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas con NBI en servicio higiénico


i17 <- dfp %>%
  filter(NBI_servhigien11 != 99) %>%  
  mutate(NBI_servhigien11 = as.numeric(NBI_servhigien11)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_servhigien11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i17 <- i17 %>% select(-mean_NBI)
colnames(i17) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p45"
NOMINDICADOR <- "Porcentaje de personas con NBI en servicio higiénico"
EDAD <- "NA"
SEXO <- "NA"

i17 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i17, RESPONSABLE, FUENTE)

i17t <- dfp %>%
  filter(NBI_servhigien11 != 99) %>%  
  summarise(mean_NBI = mean(NBI_servhigien11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i17t <- i17t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i17t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i17t, RESPONSABLE, FUENTE)

i17 <- rbind(i17,i17t)



#Porcentaje de personas con NBI en energía eléctrica: no relevado

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas con NBI en artefactos básicos de confort


i18 <- dfp %>%
  mutate(NBI_artefactos11 = as.numeric(NBI_artefactos11)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_artefactos11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i18 <- i18 %>% select(-mean_NBI)
colnames(i18) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p47"
NOMINDICADOR <- "Porcentaje de personas con NBI en artefactos básicos de confort"
EDAD <- "NA"
SEXO <- "NA"

i18 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i18, RESPONSABLE, FUENTE)

i18t <- dfp %>%
  mutate(NBI_artefactos11 = as.numeric(NBI_artefactos11)) %>%  
  summarise(mean_NBI = mean(NBI_artefactos11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i18t <- i18t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i18t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i18t, RESPONSABLE, FUENTE)

i18 <- rbind(i18,i18t)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas con NBI en educación


i19 <- dfp %>%
  mutate(NBI_educacion11 = as.numeric(NBI_educacion11)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_educacion11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i19 <- i19 %>% select(-mean_NBI)
colnames(i19) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p48"
NOMINDICADOR <- "Porcentaje de personas con NBI en educación"
EDAD <- "NA"
SEXO <- "NA"

i19 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i19, RESPONSABLE, FUENTE)

i19t <- dfp %>%
  mutate(NBI_educacion11 = as.numeric(NBI_educacion11)) %>%  
  summarise(mean_NBI = mean(NBI_educacion11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i19t <- i19t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i19t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i19t, RESPONSABLE, FUENTE)

i19 <- rbind(i19,i19t)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas con NBI en materiales de la vivienda


i20 <- dfp %>%
  mutate(NBI_materialidad11 = as.numeric(NBI_materialidad11)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_materialidad11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i20 <- i20 %>% select(-mean_NBI)
colnames(i20) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p49"
NOMINDICADOR <- "Porcentaje de personas con NBI en materiales de la vivienda"
EDAD <- "NA"
SEXO <- "NA"

i20 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i20, RESPONSABLE, FUENTE)

i20t <- dfp %>%
  mutate(NBI_materialidad11 = as.numeric(NBI_materialidad11)) %>%  
  summarise(mean_NBI = mean(NBI_materialidad11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i20t <- i20t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i20t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i20t, RESPONSABLE, FUENTE)

i20 <- rbind(i20,i20t)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas con NBI en hacinamiento


i21 <- dfp %>%
  mutate(bd_hacinamiento1 = as.numeric(bd_hacinamiento1)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(bd_hacinamiento1, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i21 <- i21 %>% select(-mean_NBI)
colnames(i21) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p50"
NOMINDICADOR <- "Porcentaje de personas con NBI en hacinamiento"
EDAD <- "NA"
SEXO <- "NA"

i21 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i21, RESPONSABLE, FUENTE)

i21t <- dfp %>%
  mutate(bd_hacinamiento1 = as.numeric(bd_hacinamiento1)) %>%  
  summarise(mean_NBI = mean(bd_hacinamiento1, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i21t <- i21t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i21t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i21t, RESPONSABLE, FUENTE)

i21 <- rbind(i21,i21t)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Porcentaje de personas con NBI en cocina


i22 <- dfp %>%
  mutate(NBI_cocina11 = as.numeric(NBI_cocina11)) %>%  
  group_by(DEPARTAMENTO) %>%  
  summarise(mean_NBI = mean(NBI_cocina11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))

i22 <- i22 %>% select(-mean_NBI)
colnames(i22) <- c("DEPARTAMENTO_UY","VALOR")

CODIND <- "p51"
NOMINDICADOR <- "Porcentaje de personas con NBI en cocina"
EDAD <- "NA"
SEXO <- "NA"

i22 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, PAIS, FECHA, i22, RESPONSABLE, FUENTE)

i22t <- dfp %>%
  mutate(NBI_cocina11 = as.numeric(NBI_cocina11)) %>%  
  summarise(mean_NBI = mean(NBI_cocina11, na.rm = TRUE)) %>%  
  mutate(VALOR = (mean_NBI * 100))
i22t <- i22t %>% select(-mean_NBI)

DEPARTAMENTO_UY <- "Total"
FUENTE <- "INE Censos de Población"

i22t <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA,i22t, RESPONSABLE, FUENTE)

i22 <- rbind(i22,i22t)

##Se relevan juntos, sería como nuevo indicador, ver luego!
#Porcentaje de personas con NBI en calefacción
#Porcentaje de personas con NBI en conservación de alimentos
#Porcentaje de personas con NBI en calentador de agua para el baño




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Distribución porcentual de personas nacidas en el exterior según país de nacimiento

i9 <- dfp %>%
  filter(PERMI01 == 4) %>% 
  count(PERMI01_4) %>%                    
  mutate(VALOR = prop.table(n) * 100) %>%
  mutate(VALOR = format(VALOR, decimal.mark = ",")) %>%
  select(PERMI01_4, VALOR) %>%        
  as.matrix()

colnames(i9) = c("País_codigo", "VALOR")

i9 <- as.data.frame(i9)
i9 <- i9 %>% mutate(País_codigo = as.numeric(País_codigo))
codigopais <- codigopais %>% mutate(País_codigo = as.numeric(País_codigo))

i9 <- merge(i9, codigopais, by = "País_codigo")
i9 <- i9 %>% select(-"País_codigo")

colnames(i9) <- c("VALOR", "PAIS")


CODIND <- "p55"
NOMINDICADOR <- "Distribución porcentual de personas nacidas en el exterior según país de nacimiento"
EDAD <- "Total"
SEXO <- "Total"
DEPARTAMENTO_UY <- "NA"

i09 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, FECHA, i9, RESPONSABLE, FUENTE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Población nacida en el exterior según país de nacimiento

i10 <- dfp %>%
  filter(PERMI01 == 4) %>% 
  count(PERMI01_4)

colnames(i10) = c("País_codigo", "VALOR")

i10 <- as.data.frame(i10)
i10 <- i10 %>% mutate(País_codigo = as.numeric(País_codigo))
codigopais <- codigopais %>% mutate(País_codigo = as.numeric(País_codigo))

i10 <- merge(i10, codigopais, by = "País_codigo")
i10 <- i10 %>% select(-"País_codigo")

colnames(i10) <- c("VALOR", "PAIS")

CODIND <- "p56"
NOMINDICADOR <- "Población nacida en el exterior según país de nacimiento"
EDAD <- "Total"
SEXO <- "Total"
DEPARTAMENTO_UY <- "NA"

i10 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, FECHA, i10, RESPONSABLE, FUENTE)




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Tasa inmigración departamental interna

#Población media
proydptos <- proydptos %>%
  mutate(
    pobmedia = ((proydptos$pob2018+proydptos$pob2023)/2))

#Migrantes
dfp <- dfp %>%
  mutate(
    migrante_2018 = 0,  
    migrante_2018 = ifelse(PERNA01 > 5 & PERNA01 != 5555 &  (PERMI07 == 3 | PERMI07 == 4), 1, migrante_2018)
  )

#Tasa de inmigración
df_inmig <- dfp %>%
  group_by(DEPARTAMENTO) %>%  
  summarise(across(c(
    migrante_2018,
  ), sum, na.rm = TRUE))  

df_inmig <- df_inmig %>%
  mutate(DPTO = df_inmig$DEPARTAMENTO)

proydptos <- proydptos %>%
  mutate(DPTO = as.numeric(proydptos$DPTO))

df_inmig <- merge(df_inmig, proydptos, by = "DPTO")

df_inmig <- df_inmig %>%
  mutate(
    tasainmig = migrante_2018/pobmedia*1000/5 
  )

VALOR <- df_inmig$tasainmig
DEPARTAMENTO_UY <- df_inmig$Departamento
CODIND <- "p16"
NOMINDICADOR <- "Tasa inmigración departamental interna"
EDAD <- "NA"
SEXO <- "NA"
FECHA <- "2018-2023"
  
i11 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA, VALOR, RESPONSABLE, FUENTE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Tasa emigración departamental interna

df_emig <- dfp %>%
  filter(PERMI07_2 >= 1 & PERMI07_2 <= 19) %>%
  group_by(PERMI07_2) %>%  
  summarise(across(c(
    migrante_2018,
  ), sum, na.rm = TRUE)) 

df_emig <- df_emig %>% mutate(emigrados = migrante_2018)
df_emig <- df_emig %>% mutate(DPTO = PERMI07_2)

df_mig <-  merge(df_inmig, df_emig, by = "DPTO")


df_mig <- df_mig %>%
  mutate(
    tasaemig = emigrados/pobmedia*1000/5 
  )


VALOR <- df_mig$tasaemig
DEPARTAMENTO_UY <- df_mig$Departamento
CODIND <- "p17"
NOMINDICADOR <- "Tasa emigración departamental interna"
EDAD <- "NA"
SEXO <- "NA"
FECHA <- "2018-2023"

i12 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA, VALOR, RESPONSABLE, FUENTE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ~~~ Tasa neta migración interna

df_mig <- df_mig %>%
  mutate(
    tasaneta =  tasainmig - tasaemig
  )

VALOR <- df_mig$tasaneta
DEPARTAMENTO_UY <- df_mig$Departamento
CODIND <- "p18"
NOMINDICADOR <- "Tasa neta migración interna"
EDAD <- "NA"
SEXO <- "NA"
FECHA <- "2018-2023"

i13 <- cbind(CODIND, NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA, VALOR, RESPONSABLE, FUENTE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Construcción de base motor
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

motor23 <- rbind(i01,i02,i03,i04,i05,i06,i07,i08,i09,i10,i11,i12,i13, i14,i15,i16,i17,i18,i19,i20,i21,i22)

motor23 <- motor23 %>% mutate(DEPARTAMENTO_UY =
  case_when(
    DEPARTAMENTO_UY == 1 | DEPARTAMENTO_UY == " 1" ~ "Montevideo",
    DEPARTAMENTO_UY == 2 | DEPARTAMENTO_UY == " 2"~ "Artigas",
    DEPARTAMENTO_UY == 3 | DEPARTAMENTO_UY == " 3"~ "Canelones",
    DEPARTAMENTO_UY == 4 | DEPARTAMENTO_UY == " 4"~ "Cerro Largo",
    DEPARTAMENTO_UY == 5 | DEPARTAMENTO_UY == " 5"~ "Colonia",
    DEPARTAMENTO_UY == 6 | DEPARTAMENTO_UY == " 6"~ "Durazno",
    DEPARTAMENTO_UY == 7 | DEPARTAMENTO_UY == " 7"~ "Flores",
    DEPARTAMENTO_UY == 8 | DEPARTAMENTO_UY == " 8"~ "Florida",
    DEPARTAMENTO_UY == 9 | DEPARTAMENTO_UY == " 9"~ "Lavalleja",
    DEPARTAMENTO_UY == 10 | DEPARTAMENTO_UY == " 10"~ "Maldonado",
    DEPARTAMENTO_UY == 11 | DEPARTAMENTO_UY == " 11"~ "Paysandú",
    DEPARTAMENTO_UY == 12 | DEPARTAMENTO_UY == " 12"~ "Río Negro",
    DEPARTAMENTO_UY == 13 | DEPARTAMENTO_UY == " 13"~ "Rivera",
    DEPARTAMENTO_UY == 14 | DEPARTAMENTO_UY == " 14"~ "Rocha",
    DEPARTAMENTO_UY == 15 | DEPARTAMENTO_UY == " 15"~ "Salto",
    DEPARTAMENTO_UY == 16 | DEPARTAMENTO_UY == " 16"~ "San José",
    DEPARTAMENTO_UY == 17 | DEPARTAMENTO_UY == " 17"~ "Soriano",
    DEPARTAMENTO_UY == 18 | DEPARTAMENTO_UY == " 18"~ "Tacuarembó",
    DEPARTAMENTO_UY == 19 | DEPARTAMENTO_UY == " 19"~ "Treinta y Tres",
    TRUE ~ DEPARTAMENTO_UY
    ))

actualiza <- rbind(motor, motor23)

table(is.na(actualiza$CODIND))
table(is.na(actualiza$NOMINDICADOR))
table(is.na(actualiza$EDAD))
table(is.na(actualiza$SEXO))
table(is.na(actualiza$DEPARTAMENTO_UY))
table(is.na(actualiza$PAIS))
table(is.na(actualiza$FECHA))

actualiza <- rbind(actualiza, rraa)

actualiza <- actualiza %>% mutate(FECHA = as.numeric(FECHA))
rio::export(actualiza,"bases/Base_Motor_Demografica_rev2025_vf.xlsx")

