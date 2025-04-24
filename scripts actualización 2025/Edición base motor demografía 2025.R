
library(rio)
library(tidyverse)

df <- rio::import("Demografía/Base_Motor_Demografica.xls")


# Elimino variables sin uso y filas a reemplazar

df <- df  %>% dplyr::mutate(filtro = ifelse(NOMINDICADOR == "Población total censada" & FECHA == "2023",1,0))
df <- df  %>% filter(filtro == 0)
df <- df  %>% select(-c(filtro,...1,...2))
df <- df  %>% select(-c(ASCENDENCIA,`QUINTIL DE INGRESO`,SECTOR,`URBANORURAL UY`))


# Edición de nombres de indicador

df <- df %>% dplyr::mutate(NOMINDICADOR = ifelse(NOMINDICADOR == "Distribución porcentual de personas nacidas en el exterior según país de nacimiento. Censos 2011",
                                                        "Distribución porcentual de personas nacidas en el exterior según país de nacimiento", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Defunciones",
                                                 "Cantidad de defunciones", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Edad media de la fecundidad - Proyecciones",
                                                 "Edad media de la fecundidad (proyecciones)", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Mujeres en edad fértil",
                                                 "Cantidad de mujeres en edad fértil", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Nacimientos",
                                                 "Cantidad de nacimientos", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Nacimientos según departamento de residencia materna",
                                                 "Cantidad de nacimientos según departamento de residencia materna", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Población  departamental censada por edades quinquenales y sexo, Censo 2011",
                                                 "Población  departamental censada por edades quinquenales y sexo", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Población departamental - porcentaje (censos)",
                                                 "Porcentaje de población departamental sobre total de población", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Población departamental (censos)",
                                                 "Población departamental", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Población nacida en el exterior según país de nacimiento. Censos 2011",
                                                 "Población nacida en el exterior según país de nacimiento", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Población por edades quinquenales - proyecciones",
                                                 "Población por edades quinquenales (proyecciones)", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas afrodescendientes  con al menos una NBI. Censo 2011",
                                                 "Porcentaje de personas afrodescendientes con al menos una NBI", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en abastecimiento de agua potable. Censo 2011",
                                                 "Porcentaje de personas con NBI en abastecimiento de agua potable", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en abastecimiento de agua potable. Censo 2011",
                                                 "Porcentaje de personas con NBI en abastecimiento de agua potable", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en artefactos básicos de confort . Censo 2011",
                                                 "Porcentaje de personas con NBI en artefactos básicos de confort", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en calefacción . Censo 2011",
                                                 "Porcentaje de personas con NBI en calefacción", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en calentador de agua para el baño. Censo 2011",
                                                 "Porcentaje de personas con NBI en calentador de agua para el baño", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en cocina . Censo 2011",
                                                 "Porcentaje de personas con NBI en cocina", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en conservación de alimentos . Censo 2011",
                                                 "Porcentaje de personas con NBI en conservación de alimentos", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en educación . Censo 2011",
                                                 "Porcentaje de personas con NBI en educación", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en energía eléctrica . Censo 2011",
                                                 "Porcentaje de personas con NBI en energía eléctrica", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en hacinamiento . Censo 2011",
                                                 "Porcentaje de personas con NBI en hacinamiento", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en materiales de la vivienda . Censo 2011",
                                                 "Porcentaje de personas con NBI en materiales de la vivienda", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en servicio higiénico. Censo 2011",
                                                 "Porcentaje de personas con NBI en servicio higiénico", NOMINDICADOR),
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en Vivienda decorosa . Censo 2011",
                                                 "Porcentaje de personas con NBI en Vivienda decorosa", NOMINDICADOR),                           
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas entre 0 y 14 años con al menos una NBI. Censo 2011",
                                                 "Porcentaje de personas entre 0 y 14 años con al menos una NBI", NOMINDICADOR),                           
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Porcentaje de personas no afrodescendientes  con al menos una NBI. Censo 2011",
                                                 "Porcentaje de personas no afrodescendientes con al menos una NBI", NOMINDICADOR),                           
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Tasa de mortalidad infantil (menores de 1 año por 1.000 nacidos vivos) 1984-2023",
                                                 "Tasa de mortalidad infantil (menores de 1 año por 1.000 nacidos vivos)", NOMINDICADOR),                           
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Tasa de mortalidad neonatal (por 1.000 nacidos vivos) 1984-2023",
                                                 "Tasa de mortalidad neonatal (por 1.000 nacidos vivos)", NOMINDICADOR),                           
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Tasa de mortalidad posneonatal (por 1.000 nacidos vivos) 1984-2023",
                                                 "Tasa de mortalidad posneonatal (por 1.000 nacidos vivos)", NOMINDICADOR),                           
                           NOMINDICADOR = ifelse(NOMINDICADOR == "Tasa General de Fecudidad",
                                                 "Tasa General de Fecundidad", NOMINDICADOR))  
 
                     
table(df$NOMINDICADOR)

df <- df %>% dplyr::mutate(DEPARTAMENTO_UY = ifelse(NOMINDICADOR == "Porcentaje de personas afrodescendientes con al menos una NBI", PAIS, DEPARTAMENTO_UY))
df <- df %>% dplyr::mutate(DEPARTAMENTO_UY = ifelse(NOMINDICADOR == "Porcentaje de personas afrodescendientes con al menos una NBI" & PAIS == "Total País", "Total", DEPARTAMENTO_UY))
df <- df %>% dplyr::mutate(PAIS = ifelse(NOMINDICADOR == "Porcentaje de personas afrodescendientes con al menos una NBI", "Uruguay", PAIS))

df <- df %>% dplyr::mutate(DEPARTAMENTO_UY = ifelse(NOMINDICADOR == "Porcentaje de personas no afrodescendientes con al menos una NBI", PAIS, DEPARTAMENTO_UY))
df <- df %>% dplyr::mutate(DEPARTAMENTO_UY = ifelse(NOMINDICADOR == "Porcentaje de personas no afrodescendientes con al menos una NBI" & PAIS == "Total País", "Total", DEPARTAMENTO_UY))
df <- df %>% dplyr::mutate(PAIS = ifelse(NOMINDICADOR == "Porcentaje de personas no afrodescendientes con al menos una NBI", "Uruguay", PAIS))


# Homogeinización de varialbe edad

df <- df %>% dplyr::mutate(EDAD = ifelse(EDAD == "0-4 años de edad" | EDAD == "0 - 4",
                                                 "0-4 años", EDAD),
                           EDAD = ifelse(EDAD == "10-14 años de edad" | EDAD == "10 - 14",
                                         "10-14 años", EDAD),
                           EDAD = ifelse(EDAD == "15-19 años de edad" | EDAD == "15 - 19" | EDAD == "15-19",
                                         "15-19 años", EDAD),
                           EDAD = ifelse(EDAD == "20-24 años de edad" | EDAD == "20-24" | EDAD == "20 - 24",
                                         "20-24 años", EDAD),
                           EDAD = ifelse(EDAD == "25-29 años de edad" | EDAD == "25-29" | EDAD == "25 - 29",
                                         "25-29 años", EDAD),
                           EDAD = ifelse(EDAD == "30-34 años de edad" | EDAD == "30 - 34" | EDAD == "30-34",
                                         "30-34 años", EDAD),
                           EDAD = ifelse(EDAD == "35-39 años de edad" | EDAD == "35 - 39" | EDAD == "35-39",
                                         "35-39 años", EDAD),
                           EDAD = ifelse(EDAD == "40-44 años de edad" | EDAD == "40 - 44"  | EDAD == "40-44",
                                         "40-44 años", EDAD),
                           EDAD = ifelse(EDAD == "45-49 años de edad" | EDAD == "45 - 49",
                                         "15-19 años", EDAD),
                           EDAD = ifelse(EDAD == "5-9 años de edad" | EDAD == "5 - 9",
                                         "5-9 años", EDAD),
                           EDAD = ifelse(EDAD == "50-54 años de edad" | EDAD == "50 - 54",
                                         "55-59 años", EDAD),
                           EDAD = ifelse(EDAD == "55-59 años de edad" | EDAD == "55 - 59",
                                         "55-59 años", EDAD),
                           EDAD = ifelse(EDAD == "60-64 años de edad" | EDAD == "60 - 64",
                                         "60-64 años", EDAD),
                           EDAD = ifelse(EDAD == "65-69 años de edad" | EDAD == "65 - 69",
                                         "65-69 años", EDAD),
                           EDAD = ifelse(EDAD == "70-74 años de edad" | EDAD == "70 - 74",
                                         "70-74 años", EDAD),
                           EDAD = ifelse(EDAD == "75-79 años de edad" | EDAD == "75 - 79",
                                         "75-79 años", EDAD),
                           EDAD = ifelse(EDAD == "80-84 años de edad" | EDAD == "80 - 84",
                                         "80-84 años", EDAD),
                           EDAD = ifelse(EDAD == "85-89 años de edad" | EDAD == "85 - 89",
                                         "85-89 años", EDAD),
                           EDAD = ifelse(EDAD == "90-94 años de edad" | EDAD == "90 - 94",
                                         "90-94 años", EDAD),
                           EDAD = ifelse(EDAD == "95-99 años de edad" | EDAD == "95 - 99",
                                         "95-99 años", EDAD),
                           EDAD = ifelse(EDAD == "14 o menos",
                                         "14 años o menos", EDAD),
                           EDAD = ifelse(EDAD == "de 100 y más años de edad",
                                         "100 años y más", EDAD),
                           EDAD = ifelse(EDAD == "100 - 104",
                                         "100 años y más", EDAD),
                           EDAD = ifelse(EDAD == "todas las edades",
                                         "Total", EDAD))
                           
df <- df %>% dplyr::mutate(FECHA = ifelse(is.na(EDAD), FECHA, ifelse(EDAD == "1975-1970", "1975-1970", FECHA)))
df <- df %>% dplyr::mutate(FECHA = ifelse(is.na(EDAD), FECHA, ifelse(EDAD == "1985-1980", "1985-1980", FECHA)))
df <- df %>% dplyr::mutate(FECHA = ifelse(is.na(EDAD), FECHA, ifelse(EDAD == "1996-1991", "1996-1991", FECHA)))
df <- df %>% dplyr::mutate(FECHA = ifelse(is.na(EDAD), FECHA, ifelse(EDAD == "2011-2006", "2011-2006", FECHA)))


df <- df %>% dplyr::mutate(EDAD = ifelse(is.na(EDAD), "NA", ifelse(EDAD == "1975-1970", "NA", EDAD)))
df <- df %>% dplyr::mutate(EDAD = ifelse(is.na(EDAD), "NA", ifelse(EDAD == "1985-1980", "NA", EDAD)))
df <- df %>% dplyr::mutate(EDAD = ifelse(is.na(EDAD), "NA", ifelse(EDAD == "1996-1991", "NA", EDAD)))
df <- df %>% dplyr::mutate(EDAD = ifelse(is.na(EDAD), "NA", ifelse(EDAD == "2011-2006", "NA", EDAD)))

table(df$EDAD)
table(df$FECHA)

table(is.na(df$EDAD))
table(is.na(df$FECHA))


# Homogeinización variable sexo

df <- df %>% dplyr::mutate(SEXO = ifelse(SEXO == "mujeres", "Mujeres", SEXO))
df <- df %>% dplyr::mutate(SEXO = ifelse(SEXO == "varones", "Varones", SEXO))
df <- df %>% dplyr::mutate(SEXO = ifelse(SEXO == "ambos sexos", "Total", SEXO))
df <- df %>% dplyr::mutate(SEXO = ifelse(is.na(SEXO), "NA", SEXO))


table(df$SEXO)

# Homogeinización variable departamento

df <- df %>% 
  dplyr::mutate(DEPARTAMENTO_UY = paste0(toupper(substring(DEPARTAMENTO_UY, 1, 1)), tolower(substring(DEPARTAMENTO_UY, 2))))

df <- df %>% dplyr::mutate(DEPARTAMENTO_UY = ifelse(DEPARTAMENTO_UY == "Total pais" | DEPARTAMENTO_UY == "Total país", 
                                                    "Total", DEPARTAMENTO_UY),
                           DEPARTAMENTO_UY = ifelse(DEPARTAMENTO_UY == "Na" | DEPARTAMENTO_UY == "NANA" | DEPARTAMENTO_UY == "na", 
                                                    "NA", DEPARTAMENTO_UY),
                           DEPARTAMENTO_UY = ifelse(DEPARTAMENTO_UY == "Paysandu", 
                                                    "Paysandú", DEPARTAMENTO_UY),
                           DEPARTAMENTO_UY = ifelse(DEPARTAMENTO_UY == "Rio negro", 
                                                    "Río negro", DEPARTAMENTO_UY),
                           DEPARTAMENTO_UY = ifelse(DEPARTAMENTO_UY == "San jose" | DEPARTAMENTO_UY == "San josé", 
                                                    "San José", DEPARTAMENTO_UY),
                           DEPARTAMENTO_UY = ifelse(DEPARTAMENTO_UY == "Tacuarembo", 
                                                    "Tacuarembó", DEPARTAMENTO_UY))

table(df$DEPARTAMENTO_UY)

# Homogeinización variable país

df <- df %>% 
  dplyr::mutate(PAIS = paste0(toupper(substring(PAIS, 1, 1)), tolower(substring(PAIS, 2))))

table(df$PAIS)

# Homogeinización variable fuente

df <- df %>% dplyr::mutate(FUENTE = ifelse(FUENTE == "Censo 2011" | FUENTE == "INE Censo 2011"  | FUENTE == "INE Censo 1985" | FUENTE == "INE Censo 1996",
                                           "INE Censos de Población", FUENTE),
                           FUENTE = ifelse(FUENTE == "DGEC  Censo 1975", "DGEC Censo 1975", FUENTE),
                           FUENTE = ifelse(FUENTE == "Fuente: Dirección Nacional de Migración-Ministerio del Interior", "Dirección Nacional de Migración-Ministerio del Interior", FUENTE),
                           FUENTE = ifelse(FUENTE == "Fuente: Instituto Nacional de Estadística (INE) – Estimaciones y proyecciones de población (revisión 2013)." | FUENTE == "INE -Estimaciones y proyecciones de población. Revisión, 2013" | FUENTE == "Instituto Nacional de Estadística (INE) – Estimaciones y proyecciones de población (revisión 2013)",
                                           "INE Estimaciones y proyecciones de población (revisión 2013)", FUENTE),
                           FUENTE = ifelse(FUENTE == "Fuente: Instituto Nacional de Estadística (INE) con datos del Ministerio de Salud Pública (MSP) y proyecciones de población."
                                           | FUENTE == "Instituto Nacional de Estadística (INE) con datos del Ministerio de Salud Pública (MSP) y proyecciones de población", "Instituto Nacional de Estadística (INE) con datos del Ministerio de Salud Pública (MSP) y proyecciones de población", FUENTE),
                           FUENTE = ifelse(FUENTE == "Fuente: Ministerio de Salud Pública", "Ministerio de Salud Pública", FUENTE),
                           FUENTE = ifelse(FUENTE == "INE Proyecciones de población", "INE Proyecciones de Población", FUENTE),
                           
                           FUENTE = ifelse(FUENTE == "MSP", "Ministerio de Salud Pública", FUENTE),
                           FUENTE = ifelse(FUENTE == "MSP  y INE -Estimaciones y proyecciones de población. Revisión, 2013" | FUENTE == "MSP y INE -Estimaciones y proyecciones de población. Revisión, 2013" | FUENTE == "Instituto Nacional de Estadística (INE) con datos del Ministerio de Salud Pública (MSP) y proyecciones de población",
                                           "MSP e INE - Estimaciones y proyecciones de población (revisión 2013)", FUENTE)
                           )



table(df$FUENTE)


# Cödigos indicadores ausentes

df$CODIND <- tolower(df$CODIND)
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Población por edades quinquenales", "p5", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Población total censada", "p56", CODIND))

a <- df %>% filter(is.na(CODIND))
table(a$NOMINDICADOR)
table(df$CODIND)

df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Cantidad de defunciones", "p57", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Cantidad de mujeres en edad fértil", "p58", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Cantidad de nacimientos", "p59", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Cantidad de resdencias definitivas otorgadas", "p60", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Cantidad de resdencias temporarias otorgadas", "p61", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Cantidad de residencias fronterizas otorgadas", "p62", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Cantidad de residencias otorgadas", "p63", CODIND))
#df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Esperanza de vida al nacer", "p64", CODIND))
df <- df %>% filter(NOMINDICADOR != "Esperanza de vida al nacer")

df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Porcentaje de nacimientos de madres menores de 20 años sobre total de nacimientos", "p65", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Proyecciones de población (NNUU, 2019)", "p66", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Saldo migratorio", "p67", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Tasa bruta de mortalidad", "p68", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Tasa bruta de natalidad (por mil)", "p69", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Tasa General de Fecundidad", "p70", CODIND))
df <- df %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Tasas Específicas de Fecundidad por edad simple (por mil)", "p71", CODIND))


table(is.na(df$CODIND))
table(is.na(df$NOMINDICADOR))
table(is.na(df$EDAD))
table(is.na(df$SEXO))
table(is.na(df$DEPARTAMENTO_UY))
table(is.na(df$PAIS))
table(is.na(df$FECHA))



# !!! Re-calculo tasas migratorias internas 2011

df2011    <- rio::import("Bases/base_reduc_2011.dta") 
proydptos <- rio::import("Bases/proy_dptos.xlsx")


#Población media
proydptos <- proydptos %>%
  mutate(
    pobmedia = ((proydptos$pob2011+proydptos$pob2006)/2))


# Migrantes internos

df2011 <- df2011 %>%
  mutate(
    migrante_2006 = 0,  
    migrante_2006 = ifelse(MA == 0 & PERNA01 > 5 & PERNA01 != 5555 &  (PERMI07 == 3 | PERMI07 == 4), 1, migrante_2006)
  )

# Tasa de inmigración departamental

df_inmig <- df2011 %>%
  group_by(DPTO) %>%  
  summarise(across(c(
    migrante_2006,
  ), sum, na.rm = TRUE))  


df_inmig <- merge(df_inmig, proydptos, by = "DPTO")

df_inmig <- df_inmig %>%
  mutate(
    tasainmig = migrante_2006/pobmedia*1000/5 
  )


# Tasa emigración departamental interna

df2011 <- df2011 %>% mutate(PERMI07_2 = as.numeric(PERMI07_2))

df_emig <- df2011 %>%
  filter(PERMI07_2 >= 1 & PERMI07_2 <= 19) %>%
  group_by(PERMI07_2) %>%  
  summarise(across(c(
    migrante_2006,
  ), sum, na.rm = TRUE)) 

df_emig <- df_emig %>% mutate(emigrados = migrante_2006)
df_emig <- df_emig %>% mutate(DPTO = PERMI07_2)


df_inmig <- df_inmig %>% mutate(DPTO = as.numeric(DPTO))
df_mig <-  merge(df_inmig, df_emig, by = "DPTO")


df_mig <- df_mig %>%
  mutate(
    tasaemig = emigrados/pobmedia*1000/5 
  )

# Tasa neta migración interna

df_mig <- df_mig %>%
  mutate(
    tasaneta =  tasainmig - tasaemig
  )




df <- df  %>% dplyr::mutate(filtro = ifelse(NOMINDICADOR == "Tasa emigración departamental interna" & FECHA == "2011-2006",1,0))
df <- df  %>% filter(filtro == 0)

df <- df  %>% dplyr::mutate(filtro = ifelse(NOMINDICADOR == "Tasa inmigración departamental interna" & FECHA == "2011-2006",1,0))
df <- df  %>% filter(filtro == 0)

df <- df  %>% dplyr::mutate(filtro = ifelse(NOMINDICADOR == "Tasa neta migración interna" & FECHA == "2011-2006",1,0))
df <- df  %>% filter(filtro == 0)
df <- df  %>% select(-filtro)

PAIS   <- "Uruguay"
FECHA  <- "2006-2011"
FUENTE <- "INE Censos de Población"
RESPONSABLE <- "J.Pandolfi"
EDAD <- "NA"
SEXO <- "NA"
DEPARTAMENTO_UY <- df_mig$Departamento

CODIND <- "p16"
NOMINDICADOR <- "Tasa inmigración departamental interna"
VALOR  <- df_mig$tasainmig

rep1 <- cbind(CODIND,NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA, VALOR, RESPONSABLE, FUENTE)


CODIND <- "p17"
NOMINDICADOR <- "Tasa emigración departamental interna"
VALOR  <- df_mig$tasaemig

rep2 <- cbind(CODIND,NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA, VALOR, RESPONSABLE, FUENTE)

CODIND <- "p18"
NOMINDICADOR <- "Tasa neta migración interna"
VALOR  <- df_mig$tasaneta

rep3 <- cbind(CODIND,NOMINDICADOR, EDAD, SEXO, DEPARTAMENTO_UY, PAIS, FECHA, VALOR, RESPONSABLE, FUENTE)

rep <- rbind(rep1,rep2,rep3)

df <- rbind(df,rep)

# Corrección de códigos

df <- df  %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Porcentaje de personas con NBI en energía eléctrica", "p46", CODIND))
df <- df  %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Población nacida en el exterior según país de nacimiento", "p56", CODIND))
df <- df  %>% dplyr::mutate(CODIND = ifelse(NOMINDICADOR == "Población total censada", "p72", CODIND))

rio::export(df,"Demografía/Base_Motor_Demografica_rev2025.xlsx")
