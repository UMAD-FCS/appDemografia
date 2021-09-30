


a = df_generica %>%
  filter(nomindicador == "Población total - proyecciones"  | 
           nomindicador == "Población por edades quinquenales - proyecciones"|
           nomindicador == "Población departamental (censos)"|
           nomindicador == "Población departamental - porcentaje (censos)"|
           nomindicador == "Población  departamental censada por edades quinquenales y sexo, Censo 2011"
  )



##variable de corte: sexo: varones, mujeres, ambos sexos - por año
#linea con corte sexo
a1 = df_generica %>%
  filter(nomindicador == "Población total - proyecciones")


##variable de corte: sexo y edad: todos los tramos para varones, mujeres, ambos sexos - por año
#piramide?
a2 = df_generica %>%
  filter(nomindicador == "Población por edades quinquenales - proyecciones")

##variable de corte: sexo y departamento: todos los tramos para varones, mujeres, ambos sexos - por año
#mapa?
a3 = df_generica %>%
  filter(nomindicador == "Población departamental (censos)")

