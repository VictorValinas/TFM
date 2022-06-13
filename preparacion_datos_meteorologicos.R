#-----------------------------------------------------------------------------#
# En este archivo vamos a crear la base de datos meteorológica de toda España.  
# En ella se tienen en cuenta todas las estaciones meteorológicas españolas.
#-----------------------------------------------------------------------------#
# 
#-----------------------------------------------------------------------------#
# A continuación cargamos los paquetes necesarios para leer los archivos .json. 
# Estos archivos se descargaron de la pagina de AEMET y contienen la información 
# continen toda la información meteorológica
#-----------------------------------------------------------------------------#
library(jsonlite)
library(data.table)
#-----------------------------------------------------------------------------#
# cargamos los archivos meteorologicos de 2018
#-----------------------------------------------------------------------------#
datos_2018=list.files(path="/Volumes/VICTOR/Practicas Estrella Galicia
                      /Datos Meteorologicos/datos_2018",
                      pattern = "*.json")
datos_2018
#-----------------------------------------------------------------------------#
# cargamos los archivos meteorologicos de 2019
#-----------------------------------------------------------------------------#
datos_2019=list.files(path="/Volumes/VICTOR/Practicas Estrella Galicia
                      /Datos Meteorologicos/datos_2019",
                      pattern = "*.json")
datos_2019
#-----------------------------------------------------------------------------#
# cargamos los archivos meteorologicos de 2018
#-----------------------------------------------------------------------------#
datos_2020=list.files(path="/Volumes/VICTOR/Practicas Estrella Galicia
                      /Datos Meteorologicos/datos_2020",
                      pattern = "*.json")
datos_2020
#-----------------------------------------------------------------------------#
# cargamos los archivos meteorologicos de 2018
#-----------------------------------------------------------------------------#
datos_2021=list.files(path="/Volumes/VICTOR/Practicas Estrella Galicia
                      /Datos Meteorologicos/datos_2021",
                      pattern = "*.json")
datos_2021
#-----------------------------------------------------------------------------#
# A continuación, vamos a leer todos los archivos .json del año 2018 y los
# vamos a juntar por filas
#-----------------------------------------------------------------------------#
setwd("/Volumes/VICTOR/Practicas Estrella Galicia/Datos Meteorologicos/datos_2018")
multiples_json_2018=lapply(datos_2018, fromJSON)
datos_meteorologicos_2018 <- rbindlist(multiples_json_2018)
#-----------------------------------------------------------------------------#
# Ahora vamos a leer todos los archivos .json del año 2019 
#-----------------------------------------------------------------------------#
setwd("/Volumes/VICTOR/Practicas Estrella Galicia/Datos Meteorologicos/datos_2019")
multiples_json_2019=lapply(datos_2019, fromJSON)
datos_meteorologicos_2019 <- rbindlist(multiples_json_2019)
#-----------------------------------------------------------------------------#
# Ahora vamos a leer todos los archivos .json del año 2020 
#-----------------------------------------------------------------------------#
setwd("/Volumes/VICTOR/Practicas Estrella Galicia/Datos Meteorologicos/datos_2020")
multiples_json_2020=lapply(datos_2020, fromJSON)
datos_meteorologicos_2020 <- rbindlist(multiples_json_2020)
#-----------------------------------------------------------------------------#
# Ahora vamos a leer todos los archivos .json del año 2021 
#-----------------------------------------------------------------------------#
setwd("/Volumes/VICTOR/Practicas Estrella Galicia/Datos Meteorologicos/datos_2021")
multiples_json_2021=lapply(datos_2021, fromJSON)
datos_meteorologicos_2021 <- rbindlist(multiples_json_2021)
#-----------------------------------------------------------------------------#
# Finalmente, juntamos por filas la información de los diferentes años
#-----------------------------------------------------------------------------#
datos_meteorologicos=rbind(datos_meteorologicos_2018, datos_meteorologicos_2019, 
                           datos_meteorologicos_2020, datos_meteorologicos_2021)
datos_meteorologicos
#-----------------------------------------------------------------------------#
# Ahora vamos a guardar en un archivo .Rdata la base de datos que acabamos de crear.
#-----------------------------------------------------------------------------#
setwd("/Volumes/VICTOR/Practicas Estrella Galicia/Datos Meteorologicos/")
save(datos_meteorologicos, file="datos_meteorologicos.RData")


