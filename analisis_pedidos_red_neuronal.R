#-----------------------------------------------------------------------------#
# Cargamos las librerias necesarias parar trabajar
#-----------------------------------------------------------------------------#
library(dplyr)
library(lubridate)
library(MASS)
library(tidyverse)
library(MLmetrics) # para poder utilizar la funcion MAPE
library(nnet) # para trabajar con redes neuronales
library(neuralnet)  # para utilizar redes neuronales
library(fastDummies) # para crear variables artificiales
#-----------------------------------------------------------------------------#
# Cargamos los datos que vamos a utilizar
#-----------------------------------------------------------------------------#
load("datos.RData") # cargamos los datos
datos

#---- FUNCION PARA COMPARAR EL FUNCIONAMIENTO DE LOS DIFERENTES MODELOS ----
#-----------------------------------------------------------------------------#
# La siguiente función nos permite comparar los diferentes modelos que ajustemos
# en funcion de su "MAPE", "MAE" y "RMSE".
#-----------------------------------------------------------------------------#
performance=function(valor_pred, valor_real){
  MAPE=as.vector(MAPE(valor_pred,valor_real))
  MAE=as.vector(MAE(valor_pred,valor_real))
  RMSE=as.vector(RMSE(valor_pred,valor_real))
  resultados=c(MAPE,MAE,RMSE)
  names(resultados)=c("MAPE", "MAE", "RMSE")
  resultados=as.data.frame(t(resultados))
  return(resultados)
}
#-----------------------------------------------------------------------------#
# COMENTARIO: A la vista de los graficos y de los productos que mas se repiten
#             entre los 5 primeros de cada distribuidor, en nuestro caso nos 
#             vamos a centrar en analizar el comportamiento de los siguientes
#             productos: 
# 
#             "Producto_1"
#             "Producto_2"
#             "Producto_3"
#-----------------------------------------------------------------------------#



#---- ANALISIS DE UN DISTRIBUIDOR PARTICULAR CON UN UNICO PRODUCTO ----
#-----------------------------------------------------------------------------#
# Lo primero que hacemos es filtrar los datos.
#-----------------------------------------------------------------------------#
datos=datos %>% filter(Dias_ultimo_pedido<150) %>% na.omit()
#-----------------------------------------------------------------------------#
# Aqui seleccionamos el distribuidor y el producto
#-----------------------------------------------------------------------------#
Distribuidor="Distribuidor_1"  
#Distribuidor="Distribuidor_2"  

Material="Producto_1"
#Material="Producto_2"
#Material="Producto_3"
#-----------------------------------------------------------------------------#
# Creamos la variable "Pedido". Es la que utilizaremos en los modelos.
#-----------------------------------------------------------------------------#
datos_brutos=datos %>% group_by(Fecha_pedido) %>%  
  filter(Distribuidor_ID==Distribuidor) %>% 
  mutate(Pedido_litros=ifelse(Material_desc==Material, Litros_pedidos, 0),
         Pedido=ifelse(Material_desc==Material, Cantidad_pedido, 0)) %>% 
  arrange(Fecha_pedido, desc(Pedido)) %>% ungroup()  

datos_brutos
#-----------------------------------------------------------------------------#
# Eliminamos duplicados. Es decir, eliminamos las filas que tengan la misma 
# fecha y el mismo pedidoE
#-----------------------------------------------------------------------------#
datos_limpios=datos_brutos[!duplicated(datos_brutos$Fecha_pedido,
                                       datos_brutos$Pedido),] %>% na.omit()
datos_limpios
#-----------------------------------------------------------------------------#
# Creamos las siguientes variables
#-----------------------------------------------------------------------------#
# - "Promedio_mes" -> RELATIVA A NUESTRO DISTRIBUIDOR PARTICULAR
# - "Promedio_semana" -> RELATIVA A NUESTRO DISTRIBUIDOR PARTICULAR
# - "Promedio_mes_distribuidores" -> RELATIVA A TODOS LOS DISTRIBUIDORES
# - "Promedio_semana_distribuidores" -> RELATIVA A TODOS LOS DISTRIBUIDORES
#-----------------------------------------------------------------------------#
datos_distribuidores_material=datos %>% 
  filter(!Distribuidor_ID==Distribuidor) %>% # Sacamos a nuestro distribuidor
  filter(Material_desc==Material) %>% 
  group_by(Mes,Ano) %>% 
  mutate(Promedio_mes_distribuidores=mean(Litros_pedidos)) %>% 
  ungroup() %>% 
  group_by(Semana,Ano) %>% 
  mutate(Promedio_semana_distribuidores=mean(Litros_pedidos))

datos_limpios_material=datos_limpios %>% 
  filter(Material_desc==Material) %>% 
  group_by(Mes,Ano) %>% 
  mutate(Promedio_mes=mean(Pedido)) %>% 
  ungroup() %>% 
  group_by(Semana,Ano) %>% 
  mutate(Promedio_semana=mean(Pedido))

datos=datos_limpios %>% 
  left_join(datos_limpios_material[,c("Mes","Ano","Promedio_mes")],
            by=c("Mes"="Mes","Ano"="Ano")) %>% unique() %>% 
  left_join(datos_distribuidores_material[,c("Mes","Ano",
                                             "Promedio_mes_distribuidores")],
            by=c("Mes"="Mes", "Ano"="Ano")) %>% unique() %>% 
  left_join(datos_limpios_material[,c("Semana","Ano","Promedio_semana")],
            by=c("Semana"="Semana","Ano"="Ano")) %>% unique() %>% 
  left_join(datos_distribuidores_material[,c("Semana","Ano",
                                             "Promedio_semana_distribuidores")],
            by=c("Semana"="Semana","Ano"="Ano")) %>% unique()

datos
#-----------------------------------------------------------------------------#
# Cambiamos los posibles 'NA' POR 0
#-----------------------------------------------------------------------------#
datos[is.na(datos)]=0

datos %>% filter(Material_desc==Material) # Para ver los pedidos que tenemos 
                                          # del material que nos interesa 
#-----------------------------------------------------------------------------#
# Ajustamos una red neuronal
#-----------------------------------------------------------------------------#
datos
n=nrow(datos)
#-----------------------------------------------------------------------------#
# Nos quedamos con las columnas que nos interesan
#-----------------------------------------------------------------------------#
# - Pedido 
# - Litros_1_pedido_antes
# - Litros_2_pedidos_antes
# - Litros_3_pedidos_antes
# - Litros_4_pedidos_antes
# - Litros_5_pedidos_antes
# - Litros_6_pedidos_antes
# - Litros_7_pedidos_antes
# - Promedio_mes 
# - Promedio_mes_distribuidores
# - Promedio_semana
# - Promedio_semana_distribuidores
# - Semana
# - Estacion
# - Navidad
# - Covid
# - Dia_mes
# - Dia_semana
# - Mes
# - Ano
# - Temperatura
#-----------------------------------------------------------------------------#
datos_simplificados=datos[,c("Pedido", "Litros_1_pedido_antes", "Litros_2_pedidos_antes", 
         "Litros_3_pedidos_antes", "Litros_4_pedidos_antes", "Litros_5_pedidos_antes",
         "Litros_6_pedidos_antes", "Litros_7_pedidos_antes","Promedio_mes",
         "Promedio_mes_distribuidores","Promedio_semana","Promedio_semana_distribuidores",
         "Semana","Estacion", "Navidad", "Covid", "Dia_mes", "Dia_semana", "Mes", 
         "Ano", "Temperatura")]

datos_simplificados

#-----------------------------------------------------------------------------#
# Normalizamos los datos en el intervalo [0,1]. 
# Para ello solo podemos considerar variables numericas
#-----------------------------------------------------------------------------#
maxs <- apply(datos_simplificados[,c(1:12,21)], 2, max) 
mins <- apply(datos_simplificados[,c(1:12,21)], 2, min)
datos_escalados <- as.data.frame(scale(datos_simplificados[,c(1:12,21)], 
                                       center = mins, scale = maxs - mins))
#-----------------------------------------------------------------------------#
# Añadimos las variables categoricas a los datos escalados
#-----------------------------------------------------------------------------#
datos_escalados$Semana=datos$Semana
datos_escalados$Estacion=datos$Estacion
datos_escalados$Navidad=datos$Navidad
datos_escalados$Covid=datos$Covid
datos_escalados$Dia_mes=datos$Dia_mes
datos_escalados$Dia_semana=datos$Dia_semana
datos_escalados$Mes=datos$Mes
datos_escalados$Ano=datos$Ano
#-----------------------------------------------------------------------------#
# Datos con los que vamos a trabajar
#-----------------------------------------------------------------------------#
datos_escalados
#------------------------------------------------------------------------#
# Red neuronal sencilla
#------------------------------------------------------------------------#
p=0.8
#-----------------------------------------------------------------------------#
# Conjunto entrenamiento
#-----------------------------------------------------------------------------#
datos_train=datos_escalados[1:ceiling(p*n),]
n1=nrow(datos_train)
datos_train_dummy=dummy_cols(datos_train, select_columns = c("Navidad", "Estacion","Covid"),
                             remove_selected_columns=TRUE ) 
#-----------------------------------------------------------------------------#
# Conjunto test
#-----------------------------------------------------------------------------#
datos_test = datos_escalados[(1+ceiling(p*n)):n,]
n2=nrow(datos_test)
datos_test_dummy=dummy_cols(datos_test,select_columns=c("Navidad", "Estacion","Covid"),
                            remove_selected_columns=TRUE )

set.seed(15)
neural_sencilla=neuralnet(Pedido~Litros_1_pedido_antes+
                            Litros_2_pedidos_antes+
                            Litros_3_pedidos_antes+
                            Litros_4_pedidos_antes+
                            Semana+
                            Promedio_semana+
                            Promedio_semana_distribuidores+
                            Mes+
                            Promedio_mes+
                            Promedio_mes_distribuidores+
                            Dia_mes+
                            Navidad_0+Navidad_1+
                            Estacion_Verano+Estacion_Otoño+
                            Estacion_Invierno+Estacion_Primavera+
                            Covid_0+Covid_1+
                            Temperatura,
                          data=datos_train_dummy,
                          hidden=c(3),
                          threshold = 0.01,
                          stepmax = 1e+05,
                          rep=1, 
                          algorithm = "rprop+", 
                          err.fct = "sse",
                          act.fct = "tanh")
                          # act.fct funcion diferenciable que se utiliza
                          # para suavizar el resultado
  
  pred_neural_sencilla=compute(x=neural_sencilla,
                               covariate=datos_test_dummy)$net.result
  
  pred_neural_sencilla=pred_neural_sencilla*((max(datos$Pedido)-min(datos$Pedido)) 
                                             + min(datos$Pedido)) 
  
  minimo=unique(sort(tail(datos$Pedido,n=n2)))[2]
  
  pred_neural_sencilla=ifelse(pred_neural_sencilla<minimo,0,pred_neural_sencilla)
  
  pred_neural_sencilla=round(pred_neural_sencilla)
  
  datos_test_dummy$Pedido=datos_test_dummy$Pedido *((max(datos$Pedido)-min(datos$Pedido)) 
                                                    + min(datos$Pedido) )  
  # tenemos que hacer esto porque los datos estan escalados
  
  
  performance_neural_sencilla=performance(pred_neural_sencilla, 
                                          datos_test_dummy$Pedido)$MAE
  performance_neural_sencilla

#-----------------------------------------------------------------------------#
# Tabla para comparar los pedidos con las predicciones
#-----------------------------------------------------------------------------#
tabla_nn_1=data.frame(Pedido=tail(datos$Pedido,n=length(pred_neural_sencilla)),
                        Prediccion=pred_neural_sencilla,
                        Diferencia=pred_neural_sencilla-tail(datos$Pedido,
                                                             n=length(pred_neural_sencilla)))
tabla_nn_1
#------------------------------------------------------------------------#
# Calculemos el MAPE con Red Neuronal  para pedidos no nulos
#------------------------------------------------------------------------#
datos_mape_1=tabla_nn_1[!tabla_nn_1$Pedido==0,]
performance_nn=performance(datos_mape_1$Prediccion,datos_mape_1$Pedido)
performance_nn
#------------------------------------------------------------------------#
# Calculemos la proporcion de acierto en los pedidos nulos 
#------------------------------------------------------------------------#
proporcion_nulos=(nrow(filter(tabla_nn_1,Pedido==0,Prediccion==0)))/(nrow(filter(tabla_nn_1,Pedido==0)))
proporcion_nulos
#------------------------------------------------------------------------#


#------------------------------------------------------------------------#
# Red neuronal retroalimentada
#------------------------------------------------------------------------#
p=0.8
#-----------------------------------------------------------------------------#
# Conjunto entrenamiento
#-----------------------------------------------------------------------------#
datos_train=datos_escalados[1:ceiling(p*n),]
n1=nrow(datos_train)
#-----------------------------------------------------------------------------#
# Conjunto test
#-----------------------------------------------------------------------------#
datos_test = datos_escalados[(1+ceiling(p*n)):n,]
n2=nrow(datos_test)

set.seed(15)
performance_neural_mape=numeric()
performance_neural_mae=numeric()
pred_neural=numeric()
n3=ceiling(p*n)
for (n3 in n1:(n-1)){ # tenemos que ir hasta n-1 porque sino no tenemos datos de test
  datos_train=datos_escalados[1:n3,]
  datos_test = datos_escalados[n3+1,]
  
  datos_train_dummy=dummy_cols(datos_train,select_columns=c("Navidad","Estacion","Covid"),
                               remove_selected_columns=TRUE ) 
  datos_test_dummy=dummy_cols(datos_test,select_columns=c("Navidad","Estacion","Covid"),
                              remove_selected_columns=TRUE )
  
  
  neural=neuralnet(Pedido~Litros_1_pedido_antes+
                     Litros_2_pedidos_antes+
                     Litros_3_pedidos_antes+
                     Litros_4_pedidos_antes+
                     Semana+
                     Promedio_semana+
                     Promedio_semana_distribuidores+
                     Mes+
                     Promedio_mes+
                     Promedio_mes_distribuidores+
                     Dia_mes+
                     Navidad_0+Navidad_1+
                     Estacion_Verano+Estacion_Otoño+
                     Estacion_Invierno+Estacion_Primavera+
                     Covid_0+Covid_1+
                     Temperatura,
                   data=datos_train_dummy,
                   hidden=c(3),
                   threshold = 0.01,
                   stepmax = 1e+07,
                   rep=1, 
                   algorithm = "rprop+", 
                   err.fct = "sse",
                   act.fct = "tanh")
                  # act.fct funcion diferenciable que se utiliza
                  # para suavizar el resultado
  
  pred_neural[n3]=compute(x=neural,covariate=datos_test_dummy)$net.result
  
  pred_neural[n3]=pred_neural[n3]*((max(datos$Pedido) - min(datos$Pedido)) 
  + min(datos$Pedido))  # tenemos que hacer esto porque los datos estan escalados
 
  minimo=unique(sort(tail(datos$Pedido,n=n2)))[2]
  
  pred_neural[n3]=ifelse(pred_neural[n3]<minimo,0,pred_neural[n3])
  
  pred_neural[n3]=round(pred_neural[n3])
  
  datos_test_dummy$Pedido=datos_test_dummy$Pedido*((max(datos$Pedido)-min(datos$Pedido)) 
  + min(datos$Pedido) ) # tenemos que hacer esto porque los datos estan escalados
  
  performance_neural_mae[n3]=performance(pred_neural[n3], datos_test_dummy$Pedido)$MAE
  
  n3=n3+1
}

#-----------------------------------------------------------------------------#
# Eliminamos los "NA" del vector inicializado anteriormente. 
# Asi nos quedamos solo con los valores que nos interesan
#-----------------------------------------------------------------------------#
pred_neural=pred_neural[!is.na(pred_neural)]
performance_neural_mae=performance_neural_mae[!is.na(performance_neural_mae)]
performance_total=mean(performance_neural_mae)
performance_total


#-----------------------------------------------------------------------------#
# Tabla para comparar los pedidos con las predicciones
#-----------------------------------------------------------------------------#
tabla_nn=data.frame(Pedido=tail(datos$Pedido,n=length(pred_neural)),
                    Prediccion=pred_neural,
                    Diferencia=tail(datos$Pedido,n=length(pred_neural))-pred_neural)

#------------------------------------------------------------------------#
# Calculemos el MAPE con Red Neuronal  para pedidos no nulos
#------------------------------------------------------------------------#
datos_mape=tabla_nn[!tabla_nn$Pedido==0,]
performance_nn_r=performance(datos_mape$Prediccion,datos_mape$Pedido)
performance_nn_r
#------------------------------------------------------------------------#
# Calculemos la proporcion de pedidos nulos correctamente
#------------------------------------------------------------------------#
proporcion_nulos=(nrow(filter(tabla_nn,Pedido==0,Prediccion==0)))/(nrow(filter(tabla_nn,Pedido==0)))
proporcion_nulos
#------------------------------------------------------------------------#
