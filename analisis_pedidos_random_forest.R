#-----------------------------------------------------------------------------#
# Cargamos las librerias necesarias parar trabajar
#-----------------------------------------------------------------------------#
library(dplyr)
library(lubridate)
library(MASS)
library(tidyverse)
library(MLmetrics) # para poder utilizar la funcion MAPE
library(randomForest) # para utilizar modelos "Random Forest"
library(caret)
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
#------------------------------------------------------------------------#
datos[is.na(datos)]=0

datos %>% filter(Material_desc==Material) # Para ver los pedidos que tenemos 
                                          # del material que nos interesa 


#---- MODELO BASE (MODELO MEDIA) ----
#------------------------------------------------------------------------#
# Creamos un modelo base
#------------------------------------------------------------------------#
datos
n=nrow(datos)
p=0.8
# Conjunto entrenamiento
datos_train=datos[1:ceiling(p*n),]
# Conjunto test
datos_test=datos[(1+ceiling(p*n)):n,]

modelo_media=mean(datos_train$Pedido)
modelo_media=round(modelo_media)

Media_datos=rep(modelo_media,nrow(datos_test))

performance_modelo_media=performance(Media_datos,datos_test$Pedido)$MAE
performance_modelo_media

#------------------------------------------------------------------------#
# Ajustamos un Random Forest
#------------------------------------------------------------------------#
datos
n=nrow(datos)
#------------------------------------------------------------------------#
# Ajuste sencillo
#------------------------------------------------------------------------#
p=0.8
# Conjunto entrenamiento
datos_train=datos[1:ceiling(p*n),]
# Conjunto test
datos_test=datos[(1+ceiling(p*n)):n,]

set.seed(15)
modelo_rf_1=randomForest(Pedido~Litros_1_pedido_antes+
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
                           Navidad+
                           Estacion+
                           Covid+
                           Temperatura,
                       data = datos_train,
                       ntree=500,
                       importance = TRUE)


predicciones_rf_1=predict(modelo_rf_1,newdata = datos_test)
minimo=unique(sort(datos_test$Pedido,n=length(datos_test)))[2]
predicciones_rf_1=ifelse(predicciones_rf_1<minimo,0,predicciones_rf_1)
predicciones_rf_1=round(predicciones_rf_1)
performance_rf_1=performance(predicciones_rf_1,datos_test$Pedido)$MAE
performance_rf_1
#-----------------------------------------------------------------------------#
# Tabla para comparar los pedidos con las predicciones
#-----------------------------------------------------------------------------#
tabla_rf_1=data.frame(Pedido=tail(datos$Pedido,n=length(predicciones_rf_1)),
           Prediccion=predicciones_rf_1,
           Diferencia=predicciones_rf_1-tail(datos$Pedido,n=length(predicciones_rf_1)))
tabla_rf_1
#------------------------------------------------------------------------#
# Calculemos el MAPE con el Random Forest  para pedidos no nulos
#------------------------------------------------------------------------#
datos_mape_1=tabla_rf_1[!tabla_rf_1$Pedido==0,]
performance_rf=performance(datos_mape_1$Prediccion,datos_mape_1$Pedido)
performance_rf
#------------------------------------------------------------------------#
# Calculemos la proporcion de acierto en los  pedidos nulos
#------------------------------------------------------------------------#
proporcion_nulos=(nrow(filter(tabla_rf_1,Pedido==0,Prediccion==0)))/(nrow(filter(tabla_rf_1,Pedido==0)))
proporcion_nulos

#-----------------------------------------------------------------------------#
# Veamos la importancia de cada predictor
#-----------------------------------------------------------------------------#
modelo_rf_1$importanceSD

varImpPlot(modelo_rf_1)

lista_predictores=data.frame("Predictor"= c("Litros 1 pedido antes",
                                            "Litros 2 pedidos antes",
                                            "Litros 3 pedidos antes",
                                            "Litros 4 pedidos antes",
                                            "Semana",
                                            "Promedio semana",
                                            "Promedio semana distribuidores",
                                            "Mes",
                                            "Promedio mes",
                                            "Promedio mes distribuidores",
                                            "Dia mes",
                                            "Navidad",
                                            "Estacion",
                                            "Covid",
                                            "Temperatura"),
                             "Valor"=unname(modelo_rf_1$importanceSD))

lista_predictores %>%
  dplyr::arrange(desc(Valor)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(Predictor, Valor), Valor)) +
  geom_col(fill="blue") +
  coord_flip() +
  xlab("") + ylab("") +
  ggtitle("") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0),
        plot.title = element_text(hjust = 0.5))
#------------------------------------------------------------------------#



#------------------------------------------------------------------------#
# Ajustamos un Random Forest cuyo conjunto e entrenamiento se vaya 
# alimentando del conjunto de validacion, para ello tenemos que hacer
# uso de un bucle.
#------------------------------------------------------------------------#
datos
n=nrow(datos)
p=0.8
# Conjunto entrenamiento
datos_train=datos[1:ceiling(p*n),]
n1=nrow(datos_train)
# Conjunto test
datos_test=datos[(1+ceiling(p*n)):n,]
n2=nrow(datos_test)

set.seed(15)
performance_rf=numeric()
pred_rf=numeric()
n3=ceiling(p*n)
for (n3 in n1:(n-1)){ # tenemos que ir hasta n-1 porque sino no tenemos datos de test
  datos_train=datos[1:n3,]
  datos_test = datos[n3+1,]
  
  modelo_rf=randomForest(Pedido~Litros_1_pedido_antes+
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
                           Navidad+
                           Estacion+
                           Covid+
                           Temperatura,
                         data = datos_train,
                         ntree=500)
  
  pred_rf[n3]=predict(modelo_rf, newdata = datos_test)
  
  minimo=unique(sort(tail(datos$Pedido,n=n2)))[2]
  
  pred_rf[n3]=ifelse(pred_rf[n3]< minimo,0,pred_rf[n3])
  
  pred_rf[n3]=round(pred_rf[n3])
  
  performance_rf[n3]=performance(pred_rf[n3],datos_test$Pedido)$MAE
  
  n3=n3+1
}

pred_rf=pred_rf[!is.na(pred_rf)]
performance_rf_mae=performance_rf[!is.na(performance_rf)]
performance_total=mean(performance_rf_mae)
performance_total
#-----------------------------------------------------------------------------#
# Tabla para comparar los pedidos con las predicciones
#-----------------------------------------------------------------------------#
tabla_rf=data.frame(Pedido=tail(datos$Pedido,n=length(pred_rf)),
           Prediccion=pred_rf,
           Diferencia=pred_rf-tail(datos$Pedido,n=length(pred_rf)))
tabla_rf

#------------------------------------------------------------------------#
# Calculemos el MAPE con el Random Forest  para pedidos no nulos
#------------------------------------------------------------------------#
datos_mape=tabla_rf[!tabla_rf$Pedido==0,]
performance_rf_r=performance(datos_mape$Prediccion,datos_mape$Pedido)
performance_rf_r
#------------------------------------------------------------------------#
# Calculemos la proporcion de acierto en los  pedidos nulos
#------------------------------------------------------------------------#
proporcion_nulos=(nrow(filter(tabla_rf,Pedido==0,Prediccion==0)))/(nrow(filter(tabla_rf,Pedido==0)))
proporcion_nulos

#-----------------------------------------------------------------------------#
# Tabla para comparar los dos modelos Random Forest
#-----------------------------------------------------------------------------#
tabla_comparaciones=data.frame(tabla_rf_1,tabla_rf)
tabla_comparaciones

#------------------------------------------------------------------------#
# Histograma con las diferencias entre las predicciones y los pedidos
#------------------------------------------------------------------------#
tabla_rf
hist(tabla_rf$Diferencia,
     freq = FALSE,
     breaks = 10,
     xlab = " ",
     ylab=" ",
     main=" ")
lines(density(tabla_rf$Diferencia), col="red",lty = 2, lwd = 3)






