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

#---- Prediccion Cartera 80-20 ----
#-----------------------------------------------------------------------------#
#  Cargamos los datos
#-----------------------------------------------------------------------------#
load("datos.RData") 
datos

#-----------------------------------------------------------------------------#
# Filtramos los datos
#-----------------------------------------------------------------------------#
datos=datos %>% filter(Dias_ultimo_pedido<150) %>% na.omit()
#-----------------------------------------------------------------------------#
# Analizamos el Distribuidor ESB36392850 de Lalin. 
# Vamos a crear un bucle para poder predecir la cartera de productos del 
# siguiente pedido
#-----------------------------------------------------------------------------#
Distribuidor="Distribuidor_1"
Material=c("Producto_1",  "Producto_2", "Producto_3")

pred=list()
for (i in 1:length(Material)){
  #-----------------------------------------------------------------------------#
  # Creamos la variable que nos intersa: Pedido
  #-----------------------------------------------------------------------------#
  datos_brutos=datos %>% group_by(Fecha_pedido) %>%  
    filter(Distribuidor_ID==Distribuidor) %>% 
    mutate(Pedido_litros=ifelse(Material_desc==Material, Litros_pedidos, 0),
           Pedido=ifelse(Material_desc==Material, Cantidad_pedido, 0)) %>% 
    arrange(Fecha_pedido, desc(Pedido)) %>% ungroup()  
  
  datos_brutos
  #-----------------------------------------------------------------------------#
  # Eliminamos duplicados
  #-----------------------------------------------------------------------------#
  datos_limpios=datos_brutos[!duplicated(datos_brutos$Fecha_pedido,
                                         datos_brutos$Pedido),] %>% na.omit()
  datos_limpios
  #-----------------------------------------------------------------------------#
  # Creamos nuevas variables
  #-----------------------------------------------------------------------------#
  datos_distribuidores_material=datos %>% 
    filter(!Distribuidor_ID==Distribuidor) %>% # sacamos a nuestro distribuidor
    filter(Material_desc==Material[i]) %>% 
    group_by(Mes,Ano) %>% 
    mutate(Promedio_mes_distribuidores=mean(Litros_pedidos)) %>% 
    ungroup() %>% 
    group_by(Semana,Ano) %>% 
    mutate(Promedio_semana_distribuidores=mean(Litros_pedidos))

  datos_limpios_material=datos_limpios %>% 
    filter(Material_desc==Material[i]) %>% 
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

  datos[is.na(datos)]=0
  #-----------------------------------------------------------------------------#
  # Ajustamos el modelo Random Forest
  #-----------------------------------------------------------------------------#
  n=nrow(datos)
  p=0.8
  datos_train=datos[1:ceiling(p*n),]
  n1=nrow(datos_train)

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
                          ntree=500,
                          importance = TRUE)
  
    pred_rf[n3]=predict(modelo_rf, newdata = datos_test)
  
    minimo=unique(sort(tail(datos$PEDIDO,n=n2)))[2]
  
    pred_rf[n3]=ifelse(pred_rf[n3]<minimo,0,pred_rf[n3])
  
    pred_rf[n3]=round(pred_rf[n3])
  
    performance_rf[n3]=performance(pred_rf[n3],datos_test$PEDIDO)$MAE
  
    n3=n3+1
}
  pred_rf=pred_rf[!is.na(pred_rf)]
  pred[[i]]=pred_rf
}
#-----------------------------------------------------------------------------#
# Predicciones obtenidas
#-----------------------------------------------------------------------------#
prediccion=as.data.frame(pred, col.names = Material)
#-----------------------------------------------------------------------------#


#---- Prediccion Cartera para los proximos 5 ----
#-----------------------------------------------------------------------------#
#  Cargamos los datos
#-----------------------------------------------------------------------------#
load("datos.RData") 
datos
#-----------------------------------------------------------------------------#
# Filtramos los datos
#-----------------------------------------------------------------------------#
datos=datos %>% filter(Dias_ultimo_pedido<150) %>% na.omit()
#-----------------------------------------------------------------------------#
# Analizamos el Distribuidor_1. 
# Vamos a crear un bucle para poder predecir la cartera de productos del 
# siguiente pedido
#-----------------------------------------------------------------------------#
Distribuidor="Distribuidor_1"
Material=c("Producto_1",  "Producto_2", "Producto_3")

pred=list()
pedido_real=list()

for (i in 1:length(Material)){
  #-----------------------------------------------------------------------------#
  # Creamos la variable que nos interesa: Pedido
  #-----------------------------------------------------------------------------#
  datos_brutos=datos %>% group_by(Fecha_pedido) %>%  
    filter(Distribuidor_ID==Distribuidor) %>% 
    mutate(Pedido_litros = ifelse(Material_desc==Material[i], Litros_pedidos, 0),
           Pedido = ifelse(Material_desc==Material[i], Cantidad_pedido, 0)) %>% 
    arrange(Fecha_pedido, desc(Pedido)) %>% ungroup()  
  
  datos_brutos
  #-----------------------------------------------------------------------------#
  # Eliminamos duplicados
  #-----------------------------------------------------------------------------#
  datos_limpios=datos_brutos[!duplicated(datos_brutos$Fecha_pedido,datos_brutos$Pedido),] %>% na.omit()
  datos_limpios
  #-----------------------------------------------------------------------------#
  # Creamos las siguientes variables
  #-----------------------------------------------------------------------------#
  datos_distribuidores_material=datos %>% 
    filter(!Distribuidor_ID==Distribuidor) %>% # QUITAMOS EL DISTRIBUIDOR QUE ANALIZAMOS
    filter(Material_desc==Material[i]) %>% 
    group_by(Mes,Ano) %>% 
    mutate(Promedio_mes_distribuidores=mean(Litros_pedidos)) %>% 
    ungroup() %>% 
    group_by(Semana,Ano) %>% 
    mutate(Promedio_semana_distribuidores=mean(Litros_pedidos))
  
  datos_limpios_material=datos_limpios %>% 
    filter(Material_desc==Material[i]) %>% 
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
  
  datos[is.na(datos)]=0
  #-----------------------------------------------------------------------------#
  # Ajustamos el Random Forest
  #-----------------------------------------------------------------------------#
  n=nrow(datos)
  
  datos_train=datos[1:(n-5),]
  n1=nrow(datos_train)
  
  datos_test=datos[(n-4):n,]
  n2=nrow(datos_test)
  
  set.seed(15)
  performance_rf=numeric()
  pred_rf=numeric()
  n3=n-5
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
                           ntree=500,
                           importance = TRUE)
    
    pred_rf[n3]=predict(modelo_rf, newdata = datos_test)
    
    minimo=unique(sort(tail(datos$Pedido,n=n1)))[2]
    
    pred_rf[n3]=ifelse(pred_rf[n3]<minimo,0,pred_rf[n3])
    
    pred_rf[n3]=round(pred_rf[n3])
    
    performance_rf[n3]=performance(pred_rf[n3],datos_test$Pedido)$MAE
    
    n3=n3+1
  }
  
  pred_rf=pred_rf[!is.na(pred_rf)]
  
  pred[[i]]=pred_rf
  
  pedido_real[[i]]=datos$Pedido[(n-4):n] 
}
#-----------------------------------------------------------------------------#
# Predicciones
#-----------------------------------------------------------------------------#
prediccion=as.data.frame(pred, col.names = Material)
prediccion
#-----------------------------------------------------------------------------#
# Pedido real
#-----------------------------------------------------------------------------#
pedido_real=as.data.frame(pedido_real, col.names = Material)
pedido_real
#-----------------------------------------------------------------------------#



#---- PREDICCION CARTERA SIN RETROALIMENTAR n=5 ----
#-----------------------------------------------------------------------------#
#  Cargamos los datos
#-----------------------------------------------------------------------------#
load("datos.RData") 
datos
#-----------------------------------------------------------------------------#
# Filtramos los datos
#-----------------------------------------------------------------------------#
datos=datos %>% filter(Dias_ultimo_pedido<150) %>% na.omit()
#-----------------------------------------------------------------------------#
# Analizamos el Distribuidor ESB36392850 de Lalin. 
# Vamos a crear un bucle para poder predecir la cartera de productos del 
# siguiente pedido
#-----------------------------------------------------------------------------#
Distribuidor="Distribuidor_1"
Material=c("Producto_1",  "Producto_2", "Producto_3")

pred=list()
pedido_real=list()

for (i in 1:length(Material)){
  #-----------------------------------------------------------------------------#
  # Creamos la variable que nos interesa: Pedido
  #-----------------------------------------------------------------------------#
  datos_brutos=datos %>% group_by(Fecha_pedido) %>%  
    filter(Distribuidor_ID==Distribuidor) %>% 
    mutate(Pedido_litros = ifelse(Material_desc==Material[i], Litros_pedidos, 0),
           Pedido = ifelse(Material_desc==Material[i], Cantidad_pedido, 0)) %>% 
    arrange(Fecha_pedido, desc(Pedido)) %>% ungroup()  
  
  datos_brutos
  #-----------------------------------------------------------------------------#
  # Eliminamos duplicados
  #-----------------------------------------------------------------------------#
  datos_limpios=datos_brutos[!duplicated(datos_brutos$Fecha_pedido,
                                         datos_brutos$Pedido),] %>% na.omit()
  datos_limpios
  #-----------------------------------------------------------------------------#
  # Creamos nuevas variables
  #-----------------------------------------------------------------------------#
  datos_distribuidores_material=datos %>% 
    filter(!Distribuidor_ID==Distribuidor) %>% # sacamos nuestro distribuidor
    filter(Material_desc==Material[i]) %>% 
    group_by(Mes,Ano) %>% 
    mutate(Promedio_mes_distribuidores=mean(Litros_pedidos)) %>% 
    ungroup() %>% 
    group_by(Semana,Ano) %>% 
    mutate(Promedio_semana_distribuidores=mean(Litros_pedidos))
  
  datos_limpios_material=datos_limpios %>% 
    filter(Material_desc==Material[i]) %>% 
    group_by(Mes,Ano) %>% 
    mutate(Promedio_mes=mean(Pedido)) %>% 
    ungroup() %>% 
    group_by(Semana,Ano) %>% 
    mutate(Promedio_semana=mean(Pedido))
  
  datos=datos_limpios %>% 
    left_join(datos_limpios_material[,c("Mes","Ano","Promedio_mes")],
              by=c("Mes"="Mes","Ano"="Ano")) %>% unique() %>% 
    left_join(datos_distribuidores_material[,c("Mes","Ano","Promedio_mes_distribuidores")],
              by=c("Mes"="Mes", "Ano"="Ano")) %>% unique() %>% 
    left_join(datos_limpios_material[,c("Semana","Ano","Promedio_semana")],
              by=c("Semana"="Semana","Ano"="Ano")) %>% unique() %>% 
    left_join(datos_distribuidores_material[,c("Semana","Ano","Promedio_semana_distribuidores")],
              by=c("Semana"="Semana","Ano"="Ano")) %>% unique()
  
  datos[is.na(datos)]=0
  
  #-----------------------------------------------------------------------------#
  # Ajustamos un Random Forest sencillo
  #------------------------------------------------------------------------#
  n=nrow(datos)
  
  datos_train=datos[1:(n-5),]
  n1=nrow(datos_train)
  
  datos_test=datos[(n-4):n,]
  n2=nrow(datos_test)
  
  set.seed(15)
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
                         ntree=500,
                         importance = TRUE)
  
  pred_rf=predict(modelo_rf, newdata = datos_test)
  
  minimo=unique(sort(tail(datos$Pedido,n=n1)))[2]
  
  pred_rf=ifelse(pred_rf<minimo,0,pred_rf)
  
  pred_rf=round(pred_rf)
  
  performance_rf=performance(pred_rf,datos_test$Pedido)$MAE
  #-----------------------------------------------------------------------------#
  # Guardamos en la lista los resultados obtenidos
  #-----------------------------------------------------------------------------#
  pred[[i]]=pred_rf
  #-----------------------------------------------------------------------------#
  # Guardamos en la lista los pedidos reales 
  #-----------------------------------------------------------------------------#
  pedido_real[[i]]=datos$Pedido[(n-4):n] 
}
#-----------------------------------------------------------------------------#
# Predicciones
#-----------------------------------------------------------------------------#
prediccion=as.data.frame(pred, col.names = Material)
prediccion
#-----------------------------------------------------------------------------#
# Pedidos reales
#-----------------------------------------------------------------------------#
pedido_real=as.data.frame(pedido_real, col.names = Material)
pedido_real
                      
