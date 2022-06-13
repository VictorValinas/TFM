#------------------------------------------------------------------#
# Cargamos las librerias necesarias
#------------------------------------------------------------------#
library(ggplot2)
library(ggformula)
library(dplyr)
library(tree) 
library(ROCR)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
library(grDevices)
library(parsnip)
library(parttree)
library(MASS)
library(boot)
library(randomForest)
library(nnet)
library(NeuralNetTools)
#------------------------------------------------------------------#


#------------------------------------------------------------------#
# Ilustracion del fenomeno "overfitting". 
#------------------------------------------------------------------#
# Las suposiciones son las siguientes: 
# 
# Y=x+eps
# n=200
# X~U(0,1)
# f(X)=X
# Epsilon~N(0,1/10)
#------------------------------------------------------------------#
n=150
set.seed(15)  # semilla para poder replicar esta simulacion
x=runif(n)    # simulamos 200 valores de una U(0,1)
eps=rnorm(n, mean = 0, sd=1/20)
y=x+eps
#------------------------------------------------------------------#
# Ajuste de regresion lineal
#------------------------------------------------------------------#
mod_lineal=lm(y~x)
#------------------------------------------------------------------#
# Ajuste de suavizacion spline
#------------------------------------------------------------------#
mod_smooth_spline = smooth.spline(y ~ x, df=50,cv=TRUE)

#------------------------------------------------------------------#
# Grafico
#------------------------------------------------------------------#
datos=data.frame(x,y)

ggplot(datos,aes(x,y))+
  geom_point()+
  geom_smooth(method = "lm",
              se = FALSE,
              col="red",
              lwd=1)+
  geom_spline(stat = "spline",
              df=50,
              col="blue",
              lwd=1)+
  xlab("")+
  ylab("")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))

#------------------------------------------------------------------#
# Veamos la calidad de cada ajuste
#------------------------------------------------------------------#
a=(y-fitted.values(mod_lineal))^2
mse_lineal=1/n*sum(a)
mse_lineal

b=(y-fitted.values(mod_smooth_spline))^2
mse_spline=1/n*sum(b)
mse_spline
#------------------------------------------------------------------#
# Creamos el conjunto de valicacion para testear los modelos
#------------------------------------------------------------------#
n2=5000
set.seed(15)
x_2=runif(n2)
eps_2=rnorm(n2,mean = 0, sd=1/20)
y_2=x_2+eps_2
#------------------------------------------------------------------#
# Grafico
#------------------------------------------------------------------#
datos_2=data.frame(x=x_2, y=y_2)

ggplot(datos_2,aes(x,y))+
  geom_point()+
  geom_smooth(data=datos,
              method = "lm",
              se = FALSE,
              col="red",
              lwd=1)+
  geom_smooth(data = datos,
              stat = "spline",
              df=50,
              col="blue",
              lwd=1)+
  xlab("")+
  ylab("")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))
  
#------------------------------------------------------------------#
# Realizamos las predicciones y medimos su calidad
#------------------------------------------------------------------#
pred_lineal=predict(mod_lineal,newdata = datos_2)

a_2=(y_2-pred_lineal)^2
mse_lineal_2=1/n2*sum(a_2)

pred_spline=predict(mod_smooth_spline,x_2)$y
b_2=(y_2-pred_spline)^2
mse_spline_2=1/n2*sum(b_2)
#------------------------------------------------------------------#


#----------------------------------------------------------------# 
# Ilustracion del compromiso entre el sesgo y la varianza
#----------------------------------------------------------------# 
# Las suposiciones son las siguientes: 

# Y=x+eps
# n=500
# X~U(0,1)
# f(X)=X^4
# Epsilon~N(0,1/10)
#----------------------------------------------------------------# 
n=5000
set.seed(15) # fijamos una semilla para poder replicar esta simulacion
x=runif(n)   # simulamos 200 valores de una U(0,1)
eps=rnorm(n,mean = 0, sd=1/20)
y=x^4+eps
#------------------------------------------#
# Ajustamos el modelo lineal
#------------------------------------------#
mod_lineal=lm(y~x)
#------------------------------------------#
# Ajustamos el modelo de suavizacion spline
#------------------------------------------#
mod_smooth_spline=smooth.spline(y ~ x, df=50)

#------------------------------------------#
# Grafico con el ajuste lineal
#------------------------------------------#
datos=data.frame(x,y)

ggplot(datos,aes(x,y))+
  geom_point()+
  geom_smooth(data=datos,
              method = "lm",
              se = FALSE,
              col="red",
              lwd=1)+
  xlab("")+
  ylab("")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))

#----------------------------------------------------------------# 
# Grafico comparacion ajuste lineal y spline
#----------------------------------------------------------------# 
datos=data.frame(x,y)

ggplot(datos,aes(x,y))+
  geom_point()+
  geom_smooth(data=datos,
              method = "lm",
              se = FALSE,
              col="red",
              lwd=1)+
  geom_smooth(data=datos,
              stat = "spline",
              df=50,
              col="blue",
              lwd=1)+
  xlab("")+
  ylab("")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))
#----------------------------------------------------------------# 


#----------------------------------------------------------------# 
# Ilustramos el arbol de regresion. 
#----------------------------------------------------------------# 
# Cargamos los datos con los que vamos a trabajar. 
# 
# Luego creamos un conjunto de datos con las variables de interes
# 
# Finalmente creamos le arbol de regresion.
#----------------------------------------------------------------# 
load("datos.RData") 

datos=datos %>% filter(Dias_ultimo_pedido<150) %>% 
  na.omit()

 Distribuidor="Distribuidor_1"   # 1º Arbol de Regresion
 Distribuidor="Distribuidor_2"   # 2º Arbol de Regresion 
 
 
Material="Producto_1"    

#----------------------------------------------------------------# 
# Creamos la variable Pedido
#----------------------------------------------------------------# 
datos_brutos=datos %>% group_by(Fecha_pedido) %>%  
  filter(Distribuidor_ID==Distribuidor) %>% 
  mutate(Pedido=ifelse(Material_desc==Material, 
                       Litros_pedidos, 0)) %>% 
  arrange(Fecha_pedido, desc(Pedido)) %>% ungroup()  

#----------------------------------------------------------------# 
# Eliminamos duplicados 
#----------------------------------------------------------------# 
datos_limpios=datos_brutos[!duplicated(datos_brutos$Fecha_pedido,
                                       datos_brutos$Pedido),] %>% 
  na.omit()
#----------------------------------------------------------------# 
# Eliminamos NA'S
#----------------------------------------------------------------# 
datos_limpios[is.na(datos_limpios)]=0

#----------------------------------------------------------------# 
# Seleccionamos las variables que nos interesan
#----------------------------------------------------------------# 
datos=datos_limpios[,c("Pedido","Litros_1_pedido_antes","Covid",
                       "Litros_2_pedidos_antes","Semana","Estacion",
                       "Litros_3_pedidos_antes","Dias_ultimo_pedido")]
datos

#----------------------------------------------------------------# 
# Arbol de Regresion
#----------------------------------------------------------------# 
fancyRpartPlot(rpart(Pedido~Litros_1_pedido_antes+
                       Litros_2_pedidos_antes+
                       Estacion,
                     data = datos),
               yesno=2,
               type=1,
               split.col="black",
               nn.col="black", 
               main=" ",
               caption="",
               palette="YlOrBr",
               branch.col="black")

#----------------------------------------------------------------# 
# Grafico espacio predictor
#----------------------------------------------------------------# 
tree =decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression") %>%
  fit(Pedido~Litros_1_pedido_antes+
        Litros_2_pedidos_antes+
        Estacion,
      data = datos)


datos %>%
  ggplot(aes(x=Litros_1_pedido_antes, y=Litros_2_pedidos_antes),
         cex=1.5, col="Orange") +
  geom_jitter(aes(col=Pedido), alpha=0.8,cex=3) +
  geom_parttree(data = tree,alpha=0.2,cex=1.2,
                aes(fill=Pedido), show.legend = FALSE) +
  scale_colour_distiller(palette = "Oranges")+
  scale_fill_distiller(palette = "Oranges") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))+
  xlab("") + 
  ylab("") +
  labs(col='Pedido',size=15)
#----------------------------------------------------------------#



#----------------------------------------------------------------# 
# Ilustracion del  método bootstrap 
#----------------------------------------------------------------# 
# Vamos a generar 100 pares de distribuciones normales de media 
# cero. La matriz de covarianzas viene dada por los siguientes
# valores:
#
# sigma^2_x=1
# sigma^2_y=1.5
# sigma_xy=cov(x,y)=0.5
#
# Utilizaremos la funcion mvrnorm de la libreria MASS
#------------------------------------------#
n=100
mu=c(0,0)
sigma=matrix(c(1,0.5,0.5,1.25), ncol=2)
alpha=(sigma[2,2]-sigma[1,2])/(sigma[1,1]+sigma[2,2]-2*sigma[1,2])

set.seed(15)
datos=mvrnorm(n,mu,sigma)

set.seed(15)
alpha_est=numeric()
for (i in 1:1000){
  datos=mvrnorm(n,mu,sigma)
  # parametros estimados
  mu_est=colMeans(datos)
  sigma_est=cov(datos)
  # alpha estimado
  alpha_est[i]=(sigma_est[2,2]-sigma_est[1,2])/(sigma_est[1,1]+sigma_est[2,2]-2*sigma_est[1,2])
  alpha_est[i]
}

alpha_est
media_alpha_est=mean(alpha_est)
sd_alpha_est=sd(alpha_est)

#----------------------------------------------------------------# 
# creamos el histograma idealizado utilizando ggplot
#----------------------------------------------------------------# 
alpha_est=data.frame(alpha_est, metodo="Poblacional")


histogrma=ggplot(alpha_est,aes(x=alpha_est))+
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.03,
                 col='black', 
                 fill='khaki2', 
                 alpha=0.7)+
  geom_vline(xintercept = 0.5996698, 
             col="black",
             lwd=1.2,
             linetype=2) +
  geom_density(lwd = 1.2,
               linetype = 1,
               colour = "blue")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))+
  xlab("") + 
  ylab("") 
  
histogrma


kernel=ggplot(alpha_est,aes(x=alpha_est))+
  #geom_vline(xintercept = 0.5996698, 
  #           col="blue",
  #           lwd=1.3) +
  geom_density(lwd = 1.2,
               linetype = 1,
               colour = "blue")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))+
  xlab("") + 
  ylab("") 

kernel


#----------------------------------------------------------------# 
# Aproximacion utilizando bootstrap
#----------------------------------------------------------------# 
set.seed(15)
datos=mvrnorm(n,mu,sigma) # datos originales
datos=data.frame(X=datos[,1],Y=datos[,2]) 
#----------------------------------------------------------------# 
# funcion para calcular alpha estimado
#----------------------------------------------------------------# 
alpha.fn <- function(data , index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}

alpha.fn(datos,1:100)
#----------------------------------------------------------------# 
# creamos una muestra bootstrap a partir de los  datos originales
#----------------------------------------------------------------# 
set.seed(15)
alpha.fn(datos , sample (100 , 100, replace = T))

set.seed(15)
aproximacion_boot=boot(datos, alpha.fn, R = 1000)

alpha_est_boot=aproximacion_boot$t
media_alpha_est_boot=mean(alpha_est_boot)
sd_alpha_est_boot=sd(alpha_est_boot)

#----------------------------------------------------------------# 
# Creamos el histograma bootstrap utilizando ggplot
#----------------------------------------------------------------# 
alpha_est_boot=data.frame(alpha_est_boot, metodo="Bootstrap")

histogrma_boot=ggplot(alpha_est_boot,aes(x=alpha_est_boot))+
  geom_histogram(aes(y = ..density..), 
                 binwidth = 0.03,
                 col='black', 
                 fill='khaki2', 
                 alpha=0.7)+
  geom_vline(xintercept = 0.5932785, 
             col="black",
             lwd=1.2,
             linetype=2)+
  geom_density(lwd = 1.2,
               linetype = 1,
               colour = "red")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))+
  xlab("") + 
  ylab("") 

histogrma_boot


kernel_boot=ggplot(alpha_est_boot,aes(x=alpha_est_boot))+
  geom_density(lwd = 1.2,
               linetype = 1,
               colour = 2)+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))+
  xlab("") + 
  ylab("") 

kernel_boot


colnames(alpha_est)=c("alpha","Método")
colnames(alpha_est_boot)=c("alpha","Método")

comparacion_kernel=ggplot(data=alpha_est, 
                          aes(x=alpha, 
                              col=Método))+
  geom_density(lwd = 1.2,
               linetype = 1)+
  geom_density(data = alpha_est_boot,
               aes(x=alpha, 
                   col=Método),
               lwd = 1.2,
               linetype = 1)+
  scale_color_manual(values=c("red","blue"))+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0),
        legend.title = element_text (size = 13),
        legend.text = element_text (size = 11))+
  xlab("") + 
  ylab("")

comparacion_kernel
#----------------------------------------------------------------# 




#----------------------------------------------------------------# 
# Ilustracion del  procedimiento bagging y random forest para el primer 
# arbol de regresion
#----------------------------------------------------------------# 
load("datos.RData") 

datos=datos %>% 
  filter(Dias_ultimo_pedido<150) %>% 
  na.omit()
#----------------------------------------------------------------# 
# Seleccionamos Distribuidor y Producto
#----------------------------------------------------------------# 
Distribuidor="Distribuidor_1"   # arbol regresion 1
# Distribuidor="Distribuidor_2"   # arbol regresion 2

Material="Producto_1"
#----------------------------------------------------------------# 
# CREAMOS LA VARIABLE PEDIDO 
#----------------------------------------------------------------# 
datos_brutos=datos %>% group_by(Fecha_pedido) %>%  
  filter(Distribuidor_ID==Distribuidor) %>% 
  mutate(Pedido=ifelse(Material_desc==Material, Litros_pedidos, 0)) %>% 
  arrange(Fecha_pedido, desc(Pedido)) %>% ungroup()  
#----------------------------------------------------------------# 
# Eliminamos duplicados
#----------------------------------------------------------------# 
datos_limpios=datos_brutos[!duplicated(datos_brutos$Fecha_pedido,datos_brutos$Pedido),] %>% 
  na.omit()
#----------------------------------------------------------------# 
# Eliminamos NA'S
#----------------------------------------------------------------# 
datos_limpios[is.na(datos_limpios)]=0
#----------------------------------------------------------------# 
# Seleccionamos las variables que nos interesan
#----------------------------------------------------------------# 
datos=datos_limpios[,c("Pedido","Litros_1_pedido_antes","Covid",
                       "Litros_2_pedidos_antes","Semana","Estacion",
                       "Litros_3_pedidos_antes","Dias_ultimo_pedido")]
#----------------------------------------------------------------# 
# Creamos nuestro modelo Bagging
#----------------------------------------------------------------# 
# Para aplicar el procedimiento bagging se tiene que cumplir que:
# mtry=p=numero_predictores
#----------------------------------------------------------------# 
set.seed(15)

modelo_bagging=randomForest(Pedido~Litros_1_pedido_antes+
                              Litros_2_pedidos_antes+
                              Estacion,
                            data = datos,
                            mtry=3,
                            importante=TRUE,
                            ntree = 500)


datos_error_1=data.frame(Árboles=1:500,Error=modelo_bagging$mse,
                          Método="Bagging")

grafica_bagging=ggplot(datos_error_1,aes(x=Árboles,y=Error))+
  geom_line(col="red", lwd=1.2)+
  xlab("")+
  ylab("")+
  #ggtitle("Método Bagging") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))
  
grafica_bagging


lista_predictores=data.frame("Predictor"= c("Litros 1 pedido antes",
                                            "Litros 2 pedidos antes",
                                            "Estacion"),
                             "Valor"=unname(modelo_bagging$importance))

modelo_bagging$importance

importancia_bagging=lista_predictores %>%
  dplyr::arrange(desc(Valor)) %>%
  ggplot(aes(reorder(Predictor, Valor), Valor)) +
  geom_col(fill="blue") +
  coord_flip() +
  xlab("") +
  ylab("") +
  #ggtitle("Variables más importantes del Bagging") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=15),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))

importancia_bagging

#----------------------------------------------------------------# 
# Creamos el modelo Random Forest
#----------------------------------------------------------------# 
set.seed(15)
modelo_rf=randomForest(Pedido~Litros_1_pedido_antes+
                         Litros_2_pedidos_antes+
                         Estacion,
                       data = datos,
                       importante=TRUE,
                       ntree = 500)

datos_error_2=data.frame(Árboles=1:500,Error=modelo_rf$mse,
                          Método="Random Forest")

grafica_rf=ggplot(datos_error_2,aes(x=Árboles,y=Error))+
  geom_line(col="red", lwd=1.2)+
  xlab("")+
  ylab("")+
  #ggtitle("Método Random Forest") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))

grafica_rf


lista_predictores=data.frame("Predictor"= c("Litros 1 pedido antes",
                                            "Litros 2 pedidos antes",
                                            "Estacion"),
                             "Valor"=unname(modelo_rf$importance))
modelo_rf$importance

importancia_rf=lista_predictores %>%
  dplyr::arrange(desc(Valor)) %>%
  ggplot(aes(reorder(Predictor, Valor), Valor)) +
  geom_col(fill="blue") +
  coord_flip() +
  xlab("") + ylab("") +
  #ggtitle("Variables más importantes del Random Forest") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=15),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))

importancia_rf

#----------------------------------------------------------------# 
# Comparativa entre el metodo Bagging y el Random Forest
#----------------------------------------------------------------# 
datos_error=rbind(datos_error_1,datos_error_2)

ggplot(datos_error_1,aes(x=Árboles,y=Error, col=Método))+
  geom_line(lwd=1.2)+
  geom_line(data=datos_error_2,
            aes(x=Árboles,y=Error, col=Método),
            lwd=1.2)+
  scale_color_manual(values=c('blue','red'))+
  xlab("")+
  ylab("")+
  #ggtitle("Comparativa entre el Bagging y el Random Forest")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0),
        legend.title = element_text (size = 13),
        legend.text = element_text (size = 11))

#----------------------------------------------------------------# 



#----------------------------------------------------------------# 
# Ilustracion de las funciones de activacion para las redes neuronales. 
#----------------------------------------------------------------# 

#----------------------------------------------------------------# 
# Funcion sigmoide
#----------------------------------------------------------------# 
ggplot() +
  xlim(-10, 10) +
  geom_function(
    aes(color = "Normal"),
    fun =~ 1/(1+exp(-.x)),
    linetype="solid",
    size=1.5,
    show.legend = FALSE) +
  ylab("") + 
  #ggtitle("Función Sigmoide") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))

#----------------------------------------------------------------# 
# Funcion ReLU
#----------------------------------------------------------------# 
ggplot() +
  xlim(-10, 10) +
  geom_function(
    aes(color = "Normal"),
    fun =~ case_when(.x<=0~0,
                     .x>0~.x),
    linetype="solid",
    size=1.5,
    show.legend = FALSE)+
  ylab("") + 
  #ggtitle("Función ReLU") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))


#----------------------------------------------------------------# 
# Funcion ReLU y variantes LReLU y PReLU
#----------------------------------------------------------------# 
alpha=0.2
ggplot() +
  xlim(-10, 10) +
  geom_function(
    aes(color = "ReLU"),
    fun =~ case_when(.x<=0~0,
                     .x>0~.x),
    linetype="solid",
    size=1.5) +
  ylab("") + 
  geom_function(
    aes(color = "LReLU"),
    fun =~ case_when(.x<=0~ alpha*.x,
                     .x>0~.x),
    linetype="solid",
    size=1.5) +
  ylab("") + 
  geom_function(
    aes(color = "ELU"),
    fun =~ case_when(.x<=0~ alpha*(exp(.x)-1),
                     .x>0~.x),
    linetype="solid",
    size=1.5,
    show.legend = FALSE)+
  ylab("") + 
  #ggtitle("ReLU, LReLU y ELU") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0),
        legend.title = element_text (size = 13),
        legend.text = element_text (size = 11))+
  labs(color="Función")
  

#----------------------------------------------------------------# 
# Funcion ELU
#----------------------------------------------------------------# 
alpha=0.2
ggplot() +
  xlim(-10, 10) +
  geom_function(
    aes(color = "Normal"),
    fun =~ case_when(.x<=0~ alpha*(exp(.x)-1),
                     .x>0~.x),
    linetype="solid",
    size=1.5,
    show.legend = FALSE)+
  ylab("") + 
  ggtitle("Función ELU") +
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0))


#----------------------------------------------------------------# 
# Modificacion Funcion sigmoide
#----------------------------------------------------------------# 
ggplot() +
  xlim(-10, 10) +
  geom_function(
    aes(color = "blue"),
    fun =~ 1/(1+exp(-.x)),
    linetype="solid",
    size=1.5,
    show.legend = FALSE) +
  ylab("") + 
  geom_function(
    aes(color= "red"),
    fun =~ 1/(1+exp(-0.5*.x)),
    linetype="solid",
    size=1.5,
    show.legend = FALSE) +
  ylab("") + 
  geom_function(
    aes(color= "green"),
    fun =~ 1/(1+exp(-10*.x)),
    linetype="solid",
    size=1.5,
    show.legend = FALSE) +
  ylab("") + 
  #ggtitle("Variantes función Sigmoide")+
  theme_test()+
  #theme_classic()+
  theme(axis.text = element_text(size=13),
        axis.title.x = element_text(size=0),
        axis.title.y = element_text(size=0),
        legend.title = element_text (size = 13),
        legend.text = element_text (size = 11))
#----------------------------------------------------------------# 



#----------------------------------------------------------------# 
# Ilustracion de un ejemplo de red neuronal con los mismos datos
# que hemos utilizado para realizar el argol de regresion 1
#----------------------------------------------------------------# 
load("datos.RData") 

datos=datos %>% 
  filter(Dias_ultimo_pedido<150) %>% 
  na.omit()
#----------------------------------------------------------------# 
# Seleccionamos Distribuidor y Producto
#----------------------------------------------------------------# 
Distribuidor="Distribuidor_1"   # arbol regresion 1
# Distribuidor="Distribuidor_2"   # arbol regresion 2

Material="Producto_1"
#----------------------------------------------------------------# 
# CREAMOS LA VARIABLE PEDIDO 
#----------------------------------------------------------------# 
datos_brutos=datos %>% group_by(Fecha_pedido) %>%  
  filter(Distribuidor_ID==Distribuidor) %>% 
  mutate(Pedido=ifelse(Material_desc==Material, Litros_pedidos, 0)) %>% 
  arrange(Fecha_pedido, desc(Pedido)) %>% ungroup()  
#----------------------------------------------------------------# 
# Eliminamos duplicados
#----------------------------------------------------------------# 
datos_limpios=datos_brutos[!duplicated(datos_brutos$Fecha_pedido,datos_brutos$Pedido),] %>% 
  na.omit()
#----------------------------------------------------------------# 
# Eliminamos NA'S
#----------------------------------------------------------------# 
datos_limpios[is.na(datos_limpios)]=0
#----------------------------------------------------------------# 
# Seleccionamos las variables que nos interesan
#----------------------------------------------------------------# 
datos=datos_limpios[,c("Pedido","Litros_1_pedido_antes","Covid",
                       "Litros_2_pedidos_antes","Semana","Estacion",
                       "Litros_3_pedidos_antes","Dias_ultimo_pedido")]

set.seed(15)

neural=nnet(Pedido~Litros_1_pedido_antes+Litros_2_pedidos_antes+
              +Estacion,
            data=datos,
            size=2,
            linout=FALSE, # permite utilizar la identidad como funcion
            # de activacion en los nodos finales
            skip=TRUE,
            decay=1e-5, 
            maxit=1000, 
            err.fct = "sse")

#----------------------------------------------------------------# 
# Grafico red neuronal
#----------------------------------------------------------------# 
plotnet(neural,
        x_names = c("Litros 1 pedido antes","Litros 2 pedidos antes",
                    "Otoño", "Primavera", "Verano"),
        y_names="Pedido",
        circle_cex=5,
        cex_val=1,
        pos_col="black",
        neg_col="grey",
        pad_x=0.7)



