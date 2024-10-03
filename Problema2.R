#Codigo para problema 2

mis_dades <- iris
mis_dades

x <- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length
#quiero saber la correlación entre ellos

plot(x,y)

#CÁLCULO DE LA PENDIENTE= sum((x-PROMEDIO DE X)*(y-PROMEDIO DE Y))/sum((x-PROMEDIO DE X)^2)
x_bar <- mean(x)
y_bar <- mean(y)
#al poner bar me refiero a x/y barra, es decir el promedio
m <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
m
#resultado m= 0.4089223

#CÁLCULO DE B= PROMEDIO DE Y- PENDIENTE*PROMEDIO DE X
b <- y_bar- m*x_bar
b
#resultado b=4.306603

#PREDICCIÓN:
m*1.5+b #he aislado la y. me daban la x
#resultado= 4.919987

#otra manera
y_pred <- predict(mod, data.frame(x=1.5))
y_pred


x_pred <- x
y_pred <- m*x_pred+b
plot(x,y)
lines(x_pred, y_pred) #linea de regresión

#Qué tan buena es la predicción? esta medida nos la proporciona el coeficiente de determinación
Rsq <- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)
Rsq

#el coeficiente de correlación es la raiz de
cor<- sqrt(Rsq)
cor
#otra manera de calcularlo:
cor.test(x,y)

lm(y~x) #me da directamente la pendiente y b
#incercep es la pendiente
#la x es b

mod<- lm(y~x)
summary(mod)



