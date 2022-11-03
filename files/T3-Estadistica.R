
#Resaltar el peligro que representan las motocicletas para la gente joven

#>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>HISTOGRAMA
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#Datos
datos <- read.csv("B:/DOCS/T3-Estadistica.csv")
edades <- c(datos$Edades)

#Número de clases
numClases <- round(1 + 3.322*log10(length(datos$Edades)))

#Ancho de clase
A = diff(range(edades))/numClases

#Punto de partida || puede ser un val < limiteInferior1
# 14 - .5 = 13.5
L1 <- min(edades)-.5

#Limites de clase
LimitesClase <- L1 + A*(0:numClases)

#Calculo de marcas de clase
calcularMedia <- function(val1, val2){
  media <- (val1 + val2)/2
  return(media)
}

mClase1 <- calcularMedia(LimitesClase[1], LimitesClase[2])
mClase2 <- calcularMedia(LimitesClase[2], LimitesClase[3])
mClase3 <- calcularMedia(LimitesClase[3], LimitesClase[4])
mClase4 <- calcularMedia(LimitesClase[4], LimitesClase[5])
mClase5 <- calcularMedia(LimitesClase[5], LimitesClase[6])
mClase6 <- calcularMedia(LimitesClase[6], LimitesClase[7])

#Lista de marcas
listaMarcas <- c(mClase1, mClase2, mClase3, mClase4, mClase5, mClase6)

#Tabla de Frecuencias
tabla <- transform(table(cut(edades, breaks = numClases)))

#Lista de frecuencias
listaFrecuencias <- c(tabla$Freq)

#Copia columna Edades
listaEdades <- c(edades)

#Replica datos
edadesFreq <- rep(listaMarcas, listaFrecuencias)

#Histograma
histograma <- hist(edadesFreq,
                   breaks = LimitesClase,
                   main = "Edades con más accidentes en motocicleta")
histograma

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Diagrama de tallo y hoja
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<

arbol_x1 <- c(edades)
diagrama_Arbol <- stem(example_x1)
diagrama_Arbol

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Tabla de frecuencias con FDT || fdt: Frequency distribution table for numerical data
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Documentación -> https://www.rdocumentation.org/packages/fdth/versions/1.2-6/topics/fdt

datos2 <- read.csv("B:/DOWNLOADS/T1-P1-Estadistica.csv")
listaNiveles2 <- c(datos2$Nivel)

valMinino <- min(listaNiveles2)
valMax <- max(listaNiveles2)
#Tamanio de elementos
nElementos2 <- length(datos2$Individuo)

#Número de clases
numClases2 <- round(1 + 3.322*log10(nElementos2))

#Ancho de clase
A2 = diff(range(listaNiveles2))/numClases2

#Tabla de distribucion de frecuencias con FDT
distribucion <- fdt(listaNiveles2,
                    start = valMinimo,
                    end = valMax+1,
                    h = A2)
distribucion

#>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>DIAGRANA DE PASTEL
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#Vector de proporciones
listaFR <- c(distribucion$table$`rf(%)`)

#Vector de etiquetas
listaClases <- c(distribucion$table$`Class limits`)

#pct2
pct2 <- round(listaFR/sum(listaFR)*100)

#nuevos
listaClases <- paste(listaClases, pct2)
listaClases <- paste(listaClases, "%", sep = "")

#grafica
pie(listaFR,
    labels= listaClases,
    col=rainbow(length(listaClases)),
    main="Grafica de pastel edades")

#Cuadro de resumen
legend("topright",
       c(listaClases),
       cex=0.9,
       fill = rainbow((length(listaFR))))


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Diagrama de caja
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<
diagramaCaja <- plot(x=tabla$Var1, y=tabla$Freq,
                     main="Edades con más frecuencia de accidentes en motocicleta",
                     xlab = "Rango de edades",
                     ylab = "Frecuencia",
                     col = c("orange3", "yellow3", "green3", "grey", "blue", "red"))



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Coeficiente de variación de Pearson
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<

