datos <- read.arff("Waveform.arff")
head(datos)

columnas.num <- sapply(c(1:ncol(datos)) , function(x) is.numeric(datos[, x]))
columnas.num
datos.num <- datos[, columnas.num]
columnas.a.quitar <- c(22)
datos.num <- datos.num[,-columnas.a.quitar]
datos.num <- na.omit(datos.num) #No tiene NAs, por lo que no elimina nada
summary(datos.num)

#Pintar histogramas de las variables
pdf("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/histogramas_waveform.pdf", width = 10, height = 8)
par(mfrow = c(2, 2))

# Crear histogramas para las columnas seleccionadas
sapply(1:ncol(datos.num), function(i) {
  hist(datos.num[[i]], main = "", # Vacío
    xlab = names(datos.num)[i], ylab = "Frecuencia", border = "black")
})
dev.off()

#Selección de la cuarta variable
indice.columna <- 4
columna <- datos.num[, indice.columna]
nombre.columna <- names(datos.num) [indice.columna]

#Outliers IQR
cuartil.primero <- quantile(columna, 0.25)
cuartil.tercero <- quantile(columna, 0.75)
iqr <- IQR(columna)
extremo.superior.outlier.IQR <- cuartil.tercero + 1.5*iqr 
extremo.inferior.outlier.IQR <- cuartil.primero - 1.5*iqr
extremo.superior.outlier.IQR.extremo <- cuartil.tercero + 3*iqr
extremo.inferior.outlier.IQR.extremo <- cuartil.primero - 3*iqr
son.outliers.IQR <- ((columna < extremo.inferior.outlier.IQR) | (columna > extremo.superior.outlier.IQR))
son.outliers.IQR.extremos <- ((columna < extremo.inferior.outlier.IQR.extremo) | (columna > extremo.superior.outlier.IQR.extremo))
claves.outliers.IQR <- which(son.outliers.IQR)
claves.outliers.IQR.extremos <- which(son.outliers.IQR.extremos)
df.outliers.IQR <- datos.num[claves.outliers.IQR,]
df.outliers.IQR.extremos <- datos.num[claves.outliers.IQR.extremos,]
nombres.outliers.IQR <- row.names(df.outliers.IQR)
nombres.outliers.IQR.extremos <- row.names(df.outliers.IQR.extremos)
valores.outliers.IQR <- columna[claves.outliers.IQR]
valores.outliers.IQR.extremos <- columna[claves.outliers.IQR.extremos]

cuartil.primero
cuartil.tercero
iqr
extremo.superior.outlier.IQR
extremo.inferior.outlier.IQR
extremo.superior.outlier.IQR.extremo
extremo.inferior.outlier.IQR.extremo
claves.outliers.IQR
claves.outliers.IQR.extremos
df.outliers.IQR
df.outliers.IQR.extremos
nombres.outliers.IQR
nombres.outliers.IQR.extremos
valores.outliers.IQR
valores.outliers.IQR.extremos

son.outliers.IQR <- son_outliers_IQR (datos.num, indice.columna)
claves.outliers.IQR <- claves_outliers_IQR (datos.num, indice.columna)
son.outliers.IQR.extremos <- son_outliers_IQR (datos.num, indice.columna, 3)
claves.outliers.IQR.extremos <- claves_outliers_IQR (datos.num, indice.columna, 3)

#3.1.4
datos.num.zscore <- scale(datos.num)
columna.norm <- datos.num.zscore[, indice.columna]
valores.outliers.IQR.norm <- columna.norm[son.outliers.IQR]
datos.num.zscore.outliers.IQR <- datos.num.zscore[son.outliers.IQR, ]

#3.1.5
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/grafico1.png")
grafico1 <- plot_2_colores(columna.norm, claves.outliers.IQR, "att4", )
dev.off()
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/grafico2.png")
grafico2 <- plot_2_colores(columna.norm, claves.outliers.IQR.extremos, "att4", )
dev.off()

#3.1.6
boxplot1 <- diag_caja_outliers_IQR(datos.num, indice.columna)
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/boxplot1.png")
boxplot1
dev.off()
boxplot2 <- diag_caja(datos.num, indice.columna, claves.outliers.IQR)
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/boxplot2.png")
boxplot2
dev.off()
boxplot2.extremos <- diag_caja(datos.num, indice.columna, claves.outliers.IQR.extremos)
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/boxplot2_extremos.png")
boxplot2.extremos
dev.off()
boxplot.junto <- diag_caja_juntos(datos.num, "Outliers en alguna columna", claves.outliers.IQR)
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/boxplot_junto.png", width = 2000, height = 1200, res = 300)
boxplot.junto
dev.off()

#3.2
#3.2.1
test.Grubbs = grubbs.test(columna, two.sided = TRUE)
test.Grubbs$p.value
valor.posible.outlier = outlier(columna)
valor.posible.outlier
es.posible.outlier = outlier(columna, logical = TRUE)
clave.posible.outlier = which(es.posible.outlier == TRUE)
clave.posible.outlier

#3.2.2
datos.num.sin.outlier <- datos.num[-clave.posible.outlier,]
columna.sin.outlier <- datos.num.sin.outlier[,indice.columna]
columna.sin.outlier
ajusteNormal = fitdist(columna.sin.outlier , "norm")
denscomp (ajusteNormal,  xlab = nombre.columna)
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/Graf_QQ.png")
ggqqplot(columna.sin.outlier) 
dev.off()
shapiro.test(columna.sin.outlier)

#3.2.3
#######################################################################
# Aplica el test de Grubbs sobre la columna ind.col de datos y devuelve una lista con:

# nombre.columna: Nombre de la columna datos[, ind.col]
# clave.mas.alejado.media: Clave del valor O que está más alejado de la media
# valor.mas.alejado.media: Valor de O en datos[, ind.col]
# nombre.mas.alejado.media: Nombre de O en datos
# es.outlier: TRUE/FALSE dependiendo del resultado del test de Grubbs sobre O
# p.value:  p-value calculado por el test de Grubbs
# es.distrib.norm: Resultado de aplicar el test de Normalidad 
#    de Shapiro-Wilk sobre datos[, ind.col]
#    El test de normalidad se aplica sin tener en cuenta el 
#    valor más alejado de la media (el posible outlier O)
#    TRUE si el test no ha podido rechazar
#       -> Sólo podemos concluir que los datos no contradicen una Normal
#    FALSE si el test rechaza 
#       -> Los datos no siguen una Normal
# p.value.test.normalidad: p-value del test de Shapiro

# Requiere el paquete outliers

test_Grubbs <- function(data.frame, indice.columna, alpha = 0.05){
  columna <- data.frame[,indice.columna]
  desvios <- abs(columna - mean(columna, na.rm = TRUE))
  clave.mas.alejado.media <- which.max(desvios) 
  valor.mas.alejado.media <- columna[clave.mas.alejado.media]  
  nombre.mas.alejado.media <- row.names(data.frame)[clave.mas.alejado.media]
  grubbs.test <- grubbs.test(columna)
  es.outlier <- grubbs.test$p.value < alpha  
  p.value <- min(grubbs.test$p.value * 2)
  columna.sin.alejado <- columna[-clave.mas.alejado.media]
  test.normalidad <- shapiro.test(columna.sin.alejado)
  es.distrib.norm <- test.normalidad$p.value >= alpha  
  p.value.test.normalidad <- test.normalidad$p.value
  listafinal <- list(
    nombre.columna = colnames(data.frame)[indice.columna],
    clave.mas.alejado.media = clave.mas.alejado.media,
    valor.mas.alejado.media = valor.mas.alejado.media,
    nombre.mas.alejado.media = nombre.mas.alejado.media,
    es.outlier = es.outlier,
    p.value = p.value,
    p.value.test.normalidad = p.value.test.normalidad,
    es.distrib.norm = es.distrib.norm
  )
  
  listafinal
}

test <- test_Grubbs(datos.num, indice.columna, )
test

#3.3
#3.3.1
claves.outliers.IQR.en.alguna.columna <- claves_outliers_IQR_en_alguna_columna(datos.num, 1.5)
claves.outliers.IQR.en.alguna.columna
claves.outliers.IQR.extremos.en.alguna.columna <- claves_outliers_IQR_en_alguna_columna(datos.num, 3)
claves.outliers.IQR.extremos.en.alguna.columna

#Si se da en más de una columna
claves.outliers.IQR.en.mas.de.una.columna <- unique(
    claves.outliers.IQR.en.alguna.columna[
      duplicated(claves.outliers.IQR.en.alguna.columna)])
claves.outliers.IQR.en.alguna.columna <- unique (claves.outliers.IQR.en.alguna.columna)
claves.outliers.IQR.en.mas.de.una.columna
claves.outliers.IQR.en.alguna.columna 
nombres_filas(datos.num, claves.outliers.IQR.en.mas.de.una.columna)
nombres_filas(datos.num, claves.outliers.IQR.en.alguna.columna)

valores.outliers.IQR.alguna.columna.norm <- datos.num.zscore[claves.outliers.IQR.en.alguna.columna, ]
valores.outliers.IQR.alguna.columna.norm

diagrama <- diag_caja_juntos(datos.num, "Outliers en alguna columna", claves.outliers.IQR.en.alguna.columna)
par(mfrow = c(2, 3)) 

diagrama2 <- diag_caja_juntos(datos.num, "Outliers en alguna columna", claves.outliers.IQR.en.mas.de.una.columna)
par(mfrow = c(2, 3)) 

#3.3.2
# Usar lapply para recorrer las columnas
pdf("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/Grubbs.pdf", width = 10, height = 8)
par(mfrow = c(2, 2))
sapply(seq_along(datos.num.sin.outlier), function(i) {
  columna <- datos.num.sin.outlier[, i]
  nombre_columna <- colnames(datos.num.sin.outlier)[i]
  ajusteNormal <- fitdist(columna, "norm")
  denscomp(ajusteNormal, xlab = nombre_columna, main = paste("Ajuste Normal para", nombre_columna))
})
dev.off()

datos.num.var.norm <- datos.num
head(datos.num.var.norm)

resultados.grubbs <- sapply(seq_along(datos.num.var.norm), function(i) {
  columna <- datos.num.var.norm[, i]
  nombre_columna <- colnames(datos.num.var.norm)[i]
  resultado <- test_Grubbs(datos.num.var.norm, i, )
  return(c(
    nombre_columna = nombre_columna,
    clave_mas_alejado_media = resultado$clave.mas.alejado.media,
    valor_mas_alejado_media = resultado$valor.mas.alejado.media,
    nombre_mas_alejado_media = resultado$nombre.mas.alejado.media,
    es_outlier = resultado$es.outlier,
    p_value = resultado$p.value,
    es_distrib_norm = resultado$es.distrib.norm,
    p_value_test_normalidad = resultado$p.value.test.normalidad
  ))
})
resultados.grubbs

#4
#4.1
#4.1.1
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/test-MVN.png")
cqplot(datos.num.var.norm , method = "classical")
dev.off()

#4.1.2
test.MVN = mvn(datos.num.var.norm, mvnTest = "energy")
test.MVN$multivariateNormality["MVN"]
test.MVN$multivariateNormality["p value"]

#4.1.3
dist.mah.clas <- Mahalanobis(datos.num.var.norm, method = "classical")
cuantil_0975 <- quantile(dist.mah.clas, 0.975)
claves.outliers.mah.clas <- which(dist.mah.clas > cuantil_0975)
nombres_filas <- function(datos.num, claves.outliers) {
  # Extraer los nombres de las filas correspondientes a los outliers
  rownames(datos.num)[claves.outliers]
}
nombres_filas(datos.num.var.norm,claves.outliers.mah.clas)
#Extra
# Calcular el vector de distancias de Mahalanobis usando MCD
dist.mah.mcd <- Mahalanobis(datos.num.var.norm, method = "mcd")
cuantil_97_5_mcd <- quantile(dist.mah.mcd, 0.975)
claves.outliers.mah.mcd <- which(dist.mah.mcd > cuantil_97_5_mcd)
claves.outliers.mah.mcd
nombres_filas (datos.num.var.norm, claves.outliers.mah.mcd)

#4.1.4
# Establecemos la semilla
set.seed(2)
#test individual
test.individual.Cerioli = cerioli2010.fsrmcd.test(datos.num.var.norm, signif.alpha = 0.05)
son.posibles.outliers.individual.Cerioli = test.individual.Cerioli$outliers
claves.test.individual = which (son.posibles.outliers.individual.Cerioli == TRUE)
nombres.test.individual = nombres_filas(datos.num.var.norm, claves.test.individual)
# test interseccion
n = nrow(datos.num.var.norm)
signif.interseccion = 1. - ((1. - 0.05)^(1./n))
test.interseccion.Cerioli = cerioli2010.fsrmcd.test(datos.num.var.norm, signif.alpha = signif.interseccion)  
son.posibles.outliers.Cerioli.interseccion = test.interseccion.Cerioli$outliers
claves.test.interseccion = which (son.posibles.outliers.Cerioli.interseccion == TRUE)
nombres.test.interseccion = nombres_filas(datos.num.var.norm, claves.test.interseccion)
claves.test.individual
nombres.test.individual
claves.test.interseccion
nombres.test.interseccion

#4.2
claves.outliers.IQR.en.alguna.columna <- claves_outliers_IQR_en_alguna_columna(datos.num, 1.5)
claves.outliers.IQR.en.alguna.columna

biplot.outliers.IQR = biplot_2_colores(datos.num, 
                                       claves.outliers.IQR.en.alguna.columna, 
                                       titulo.grupo.a.mostrar = "Outliers IQR",
                                       titulo ="Biplot Outliers IQR")
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/biplot-out-IQR.png")
biplot.outliers.IQR
dev.off()

#4.3
num.vecinos.lof = 5
lof.scores = LOF(dataset = datos.num.zscore, k = num.vecinos.lof)
indices.ordenados <- order(lof.scores,decreasing=TRUE)
lof.scores.ordenados <- lof.scores[indices.ordenados]
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/lof_scores_ordenados.png")
plot(lof.scores.ordenados, col = "black")
dev.off()

png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/prube.png")
plot(lof.scores.ordenados, col = "black", xlim = c(0,50))
dev.off()

# Número de outliers a analizar
num.outliers <- 12
claves.outliers.lof <- indices.ordenados[1:num.outliers]
nombres.outliers <- nombres_filas(datos.num.zscore, claves.outliers.lof)
clave.max.outlier.lof = claves.outliers.lof[1]
colores = rep("black", times = nrow(datos.num.zscore))
colores[clave.max.outlier.lof] = "red"
datos.num.zscore1 <- datos.num.zscore[,c(13,16,17,18,19,20,21)]
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/par22.png")
pairs(datos.num.zscore1, pch = 19,  cex = 0.5, col = colores, lower.panel = NULL)
dev.off()

biplot.max.outlier.lof = biplot_2_colores(datos.num.zscore, clave.max.outlier.lof, titulo = "Mayor outlier LOF")
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/biplot-max-out-lof.png")
biplot.max.outlier.lof
dev.off()

#4.4
#4.4.1
num.outliers <- 5
num.clusters <- 3
set.seed(2)
#Modelo k-means
modelo.kmeans <- kmeans(datos.num.zscore, centers = num.clusters)
asignaciones.clustering.kmeans <- modelo.kmeans$cluster
centroides.normalizados <- modelo.kmeans$centers
head(asignaciones.clustering.kmeans)
centroides.normalizados

desnormaliza <- function(datos, filas.normalizadas) {
  media <- colMeans(datos)
  desviacion <- apply(datos, 2, sd)
  filas.normalizadas * desviacion + media
}
centroides.desnormalizados <- desnormaliza(datos.num, centroides.normalizados)
centroides.desnormalizados
# Función para calcular los outliers del clustering
top_clustering_outliers <- function(datos.normalizados, 
                                    asignaciones.clustering, 
                                    datos.centroides.normalizados, 
                                    num.outliers) {
  distancias <- distancias_a_centroides(datos.normalizados, 
                                        asignaciones.clustering, 
                                        datos.centroides.normalizados)
  indices_ordenados <- order(distancias, decreasing = TRUE)
  claves <- indices_ordenados[seq_len(num.outliers)]
  list(distancias = distancias[claves], claves = claves)
}
outliers <- top_clustering_outliers(datos.num.zscore, 
                                    asignaciones.clustering.kmeans, 
                                    centroides.normalizados, 
                                    num.outliers = nrow(datos.num.zscore))
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/kmeans2.png")
plot(outliers$distancias, col = "black", xlim = c(3200,3500))
dev.off()
num.outliers <- 4
outliers.kmeans <- top_clustering_outliers(datos.num.zscore, 
                                      asignaciones.clustering.kmeans, 
                                      centroides.normalizados, num.outliers)
claves.outliers.kmeans <- outliers.kmeans$claves
nombres.outliers.kmeans <- rownames(datos.num)[claves.outliers.kmeans]
claves.outliers.kmeans
nombres.outliers.kmeans
outliers.kmeans$distancias
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/biplot-kmeans.png")
biplot_outliers_clustering(datos.num, 
                           titulo = "Outliers k-means",
                           asignaciones.clustering = asignaciones.clustering.kmeans,
                           claves.outliers = claves.outliers.kmeans)
dev.off()
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/boxplot-kmeans.png")
diag_caja_juntos(datos.num, "Outliers k-means", claves.outliers.kmeans)
dev.off()

#4.4.2
set.seed(2)
matriz.distancias = dist(datos.num.zscore)
modelo.pam = pam(matriz.distancias , k = num.clusters)
asignaciones.clustering.pam = modelo.pam$clustering   
nombres.medoides = modelo.pam$medoids    
medoides = datos.num[nombres.medoides, ]
medoides.normalizados = datos.num.zscore[nombres.medoides, ]
nombres.medoides
medoides
medoides.normalizados
outliers.pam <- top_clustering_outliers(datos.num.zscore, 
                                        asignaciones.clustering.pam, 
                                        medoides.normalizados, num.outliers)
claves.outliers.pam <- outliers.pam$claves
nombres.outliers.pam <- rownames(datos.num)[claves.outliers.pam]
claves.outliers.pam
nombres.outliers.pam
outliers.pam$distancias
png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/biplot-medoides.png")
biplot_outliers_clustering(datos.num, 
                           titulo = "Outliers pam",
                           asignaciones.clustering = asignaciones.clustering.pam,
                           claves.outliers = claves.outliers.pam)
dev.off()

#4.5
claves.outliers.lof.no.IQR <- setdiff(claves.outliers.lof,claves.outliers.IQR.en.alguna.columna)
nombres.outliers.lof.no.IQR <- nombres_filas(datos.num,claves.outliers.lof.no.IQR)
ejemplo <- biplot_2_colores(datos.num, claves.outliers.lof.no.IQR,
                            titulo = "Outliers LOF (excluidos los que son IQR")
datos.num.zscore[claves.outliers.lof.no.IQR, ]

png("C:/Users/Usuario/Desktop/Master/Mineria-No_supervisado/Outliers/biplot-ejemplo.png")
ejemplo
dev.off()






