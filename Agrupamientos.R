#Cluster jerarquico

# Agrupamientos con variables numericas
#
# Cargar paquetes
#
library(cluster)   # daisy()
library(RcmdrMisc) # rowPercents()
#
# Cargar datos

enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/main/tabla_CGS.txt"
tumbas_CGS <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)


# Crear tabla con datos numéricos

nueva_tabla <- data.frame( ceramica = tumbas_CGS$Total_ceramica, fauna = tumbas_CGS$Total_fauna, 
                           conchas = tumbas_CGS$Total_conchas, liticos = tumbas_CGS$Total_liticos - tumbas_CGS$Lápida.lajas,
                           lajas = tumbas_CGS$Lápida.lajas, volantes = tumbas_CGS$Volantes_de_uso,
                           fragmentos = tumbas_CGS$Total_fragmentos_ceramicos, collar = tumbas_CGS$Cuentas_de_collar,
                           semillas = tumbas_CGS$Material_Vegetal_Calcinado, row.names = row.names(tumbas_CGS))

print(nueva_tabla)
#
#
# Escalar datos
#
tumbas_esc <- data.frame(scale(nueva_tabla))

#
# Matrices de distancia y de similitud
#

(DISTMAT <- daisy(tumbas_esc, metric = "gower")) # Calcula matriz de distancia
#
# Evaluar si hay mucha o poca distancia entre casos
#
hist(DISTMAT) 
#
# Calcular coeficiente de aglomeración
#
aglomeracion <- agnes(tumbas_esc, method = "complete")
cat("el coeficiente de aglomeración es ", aglomeracion$ac, "\n")
#
# Hacer el dendrograma
#
summary(DISTMAT) # mediana, media y cuartiles de la matriz de distancia
#
hc <- hclust(DISTMAT, method = "complete") # crea datos para construir dendrograma
dendrohc <- as.dendrogram(hc)
#
# Ver el dendrograma
#
par(mfrow = c(1,1))
plot(dendrohc, edgePar = list(col = 1:2, lty = 2:3),
     edge.root = TRUE) # Plano de dendrograma
abline(h = c(seq(0,0.6, by=0.1)), col =c(2:13)) # lineas
#
memb <- cutree(hc, h = 0.2)
nueva_tabla$Grupo <- memb
plot(dendrohc, edgePar = list(col = 1:2, lty = 2:3),
     edge.root = TRUE) # Plano de dendrograma
rect.hclust(hc, h = 0.25, border = c(1:5), cluster = cutree(hc, h = 0.25)) # Plano de dendrograma
#
help("rect.hclust")
#	Agrupamiento por variables:
par(mar = c(2,4,2,2))
dv <- dist(cor(nueva_tabla[1:8]))						# Correlaciones
plot(hclust(dv, method="average"),hang = -1)			# Agrupamiento simple
#
#	Agrupamiento por variables, dendrograma horizontal:
hc <- hclust(dv, method="average")
plot(as.dendrogram(hc, hang=-1), horiz = TRUE)
#
#












# Exploración 


# Crear tabla con datos numéricos

nueva_tabla <- data.frame( ceramica = tumbas_CGS$Total_ceramica, fauna = tumbas_CGS$Total_fauna, 
                           conchas = tumbas_CGS$Total_conchas, liticos = tumbas_CGS$Total_liticos,
                           fragmentos = tumbas_CGS$Total_fragmentos_ceramicos,
                           row.names = row.names(tumbas_CGS))

nueva_tabla <- data.frame( Venado = tumbas_CGS$Cervidae, Roedor = tumbas_CGS$Caviidae, 
                           Semillas = tumbas_CGS$Material_Vegetal_Calcinado, Conchas = tumbas_CGS$Total_conchas, 
                           row.names = row.names(tumbas_CGS))

nueva_tabla <- data.frame( Piedras = tumbas_CGS$Piedras_naturales, Lajas = tumbas_CGS$Lápida.lajas,
                           Ceramica = tumbas_CGS$Total_ceramica, row.names = row.names(tumbas_CGS))


nueva_tabla <- data.frame( Jarras = tumbas_CGS$Jarras, Urnas = tumbas_CGS$Urna_funeraria, 
                           Ollas = tumbas_CGS$Ollas, Alcarraza = tumbas_CGS$Alcarraza,
                           Copas = tumbas_CGS$Copas, Cuencos = tumbas_CGS$Cuenco, Murcura = tumbas_CGS$Múcura,
                           Cuentas_de_collar = tumbas_CGS$Cuentas_de_collar, Volantes_de_uso = tumbas_CGS$Volantes_de_uso, 
                           row.names = row.names(tumbas_CGS))

nueva_tabla <- data.frame( Venado = tumbas_CGS$Cervidae, Roedor = tumbas_CGS$Caviidae, 
                           Felino = tumbas_CGS$Felidae, Ave = tumbas_CGS$Ave, Indeterminado = tumbas_CGS$Indeterminado,
                           Canino = tumbas_CGS$Canidae, row.names = row.names(tumbas_CGS))   

nueva_tabla <- data.frame( Maiz = tumbas_CGS$Maiz, Tusas = tumbas_CGS$Tusas, Frijol = tumbas_CGS$Frijol,
                           Ramas = tumbas_CGS$Ramas, Espigas = tumbas_CGS$Espigas, Leguminosa = tumbas_CGS$Leguminosa,
                           Indeterminada = tumbas_CGS$Indeterminada, row.names = row.names(tumbas_CGS))  

nueva_tabla <- data.frame( Euglandina = tumbas_CGS$Euglandina, Plekokelius_Fulminans = tumbas_CGS$Plekokelius_Fulminans,
                           P_Plekocheilus = tumbas_CGS$P_Plekocheilus, Bulimulidae = tumbas_CGS$Bulimulidae,
                           Drymaeus_Nigrofasciatus = tumbas_CGS$Drymaeus_Nigrofasciatus, P_Succinoides = tumbas_CGS$P_Succinoides,
                           Indeterminado = tumbas_CGS$Caracol_Indeterminado, row.names = row.names(tumbas_CGS))  

nueva_tabla <- data.frame( lajas = tumbas_CGS$Lápida.lajas, Cuarzo = tumbas_CGS$Cuarzo, Mano_de_moler = tumbas_CGS$Mano_de_moler,
                           Raspador = tumbas_CGS$Raspadores, Lascas = tumbas_CGS$Lascas, Perforador = tumbas_CGS$Perforador,
                           Desechos_de_talla = tumbas_CGS$Desechos_de_talla, Martillo = tumbas_CGS$Martillo,
                           Cantos_rodados = tumbas_CGS$Cantos_rodados, Metate = tumbas_CGS$Metate, Nucleos = tumbas_CGS$Nucleos,
                           Piedras_naturales = tumbas_CGS$Piedras_naturales, row.names = row.names(tumbas_CGS))  


nueva_tabla <- data.frame( Jarras = tumbas_CGS$Jarras, Urnas = tumbas_CGS$Urna_funeraria, 
                           Cuentas_de_collar = tumbas_CGS$Cuentas_de_collar, Volantes_de_uso = tumbas_CGS$Volantes_de_uso,
                           Ollas = tumbas_CGS$Ollas, Alcarraza = tumbas_CGS$Alcarraza,
                           Copas = tumbas_CGS$Copas, Cuencos = tumbas_CGS$Cuenco, Murcura = tumbas_CGS$Múcura,
                           Venado = tumbas_CGS$Cervidae, Roedor = tumbas_CGS$Caviidae, 
                           Felino = tumbas_CGS$Felidae, Ave = tumbas_CGS$Ave, Indeterminado = tumbas_CGS$Indeterminado,
                           Canino = tumbas_CGS$Canidae, row.names = row.names(tumbas_CGS))

nueva_tabla <- data.frame( Venado = tumbas_CGS$Cervidae, Roedor = tumbas_CGS$Caviidae, 
                           Felino = tumbas_CGS$Felidae, Ave = tumbas_CGS$Ave, Indeterminado = tumbas_CGS$Indeterminado,
                           Canino = tumbas_CGS$Canidae, Maiz = tumbas_CGS$Maiz, Tusas = tumbas_CGS$Tusas, Frijol = tumbas_CGS$Frijol,
                           Ramas = tumbas_CGS$Ramas, Espigas = tumbas_CGS$Espigas, Leguminosa = tumbas_CGS$Leguminosa,
                           Indeterminada = tumbas_CGS$Indeterminada, row.names = row.names(tumbas_CGS))   

nueva_tabla <- data.frame( lajas = tumbas_CGS$Lápida.lajas, Cuarzo = tumbas_CGS$Cuarzo, Mano_de_moler = tumbas_CGS$Mano_de_moler,
                           Raspador = tumbas_CGS$Raspadores, Lascas = tumbas_CGS$Lascas, Perforador = tumbas_CGS$Perforador,
                           Desechos_de_talla = tumbas_CGS$Desechos_de_talla, Martillo = tumbas_CGS$Martillo,
                           Cantos_rodados = tumbas_CGS$Cantos_rodados, Metate = tumbas_CGS$Metate, Nucleos = tumbas_CGS$Nucleos,
                           Piedras_naturales = tumbas_CGS$Piedras_naturales, Venado = tumbas_CGS$Cervidae, Roedor = tumbas_CGS$Caviidae, 
                           Felino = tumbas_CGS$Felidae, Ave = tumbas_CGS$Ave, Indeterminado = tumbas_CGS$Indeterminado,
                           Canino = tumbas_CGS$Canidae, row.names = row.names(tumbas_CGS))

nueva_tabla <- data.frame( lajas = tumbas_CGS$Lápida.lajas, Cuarzo = tumbas_CGS$Cuarzo, Mano_de_moler = tumbas_CGS$Mano_de_moler,
                           Raspador = tumbas_CGS$Raspadores, Lascas = tumbas_CGS$Lascas, Perforador = tumbas_CGS$Perforador,
                           Desechos_de_talla = tumbas_CGS$Desechos_de_talla, Martillo = tumbas_CGS$Martillo,
                           Cantos_rodados = tumbas_CGS$Cantos_rodados, Metate = tumbas_CGS$Metate, Nucleos = tumbas_CGS$Nucleos,
                           Piedras_naturales = tumbas_CGS$Piedras_naturales, Euglandina = tumbas_CGS$Euglandina, Plekokelius_Fulminans = tumbas_CGS$Plekokelius_Fulminans,
                           P_Plekocheilus = tumbas_CGS$P_Plekocheilus, Bulimulidae = tumbas_CGS$Bulimulidae,
                           Drymaeus_Nigrofasciatus = tumbas_CGS$Drymaeus_Nigrofasciatus, P_Succinoides = tumbas_CGS$P_Succinoides,
                           Indeterminado = tumbas_CGS$Caracol_Indeterminado, row.names = row.names(tumbas_CGS))



nueva_tabla <- data.frame( lajas = tumbas_CGS$Lápida.lajas, Cuarzo = tumbas_CGS$Cuarzo, Mano_de_moler = tumbas_CGS$Mano_de_moler,
                           Raspador = tumbas_CGS$Raspadores, Lascas = tumbas_CGS$Lascas, Perforador = tumbas_CGS$Perforador,
                           Desechos_de_talla = tumbas_CGS$Desechos_de_talla, Martillo = tumbas_CGS$Martillo,
                           Cantos_rodados = tumbas_CGS$Cantos_rodados, Metate = tumbas_CGS$Metate, Nucleos = tumbas_CGS$Nucleos,
                           Piedras_naturales = tumbas_CGS$Piedras_naturales, Euglandina = tumbas_CGS$Euglandina, Plekokelius_Fulminans = tumbas_CGS$Plekokelius_Fulminans,
                           P_Plekocheilus = tumbas_CGS$P_Plekocheilus, Bulimulidae = tumbas_CGS$Bulimulidae,
                           Drymaeus_Nigrofasciatus = tumbas_CGS$Drymaeus_Nigrofasciatus, P_Succinoides = tumbas_CGS$P_Succinoides,
                           Indeterminado = tumbas_CGS$Caracol_Indeterminado, row.names = row.names(tumbas_CGS))



  
print(nueva_tabla)
#
#
# Escalar datos
#
tumbas_esc <- data.frame(scale(nueva_tabla))

#
# Matrices de distancia y de similitud
#

(DISTMAT <- daisy(tumbas_esc, metric = "euclidean")) # Calcula matriz de distancia
#
# Evaluar si hay mucha o poca distancia entre casos
#
hist(DISTMAT) 
#
# Calcular coeficiente de aglomeración
#
aglomeracion <- agnes(tumbas_esc, method = "complete")
cat("el coeficiente de aglomeración es ", aglomeracion$ac, "\n")
#
# Hacer el dendrograma
#
summary(DISTMAT) # mediana, media y cuartiles de la matriz de distancia
#
hc <- hclust(DISTMAT, method = "complete") # crea datos para construir dendrograma
dendrohc <- as.dendrogram(hc)
#
# Ver el dendrograma
#
plot(dendrohc) # Plano de dendrograma
abline(h = c(seq(1,10, by=1)), col =c(2:13)) # lineas
#
#
#	Agrupamiento por variables:
par(mar = c(2,4,2,2))
dv <- dist(cor(nueva_tabla))						# Correlaciones
plot(hclust(dv, method="average"),hang = -1)			# Agrupamiento simple
#
#	Agrupamiento por variables, dendrograma horizontal:
hc <- hclust(dv, method="average")
plot(as.dendrogram(hc, hang=-1), horiz = TRUE)





























# Análisis de K-means
#
# Gráfico de codo
#
wss <- sapply(1:10,
              function(k){kmeans(tumbas_esc,k,nstart=10,iter.max=20000)$tot.withinss})
plot(1:10,wss, type="b", pch=19, frame=FALSE, xlab="No. de grupos",
     ylab="Suma de cuadrados en grupos")
abline(v=4, lty=2)
#
# 
#  Revisar la diferencia entre withinss (suma de cuadrados) de 2 a k grupos.
#
#  A menor valor de la suma de cuadrados de los grupos, menos es la
#  diferencia entre los grupos.
# 
#  El estrés disminuye a los 4 grupos
# 
#  A partir de 5 o más grupos el cambio no es mayor, es decir no se 
#  reducen mucho las diferencias en los grupos si se crea un mayor 
#  número de categorías de agurpación.
#
#
#	K-means para 5 grupos
#
# Cambiar el número de grupos
#
(k=8) # asigna 5 grupos
#
kc <- kmeans(tumbas_esc, k, iter.max = 1000) # crear el kmeans
print(kc) # Imprimir kc
str(kc) # Revisar la estructura del objeto kc
#
# Asignar la columna de pertencia grupal a la tabla de datos en bruto
#
# nueva_tabla$Grupo_deposito <- kc$cluster
# write.csv(depositos, "grupo_depositos.csv")
#
# Extraer e imprimir los centroides
#
centroides <- kc$centers
centroides
#
#
centroides[,1]*sd(nueva_tabla$ceramica)+mean(nueva_tabla$ceramica)
centroides[,2]*sd(nueva_tabla$fauna)+mean(nueva_tabla$fauna)
centroides[,3]*sd(nueva_tabla$conchas)+mean(nueva_tabla$conchas)







