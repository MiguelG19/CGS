### TESIS Miguel Angel Guerrero






enlace <- "https://raw.githubusercontent.com/rafaelroblesc/Usme/main/tabla_CGS.txt"
tumbas_CGS <- read.table(enlace, header = TRUE, dec = ".", row.names = 1)

print(tumbas_CGS$Rango_etario)
#
str(tumbas_CGS) # Revisar la tabla de datos


#
# RESUMEN ESTADISTICO VARIABLES CATEG?RICAS

table(tumbas_CGS$SItio) #frecuencia de tumba por sitio
round(prop.table(table(tumbas_CGS$SItio))*100, 2) #Porcentaje de tumbas por sitio

table(tumbas_CGS$Sexo, useNA = "always")
round(prop.table(table(tumbas_CGS$Sexo, useNA = "always"))*100, 2)

table(tumbas_CGS$Sexo)
round(prop.table(table(tumbas_CGS$Sexo))*100, 2)

table(tumbas_CGS$Formatumba)
round(prop.table(table(tumbas_CGS$Formatumba))*100, 2)

table(tumbas_CGS$Orientacion)
round(prop.table(table(tumbas_CGS$Orientacion))*100, 2)

table(tumbas_CGS$Posicion_del_cuerpo)
round(prop.table(table(tumbas_CGS$Posicion_del_cuerpo))*100, 2)

table(tumbas_CGS$Articulado)
round(prop.table(table(tumbas_CGS$Articulado))*100, 2)

#Resumen de variables numericas 

round(mean(tumbas_CGS$Total_fragmentos_ceramicos),2)
hist(tumbas_CGS$Total_fragmentos_ceramicos)

round(mean(tumbas_CGS$Total_ceramica),2)
hist(tumbas_CGS$Total_ceramica)

round(mean(tumbas_CGS$Jarras),2)
hist (tumbas_CGS$Jarras)

round(mean(tumbas_CGS$Urna_funeraria),2)

round(mean(tumbas_CGS$Alcarraza),2)

round(mean(tumbas_CGS$Ollas),2)

round(mean(tumbas_CGS$Copas),2)

round(mean(tumbas_CGS$Cuenco),2)

round(mean(tumbas_CGS$Múcura),2)

#Total fauna

round(mean(tumbas_CGS$Total_fauna),2)

round(mean(tumbas_CGS$Cervidae),2)

round(mean(tumbas_CGS$X.Caviidae),2)
  
round(mean(tumbas_CGS$Canidae),2)
  
round(mean(tumbas_CGS$Felidae),2)

round(mean(tumbas_CGS$Ave),2)

round(mean(tumbas_CGS$Indeterminado),2)

#Conchas

round(mean(tumbas_CGS$Total_conchas),2)

round(mean(tumbas_CGS$Euglandina),2)

round(mean(tumbas_CGS$Plekokelius_Fulminans),2)

round(mean(tumbas_CGS$P_Plekocheilus),2)

round(mean(tumbas_CGS$Bulimulidae),2)

round(mean(tumbas_CGS$Drymaeus_Nigrofasciatus),2)

round(mean(tumbas_CGS$P_Succinoides),2)

round(mean(tumbas_CGS$Caracol_Indeterminado),2)

#Material_Vegetal_Calcinado

round(mean(tumbas_CGS$Material_Vegetal_Calcinado),2) #Material_Vegetal_Calcinado

round(mean(tumbas_CGS$Maiz),2)

round(mean(tumbas_CGS$Tusas),2)

round(mean(tumbas_CGS$Frijol),2)

round(mean(tumbas_CGS$Ramas),2)

round(mean(tumbas_CGS$Espigas),2)

round(mean(tumbas_CGS$Leguminosa),2)

round(mean(tumbas_CGS$Indeterminada),2)

#Liticos

round(mean(tumbas_CGS$Total_liticos),2)

round(mean(tumbas_CGS$Lápida.lajas),2)           

round(mean(tumbas_CGS$Cuarzo),2)

round(mean(tumbas_CGS$Mano_de_moler),2)

round(mean(tumbas_CGS$Raspadores),2)

round(mean(tumbas_CGS$Lascas),2)

round(mean(tumbas_CGS$Perforador),2)

round(mean(tumbas_CGS$Desechos_de_talla),2)

round(mean(tumbas_CGS$Martillo),2)

round(mean(tumbas_CGS$Cantos_rodados),2)

round(mean(tumbas_CGS$Metate),2)

round(mean(tumbas_CGS$Nucleos),2)

round(mean(tumbas_CGS$Piedras_naturales),2)

#
      
round(mean(tumbas_CGS$Cuentas_de_collar),2)

round(mean(tumbas_CGS$Volantes_de_uso),2)

#

round(mean(tumbas_CGS$Profundida_en_cm),2)

round(mean(tumbas_CGS$Diametro.Ancho_en_cm),2)

#

table(tumbas_CGS$Total_ceramica ,tumbas_CGS$SItio, useNA = "always")

table(tumbas_CGS$Total_fauna ,tumbas_CGS$SItio, useNA = "always")

table(tumbas_CGS$Total_conchas ,tumbas_CGS$SItio, useNA = "always")

table(tumbas_CGS$Total_liticos ,tumbas_CGS$SItio, useNA = "always")

table(tumbas_CGS$Material_Vegetal_Calcinado ,tumbas_CGS$SItio, useNA = "always")

round(prop.table(table(tumbas_CGS$Total_ceramica,tumbas_CGS$Sexo, useNA = "always"))*100, 2)


install.packages("aplpack")
library(aplpack)

stem.leaf.backback(x = tumbas_CGS$Total_fauna[tumbas_CGS$SItio == "La_Muela"], 
                   y = tumbas_CGS$Total_fauna[tumbas_CGS$SItio == "Bosque_Alto"])

stem.leaf.backback(x = c(1,4,4,2,4,11,20,1,16,1,17,2,6,18,13,3,23,11,10,13,3,6,14,19,10,5),
                   y = c(4,3,1), unit = 3)


#Numero total de los objetos

tumbas_CGS$total_Objetos <- tumbas_CGS$Total_ceramica+tumbas_CGS$Total_fauna+tumbas_CGS$Total_conchas+tumbas_CGS$Material_Vegetal_Calcinado+tumbas_CGS$Total_liticos+tumbas_CGS$Cuentas_de_collar+tumbas_CGS$Volantes_de_uso

table(tumbas_CGS$total_Objetos ,tumbas_CGS$SItio, useNA = "always")

hist(tumbas_CGS$total_Objetos)


stem.leaf.backback(x = subset(tumbas_CGS$Total_fauna[tumbas_CGS$SItio == "La_Muela"], subset = tumbas_CGS$Total_fauna[tumbas_CGS$SItio == "La_Muela"] > 0 ),
                   y = subset(tumbas_CGS$Total_fauna[tumbas_CGS$SItio == "Bosque_Alto"], subset = tumbas_CGS$Total_fauna[tumbas_CGS$SItio == "Bosque_Alto"] > 0) )

stem.leaf.backback(x=tumbas_CGS$total_Objetos[tumbas_CGS$SItio=="La_Muela"], 
                   y=tumbas_CGS$total_Objetos[tumbas_CGS$SItio=="Bosque_Alto"])



