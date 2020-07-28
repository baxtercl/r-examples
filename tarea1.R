#--------------------
# TALLER 1 # PARTE 1
#--------------------


#a) ??Qu?? d??a Chile report?? su primer caso?
# Importar datos
datos = read.csv('/Users/asaavedra/Documents/Magister Data Science/03 Programacion con R/Clase 03/Evaluacion/datos.csv')
# Seleccionar ubicaci??n=Chile y nuevos casos mayor a 0
Yapo = datos[(datos$location=='Chile') & (datos$new_cases>0),]
# Ordenar las fechas de forma ascendente
Yapo = Yapo[order(as.Date(Yapo$date, format="%d/%m/%Y")),] 
# Seleccionar la primera fecha en la cual Chile tuvo el primer caso positivo de Covid19
print(paste('Chile present?? su primer caso en la fecha:', Yapo[1,"date"]))

#b) ??Qu?? d??a M??xico super?? los 1000 casos totales?
Wey = datos[(datos$location=='Mexico') & (!is.na(datos$total_cases) & (datos$total_cases > 1000)),]
Wey = Wey[order(as.Date(Wey$date, format="%d/%m/%Y")),] 
print(paste('M??xico super?? los 1000 casos en la fecha:', Wey[1,"date"]))

#c) ??Qu?? pa??s report?? el mayor n??mero de nuevos casos el 14 de abril? ??Cu??ntos casos fueron?
Top1 = datos[(datos$date=='2020-04-14') & (!is.na(datos$new_cases) & (datos$location != 'World')),]
Top1 = Top1[rev(order(Top1$new_cases)), ]
print(paste('El pa??s que report?? m??s neuvos casos para el 14/04/2020, fue:', Top1[1, "location"], ' con un total de ', Top1[1, 'new_cases'], 'casos nuevos.'))

#d) ??Cu??les son los tres pa??ses que, al 1 de junio, ten??an la mayor cantidad de total de casos por mill??n de habitantes?
Top3 = datos[(datos$date=='2020-06-01') & (!is.na(datos$total_cases_per_million)),]
Top3 = Top3[order(Top3$total_cases_per_million, decreasing = TRUE), ]
print(paste('Los 3 paises con mayor cantidad total de casos por millon al 01/06/2020 son:', Top3[1:1, 'location'], ',', Top3[2:2, 'location'], 'y', Top3[3:3,'location']))

#e) Entre los pa??ses que comienzan con A, ??Cu??l es el pa??s con mayor total de casos? ??Cu??ntos casos son?
lol = datos[(!is.na(datos$total_cases)),]
lol = lol[order(lol$total_cases, decreasing = TRUE), ]
lol = lol[(grep('^A', lol$location)), ]
print(paste('El pa??s con mayor total de causa es', lol[1:1, 'location'], 'con', lol[1:1, 'total_cases'], 'casos'))

#--------------------
# TALLER 1 # PARTE 2
#--------------------
datos$TasaEfectividad = round(datos$new_cases/datos$new_tests, 4)
datos$TasaFatalidad = round(datos$total_deaths/datos$total_cases, 4)

DatosChile = datos[(!is.na(datos$new_cases)) & (!is.na(datos$new_tests)) & (datos$new_tests > 0) & (datos$date>='2020-06-01') & (datos$date<='2020-06-30'),]
DatosChile = DatosChile[(grep('Chile', DatosChile$location)), ]
DatosChile = subset(DatosChile, select = c('date', 'TasaEfectividad', 'TasaFatalidad'))
#Se muestra una tabla con todos los datos de junio para Chile, d??nde el d??a 18, existi?? un aumento de 30.000 casos, en relaci??n al d??a anterior
#este aumento se deben a la forma en que como contaban los casos
DatosChile

#--------------------
# TALLER 1 # PARTE 3
#--------------------
#DatosContinente = datos[(!is.na(datos$new_cases)) & (!is.na(datos$new_tests)) & (datos$new_tests > 0) & (datos$total_cases > 0) & (datos$date>='2020-01-01') & (datos$date<='2020-06-30'),]
#DatosContinente = DatosContinente[order(DatosContinente$TasaFatalidad, decreasing = TRUE), ]

DatosContinente = subset(datos, (!is.na(datos$continent)) & (datos$continent != ''))
Cases = aggregate(DatosContinente$new_cases ~ DatosContinente$continent, FUN=sum)
colnames(Cases) = c('continent', 'new_cases')

Population = aggregate(DatosContinente$population ~ DatosContinente$continent, FUN=sum)
colnames(Population) = c('continent', 'population')

tablilla = merge(Cases, Population, by.x = 'continent', by.y = 'continent')
tablilla = tablilla[order(tablilla$new_cases, decreasing = TRUE), ]
tablilla
#Conclusi??n: A pesar de que en Europa hay m??s poblaci??n que en America del Norte, est ??ltimo posee m??s nuevos casos, donde
# se podr??a relacionar con la alta tasa de migracion (legal/ilegal) que posee Estados Unidos, principalmente.