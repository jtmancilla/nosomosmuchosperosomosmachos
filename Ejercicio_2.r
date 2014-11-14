#carga de archivos a utilizar
source("0-load.r")
source("1_eda.r")

#asignación de datos
algas.data  <- load()


#Exploración general.

summary(algas.data)
str(algas.data)
algas.data2  <- algas.data
algas.data2[1]  <- algas.data[6]

str(algas.data2)
summary(algas.data2)


# visualización de NA`s

na_algas <- as.data.frame(abs(is.na(algas.data)))
colSums(na_algas)

# visualización de 6 covariables de la base de datos algas.data
eda1(algas.data2,6)

# visualización de las covariables comparada contra otra. (la base en este caso es la columna 1).
eda2(algas.data2)
