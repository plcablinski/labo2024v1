# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("c:/labi") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")
# Reemplazar en dataset clase_ternaria donde dice "BAJA+1" por "CONTINUA"
dataset$clase_ternaria <- ifelse(dataset$clase_ternaria == "BAJA+1", "CONTINUA", dataset$clase_ternaria)


dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# agregar la columna pesos a dtrain en funcion de los valores de clase_ternaria si es BAJA+1 entonces 1,si es  BAJA+2 entonces 400 , si es  CONTINUA entonces .1
pesos <- c("BAJA+1" = 1, "BAJA+2" = 1, "CONTINUA" = 1)
dtrain$pesos <- pesos[dtrain$clase_ternaria]

# Ajustar el modelo con los pesos
modelo <- rpart(
  formula = "clase_ternaria ~ .",
  # los datos donde voy a entrenar excluyendo la columna pesos
  data = dtrain[, -"pesos"] , 
  #weights = dtrain$pesos, # los pesos
  xval = 0,
  cp = -0.5, # esto significa no limitar la complejidad de los splits
  minsplit = 600, # minima cantidad de registros para que se haga el split
  minbucket = 170, # tamaño minimo de una hoja
  maxdepth = 6
) # profundidad maxima del arbol

# grafico el arbol
prp(modelo,
    extra = 101, digits = -5,
    branch = 1, type = 4, varlen = 0, faclen = 0
)

# aplico el modelo a los datos nuevos
prediccion <- predict(
  object = modelo,
  newdata = dapply,
  type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades


# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1/40)]




# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
dir.create("./exp/")
dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
       file = "./exp/KA2001/K101_001.csv",
       sep = ","
)

# Para cada distinto valor de prob_baja2 en dapply
#  genero un archivo con los campos numero_de_cliente y Predicted
#  en la carpeta ./exp/KA2001
#  con el nombre K101_00 el valor .csv
#  separado por comas
#  y guardo el archivo
for (i in unique(dapply$prob_baja2)) {
  dapply[, Predicted := as.numeric(prob_baja2 > i)]
  fwrite(dapply[, list(numero_de_cliente, Predicted)],
         file = paste0("./exp/KA2001/K101_", formatC(sum(dapply$Predicted), width = 6, format = "d", flag = "0"),"_" , i,".csv"),
         sep = ","
  )
}
8


