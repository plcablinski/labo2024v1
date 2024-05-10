# limpiar entorno
rm(list = ls())

library(readxl)
library(dplyr)
# setear el directorio
setwd("C:/Users/pcablinski/OneDrive - Universidad Austral/Laboratorio Implementacion I/Features colaborativo/serie 200")

# abrir el archivo excel Ganancias_200.xlsx
comparacion_ganancias <- read_excel("comp_experimentos.xlsx")
# redondear a 0 decimales las columnas de comparacion_ganancias y convertirlos a enteros
comparacion_ganancias <- comparacion_ganancias %>% mutate(across(everything(), round, 0)) %>% mutate(across(everything(), as.integer))

# hacer test de wilcoxon entre Baseline, Catedra, Iteracion_1, Iteracion_2 
Baseline_vs_Catedra <- wilcox.test(comparacion_ganancias$Baseline, comparacion_ganancias$Catedra, paired = TRUE)
Baseline_vs_Iteracion_1 <- wilcox.test(comparacion_ganancias$Baseline, comparacion_ganancias$Iteracion_1, paired = TRUE)
Baseline_vs_Iteracion_2 <- wilcox.test(comparacion_ganancias$Baseline, comparacion_ganancias$Iteracion_2, paired = TRUE)
Catedra_vs_Iteracion_1 <- wilcox.test(comparacion_ganancias$Catedra, comparacion_ganancias$Iteracion_1, paired = TRUE)
Catedra_vs_Iteracion_2 <- wilcox.test(comparacion_ganancias$Catedra, comparacion_ganancias$Iteracion_2, paired = TRUE)
Iteracion_1_vs_Iteracion_2 <- wilcox.test(comparacion_ganancias$Iteracion_1, comparacion_ganancias$Iteracion_2, paired = TRUE)

# Hacer un arreglo de 4 x 4 con los resultados de los test de wilcoxon en cada fila y columna correspondiente poner el p-valor
resultados_test_wilcoxon <- matrix(NA, nrow = 4, ncol = 4)
resultados_test_wilcoxon[1, 2] <- Baseline_vs_Catedra$p.value
resultados_test_wilcoxon[1, 3] <- Baseline_vs_Iteracion_1$p.value
resultados_test_wilcoxon[1, 4] <- Baseline_vs_Iteracion_2$p.value
resultados_test_wilcoxon[2, 3] <- Catedra_vs_Iteracion_1$p.value
resultados_test_wilcoxon[2, 4] <- Catedra_vs_Iteracion_2$p.value
resultados_test_wilcoxon[3, 4] <- Iteracion_1_vs_Iteracion_2$p.value
resultados_test_wilcoxon[2, 1] <- Baseline_vs_Catedra$p.value
resultados_test_wilcoxon[3, 1] <- Baseline_vs_Iteracion_1$p.value
resultados_test_wilcoxon[4, 1] <- Baseline_vs_Iteracion_2$p.value
resultados_test_wilcoxon[3, 2] <- Catedra_vs_Iteracion_1$p.value
resultados_test_wilcoxon[4, 2] <- Catedra_vs_Iteracion_2$p.value
resultados_test_wilcoxon[4, 3] <- Iteracion_1_vs_Iteracion_2$p.value

# poner nombres a filas y columnas
rownames(resultados_test_wilcoxon) <- c("Baseline", "Catedra", "Iteracion_1", "Iteracion_2")
colnames(resultados_test_wilcoxon) <- c("Baseline", "Catedra", "Iteracion_1", "Iteracion_2")
resultados_test_wilcoxon

# exportar resultados a un archivo excel
write.csv(resultados_test_wilcoxon, "resultados_test_wilcoxon.csv")





