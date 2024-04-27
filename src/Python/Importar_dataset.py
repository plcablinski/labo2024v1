# Generar un script para importar el archivo C:\Labi\datasets\dataset_pequeno.csv
# a un dataframe de pandas. Luego, mostrar las primeras 5 filas del dataframe.

import pandas as pd
import numpy as np

# importar el archivo csv a un dataframe
df = pd.read_csv('C:/Labi/datasets/dataset_pequeno.csv', low_memory=False)

# mostrar cuantas filas y columnas tiene el dataframe
print(df.shape)

# por cada columna mostrar el nombre, el tipo de datos 
# y la cantidad de valores no nulos que tiene
print(df.info())

# para cada foto_mes de df mostrar cuantos registros hay
print(df['foto_mes'].value_counts())