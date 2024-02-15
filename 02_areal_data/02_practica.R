# ________________________________________________________________________ #
# MOOC BOSQUE DIGITAL - Análisis Estadístico de Datos Espaciales con R
#
# - Autor: Adrián Cidre González
# - https://adrian-cidre.com
#
# SEMANA 02 - Areal Data
# - Tema 07: Práctica en R
# ________________________________________________________________________ #

# 1. Cargar paquetes ------------------------------------------------------

library(pacman)

p_load(tidyverse, sf, sfdep, ggtext, mapview, leafpop)

# 2. Cargar datos ---------------------------------------------------------

## Cargar incendios Asturias
## - Nombre objeto: incendios_sf
## - layer: asturias o andalucia


# 3. Preparar datos -------------------------------------------------------

## Seleccionar columnas necesarias
## - Municipio, Total_incendios


## Transformar a EPSG:25830


## Calcular densidad de incendios
## - Crear columna de area en km^2
## - Crear columna de d_incendios


# 4. Visualización --------------------------------------------------------

## Histograma de distribución de d_incendios
## - Sigue una distribución normal?


## Visualizar mapa de coropletas de la variable d_incendios


# 5. Vecinos y pesos ------------------------------------------------------

## Asignar vecinos y pesos
## - Nombre objeto: incendios_wt_sf
## - Vecinos: contiguos (nb)
## - Utilizar método estandarizado por filas (wt)


## Cuántos vecinos se han generado en total?
## Cuál es la densidad media de vecinos por región?


# 6. Análisis de autocorrelación ------------------------------------------

## 6.1. Moran's I Global -----------------------

### Calcular I de Moran Global
### - Nombre objeto: incendios_moran
### - Utilizar 2000 simulaciones de Monte Carlo
### - Alternativa greater


### Visualizar y contestar
### - Cuál es el p-valor?
### - Para un nivel de significación del 0.05, cuál es nuestra conclusión?


## 6.2. Moran's I Local ------------------------

### Realizar el test de Moran Local
### - Nombre objeto: incendios_moran_sf
### - Utilizar alternative two.sided
### - Desanidar resultados


### Explorar resultados


### Preparar para visualizar
### - Nombre objeto: incendios_final_sf
### - Convertir a "Insignificant" las observaciones de la columna pysal,
###   cuyo p_ii_sim sea superior a 0.01. Además convertir a factor


### Explorar nivel de la columna pysal
### - Reordenarlos de una forma lógica en el código previo


### Visualizar clasificación final con mapview

